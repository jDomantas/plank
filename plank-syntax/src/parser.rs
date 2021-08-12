use ast::{
    BinaryOp, CallParam, Expr, Field, FnParam, Function, FunctionType, Ident, ItemName, Literal,
    Mutability, Program, Statement, Struct, Type, UnaryOp,
};
use plank_errors::Reporter;
use position::{Position, Span, Spanned};
use std::collections::{HashMap, HashSet, VecDeque};
use tokens::{Keyword, Token, TokenKind};

macro_rules! parse_infix {
    ($parser:expr, $tok:ident, $op:ident, $prec:ident, $left_assoc:expr) => {{
        $parser.infix(
            TokenKind::Token(Token::$tok),
            &BinaryOpParser {
                prec: Precedence::$prec,
                op: BinaryOp::$op,
                left_assoc: $left_assoc,
            },
        );
    }};
}

pub fn parse(tokens: Vec<Spanned<Token>>, reporter: Reporter) -> Program {
    let mut parser = Parser::new(tokens, reporter);

    parser.prefix(TokenKind::Literal, &LiteralParser);
    parser.prefix(
        TokenKind::Token(Token::Keyword(Keyword::Unit)),
        &LiteralParser,
    );
    parser.prefix(TokenKind::Ident, &NameParser);
    parser.prefix(TokenKind::Token(Token::Ampersand), &AddressOfParser);
    parser.prefix(TokenKind::Token(Token::Plus), &UnaryOpParser(UnaryOp::Plus));
    parser.prefix(
        TokenKind::Token(Token::Minus),
        &UnaryOpParser(UnaryOp::Minus),
    );
    parser.prefix(
        TokenKind::Token(Token::Star),
        &UnaryOpParser(UnaryOp::Deref),
    );
    parser.prefix(TokenKind::Token(Token::Not), &UnaryOpParser(UnaryOp::Not));
    parser.prefix(TokenKind::Token(Token::LeftParen), &ParenthesisedParser);

    parser.infix(TokenKind::Token(Token::LeftParen), &CallParser);
    parser.infix(TokenKind::Token(Token::Dot), &FieldParser);
    parser.infix(TokenKind::Token(Token::Keyword(Keyword::As)), &CastParser);

    parse_infix!(parser, And, And, And, true);
    parse_infix!(parser, Or, Or, Or, true);
    parse_infix!(parser, Plus, Add, Addition, true);
    parse_infix!(parser, Minus, Subtract, Addition, true);
    parse_infix!(parser, Star, Multiply, Multiplication, true);
    parse_infix!(parser, Slash, Divide, Multiplication, true);
    parse_infix!(parser, Percent, Modulo, Multiplication, true);
    parse_infix!(parser, Less, Less, Comparision, true);
    parse_infix!(parser, LessEqual, LessEqual, Comparision, true);
    parse_infix!(parser, Greater, Greater, Comparision, true);
    parse_infix!(parser, GreaterEqual, GreaterEqual, Comparision, true);
    parse_infix!(parser, Equal, Equal, Equation, true);
    parse_infix!(parser, NotEqual, NotEqual, Equation, true);
    parse_infix!(parser, Assign, Assign, Assignment, false);

    parser.parse_program()
}

type ParseResult<T> = Result<T, ()>;

enum PartialResult<T, U> {
    Ok(T),
    Partial(U),
    Error,
}

impl<T, U> From<ParseResult<T>> for PartialResult<T, U> {
    fn from(res: ParseResult<T>) -> PartialResult<T, U> {
        match res {
            Ok(x) => PartialResult::Ok(x),
            Err(()) => PartialResult::Error,
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
enum Expectation {
    Expression,
    Operator,
    Type,
    Token(TokenKind),
}

impl ::std::fmt::Display for Expectation {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Expectation::Operator => write!(f, "operator"),
            Expectation::Expression => write!(f, "expression"),
            Expectation::Type => write!(f, "type"),
            Expectation::Token(ref tok) => write!(f, "{}", tok),
        }
    }
}

struct Parser<'a> {
    reporter: Reporter,
    expected: HashSet<Expectation>,
    expected2: HashSet<Expectation>,
    tokens: VecDeque<Spanned<Token>>,
    next_token: Option<Spanned<Token>>,
    prev_span: Option<Span>,
    prefix_parsers: HashMap<TokenKind, &'a dyn PrefixParser>,
    infix_parsers: HashMap<TokenKind, &'a dyn InfixParser>,
    last_line_completed: bool,
}

impl<'a> Parser<'a> {
    fn new<I>(tokens: I, reporter: Reporter) -> Self
    where
        I: IntoIterator<Item = Spanned<Token>>,
    {
        let mut tokens = tokens.into_iter().collect::<VecDeque<_>>();
        let next_token = tokens.pop_front();
        Parser {
            reporter,
            tokens,
            next_token,
            prev_span: None,
            prefix_parsers: HashMap::new(),
            infix_parsers: HashMap::new(),
            expected: HashSet::new(),
            expected2: HashSet::new(),
            last_line_completed: false,
        }
    }

    fn infix<T: InfixParser + 'a>(&mut self, tok: TokenKind, parser: &'a T) {
        self.infix_parsers.insert(tok, parser);
    }

    fn prefix<T: PrefixParser + 'a>(&mut self, tok: TokenKind, parser: &'a T) {
        self.prefix_parsers.insert(tok, parser);
    }

    fn emit_error(&mut self, helper: Option<(Span, String)>) {
        if self.peek() == Some(&Token::Error) {
            // lexer should have already reported this
            return;
        }
        if self.expected.contains(&Expectation::Expression) {
            self.expected.retain(|e| match *e {
                Expectation::Token(ref tok) => !tok.can_start_expression(),
                _ => true,
            });
        }
        if self.expected.contains(&Expectation::Operator) {
            self.expected.retain(|e| match *e {
                Expectation::Token(ref tok) => !tok.is_operator(),
                _ => true,
            });
        }
        if self.expected.contains(&Expectation::Type) {
            self.expected.retain(|e| match *e {
                Expectation::Token(ref tok) => !tok.can_start_type(),
                _ => true,
            });
        }
        let mut expected = self
            .expected
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>();
        expected.sort();
        let got = self
            .peek()
            .cloned()
            .map(|t| TokenKind::Token(t).to_string())
            .unwrap_or_else(|| "end of input".into());
        let expected = match expected.len() {
            0 => panic!("no tokens expected"),
            1 => format!("expected {}", expected[0]),
            2 => format!("expected {} or {}", expected[0], expected[1]),
            _ => {
                let mut msg = "expected one of ".to_string();
                for (index, exp) in expected.iter().enumerate() {
                    if index > 0 {
                        msg.push_str(", ");
                    }
                    msg.push_str(exp);
                }
                msg
            }
        };
        let span = self.peek_span();
        let builder = self
            .reporter
            .error(format!("{}, got {}.", expected, got), span)
            .span_note(format!("unexpected {}", got), span);
        if let Some((span, msg)) = helper {
            builder.span_note(msg, span).build();
        } else if !self.last_line_completed
            && self.prev_span.is_some()
            && self.prev_span.unwrap().end.line < self.peek_span().start.line
        {
            let last_pos = self.prev_span.unwrap().end;
            let help_span = last_pos.forward(1).span_to(last_pos.forward(2));
            builder.span_note(expected, help_span).build();
        } else {
            builder.build();
        }
    }

    fn expect_semicolon(&mut self) -> ParseResult<()> {
        if self.check(Token::Semicolon) {
            return Ok(());
        }
        let (helper, res) = match (self.prev_span, &self.next_token) {
            (Some(span), &Some(ref tok)) => {
                let prev_line = span.end.line;
                let next_line = Spanned::span(tok).start.line;
                assert!(prev_line <= next_line);
                if next_line > prev_line {
                    // make specialized error about expected
                    // semicolon, and pretend that it exists
                    let help_span = Span::new(span.end, span.end.forward(1));
                    (Some((help_span, "maybe you missed a `;`?".into())), Ok(()))
                } else {
                    // regular error
                    (None, Err(()))
                }
            }
            _ => (None, Err(())),
        };
        self.emit_error(helper);
        res
    }

    fn expect_closing(&mut self, tok: Token, opener: Span) -> ParseResult<()> {
        if self.check(tok) {
            Ok(())
        } else {
            let helper = (opener, "unclosed delimiter".into());
            self.emit_error(Some(helper));
            Err(())
        }
    }

    fn previous_span(&self) -> Span {
        self.prev_span.expect("no previous token")
    }

    fn peek_span(&self) -> Span {
        match (self.next_token.as_ref(), self.prev_span) {
            (Some(tok), _) => Spanned::span(tok),
            (None, Some(span)) => {
                let start = span.end.forward(1);
                let end = start.forward(1);
                start.span_to(end)
            }
            (None, None) => Position::new(1, 1).span_to(Position::new(1, 2)),
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.next_token.as_ref().map(Spanned::value)
    }

    fn peek2(&self) -> Option<&Token> {
        self.tokens.front().map(Spanned::value)
    }

    fn check(&mut self, tok: Token) -> bool {
        self.expected.insert(Expectation::Token(tok.kind()));
        if self.peek() == Some(&tok) {
            self.consume().expect("token disappeared");
            true
        } else {
            false
        }
    }

    fn consume(&mut self) -> ParseResult<Spanned<Token>> {
        self.last_line_completed = false;
        match self.next_token.take() {
            Some(tok) => {
                self.expected = std::mem::take(&mut self.expected2);
                self.next_token = self.tokens.pop_front();
                self.prev_span = Some(Spanned::span(&tok));
                Ok(tok)
            }
            None => Err(()),
        }
    }

    fn expect(&mut self, tok: Token) -> ParseResult<()> {
        if self.check(tok) {
            Ok(())
        } else {
            self.emit_error(None);
            Err(())
        }
    }

    fn is_at_end(&self) -> bool {
        self.next_token.is_none()
    }

    fn check_ident(&mut self) -> Option<Spanned<Ident>> {
        self.expected.insert(Expectation::Token(TokenKind::Ident));
        match self.peek() {
            Some(&Token::Ident(_)) => {}
            _ => return None,
        }
        match self.consume() {
            Ok(tok) => {
                let span = Spanned::span(&tok);
                match Spanned::into_value(tok) {
                    Token::Ident(ident) => Some(Spanned::new(Ident(ident), span)),
                    _ => unreachable!(),
                }
            }
            _ => unreachable!(),
        }
    }

    fn consume_ident(&mut self) -> ParseResult<Spanned<Ident>> {
        if let Some(name) = self.check_ident() {
            Ok(name)
        } else {
            self.emit_error(None);
            Err(())
        }
    }

    fn synchronize_item(&mut self) {
        loop {
            match self.peek() {
                Some(&Token::Keyword(Keyword::Struct))
                | Some(&Token::Keyword(Keyword::Extern))
                | None => {
                    return;
                }
                // don't stop on fn type - also check that after fn goes an ident
                Some(&Token::Keyword(Keyword::Fn)) => {
                    if let Some(&Token::Ident(_)) = self.peek2() {
                        return;
                    }
                }
                _ => {}
            }
            self.consume().expect("token disappeared");
        }
    }

    fn synchronize_statement(&mut self) -> ParseResult<()> {
        loop {
            match self.peek() {
                Some(&Token::Keyword(Keyword::If))
                | Some(&Token::Keyword(Keyword::Loop))
                | Some(&Token::Keyword(Keyword::While))
                | Some(&Token::Keyword(Keyword::Break))
                | Some(&Token::Keyword(Keyword::Continue))
                | Some(&Token::Keyword(Keyword::Let))
                | Some(&Token::Keyword(Keyword::Return))
                | Some(&Token::LeftBrace)
                | Some(&Token::RightBrace) => {
                    return Ok(());
                }
                Some(&Token::Keyword(Keyword::Fn)) => {
                    if let Some(&Token::Ident(_)) = self.peek2() {
                        return Err(());
                    }
                }
                Some(&Token::Keyword(Keyword::Struct)) | None => {
                    return Err(());
                }
                _ => {}
            }
            if self.check(Token::Semicolon) {
                return Ok(());
            }
            self.consume().expect("token disappeared");
        }
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program {
            structs: Vec::new(),
            functions: Vec::new(),
            possible_structs: Vec::new(),
            possible_functions: Vec::new(),
        };
        loop {
            self.last_line_completed = true;
            if self.is_at_end() {
                return program;
            } else if self.check(Token::Keyword(Keyword::Struct)) {
                match self.parse_struct() {
                    PartialResult::Ok(s) => program.structs.push(s),
                    PartialResult::Partial(name) => {
                        program.possible_structs.push(name);
                        self.synchronize_item();
                    }
                    PartialResult::Error => {
                        self.synchronize_item();
                    }
                }
            } else if self.check(Token::Keyword(Keyword::Fn)) {
                let start_span = self.previous_span();
                match self.parse_function(start_span, FunctionType::Normal) {
                    PartialResult::Ok(f) => program.functions.push(f),
                    PartialResult::Partial(name) => {
                        program.possible_functions.push(name);
                        self.synchronize_item();
                    }
                    PartialResult::Error => {
                        self.synchronize_item();
                    }
                }
            } else if self.check(Token::Keyword(Keyword::Extern)) {
                let start_span = self.previous_span();
                if self.expect(Token::Keyword(Keyword::Fn)).is_err() {
                    self.synchronize_item();
                } else {
                    match self.parse_function(start_span, FunctionType::Extern) {
                        PartialResult::Ok(f) => program.functions.push(f),
                        PartialResult::Partial(name) => {
                            program.possible_functions.push(name);
                            self.synchronize_item();
                        }
                        PartialResult::Error => {
                            self.synchronize_item();
                        }
                    }
                }
            } else {
                self.emit_error(None);
                self.synchronize_item();
            }
        }
    }

    fn parse_struct(&mut self) -> PartialResult<Struct, Ident> {
        let start_span = self.previous_span();
        let name = match self.parse_item_name() {
            PartialResult::Ok(name) => name,
            PartialResult::Partial(name) => return PartialResult::Partial(name),
            PartialResult::Error => return PartialResult::Error,
        };
        match self.parse_struct_fields() {
            Ok(fields) => {
                let complete_span = start_span.merge(self.previous_span());
                PartialResult::Ok(Struct {
                    name,
                    fields,
                    complete_span,
                })
            }
            Err(()) => PartialResult::Partial(Spanned::into_value(name.name)),
        }
    }

    fn parse_struct_fields(&mut self) -> ParseResult<Vec<Field>> {
        self.expect(Token::LeftBrace)?;
        let mut fields = Vec::new();
        while !self.check(Token::RightBrace) {
            self.last_line_completed = true;
            let name = self.consume_ident()?;
            self.expect(Token::Colon)?;
            let typ = self.parse_type()?;
            fields.push(Field { name, typ });
            if self.check(Token::RightBrace) {
                break;
            }
            self.expect(Token::Comma)?;
        }
        Ok(fields)
    }

    fn parse_function(
        &mut self,
        start_span: Span,
        fn_type: FunctionType,
    ) -> PartialResult<Function, Ident> {
        let name = match self.parse_item_name() {
            PartialResult::Ok(name) => name,
            PartialResult::Partial(name) => return PartialResult::Partial(name),
            PartialResult::Error => return PartialResult::Error,
        };
        match self.parse_function_end() {
            Ok((params, return_type, body)) => {
                let complete_span = start_span.merge(self.previous_span());
                PartialResult::Ok(Function {
                    complete_span,
                    fn_type,
                    name,
                    params,
                    return_type,
                    body,
                })
            }
            Err(()) => PartialResult::Partial(Spanned::into_value(name.name)),
        }
    }

    fn parse_function_end(
        &mut self,
    ) -> ParseResult<(Vec<FnParam>, Spanned<Type>, Option<Spanned<Statement>>)> {
        self.expect(Token::LeftParen)?;
        let params = self.parse_function_params()?;
        let return_type = if self.check(Token::Arrow) {
            self.parse_type()?
        } else {
            Spanned::new(Type::Unit, self.previous_span())
        };
        let body = if self.check(Token::LeftBrace) {
            Some(self.parse_block()?)
        } else {
            self.expect_semicolon()?;
            None
        };
        Ok((params, return_type, body))
    }

    fn parse_function_params(&mut self) -> ParseResult<Vec<FnParam>> {
        let open_span = self.previous_span();
        let mut params = Vec::new();
        while !self.check(Token::RightParen) {
            let mutability = if self.check(Token::Keyword(Keyword::Mut)) {
                Mutability::Mut
            } else {
                Mutability::Const
            };
            let name = self.consume_ident()?;
            self.expect(Token::Colon)?;
            let typ = self.parse_type()?;
            params.push(FnParam {
                mutability,
                name,
                typ,
            });
            if self.check(Token::RightParen) {
                break;
            }
            self.expect_closing(Token::Comma, open_span)?;
        }
        Ok(params)
    }

    fn parse_item_name(&mut self) -> PartialResult<ItemName, Ident> {
        let name = match self.consume_ident() {
            Ok(ident) => ident,
            Err(()) => return PartialResult::Error,
        };
        match self.parse_generic_params() {
            Ok(type_params) => PartialResult::Ok(ItemName { name, type_params }),
            Err(()) => PartialResult::Partial(Spanned::into_value(name)),
        }
    }

    fn parse_generic_params(&mut self) -> ParseResult<Vec<Spanned<Ident>>> {
        if self.check(Token::Less) {
            let open_span = self.previous_span();
            let mut type_params = vec![self.consume_ident()?];
            while self.check(Token::Comma) {
                type_params.push(self.consume_ident()?);
            }
            self.expect_closing(Token::Greater, open_span)?;
            Ok(type_params)
        } else {
            Ok(Vec::new())
        }
    }

    fn parse_type(&mut self) -> ParseResult<Spanned<Type>> {
        self.expected.insert(Expectation::Type);
        if self.check(Token::Keyword(Keyword::Unit)) {
            let span = self.previous_span();
            let typ = Type::Unit;
            Ok(Spanned::new(typ, span))
        } else if self.check(Token::Star) {
            let start = self.previous_span();
            let mutability = if self.check(Token::Keyword(Keyword::Mut)) {
                Mutability::Mut
            } else {
                Mutability::Const
            };
            let typ = self.parse_type()?;
            let span = start.merge(Spanned::span(&typ));
            let typ = Type::Pointer(mutability, Box::new(typ));
            Ok(Spanned::new(typ, span))
        } else if self.check(Token::Keyword(Keyword::Fn)) {
            let start = self.previous_span();
            self.expect(Token::LeftParen)?;
            let open_span = self.previous_span();
            let mut param_types = Vec::new();
            while !self.check(Token::RightParen) {
                param_types.push(self.parse_type()?);
                if self.check(Token::RightParen) {
                    break;
                }
                self.expect_closing(Token::Comma, open_span)?;
            }
            let return_type = if self.check(Token::Arrow) {
                self.parse_type()?
            } else {
                Spanned::new(Type::Unit, self.previous_span())
            };
            let span = start.merge(Spanned::span(&return_type));
            let typ = Type::Function(param_types, Box::new(return_type));
            Ok(Spanned::new(typ, span))
        } else if self.check(Token::Underscore) {
            let span = self.previous_span();
            Ok(Spanned::new(Type::Wildcard, span))
        } else if self.check(Token::Keyword(Keyword::I8)) {
            let span = self.previous_span();
            Ok(Spanned::new(Type::I8, span))
        } else if self.check(Token::Keyword(Keyword::I16)) {
            let span = self.previous_span();
            Ok(Spanned::new(Type::I16, span))
        } else if self.check(Token::Keyword(Keyword::I32)) {
            let span = self.previous_span();
            Ok(Spanned::new(Type::I32, span))
        } else if self.check(Token::Keyword(Keyword::U8)) {
            let span = self.previous_span();
            Ok(Spanned::new(Type::U8, span))
        } else if self.check(Token::Keyword(Keyword::U16)) {
            let span = self.previous_span();
            Ok(Spanned::new(Type::U16, span))
        } else if self.check(Token::Keyword(Keyword::U32)) {
            let span = self.previous_span();
            Ok(Spanned::new(Type::U32, span))
        } else if self.check(Token::Keyword(Keyword::Bool)) {
            let span = self.previous_span();
            Ok(Spanned::new(Type::Bool, span))
        } else {
            let name = self.consume_ident()?;
            let params = if self.check(Token::Less) {
                let open_span = self.previous_span();
                let types = self.parse_type_params()?;
                self.expect_closing(Token::Greater, open_span)?;
                types
            } else {
                Vec::new()
            };
            let span = Spanned::span(&name).merge(self.previous_span());
            let typ = Type::Concrete(name, params);
            Ok(Spanned::new(typ, span))
        }
    }

    fn parse_type_params(&mut self) -> ParseResult<Vec<Spanned<Type>>> {
        let mut types = vec![self.parse_type()?];
        while self.check(Token::Comma) {
            types.push(self.parse_type()?);
        }
        Ok(types)
    }

    fn parse_statement(&mut self) -> PartialResult<Spanned<Statement>, Spanned<Statement>> {
        self.last_line_completed = true;
        if self.check(Token::Keyword(Keyword::Let)) {
            let start = self.previous_span();
            let mutability = if self.check(Token::Keyword(Keyword::Mut)) {
                Mutability::Mut
            } else {
                Mutability::Const
            };
            let name = match self.consume_ident() {
                Ok(ident) => ident,
                Err(()) => return PartialResult::Error,
            };
            match self.parse_let_end() {
                Ok((typ, value)) => {
                    let span = start.merge(self.previous_span());
                    let stmt = Statement::Let(mutability, name, typ, value);
                    PartialResult::Ok(Spanned::new(stmt, span))
                }
                Err(()) => {
                    let name_span = Spanned::span(&name);
                    let span = start.merge(name_span);
                    let typ = Spanned::new(Type::Error, name_span);
                    let value = Spanned::new(Expr::Error, name_span);
                    let stmt = Statement::Let(mutability, name, Some(typ), Some(value));
                    PartialResult::Partial(Spanned::new(stmt, span))
                }
            }
        } else {
            self.parse_non_let_stmt().into()
        }
    }

    fn parse_non_let_stmt(&mut self) -> ParseResult<Spanned<Statement>> {
        if self.check(Token::Keyword(Keyword::If)) {
            self.parse_if()
        } else if self.check(Token::Keyword(Keyword::Loop)) {
            let start = self.previous_span();
            self.expect(Token::LeftBrace)?;
            let body = self.parse_block()?;
            let span = start.merge(self.previous_span());
            let stmt = Statement::Loop(Box::new(body));
            Ok(Spanned::new(stmt, span))
        } else if self.check(Token::Keyword(Keyword::While)) {
            let start = self.previous_span();
            let cond = self.parse_expr()?;
            self.expect(Token::LeftBrace)?;
            let body = self.parse_block()?;
            let span = start.merge(self.previous_span());
            let stmt = Statement::While(cond, Box::new(body));
            Ok(Spanned::new(stmt, span))
        } else if self.check(Token::Keyword(Keyword::Break)) {
            let span = self.previous_span();
            self.expect_semicolon()?;
            Ok(Spanned::new(Statement::Break, span))
        } else if self.check(Token::Keyword(Keyword::Continue)) {
            let span = self.previous_span();
            self.expect_semicolon()?;
            Ok(Spanned::new(Statement::Continue, span))
        } else if self.check(Token::Keyword(Keyword::Return)) {
            let start = self.previous_span();
            let value = if self.check(Token::Semicolon) {
                Spanned::new(Expr::Literal(Literal::Unit), start)
            } else {
                let value = self.parse_expr()?;
                self.expect_semicolon()?;
                value
            };
            let span = start.merge(self.previous_span());
            Ok(Spanned::new(Statement::Return(value), span))
        } else if self.check(Token::LeftBrace) {
            self.parse_block().into()
        } else {
            let expr = self.parse_expr()?;
            self.expect_semicolon()?;
            let span = Spanned::span(&expr);
            let stmt = Statement::Expr(expr);
            Ok(Spanned::new(stmt, span))
        }
    }

    fn parse_let_end(&mut self) -> ParseResult<(Option<Spanned<Type>>, Option<Spanned<Expr>>)> {
        let typ = if self.check(Token::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };
        let value = if self.check(Token::Semicolon) {
            None
        } else {
            self.expect(Token::Assign)?;
            let value = self.parse_expr()?;
            self.expect_semicolon()?;
            Some(value)
        };
        Ok((typ, value))
    }

    fn parse_if(&mut self) -> ParseResult<Spanned<Statement>> {
        let start = self.previous_span();
        let cond = self.parse_expr()?;
        self.expect(Token::LeftBrace)?;
        let then = self.parse_block()?;
        let else_ = if self.check(Token::Keyword(Keyword::Else)) {
            if self.check(Token::Keyword(Keyword::If)) {
                Some(Box::new(self.parse_if()?))
            } else {
                self.expect(Token::LeftBrace)?;
                Some(Box::new(self.parse_block()?))
            }
        } else {
            None
        };
        let span = start.merge(self.previous_span());
        let stmt = Statement::If(cond, Box::new(then), else_);
        Ok(Spanned::new(stmt, span))
    }

    fn parse_block(&mut self) -> ParseResult<Spanned<Statement>> {
        let start = self.previous_span();
        let mut statements = Vec::new();
        while !self.check(Token::RightBrace) {
            let statement_start = self.previous_span();
            match self.parse_statement() {
                PartialResult::Ok(stmt) => statements.push(stmt),
                PartialResult::Partial(stmt) => {
                    statements.push(stmt);
                    self.synchronize_statement()?;
                }
                PartialResult::Error => {
                    let span = statement_start.merge(self.previous_span());
                    statements.push(Spanned::new(Statement::Error, span));
                    self.synchronize_statement()?;
                }
            }
        }
        let span = start.merge(self.previous_span());
        Ok(Spanned::new(Statement::Block(statements), span))
    }

    fn parse_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        self.pratt_parse(Precedence::Lowest)
    }

    fn pratt_parse(&mut self, prec: Precedence) -> ParseResult<Spanned<Expr>> {
        self.expected.insert(Expectation::Expression);
        let mut expr = self
            .peek()
            .map(|tok| tok.kind())
            .and_then(|tok| self.prefix_parsers.get(&tok).cloned())
            .ok_or_else(|| self.emit_error(None))?
            .parse(self)?;
        loop {
            self.expected.insert(Expectation::Operator);
            self.expected
                .extend(self.infix_parsers.keys().cloned().map(Expectation::Token));
            if prec >= self.next_precedence() {
                break;
            }
            let tok = self.peek().expect("token dissapeared").kind();
            let parser = self.infix_parsers[&tok];
            expr = parser.parse(self, expr)?;
        }
        Ok(expr)
    }

    fn next_precedence(&self) -> Precedence {
        self.peek()
            .map(|tok| tok.kind())
            .and_then(|tok| self.infix_parsers.get(&tok))
            .map(|p| p.precedence())
            .unwrap_or(Precedence::Lowest)
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
enum Precedence {
    Lowest,
    Assignment,
    Or,
    And,
    Equation,
    Comparision,
    Addition,
    Multiplication,
    Cast,
    Prefix,
    CallOrField,
}

impl Precedence {
    fn one_lower(self) -> Precedence {
        use self::Precedence::*;
        match self {
            Lowest | Assignment => Lowest,
            Or => Assignment,
            And => Or,
            Equation => And,
            Comparision => Equation,
            Addition => Comparision,
            Multiplication => Addition,
            Prefix => Multiplication,
            Cast => Prefix,
            CallOrField => Cast,
        }
    }
}

trait PrefixParser {
    fn parse(&self, _: &mut Parser) -> ParseResult<Spanned<Expr>>;
}

trait InfixParser {
    fn precedence(&self) -> Precedence;
    fn parse(&self, _: &mut Parser, lhs: Spanned<Expr>) -> ParseResult<Spanned<Expr>>;
}

struct BinaryOpParser {
    prec: Precedence,
    op: BinaryOp,
    left_assoc: bool,
}

impl InfixParser for BinaryOpParser {
    fn precedence(&self) -> Precedence {
        self.prec
    }

    fn parse(&self, parser: &mut Parser, lhs: Spanned<Expr>) -> ParseResult<Spanned<Expr>> {
        let op = parser.consume().expect("token disappeared");
        let op_span = Spanned::span(&op);
        let binop = Spanned::new(self.op, op_span);
        let rhs_prec = if self.left_assoc {
            self.prec
        } else {
            self.prec.one_lower()
        };
        let rhs = parser.pratt_parse(rhs_prec)?;
        let span = Spanned::span(&lhs).merge(Spanned::span(&rhs));
        let expr = Expr::Binary(Box::new(lhs), binop, Box::new(rhs));
        Ok(Spanned::new(expr, span))
    }
}

struct CallParser;

impl InfixParser for CallParser {
    fn precedence(&self) -> Precedence {
        Precedence::CallOrField
    }

    fn parse(&self, parser: &mut Parser, callee: Spanned<Expr>) -> ParseResult<Spanned<Expr>> {
        parser
            .expect(Token::LeftParen)
            .expect("expected left paren");
        let open_span = parser.previous_span();
        let mut params = Vec::new();
        while !parser.check(Token::RightParen) {
            let ident_next = parser.peek().map(Token::kind) == Some(TokenKind::Ident);
            if ident_next {
                parser
                    .expected2
                    .insert(Expectation::Token(TokenKind::Token(Token::Colon)));
            }
            if ident_next && parser.peek2() == Some(&Token::Colon) {
                let name = parser.consume_ident().expect("expected ident");
                parser.expect(Token::Colon).expect("expected ':'");
                let value = parser.parse_expr()?;
                params.push(CallParam::Named(name, value));
            } else {
                params.push(CallParam::Unnamed(parser.parse_expr()?));
            }
            if parser.check(Token::RightParen) {
                break;
            }
            parser.expect_closing(Token::Comma, open_span)?;
        }
        let span = Spanned::span(&callee).merge(parser.previous_span());
        let expr = Expr::Call(Box::new(callee), params);
        Ok(Spanned::new(expr, span))
    }
}

struct FieldParser;

impl InfixParser for FieldParser {
    fn precedence(&self) -> Precedence {
        Precedence::CallOrField
    }

    fn parse(&self, parser: &mut Parser, value: Spanned<Expr>) -> ParseResult<Spanned<Expr>> {
        parser.expect(Token::Dot).expect("expected dot");
        let field = parser.consume_ident()?;
        let span = Spanned::span(&value).merge(Spanned::span(&field));
        let expr = Expr::Field(Box::new(value), field);
        Ok(Spanned::new(expr, span))
    }
}

struct CastParser;

impl InfixParser for CastParser {
    fn precedence(&self) -> Precedence {
        Precedence::Cast
    }

    fn parse(&self, parser: &mut Parser, value: Spanned<Expr>) -> ParseResult<Spanned<Expr>> {
        parser
            .expect(Token::Keyword(Keyword::As))
            .expect("expected `as`");
        let typ = parser.parse_type()?;
        let span = Spanned::span(&value).merge(Spanned::span(&typ));
        let expr = Expr::Cast(Box::new(value), typ);
        Ok(Spanned::new(expr, span))
    }
}

struct NameParser;

impl PrefixParser for NameParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
        let ident = parser.consume_ident().expect("identifier disappeared");
        let type_params = if parser.check(Token::DoubleColon) {
            parser.expect(Token::Less)?;
            let open_span = parser.previous_span();
            let types = parser.parse_type_params()?;
            parser.expect_closing(Token::Greater, open_span)?;
            types
        } else {
            Vec::new()
        };
        let span = Spanned::span(&ident).merge(parser.previous_span());
        let expr = Expr::Name(ident, type_params);
        Ok(Spanned::new(expr, span))
    }
}

struct UnaryOpParser(UnaryOp);

impl PrefixParser for UnaryOpParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
        let op = parser.consume().expect("token disappeared");
        let op_span = Spanned::span(&op);
        let op = Spanned::new(self.0, op_span);
        let operand = parser.pratt_parse(Precedence::Prefix)?;
        let span = op_span.merge(Spanned::span(&operand));
        let expr = Expr::Unary(op, Box::new(operand));
        Ok(Spanned::new(expr, span))
    }
}

struct AddressOfParser;

impl PrefixParser for AddressOfParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
        let op = parser.consume().expect("token disappeared");
        let op = if parser.check(Token::Keyword(Keyword::Mut)) {
            let span = Spanned::span(&op).merge(parser.previous_span());
            Spanned::new(UnaryOp::MutAddressOf, span)
        } else {
            let span = Spanned::span(&op);
            Spanned::new(UnaryOp::AddressOf, span)
        };
        let operand = parser.pratt_parse(Precedence::Prefix)?;
        let span = Spanned::span(&op).merge(Spanned::span(&operand));
        let expr = Expr::Unary(op, Box::new(operand));
        Ok(Spanned::new(expr, span))
    }
}

struct LiteralParser;

impl PrefixParser for LiteralParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
        let tok = parser.consume().expect("token disappeared");
        let span = Spanned::span(&tok);
        let literal = match Spanned::into_value(tok) {
            Token::Number(n) => Literal::Number(n),
            Token::Bool(b) => Literal::Bool(b),
            Token::Char(c) => Literal::Char(c),
            Token::Str(s) => Literal::Str(s),
            Token::Keyword(Keyword::Unit) => Literal::Unit,
            _ => panic!("expected a literal"),
        };
        let expr = Expr::Literal(literal);
        Ok(Spanned::new(expr, span))
    }
}

struct ParenthesisedParser;

impl PrefixParser for ParenthesisedParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
        let tok = parser.consume().expect("token disappeared");
        let open_span = Spanned::span(&tok);
        let expr = parser.parse_expr()?;
        parser.expect_closing(Token::RightParen, open_span)?;
        Ok(expr)
    }
}
