use std::collections::{HashMap, HashSet, VecDeque};
use errors::Reporter;
use ast::{
    BinaryOp, Expr, Function, Ident, ItemName, Literal, Program, Statement,
    Struct, UnaryOp, Type, Var,
};
use position::{Position, Span, Spanned};
use tokens::{Keyword, Token, TokenKind};


macro_rules! parse_infix {
    ($parser:expr, $tok:ident, $op:ident, $prec:ident, $left_assoc:expr) => {{
        $parser.infix(TokenKind::Token(Token::$tok), &BinaryOpParser {
            prec: Precedence::$prec,
            op: BinaryOp::$op,
            left_assoc: $left_assoc,
        });
    }}
}

pub fn parse(tokens: Vec<Spanned<Token>>, reporter: Reporter) -> Program {
    let mut parser = Parser::new(tokens, reporter);

    parser.prefix(TokenKind::Bool, &LiteralParser);
    parser.prefix(TokenKind::Number, &LiteralParser);
    parser.prefix(TokenKind::Ident, &NameParser);
    parser.prefix(TokenKind::Token(Token::Ampersand), &UnaryOpParser(UnaryOp::AddressOf));
    parser.prefix(TokenKind::Token(Token::Plus), &UnaryOpParser(UnaryOp::Plus));
    parser.prefix(TokenKind::Token(Token::Minus), &UnaryOpParser(UnaryOp::Minus));
    parser.prefix(TokenKind::Token(Token::Star), &UnaryOpParser(UnaryOp::Deref));
    parser.prefix(TokenKind::Token(Token::Not), &UnaryOpParser(UnaryOp::Not));
    parser.prefix(TokenKind::Token(Token::LeftParen), &ParenthesisedParser);

    parser.infix(TokenKind::Token(Token::LeftParen), &CallParser);
    parser.infix(TokenKind::Token(Token::Dot), &CallParser);

    parse_infix!(parser, And,           And,            And,            true);
    parse_infix!(parser, Or,            Or,             Or,             true);
    parse_infix!(parser, Plus,          Add,            Addition,       true);
    parse_infix!(parser, Minus,         Subtract,       Addition,       true);
    parse_infix!(parser, Star,          Multiply,       Multiplication, true);
    parse_infix!(parser, Slash,         Divide,         Multiplication, true);
    parse_infix!(parser, Percent,       Modulo,         Multiplication, true);
    parse_infix!(parser, Less,          Less,           Comparision,    true);
    parse_infix!(parser, LessEqual,     LessEqual,      Comparision,    true);
    parse_infix!(parser, Greater,       Greater,        Comparision,    true);
    parse_infix!(parser, GreaterEqual,  GreaterEqual,   Comparision,    true);
    parse_infix!(parser, Equal,         Equal,          Equation,       true);
    parse_infix!(parser, NotEqual,      NotEqual,       Equation,       true);
    parse_infix!(parser, Assign,        Assign,         Assignment,     false);

    parser.parse_program()
}

type ParseResult<T> = Result<T, ()>;

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
enum Expectation {
    Expression,
    Operator,
    Token(TokenKind),
}

impl ::std::fmt::Display for Expectation {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        match *self {
            Expectation::Operator => write!(f, "operator"),
            Expectation::Expression => write!(f, "expression"),
            Expectation::Token(ref tok) => write!(f, "{}", tok),
        }
    }
}

struct Parser<'a> {
    reporter: Reporter,
    expected: HashSet<Expectation>,
    tokens: VecDeque<Spanned<Token>>,
    next_token: Option<Spanned<Token>>,
    prev_span: Option<Span>,
    prefix_parsers: HashMap<TokenKind, &'a PrefixParser>,
    infix_parsers: HashMap<TokenKind, &'a InfixParser>,
}

impl<'a> Parser<'a> {
    fn new<I>(tokens: I, reporter: Reporter) -> Self
        where I: IntoIterator<Item=Spanned<Token>>
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
        }
    }

    fn infix<T: InfixParser + 'a>(&mut self, tok: TokenKind, parser: &'a T) {
        self.infix_parsers.insert(tok, parser);
    }

    fn prefix<T: PrefixParser + 'a>(&mut self, tok: TokenKind, parser: &'a T) {
        self.prefix_parsers.insert(tok, parser);
    }

    fn emit_error(&mut self) {
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
        let mut expected = self.expected
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>();
        expected.sort();
        let got = self
            .peek()
            .cloned()
            .map(|t| TokenKind::Token(t).to_string())
            .unwrap_or_else(|| "end of input".into());
        let msg = match expected.len() {
            0 => format!("expected nothing"),// panic!("no tokens expected"),
            1 => format!("expected {}, got {}.", expected[0], got),
            2 => format!("expected {} or {}, got {}.", expected[0], expected[1], got),
            _ => {
                let mut msg = "expected one of ".to_string();
                for exp in &expected {
                    msg.push_str(exp);
                    msg.push_str(", ");
                }
                msg.push_str("got ");
                msg.push_str(&got);
                msg.push('.');
                msg
            }
        };
        self.reporter
            .error(msg)
            .span(self.peek_span())
            .build();
    }

    fn previous_span(&self) -> Span {
        self.prev_span.expect("no previous token")
    }

    fn peek_span(&self) -> Span {
        match (self.next_token.as_ref(), self.prev_span) {
            (Some(tok), _) => {
                Spanned::span(tok)
            }
            (None, Some(span)) => {
                let start = span.end.forward(1);
                let end = start.forward(1);
                start.span_to(end)
            }
            (None, None) => {
                Position::new(1, 1).span_to(Position::new(1, 2))
            }
        }
    }

    fn peek(&self) -> Option<&Token> {
        self.next_token.as_ref().map(Spanned::value)
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
        match self.next_token.take() {
            Some(tok) => {
                self.expected.clear();
                self.next_token = self.tokens.pop_front();
                self.prev_span = Some(Spanned::span(&tok));
                Ok(tok)
            }
            None => {
                Err(())
            }
        }
    }

    fn expect(&mut self, tok: Token) -> ParseResult<()> {
        if self.check(tok) {
            Ok(())
        } else {
            self.emit_error();
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
                    Token::Ident(ident) => {
                        Some(Spanned::new(Ident(ident), span))
                    }
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
            self.emit_error();
            Err(())
        }
    }

    fn parse_program(&mut self) -> Program {
        let mut program = Program {
            structs: Vec::new(),
            functions: Vec::new(),
        };
        loop {
            if self.is_at_end() {
                return program;
            } else if self.check(Token::Keyword(Keyword::Struct)) {
                if let Ok(s) = self.parse_struct() {
                    program.structs.push(s);
                } else {
                    return program;
                }
            } else if self.check(Token::Keyword(Keyword::Fn)) {
                if let Ok(f) = self.parse_function() {
                    program.functions.push(f);
                } else {
                    return program;
                }
            } else {
                self.emit_error();
                return program;
            }
        }
    }

    fn parse_struct(&mut self) -> ParseResult<Struct> {
        let name = self.parse_item_name()?;
        self.expect(Token::LeftBrace)?;
        let mut fields = Vec::new();
        while !self.check(Token::RightBrace) {
            let name = self.consume_ident()?;
            self.expect(Token::Colon)?;
            let typ = self.parse_type()?;
            self.expect(Token::Comma)?;
            fields.push(Var { name, typ });
        }
        Ok(Struct { name, fields })
    }

    fn parse_function(&mut self) -> ParseResult<Function> {
        let name = self.parse_item_name()?;
        self.expect(Token::LeftParen)?;
        let params = if self.check(Token::RightParen) {
            Vec::new()
        } else {
            let params = self.parse_function_params()?;
            self.expect(Token::RightParen)?;
            params
        };
        self.expect(Token::Arrow)?;
        let return_type = self.parse_type()?;
        self.expect(Token::LeftBrace)?;
        let body = self.parse_block()?;
        Ok(Function {
            name,
            params,
            return_type,
            body,
        })
    }

    fn parse_function_params(&mut self) -> ParseResult<Vec<Var>> {
        let mut params = Vec::new();
        let name = self.consume_ident()?;
        self.expect(Token::Colon)?;
        let typ = self.parse_type()?;
        params.push(Var { name, typ });
        while self.check(Token::Comma) {
            let name = self.consume_ident()?;
            self.expect(Token::Colon)?;
            let typ = self.parse_type()?;
            params.push(Var { name, typ });
        }
        Ok(params)
    }

    fn parse_item_name(&mut self) -> ParseResult<ItemName> {
        let name = self.consume_ident()?;
        let type_params = if self.check(Token::Less) {
            let mut type_params = Vec::new();
            type_params.push(self.consume_ident()?);
            while self.check(Token::Comma) {
                type_params.push(self.consume_ident()?);
            }
            self.expect(Token::Greater)?;
            type_params
        } else {
            Vec::new()
        };
        Ok(ItemName {
            name,
            type_params,
        })
    }

    fn parse_type(&mut self) -> ParseResult<Spanned<Type>> {
        if self.check(Token::Star) {
            let start = self.previous_span();
            let typ = self.parse_type()?;
            let span = start.merge(Spanned::span(&typ));
            let typ = Type::Pointer(Box::new(typ));
            Ok(Spanned::new(typ, span))
        } else if self.check(Token::Keyword(Keyword::Fn)) {
            let start = self.previous_span();
            self.expect(Token::LeftParen)?;
            let param_types = if self.check(Token::RightParen) {
                Vec::new()
            } else {
                let types = self.parse_type_list()?;
                self.expect(Token::RightParen)?;
                types
            };
            self.expect(Token::Arrow)?;
            let return_type = self.parse_type()?;
            let span = start.merge(Spanned::span(&return_type));
            let typ = Type::Function(param_types, Box::new(return_type));
            Ok(Spanned::new(typ, span))
        } else if self.check(Token::Underscore) {
            let span = self.previous_span();
            Ok(Spanned::new(Type::Wildcard, span))
        } else {
            let name = self.consume_ident()?;
            let params = if self.check(Token::Less) {
                self.parse_type_list()?
            } else {
                Vec::new()
            };
            // TODO: make this nicer?
            let span = Spanned::span(&name).merge(self.previous_span());
            let typ = Type::Concrete(name, params);
            Ok(Spanned::new(typ, span))
        }
    }

    fn parse_type_list(&mut self) -> ParseResult<Vec<Spanned<Type>>> {
        let mut types = Vec::new();
        types.push(self.parse_type()?);
        while self.check(Token::Comma) {
            types.push(self.parse_type()?);
        }
        Ok(types)
    }

    fn parse_statement(&mut self) -> ParseResult<Spanned<Statement>> {
        if self.check(Token::Keyword(Keyword::If)) {
            let start = self.previous_span();
            let cond = self.parse_expr()?;
            self.expect(Token::LeftBrace)?;
            let then = self.parse_block()?;
            let else_ = if self.check(Token::Keyword(Keyword::Else)) {
                self.expect(Token::LeftBrace)?;
                Some(Box::new(self.parse_block()?))
            } else {
                None
            };
            let span = start.merge(self.previous_span());
            let stmt = Statement::If(cond, Box::new(then), else_);
            Ok(Spanned::new(stmt, span))
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
            self.expect(Token::Semicolon)?;
            Ok(Spanned::new(Statement::Break, span))
        } else if self.check(Token::Keyword(Keyword::Continue)) {
            let span = self.previous_span();
            self.expect(Token::Semicolon)?;
            Ok(Spanned::new(Statement::Continue, span))
        } else if self.check(Token::Keyword(Keyword::Return)) {
            let start = self.previous_span();
            let value = self.parse_expr()?;
            self.expect(Token::Semicolon)?;
            let span = start.merge(self.previous_span());
            Ok(Spanned::new(Statement::Return(value), span))
        } else if self.check(Token::Keyword(Keyword::Let)) {
            let start = self.previous_span();
            let name = self.consume_ident()?;
            let typ = if self.check(Token::Colon) {
                Some(self.parse_type()?)
            } else {
                None
            };
            self.expect(Token::Assign)?;
            let value = self.parse_expr()?;
            self.expect(Token::Semicolon)?;
            let span = start.merge(self.previous_span());
            let stmt = Statement::Let(name, typ, value);
            Ok(Spanned::new(stmt, span))
        } else {
            let expr = self.parse_expr()?;
            self.expect(Token::Semicolon)?;
            let span = Spanned::span(&expr);
            let stmt = Statement::Expr(expr);
            Ok(Spanned::new(stmt, span))
        }
    }

    fn parse_block(&mut self) -> ParseResult<Spanned<Statement>> {
        let start = self.previous_span();
        let mut statements = Vec::new();
        while !self.check(Token::RightBrace) {
            statements.push(self.parse_statement()?);
        }
        let span = start.merge(self.previous_span());
        Ok(Spanned::new(Statement::Block(statements), span))
    }

    fn parse_expr(&mut self) -> ParseResult<Spanned<Expr>> {
        self.pratt_parse(Precedence::Lowest)
    }

    fn pratt_parse(&mut self, prec: Precedence) -> ParseResult<Spanned<Expr>> {
        self.expected.insert(Expectation::Expression);
        let mut expr = self.peek()
            .map(|tok| tok.kind())
            .and_then(|tok| self.prefix_parsers.get(&tok).cloned())
            .ok_or_else(|| self.emit_error())?
            .parse(self)?;
        loop {
            self.expected.insert(Expectation::Operator);
            self.expected.extend(self.infix_parsers
                .keys()
                .cloned()
                .map(Expectation::Token));
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
            CallOrField => Prefix,
        }
    }
}

trait PrefixParser {
    fn parse(&self, &mut Parser) -> ParseResult<Spanned<Expr>>;
}

trait InfixParser {
    fn precedence(&self) -> Precedence;
    fn parse(&self, &mut Parser, lhs: Spanned<Expr>) -> ParseResult<Spanned<Expr>>;
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
            self.prec.one_lower()
        } else {
            self.prec
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
        if parser.check(Token::LeftParen) {
            parser.consume().expect("token disappeared");
            let mut args = Vec::new();
            if !parser.check(Token::RightParen) {
                args.push(parser.parse_expr()?);
                while parser.check(Token::Comma) {
                    args.push(parser.parse_expr()?);
                }
                parser.expect(Token::RightParen)?;
            }
            let span = Spanned::span(&callee).merge(parser.previous_span());
            let expr = Expr::Call(Box::new(callee), args);
            Ok(Spanned::new(expr, span))
        } else if parser.check(Token::Dot) {
            let field = parser.consume_ident()?;
            let span = Spanned::span(&callee).merge(Spanned::span(&field));
            let expr = Expr::Field(Box::new(callee), field);
            Ok(Spanned::new(expr, span))
        } else {
            panic!("expected dot or left paren")
        }
    }
}

struct NameParser;

impl PrefixParser for NameParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
        let ident = parser.consume_ident().expect("identifier disappeared");
        let type_params = if parser.check(Token::DoubleColon) {
            parser.expect(Token::Less)?;
            let types = parser.parse_type_list()?;
            parser.expect(Token::Greater)?;
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

struct LiteralParser;

impl PrefixParser for LiteralParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
        let tok = parser.consume().expect("token disappeared");
        let span = Spanned::span(&tok);
        let literal = match Spanned::into_value(tok) {
            Token::Number(n) => Literal::Number(n),
            Token::Bool(b) => Literal::Bool(b),
            _ => panic!("expected a literal"),
        };
        let expr = Expr::Literal(literal);
        Ok(Spanned::new(expr, span))
    }
}

struct ParenthesisedParser;

impl PrefixParser for ParenthesisedParser {
    fn parse(&self, parser: &mut Parser) -> ParseResult<Spanned<Expr>> {
        parser.consume().expect("token disappeared");
        let expr = parser.parse_expr()?;
        parser.expect(Token::RightParen)?;
        Ok(expr)
    }
}
