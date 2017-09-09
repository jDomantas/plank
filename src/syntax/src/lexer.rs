use std::str::Chars;
use errors::Reporter;
use position::{Position, Span, Spanned};
use tokens::{Token, Keyword};


pub fn lex(source: &str, reporter: Reporter) -> Vec<Spanned<Token>> {
    let mut lexer = Lexer::new(source, reporter);
    let mut tokens = Vec::new();
    while let Some(tok) = lexer.next_token() {
        tokens.push(tok);
    }
    tokens
}

struct Lexer<'a> {
    chars: Chars<'a>,
    next_char: Option<char>,
    current_pos: Position,
    reporter: Reporter,
    previous_error: bool,
}

impl<'a> Lexer<'a> {
    fn new(source: &'a str, reporter: Reporter) -> Self {
        let mut chars = source.chars();
        let next_char = chars.next();
        Lexer {
            chars,
            next_char,
            current_pos: Position::new(1, 1),
            reporter,
            previous_error: false,
        }
    }

    fn peek(&self) -> Option<char> {
        self.next_char
    }

    fn advance(&mut self) {
        match self.next_char {
            Some('\n') => {
                self.current_pos.line += 1;
                self.current_pos.column = 1;
            }
            Some(_) => {
                self.current_pos.column += 1;
            }
            None => {}
        }
        self.next_char = self.chars.next();
    }

    fn check(&mut self, ch: char) -> bool {
        if self.peek() == Some(ch) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn consume(&mut self) -> Option<char> {
        let result = self.peek();
        self.advance();
        result
    }

    fn single_char(&mut self, tok: Token) -> Spanned<Token> {
        let start = self.current_pos;
        self.advance();
        let span = start.span_to(self.current_pos);
        Spanned::new(tok, span)
    }

    fn two_char(&mut self, ch: char, tok: Token) -> Spanned<Token> {
        let start = self.current_pos;
        let first = self.consume().expect("char disappeared");
        let tok = if self.check(ch) {
            tok
        } else {
            let span = start.span_to(self.current_pos.forward(1));
            self.reporter
                .error(format!("unknown token: `{}`", first))
                .span_note(span, format!("maybe you wanted `{}{}`?", first, ch))
                .build();
            Token::Error
        };
        let span = start.span_to(self.current_pos);
        Spanned::new(tok, span)
    }

    fn test_second(&mut self, ch: char, if_one: Token, if_two: Token) -> Spanned<Token> {
        let start = self.current_pos;
        self.advance();
        let tok = if self.check(ch) { if_two } else { if_one };
        let span = start.span_to(self.current_pos);
        Spanned::new(tok, span)
    }

    fn skip_line_comment(&mut self) {
        loop {
            match self.consume() {
                Some('\n') | None => break,
                _ => {}
            }
        }
    }

    fn skip_block_comment(&mut self) {
        // make span of starting `/*`
        let opener_span = Span::new(
            self.current_pos.backwards(2),
            self.current_pos
        );
        let mut depth = 1;
        while depth > 0 {
            match self.consume() {
                Some('*') => {
                    if self.consume() == Some('/') {
                        depth -= 1;
                    }
                }
                Some('/') => {
                    if self.consume() == Some('*') {
                        depth += 1;
                    }
                }
                None => {
                    self.reporter
                        .error("unterminated block comment")
                        .span(opener_span)
                        .build();
                    break;
                }
                _ => {}
            }
        }
    }

    fn slash_or_comment(&mut self) -> Option<Spanned<Token>> {
        let start = self.current_pos;
        self.advance();
        if self.check('/') {
            self.skip_line_comment();
            None
        } else if self.check('*') {
            self.skip_block_comment();
            None
        } else {
            let span = start.span_to(self.current_pos);
            Some(Spanned::new(Token::Slash, span))
        }
    }

    fn lex_number(&mut self) -> Spanned<Token> {
        let mut string = String::new();
        let start = self.current_pos;
        loop {
            match self.peek() {
                Some(ch) if ch.is_alphanumeric() => {
                    string.push(ch);
                    self.advance();
                }
                _ => break,
            }
        }
        let span = start.span_to(self.current_pos);
        let tok = match str::parse::<u64>(&string) {
            Ok(int) => Token::Number(int),
            Err(_) => {
                // TODO: make error more precise
                self.reporter
                    .error("invalid int literal")
                    .span(span)
                    .build();
                Token::Error
            }
        };
        Spanned::new(tok, span)
    }

    fn lex_name(&mut self) -> Spanned<Token> {
        let mut string = String::new();
        let start = self.current_pos;
        loop {
            match self.peek() {
                Some(ch) if is_ident_char(ch) => {
                    string.push(ch);
                    self.advance();
                }
                _ => break,
            }
        }
        let span = start.span_to(self.current_pos);
        let tok = if let Some(tok) = keyword(&string) {
            tok
        } else {
            Token::Ident(string)
        };
        Spanned::new(tok, span)
    }

    fn next_raw_token(&mut self) -> Option<Spanned<Token>> {
        loop {
            return Some(match self.peek() {
                Some(' ') | Some('\t') | Some('\r') | Some('\n') => {
                    self.advance();
                    continue;
                }
                Some(ch) if ch.is_digit(10) => self.lex_number(),
                Some(ch) if is_ident_char(ch) => self.lex_name(),
                Some('.') => self.single_char(Token::Dot),
                Some(',') => self.single_char(Token::Comma),
                Some(';') => self.single_char(Token::Semicolon),
                Some('*') => self.single_char(Token::Star),
                Some('+') => self.single_char(Token::Plus),
                Some('%') => self.single_char(Token::Percent),
                Some('(') => self.single_char(Token::LeftParen),
                Some(')') => self.single_char(Token::RightParen),
                Some('{') => self.single_char(Token::LeftBrace),
                Some('}') => self.single_char(Token::RightBrace),
                Some('|') => self.two_char('|', Token::Or),
                Some('!') => self.test_second('=', Token::Not, Token::NotEqual),
                Some('&') => self.test_second('&', Token::Ampersand, Token::And),
                Some(':') => self.test_second(':', Token::Colon, Token::DoubleColon),
                Some('<') => self.test_second('=', Token::Less, Token::LessEqual),
                Some('>') => self.test_second('=', Token::Greater, Token::GreaterEqual),
                Some('-') => self.test_second('>', Token::Minus, Token::Arrow),
                Some('=') => self.test_second('=', Token::Assign, Token::Equal),
                Some('/') => {
                    if let Some(tok) = self.slash_or_comment() {
                        tok
                    } else {
                        continue;
                    }
                }
                Some('"') => unimplemented!("no string literals yet"),
                Some('\'') => unimplemented!("no char literals yet"),
                Some(ch) => {
                    let tok = self.single_char(Token::Error);
                    if !self.previous_error {
                        let codepoint = ch as u32;
                        let msg = if 32 <= codepoint && codepoint < 127 {
                            format!("unknown char: `{}`", ch)
                        } else {
                            format!("unknown char (codepoint: {})", codepoint)
                        };
                        self.reporter
                            .error(msg)
                            .span(Spanned::span(&tok))
                            .build();
                    }
                    tok
                }
                None => return None,
            });
        }
    }

    fn next_token(&mut self) -> Option<Spanned<Token>> {
        while let Some(tok) = self.next_raw_token() {
            if Spanned::value(&tok) == &Token::Error {
                // don't report two lexing errors in a row
                if !self.previous_error {
                    return Some(tok)
                }
            } else {
                self.previous_error = false;
                return Some(tok);
            }
        }
        None
    }
}

fn is_ident_char(ch: char) -> bool {
    // TODO: grammar allows only /[a-zA-Z0-9_]/,
    // but here we allow funky unicode stuff
    ch.is_alphanumeric() || ch == '_'
}

fn keyword(s: &str) -> Option<Token> {
    match s {
        "extern" => Some(Token::Keyword(Keyword::Extern)),
        "fn" => Some(Token::Keyword(Keyword::Fn)),
        "struct" => Some(Token::Keyword(Keyword::Struct)),
        "if" => Some(Token::Keyword(Keyword::If)),
        "else" => Some(Token::Keyword(Keyword::Else)),
        "loop" => Some(Token::Keyword(Keyword::Loop)),
        "while" => Some(Token::Keyword(Keyword::While)),
        "continue" => Some(Token::Keyword(Keyword::Continue)),
        "break" => Some(Token::Keyword(Keyword::Break)),
        "let" => Some(Token::Keyword(Keyword::Let)),
        "return" => Some(Token::Keyword(Keyword::Return)),
        "true" => Some(Token::Bool(true)),
        "false" => Some(Token::Bool(false)),
        "_" => Some(Token::Underscore),
        _ => None,
    }
}
