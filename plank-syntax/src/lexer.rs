use std::str::Chars;
use plank_errors::Reporter;
use position::{Position, Span, Spanned};
use tokens::{Keyword, Number, Token};
use ast::{Signedness, Size};


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
            current_pos: Position::new(0, 0),
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
                self.current_pos.column = 0;
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

    fn consume_hex_digit(&mut self) -> Option<u8> {
        match self.peek() {
            Some(ch) if ch.is_digit(16) => self.consume().map(|c| c.to_digit(16).unwrap() as u8),
            _ => None,
        }
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
                .error(format!("unknown token: `{}`", first), span)
                .span_note(format!("maybe you wanted `{}{}`?", first, ch), span)
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
        let opener_span = Span::new(self.current_pos.backwards(2), self.current_pos);
        let mut depth = 1;
        while depth > 0 {
            match self.consume() {
                Some('*') => loop {
                    match self.consume() {
                        Some('/') => {
                            depth -= 1;
                            break;
                        }
                        Some('*') => {}
                        _ => break,
                    }
                },
                Some('/') => if self.consume() == Some('*') {
                    depth += 1;
                },
                None => {
                    self.reporter
                        .error("unterminated block comment", opener_span)
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
        let tok = match parse_number(&string) {
            Ok(num) => Token::Number(num),
            Err(err) => {
                let msg = err.to_string();
                self.reporter.error(msg, span).span(span).build();
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

    fn lex_string_raw(&mut self, closing: char) -> Spanned<Option<Vec<u8>>> {
        let mut string = Vec::new();
        let start_pos = self.current_pos;
        self.advance();
        loop {
            match self.peek() {
                Some(ch) if ch == closing => {
                    self.advance();
                    let span = start_pos.span_to(self.current_pos);
                    return Spanned::new(Some(string), span);
                }
                Some('\\') => {
                    let escape_start = self.current_pos;
                    self.advance();
                    match self.consume() {
                        Some('\\') => string.push(b'\\'),
                        Some('\'') => string.push(b'\''),
                        Some('"') => string.push(b'"'),
                        Some('n') => string.push(b'\n'),
                        Some('x') => {
                            let byte = self.consume_hex_digit().and_then(
                                |high| self.consume_hex_digit().map(|low| (high << 4) | low),
                            );
                            match byte {
                                Some(byte) => string.push(byte),
                                None => {
                                    let span =
                                        self.current_pos.span_to(self.current_pos.forward(1));
                                    self.reporter
                                        .error("`x` should be followed by two hex digits", span)
                                        .span(span)
                                        .build();
                                }
                            }
                        }
                        _ => {
                            let span = escape_start.span_to(self.current_pos);
                            self.reporter
                                .error("invalid escape sequence", span)
                                .span(span)
                                .build();
                        }
                    }
                }
                Some('\n') | Some('\r') | None => {
                    let span = start_pos.span_to(self.current_pos);
                    self.reporter
                        .error("unterminated string", span)
                        .span(span)
                        .build();
                    return Spanned::new(None, span);
                }
                Some(ch) => {
                    let ch = ch as u32;
                    if 32 <= ch && ch < 127 {
                        string.push(ch as u8);
                        self.advance();
                    } else {
                        let start = self.current_pos;
                        self.advance();
                        let span = start.span_to(self.current_pos);
                        self.reporter
                            .error(format!("unknown char in string (codepoint: {})", ch), span)
                            .span(span)
                            .build();
                    }
                }
            }
        }
    }

    fn lex_string(&mut self) -> Spanned<Token> {
        Spanned::map(self.lex_string_raw('"'), |t| {
            t.map(Token::Str).unwrap_or(Token::Error)
        })
    }

    fn lex_char(&mut self) -> Spanned<Token> {
        let tok = self.lex_string_raw('\'');
        let span = Spanned::span(&tok);
        if let Some(value) = Spanned::into_value(tok) {
            if value.len() != 1 {
                self.reporter
                    .error(
                        format!("char literal must have 1 char, but it has {}", value.len()),
                        span,
                    )
                    .span(span)
                    .build();
                Spanned::new(Token::Error, span)
            } else {
                Spanned::new(Token::Char(value[0]), span)
            }
        } else {
            Spanned::new(Token::Error, span)
        }
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
                Some('/') => if let Some(tok) = self.slash_or_comment() {
                    tok
                } else {
                    continue;
                },
                Some('"') => self.lex_string(),
                Some('\'') => self.lex_char(),
                Some(ch) => {
                    let tok = self.single_char(Token::Error);
                    if !self.previous_error {
                        let codepoint = ch as u32;
                        let msg = if 32 <= codepoint && codepoint < 127 {
                            format!("unknown char: `{}`", ch)
                        } else {
                            format!("unknown char (codepoint: {})", codepoint)
                        };
                        let span = Spanned::span(&tok);
                        self.reporter.error(msg, span).span(span).build();
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
                    return Some(tok);
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
        "as" => Some(Token::Keyword(Keyword::As)),
        "true" => Some(Token::Bool(true)),
        "false" => Some(Token::Bool(false)),
        "_" => Some(Token::Underscore),
        "i8" => Some(Token::Keyword(Keyword::I8)),
        "u8" => Some(Token::Keyword(Keyword::U8)),
        "i16" => Some(Token::Keyword(Keyword::I16)),
        "u16" => Some(Token::Keyword(Keyword::U16)),
        "i32" => Some(Token::Keyword(Keyword::I32)),
        "u32" => Some(Token::Keyword(Keyword::U32)),
        "bool" => Some(Token::Keyword(Keyword::Bool)),
        _ => None,
    }
}

enum ParseNumberError {
    BadInt,
    TooLarge,
    BadBitCount,
}

impl ::std::fmt::Display for ParseNumberError {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        let msg = match *self {
            ParseNumberError::BadInt => "invalid int literal",
            ParseNumberError::TooLarge => "int literal is too big",
            ParseNumberError::BadBitCount => "invalid int size",
        };
        write!(f, "{}", msg)
    }
}

fn parse_number(s: &str) -> Result<Number, ParseNumberError> {
    match s.find(|c| c == 'i' || c == 'u') {
        Some(index) => {
            // we have a suffix
            // parse prefix as number, parse rest as bit count
            let prefix = s.get(..index).unwrap();
            let suffix = s.get((index + 1)..).unwrap();
            let value = parse_simple_number(prefix)?;
            let signedness = if s.get(index..(index + 1)).unwrap() == "i" {
                Signedness::Signed
            } else {
                Signedness::Unsigned
            };
            let size = match suffix {
                "8" => Some(Size::Bit8),
                "16" => Some(Size::Bit16),
                "32" => Some(Size::Bit32),
                "" => None,
                _ => return Err(ParseNumberError::BadBitCount),
            };
            Ok(Number {
                value,
                signedness: Some(signedness),
                size,
            })
        }
        None => {
            // no suffix, parse simple number
            let value = parse_simple_number(s)?;
            Ok(Number {
                value,
                signedness: None,
                size: None,
            })
        }
    }
}

fn parse_simple_number(s: &str) -> Result<u64, ParseNumberError> {
    if s.is_empty() {
        return Err(ParseNumberError::BadInt);
    }
    if s.len() > 1 && s.starts_with('0') {
        // we have a leading zero, and number is longer than one digit
        return Err(ParseNumberError::BadInt);
    }
    let mut result = 0u64;
    for ch in s.chars() {
        match ch.to_digit(10) {
            Some(digit) => {
                result = result
                    .checked_mul(10)
                    .and_then(|n| n.checked_add(u64::from(digit)))
                    .ok_or(ParseNumberError::TooLarge)?;
            }
            None => {
                return Err(ParseNumberError::BadInt);
            }
        }
    }
    Ok(result)
}
