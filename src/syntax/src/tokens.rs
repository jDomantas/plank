use std::fmt;


#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum Token {
    Ident(String),
    Number(u64),
    Bool(bool),
    Keyword(Keyword),
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    Equal,
    NotEqual,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Ampersand,
    Dot,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Arrow,
    And,
    Or,
    Comma,
    Underscore,
    Colon,
    DoubleColon,
    Semicolon,
    Assign,
    Error,
}

impl Token {
    pub fn kind(&self) -> TokenKind {
        match self {
            &Token::Ident(_) => TokenKind::Ident,
            &Token::Number(_) => TokenKind::Number,
            &Token::Bool(_) => TokenKind::Bool,
            tok => TokenKind::Token(tok.clone()),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Ident(ref s) => write!(f, "{}", s),
            Token::Number(n) => write!(f, "{}", n),
            Token::Bool(b) => write!(f, "{}", b),
            Token::Keyword(k) => write!(f, "{}", k),
            Token::Less => write!(f, "<"),
            Token::LessEqual => write!(f, "<="),
            Token::Greater => write!(f, ">"),
            Token::GreaterEqual => write!(f, ">="),
            Token::Equal => write!(f, "=="),
            Token::NotEqual => write!(f, "!="),
            Token::Plus => write!(f, "+"),
            Token::Minus => write!(f, "-"),
            Token::Star => write!(f, "*"),
            Token::Slash => write!(f, "/"),
            Token::Percent => write!(f, "%"),
            Token::Ampersand => write!(f, "&"),
            Token::Dot => write!(f, "."),
            Token::LeftParen => write!(f, "("),
            Token::RightParen => write!(f, ")"),
            Token::LeftBrace => write!(f, "{{"),
            Token::RightBrace => write!(f, "}}"),
            Token::Arrow => write!(f, "->"),
            Token::And => write!(f, "&&"),
            Token::Or => write!(f, "||"),
            Token::Comma => write!(f, ","),
            Token::Underscore => write!(f, "_"),
            Token::Colon => write!(f, ":"),
            Token::DoubleColon => write!(f, "::"),
            Token::Semicolon => write!(f, ";"),
            Token::Assign => write!(f, "="),
            Token::Error => write!(f, "?"),
        }
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
pub enum Keyword {
    Fn,
    Struct,
    If,
    Else,
    Loop,
    While,
    Continue,
    Break,
    Let,
    Return,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Keyword::Fn => write!(f, "fn"),
            Keyword::Struct => write!(f, "struct"),
            Keyword::If => write!(f, "if"),
            Keyword::Else => write!(f, "else"),
            Keyword::Loop => write!(f, "loop"),
            Keyword::While => write!(f, "while"),
            Keyword::Continue => write!(f, "continue"),
            Keyword::Break => write!(f, "break"),
            Keyword::Let => write!(f, "let"),
            Keyword::Return => write!(f, "return"),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum TokenKind {
    Token(Token),
    Ident,
    Number,
    Bool,
}

impl TokenKind {
    pub fn is_operator(&self) -> bool {
        match *self {
            TokenKind::Token(Token::Less) |
            TokenKind::Token(Token::LessEqual) |
            TokenKind::Token(Token::Greater) |
            TokenKind::Token(Token::GreaterEqual) |
            TokenKind::Token(Token::Equal) |
            TokenKind::Token(Token::NotEqual) |
            TokenKind::Token(Token::Plus) |
            TokenKind::Token(Token::Minus) |
            TokenKind::Token(Token::Star) |
            TokenKind::Token(Token::Slash) |
            TokenKind::Token(Token::Percent) |
            TokenKind::Token(Token::And) |
            TokenKind::Token(Token::Or) |
            TokenKind::Token(Token::Assign) => true,
            _ => false,
        }
    }

    pub fn can_start_expression(&self) -> bool {
        match *self {
            TokenKind::Ident |
            TokenKind::Number |
            TokenKind::Bool |
            TokenKind::Token(Token::Plus) |
            TokenKind::Token(Token::Minus) |
            TokenKind::Token(Token::Star) |
            TokenKind::Token(Token::Ampersand) |
            TokenKind::Token(Token::LeftParen) => true,
            _ => false,
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenKind::Ident => write!(f, "identifier"),
            TokenKind::Bool => write!(f, "bool"),
            TokenKind::Number => write!(f, "int"),
            TokenKind::Token(ref tok) => write!(f, "`{}`", tok),
        }
    }
}
