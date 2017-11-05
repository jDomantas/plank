use std::fmt;
pub use ast::Number;


#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum Token {
    Ident(String),
    Number(Number),
    Bool(bool),
    Char(u8),
    Str(Vec<u8>),
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
    Not,
    Assign,
    Error,
}

impl Token {
    pub fn kind(&self) -> TokenKind {
        match *self {
            Token::Ident(_) => TokenKind::Ident,
            Token::Number(_) | Token::Bool(_) | Token::Char(_) | Token::Str(_) => {
                TokenKind::Literal
            }
            ref tok => TokenKind::Token(tok.clone()),
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Token::Ident(ref s) => write!(f, "{}", s),
            Token::Number(n) => write!(f, "{}", n),
            Token::Bool(b) => write!(f, "{}", b),
            Token::Char(_) => write!(f, "'...'"),
            Token::Str(_) => write!(f, "\"...\""),
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
            Token::Not => write!(f, "!"),
            Token::Error => write!(f, "?"),
        }
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
pub enum Keyword {
    Extern,
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
    As,
    Bool,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    Unit,
}

impl fmt::Display for Keyword {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Keyword::Extern => write!(f, "extern"),
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
            Keyword::As => write!(f, "as"),
            Keyword::I8 => write!(f, "i8"),
            Keyword::I16 => write!(f, "i16"),
            Keyword::I32 => write!(f, "i32"),
            Keyword::U8 => write!(f, "u8"),
            Keyword::U16 => write!(f, "u16"),
            Keyword::U32 => write!(f, "u32"),
            Keyword::Bool => write!(f, "bool"),
            Keyword::Unit => write!(f, "unit"),
        }
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Clone)]
pub enum TokenKind {
    Token(Token),
    Ident,
    Literal,
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
            TokenKind::Literal |
            TokenKind::Token(Token::Plus) |
            TokenKind::Token(Token::Minus) |
            TokenKind::Token(Token::Star) |
            TokenKind::Token(Token::Ampersand) |
            TokenKind::Token(Token::LeftParen) |
            TokenKind::Token(Token::Not) => true,
            _ => false,
        }
    }

    pub fn can_start_type(&self) -> bool {
        match *self {
            TokenKind::Token(Token::Keyword(Keyword::Unit)) |
            TokenKind::Token(Token::Star) |
            TokenKind::Token(Token::Keyword(Keyword::Fn)) |
            TokenKind::Token(Token::Underscore) |
            TokenKind::Token(Token::Keyword(Keyword::I8)) |
            TokenKind::Token(Token::Keyword(Keyword::I16)) |
            TokenKind::Token(Token::Keyword(Keyword::I32)) |
            TokenKind::Token(Token::Keyword(Keyword::U8)) |
            TokenKind::Token(Token::Keyword(Keyword::U16)) |
            TokenKind::Token(Token::Keyword(Keyword::U32)) |
            TokenKind::Token(Token::Keyword(Keyword::Bool)) |
            TokenKind::Ident => true,
            _ => false,
        }
    }
}

impl fmt::Display for TokenKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            TokenKind::Ident => write!(f, "identifier"),
            TokenKind::Literal => write!(f, "literal"),
            TokenKind::Token(ref tok) => write!(f, "`{}`", tok),
        }
    }
}
