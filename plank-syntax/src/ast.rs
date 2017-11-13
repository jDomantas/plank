use position::{Span, Spanned};


#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Clone)]
pub struct Ident(pub String);

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Literal {
    Number(Number),
    Bool(bool),
    Char(u8),
    Str(Vec<u8>),
    Unit,
}

#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone)]
pub struct Number {
    pub value: u64,
    pub signedness: Option<Signedness>,
    pub size: Option<Size>,
}

impl ::std::fmt::Display for Number {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}", self.value)?;
        match self.signedness {
            Some(Signedness::Signed) => write!(f, "i")?,
            Some(Signedness::Unsigned) => write!(f, "u")?,
            None => {}
        }
        match self.size {
            Some(Size::Bit8) => write!(f, "8")?,
            Some(Size::Bit16) => write!(f, "16")?,
            Some(Size::Bit32) => write!(f, "32")?,
            None => {}
        }
        Ok(())
    }
}

#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone)]
pub enum Signedness {
    Signed,
    Unsigned,
}

#[derive(PartialEq, Eq, Hash, Debug, Copy, Clone)]
pub enum Size {
    Bit8,
    Bit16,
    Bit32,
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Box<Spanned<Expr>>, Spanned<BinaryOp>, Box<Spanned<Expr>>),
    Unary(Spanned<UnaryOp>, Box<Spanned<Expr>>),
    Call(Box<Spanned<Expr>>, Vec<CallParam>),
    Field(Box<Spanned<Expr>>, Spanned<Ident>),
    Name(Spanned<Ident>, Vec<Spanned<Type>>),
    Literal(Literal),
    Cast(Box<Spanned<Expr>>, Spanned<Type>),
    Error,
}

#[derive(Debug, Clone)]
pub enum CallParam {
    Named(Spanned<Ident>, Spanned<Expr>),
    Unnamed(Spanned<Expr>),
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    Equal,
    NotEqual,
    Less,
    LessEqual,
    Greater,
    GreaterEqual,
    And,
    Or,
    Assign,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
    Deref,
    AddressOf,
    MutAddressOf,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Copy, Clone)]
pub enum Mutability {
    Mut,
    Const,
}

#[derive(Debug, Clone)]
pub enum Statement {
    If(
        Spanned<Expr>,
        Box<Spanned<Statement>>,
        Option<Box<Spanned<Statement>>>,
    ),
    Loop(Box<Spanned<Statement>>),
    While(Spanned<Expr>, Box<Spanned<Statement>>),
    Break,
    Continue,
    Return(Spanned<Expr>),
    Let(Mutability, Spanned<Ident>, Option<Spanned<Type>>, Option<Spanned<Expr>>),
    Block(Vec<Spanned<Statement>>),
    Expr(Spanned<Expr>),
    Error,
}

#[derive(Debug, Clone)]
pub enum Type {
    Wildcard,
    I8,
    U8,
    I16,
    U16,
    I32,
    U32,
    Bool,
    Unit,
    Concrete(Spanned<Ident>, Vec<Spanned<Type>>),
    Pointer(Mutability, Box<Spanned<Type>>),
    Function(Vec<Spanned<Type>>, Box<Spanned<Type>>),
    Error,
}

#[derive(Debug, Clone)]
pub struct ItemName {
    pub name: Spanned<Ident>,
    pub type_params: Vec<Spanned<Ident>>,
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
pub enum FunctionType {
    Normal,
    Extern,
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub mutability: Mutability,
    pub name: Spanned<Ident>,
    pub typ: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub complete_span: Span,
    pub fn_type: FunctionType,
    pub name: ItemName,
    pub params: Vec<FnParam>,
    pub return_type: Spanned<Type>,
    pub body: Option<Spanned<Statement>>,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Spanned<Ident>,
    pub typ: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub complete_span: Span,
    pub name: ItemName,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub structs: Vec<Struct>,
    pub functions: Vec<Function>,
    pub possible_structs: Vec<Ident>,
    pub possible_functions: Vec<Ident>,
}
