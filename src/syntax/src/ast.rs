use position::Spanned;


#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Clone)]
pub struct Ident(pub String);

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
pub enum Literal {
    Number(u64),
    Bool(bool),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Binary(Box<Spanned<Expr>>, Spanned<BinaryOp>, Box<Spanned<Expr>>),
    Unary(Spanned<UnaryOp>, Box<Spanned<Expr>>),
    Call(Box<Spanned<Expr>>, Vec<CallParam>),
    Field(Box<Spanned<Expr>>, Spanned<Ident>),
    Name(Spanned<Ident>, Vec<Spanned<Type>>),
    Literal(Literal),
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
}

#[derive(Debug, Clone)]
pub enum Statement {
    If(Spanned<Expr>, Box<Spanned<Statement>>, Option<Box<Spanned<Statement>>>),
    Loop(Box<Spanned<Statement>>),
    While(Spanned<Expr>, Box<Spanned<Statement>>),
    Break,
    Continue,
    Return(Spanned<Expr>),
    Let(Spanned<Ident>, Option<Spanned<Type>>, Spanned<Expr>),
    Block(Vec<Spanned<Statement>>),
    Expr(Spanned<Expr>),
}

#[derive(Debug, Clone)]
pub enum Type {
    Wildcard,
    Concrete(Spanned<Ident>, Vec<Spanned<Type>>),
    Pointer(Box<Spanned<Type>>),
    Function(Vec<Spanned<Type>>, Box<Spanned<Type>>),
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
pub struct Function {
    pub fn_type: FunctionType,
    pub name: ItemName,
    pub params: Vec<Var>,
    pub return_type: Spanned<Type>,
    pub body: Option<Spanned<Statement>>,
}

#[derive(Debug, Clone)]
pub struct Var {
    pub name: Spanned<Ident>,
    pub typ: Spanned<Type>,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub name: ItemName,
    pub fields: Vec<Var>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub structs: Vec<Struct>,
    pub functions: Vec<Function>,
}
