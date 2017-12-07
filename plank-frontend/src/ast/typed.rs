use std::collections::HashMap;
use std::rc::Rc;
pub use plank_syntax::ast::{BinaryOp, FunctionType, Literal, Number, Signedness, Size, UnaryOp};
use plank_syntax::position::{Span, Spanned};
pub use ast::resolved::{Mutability, Symbol};


#[derive(Debug, Clone)]
pub enum Expr {
    Binary(TypedExpr, Spanned<BinaryOp>, TypedExpr),
    Unary(Spanned<UnaryOp>, TypedExpr),
    Call(TypedExpr, Vec<TypedExpr>),
    Field(TypedExpr, Spanned<usize>),
    Name(Spanned<Symbol>, Vec<Spanned<Type>>),
    Literal(Literal),
    Cast(TypedExpr, Spanned<Type>),
    Error,
}

#[derive(Debug, Clone)]
pub struct TypedExpr {
    pub expr: Box<Expr>,
    pub span: Span,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub enum Statement {
    If(
        TypedExpr,
        Box<Spanned<Statement>>,
        Option<Box<Spanned<Statement>>>,
    ),
    Loop(Box<Spanned<Statement>>),
    While(TypedExpr, Box<Spanned<Statement>>),
    Break,
    Continue,
    Return(TypedExpr),
    Let(Mutability, Spanned<Symbol>, Spanned<Type>, Option<TypedExpr>),
    Block(Vec<Spanned<Statement>>),
    Expr(TypedExpr),
    Error,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Hash, Debug, Copy, Clone)]
pub struct TypeVar(pub u32);

#[derive(Debug, Clone)]
pub enum Type {
    Var(TypeVar),
    Bool,
    Unit,
    Int(Signedness, Size),
    Concrete(Symbol, Rc<[Type]>),
    Pointer(Mutability, Rc<Type>),
    Function(Rc<[Type]>, Rc<Type>),
    Error,
}

impl Type {
    pub fn replace(&self, mapping: &HashMap<Symbol, Type>) -> Type {
        match *self {
            Type::Bool | Type::Error | Type::Int(_, _) | Type::Var(_) | Type::Unit => self.clone(),
            Type::Concrete(sym, ref params) => if let Some(typ) = mapping.get(&sym).cloned() {
                typ
            } else {
                let params = params
                    .iter()
                    .map(|ty| ty.replace(mapping))
                    .collect::<Vec<_>>()
                    .into();
                Type::Concrete(sym, params)
            },
            Type::Function(ref params, ref out) => {
                let params = params
                    .iter()
                    .map(|ty| ty.replace(mapping))
                    .collect::<Vec<_>>()
                    .into();
                let out = out.replace(mapping);
                Type::Function(params, Rc::new(out))
            }
            Type::Pointer(mutability, ref to) => {
                let to = to.replace(mapping);
                Type::Pointer(mutability, Rc::new(to))
            }
        }
    }

    pub fn is_atomic(&self) -> bool {
        match *self {
            Type::Bool |
            Type::Function(_, _) |
            Type::Int(_, _) |
            Type::Pointer(_, _) |
            Type::Unit => true,
            Type::Concrete(_, _) => false,
            Type::Error |
            Type::Var(_) => panic!("cannot say atomicity of {:?}", self),
        }
    }
}

#[derive(Debug, Clone)]
pub struct FnParam {
    pub mutability: Mutability,
    pub name: Symbol,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub complete_span: Span,
    pub fn_type: FunctionType,
    pub name: Symbol,
    pub type_params: Vec<Symbol>,
    pub params: Vec<FnParam>,
    pub return_type: Type,
    pub body: Option<Spanned<Statement>>,
}

#[derive(Debug, Clone)]
pub struct Field {
    pub name: Symbol,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct Struct {
    pub complete_span: Span,
    pub name: Symbol,
    pub type_params: Vec<Symbol>,
    pub fields: Vec<Field>,
}

#[derive(Debug, Clone)]
pub struct Program {
    pub structs: HashMap<Symbol, Struct>,
    pub functions: Vec<Function>,
}
