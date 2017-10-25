use std::collections::HashMap;
use plank_syntax::position::Spanned;
use ast::typed;
pub use ast::typed::{Symbol, Type, Signedness, Size};


#[derive(Debug, Clone)]
pub struct Program {
    pub structs: HashMap<Symbol, typed::Struct>,
    pub functions: HashMap<Symbol, Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub type_params: Vec<Symbol>,
    pub registers: HashMap<Reg, Type>,
    pub blocks: HashMap<BlockId, Block>,
    pub start_block: BlockId,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Copy, Clone)]
pub struct Reg(pub u32);

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Copy, Clone)]
pub struct BlockId(pub u32);

#[derive(Debug, Clone)]
pub struct Block {
    pub ops: Vec<Spanned<Instruction>>,
    pub end: BlockEnd,
    pub link: BlockLink,
}

#[derive(Debug, Copy, Clone)]
pub enum BlockLink {
    /// Block would extend this block if not for `return`, `break`, or `continue`
    Strong(BlockId),
    /// Block lexically follows this block, but no real relation
    Weak(BlockId),
    /// No link
    None,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    StartStatement,
    Drop(Reg),
    BinaryOp(Reg, BinaryOp, Value, Value),
    UnaryOp(Reg, UnaryOp, Value),
    Call(Reg, Symbol, Vec<Type>, Vec<Value>),
    VirtualCall(Reg, Value, Vec<Value>),
    /// (*value1).field1.field2... = value1
    DerefStore(Value, Vec<usize>, Value),
    /// reg.field1.field2... = value
    FieldStore(Reg, Vec<usize>, Value),
    /// reg = &reg.field1.field2...
    TakeAddress(Reg, Reg, Vec<usize>),
    Assign(Reg, Value),
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(u64),
    Reg(Reg),
    Symbol(Symbol, Vec<Type>),
    Error,
}

#[derive(Debug, Clone)]
pub enum BlockEnd {
    Return(Value),
    Jump(BlockId),
    Branch(Value, BlockId, BlockId),
    Error,
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add(Signedness, Size),
    Sub(Signedness, Size),
    Mul(Signedness, Size),
    Div(Signedness, Size),
    Mod(Signedness, Size),
    Less(Signedness, Size),
    LessEq(Signedness, Size),
    Greater(Signedness, Size),
    GreaterEq(Signedness, Size),
    Eq,
    Neq,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negate(Signedness, Size),
    Not,
    DerefLoad,
    /// <arg>.field1.field2...
    FieldLoad(Vec<usize>),
    /// &(*<arg>).field1.field2...
    OffsetAddress(Vec<usize>),
}
