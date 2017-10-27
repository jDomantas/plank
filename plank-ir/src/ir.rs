use std::collections::HashMap;
use std::rc::Rc;


#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub struct Symbol(pub Rc<str>);

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: HashMap<Symbol, Function>,
}

#[derive(Debug, Copy, Clone)]
pub struct Layout {
    pub size: u32,
    pub align: u32,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub parameters: Vec<Reg>,
    pub output_layout: Option<Layout>,
    pub registers: HashMap<Reg, Layout>,
    pub blocks: HashMap<BlockId, Block>,
    pub start_block: BlockId,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Copy, Clone)]
pub struct Reg(pub u32);

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Copy, Clone)]
pub struct BlockId(pub u32);

#[derive(Debug, Clone)]
pub struct Block {
    pub ops: Vec<Instruction>,
    pub end: BlockEnd,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    /// `drop reg`
    Drop(Reg),
    /// `reg = op a b`
    BinaryOp(Reg, BinaryOp, Value, Value),
    /// `reg = op value`
    UnaryOp(Reg, UnaryOp, Value),
    /// `reg = sym(val1, val2, ...)`
    Call(Reg, Symbol, Vec<Value>),
    /// sym(val1, val2, ...)
    CallProc(Symbol, Vec<Value>),
    /// `reg = sym(val1, val2, ...)`
    CallVirt(Reg, Value, Vec<Value>),
    /// sym(val1, val2, ...)
    CallProcVirt(Value, Vec<Value>),
    /// `*(value + offset) = value`
    DerefStore(Value, u32, Value),
    /// `reg = *(value + offset)`
    DerefLoad(Reg, Value, u32),
    /// `*(&reg + offset) = value`
    Store(Reg, u32, Value),
    /// `reg = *(&reg + offset)`
    Load(Reg, Reg, u32),
    /// `reg = &reg + offset`
    TakeAddress(Reg, Reg, u32),
    /// `reg = value`
    Assign(Reg, Value),
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(u64, Size),
    Reg(Reg),
    Symbol(Symbol),
    Bytes(Vec<u8>),
}

#[derive(Debug, Clone)]
pub enum BlockEnd {
    Return(Value),
    ReturnProc,
    Jump(BlockId),
    Branch(Value, BlockId, BlockId),
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    IntOp(IntOp, Signedness, Size),
    BitOp(BitOp, Size),
    Eq,
    Neq,
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Signedness {
    Unsigned,
    Signed,
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum Size {
    Bit8,
    Bit16,
    Bit32,
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum IntOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Less,
    LessEq,
    Greater,
    GreaterEq,
}

#[derive(PartialEq, Eq, Debug, Copy, Clone)]
pub enum BitOp {
    And,
    Or,
    Xor,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negate(Signedness, Size),
}
