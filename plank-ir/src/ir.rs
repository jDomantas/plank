use std::collections::HashMap;
use std::rc::Rc;


pub const POINTER_SIZE: u32 = 4;
pub const FUNCTION_SIZE: u32 = 4;

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
    pub start_block: Option<BlockId>,
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
    /// `unreachable`
    Unreachable,
    /// `nop`
    Nop,
    /// `init reg`
    Init(Reg),
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
    /// `reg = cast value`
    CastAssign(Reg, Value),
}

#[derive(PartialEq, Eq, Debug, Clone)]
pub enum Value {
    Int(u64, Size),
    Reg(Reg),
    Symbol(Symbol),
    Bytes(Vec<u8>),
    Undef,
}

#[derive(Debug, Clone)]
pub enum BlockEnd {
    Return(Value),
    ReturnProc,
    Jump(BlockId),
    Branch(Value, BlockId, BlockId),
    Unreachable,
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

impl Size {
    pub fn in_bytes(&self) -> u32 {
        match *self {
            Size::Bit8 => 1,
            Size::Bit16 => 2,
            Size::Bit32 => 4,
        }
    }

    pub fn truncate(&self, value: u64) -> u64 {
        match *self {
            Size::Bit8 => value & 0xff,
            Size::Bit16 => value & 0xffff,
            Size::Bit32 => value & 0xffffffff,
        }
    }

    pub fn to_signed(&self, value: u64) -> i64 {
        match *self {
            Size::Bit8 => (value as i8) as i64,
            Size::Bit16 => (value as i16) as i64,
            Size::Bit32 => (value as i32) as i64,
        }
    }
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

#[derive(Debug, Copy, Clone)]
pub enum UnaryOp {
    Negate(Signedness, Size),
}
