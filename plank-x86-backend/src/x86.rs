use std::rc::Rc;

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub enum Register {
    Eax,
    Ebx,
    Ecx,
    Edx,
    Ebp,
    Esp,
    Esi,
    Edi,
    Ax,
    Bx,
    Cx,
    Dx,
    Al,
    Ah,
    Bl,
    Bh,
    Cl,
    Ch,
    Dl,
    Dh,
}

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub struct Memory {
    pub register: Register,
    pub offset: i32,
    pub ptr_size: u32,
}

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub enum Rm {
    Register(Register),
    Memory(Memory),
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum TwoArgs {
    RmImm(Rm, Immediate),
    RegRm(Register, Rm),
    RmReg(Rm, Register),
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Immediate {
    Label(Label),
    Constant(u64),
}

#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub enum Condition {
    Above,
    AboveEqual,
    Below,
    BelowEqual,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
}

impl Condition {
    pub fn opposite(&self) -> Condition {
        match *self {
            Condition::Above => Condition::BelowEqual,
            Condition::AboveEqual => Condition::Below,
            Condition::Below => Condition::AboveEqual,
            Condition::BelowEqual => Condition::Above,
            Condition::Equal => Condition::NotEqual,
            Condition::Greater => Condition::LessEqual,
            Condition::GreaterEqual => Condition::Less,
            Condition::Less => Condition::GreaterEqual,
            Condition::LessEqual => Condition::Greater,
            Condition::NotEqual => Condition::Equal,
        }
    }

    pub fn order_opposite(&self) -> Condition {
        match *self {
            Condition::Above => Condition::Below,
            Condition::AboveEqual => Condition::BelowEqual,
            Condition::Below => Condition::Above,
            Condition::BelowEqual => Condition::AboveEqual,
            Condition::Equal => panic!("no order opposite for Condition::Equal"),
            Condition::Greater => Condition::Less,
            Condition::GreaterEqual => Condition::LessEqual,
            Condition::Less => Condition::Greater,
            Condition::LessEqual => Condition::GreaterEqual,
            Condition::NotEqual => panic!("no order opposite for Condition::NotEqual"),
        }
    }
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Label {
    Unnamed(u32),
    Named(Rc<str>),
    String(u32),
}

#[derive(PartialEq, Eq, Debug, Hash, Clone)]
pub enum Instruction {
    Invalid,
    Mov(TwoArgs),
    MovSX(Register, Rm),
    MovZX(Register, Rm),
    Add(TwoArgs),
    Sub(TwoArgs),
    Mul(Rm),
    Imul(Rm),
    ImulReg(Register, Rm),
    Div(Rm),
    Idiv(Rm),
    And(TwoArgs),
    Or(TwoArgs),
    Xor(TwoArgs),
    Cwd,
    Cdq,
    Setcc(Condition, Rm),
    Jmp(Label),
    Jcc(Condition, Label),
    Neg(Rm),
    Lea(Register, Memory),
    Push(Rm),
    Pop(Rm),
    Test(TwoArgs),
    Cmp(TwoArgs),
    Call(Immediate),
    CallVirt(Rm),
    Ret,
    Label(Label),
}

#[derive(Debug, Clone)]
pub struct Program {
    pub functions: Vec<Vec<Instruction>>,
    pub strings: Vec<Vec<u8>>,
}
