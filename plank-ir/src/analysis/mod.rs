pub mod liveness;
pub mod volatility;
pub mod usage;

use ir::{BlockId, Instruction, Reg};


#[derive(PartialEq, Eq, Debug, Hash, Copy, Clone)]
pub struct Loc {
    pub block: BlockId,
    pub pos: usize,
}

pub fn initialized_register(instr: &Instruction) -> Option<Reg> {
    match *instr {
        Instruction::Assign(reg, _) |
        Instruction::BinaryOp(reg, _, _, _) |
        Instruction::Call(reg, _, _) |
        Instruction::CallVirt(reg, _, _) |
        Instruction::CastAssign(reg, _) |
        Instruction::DerefLoad(reg, _, _) |
        Instruction::Init(reg) |
        Instruction::Load(reg, _, _) |
        Instruction::TakeAddress(reg, _, _) |
        Instruction::UnaryOp(reg, _, _) => Some(reg),
        _ => None,
    }
}
