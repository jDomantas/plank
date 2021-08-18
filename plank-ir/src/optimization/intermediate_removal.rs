use analysis::{self, Loc};
use ir::{BlockId, Function, Instruction, Program, Value};

struct Fix {
    block: BlockId,
    init_at: usize,
}

fn inspect_function(f: &Function) -> Vec<Fix> {
    let mut fixes = Vec::new();
    let volatility = &analysis::volatility::volatile_locations(f);
    let ctx = analysis::usage::Context::new(f, volatility);
    for (&id, block) in &f.blocks {
        for (pos, op) in block.ops.iter().enumerate() {
            if let Some(reg) = analysis::initialized_register(op) {
                match block.ops.get(pos + 1) {
                    Some(&Instruction::Assign(_, Value::Reg(r2))) if r2 == reg => {
                        let loc = Loc {
                            block: id,
                            pos: pos + 2,
                        };
                        if !ctx.is_value_used(loc, r2) {
                            fixes.push(Fix {
                                block: id,
                                init_at: pos,
                            })
                        }
                    }
                    _ => {}
                }
            }
        }
    }
    fixes
}

fn rewrite_function(f: &mut Function) {
    let fixes = inspect_function(f);
    for fix in fixes {
        let block = f.blocks.get_mut(&fix.block).unwrap();
        let reg = match block.ops[fix.init_at + 1] {
            Instruction::Assign(to, _) => to,
            _ => panic!("invalid fix"),
        };
        block.ops[fix.init_at + 1] = Instruction::Nop;
        match block.ops[fix.init_at] {
            Instruction::Assign(ref mut r, _)
            | Instruction::BinaryOp(ref mut r, _, _, _)
            | Instruction::Call(ref mut r, _, _)
            | Instruction::CallVirt(ref mut r, _, _)
            | Instruction::CastAssign(ref mut r, _)
            | Instruction::DerefLoad(ref mut r, _, _)
            | Instruction::Init(ref mut r)
            | Instruction::Load(ref mut r, _, _)
            | Instruction::TakeAddress(ref mut r, _, _)
            | Instruction::UnaryOp(ref mut r, _, _) => *r = reg,
            _ => panic!("invalid fix"),
        }
    }
}

pub fn rewrite(program: &mut Program) {
    for f in program.functions.values_mut() {
        rewrite_function(f);
    }
}
