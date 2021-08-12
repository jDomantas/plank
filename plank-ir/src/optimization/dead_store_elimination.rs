use analysis::{self, usage, volatility, Loc};
use ir::{Instruction, Program};

fn is_call(instr: &Instruction) -> bool {
    matches!(
        *instr,
        Instruction::Call(..) | Instruction::CallProc(..) | Instruction::CallProcVirt(..)
    )
}

pub fn rewrite(program: &mut Program) {
    loop {
        let mut changed_anything = false;
        for f in program.functions.values_mut() {
            let volatile = &volatility::volatile_locations(f);
            let mut to_remove = Vec::new();
            {
                let ctx = usage::Context::new(f, volatile);
                for (&id, block) in &f.blocks {
                    for (pos, instr) in block.ops.iter().enumerate() {
                        let loc = Loc {
                            block: id,
                            pos: pos + 1,
                        };
                        let written = match *instr {
                            Instruction::Store(reg, _, _) => Some(reg),
                            ref other => analysis::initialized_register(other),
                        };
                        if let Some(reg) = written {
                            if !ctx.is_value_used(loc, reg) {
                                to_remove.push(Loc { block: id, pos });
                            }
                        }
                    }
                }
            }
            for loc in to_remove {
                let block = f.blocks.get_mut(&loc.block).unwrap();
                if !is_call(&block.ops[loc.pos]) {
                    changed_anything = true;
                    block.ops[loc.pos] = Instruction::Nop;
                }
            }
        }
        if !changed_anything {
            return;
        }
    }
}
