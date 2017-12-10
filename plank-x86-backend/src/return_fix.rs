use std::collections::HashMap;
use plank_ir::ir::{self, Program, Function, Instruction, Reg, Layout, Value, BlockEnd};


fn fresh_register(regs: &HashMap<Reg, Layout>) -> Reg {
    for i in 0..regs.len() {
        if !regs.contains_key(&Reg(i as u32)) {
            return Reg(i as u32);
        }
    }
    Reg(regs.len() as u32)
}

fn fix_function(f: &mut Function) {
    let output_address = if let Some(layout) = f.output_layout {
        if layout.atomic {
            None
        } else {
            let reg = fresh_register(&f.registers);
            f.registers.insert(reg, Layout {
                size: ir::POINTER_SIZE,
                align: ir::POINTER_SIZE,
                atomic: true,
            });
            f.parameters.insert(0, reg);
            f.output_layout = None;
            Some(reg)
        }
    } else {
        None
    };
    for block in f.blocks.values_mut() {
        for i in (0..block.ops.len()).rev() {
            let initializer = match block.ops[i] {
                Instruction::Call(r, _, ref mut params) |
                Instruction::CallVirt(r, _, ref mut params) if !f.registers[&r].atomic => {
                    let reg = fresh_register(&f.registers);
                    f.registers.insert(reg, Layout {
                        size: ir::POINTER_SIZE,
                        align: ir::POINTER_SIZE,
                        atomic: true,
                    });
                    params.insert(0, Value::Reg(reg));
                    Some((reg, r))
                }
                _ => None,
            };
            if let Some((param, out)) = initializer {
                match block.ops[i].clone() {
                    Instruction::Call(_, f, params) => {
                        block.ops[i] = Instruction::CallProc(f, params);
                    }
                    Instruction::CallVirt(_, f, params) => {
                        block.ops[i] = Instruction::CallProcVirt(f, params);
                    }
                    _ => panic!("shit"),
                }
                block.ops.insert(i + 1, Instruction::Drop(param));
                block.ops.insert(i, Instruction::TakeAddress(param, out, 0));
                block.ops.insert(i, Instruction::Init(out));
            }
        }
        match (block.end.clone(), output_address) {
            (BlockEnd::Return(val), Some(reg)) => {
                block.ops.push(Instruction::DerefStore(Value::Reg(reg), 0, val));
                block.end = BlockEnd::ReturnProc;
            }
            _ => {}
        }
    }
}

pub fn fix_function_returns(program: &mut Program) {
    for f in program.functions.values_mut() {
        fix_function(f);
    }
}
