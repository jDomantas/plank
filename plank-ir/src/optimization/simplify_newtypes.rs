use std::collections::HashMap;
use ir::{Program, Reg, Function, Instruction, Value};
use optimization::{self as opt, Rewriter};


#[derive(Default)]
struct Simplifier {
    reg_size: HashMap<Reg, u32>,
}

impl Simplifier {
    fn value_size(&self, val: &Value) -> u32 {
        match *val {
            Value::Bytes(_) => ::ir::POINTER_SIZE,
            Value::Int(_, size) => size.in_bytes(),
            Value::Reg(reg) => self.reg_size[&reg],
            Value::Symbol(_) => ::ir::FUNCTION_SIZE,
        }
    }
}

impl Rewriter for Simplifier {
    fn rewrite_function(&mut self, f: &mut Function) {
        self.reg_size = f.registers
            .iter()
            .map(|(&reg, &layout)| (reg, layout.size))
            .collect();
        opt::rewrite_function(self, f);
    }

    fn rewrite_instruction(&mut self, instr: &mut Instruction) {
        let mut result = None;
        match *instr {
            Instruction::Store(reg, 0, ref val) => {
                if self.reg_size[&reg] == self.value_size(val) {
                    result = Some(Instruction::Assign(reg, val.clone()));
                }
            }
            Instruction::Load(reg, reg2, 0) => {
                if self.reg_size[&reg] == self.reg_size[&reg2] {
                    result = Some(Instruction::Assign(reg, Value::Reg(reg2)))
                }
            }
            _ => {}
        }
        if let Some(new) = result {
            *instr = new;
        }
    }
}

pub fn rewrite(program: &mut Program) {
    Simplifier::default().rewrite_program(program);
}
