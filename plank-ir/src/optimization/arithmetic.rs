use analysis::Loc;
use ir::{BinaryOp, Instruction, IntOp, Program, Value};
use optimization::Rewriter;

struct Simplifier;

impl Rewriter for Simplifier {
    fn rewrite_instruction(&mut self, _loc: Loc, instr: &mut Instruction) {
        let mut result = None;
        match *instr {
            Instruction::BinaryOp(r, BinaryOp::IntOp(IntOp::Add, sign, size), ref a, ref b) => {
                if is_immediate(a) {
                    result = Some(Instruction::BinaryOp(
                        r,
                        BinaryOp::IntOp(IntOp::Add, sign, size),
                        b.clone(),
                        a.clone(),
                    ));
                }
            }
            Instruction::BinaryOp(r, BinaryOp::IntOp(IntOp::Mul, sign, size), ref a, ref b) => {
                if is_immediate(a) {
                    result = Some(Instruction::BinaryOp(
                        r,
                        BinaryOp::IntOp(IntOp::Mul, sign, size),
                        b.clone(),
                        a.clone(),
                    ));
                }
            }
            _ => {}
        }
        if let Some(new) = result {
            *instr = new;
        }
    }
}

fn is_immediate(val: &Value) -> bool {
    match *val {
        Value::Bytes(_) | Value::Int(_, _) | Value::Symbol(_) => true,
        Value::Reg(_) | Value::Undef => false,
    }
}

pub fn rewrite(program: &mut Program) {
    Simplifier.rewrite_program(program);
}
