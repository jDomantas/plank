use std::collections::HashMap;
use ir::{BinaryOp, Block, BlockEnd, Function, Instruction, Program, Reg, Size, Symbol, UnaryOp,
         Value};


struct Context<'a> {
    functions: &'a HashMap<Symbol, Function>,
    function: &'a Function,
}

impl<'a> Context<'a> {
    fn new(program: &'a Program, function: &'a Function) -> Self {
        Context {
            functions: &program.functions,
            function,
        }
    }

    fn validate(&self) {
        if let Some(layout) = self.function.output_layout {
            assert!(layout.size > 0);
        }
        for &layout in self.function.registers.values() {
            assert!(layout.size > 0);
        }
        for &reg in &self.function.parameters {
            assert!(self.function.registers.contains_key(&reg));
        }
        if let Some(block) = self.function.start_block {
            assert!(self.function.blocks.contains_key(&block));
        }
        for block in self.function.blocks.values() {
            self.validate_block(block);
        }
    }

    fn validate_block(&self, block: &Block) {
        for op in &block.ops {
            self.validate_instruction(op);
        }
        match block.end {
            BlockEnd::Branch(ref val, a, b) => {
                assert_eq!(self.value_size(val), 1);
                assert!(self.function.blocks.contains_key(&a));
                assert!(self.function.blocks.contains_key(&b));
            }
            BlockEnd::Jump(block) => {
                assert!(self.function.blocks.contains_key(&block));
            }
            BlockEnd::Return(ref val) => {
                let return_size = self.function.output_layout.unwrap().size;
                assert_eq!(self.value_size(val), return_size);
            }
            BlockEnd::ReturnProc => {
                assert!(self.function.output_layout.is_none());
            }
        }
    }

    fn validate_instruction(&self, i: &Instruction) {
        match *i {
            Instruction::Assign(reg, ref val) => {
                assert_eq!(self.register_size(reg), self.value_size(val));
            }
            Instruction::BinaryOp(dest, BinaryOp::BitOp(_, size), ref a, ref b) |
            Instruction::BinaryOp(dest, BinaryOp::IntOp(_, _, size), ref a, ref b) => {
                assert_eq!(self.register_size(dest), in_bytes(size));
                assert_eq!(self.value_size(a), in_bytes(size));
                assert_eq!(self.value_size(b), in_bytes(size));
            }
            Instruction::BinaryOp(dest, BinaryOp::Eq, ref a, ref b) |
            Instruction::BinaryOp(dest, BinaryOp::Neq, ref a, ref b) => {
                assert_eq!(self.register_size(dest), 1);
                assert_eq!(self.value_size(a), self.value_size(b));
            }
            Instruction::Call(dest, ref sym, ref params) => {
                let callee = &self.functions[sym];
                let out_size = callee.output_layout.unwrap().size;
                assert_eq!(self.register_size(dest), out_size);
                assert_eq!(params.len(), callee.parameters.len());
                for (val, &reg) in params.iter().zip(callee.parameters.iter()) {
                    let reg_size = callee.registers[&reg].size;
                    assert_eq!(self.value_size(val), reg_size);
                }
            }
            Instruction::CallProc(ref sym, ref params) => {
                let callee = &self.functions[sym];
                assert!(callee.output_layout.is_none());
                assert_eq!(params.len(), callee.parameters.len());
                for (val, &reg) in params.iter().zip(callee.parameters.iter()) {
                    let reg_size = callee.registers[&reg].size;
                    assert_eq!(self.value_size(val), reg_size);
                }
            }
            Instruction::CallVirt(_, ref address, _) |
            Instruction::CallProcVirt(ref address, _) => {
                assert_eq!(self.value_size(address), ::ir::POINTER_SIZE);
            }
            Instruction::DerefLoad(_, ref val, _) => {
                assert_eq!(self.value_size(val), ::ir::POINTER_SIZE);
            }
            Instruction::DerefStore(ref address, _, ref value) => {
                assert_eq!(self.value_size(address), ::ir::POINTER_SIZE);
                assert!(self.value_size(value) > 0);
            }
            Instruction::Drop(reg) => {
                assert!(self.function.registers.contains_key(&reg));
            }
            Instruction::Load(dest, reg, offset) => {
                let dest_size = self.register_size(dest);
                let reg_size = self.register_size(reg);
                assert!(dest_size + offset <= reg_size);
            }
            Instruction::Store(reg, offset, ref value) => {
                let reg_size = self.register_size(reg);
                let val_size = self.value_size(value);
                assert!(reg_size >= offset + val_size);
            }
            Instruction::TakeAddress(dest, reg, offset) => {
                assert_eq!(self.register_size(dest), ::ir::POINTER_SIZE);
                let reg_size = self.register_size(reg);
                assert!(offset < reg_size);
            }
            Instruction::UnaryOp(dest, UnaryOp::Negate(_, size), ref value) => {
                assert_eq!(self.register_size(dest), in_bytes(size));
                assert_eq!(self.value_size(value), in_bytes(size));
            }
        }
    }

    fn register_size(&self, reg: Reg) -> u32 {
        self.function.registers[&reg].size
    }

    fn value_size(&self, value: &Value) -> u32 {
        match *value {
            Value::Bytes(_) => ::ir::POINTER_SIZE,
            Value::Int(_, size) => in_bytes(size),
            Value::Reg(reg) => self.register_size(reg),
            Value::Symbol(_) => ::ir::FUNCTION_SIZE,
        }
    }
}

fn in_bytes(size: Size) -> u32 {
    match size {
        Size::Bit8 => 1,
        Size::Bit16 => 2,
        Size::Bit32 => 4,
    }
}

pub fn validate_ir(program: &Program) {
    for f in program.functions.values() {
        let ctx = Context::new(program, f);
        ctx.validate();
    }
}
