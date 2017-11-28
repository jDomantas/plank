use std::collections::{HashMap, HashSet};
use analysis::Loc;
use ir::{BinaryOp, Block, BlockEnd, BlockId, Function, Instruction, IntOp, Program, Reg, Symbol,
         UnaryOp, Value};


#[derive(Debug)]
pub enum Error {
    BadLayout,
    UnknownRegister(Reg),
    UnknownBlock(BlockId),
    NonLiveRegUsage(Reg, Loc),
    BadValueSize(Loc),
    BadParamCount(Loc),
    BadCall(Loc),
    ZeroSizedVal(Loc),
    OutOfBounds(Loc),
    InvalidReturn,
}

struct Context<'a> {
    functions: &'a HashMap<Symbol, Function>,
    function: &'a Function,
    live_locations: HashMap<Reg, HashSet<Loc>>,
}

impl<'a> Context<'a> {
    fn new(program: &'a Program, function: &'a Function) -> Self {
        Context {
            functions: &program.functions,
            function,
            live_locations: ::analysis::liveness::live_locations(function),
        }
    }

    fn validate(&self) -> Result<(), Error> {
        if let Some(layout) = self.function.output_layout {
            if layout.size <= 0 {
                return Err(Error::BadLayout);
            }
        }
        for &layout in self.function.registers.values() {
            if layout.size <= 0 {
                return Err(Error::BadLayout);
            }
        }
        for &reg in &self.function.parameters {
            if !self.function.registers.contains_key(&reg) {
                return Err(Error::UnknownRegister(reg));
            }
        }
        if let Some(block) = self.function.start_block {
            if !self.function.blocks.contains_key(&block) {
                return Err(Error::UnknownBlock(block));
            }
        }
        for (&id, block) in &self.function.blocks {
            self.validate_block(id, block)?;
        }
        Ok(())
    }

    fn validate_block(&self, id: BlockId, block: &Block) -> Result<(), Error> {
        for (index, op) in block.ops.iter().enumerate() {
            let loc = Loc { block: id, pos: index };
            self.validate_instruction(op, loc)?;
        }
        let loc = Loc { block: id, pos: block.ops.len() };
        match block.end {
            BlockEnd::Branch(ref val, a, b) => {
                assert_equal(self.value_size(val), 1, loc)?;
                self.assert_live_val(val, loc)?;
                if !self.function.blocks.contains_key(&a) {
                    return Err(Error::UnknownBlock(a));
                }
                if !self.function.blocks.contains_key(&b) {
                    return Err(Error::UnknownBlock(b));
                }
            }
            BlockEnd::Jump(block) => {
                if !self.function.blocks.contains_key(&block) {
                    return Err(Error::UnknownBlock(block));
                }
            }
            BlockEnd::Return(ref val) => {
                self.assert_live_val(val, loc)?;
                match self.function.output_layout {
                    Some(layout) => {
                        assert_equal(self.value_size(val), layout.size, loc)?;
                    }
                    None => {
                        return Err(Error::InvalidReturn);
                    }
                }
            }
            BlockEnd::ReturnProc => {
                if self.function.output_layout.is_some() {
                    return Err(Error::InvalidReturn);
                }
            }
        }
        Ok(())
    }

    fn validate_instruction(&self, i: &Instruction, loc: Loc) -> Result<(), Error> {
        match *i {
            Instruction::Assign(reg, ref val) |
            Instruction::CastAssign(reg, ref val) => {
                self.assert_live_val(val, loc)?;
                assert_equal(self.register_size(reg), self.value_size(val), loc)?;
            }
            Instruction::BinaryOp(dest, BinaryOp::IntOp(IntOp::Greater, _, size), ref a, ref b) |
            Instruction::BinaryOp(dest, BinaryOp::IntOp(IntOp::GreaterEq, _, size), ref a, ref b) |
            Instruction::BinaryOp(dest, BinaryOp::IntOp(IntOp::Less, _, size), ref a, ref b) |
            Instruction::BinaryOp(dest, BinaryOp::IntOp(IntOp::LessEq, _, size), ref a, ref b) => {
                self.assert_live_val(a, loc)?;
                self.assert_live_val(b, loc)?;
                assert_equal(self.register_size(dest), 1, loc)?;
                assert_equal(self.value_size(a), size.in_bytes(), loc)?;
                assert_equal(self.value_size(b), size.in_bytes(), loc)?;
            }
            Instruction::BinaryOp(dest, BinaryOp::BitOp(_, size), ref a, ref b) |
            Instruction::BinaryOp(dest, BinaryOp::IntOp(_, _, size), ref a, ref b) => {
                self.assert_live_val(a, loc)?;
                self.assert_live_val(b, loc)?;
                assert_equal(self.register_size(dest), size.in_bytes(), loc)?;
                assert_equal(self.value_size(a), size.in_bytes(), loc)?;
                assert_equal(self.value_size(b), size.in_bytes(), loc)?;
            }
            Instruction::BinaryOp(dest, BinaryOp::Eq, ref a, ref b) |
            Instruction::BinaryOp(dest, BinaryOp::Neq, ref a, ref b) => {
                self.assert_live_val(a, loc)?;
                self.assert_live_val(b, loc)?;
                assert_equal(self.register_size(dest), 1, loc)?;
                assert_equal(self.value_size(a), self.value_size(b), loc)?;
            }
            Instruction::Call(dest, ref sym, ref params) => {
                let callee = &self.functions[sym];
                let out_size = match callee.output_layout {
                    Some(layout) => layout.size,
                    None => return Err(Error::BadCall(loc)),
                };
                assert_equal(self.register_size(dest), out_size, loc)?;
                assert_equal_counts(params.len(), callee.parameters.len(), loc)?;
                for (val, &reg) in params.iter().zip(callee.parameters.iter()) {
                    self.assert_live_val(val, loc)?;
                    let reg_size = callee.registers[&reg].size;
                    assert_equal(self.value_size(val), reg_size, loc)?;
                }
            }
            Instruction::CallProc(ref sym, ref params) => {
                let callee = &self.functions[sym];
                if callee.output_layout.is_some() {
                    return Err(Error::BadCall(loc));
                }
                assert_equal_counts(params.len(), callee.parameters.len(), loc)?;
                for (val, &reg) in params.iter().zip(callee.parameters.iter()) {
                    self.assert_live_val(val, loc)?;
                    let reg_size = callee.registers[&reg].size;
                    assert_equal(self.value_size(val), reg_size, loc)?;
                }
            }
            Instruction::CallVirt(_, ref address, ref params) |
            Instruction::CallProcVirt(ref address, ref params) => {
                assert_equal(self.value_size(address), ::ir::POINTER_SIZE, loc)?;
                for val in params {
                    self.assert_live_val(val, loc)?;
                }
            }
            Instruction::DerefLoad(_, ref val, _) => {
                self.assert_live_val(val, loc)?;
                assert_equal(self.value_size(val), ::ir::POINTER_SIZE, loc)?;
            }
            Instruction::DerefStore(ref address, _, ref value) => {
                self.assert_live_val(address, loc)?;
                self.assert_live_val(value, loc)?;
                assert_equal(self.value_size(address), ::ir::POINTER_SIZE, loc)?;
                if self.value_size(value) == 0 {
                    return Err(Error::ZeroSizedVal(loc));
                }
            }
            Instruction::Drop(reg) |
            Instruction::Init(reg) => {
                if !self.function.registers.contains_key(&reg) {
                    return Err(Error::UnknownRegister(reg));
                }
            }
            Instruction::Load(dest, reg, offset) => {
                self.assert_live(reg, loc)?;
                let dest_size = self.register_size(dest);
                let reg_size = self.register_size(reg);
                if dest_size + offset > reg_size {
                    return Err(Error::OutOfBounds(loc));
                }
            }
            Instruction::Nop => {}
            Instruction::Store(reg, offset, ref value) => {
                self.assert_live(reg, loc)?;
                self.assert_live_val(value, loc)?;
                let reg_size = self.register_size(reg);
                let val_size = self.value_size(value);
                if reg_size < offset + val_size {
                    return Err(Error::OutOfBounds(loc));
                }
            }
            Instruction::TakeAddress(dest, reg, offset) => {
                self.assert_live(reg, loc)?;
                assert_equal(self.register_size(dest), ::ir::POINTER_SIZE, loc)?;
                let reg_size = self.register_size(reg);
                if offset >= reg_size {
                    return Err(Error::OutOfBounds(loc));
                }
            }
            Instruction::UnaryOp(dest, UnaryOp::Negate(_, size), ref value) => {
                self.assert_live_val(value, loc)?;
                assert_equal(self.register_size(dest), size.in_bytes(), loc)?;
                assert_equal(self.value_size(value), size.in_bytes(), loc)?;
            }
            Instruction::Unreachable => {}
        }
        Ok(())
    }

    fn register_size(&self, reg: Reg) -> u32 {
        self.function.registers[&reg].size
    }

    fn value_size(&self, value: &Value) -> u32 {
        match *value {
            Value::Undef => 1,
            Value::Bytes(_) => ::ir::POINTER_SIZE,
            Value::Int(_, size) => size.in_bytes(),
            Value::Reg(reg) => self.register_size(reg),
            Value::Symbol(_) => ::ir::FUNCTION_SIZE,
        }
    }

    fn assert_live_val(&self, val: &Value, loc: Loc) -> Result<(), Error> {
        match *val {
            Value::Reg(reg) => self.assert_live(reg, loc),
            _ => Ok(()),
        }
    }

    fn assert_live(&self, reg: Reg, loc: Loc) -> Result<(), Error> {
        self.live_locations
            .get(&reg)
            .and_then(|locs| if locs.contains(&loc) {
                Some(())
            } else {
                None
            })
            .ok_or(Error::NonLiveRegUsage(reg, loc))
    }
}

fn assert_equal(size1: u32, size2: u32, loc: Loc) -> Result<(), Error> {
    if size1 == size2 {
        Ok(())
    } else {
        Err(Error::BadValueSize(loc))
    }
}

fn assert_equal_counts(cnt1: usize, cnt2: usize, loc: Loc) -> Result<(), Error> {
    if cnt1 == cnt2 {
        Ok(())
    } else {
        Err(Error::BadParamCount(loc))
    }
}

pub fn validate_ir(program: &Program) -> Result<(), (&::ir::Symbol, Error)> {
    for (sym, f) in &program.functions {
        let ctx = Context::new(program, f);
        if let Err(e) = ctx.validate() {
            return Err((sym, e));
        }
    }
    Ok(())
}
