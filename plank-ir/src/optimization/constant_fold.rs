use std::collections::{HashMap, HashSet};
use ir::{Function, BinaryOp, BitOp, IntOp, UnaryOp, Signedness, Size, Program, Reg, Block, Instruction, Value, BlockEnd, BlockId};
use optimization::{self as opt, Rewriter};


#[derive(Debug, Default)]
struct BlockState {
    have_any: bool,
    values: HashMap<Reg, Value>,
    volatile: HashSet<Reg>,
    entries_left: u32,
}

struct BlockFolder {
    reg_value: HashMap<Reg, Value>,
    volatile_regs: HashSet<Reg>,
}

impl BlockFolder {
    fn get_value(&self, reg: Reg) -> Option<Value> {
        self.reg_value.get(&reg).cloned()
    }

    fn simplify(&self, value: &mut Value) {
        match *value {
            Value::Reg(reg) => {
                if let Some(val) = self.get_value(reg) {
                    *value = val;
                }
            }
            _ => {}
        }
    }
}

impl Rewriter for BlockFolder {
    fn rewrite_block(&mut self, block: &mut Block) {
        opt::rewrite_block(self, block);
        match block.end {
            BlockEnd::Branch(ref mut val, _, _) |
            BlockEnd::Return(ref mut val) => self.simplify(val),
            _ => {}
        }
        match block.end {
            BlockEnd::Branch(Value::Int(0, _), _, a) |
            BlockEnd::Branch(Value::Int(_, _), a, _) => {
                block.end = BlockEnd::Jump(a);
            }
            _ => {}
        }
    }

    fn rewrite_instruction(&mut self, instr: &mut Instruction) {
        let mut result = None;
        match *instr {
            Instruction::Assign(reg, ref mut value) => {
                self.simplify(value);
                if let Value::Int(_, _) = *value {
                    self.reg_value.insert(reg, value.clone());
                }
            }
            Instruction::BinaryOp(dest, op, ref mut a, ref mut b) => {
                self.simplify(a);
                self.simplify(b);
                if let Some(value) = eval_binary_op(op, a, b) {
                    self.reg_value.insert(dest, value.clone());
                    result = Some(Instruction::Assign(dest, value));
                }
            }
            Instruction::Call(_, _, ref mut params) |
            Instruction::CallProc(_, ref mut params) |
            Instruction::CallProcVirt(_, ref mut params) |
            Instruction::CallVirt(_, _, ref mut params) => {
                for param in params {
                    self.simplify(param);
                }
                for &reg in &self.volatile_regs {
                    self.reg_value.remove(&reg);
                }
            }
            Instruction::CastAssign(_, ref mut value) |
            Instruction::DerefLoad(_, ref mut value, _) |
            Instruction::Store(_, _, ref mut value) => {
                self.simplify(value);
            }
            Instruction::DerefStore(ref mut val1, _, ref mut val2) => {
                self.simplify(val1);
                self.simplify(val2);
                for &reg in &self.volatile_regs {
                    self.reg_value.remove(&reg);
                }
            }
            Instruction::TakeAddress(_, reg, _) => {
                self.volatile_regs.insert(reg);
            }
            Instruction::UnaryOp(dest, op, ref mut arg) => {
                self.simplify(arg);
                if let Some(value) = eval_unary_op(op, arg) {
                    self.reg_value.insert(dest, value.clone());
                    result = Some(Instruction::Assign(dest, value));
                }
            }
            _ => {}
        }
        if let Some(op) = result {
            *instr = op;
        }
    }
}

fn eval_binary_op(op: BinaryOp, a: &Value, b: &Value) -> Option<Value> {
    let (a, b) = match (a, b) {
        (&Value::Int(a, _), &Value::Int(b, _)) => (a, b),
        _ => return None,
    };
    match op {
        BinaryOp::BitOp(BitOp::And, size) => {
            Some(Value::Int(a & b, size))
        }
        BinaryOp::BitOp(BitOp::Or, size) => {
            Some(Value::Int(a | b, size))
        }
        BinaryOp::BitOp(BitOp::Xor, size) => {
            Some(Value::Int(a ^ b, size))
        }
        BinaryOp::Eq => {
            let res = if a == b { 1 } else { 0 };
            Some(Value::Int(res, Size::Bit8))
        }
        BinaryOp::Neq => {
            let res = if a != b { 1 } else { 0 };
            Some(Value::Int(res, Size::Bit8))
        }
        BinaryOp::IntOp(IntOp::Add, _, size) => {
            let res = size.truncate(a.wrapping_add(b));
            Some(Value::Int(res, size))
        }
        BinaryOp::IntOp(IntOp::Sub, _, size) => {
            let res = size.truncate(a.wrapping_sub(b));
            Some(Value::Int(res, size))
        }
        BinaryOp::IntOp(IntOp::Mul, Signedness::Unsigned, size) => {
            let res = size.truncate(a.wrapping_mul(b));
            Some(Value::Int(res, size))
        }
        BinaryOp::IntOp(IntOp::Mul, Signedness::Signed, size) => {
            let a = size.to_signed(a);
            let b = size.to_signed(b);
            let res = size.truncate(a.wrapping_mul(b) as u64);
            Some(Value::Int(res, size))
        }
        BinaryOp::IntOp(IntOp::Div, Signedness::Unsigned, size) => {
            if b != 0 {
                let res = size.truncate(a.wrapping_div(b));
                Some(Value::Int(res, size))
            } else {
                // TODO: implement something like Value::Undef ?
                None
            }
        }
        BinaryOp::IntOp(IntOp::Div, Signedness::Signed, size) => {
            if b != 0 {
                let a = size.to_signed(a);
                let b = size.to_signed(b);
                let res = size.truncate(a.wrapping_div(b) as u64);
                Some(Value::Int(res, size))
            } else {
                // TODO: implement something like Value::Undef ?
                None
            }
        }
        BinaryOp::IntOp(IntOp::Mod, Signedness::Unsigned, size) => {
            if b != 0 {
                let res = size.truncate(a.wrapping_rem(b));
                Some(Value::Int(res, size))
            } else {
                // TODO: implement something like Value::Undef ?
                None
            }
        }
        BinaryOp::IntOp(IntOp::Mod, Signedness::Signed, size) => {
            if b != 0 {
                let a = size.to_signed(a);
                let b = size.to_signed(b);
                let res = size.truncate(a.wrapping_rem(b) as u64);
                Some(Value::Int(res, size))
            } else {
                // TODO: implement something like Value::Undef ?
                None
            }
        }
        BinaryOp::IntOp(IntOp::Greater, Signedness::Unsigned, size) => {
            Some(Value::Int(if a > b { 1 } else { 0 }, size))
        }
        BinaryOp::IntOp(IntOp::Greater, Signedness::Signed, size) => {
            let a = size.to_signed(a);
            let b = size.to_signed(b);
            Some(Value::Int(if a > b { 1 } else { 0 }, size))
        }
        BinaryOp::IntOp(IntOp::Less, Signedness::Unsigned, size) => {
            Some(Value::Int(if a < b { 1 } else { 0 }, size))
        }
        BinaryOp::IntOp(IntOp::Less, Signedness::Signed, size) => {
            let a = size.to_signed(a);
            let b = size.to_signed(b);
            Some(Value::Int(if a < b { 1 } else { 0 }, size))
        }
        BinaryOp::IntOp(IntOp::GreaterEq, Signedness::Unsigned, size) => {
            Some(Value::Int(if a >= b { 1 } else { 0 }, size))
        }
        BinaryOp::IntOp(IntOp::GreaterEq, Signedness::Signed, size) => {
            let a = size.to_signed(a);
            let b = size.to_signed(b);
            Some(Value::Int(if a >= b { 1 } else { 0 }, size))
        }
        BinaryOp::IntOp(IntOp::LessEq, Signedness::Unsigned, size) => {
            Some(Value::Int(if a > b { 1 } else { 0 }, size))
        }
        BinaryOp::IntOp(IntOp::LessEq, Signedness::Signed, size) => {
            let a = size.to_signed(a);
            let b = size.to_signed(b);
            Some(Value::Int(if a <= b { 1 } else { 0 }, size))
        }
    }
}

fn eval_unary_op(op: UnaryOp, a: &Value) -> Option<Value> {
    let a = match *a {
        Value::Int(a, _) => a,
        _ => return None,
    };
    match op {
        UnaryOp::Negate(_, size) => {
            let res = size.truncate((!a).wrapping_add(1));
            Some(Value::Int(res, size))
        }
    }
}

#[derive(Default)]
struct VolatileGather(HashSet<Reg>);

impl VolatileGather {
    fn volatile_regs(f: &mut Function) -> HashSet<Reg> {
        let mut gather = VolatileGather::default();
        gather.rewrite_function(f);
        gather.0
    }
}

impl Rewriter for VolatileGather {
    fn rewrite_instruction(&mut self, instr: &mut Instruction) {
        match *instr {
            Instruction::TakeAddress(_, reg, _) => {
                self.0.insert(reg);
            }
            _ => {}
        }
    }
}

#[derive(Default)]
struct Folder {
    block_states: HashMap<BlockId, BlockState>,
}

impl Folder {
    fn add_block_entry(&mut self, id: BlockId) {
        self.block_states
            .entry(id)
            .or_insert_with(Default::default)
            .entries_left += 1;
    }

    fn got_entry_state(&mut self, id: BlockId, folder: &BlockFolder) {
        if let Some(state) = self.block_states.get_mut(&id) {
            state.entries_left -= 1;
            state.volatile.extend(folder.volatile_regs.iter().cloned());
            if state.have_any {
                for (&reg, val) in &folder.reg_value {
                    match state.values.get(&reg) {
                        Some(value) if value == val => continue,
                        _ => {}
                    }
                    state.values.remove(&reg);
                }
            } else {
                state.values.extend(folder
                    .reg_value
                    .iter()
                    .map(|(a, b)| (a.clone(), b.clone())));
                state.have_any = true;
            }
        }
    }

    fn run_block(&mut self, id: BlockId, block: &mut Block, fn_volatile: &HashSet<Reg>) {
        let state = self.block_states.remove(&id).unwrap();
        let mut folder = if state.entries_left == 0 {
            BlockFolder {
                reg_value: state.values,
                volatile_regs: state.volatile,
            }
        } else {
            BlockFolder {
                reg_value: HashMap::new(),
                volatile_regs: fn_volatile.clone(),
            }
        };
        match block.end {
            BlockEnd::Branch(_, a, b) => {
                folder.rewrite_block(block);
                self.got_entry_state(a, &folder);
                self.got_entry_state(b, &folder);
            }
            BlockEnd::Jump(a) => {
                folder.rewrite_block(block);
                self.got_entry_state(a, &folder);
            }
            _ => {
                folder.rewrite_block(block);
            }
        }
    }
}

impl Rewriter for Folder {
    fn rewrite_function(&mut self, f: &mut Function) {
        if let Some(block) = f.start_block {
            self.add_block_entry(block);
        }
        for block in f.blocks.values() {
            match block.end {
                BlockEnd::Branch(_, a, b) => {
                    self.add_block_entry(a);
                    self.add_block_entry(b);
                }
                BlockEnd::Jump(a) => {
                    self.add_block_entry(a);
                }
                _ => {}
            }
        }
        let fn_volatile_regs = VolatileGather::volatile_regs(f);
        if let Some(id) = f.start_block {
            let block = f.blocks.get_mut(&id).unwrap();
            self.run_block(id, block, &fn_volatile_regs);
        }
        loop {
            let mut best = None;
            for &id in f.blocks.keys() {
                match (best, self.block_states.get(&id)) {
                    (_, None) => {}
                    (None, Some(state)) => {
                        best = Some((id, state.entries_left));
                        if state.entries_left == 0 {
                            break;
                        }
                    }
                    (Some((_, e)), Some(state)) => {
                        if state.entries_left == 0 {
                            best = Some((id, state.entries_left));
                            break;
                        } else if state.entries_left > e {
                            best = Some((id, state.entries_left));
                        }
                    }
                }
            }
            if let Some((id, _)) = best {
                let block = f.blocks.get_mut(&id).unwrap();
                self.run_block(id, block, &fn_volatile_regs);
            } else {
                break;
            }
        }
    }
}

pub fn rewrite(program: &mut Program) {
    Folder::default().rewrite_program(program);
}
