use analysis::{self, Loc};
use ir::{
    BinaryOp, BitOp, Block, BlockEnd, BlockId, Function, Instruction, IntOp, Program, Reg,
    Signedness, Size, UnaryOp, Value,
};
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
enum Val {
    Ir(Value),
    Ref(Reg, u32),
    Unknown,
    Any,
}

impl Val {
    fn from_ir(v: &Value) -> Val {
        if let Value::Reg(_) = *v {
            Val::Unknown
        } else {
            Val::Ir(v.clone())
        }
    }

    fn merge(self, other: Val) -> Val {
        match (self, other) {
            (Val::Any, other) | (other, Val::Any) => other,
            (Val::Unknown, _) | (_, Val::Unknown) => Val::Unknown,
            (Val::Ref(a, oa), Val::Ref(b, ob)) if (a, oa) == (b, ob) => Val::Ref(a, oa),
            (Val::Ir(a), Val::Ir(b)) => {
                if a == b {
                    Val::Ir(a)
                } else {
                    Val::Unknown
                }
            }
            _ => Val::Unknown,
        }
    }
}

struct Context<'a> {
    volatile: HashMap<Reg, HashSet<Loc>>,
    f: &'a Function,
    incoming: &'a HashMap<BlockId, Vec<BlockId>>,
    visited: HashSet<Loc>,
}

impl<'a> Context<'a> {
    fn is_volatile_at(&self, reg: Reg, loc: Loc) -> bool {
        if let Some(locs) = self.volatile.get(&reg) {
            locs.contains(&loc)
        } else {
            false
        }
    }

    fn dfs(&mut self, reg: Reg, block_id: BlockId, block: &Block, mut pos: usize) -> Val {
        loop {
            let loc = Loc {
                block: block_id,
                pos,
            };
            if self.visited.contains(&loc) {
                return Val::Any;
            }
            self.visited.insert(loc);
            if pos == 0 {
                let mut total = if self.f.start_block == Some(block_id) {
                    Val::Unknown
                } else {
                    Val::Any
                };
                if let Some(inc) = self.incoming.get(&block_id) {
                    for &parent in inc {
                        total = total.merge(self.dfs_block_end(reg, parent));
                    }
                }
                return total;
            } else {
                let loc = Loc {
                    block: block_id,
                    pos: pos - 1,
                };
                let op = &block.ops[pos - 1];
                match *op {
                    Instruction::Assign(r, ref val) | Instruction::CastAssign(r, ref val)
                        if r == reg =>
                    {
                        return Val::from_ir(val)
                    }
                    Instruction::BinaryOp(r, _, _, _)
                    | Instruction::DerefLoad(r, _, _)
                    | Instruction::Load(r, _, _)
                    | Instruction::Store(r, _, _)
                        if r == reg =>
                    {
                        return Val::Unknown
                    }
                    Instruction::Call(r, _, _) | Instruction::CallVirt(r, _, _) => {
                        if r == reg || self.is_volatile_at(reg, loc) {
                            return Val::Unknown;
                        }
                    }
                    Instruction::CallProc(_, _)
                    | Instruction::CallProcVirt(_, _)
                    | Instruction::DerefStore(_, _, _) => {
                        if self.is_volatile_at(reg, loc) {
                            return Val::Unknown;
                        }
                    }
                    Instruction::Drop(r) | Instruction::Init(r) if r == reg => {
                        return Val::Any;
                    }
                    Instruction::TakeAddress(r, r2, offset) => {
                        if r == reg {
                            return Val::Ref(r2, offset);
                        }
                    }
                    _ => {}
                }
                pos -= 1;
            }
        }
    }

    fn dfs_block_end(&mut self, reg: Reg, block_id: BlockId) -> Val {
        let block = &self.f.blocks[&block_id];
        let pos = block.ops.len();
        self.dfs(reg, block_id, block, pos)
    }

    fn get_value(&mut self, reg: Reg, loc: Loc) -> Val {
        self.visited.clear();
        let block = &self.f.blocks[&loc.block];
        self.dfs(reg, loc.block, block, loc.pos)
    }
}

fn build_context<'a>(
    f: &'a Function,
    incoming: &'a mut HashMap<BlockId, Vec<BlockId>>,
) -> Context<'a> {
    incoming.clear();
    for (&id, block) in &f.blocks {
        match block.end {
            BlockEnd::Branch(_, a, b) => {
                incoming.entry(a).or_insert_with(Vec::new).push(id);
                incoming.entry(b).or_insert_with(Vec::new).push(id);
            }
            BlockEnd::Jump(a) => {
                incoming.entry(a).or_insert_with(Vec::new).push(id);
            }
            _ => {}
        }
    }
    Context {
        volatile: analysis::volatility::volatile_locations(f),
        f,
        incoming,
        visited: HashSet::new(),
    }
}

fn try_replace_val(val: &mut Value, ctx: &mut Context, loc: Loc) -> bool {
    if let Value::Reg(r) = *val {
        match ctx.get_value(r, loc) {
            Val::Any => {
                *val = Value::Undef;
                true
            }
            Val::Ir(v) => {
                *val = v;
                true
            }
            Val::Ref(_, _) | Val::Unknown => false,
        }
    } else {
        false
    }
}

fn rewrite_function(f: &mut Function) {
    loop {
        let mut changed = false;
        let mut incoming = HashMap::new();
        let analyzed_f = f.clone();
        let ctx = &mut build_context(&analyzed_f, &mut incoming);
        for (&id, block) in &mut f.blocks {
            for (pos, op) in block.ops.iter_mut().enumerate() {
                let loc = Loc { block: id, pos };
                let mut replace_with = None;
                match *op {
                    Instruction::Assign(_, ref mut val)
                    | Instruction::CastAssign(_, ref mut val) => {
                        if try_replace_val(val, ctx, loc) {
                            changed = true;
                        }
                    }
                    Instruction::BinaryOp(r, op, ref mut a, ref mut b) => {
                        if let Some(result) = eval_binary_op(op, a, b) {
                            replace_with = Some(Instruction::Assign(r, result));
                        } else {
                            if try_replace_val(a, ctx, loc) {
                                changed = true;
                            }
                            if try_replace_val(b, ctx, loc) {
                                changed = true;
                            }
                        }
                    }
                    Instruction::Call(_, _, ref mut params)
                    | Instruction::CallProc(_, ref mut params) => {
                        for param in params {
                            if try_replace_val(param, ctx, loc) {
                                changed = true;
                            }
                        }
                    }
                    Instruction::CallVirt(_, ref mut f, ref mut params)
                    | Instruction::CallProcVirt(ref mut f, ref mut params) => {
                        if try_replace_val(f, ctx, loc) {
                            changed = true;
                        }
                        for param in params {
                            if try_replace_val(param, ctx, loc) {
                                changed = true;
                            }
                        }
                    }
                    Instruction::DerefLoad(r, Value::Reg(r2), 0) => match ctx.get_value(r2, loc) {
                        Val::Any => replace_with = Some(Instruction::Unreachable),
                        Val::Ir(v) => replace_with = Some(Instruction::DerefLoad(r, v, 0)),
                        Val::Ref(r3, 0) => {
                            replace_with = Some(Instruction::Assign(r, Value::Reg(r3)))
                        }
                        Val::Ref(_, _) | Val::Unknown => {}
                    },
                    Instruction::DerefStore(ref to, offset, ref mut val) => {
                        if try_replace_val(val, ctx, loc) {
                            changed = true;
                        }
                        if let Value::Reg(r) = *to {
                            if offset == 0 {
                                match ctx.get_value(r, loc) {
                                    Val::Any => replace_with = Some(Instruction::Unreachable),
                                    Val::Ir(v) => {
                                        replace_with =
                                            Some(Instruction::DerefStore(v, 0, val.clone()))
                                    }
                                    Val::Ref(r, 0) => {
                                        replace_with = Some(Instruction::Assign(r, val.clone()))
                                    }
                                    Val::Ref(_, _) | Val::Unknown => {}
                                }
                            }
                        }
                    }
                    Instruction::DerefLoad(_, _, _)
                    | Instruction::Drop(_)
                    | Instruction::Init(_)
                    | Instruction::Nop
                    | Instruction::Load(_, _, _)
                    | Instruction::Store(_, _, _)
                    | Instruction::TakeAddress(_, _, _)
                    | Instruction::Unreachable => {}
                    Instruction::UnaryOp(r, op, ref mut val) => {
                        if let Some(result) = eval_unary_op(op, val) {
                            replace_with = Some(Instruction::Assign(r, result));
                        } else if try_replace_val(val, ctx, loc) {
                            changed = true;
                        }
                    }
                }
                if let Some(new_op) = replace_with {
                    *op = new_op;
                    changed = true;
                }
            }
            let loc = Loc {
                block: id,
                pos: block.ops.len(),
            };
            match block.end {
                BlockEnd::Branch(ref mut val, _, _) | BlockEnd::Return(ref mut val) => {
                    if try_replace_val(val, ctx, loc) {
                        changed = true;
                    }
                }
                BlockEnd::Jump(_) | BlockEnd::ReturnProc | BlockEnd::Unreachable => {}
            }
            if let BlockEnd::Branch(Value::Int(0, _), _, a) = block.end {
                block.end = BlockEnd::Jump(a);
                changed = true;
            } else if let BlockEnd::Branch(Value::Int(_, _), a, _) = block.end {
                block.end = BlockEnd::Jump(a);
                changed = true;
            }
        }
        if !changed {
            break;
        }
    }
}

fn eval_binary_op(op: BinaryOp, a: &Value, b: &Value) -> Option<Value> {
    let (a, b) = match (a, b) {
        (&Value::Int(a, _), &Value::Int(b, _)) => (a, b),
        _ => return None,
    };
    match op {
        BinaryOp::BitOp(BitOp::And, size) => Some(Value::Int(a & b, size)),
        BinaryOp::BitOp(BitOp::Or, size) => Some(Value::Int(a | b, size)),
        BinaryOp::BitOp(BitOp::Xor, size) => Some(Value::Int(a ^ b, size)),
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

pub fn rewrite(program: &mut Program) {
    for f in program.functions.values_mut() {
        rewrite_function(f);
    }
}
