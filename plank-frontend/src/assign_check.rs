use std::collections::{HashMap, HashSet};
use plank_syntax::position::Spanned;
use ast::cfg::{Program, Function, Block, Reg, Instruction, Value, BlockId, BlockEnd};
use CompileCtx;


#[derive(PartialEq, Eq, Debug, Copy, Clone)]
enum VarState {
    Unassigned(u32),
    MaybeAssigned(u32),
    Assigned,
}

impl VarState {
    fn from_entries(entries: u32) -> VarState {
        if entries > 0 {
            VarState::Unassigned(entries)
        } else {
            VarState::Assigned
        }
    }

    fn reduce(&mut self) -> bool {
        match *self {
            VarState::Assigned => false,
            VarState::MaybeAssigned(1) |
            VarState::Unassigned(1) => {
                *self = VarState::Assigned;
                true
            }
            VarState::MaybeAssigned(ref mut x) => {
                debug_assert!(*x > 1);
                *x -= 1;
                false
            }
            VarState::Unassigned(x) => {
                debug_assert!(x > 1);
                *self = VarState::MaybeAssigned(x - 1);
                false
            }
        }
    }
}

struct Context<'a> {
    ctx: &'a mut CompileCtx,
    function: &'a Function,
    reported_regs: HashSet<Reg>,
    start_state: HashMap<(Reg, BlockId), VarState>,
}

impl<'a> Context<'a> {
    fn new(function: &'a Function, ctx: &'a mut CompileCtx) -> Self {
        Context {
            ctx,
            function,
            reported_regs: HashSet::new(),
            start_state: HashMap::new(),
        }
    }

    fn collect_assigned(&self, block: &Block) -> HashSet<Reg> {
        let mut assigned = HashSet::new();
        for op in &block.ops {
            match **op {
                Instruction::Init(reg) |
                Instruction::Assign(reg, _) |
                Instruction::BinaryOp(reg, _, _, _) |
                Instruction::Call(reg, _, _) |
                Instruction::UnaryOp(reg, _, _) |
                Instruction::TakeAddress(reg, _, _) |
                Instruction::CastAssign(reg, _) => {
                    assigned.insert(reg);
                }
                Instruction::Error |
                Instruction::Drop(_) |
                Instruction::DerefStore(_, _, _, _) |
                Instruction::FieldStore(_, _, _) |
                Instruction::StartStatement => {}
            }
        }
        assigned
    }

    fn check_block(&mut self, id: BlockId, block: &Block) {
        let mut assigned = HashSet::new();
        for op in &block.ops {
            match **op {
                Instruction::Assign(_, ref val) |
                Instruction::UnaryOp(_, _, ref val) |
                Instruction::FieldStore(_, _, ref val) |
                Instruction::CastAssign(_, ref val) => {
                    self.check_value(val, id, &assigned);
                }
                Instruction::DerefStore(ref a, _, _, ref b) |
                Instruction::BinaryOp(_, _, ref a, ref b) => {
                    self.check_value(a, id, &assigned);
                    self.check_value(b, id, &assigned);
                }
                Instruction::Call(_, ref value, ref params) => {
                    self.check_value(value, id, &assigned);
                    for param in params {
                        self.check_value(param, id, &assigned);
                    }
                }
                Instruction::Error |
                Instruction::StartStatement |
                Instruction::Drop(_) |
                Instruction::Init(_) => {}
                Instruction::TakeAddress(_, reg, _) => {
                    let val = Spanned::map(reg, Value::Reg);
                    self.check_value(&val, id, &assigned);
                }
            }
            match **op {
                Instruction::Init(reg) |
                Instruction::Assign(reg, _) |
                Instruction::BinaryOp(reg, _, _, _) |
                Instruction::Call(reg, _, _) |
                Instruction::UnaryOp(reg, _, _) |
                Instruction::TakeAddress(reg, _, _) |
                Instruction::CastAssign(reg, _) => {
                    assigned.insert(reg);
                }
                Instruction::Error |
                Instruction::Drop(_) |
                Instruction::DerefStore(_, _, _, _) |
                Instruction::FieldStore(_, _, _) |
                Instruction::StartStatement => {}
            }
        }
    }

    fn check_value(&mut self, value: &Spanned<Value>, block: BlockId, block_assigned: &HashSet<Reg>) {
        match **value {
            Value::Int(_, _) |
            Value::Symbol(_, _) |
            Value::Bytes(_) |
            Value::Unit |
            Value::Error => {}
            Value::Reg(reg) if self.reported_regs.contains(&reg) => {}
            Value::Reg(reg) if block_assigned.contains(&reg) => {}
            Value::Reg(reg) => {
                let var_symbol = self.function.register_symbols[&reg];
                let name = self.ctx.symbols.get_name(var_symbol);
                println!("state of {:?}, block {:?}", reg, block);
                let msg = match self.start_state[&(reg, block)] {
                    VarState::Assigned => return,
                    VarState::Unassigned(_) => format!(
                        "var `{}` is not initialized before usage",
                        name,
                    ),
                    VarState::MaybeAssigned(_) => format!(
                        "var `{}` might be uninitialized here",
                        name,
                    ),
                };
                let span = Spanned::span(value);
                self.ctx
                    .reporter
                    .error(msg, span)
                    .span(span)
                    .build();
                self.reported_regs.insert(reg);
            }
        }
    }

    fn visit_start(&mut self, block: BlockId, reg: Reg) {
        if self.start_state.get_mut(&(reg, block)).unwrap().reduce() {
            match self.function.blocks[&block].end {
                BlockEnd::Jump(to) => {
                    self.visit_start(to, reg);
                }
                BlockEnd::Branch(_, a, b) => {
                    self.visit_start(a, reg);
                    self.visit_start(b, reg);
                }
                BlockEnd::Return(_) | BlockEnd::Error => {}
            }
        }
    }

    fn count_entries(&self) -> HashMap<BlockId, u32> {
        let mut entries = HashMap::new();
        for (&id, block) in &self.function.blocks {
            entries.entry(id).or_insert(0);
            match block.end {
                BlockEnd::Jump(to) => {
                    *entries.entry(to).or_insert(0) += 1;
                }
                BlockEnd::Branch(_, a, b) => {
                    *entries.entry(a).or_insert(0) += 1;
                    *entries.entry(b).or_insert(0) += 1;
                }
                BlockEnd::Return(_) | BlockEnd::Error => {}
            }
        }
        entries
    }

    fn check_function(&mut self) {
        for (block, entries) in self.count_entries() {
            for &reg in self.function.registers.keys() {
                self.start_state.insert((reg, block), VarState::from_entries(entries));
            }
        }
        if let Some(block) = self.function.start_block {
            for &reg in &self.function.parameters {
                self.start_state.insert((reg, block), VarState::Unassigned(1));
                self.visit_start(block, reg);
            }
        }
        for block in self.function.blocks.values() {
            for reg in self.collect_assigned(block) {
                match block.end {
                    BlockEnd::Jump(to) => {
                        self.visit_start(to, reg);
                    }
                    BlockEnd::Branch(_, a, b) => {
                        self.visit_start(a, reg);
                        self.visit_start(b, reg);
                    }
                    BlockEnd::Return(_) | BlockEnd::Error => {}
                }
            }
        }
        for (&id, block) in &self.function.blocks {
            self.check_block(id, block);
        }
    }
}

pub(crate) fn check_program(program: &Program, ctx: &mut CompileCtx) {
    for f in program.functions.values() {
        let mut ctx = Context::new(f, ctx);
        ctx.check_function();
    }
}
