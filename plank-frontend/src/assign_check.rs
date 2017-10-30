use std::collections::{HashMap, HashSet};
use plank_syntax::position::{Span, Spanned};
use ast::cfg::{Program, Function, Block, Reg, Instruction, Value, BlockId, BlockEnd};
use CompileCtx;


struct Context<'a> {
    ctx: &'a mut CompileCtx,
    function: &'a Function,
    reported_regs: HashSet<Reg>,
    live_starts: HashMap<Reg, HashSet<BlockId>>,
}

impl<'a> Context<'a> {
    fn new(function: &'a Function, ctx: &'a mut CompileCtx) -> Self {
        Context {
            ctx,
            function,
            reported_regs: HashSet::new(),
            live_starts: HashMap::new(),
        }
    }

    fn collect_assigned(&self, block: &Block) -> HashSet<Reg> {
        let mut assigned = HashSet::new();
        for op in &block.ops {
            match **op {
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
            let span = Spanned::span(op);
            match **op {
                Instruction::Assign(_, ref val) |
                Instruction::UnaryOp(_, _, ref val) |
                Instruction::FieldStore(_, _, ref val) |
                Instruction::CastAssign(_, ref val) => {
                    self.check_value(val, span, id, &assigned);
                }
                Instruction::DerefStore(ref a, _, _, ref b) |
                Instruction::BinaryOp(_, _, ref a, ref b) => {
                    self.check_value(a, span, id, &assigned);
                    self.check_value(b, span, id, &assigned);
                }
                Instruction::Call(_, ref value, ref params) => {
                    self.check_value(value, span, id, &assigned);
                    for param in params {
                        self.check_value(param, span, id, &assigned);
                    }
                }
                Instruction::Error |
                Instruction::StartStatement |
                Instruction::Drop(_) => {}
                Instruction::TakeAddress(_, reg, _) => {
                    let val = Value::Reg(reg);
                    self.check_value(&val, span, id, &assigned);
                }
            }
            match **op {
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

    fn check_value(&mut self, value: &Value, span: Span, block: BlockId, block_assigned: &HashSet<Reg>) {
        match *value {
            Value::Int(_, _) |
            Value::Symbol(_, _) |
            Value::Bytes(_) |
            Value::Unit |
            Value::Error => {}
            Value::Reg(reg) if self.reported_regs.contains(&reg) => {}
            Value::Reg(reg) if block_assigned.contains(&reg) => {}
            Value::Reg(reg) => {
                let live_start = self.live_starts
                    .get(&reg)
                    .map(|set| set.contains(&block))
                    .unwrap_or(false);
                if !live_start {
                    self.ctx
                        .reporter
                        .error("variable used before being assigned", span)
                        .span(span)
                        .build();
                    self.reported_regs.insert(reg);
                }
            }
        }
    }

    fn add_start(&mut self, block: BlockId, reg: Reg) {
        {
            let starts = self.live_starts.entry(reg).or_insert_with(HashSet::new);
            if starts.contains(&block) {
                return;
            }
            starts.insert(block);
        }
        match self.function.blocks[&block].end {
            BlockEnd::Jump(to) => {
                self.add_start(to, reg);
            }
            BlockEnd::Branch(_, a, b) => {
                self.add_start(a, reg);
                self.add_start(b, reg);
            }
            BlockEnd::Return(_) | BlockEnd::Error => {}
        }
    }

    fn check_function(&mut self) {
        if let Some(block) = self.function.start_block {
            for &param in &self.function.parameters {
                self.add_start(block, param);
            }
        }
        for block in self.function.blocks.values() {
            for reg in self.collect_assigned(block) {
                match block.end {
                    BlockEnd::Jump(to) => {
                        self.add_start(to, reg);
                    }
                    BlockEnd::Branch(_, a, b) => {
                        self.add_start(a, reg);
                        self.add_start(b, reg);
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
