use ast::cfg::{Block, BlockEnd, BlockId, Function, Instruction, Program, Reg, Value};
use plank_syntax::position::Spanned;
use std::collections::{HashMap, HashSet};
use CompileCtx;

struct Context<'a> {
    ctx: &'a mut CompileCtx,
    function: &'a Function,
    reported_regs: HashSet<Reg>,
    assign_position: HashMap<(Reg, BlockId), usize>,
    reachable_labels: HashSet<(Reg, BlockId)>,
}

impl<'a> Context<'a> {
    fn new(function: &'a Function, ctx: &'a mut CompileCtx) -> Self {
        Context {
            ctx,
            function,
            reported_regs: HashSet::new(),
            assign_position: HashMap::new(),
            reachable_labels: HashSet::new(),
        }
    }

    fn store_assigns(&mut self, id: BlockId, block: &Block) {
        for (index, op) in block.ops.iter().enumerate() {
            match **op {
                Instruction::Init(reg)
                | Instruction::Assign(reg, _)
                | Instruction::BinaryOp(reg, _, _, _)
                | Instruction::Call(reg, _, _)
                | Instruction::UnaryOp(reg, _, _)
                | Instruction::TakeAddress(reg, _, _)
                | Instruction::CastAssign(reg, _) => {
                    self.assign_position.entry((reg, id)).or_insert(index);
                }
                Instruction::Error
                | Instruction::Drop(_)
                | Instruction::DerefStore(_, _, _, _)
                | Instruction::FieldStore(_, _, _)
                | Instruction::StartStatement => {}
            }
        }
    }

    fn check_block(&mut self, id: BlockId, block: &Block) {
        for (index, op) in block.ops.iter().enumerate() {
            match **op {
                Instruction::Assign(_, ref val)
                | Instruction::UnaryOp(_, _, ref val)
                | Instruction::CastAssign(_, ref val) => {
                    self.check_value(val, id, index);
                }
                Instruction::FieldStore(reg, _, ref val) => {
                    self.check_value(val, id, index);
                    let reg = Spanned::map(reg, Value::Reg);
                    self.check_value(&reg, id, index);
                }
                Instruction::DerefStore(ref a, _, _, ref b)
                | Instruction::BinaryOp(_, _, ref a, ref b) => {
                    self.check_value(a, id, index);
                    self.check_value(b, id, index);
                }
                Instruction::Call(_, ref value, ref params) => {
                    self.check_value(value, id, index);
                    for param in params {
                        self.check_value(param, id, index);
                    }
                }
                Instruction::TakeAddress(_, reg, _) => {
                    let val = Spanned::map(reg, Value::Reg);
                    self.check_value(&val, id, index);
                }
                Instruction::Error
                | Instruction::StartStatement
                | Instruction::Drop(_)
                | Instruction::Init(_) => {}
            }
        }
    }

    fn check_value(&mut self, value: &Spanned<Value>, block: BlockId, pos: usize) {
        match **value {
            Value::Int(_, _)
            | Value::Symbol(_, _)
            | Value::Bytes(_)
            | Value::Unit
            | Value::Error => {}
            Value::Reg(reg) if self.reported_regs.contains(&reg) => {}
            Value::Reg(reg) => {
                let assign_pos = self.assign_position.get(&(reg, block)).cloned();
                if assign_pos.map(|p| p < pos) == Some(true) {
                    return;
                }
                if !self.reachable_labels.contains(&(reg, block)) {
                    return;
                }
                // occasionally temporary registers happen to be uninitialized
                // (in cases of constness mismatch when taking references),
                // so don't report about that
                if let Some(&var_symbol) = self.function.register_symbols.get(&reg) {
                    let name = self.ctx.symbols.get_name(var_symbol);
                    let msg = format!("var `{}` might be uninitialized here", name);
                    let span = Spanned::span(value);
                    self.ctx.reporter.error(msg, span).span(span).build();
                    self.reported_regs.insert(reg);
                }
            }
        }
    }

    fn visit_label(&mut self, reg: Reg, block: BlockId) {
        if !self.reachable_labels.contains(&(reg, block)) {
            self.reachable_labels.insert((reg, block));
            if self.assign_position.contains_key(&(reg, block)) {
                return;
            }
            match self.function.blocks[&block].end {
                BlockEnd::Jump(to) => {
                    self.visit_label(reg, to);
                }
                BlockEnd::Branch(_, a, b) => {
                    self.visit_label(reg, a);
                    self.visit_label(reg, b);
                }
                BlockEnd::Return(_) | BlockEnd::Error => {}
            }
        }
    }

    fn check_function(&mut self) {
        for (&id, block) in &self.function.blocks {
            self.store_assigns(id, block);
        }
        let parameters = self
            .function
            .parameters
            .iter()
            .cloned()
            .collect::<HashSet<_>>();
        if let Some(block) = self.function.start_block {
            for &reg in self.function.registers.keys() {
                if !parameters.contains(&reg) {
                    self.visit_label(reg, block);
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
