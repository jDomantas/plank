use super::Rewriter;
use analysis::Loc;
use ir::{Block, BlockEnd, BlockId, Function, Instruction, Program, Reg, Value};
use std::collections::{HashMap, HashSet};

struct RemoveNops;

impl Rewriter for RemoveNops {
    fn rewrite_block(&mut self, _id: BlockId, block: &mut Block) {
        block.ops.retain(|o| !matches!(*o, Instruction::Nop));
    }
}

struct ShortenUnreachable;

impl Rewriter for ShortenUnreachable {
    fn rewrite_block(&mut self, _id: BlockId, block: &mut Block) {
        let mut to_keep = None;
        for (index, op) in block.ops.iter().enumerate() {
            if let Instruction::Unreachable = *op {
                to_keep = Some(index);
            }
        }
        if let Some(index) = to_keep {
            block.ops.truncate(index);
            block.end = BlockEnd::Unreachable;
        }
    }
}

#[derive(Default)]
struct JoinBlocks {
    references: HashMap<BlockId, u32>,
}

impl Rewriter for JoinBlocks {
    fn rewrite_function(&mut self, f: &mut Function) {
        self.references.clear();
        for block in f.blocks.values() {
            match block.end {
                BlockEnd::Branch(_, a, b) => {
                    *self.references.entry(a).or_insert(0) += 1;
                    *self.references.entry(b).or_insert(0) += 1;
                }
                BlockEnd::Jump(a) => {
                    *self.references.entry(a).or_insert(0) += 1;
                }
                BlockEnd::Return(_) | BlockEnd::ReturnProc | BlockEnd::Unreachable => {}
            }
        }
        if let Some(block) = f.start_block {
            *self.references.entry(block).or_insert(0) += 1;
        }
        loop {
            enum Change {
                Remove(BlockId),
                Join(BlockId, BlockId),
                None,
            }
            let mut change = Change::None;
            for (&id, block) in &f.blocks {
                if *self.references.entry(id).or_insert(0) == 0 {
                    change = Change::Remove(id);
                    break;
                }
                if let BlockEnd::Jump(to) = block.end {
                    if *self.references.entry(to).or_insert(0) == 1 {
                        change = if id == to {
                            Change::Remove(id)
                        } else {
                            Change::Join(id, to)
                        };
                        break;
                    }
                }
            }
            match change {
                Change::Join(a, b) => {
                    let removed = f.blocks.remove(&b).unwrap();
                    let first = f.blocks.get_mut(&a).unwrap();
                    first.ops.extend(removed.ops);
                    match first.end {
                        BlockEnd::Branch(_, a, b) => {
                            *self.references.get_mut(&a).unwrap() -= 1;
                            *self.references.get_mut(&b).unwrap() -= 1;
                        }
                        BlockEnd::Jump(a) => {
                            *self.references.get_mut(&a).unwrap() -= 1;
                        }
                        BlockEnd::Return(_) | BlockEnd::ReturnProc | BlockEnd::Unreachable => {}
                    }
                    first.end = removed.end;
                }
                Change::Remove(block) => {
                    let block = f.blocks.remove(&block).unwrap();
                    match block.end {
                        BlockEnd::Branch(_, a, b) => {
                            *self.references.get_mut(&a).unwrap() -= 1;
                            *self.references.get_mut(&b).unwrap() -= 1;
                        }
                        BlockEnd::Jump(a) => {
                            *self.references.get_mut(&a).unwrap() -= 1;
                        }
                        BlockEnd::Return(_) | BlockEnd::ReturnProc | BlockEnd::Unreachable => {}
                    }
                }
                Change::None => break,
            }
        }
    }
}

#[derive(Default)]
struct RemoveUnusedRegs {
    used: HashSet<Reg>,
}

impl RemoveUnusedRegs {
    fn track(&mut self, reg: Reg) {
        self.used.insert(reg);
    }

    fn track_val(&mut self, val: &Value) {
        if let Value::Reg(r) = *val {
            self.track(r);
        }
    }

    fn track_values(&mut self, values: &[Value]) {
        for val in values {
            self.track_val(val);
        }
    }
}

impl Rewriter for RemoveUnusedRegs {
    fn rewrite_function(&mut self, f: &mut Function) {
        self.used.clear();
        super::rewrite_function(self, f);
        for &r in &f.parameters {
            self.track(r);
        }
        f.registers.retain(|&r, _| self.used.contains(&r));
    }

    fn rewrite_instruction(&mut self, _loc: Loc, instr: &mut Instruction) {
        match *instr {
            Instruction::Assign(r, ref val)
            | Instruction::CastAssign(r, ref val)
            | Instruction::DerefLoad(r, ref val, _)
            | Instruction::Store(r, _, ref val)
            | Instruction::UnaryOp(r, _, ref val) => {
                self.track(r);
                self.track_val(val);
            }
            Instruction::BinaryOp(r, _, ref a, ref b) => {
                self.track(r);
                self.track_val(a);
                self.track_val(b);
            }
            Instruction::Call(r, _, ref params) => {
                self.track(r);
                self.track_values(params);
            }
            Instruction::CallProc(_, ref params) => {
                self.track_values(params);
            }
            Instruction::CallProcVirt(ref f, ref params) => {
                self.track_val(f);
                self.track_values(params);
            }
            Instruction::CallVirt(r, ref f, ref params) => {
                self.track(r);
                self.track_val(f);
                self.track_values(params);
            }
            Instruction::DerefStore(ref a, _, ref b) => {
                self.track_val(a);
                self.track_val(b);
            }
            Instruction::Drop(r) | Instruction::Init(r) => {
                self.track(r);
            }
            Instruction::Load(r1, r2, _) | Instruction::TakeAddress(r1, r2, _) => {
                self.track(r1);
                self.track(r2);
            }
            Instruction::Nop | Instruction::Unreachable => {}
        }
    }
}

pub fn rewrite(program: &mut Program) {
    RemoveNops.rewrite_program(program);
    JoinBlocks::default().rewrite_program(program);
    ShortenUnreachable.rewrite_program(program);
    RemoveUnusedRegs::default().rewrite_program(program);
}
