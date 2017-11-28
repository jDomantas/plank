use std::collections::{HashMap, HashSet};
use ir::{Function, Instruction, Reg, Value, BlockEnd};
use analysis::Loc;


pub struct Context<'a> {
    f: &'a Function,
    volatile: &'a HashMap<Reg, HashSet<Loc>>,
}

impl<'a> Context<'a> {
    pub fn new(f: &'a Function, volatile: &'a HashMap<Reg, HashSet<Loc>>) -> Self {
        Context {
            f,
            volatile,
        }
    }

    pub fn is_value_used(&self, loc: Loc, reg: Reg) -> bool {
        let mut visited = HashSet::new();
        let mut frontier = Vec::new();
        frontier.push(loc);
        while let Some(loc) = frontier.pop() {
            if visited.contains(&loc) {
                continue;
            }
            visited.insert(loc);
            let block = &self.f.blocks[&loc.block];
            if loc.pos == block.ops.len() {
                match block.end {
                    BlockEnd::Branch(ref val, a, b) => {
                        if is_used_in_val(reg, val) {
                            return true;
                        }
                        frontier.push(Loc {
                            block: a,
                            pos: 0,
                        });
                        frontier.push(Loc {
                            block: b,
                            pos: 0,
                        });
                    }
                    BlockEnd::Jump(a) => {
                        frontier.push(Loc {
                            block: a,
                            pos: 0,
                        });
                    }
                    BlockEnd::Return(ref val) => {
                        if is_used_in_val(reg, val) {
                            return true;
                        }
                    }
                    BlockEnd::ReturnProc => {}
                }
            } else {
                match block.ops[loc.pos] {
                    Instruction::Assign(r, ref val) |
                    Instruction::CastAssign(r, ref val) |
                    Instruction::UnaryOp(r, _, ref val) => {
                        if is_used_in_val(reg, val) {
                            return true;
                        }
                        if r == reg {
                            continue;
                        }
                    }
                    Instruction::BinaryOp(r, _, ref a, ref b) => {
                        if is_used_in_val(reg, a) {
                            return true;
                        }
                        if is_used_in_val(reg, b) {
                            return true;
                        }
                        if r == reg {
                            continue;
                        }
                    }
                    Instruction::Call(r, _, ref params) => {
                        for param in params {
                            if is_used_in_val(reg, param) {
                                return true;
                            }
                        }
                        if r == reg {
                            continue;
                        }
                    }
                    Instruction::CallProc(_, ref params) => {
                        for param in params {
                            if is_used_in_val(reg, param) {
                                return true;
                            }
                        }
                    }
                    Instruction::CallVirt(r, ref f, ref params) => {
                        for param in params {
                            if is_used_in_val(reg, param) {
                                return true;
                            }
                        }
                        if is_used_in_val(reg, f) {
                            return true;
                        }
                        if r == reg {
                            continue;
                        }
                    }
                    Instruction::CallProcVirt(ref f, ref params) => {
                        for param in params {
                            if is_used_in_val(reg, param) {
                                return true;
                            }
                        }
                        if is_used_in_val(reg, f) {
                            return true;
                        }
                    }
                    Instruction::DerefLoad(r, ref val, _) => {
                        if is_used_in_val(reg, val) {
                            return true;
                        }
                        if self.is_volatile_at(reg, loc) {
                            return true;
                        }
                        if r == reg {
                            continue;
                        }
                    }
                    Instruction::DerefStore(ref to, _, ref val) => {
                        if is_used_in_val(reg, to) {
                            return true;
                        }
                        if is_used_in_val(reg, val) {
                            return true;
                        }
                    }
                    Instruction::Drop(r) |
                    Instruction::Init(r) |
                    Instruction::TakeAddress(r, _, _) => {
                        if r == reg {
                            continue;
                        }
                    }
                    Instruction::Load(r, from, _) => {
                        if from == reg {
                            return true;
                        }
                        if r == reg {
                            continue;
                        }
                    }
                    Instruction::Nop => {}
                    Instruction::Store(_, _, ref val) => {
                        if is_used_in_val(reg, val) {
                            return true;
                        }
                    }
                    Instruction::Unreachable => {
                        return false;
                    }
                }
                frontier.push(Loc {
                    block: loc.block,
                    pos: loc.pos + 1,
                });
            }
        }
        false
    }

    fn is_volatile_at(&self, reg: Reg, at: Loc) -> bool {
        if let Some(locs) = self.volatile.get(&reg) {
            locs.contains(&at)
        } else {
            false
        }
    }
}

fn is_used_in_val(reg: Reg, val: &Value) -> bool {
    val == &Value::Reg(reg)
}
