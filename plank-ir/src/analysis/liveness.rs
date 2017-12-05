use std::collections::{HashMap, HashSet};
use ir::{Function, Reg, BlockId, BlockEnd, Instruction};
use super::{Loc, initialized_register};


struct Liveness<'a> {
    function: &'a Function,
    reg: Reg,
    dead_locations: HashSet<Loc>,
    checked_blocks: HashSet<(BlockId, bool)>,
}

impl<'a> Liveness<'a> {
    fn new(function: &'a Function, reg: Reg) -> Self {
        Liveness {
            function,
            reg,
            dead_locations: HashSet::new(),
            checked_blocks: HashSet::new(),
        }
    }

    fn walk_block(&mut self, id: BlockId, live_start: bool) {
        if self.checked_blocks.contains(&(id, live_start)) {
            return;
        }
        self.checked_blocks.insert((id, live_start));
        let block = &self.function.blocks[&id];
        let mut is_live = live_start;
        for (pos, instr) in block.ops.iter().enumerate() {
            if !is_live {
                self.dead_locations.insert(Loc {
                    block: id,
                    pos,
                });
            }
            if initialized_register(instr) == Some(self.reg) {
                is_live = true;
            }
            if let Instruction::Drop(reg) = *instr {
                if reg == self.reg {
                    is_live = false;
                }
            }
        }
        if !is_live {
            self.dead_locations.insert(Loc {
                block: id,
                pos: block.ops.len(),
            });
        }
        match block.end {
            BlockEnd::Branch(_, a, b) => {
                self.walk_block(a, is_live);
                self.walk_block(b, is_live);
            }
            BlockEnd::Jump(a) => {
                self.walk_block(a, is_live);
            }
            BlockEnd::Return(_) |
            BlockEnd::ReturnProc |
            BlockEnd::Unreachable => {}
        }
    }
}

fn get_dead_locations(f: &Function, reg: Reg) -> HashSet<Loc> {
    let mut ctx = Liveness::new(f, reg);
    for &id in f.blocks.keys() {
        let is_start = f.start_block == Some(id);
        ctx.walk_block(id, !is_start || f.parameters.contains(&reg));
    }
    ctx.dead_locations
}

pub fn register_live_locations(f: &Function, reg: Reg) -> HashSet<Loc> {
    let dead_locations = get_dead_locations(f, reg);
    let mut live = HashSet::new();
    for (&id, block) in &f.blocks {
        for pos in 0..(block.ops.len() + 1) {
            let loc = Loc { block: id, pos };
            if !dead_locations.contains(&loc) {
                live.insert(loc);
            }
        }
    }
    live
}

pub fn live_locations(f: &Function) -> HashMap<Reg, HashSet<Loc>> {
    let mut locations = HashMap::new();
    for &reg in f.registers.keys() {
        locations.insert(reg, register_live_locations(f, reg));
    }
    locations
}
