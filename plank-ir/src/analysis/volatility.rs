use super::Loc;
use ir::{BlockEnd, BlockId, Function, Instruction, Reg};
use std::collections::{HashMap, HashSet};

struct Volatility<'a> {
    function: &'a Function,
    reg: Reg,
    volatile_locations: HashSet<Loc>,
    checked_blocks: HashSet<(BlockId, bool)>,
}

impl<'a> Volatility<'a> {
    fn new(function: &'a Function, reg: Reg) -> Self {
        Volatility {
            function,
            reg,
            volatile_locations: HashSet::new(),
            checked_blocks: HashSet::new(),
        }
    }

    fn walk_block(&mut self, id: BlockId, volatile_start: bool) {
        if self.checked_blocks.contains(&(id, volatile_start)) {
            return;
        }
        self.checked_blocks.insert((id, volatile_start));
        let block = &self.function.blocks[&id];
        let mut is_volatile = volatile_start;
        for (pos, instr) in block.ops.iter().enumerate() {
            if is_volatile {
                self.volatile_locations.insert(Loc { block: id, pos });
            }
            if referenced_register(instr) == Some(self.reg) {
                is_volatile = true;
            }
            if let Instruction::Drop(reg) = *instr {
                if reg == self.reg {
                    is_volatile = false;
                }
            }
        }
        if is_volatile {
            self.volatile_locations.insert(Loc {
                block: id,
                pos: block.ops.len(),
            });
        }
        match block.end {
            BlockEnd::Branch(_, a, b) => {
                self.walk_block(a, is_volatile);
                self.walk_block(b, is_volatile);
            }
            BlockEnd::Jump(a) => {
                self.walk_block(a, is_volatile);
            }
            BlockEnd::Return(_) | BlockEnd::ReturnProc | BlockEnd::Unreachable => {}
        }
    }
}

fn referenced_register(instr: &Instruction) -> Option<Reg> {
    match *instr {
        Instruction::TakeAddress(_, reg, _) => Some(reg),
        _ => None,
    }
}

pub fn register_volatile_locations(f: &Function, reg: Reg) -> HashSet<Loc> {
    let mut ctx = Volatility::new(f, reg);
    for &id in f.blocks.keys() {
        ctx.walk_block(id, false);
    }
    ctx.volatile_locations
}

pub fn volatile_locations(f: &Function) -> HashMap<Reg, HashSet<Loc>> {
    let mut locations = HashMap::new();
    for &reg in f.registers.keys() {
        locations.insert(reg, register_volatile_locations(f, reg));
    }
    locations
}
