use super::Rewriter;
use analysis::{self, Loc};
use ir::{BlockEnd, BlockId, Function, Instruction, Program, Reg};
use std::collections::{HashMap, HashSet};

struct Liveness<'a> {
    function: &'a Function,
    reg: Reg,
    live_locations: HashSet<Loc>,
    checked_blocks: HashSet<(BlockId, bool)>,
}

impl<'a> Liveness<'a> {
    fn new(function: &'a Function, reg: Reg) -> Self {
        Liveness {
            function,
            reg,
            live_locations: HashSet::new(),
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
            if is_live {
                self.live_locations.insert(Loc { block: id, pos });
            }
            if analysis::initialized_register(instr) == Some(self.reg) {
                is_live = true;
            }
            if let Instruction::Drop(reg) = *instr {
                if reg == self.reg {
                    is_live = false;
                }
            }
        }
        if is_live {
            self.live_locations.insert(Loc {
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
            BlockEnd::Return(_) | BlockEnd::ReturnProc | BlockEnd::Unreachable => {}
        }
    }
}

fn get_live_locations(f: &Function, reg: Reg) -> HashSet<Loc> {
    let mut ctx = Liveness::new(f, reg);
    for &id in f.blocks.keys() {
        let is_start = f.start_block == Some(id);
        ctx.walk_block(id, is_start && f.parameters.contains(&reg));
    }
    ctx.live_locations
}

struct Context {
    live: HashMap<Reg, HashSet<Loc>>,
}

impl Rewriter for Context {
    fn rewrite_function(&mut self, f: &mut Function) {
        self.live.clear();
        for &reg in f.registers.keys() {
            self.live.insert(reg, get_live_locations(f, reg));
        }
        super::rewrite_function(self, f);
    }

    fn rewrite_instruction(&mut self, loc: Loc, instr: &mut Instruction) {
        let remove = if let Instruction::Drop(reg) = *instr {
            if let Some(locs) = self.live.get(&reg) {
                !locs.contains(&loc)
            } else {
                true
            }
        } else {
            false
        };
        if remove {
            *instr = Instruction::Nop;
        }
    }
}

pub fn rewrite(program: &mut Program) {
    let mut ctx = Context {
        live: HashMap::new(),
    };
    ctx.rewrite_program(program);
}
