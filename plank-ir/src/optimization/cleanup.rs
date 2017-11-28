use std::collections::HashMap;
use ir::{Program, Function, Block, BlockId, Instruction, BlockEnd};
use super::Rewriter;


struct RemoveNops;

impl Rewriter for RemoveNops {
    fn rewrite_block(&mut self, _id: BlockId, block: &mut Block) {
        block.ops.retain(|o| if let Instruction::Nop = *o {
            false
        } else {
            true
        });
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
                BlockEnd::Return(_) | BlockEnd::ReturnProc => {}
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
                        BlockEnd::Return(_) | BlockEnd::ReturnProc => {}
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
                        BlockEnd::Return(_) | BlockEnd::ReturnProc => {}
                    }
                }
                Change::None => break,
            }
        }
    }
}

pub fn rewrite(program: &mut Program) {
    RemoveNops.rewrite_program(program);
    JoinBlocks::default().rewrite_program(program);
}
