use std::collections::{HashSet, VecDeque};
use ast::cfg::{Block, BlockEnd, Function, Instruction, Program};
use CompileCtx;


fn has_error_statement(block: &Block) -> bool {
    for op in &block.ops {
        match **op {
            Instruction::Error => return true,
            _ => {}
        }
    }
    false
}

fn check_function(f: &Function, ctx: &mut CompileCtx) {
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();
    if let Some(start_block) = f.start_block {
        queue.push_back(start_block);
    }

    while let Some(block) = queue.pop_front() {
        if visited.contains(&block) {
            continue;
        }
        visited.insert(block);
        let block = &f.blocks[&block];
        if has_error_statement(block) {
            continue;
        }
        match block.end {
            BlockEnd::Error => {
                let span = f.complete_span;
                ctx.reporter
                    .error("not all paths return a value", span)
                    .span(span)
                    .build();
                return;
            }
            BlockEnd::Branch(_, a, b) => {
                queue.push_back(a);
                queue.push_back(b);
            }
            BlockEnd::Jump(a) => {
                queue.push_back(a);
            }
            BlockEnd::Return(_) => {}
        }
    }
}


pub(crate) fn check_returns(program: &Program, ctx: &mut CompileCtx) {
    for (_, f) in &program.functions {
        check_function(f, ctx);
    }
}
