use std::collections::{HashSet, VecDeque};
use plank_syntax::position::Spanned;
use ast::cfg::{Block, BlockEnd, Function, Instruction, Program, Type, Value};
use CompileCtx;


fn has_error_statement(block: &Block) -> bool {
    for op in &block.ops {
        if let Instruction::Error = **op {
            return true;
        }
    }
    false
}

fn check_function(f: &mut Function, ctx: &mut CompileCtx) {
    let mut queue = VecDeque::new();
    let mut visited = HashSet::new();
    if let Some(start_block) = f.start_block {
        queue.push_back(start_block);
    }
    let allow_no_return = match f.out_type {
        Type::Unit => true,
        _ => false,
    };

    while let Some(block) = queue.pop_front() {
        if visited.contains(&block) {
            continue;
        }
        visited.insert(block);
        let block = f.blocks.get_mut(&block).unwrap();
        if has_error_statement(block) {
            continue;
        }
        match block.end {
            BlockEnd::Error if allow_no_return => {
                // TODO: this is not a very good span for this
                let spanned = Spanned::new(Value::Unit, f.complete_span);
                block.end = BlockEnd::Return(spanned);
            }
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


pub(crate) fn check_returns(program: &mut Program, ctx: &mut CompileCtx) {
    for f in program.functions.values_mut() {
        check_function(f, ctx);
    }
}
