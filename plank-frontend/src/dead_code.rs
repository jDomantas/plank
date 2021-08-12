use ast::cfg::{Block, BlockEnd, BlockId, BlockLink, Function, Instruction, Program};
use plank_syntax::position::Spanned;
use std::collections::{HashSet, VecDeque};
use CompileCtx;

fn function_block_chain(f: &Function) -> VecDeque<BlockId> {
    let mut blocks = VecDeque::new();
    let mut current = if let Some(block) = f.start_block {
        block
    } else {
        return blocks;
    };
    loop {
        blocks.push_back(current);
        match f.blocks[&current].link {
            BlockLink::Strong(next) | BlockLink::Weak(next) => current = next,
            BlockLink::None => break,
        }
    }
    blocks
}

fn report_unreachable(block: &Block, ctx: &mut CompileCtx) {
    let mut span = Spanned::span(&block.ops[0]);
    for i in &block.ops {
        match **i {
            Instruction::StartStatement => {
                span = Spanned::span(i);
                break;
            }
            _ => {
                span = span.merge(Spanned::span(i));
            }
        }
    }
    ctx.reporter
        .warning("dead code detected", span)
        .span(span)
        .build();
}

fn can_be_dead(block: &Block) -> bool {
    for op in &block.ops {
        match **op {
            Instruction::Drop(_) => {}
            _ => return true,
        }
    }
    false
}

fn analyze_function(f: &mut Function, ctx: &mut CompileCtx) {
    let mut blocks = function_block_chain(f);
    debug_assert_eq!(blocks.len(), f.blocks.len());
    let mut reachable = HashSet::new();
    let mut strong_reachable = None;
    let mut queue = VecDeque::new();
    if let Some(block) = f.start_block {
        queue.push_back(block);
    }
    let mut follow_strong = false;
    while !queue.is_empty() {
        while let Some(block) = queue.pop_front() {
            if !reachable.contains(&block) {
                reachable.insert(block);
                let block = &f.blocks[&block];
                match block.end {
                    BlockEnd::Branch(_, a, b) => {
                        queue.push_back(a);
                        queue.push_back(b);
                    }
                    BlockEnd::Jump(next) => {
                        queue.push_back(next);
                    }
                    BlockEnd::Return(_) | BlockEnd::Error => {}
                }
                if follow_strong {
                    if let BlockLink::Strong(next) = block.link {
                        queue.push_back(next);
                    }
                }
            }
        }
        if strong_reachable.is_none() {
            strong_reachable = Some(reachable.clone());
        }
        follow_strong = true;
        while let Some(block) = blocks.pop_front() {
            if !reachable.contains(&block) && can_be_dead(&f.blocks[&block]) {
                report_unreachable(&f.blocks[&block], ctx);
                queue.push_back(block);
                break;
            }
        }
    }
    let strong_reachable = strong_reachable.unwrap_or_default();
    f.blocks.retain(|k, _| strong_reachable.contains(k));
}

pub(crate) fn remove_dead_code(program: &mut Program, ctx: &mut CompileCtx) {
    for f in program.functions.values_mut() {
        analyze_function(f, ctx);
    }
}
