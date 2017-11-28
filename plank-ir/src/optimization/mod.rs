mod constant_fold;
mod simplify_newtypes;
mod dead_store_elimination;
mod intermediate_removal;

use analysis::Loc;
use ir::{Program, Function, BlockId, Block, Instruction};


trait Rewriter {
    fn rewrite_program(&mut self, program: &mut Program) {
        rewrite_program(self, program);
    }

    fn rewrite_function(&mut self, f: &mut Function) {
        rewrite_function(self, f);
    }

    fn rewrite_block(&mut self, id: BlockId, block: &mut Block) {
        rewrite_block(self, id, block);
    }

    fn rewrite_instruction(&mut self, _loc: Loc, _instr: &mut Instruction) {}
}

fn rewrite_program<R: Rewriter + ?Sized>(r: &mut R, program: &mut Program) {
    for f in program.functions.values_mut() {
        r.rewrite_function(f);
    }
}

fn rewrite_function<R: Rewriter + ?Sized>(r: &mut R, f: &mut Function) {
    for (&id, block) in &mut f.blocks {
        r.rewrite_block(id, block);
    }
}

fn rewrite_block<R: Rewriter + ?Sized>(r: &mut R, id: BlockId, block: &mut Block) {
    for (index, i) in block.ops.iter_mut().enumerate() {
        let loc = Loc { block: id, pos: index };
        r.rewrite_instruction(loc, i);
    }
}

pub fn optimize(program: &mut Program) {
    simplify_newtypes::rewrite(program);
    intermediate_removal::rewrite(program);
    constant_fold::rewrite(program);
    dead_store_elimination::rewrite(program);
}
