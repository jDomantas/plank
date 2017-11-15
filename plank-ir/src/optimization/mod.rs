mod constant_fold;
mod simplify_newtypes;

use ir::{Program, Function, Block, Instruction};


trait Rewriter {
    fn rewrite_program(&mut self, program: &mut Program) {
        rewrite_program(self, program);
    }

    fn rewrite_function(&mut self, f: &mut Function) {
        rewrite_function(self, f);
    }

    fn rewrite_block(&mut self, block: &mut Block) {
        rewrite_block(self, block);
    }

    fn rewrite_instruction(&mut self, _instr: &mut Instruction) {}
}

fn rewrite_program<R: Rewriter + ?Sized>(r: &mut R, program: &mut Program) {
    for f in program.functions.values_mut() {
        r.rewrite_function(f);
    }
}

fn rewrite_function<R: Rewriter + ?Sized>(r: &mut R, f: &mut Function) {
    for f in f.blocks.values_mut() {
        r.rewrite_block(f);
    }
}

fn rewrite_block<R: Rewriter + ?Sized>(r: &mut R, block: &mut Block) {
    for i in &mut block.ops {
        r.rewrite_instruction(i);
    }
}

pub fn optimize(program: &mut Program) {
    simplify_newtypes::rewrite(program);
    constant_fold::rewrite(program);
}
