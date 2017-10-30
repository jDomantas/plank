use std::collections::HashMap;
use plank_syntax::position::Spanned;
use ast::cfg::{Block, BlockEnd, BlockId, BlockLink, Function, Instruction, Program, Reg, Value};
use ast::typed::{self as t, Struct};


fn generate_constructor(s: &Struct) -> Function {
    let mut ops = Vec::new();
    let output = Reg(0);
    ops.push(Spanned::new(Instruction::Init(Reg(0)), s.complete_span));
    for i in 0..(s.fields.len()) {
        let i = Instruction::FieldStore(
            Spanned::new(Reg(0), s.complete_span),
            vec![i],
            Spanned::new(Value::Reg(Reg(i as u32 + 1)), s.complete_span),
        );
        let i = Spanned::new(i, s.complete_span);
        ops.push(i);
    }
    let parameters = (1..(s.fields.len() as u32 + 1))
        .into_iter()
        .map(Reg)
        .collect();
    let mut registers = (1..(s.fields.len() + 1))
        .into_iter()
        .map(|i| (Reg(i as u32), s.fields[i - 1].typ.clone()))
        .collect::<HashMap<_, _>>();
    let complete_type = t::Type::Concrete(
        s.name,
        s.type_params
            .iter()
            .map(|&s| t::Type::Concrete(s, Vec::new().into()))
            .collect::<Vec<_>>()
            .into(),
    );
    registers.insert(Reg(0), complete_type.clone());
    let block = Block {
        ops,
        link: BlockLink::None,
        end: BlockEnd::Return(Spanned::new(Value::Reg(output), s.complete_span)),
    };
    let mut blocks = HashMap::new();
    blocks.insert(BlockId(0), block);
    Function {
        complete_span: s.complete_span,
        parameters,
        registers,
        register_symbols: HashMap::new(),
        type_params: s.type_params.clone(),
        out_type: complete_type,
        start_block: Some(BlockId(0)),
        blocks,
    }
}

pub(crate) fn add_constructors(program: &mut Program) {
    for (id, s) in &program.structs {
        let ctor = generate_constructor(s);
        program.functions.insert(*id, ctor);
    }
}
