use std::collections::HashMap;
use plank_syntax::position::{Span, Spanned};
use ast::typed;
pub use ast::typed::{Signedness, Size, Symbol, Type};


#[derive(Debug, Clone)]
pub struct Program {
    pub structs: HashMap<Symbol, typed::Struct>,
    pub functions: HashMap<Symbol, Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub complete_span: Span,
    pub type_params: Vec<Symbol>,
    pub parameters: Vec<Reg>,
    pub out_type: Type,
    pub registers: HashMap<Reg, Type>,
    pub register_symbols: HashMap<Reg, Symbol>,
    pub blocks: HashMap<BlockId, Block>,
    pub start_block: Option<BlockId>,
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Copy, Clone)]
pub struct Reg(pub u32);

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Hash, Copy, Clone)]
pub struct BlockId(pub u32);

#[derive(Debug, Clone)]
pub struct Block {
    pub ops: Vec<Spanned<Instruction>>,
    pub end: BlockEnd,
    pub link: BlockLink,
}

#[derive(Debug, Copy, Clone)]
pub enum BlockLink {
    /// Block would extend this block if not for `return`, `break`, or `continue`
    Strong(BlockId),
    /// Block lexically follows this block, but no real relation
    Weak(BlockId),
    /// No link
    None,
}

#[derive(Debug, Clone)]
pub enum Instruction {
    StartStatement,
    Init(Reg),
    Drop(Reg),
    BinaryOp(Reg, BinaryOp, Spanned<Value>, Spanned<Value>),
    UnaryOp(Reg, UnaryOp, Spanned<Value>),
    Call(Reg, Spanned<Value>, Vec<Spanned<Value>>),
    /// (*value1).field1.field2... = value1
    DerefStore(Spanned<Value>, Type, Vec<usize>, Spanned<Value>),
    /// reg.field1.field2... = value
    FieldStore(Spanned<Reg>, Vec<usize>, Spanned<Value>),
    /// reg = &reg.field1.field2...
    TakeAddress(Reg, Spanned<Reg>, Vec<usize>),
    Assign(Reg, Spanned<Value>),
    CastAssign(Reg, Spanned<Value>),
    Error,
}

#[derive(Debug, Clone)]
pub enum Value {
    Int(u64, Size),
    Reg(Reg),
    Symbol(Symbol, Vec<Type>),
    Bytes(Vec<u8>),
    Unit,
    Error,
}

#[derive(Debug, Clone)]
pub enum BlockEnd {
    Return(Spanned<Value>),
    Jump(BlockId),
    Branch(Spanned<Value>, BlockId, BlockId),
    Error,
}

#[derive(Debug, Copy, Clone)]
pub enum BinaryOp {
    Add(Signedness, Size),
    Sub(Signedness, Size),
    Mul(Signedness, Size),
    Div(Signedness, Size),
    Mod(Signedness, Size),
    Less(Signedness, Size),
    LessEq(Signedness, Size),
    Greater(Signedness, Size),
    GreaterEq(Signedness, Size),
    Eq,
    Neq,
}

#[derive(Debug, Clone)]
pub enum UnaryOp {
    Negate(Signedness, Size),
    Not,
    DerefLoad,
    /// <arg>.field1.field2...
    FieldLoad(Type, Vec<usize>),
    /// &(*<arg>).field1.field2...
    OffsetAddress(Type, Vec<usize>),
}


pub(crate) mod printer {
    #![allow(dead_code)]

    use super::*;
    use CompileCtx;


    fn write_int(
        f: &mut ::std::fmt::Formatter,
        sign: Signedness,
        size: Size,
    ) -> ::std::fmt::Result {
        match sign {
            Signedness::Signed => write!(f, "i")?,
            Signedness::Unsigned => write!(f, "u")?,
        }
        match size {
            Size::Bit8 => write!(f, "8"),
            Size::Bit16 => write!(f, "16"),
            Size::Bit32 => write!(f, "32"),
        }
    }

    fn write_type(
        f: &mut ::std::fmt::Formatter,
        ty: &Type,
        ctx: &CompileCtx,
    ) -> ::std::fmt::Result {
        match *ty {
            Type::Unit => write!(f, "unit"),
            Type::Bool => write!(f, "bool"),
            Type::Concrete(sym, ref params) => {
                write!(f, "{}", ctx.symbols.get_name(sym))?;
                write_type_list(f, params, ctx)
            }
            Type::Error => write!(f, "?"),
            Type::Function(ref _params, ref _out) => unimplemented!(),
            Type::Int(sign, size) => write_int(f, sign, size),
            Type::Pointer(ref to) => {
                write!(f, "*")?;
                write_type(f, to, ctx)
            }
            Type::Var(_) => panic!("found type var"),
        }
    }

    fn write_type_list(
        f: &mut ::std::fmt::Formatter,
        types: &[Type],
        ctx: &CompileCtx,
    ) -> ::std::fmt::Result {
        if types.is_empty() {
            Ok(())
        } else {
            write!(f, "<")?;
            let mut first = true;
            for ty in types {
                if !first {
                    write!(f, ",")?;
                }
                first = false;
                write_type(f, ty, ctx)?;
            }
            write!(f, ">")
        }
    }

    struct ValueDisplay<'a> {
        value: &'a Value,
        ctx: &'a CompileCtx,
    }

    impl<'a> ::std::fmt::Display for ValueDisplay<'a> {
        fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
            match *self.value {
                Value::Error => write!(f, "?"),
                Value::Unit => write!(f, "unit"),
                Value::Int(i, _) => write!(f, "{}", i),
                Value::Reg(reg) => write!(f, "r{}", reg.0),
                Value::Symbol(sym, ref params) => {
                    write!(f, "{}", self.ctx.symbols.get_name(sym))?;
                    write_type_list(f, params, self.ctx)
                }
                Value::Bytes(_) => write!(f, "<bytes>"),
            }
        }
    }

    fn d<'a>(value: &'a Value, ctx: &'a CompileCtx) -> ValueDisplay<'a> {
        ValueDisplay { value, ctx }
    }

    struct BinaryOpDisplay<'a> {
        op: &'a BinaryOp,
    }

    impl<'a> ::std::fmt::Display for BinaryOpDisplay<'a> {
        fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
            match *self.op {
                BinaryOp::Add(sign, size) => {
                    write!(f, "add_")?;
                    write_int(f, sign, size)
                }
                BinaryOp::Div(sign, size) => {
                    write!(f, "div_")?;
                    write_int(f, sign, size)
                }
                BinaryOp::Eq => write!(f, "eq"),
                BinaryOp::Greater(sign, size) => {
                    write!(f, "gt_")?;
                    write_int(f, sign, size)
                }
                BinaryOp::GreaterEq(sign, size) => {
                    write!(f, "gteq_")?;
                    write_int(f, sign, size)
                }
                BinaryOp::Less(sign, size) => {
                    write!(f, "le_")?;
                    write_int(f, sign, size)
                }
                BinaryOp::LessEq(sign, size) => {
                    write!(f, "leeq_")?;
                    write_int(f, sign, size)
                }
                BinaryOp::Mod(sign, size) => {
                    write!(f, "mod_")?;
                    write_int(f, sign, size)
                }
                BinaryOp::Mul(sign, size) => {
                    write!(f, "mul_")?;
                    write_int(f, sign, size)
                }
                BinaryOp::Neq => write!(f, "neq"),
                BinaryOp::Sub(sign, size) => {
                    write!(f, "sub_")?;
                    write_int(f, sign, size)
                }
            }
        }
    }

    fn db(op: &BinaryOp) -> BinaryOpDisplay {
        BinaryOpDisplay { op }
    }

    struct UnaryOpDisplay<'a> {
        op: &'a UnaryOp,
    }

    impl<'a> ::std::fmt::Display for UnaryOpDisplay<'a> {
        fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
            match *self.op {
                UnaryOp::DerefLoad => write!(f, "deref"),
                UnaryOp::FieldLoad(_, ref fields) => {
                    write!(f, "load_field ")?;
                    for field in fields {
                        write!(f, ".{}", field)?;
                    }
                    Ok(())
                }
                UnaryOp::Negate(sign, size) => {
                    write!(f, "neg_")?;
                    write_int(f, sign, size)
                }
                UnaryOp::Not => write!(f, "not"),
                UnaryOp::OffsetAddress(_, ref fields) => {
                    write!(f, "field_offset ")?;
                    for field in fields {
                        write!(f, ".{}", field)?;
                    }
                    Ok(())
                }
            }
        }
    }

    fn du(op: &UnaryOp) -> UnaryOpDisplay {
        UnaryOpDisplay { op }
    }

    pub(crate) fn print_program(program: &Program, ctx: &CompileCtx) {
        for (&id, f) in &program.functions {
            println!("function {}", ctx.symbols.get_name(id));
            print_function(f, ctx);
            println!();
        }
    }

    fn print_function(f: &Function, ctx: &CompileCtx) {
        if let Some(block) = f.start_block {
            println!("start:");
            println!("    goto label_{}", block.0);
            for (id, block) in &f.blocks {
                println!("label_{}:", id.0);
                print_block(block, ctx);
            }
        }
    }

    fn print_block(block: &Block, ctx: &CompileCtx) {
        for i in &block.ops {
            print_instruction(i, ctx);
        }
        print_block_end(&block.end, ctx);
    }

    fn print_instruction(i: &Instruction, ctx: &CompileCtx) {
        match *i {
            Instruction::Init(reg) => {
                println!("    init r{}", reg.0);
            }
            Instruction::Assign(reg, ref value) => {
                println!("    r{} = {}", reg.0, d(value, ctx));
            }
            Instruction::BinaryOp(reg, ref op, ref a, ref b) => {
                println!("    r{} = {} {} {}", reg.0, db(op), d(a, ctx), d(b, ctx));
            }
            Instruction::Call(dest, ref val, ref params) => {
                print!("    r{} = call {}", dest.0, d(val, ctx));
                print!("(");
                let mut first = true;
                for param in params {
                    if !first {
                        print!(",")
                    }
                    first = false;
                    print!("{}", d(param, ctx));
                }
                println!(")");
            }
            Instruction::CastAssign(reg, ref value) => {
                println!("    r{} = cast {}", reg.0, d(value, ctx));
            }
            Instruction::DerefStore(ref dest, _, ref fields, ref value) => {
                print!("    deref_store {} ", d(dest, ctx));
                for field in fields {
                    print!(".{}", field);
                }
                if !fields.is_empty() {
                    print!(" ")
                }
                println!("{}", d(value, ctx));
            }
            Instruction::Drop(reg) => {
                println!("    drop r{}", reg.0);
            }
            Instruction::FieldStore(dest, ref fields, ref value) => {
                print!("    store r{} ", dest.0);
                for field in fields {
                    print!(".{}", field);
                }
                if !fields.is_empty() {
                    print!(" ")
                }
                println!("{}", d(value, ctx));
            }
            Instruction::StartStatement => {
                println!("    start_stmt");
            }
            Instruction::TakeAddress(dest, reg, ref fields) => {
                print!("    r{} = address r{}", dest.0, reg.0);
                for field in fields {
                    print!(".{}", field);
                }
                println!();
            }
            Instruction::UnaryOp(dest, ref op, ref value) => {
                println!("    r{} = {} {}", dest.0, du(op), d(value, ctx));
            }
            Instruction::Error => {
                println!("    error");
            }
        }
    }

    fn print_block_end(end: &BlockEnd, ctx: &CompileCtx) {
        match *end {
            BlockEnd::Branch(ref val, a, b) => {
                println!("    branch {} label_{} label_{}", d(val, ctx), a.0, b.0);
            }
            BlockEnd::Error => {
                println!("    error");
            }
            BlockEnd::Jump(id) => {
                println!("    goto label_{}", id.0);
            }
            BlockEnd::Return(ref value) => {
                println!("    return {}", d(value, ctx));
            }
        }
    }
}
