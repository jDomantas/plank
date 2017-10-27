use std::io::{self, Write};
use ir;

pub fn emit_program<W: Write>(program: &ir::Program, mut out: W) -> io::Result<()> {
    for (name, func) in &program.functions {
        write!(out, "function {}", name.0)?;
        emit_function(func, &mut out)?;
        writeln!(out)?;
    }
    Ok(())
}

fn emit_function<W: Write>(func: &ir::Function, out: &mut W) -> io::Result<()> {
    write!(out, "(")?;
    let mut first = true;
    for param in &func.parameters {
        if !first { write!(out, ", ")?; }
        first = false;
        write!(out, "%{}", param.0)?;
    }
    write!(out, ")")?;
    if let Some(layout) = func.output_layout {
        writeln!(out, ": {{ {}, {} }}", layout.size, layout.align)?;
    } else {
        writeln!(out)?;
    }
    for (reg, layout) in &func.registers {
        writeln!(out, "    register %{}: size {}, align {}", reg.0, layout.size, layout.align)?;
    }
    writeln!(out, "start:")?;
    writeln!(out, "    goto label_{}:", func.start_block.0)?;
    for (id, block) in &func.blocks {
        writeln!(out, "label_{}:", id.0)?;
        for op in &block.ops {
            emit_instruction(op, out)?;
        }
        match block.end {
            ir::BlockEnd::Branch(ref val, a, b) => {
                write!(out, "    branch ")?;
                emit_value(val, out)?;
                writeln!(out, " label_{} label_{}", a.0, b.0)?;
            }
            ir::BlockEnd::Jump(id) => {
                writeln!(out, "    goto label_{}", id.0)?;
            }
            ir::BlockEnd::Return(ref val) => {
                write!(out, "    return ")?;
                emit_value(val, out)?;
                writeln!(out)?;
            }
            ir::BlockEnd::ReturnProc => {
                writeln!(out, "    return")?;
            }
        }
    }
    Ok(())
}

fn emit_instruction<W: Write>(i: &ir::Instruction, out: &mut W) -> io::Result<()> {
    match *i {
        ir::Instruction::Assign(reg, ref val) => {
            write!(out, "    %{} = ", reg.0)?;
            emit_value(val, out)?;
            writeln!(out)
        }
        ir::Instruction::BinaryOp(dest, op, ref a, ref b) => {
            write!(out, "    %{} = ", dest.0)?;
            emit_binop(op, out)?;
            write!(out, " ")?;
            emit_value(a, out)?;
            write!(out, " ")?;
            emit_value(b, out)?;
            writeln!(out)
        }
        ir::Instruction::Call(dest, ref sym, ref params) => {
            write!(out, "    %{} = call {}", dest.0, sym.0)?;
            emit_params(params, out)?;
            writeln!(out)
        }
        ir::Instruction::CallProc(ref sym, ref params) => {
            write!(out, "    call {}", sym.0)?;
            emit_params(params, out)?;
            writeln!(out)
        }
        ir::Instruction::CallVirt(dest, ref val, ref params) => {
            write!(out, "    %{} = callvirt ", dest.0)?;
            emit_value(val, out)?;
            emit_params(params, out)?;
            writeln!(out)
        }
        ir::Instruction::CallProcVirt(ref val, ref params) => {
            write!(out, "    callvirt ")?;
            emit_value(val, out)?;
            emit_params(params, out)?;
            writeln!(out)
        }
        ir::Instruction::DerefLoad(dest, ref value, offset) => {
            write!(out, "    %{} = deref (", dest.0)?;
            emit_value(value, out)?;
            writeln!(out, " + {})", offset)
        }
        ir::Instruction::DerefStore(ref address, offset, ref value) => {
            write!(out, "    store (")?;
            emit_value(address, out)?;
            write!(out, " + {}) ", offset)?;
            emit_value(value, out)?;
            writeln!(out)
        }
        ir::Instruction::Drop(reg) => {
            writeln!(out, "    drop %{}", reg.0)
        }
        ir::Instruction::Load(dest, reg, offset) => {
            writeln!(out, "    %{} = %{}[{}]", dest.0, reg.0, offset)
        }
        ir::Instruction::Store(dest, offset, ref value) => {
            write!(out, "    %{}[{}] = ", dest.0, offset)?;
            emit_value(value, out)?;
            writeln!(out)
        }
        ir::Instruction::TakeAddress(dest, of, offset) => {
            writeln!(out, "    %{} = address %{}[{}]", dest.0, of.0, offset)
        }
        ir::Instruction::UnaryOp(dest, ir::UnaryOp::Negate(sign, size), ref arg) => {
            write!(out, "    %{} = neg_", dest.0)?;
            emit_sign(sign, out)?;
            emit_size(size, out)?;
            write!(out, " ")?;
            emit_value(arg, out)?;
            writeln!(out)
        }
    }
}

fn emit_value<W: Write>(val: &ir::Value, out: &mut W) -> io::Result<()> {
    match *val {
        ir::Value::Bytes(ref bytes) => {
            write!(out, "\"")?;
            for &byte in bytes {
                if byte < 32 || byte > 126 {
                    write!(out, "\\x{:x}", byte)?;
                } else if byte == b'"' {
                    write!(out, "\\\"")?;
                } else {
                    write!(out, "{}", byte as char)?;
                }
            }
            write!(out, "\"")
        }
        ir::Value::Int(value, size) => {
            match size {
                ir::Size::Bit8 => write!(out, "{}_b8", value),
                ir::Size::Bit16 => write!(out, "{}_b16", value),
                ir::Size::Bit32 => write!(out, "{}_b32", value),
            }
        }
        ir::Value::Reg(reg) => {
            write!(out, "%{}", reg.0)
        }
        ir::Value::Symbol(ref sym) => {
            write!(out, "{}", sym.0)
        }
    }
}

fn emit_binop<W: Write>(op: ir::BinaryOp, out: &mut W) -> io::Result<()> {
    match op {
        ir::BinaryOp::BitOp(ir::BitOp::And, s) => {
            write!(out, "and_")?;
            emit_size(s, out)
        }
        ir::BinaryOp::BitOp(ir::BitOp::Or, s) => {
            write!(out, "or_")?;
            emit_size(s, out)
        }
        ir::BinaryOp::BitOp(ir::BitOp::Xor, s) => {
            write!(out, "xor_")?;
            emit_size(s, out)
        }
        ir::BinaryOp::Eq => {
            write!(out, "eq")
        }
        ir::BinaryOp::Neq => {
            write!(out, "neq")
        }
        ir::BinaryOp::IntOp(ir::IntOp::Add, sign, size) => {
            write!(out, "add_")?;
            emit_sign(sign, out)?;
            emit_size(size, out)
        }
        ir::BinaryOp::IntOp(ir::IntOp::Div, sign, size) => {
            write!(out, "div_")?;
            emit_sign(sign, out)?;
            emit_size(size, out)
        }
        ir::BinaryOp::IntOp(ir::IntOp::Greater, sign, size) => {
            write!(out, "gt_")?;
            emit_sign(sign, out)?;
            emit_size(size, out)
        }
        ir::BinaryOp::IntOp(ir::IntOp::GreaterEq, sign, size) => {
            write!(out, "geq_")?;
            emit_sign(sign, out)?;
            emit_size(size, out)
        }
        ir::BinaryOp::IntOp(ir::IntOp::Less, sign, size) => {
            write!(out, "le_")?;
            emit_sign(sign, out)?;
            emit_size(size, out)
        }
        ir::BinaryOp::IntOp(ir::IntOp::LessEq, sign, size) => {
            write!(out, "leq_")?;
            emit_sign(sign, out)?;
            emit_size(size, out)
        }
        ir::BinaryOp::IntOp(ir::IntOp::Mod, sign, size) => {
            write!(out, "mod_")?;
            emit_sign(sign, out)?;
            emit_size(size, out)
        }
        ir::BinaryOp::IntOp(ir::IntOp::Mul, sign, size) => {
            write!(out, "mul_")?;
            emit_sign(sign, out)?;
            emit_size(size, out)
        }
        ir::BinaryOp::IntOp(ir::IntOp::Sub, sign, size) => {
            write!(out, "sub_")?;
            emit_sign(sign, out)?;
            emit_size(size, out)
        }
    }
}

fn emit_sign<W: Write>(sign: ir::Signedness, out: &mut W) -> io::Result<()> {
    match sign {
        ir::Signedness::Unsigned => write!(out, "u"),
        ir::Signedness::Signed => write!(out, "i"),
    }
}

fn emit_size<W: Write>(size: ir::Size, out: &mut W) -> io::Result<()> {
    match size {
        ir::Size::Bit8 => write!(out, "8"),
        ir::Size::Bit16 => write!(out, "16"),
        ir::Size::Bit32 => write!(out, "32"),
    }
}

fn emit_params<W: Write>(params: &[ir::Value], out: &mut W) -> io::Result<()> {
    write!(out, "(")?;
    let mut first = true;
    for param in params {
        if !first { write!(out, ", ")?; }
        first = false;
        emit_value(param, out)?;
    }
    write!(out, ")")
}
