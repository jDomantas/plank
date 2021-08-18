use std::io;
use std::io::prelude::*;
use x86;

pub fn print_prelude<W: Write>(mut to: W) -> io::Result<()> {
    const PRELUDE: &'static str = "BITS 32
GLOBAL _start
SECTION .text

_start:
    call fn_umain
    mov ebx, eax
    mov eax, 1
    int 0x80

builtin_uputc:
    push ebx
    push ecx
    mov eax, 4
    mov ebx, 1
    lea ecx, [esp + 12]
    mov edx, 1
    int 0x80
    pop ecx
    pop ebx
    ret

builtin_ugetc:
    push ebx
    push ecx
    sub esp, 4
    mov eax, 3
    mov ebx, 2
    lea ecx, [esp]
    mov edx, 1
    int 0x80
    pop eax
    pop ecx
    pop ebx
    ret
";
    writeln!(to, "{}", PRELUDE)
}

pub fn print_asm<W: Write>(mut to: W, asm: &x86::Program) -> io::Result<()> {
    writeln!(to, "SECTION .text\n")?;
    for f in &asm.functions {
        for op in f {
            print_instruction(&mut to, op)?;
        }
        writeln!(to)?;
    }
    writeln!(to, "SECTION .rodata\n")?;
    for (i, s) in asm.strings.iter().enumerate() {
        write!(to, "string_{} db ", i)?;
        let mut first = true;
        for ch in s {
            if first {
                write!(to, "{}", ch)?;
                first = false;
            } else {
                write!(to, ", {}", ch)?;
            }
        }
        writeln!(to)?;
    }
    Ok(())
}

fn print_instruction<W: Write>(to: &mut W, i: &x86::Instruction) -> io::Result<()> {
    match *i {
        x86::Instruction::Add(ref args) => {
            write!(to, "    add ")?;
            print_args(to, args)?;
            writeln!(to)
        }
        x86::Instruction::And(ref args) => {
            write!(to, "    and ")?;
            print_args(to, args)?;
            writeln!(to)
        }
        x86::Instruction::Call(ref imm) => {
            write!(to, "    call ")?;
            print_immediate(to, imm)?;
            writeln!(to)
        }
        x86::Instruction::CallVirt(rm) => {
            write!(to, "    call ")?;
            print_rm(to, rm)?;
            writeln!(to)
        }
        x86::Instruction::Cdq => {
            writeln!(to, "    cdq")
        }
        x86::Instruction::Cmp(ref args) => {
            write!(to, "    cmp ")?;
            print_args(to, args)?;
            writeln!(to)
        }
        x86::Instruction::Cwd => {
            writeln!(to, "    cwd")
        }
        x86::Instruction::Div(rm) => {
            write!(to, "    div ")?;
            print_rm(to, rm)?;
            writeln!(to)
        }
        x86::Instruction::Idiv(rm) => {
            write!(to, "    idiv ")?;
            print_rm(to, rm)?;
            writeln!(to)
        }
        x86::Instruction::Imul(rm) => {
            write!(to, "    imul ")?;
            print_rm(to, rm)?;
            writeln!(to)
        }
        x86::Instruction::ImulReg(reg, rm) => {
            write!(to, "    imul {}, ", reg_name(reg))?;
            print_rm(to, rm)?;
            writeln!(to)
        }
        x86::Instruction::Invalid => {
            writeln!(to, "    ud2")
        }
        x86::Instruction::Jcc(cond, ref label) => {
            write!(to, "    j{} ", cond_name(cond))?;
            print_label(to, label)?;
            writeln!(to)
        }
        x86::Instruction::Jmp(ref label) => {
            write!(to, "    jmp ")?;
            print_label(to, label)?;
            writeln!(to)
        }
        x86::Instruction::Label(ref label) => {
            print_label(to, label)?;
            writeln!(to, ":")
        }
        x86::Instruction::Lea(reg, mem) => {
            write!(to, "    lea {}, ", reg_name(reg))?;
            if mem.offset > 0 {
                write!(to, "[{} + {}]", reg_name(mem.register), mem.offset)?;
            } else if mem.offset < 0 {
                write!(to, "[{} - {}]", reg_name(mem.register), -mem.offset)?;
            } else {
                write!(to, "[{}]", reg_name(mem.register))?;
            }
            writeln!(to)
        }
        x86::Instruction::Mov(ref args) => {
            write!(to, "    mov ")?;
            print_args(to, args)?;
            writeln!(to)
        }
        x86::Instruction::MovSX(reg, rm) => {
            write!(to, "    movsx {}, ", reg_name(reg))?;
            print_rm(to, rm)?;
            writeln!(to)
        }
        x86::Instruction::MovZX(reg, rm) => {
            write!(to, "    movzx {}, ", reg_name(reg))?;
            print_rm(to, rm)?;
            writeln!(to)
        }
        x86::Instruction::Mul(rm) => {
            write!(to, "    mul ")?;
            print_rm(to, rm)?;
            writeln!(to)
        }
        x86::Instruction::Neg(rm) => {
            write!(to, "    neg ")?;
            print_rm(to, rm)?;
            writeln!(to)
        }
        x86::Instruction::Or(ref args) => {
            write!(to, "    or ")?;
            print_args(to, args)?;
            writeln!(to)
        }
        x86::Instruction::Pop(rm) => {
            write!(to, "    pop ")?;
            print_rm(to, rm)?;
            writeln!(to)
        }
        x86::Instruction::Push(rm) => {
            write!(to, "    push ")?;
            print_rm(to, rm)?;
            writeln!(to)
        }
        x86::Instruction::Ret => {
            writeln!(to, "    ret")
        }
        x86::Instruction::Setcc(cond, rm) => {
            write!(to, "    set{} ", cond_name(cond))?;
            print_rm(to, rm)?;
            writeln!(to)
        }
        x86::Instruction::Sub(ref args) => {
            write!(to, "    sub ")?;
            print_args(to, args)?;
            writeln!(to)
        }
        x86::Instruction::Test(ref args) => {
            write!(to, "    test ")?;
            print_args(to, args)?;
            writeln!(to)
        }
        x86::Instruction::Xor(ref args) => {
            write!(to, "    xor ")?;
            print_args(to, args)?;
            writeln!(to)
        }
    }
}

fn print_args<W: Write>(to: &mut W, args: &x86::TwoArgs) -> io::Result<()> {
    match *args {
        x86::TwoArgs::RegRm(reg, rm) => {
            write!(to, "{}, ", reg_name(reg))?;
            print_rm(to, rm)
        }
        x86::TwoArgs::RmImm(rm, ref imm) => {
            print_rm(to, rm)?;
            write!(to, ", ")?;
            print_immediate(to, imm)
        }
        x86::TwoArgs::RmReg(rm, reg) => {
            print_rm(to, rm)?;
            write!(to, ", {}", reg_name(reg))
        }
    }
}

fn print_immediate<W: Write>(to: &mut W, imm: &x86::Immediate) -> io::Result<()> {
    match *imm {
        x86::Immediate::Constant(val) => write!(to, "{}", val),
        x86::Immediate::Label(ref label) => print_label(to, label),
    }
}

fn print_label<W: Write>(to: &mut W, label: &x86::Label) -> io::Result<()> {
    match *label {
        x86::Label::Named(ref name) => print_name(to, name),
        x86::Label::Unnamed(id) => write!(to, "label_{}", id),
        x86::Label::String(id) => write!(to, "string_{}", id),
    }
}

fn print_name<W: Write>(to: &mut W, name: &str) -> io::Result<()> {
    for ch in name.chars() {
        match ch {
            'a'..='z' | 'A'..='Z' | '0'..='9' => write!(to, "{}", ch)?,
            '_' => write!(to, "_u")?,
            ',' => write!(to, "_c")?,
            '<' => write!(to, "_l")?,
            '>' => write!(to, "_g")?,
            ':' => write!(to, "_o")?,
            '*' => write!(to, "_s")?,
            '(' => write!(to, "_p")?,
            ')' => write!(to, "_c")?,
            '-' => write!(to, "_d")?,
            c => panic!("bad label char: '{}', in label: '{}'", c, name),
        }
    }
    Ok(())
}

fn print_rm<W: Write>(to: &mut W, rm: x86::Rm) -> io::Result<()> {
    match rm {
        x86::Rm::Memory(mem) => print_memory(to, mem),
        x86::Rm::Register(reg) => write!(to, "{}", reg_name(reg)),
    }
}

fn print_memory<W: Write>(to: &mut W, memory: x86::Memory) -> io::Result<()> {
    let ptr_name = match memory.ptr_size {
        1 => "byte",
        2 => "word",
        4 => "dword",
        _ => panic!("bad ptr size: {}", memory.ptr_size),
    };
    if memory.offset > 0 {
        write!(
            to,
            "{} [{} + {}]",
            ptr_name,
            reg_name(memory.register),
            memory.offset
        )
    } else if memory.offset < 0 {
        write!(
            to,
            "{} [{} - {}]",
            ptr_name,
            reg_name(memory.register),
            -memory.offset
        )
    } else {
        write!(to, "{} [{}]", ptr_name, reg_name(memory.register))
    }
}

fn reg_name(reg: x86::Register) -> &'static str {
    match reg {
        x86::Register::Al => "al",
        x86::Register::Ah => "ah",
        x86::Register::Ax => "ax",
        x86::Register::Eax => "eax",
        x86::Register::Bl => "bl",
        x86::Register::Bh => "bh",
        x86::Register::Bx => "bx",
        x86::Register::Ebx => "ebx",
        x86::Register::Cl => "cl",
        x86::Register::Ch => "ch",
        x86::Register::Cx => "cx",
        x86::Register::Ecx => "ecx",
        x86::Register::Dl => "dl",
        x86::Register::Dh => "dh",
        x86::Register::Dx => "dx",
        x86::Register::Edx => "edx",
        x86::Register::Edi => "edi",
        x86::Register::Esi => "esi",
        x86::Register::Ebp => "ebp",
        x86::Register::Esp => "esp",
    }
}

fn cond_name(cond: x86::Condition) -> &'static str {
    match cond {
        x86::Condition::Above => "a",
        x86::Condition::AboveEqual => "ae",
        x86::Condition::Below => "b",
        x86::Condition::BelowEqual => "be",
        x86::Condition::Equal => "e",
        x86::Condition::Greater => "g",
        x86::Condition::GreaterEqual => "ge",
        x86::Condition::Less => "l",
        x86::Condition::LessEqual => "le",
        x86::Condition::NotEqual => "ne",
    }
}
