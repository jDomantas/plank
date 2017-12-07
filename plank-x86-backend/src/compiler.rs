// #![allow(unused)]

use std::collections::{HashMap, HashSet};
use plank_ir::analysis;
use plank_ir::ir::{Reg, Function, Instruction, Value, UnaryOp, BinaryOp, IntOp, Program, Block, BlockId, BlockEnd, Signedness, Size, BitOp};
use x86;


#[derive(Default, Debug, Clone)]
struct Constraints {
    intersect: HashSet<(Reg, Reg)>,
    not_flag: HashSet<Reg>,
    not_register: HashSet<Reg>,
}

#[derive(Debug, Copy, Clone)]
enum Bonus {
    SameLocation(Reg, Reg),
}

fn generate_constraints(f: &Function) -> Constraints {
    let mut constraints = Constraints::default();
    let liveness = analysis::liveness::live_locations(f);
    for (&r1, locs1) in &liveness {
        for (&r2, locs2) in &liveness {
            if r1 != r2 {
                if locs1.iter().any(|loc| locs2.contains(loc)) {
                    constraints.intersect.insert((r1, r2));
                }
            }
        }
    }
    for &param in &f.parameters {
        let size = f.registers[&param].size;
        let size = (size + 3) / 4;
        for (&reg, &layout) in &f.registers {
            if layout.size > size {
                constraints.intersect.insert((param, reg));
                constraints.intersect.insert((reg, param));
            }
        }
    }
    for (&reg, &layout) in &f.registers {
        if layout.size > 4 {
            constraints.not_register.insert(reg);
        }
    }
    for block in f.blocks.values() {
        for op in &block.ops {
            match *op {
                Instruction::TakeAddress(_, r, _) |
                Instruction::Load(_, r, _) |
                Instruction::Store(r, _, _) => {
                    constraints.not_register.insert(r);
                }
                _ => {}
            }
        }
    }
    // TODO: this is temporary
    constraints.not_flag = f.registers.keys().cloned().collect();
    constraints
}

fn generate_bonuses(f: &Function) -> Vec<Bonus> {
    let mut bonuses = Vec::new();
    for block in f.blocks.values() {
        for op in &block.ops {
            match *op {
                // no bonus for these, because we have to go trough eax anyways
                Instruction::BinaryOp(_, BinaryOp::IntOp(IntOp::Mul, _, _), _, _) |
                Instruction::BinaryOp(_, BinaryOp::IntOp(IntOp::Div, _, _), _, _) |
                Instruction::BinaryOp(_, BinaryOp::IntOp(IntOp::Mod, _, _), _, _) |
                // for these it's pointless to place operand and result in same position
                Instruction::BinaryOp(_, BinaryOp::Eq, _, _) |
                Instruction::BinaryOp(_, BinaryOp::Neq, _, _) => {}
                // these are useful
                Instruction::Assign(r, Value::Reg(r2)) |
                Instruction::UnaryOp(r, UnaryOp::Negate(_, _), Value::Reg(r2)) |
                Instruction::BinaryOp(r, _, Value::Reg(r2), _) => {
                    if r != r2 {
                        bonuses.push(Bonus::SameLocation(r, r2));
                    }
                }
                _ => {}
            }
        }
    }
    bonuses
}

#[derive(Default)]
struct Emitter {
    next_label: u32,
    output: Vec<x86::Instruction>,
}

impl Emitter {
    fn emit(&mut self, i: x86::Instruction) {
        self.output.push(i);
    }

    fn make_label(&mut self) -> x86::Label {
        self.next_label += 1;
        x86::Label::Unnamed(self.next_label)
    }
}

#[derive(Debug, Copy, Clone)]
enum Location {
    Ebx,
    Ecx,
    Esi,
    Edi,
    Stack(u32),
    Param(u32),
    Flags(x86::Condition),
}

impl Location {
    fn is_same(&self, other: Location) -> bool {
        use self::Location::*;
        match (*self, other) {
            (Ebx, Ebx) |
            (Ecx, Ecx) |
            (Esi, Esi) |
            (Edi, Edi) => true,
            (Stack(x), Stack(y)) |
            (Param(x), Param(y)) => x == y,
            (Flags(_), Flags(_)) => true,
            (_, _) => false,
        }
    }
}

impl Location {
    fn is_register(&self) -> bool {
        match *self {
            Location::Ebx |
            Location::Ecx |
            Location::Esi |
            Location::Edi => true,
            Location::Flags(_) |
            Location::Stack(_) |
            Location::Param(_) => false,
        }
    }

    fn is_flags(&self) -> bool {
        match *self {
            Location::Flags(_) => true,
            _ => false,
        }
    }
}

fn allocate_locations(f: &Function) -> (HashMap<Reg, Location>, u32) {
    let constraints = generate_constraints(f);
    let bonuses = generate_bonuses(f);
    let mut priority = f
        .registers
        .keys()
        .cloned()
        .map(|r| (r, 0))
        .collect::<HashMap<_, _>>();
    for &bonus in &bonuses {
        match bonus {
            Bonus::SameLocation(a, b) => {
                if !constraints.intersect.contains(&(a, b)) {
                    *priority.get_mut(&a).unwrap() += 1;
                    *priority.get_mut(&b).unwrap() += 1;
                }
            }
        }
    }
    let mut locations = HashMap::new();
    let mut param_location = 4;
    let mut location_candidates = vec![
        // Location::Flags,
        Location::Ebx,
        Location::Ecx,
        Location::Esi,
        Location::Edi,
    ];
    for &param in &f.parameters {
        let loc = Location::Param(param_location);
        locations.insert(param, loc);
        location_candidates.push(loc);
        param_location += (f.registers[&param].size + 3) / 4;
    }
    let mut visit_order = f
        .registers
        .keys()
        .cloned()
        .filter(|reg| !locations.contains_key(reg))
        .collect::<Vec<_>>();
    visit_order.sort_by_key(|k| -priority[k]);
    let mut next_stack_slot = 0;
    for reg in visit_order {
        let mut best = None;
        let size = f.registers[&reg].size;
        for &loc in &location_candidates {
            if loc.is_flags() && constraints.not_flag.contains(&reg) {
                continue;
            }
            if loc.is_register() && constraints.not_register.contains(&reg) {
                continue;
            }
            if (loc.is_same(Location::Ebx) || loc.is_same(Location::Ecx)) &&
                (size != 1 && size != 2 && size != 4)
            {
                continue;
            }
            if (loc.is_same(Location::Edi) || loc.is_same(Location::Esi)) && size != 4 {
                continue;
            }
            let mut is_ok = true;
            for &other in f.registers.keys() {
                if other != reg &&
                    constraints.intersect.contains(&(reg, other)) &&
                    locations.get(&other).map(|l| l.is_same(loc)) == Some(true)
                {
                    is_ok = false;
                    break;
                }
            }
            if !is_ok {
                continue;
            }
            let mut score = 0;
            for &bonus in &bonuses {
                match bonus {
                    Bonus::SameLocation(a, b) if a == reg || b == reg => {
                        let other = if a == reg { b } else { a };
                        if locations.get(&other).map(|l| l.is_same(loc)) == Some(true) {
                            score += 1;
                        }
                    }
                    _ => {}
                }
            }
            match best {
                None => best = Some((loc, score)),
                Some((_, prev)) if prev < score => best = Some((loc, score)),
                _ => {}
            }
        }
        let loc = match best {
            None => {
                let loc = Location::Stack(next_stack_slot);
                next_stack_slot += 1;
                loc
            }
            Some((loc, _)) => loc,
        };
        locations.insert(reg, loc);
    }
    let mut slot_sizes = (0..next_stack_slot)
        .map(|i| (i, 4))
        .collect::<HashMap<_, _>>();
    for (&reg, &loc) in &locations {
        if let Location::Stack(slot) = loc {
            let size = (f.registers[&reg].size + 3) / 4;
            let cur = slot_sizes.get_mut(&slot).unwrap();
            if *cur < size {
                *cur = size;
            }
        }
    }
    let stack_size = slot_sizes.values().cloned().sum();
    for i in (0..next_stack_slot).rev() {
        let mut prev = 0;
        for j in 0..i {
            prev += slot_sizes[&j];
        }
        slot_sizes.insert(i, prev);
    }
    for loc in locations.values_mut() {
        if let Location::Stack(ref mut slot) = *loc {
            *slot = slot_sizes[&*slot];
        }
    }
    (locations, stack_size)
}

fn order_blocks(f: &Function) -> Vec<BlockId> {
    fn walk_from(block: BlockId, f: &Function, used: &mut HashSet<BlockId>, result: &mut Vec<BlockId>) {
        if used.contains(&block) {
            return;
        }
        result.push(block);
        used.insert(block);
        let block = &f.blocks[&block];
        match block.end {
            BlockEnd::Branch(_, a, _) |
            BlockEnd::Jump(a) => walk_from(a, f, used, result),
            _ => {}
        }
    }
    let mut result = Vec::new();
    let mut used = HashSet::new();
    if let Some(block) = f.start_block {
        walk_from(block, f, &mut used, &mut result);
    }
    for &block in f.blocks.keys() {
        walk_from(block, f, &mut used, &mut result);
    }
    result
}

struct FnCompiler<'a> {
    f: &'a Function,
    locations: HashMap<Reg, Location>,
    block_labels: HashMap<BlockId, x86::Label>,
    current_block: BlockId,
    next_block: Option<BlockId>,
    emitter: &'a mut Emitter,
    stack_size: u32,
}

impl<'a> FnCompiler<'a> {
    fn new(f: &'a Function, emitter: &'a mut Emitter) -> Self {
        let (locations, stack_size) = allocate_locations(f);
        let block_labels = f
            .blocks
            .keys()
            .map(|&id| (id, emitter.make_label()))
            .collect::<HashMap<_, _>>();
        FnCompiler {
            f,
            locations,
            block_labels,
            current_block: f.start_block.unwrap(),
            next_block: None,
            emitter,
            stack_size,
        }
    }

    fn emit_function_intro(&mut self) {
        if self.stack_size > 0 {
            let args = x86::TwoArgs::RmImm(
                x86::Rm::Register(x86::Register::Esp),
                x86::Immediate::Constant(u64::from(self.stack_size)),
            );
            self.emitter.emit(x86::Instruction::Sub(args));
        }
    }

    fn emit_block(&mut self, block: &Block) {
        for op in &block.ops {
            match *op {
                Instruction::Drop(_) |
                Instruction::Init(_) |
                Instruction::Nop => {}
                Instruction::Unreachable => {
                    self.emitter.emit(x86::Instruction::Invalid);
                }
                Instruction::Assign(to, ref val) |
                Instruction::CastAssign(to, ref val) => {
                    let to = self.to_rm(to);
                    self.emit_assign(to, val, 4);
                }
                Instruction::Load(to, from, offset) => {
                    let mut from = match self.to_rm(from) {
                        x86::Rm::Register(_) => panic!("cannot load from register"),
                        x86::Rm::Memory(mem) => mem,
                    };
                    let to = self.to_rm(to);
                    from.offset += offset as i32;
                    let from = x86::Rm::Memory(from);
                    self.emit_move(from, to, offset);
                }
                Instruction::Store(to, offset, ref value) => {
                    let mut to = match self.to_rm(to) {
                        x86::Rm::Register(_) => panic!("cannot store to register"),
                        x86::Rm::Memory(mem) => mem,
                    };
                    to.offset += offset as i32;
                    let to = x86::Rm::Memory(to);
                    self.emit_assign(to, value, offset);
                }
                Instruction::TakeAddress(to, of, offset) => {
                    self.emit_take_address(to, of, offset);
                }
                Instruction::UnaryOp(to, op, ref arg) => {
                    self.emit_unary_op(to, op, arg);
                }
                Instruction::BinaryOp(to, op, ref a, ref b) => {
                    self.emit_binary_op(to, op, a, b);
                }
                Instruction::Call(..) |
                Instruction::CallProc(..) |
                Instruction::CallVirt(..) |
                Instruction::CallProcVirt(..) => unimplemented!("calls not yet"),
                Instruction::DerefLoad(..) |
                Instruction::DerefStore(..) => unimplemented!("deref not yet"),
            }
        }
        self.emit_block_end(&block.end);
    }

    fn emit_assign(&mut self, to: x86::Rm, from: &Value, align: u32) {
        if let Value::Reg(from) = *from {
            let from = self.to_rm(from);
            self.emit_move(from, to, align);
        } else {
            let args = x86::TwoArgs::RmImm(
                to,
                self.to_immediate(from),
            );
            self.emitter.emit(x86::Instruction::Mov(args));
        }
    }

    fn emit_move(&mut self, from: x86::Rm, to: x86::Rm, align: u32) {
        match (to, from) {
            (x86::Rm::Memory(mem), x86::Rm::Register(reg)) => {
                let args = x86::TwoArgs::RmReg(
                    x86::Rm::Memory(mem),
                    reg,
                );
                self.emitter.emit(x86::Instruction::Mov(args));
            }
            (x86::Rm::Register(r1), x86::Rm::Register(r2)) => {
                let args = x86::TwoArgs::RmReg(
                    x86::Rm::Register(r1),
                    r2,
                );
                self.emitter.emit(x86::Instruction::Mov(args));
            }
            (x86::Rm::Register(reg), x86::Rm::Memory(mem)) => {
                let args = x86::TwoArgs::RegRm(
                    reg,
                    x86::Rm::Memory(mem),
                );
                self.emitter.emit(x86::Instruction::Mov(args));
            }
            (x86::Rm::Memory(mut m1), x86::Rm::Memory(mut m2)) => {
                debug_assert_eq!(m1.ptr_size, m2.ptr_size);
                fn gcd(a: u32, b: u32) -> u32 {
                    if b == 0 {
                        a
                    } else {
                        gcd(b, a % b)
                    }
                }
                let block = gcd((align + 3) % 4 + 1, m1.ptr_size).min(m1.ptr_size);
                match block {
                    0 => {}
                    1 => {
                        let args = x86::TwoArgs::RegRm(
                            x86::Register::Dl,
                            x86::Rm::Memory(m2),
                        );
                        self.emitter.emit(x86::Instruction::Mov(args));
                        let args = x86::TwoArgs::RmReg(
                            x86::Rm::Memory(m1),
                            x86::Register::Dl,
                        );
                        self.emitter.emit(x86::Instruction::Mov(args));
                    }
                    2 | 3 => {
                        let args = x86::TwoArgs::RegRm(
                            x86::Register::Dx,
                            x86::Rm::Memory(m2),
                        );
                        self.emitter.emit(x86::Instruction::Mov(args));
                        let args = x86::TwoArgs::RmReg(
                            x86::Rm::Memory(m1),
                            x86::Register::Dx,
                        );
                        self.emitter.emit(x86::Instruction::Mov(args));
                        m1.ptr_size -= 2;
                        m2.ptr_size -= 2;
                        self.emit_move(x86::Rm::Memory(m1), x86::Rm::Memory(m2), align);
                    }
                    _ => {
                        let args = x86::TwoArgs::RegRm(
                            x86::Register::Edx,
                            x86::Rm::Memory(m2),
                        );
                        self.emitter.emit(x86::Instruction::Mov(args));
                        let args = x86::TwoArgs::RmReg(
                            x86::Rm::Memory(m1),
                            x86::Register::Edx,
                        );
                        self.emitter.emit(x86::Instruction::Mov(args));
                        m1.ptr_size -= 4;
                        m2.ptr_size -= 4;
                        self.emit_move(x86::Rm::Memory(m1), x86::Rm::Memory(m2), align);
                    }
                }
            }
        }
    }

    fn to_rm(&self, reg: Reg) -> x86::Rm {
        let size = self.f.registers[&reg].size;
        match self.locations[&reg] {
            Location::Ebx => x86::Rm::Register(match size {
                1 => x86::Register::Bl,
                2 => x86::Register::Bx,
                4 => x86::Register::Ebx,
                _ => panic!("invalid register contents"),
            }),
            Location::Ecx => x86::Rm::Register(match size {
                1 => x86::Register::Cl,
                2 => x86::Register::Cx,
                4 => x86::Register::Ecx,
                _ => panic!("invalid register contents"),
            }),
            Location::Edi => {
                debug_assert_eq!(size, 4);
                x86::Rm::Register(x86::Register::Edi)
            }
            Location::Esi => {
                debug_assert_eq!(size, 4);
                x86::Rm::Register(x86::Register::Esi)
            }
            Location::Stack(offset) => {
                x86::Rm::Memory(x86::Memory {
                    register: x86::Register::Esp,
                    offset: offset as i32,
                    ptr_size: size,
                })
            }
            Location::Param(offset) => {
                x86::Rm::Memory(x86::Memory {
                    register: x86::Register::Esp,
                    offset: (offset + self.stack_size) as i32,
                    ptr_size: size,
                })
            }
            Location::Flags(_) => panic!("cannot convert flag location to r/m"),
        }
    }

    fn to_immediate(&self, val: &Value) -> x86::Immediate {
        match *val {
            Value::Bytes(_) => unimplemented!("byte literals not yet"),
            Value::Int(val, _) => x86::Immediate::Constant(val),
            Value::Reg(_) => panic!("register cannot be immediate"),
            Value::Symbol(ref sym) => x86::Immediate::Label(x86::Label::Named(sym.0.clone())),
            Value::Undef => panic!("got undef value"),
        }
    }

    fn emit_block_end(&mut self, end: &BlockEnd) {
        match *end {
            BlockEnd::Branch(Value::Int(0, _), _, b) => {
                if self.next_block != Some(b) {
                    let label = self.block_labels[&b].clone();
                    self.emitter.emit(x86::Instruction::Jmp(label));
                }
            }
            BlockEnd::Branch(Value::Int(_, _), a, _) |
            BlockEnd::Branch(Value::Bytes(_), a, _) |
            BlockEnd::Branch(Value::Symbol(_), a, _) |
            BlockEnd::Jump(a) => {
                if self.next_block != Some(a) {
                    let label = self.block_labels[&a].clone();
                    self.emitter.emit(x86::Instruction::Jmp(label));
                }
            }
            BlockEnd::Branch(Value::Reg(r), a, b) => {
                if let Location::Flags(cond) = self.locations[&r] {
                    if self.next_block == Some(a) {
                        self.emitter.emit(x86::Instruction::Jcc(
                            cond.opposite(),
                            self.block_labels[&b].clone(),
                        ));
                    } else if self.next_block == Some(b) {
                        self.emitter.emit(x86::Instruction::Jcc(
                            cond,
                            self.block_labels[&a].clone(),
                        ));
                    } else {
                        self.emitter.emit(x86::Instruction::Jcc(
                            cond.opposite(),
                            self.block_labels[&b].clone(),
                        ));
                        self.emitter.emit(x86::Instruction::Jmp(
                            self.block_labels[&a].clone(),
                        ));
                    }
                } else {
                    match self.to_rm(r) {
                        x86::Rm::Register(reg) => {
                            let args = x86::TwoArgs::RegRm(
                                reg,
                                x86::Rm::Register(reg),
                            );
                            self.emitter.emit(x86::Instruction::Test(args));
                        }
                        x86::Rm::Memory(mem) => {
                            let args = x86::TwoArgs::RmImm(
                                x86::Rm::Memory(mem),
                                x86::Immediate::Constant(0),
                            );
                            self.emitter.emit(x86::Instruction::Cmp(args));
                        }
                    }
                    if self.next_block == Some(a) {
                        self.emitter.emit(x86::Instruction::Jcc(
                            x86::Condition::Equal,
                            self.block_labels[&b].clone(),
                        ));
                    } else if self.next_block == Some(b) {
                        self.emitter.emit(x86::Instruction::Jcc(
                            x86::Condition::NotEqual,
                            self.block_labels[&a].clone(),
                        ));
                    } else {
                        self.emitter.emit(x86::Instruction::Jcc(
                            x86::Condition::Equal,
                            self.block_labels[&b].clone(),
                        ));
                        self.emitter.emit(x86::Instruction::Jmp(
                            self.block_labels[&a].clone(),
                        ));
                    }
                }
            }
            BlockEnd::Branch(Value::Undef, _, _) => {
                panic!("branching on undef");
            }
            BlockEnd::Return(ref _val) => {
                // something like
                //  mov eax, val
                //  add esp, ?
                //  ret
                unimplemented!("value returns not yet");
            }
            BlockEnd::ReturnProc => {
                if self.stack_size > 0 {
                    self.emitter.emit(x86::Instruction::Add(x86::TwoArgs::RmImm(
                        x86::Rm::Register(x86::Register::Esp),
                        x86::Immediate::Constant(u64::from(self.stack_size)),
                    )));
                }
                self.emitter.emit(x86::Instruction::Ret);
            }
            BlockEnd::Unreachable => {
                self.emitter.emit(x86::Instruction::Invalid);
            }
        }
    }

    fn emit_take_address(&mut self, to: Reg, of: Reg, offset: u32) {
        let mut address = match self.to_rm(of) {
            x86::Rm::Register(_) => panic!("cannot take address of register"),
            x86::Rm::Memory(mem) => mem,
        };
        address.offset += offset as i32;
        address.ptr_size = 1;
        match self.to_rm(to) {
            x86::Rm::Register(reg) => {
                self.emitter.emit(x86::Instruction::Lea(
                    reg,
                    address,
                ));
            }
            x86::Rm::Memory(mem) => {
                self.emitter.emit(x86::Instruction::Lea(
                    x86::Register::Edx,
                    address,
                ));
                let args = x86::TwoArgs::RmReg(
                    x86::Rm::Memory(mem),
                    x86::Register::Edx,
                );
                self.emitter.emit(x86::Instruction::Mov(args));
            }
        }
    }

    fn emit_unary_op(&mut self, to: Reg, op: UnaryOp, arg: &Value) {
        match op {
            UnaryOp::Negate(_, _) => {
                let need_assign;
                if let Value::Reg(r) = *arg {
                    if self.locations[&to].is_same(self.locations[&r]) {
                        need_assign = false;
                    } else {
                        need_assign = true;
                    }
                } else {
                    need_assign = true;
                }
                let to = self.to_rm(to);
                if need_assign {
                    self.emit_assign(to, arg, 4);
                }
                self.emitter.emit(x86::Instruction::Neg(to));
            }
        }
    }

    fn emit_binary_op(&mut self, to: Reg, op: BinaryOp, a: &Value, b: &Value) {
        match op {
            BinaryOp::BitOp(op, _) => {
                let dest = self.locations[&to];
                let to = self.to_rm(to);
                let (a, b) = match (a, b) {
                    (_, &Value::Reg(r)) if self.locations[&r].is_same(dest) => {
                        (b, a)
                    }
                    (a, b) => (a, b),
                };
                if let Value::Reg(r) = *a {
                    if !dest.is_same(self.locations[&r]) {
                        self.emit_assign(to, a, 4);
                    }
                } else {
                    self.emit_assign(to, a, 4);
                }
                let args = if let Value::Reg(r) = *b {
                    let r = self.to_rm(r);
                    match (to, r) {
                        (x86::Rm::Register(to), _) => {
                            x86::TwoArgs::RegRm(to, r)
                        }
                        (_, x86::Rm::Register(r)) => {
                            x86::TwoArgs::RmReg(to, r)
                        }
                        (x86::Rm::Memory(to), x86::Rm::Memory(r)) => {
                            let temp = match r.ptr_size {
                                1 => x86::Register::Dl,
                                2 => x86::Register::Dx,
                                4 => x86::Register::Edx,
                                _ => panic!("bad param size"),
                            };
                            let args = x86::TwoArgs::RegRm(
                                temp,
                                x86::Rm::Memory(r),
                            );
                            self.emitter.emit(x86::Instruction::Mov(args));
                            x86::TwoArgs::RmReg(
                                x86::Rm::Memory(to),
                                temp,
                            )
                        }
                    }
                } else {
                    x86::TwoArgs::RmImm(to, self.to_immediate(b))
                };
                match op {
                    BitOp::And => self.emitter.emit(x86::Instruction::And(args)),
                    BitOp::Or => self.emitter.emit(x86::Instruction::Or(args)),
                    BitOp::Xor => self.emitter.emit(x86::Instruction::Xor(args)),
                }
            }
            BinaryOp::IntOp(IntOp::Greater, _, size) |
            BinaryOp::IntOp(IntOp::GreaterEq, _, size) |
            BinaryOp::IntOp(IntOp::Less, _, size) |
            BinaryOp::IntOp(IntOp::LessEq, _, size) => {
                let mut cond = match op {
                    BinaryOp::IntOp(IntOp::Greater, Signedness::Signed, _) => x86::Condition::Greater,
                    BinaryOp::IntOp(IntOp::Greater, Signedness::Unsigned, _) => x86::Condition::Above,
                    BinaryOp::IntOp(IntOp::GreaterEq, Signedness::Signed, _) => x86::Condition::GreaterEqual,
                    BinaryOp::IntOp(IntOp::GreaterEq, Signedness::Unsigned, _) => x86::Condition::AboveEqual,
                    BinaryOp::IntOp(IntOp::Less, Signedness::Signed, _) => x86::Condition::Less,
                    BinaryOp::IntOp(IntOp::Less, Signedness::Unsigned, _) => x86::Condition::Below,
                    BinaryOp::IntOp(IntOp::LessEq, Signedness::Signed, _) => x86::Condition::LessEqual,
                    BinaryOp::IntOp(IntOp::LessEq, Signedness::Unsigned, _) => x86::Condition::BelowEqual,
                    _ => panic!("shit, got {:?}", op),
                };
                let (a, b) = match (a, b) {
                    (&Value::Reg(_), _) => {
                        (a, b)
                    }
                    (a, b) => {
                        cond = cond.order_opposite();
                        (b, a)
                    }
                };
                if let Value::Reg(a) = *a {
                    let loc = self.locations.get_mut(&a).unwrap();
                    if let Location::Flags(ref mut c) = *loc {
                        *c = cond;
                    }
                }
                match (a, b) {
                    (&Value::Reg(a), &Value::Reg(b)) => {
                        let a = self.to_rm(a);
                        let b = self.to_rm(b);
                        let args = match (a, b) {
                            (x86::Rm::Register(a), _) => {
                                x86::TwoArgs::RegRm(a, b)
                            }
                            (_, x86::Rm::Register(b)) => {
                                x86::TwoArgs::RmReg(a, b)
                            }
                            (x86::Rm::Memory(a), x86::Rm::Memory(b)) => {
                                let temp = match size {
                                    Size::Bit8 => x86::Register::Dl,
                                    Size::Bit16 => x86::Register::Dx,
                                    Size::Bit32 => x86::Register::Edx,
                                };
                                let args = x86::TwoArgs::RegRm(
                                    temp,
                                    x86::Rm::Memory(b),
                                );
                                self.emitter.emit(x86::Instruction::Mov(args));
                                x86::TwoArgs::RmReg(
                                    x86::Rm::Memory(a),
                                    temp,
                                )
                            }
                        };
                        self.emitter.emit(x86::Instruction::Cmp(args));
                    }
                    (&Value::Reg(a), imm) => {
                        let args = x86::TwoArgs::RmImm(
                            self.to_rm(a),
                            self.to_immediate(imm),
                        );
                        self.emitter.emit(x86::Instruction::Cmp(args));
                    }
                    (imma, immb) => {
                        let temp = match size {
                            Size::Bit8 => x86::Register::Dl,
                            Size::Bit16 => x86::Register::Dx,
                            Size::Bit32 => x86::Register::Edx,
                        };
                        self.emit_assign(x86::Rm::Register(temp), imma, 4);
                        let args = x86::TwoArgs::RmImm(
                            x86::Rm::Register(temp),
                            self.to_immediate(immb),
                        );
                        self.emitter.emit(x86::Instruction::Cmp(args));
                    }
                }
                let to_loc = self.locations[&to];
                if !to_loc.is_flags() {
                    let to = self.to_rm(to);
                    self.emitter.emit(x86::Instruction::Setcc(cond, to));
                }
            }
            BinaryOp::IntOp(IntOp::Add, _, size) |
            BinaryOp::IntOp(IntOp::Sub, _, size) => {
                if let Value::Reg(r) = *a {
                    if !self.locations[&r].is_same(self.locations[&to]) {
                        let to = self.to_rm(to);
                        self.emit_assign(to, a, 4);
                    }
                } else {
                    let to = self.to_rm(to);
                    self.emit_assign(to, a, 4);
                }
                let args = if let Value::Reg(r) = *b {
                    let to = self.to_rm(to);
                    let r = self.to_rm(r);
                    match (to, r) {
                        (x86::Rm::Register(to), _) => {
                            x86::TwoArgs::RegRm(to, r)
                        }
                        (_, x86::Rm::Register(r)) => {
                            x86::TwoArgs::RmReg(to, r)
                        }
                        (x86::Rm::Memory(to), x86::Rm::Memory(r)) => {
                            let temp = match size {
                                Size::Bit8 => x86::Register::Dl,
                                Size::Bit16 => x86::Register::Dx,
                                Size::Bit32 => x86::Register::Edx,
                            };
                            let args = x86::TwoArgs::RegRm(
                                temp,
                                x86::Rm::Memory(r),
                            );
                            self.emitter.emit(x86::Instruction::Mov(args));
                            x86::TwoArgs::RmReg(
                                x86::Rm::Memory(to),
                                temp,
                            )
                        }
                    }
                } else {
                    x86::TwoArgs::RmImm(
                        self.to_rm(to),
                        self.to_immediate(b),
                    )
                };
                match op {
                    BinaryOp::IntOp(IntOp::Add, _, _) => {
                        self.emitter.emit(x86::Instruction::Add(args));
                    }
                    BinaryOp::IntOp(IntOp::Sub, _, _) => {
                        self.emitter.emit(x86::Instruction::Sub(args));
                    }
                    _ => panic!("shit, got {:?}", op),
                }
            }
            BinaryOp::IntOp(IntOp::Mul, Signedness::Unsigned, size) => {
                let (areg, temp) = match size {
                    Size::Bit8 => (x86::Register::Al, x86::Register::Dl),
                    Size::Bit16 => (x86::Register::Ax, x86::Register::Dx),
                    Size::Bit32 => (x86::Register::Eax, x86::Register::Edx),
                };
                self.emit_assign(x86::Rm::Register(areg), a, 4);
                let arg = if let Value::Reg(b) = *b {
                    self.to_rm(b)
                } else {
                    self.emit_assign(x86::Rm::Register(temp), b, 4);
                    x86::Rm::Register(temp)
                };
                self.emitter.emit(x86::Instruction::Mul(arg));
                let args = x86::TwoArgs::RmReg(self.to_rm(to), areg);
                self.emitter.emit(x86::Instruction::Mov(args));
            }
            BinaryOp::IntOp(IntOp::Mul, Signedness::Signed, size) => {
                // TODO: make use of `imul reg, rm`
                let (areg, temp) = match size {
                    Size::Bit8 => (x86::Register::Al, x86::Register::Dl),
                    Size::Bit16 => (x86::Register::Ax, x86::Register::Dx),
                    Size::Bit32 => (x86::Register::Eax, x86::Register::Edx),
                };
                self.emit_assign(x86::Rm::Register(areg), a, 4);
                let arg = if let Value::Reg(b) = *b {
                    self.to_rm(b)
                } else {
                    self.emit_assign(x86::Rm::Register(temp), b, 4);
                    x86::Rm::Register(temp)
                };
                self.emitter.emit(x86::Instruction::Imul(arg));
                let args = x86::TwoArgs::RmReg(self.to_rm(to), areg);
                self.emitter.emit(x86::Instruction::Mov(args));
            }
            BinaryOp::IntOp(IntOp::Div, Signedness::Unsigned, Size::Bit8) |
            BinaryOp::IntOp(IntOp::Div, Signedness::Signed, Size::Bit8) |
            BinaryOp::IntOp(IntOp::Mod, Signedness::Unsigned, Size::Bit8) |
            BinaryOp::IntOp(IntOp::Mod, Signedness::Signed, Size::Bit8) => {
                let signed = match op {
                    BinaryOp::IntOp(_, Signedness::Unsigned, _) => false,
                    BinaryOp::IntOp(_, Signedness::Signed, _) => true,
                    _ => panic!("shit, got {:?}", op),
                };
                if let Value::Reg(r) = *a {
                    let op = if signed {
                        x86::Instruction::MovSX(
                            x86::Register::Ax,
                            self.to_rm(r),
                        )
                    } else {
                        x86::Instruction::MovZX(
                            x86::Register::Ax,
                            self.to_rm(r),
                        )
                    };
                    self.emitter.emit(op);
                } else {
                    let mut imm = self.to_immediate(a);
                    match imm {
                        x86::Immediate::Constant(ref mut a) if signed => {
                            *a = (*a as i8) as u64;
                        }
                        x86::Immediate::Constant(_) => {}
                        x86::Immediate::Label(_) => panic!("sign extending a label"),
                    };
                    self.emitter.emit(x86::Instruction::Mov(x86::TwoArgs::RmImm(
                        x86::Rm::Register(x86::Register::Ax),
                        imm,
                    )));
                }
                let (op, from_dx) = match op {
                    BinaryOp::IntOp(IntOp::Div, Signedness::Unsigned, _) =>
                        (x86::Instruction::Div as fn(_) -> x86::Instruction, false),
                    BinaryOp::IntOp(IntOp::Div, Signedness::Signed, _) =>
                        (x86::Instruction::Idiv as fn(_) -> x86::Instruction, false),
                    BinaryOp::IntOp(IntOp::Mod, Signedness::Unsigned, _) =>
                        (x86::Instruction::Div as fn(_) -> x86::Instruction, true),
                    BinaryOp::IntOp(IntOp::Mod, Signedness::Signed, _) =>
                        (x86::Instruction::Idiv as fn(_) -> x86::Instruction, true),
                    _ => panic!("shit, got {:?}", op),
                };
                if let Value::Reg(r) = *b {
                    let rm = self.to_rm(r);
                    self.emitter.emit(op(rm));
                } else {
                    let dl = x86::Rm::Register(x86::Register::Dl);
                    self.emit_assign(dl, b, 4);
                    self.emitter.emit(op(dl));
                }
                let to = self.to_rm(to);
                let from = if from_dx { x86::Register::Dl } else { x86::Register::Al };
                self.emit_move(x86::Rm::Register(from), to, 4);
            }
            BinaryOp::IntOp(IntOp::Div, Signedness::Unsigned, _) |
            BinaryOp::IntOp(IntOp::Div, Signedness::Signed, _) |
            BinaryOp::IntOp(IntOp::Mod, Signedness::Unsigned, _) |
            BinaryOp::IntOp(IntOp::Mod, Signedness::Signed, _) => {
                let (areg, dreg) = match op {
                    BinaryOp::IntOp(_, _, Size::Bit16) =>
                        (x86::Register::Ax, x86::Register::Dx),
                    BinaryOp::IntOp(_, _, Size::Bit32) =>
                        (x86::Register::Eax, x86::Register::Edx),
                    _ => panic!("shit, got {:?}", op),
                };
                let signed = match op {
                    BinaryOp::IntOp(_, Signedness::Unsigned, _) => false,
                    BinaryOp::IntOp(_, Signedness::Signed, _) => true,
                    _ => panic!("shit, got {:?}", op),
                };
                if let Value::Reg(r) = *a {
                    let a = self.to_rm(r);
                    self.emitter.emit(x86::Instruction::Mov(x86::TwoArgs::RegRm(
                        areg,
                        a,
                    )));
                } else {
                    let mut imm = self.to_immediate(a);
                    self.emitter.emit(x86::Instruction::Mov(x86::TwoArgs::RmImm(
                        x86::Rm::Register(areg),
                        imm,
                    )));
                }
                if signed {
                    if areg == x86::Register::Ax {
                        self.emitter.emit(x86::Instruction::Cwd);
                    } else {
                        self.emitter.emit(x86::Instruction::Cdq);
                    }
                } else {
                    self.emitter.emit(x86::Instruction::Xor(x86::TwoArgs::RegRm(
                        x86::Register::Edx,
                        x86::Rm::Register(x86::Register::Edx),
                    )));
                }
                let (op, from_dx) = match op {
                    BinaryOp::IntOp(IntOp::Div, Signedness::Unsigned, _) =>
                        (x86::Instruction::Div as fn(_) -> x86::Instruction, false),
                    BinaryOp::IntOp(IntOp::Div, Signedness::Signed, _) =>
                        (x86::Instruction::Idiv as fn(_) -> x86::Instruction, false),
                    BinaryOp::IntOp(IntOp::Mod, Signedness::Unsigned, _) =>
                        (x86::Instruction::Div as fn(_) -> x86::Instruction, true),
                    BinaryOp::IntOp(IntOp::Mod, Signedness::Signed, _) =>
                        (x86::Instruction::Idiv as fn(_) -> x86::Instruction, true),
                    _ => panic!("shit, got {:?}", op),
                };
                if let Value::Reg(r) = *b {
                    let rm = self.to_rm(r);
                    self.emitter.emit(op(rm));
                } else {
                    // TODO: check if temporary register is available and use that
                    let temp = x86::Rm::Memory(x86::Memory {
                        register: x86::Register::Esp,
                        offset: -4,
                        ptr_size: if areg == x86::Register::Ax { 2 } else { 4 },
                    });
                    self.emit_assign(temp, b, 4);
                    self.emitter.emit(op(temp));
                }
                let to = self.to_rm(to);
                let from = if from_dx { dreg } else { areg };
                self.emit_move(x86::Rm::Register(from), to, 4);
            }
            BinaryOp::Eq |
            BinaryOp::Neq => {
                let eq = if let BinaryOp::Eq = op { true } else { false };
                let (a, b) = if let Value::Reg(_) = *a {
                    (a, b)
                } else {
                    (b, a)
                };
                if let Value::Reg(ar) = *a {
                    let ar = self.to_rm(ar);
                    if let Value::Reg(br) = *b {
                        let br = self.to_rm(br);
                        self.emit_compare(ar, br, eq);
                    } else {
                        let imm = self.to_immediate(b);
                        self.emitter.emit(x86::Instruction::Cmp(x86::TwoArgs::RmImm(
                            ar,
                            imm,
                        )));
                    }
                } else {
                    let temp = match *a {
                        Value::Bytes(_) => x86::Register::Edx,
                        Value::Int(_, Size::Bit8) => x86::Register::Dl,
                        Value::Int(_, Size::Bit16) => x86::Register::Dx,
                        Value::Int(_, Size::Bit32) => x86::Register::Edx,
                        Value::Symbol(_) => x86::Register::Edx,
                        _ => panic!("shit, got {:?}", a),
                    };
                    let a = self.to_immediate(a);
                    let b = self.to_immediate(b);
                    self.emitter.emit(x86::Instruction::Mov(x86::TwoArgs::RmImm(
                        x86::Rm::Register(temp),
                        a,
                    )));
                    self.emitter.emit(x86::Instruction::Cmp(x86::TwoArgs::RmImm(
                        x86::Rm::Register(temp),
                        b,
                    )));
                }
                let cond = if eq {
                    x86::Condition::Equal
                } else {
                    x86::Condition::NotEqual
                };
                if self.locations[&to].is_flags() {
                    self.locations.insert(to, Location::Flags(cond));
                } else {
                    let to = self.to_rm(to);
                    self.emitter.emit(x86::Instruction::Setcc(cond, to));
                }
            }
        }
    }

    fn emit_compare(&mut self, a: x86::Rm, b: x86::Rm, eq: bool) {
        match (a, b) {
            (x86::Rm::Memory(mem), x86::Rm::Register(reg)) => {
                let args = x86::TwoArgs::RmReg(
                    x86::Rm::Memory(mem),
                    reg,
                );
                self.emitter.emit(x86::Instruction::Cmp(args));
            }
            (x86::Rm::Register(r1), x86::Rm::Register(r2)) => {
                let args = x86::TwoArgs::RmReg(
                    x86::Rm::Register(r1),
                    r2,
                );
                self.emitter.emit(x86::Instruction::Cmp(args));
            }
            (x86::Rm::Register(reg), x86::Rm::Memory(mem)) => {
                let args = x86::TwoArgs::RegRm(
                    reg,
                    x86::Rm::Memory(mem),
                );
                self.emitter.emit(x86::Instruction::Cmp(args));
            }
            (x86::Rm::Memory(mut m1), x86::Rm::Memory(mut m2)) => {
                debug_assert_eq!(m1.ptr_size, m2.ptr_size);
                let end_label = self.emitter.make_label();
                while m1.ptr_size > 0 {
                    let args = x86::TwoArgs::RegRm(
                        x86::Register::Edx,
                        x86::Rm::Memory(m2),
                    );
                    self.emitter.emit(x86::Instruction::Mov(args));
                    let args = x86::TwoArgs::RmReg(
                        x86::Rm::Memory(m1),
                        x86::Register::Edx,
                    );
                    self.emitter.emit(x86::Instruction::Cmp(args));
                    m1.ptr_size -= 4;
                    m2.ptr_size -= 4;
                    if m1.ptr_size > 0 {
                        self.emitter.emit(x86::Instruction::Jcc(
                            if eq {
                                x86::Condition::NotEqual
                            } else {
                                x86::Condition::Equal
                            },
                            end_label.clone(),
                        ));
                    }
                }
                self.emitter.emit(x86::Instruction::Label(end_label));
            }
        }
    }
}

fn compile_function(f: &Function, emitter: &mut Emitter) {
    let mut compiler = FnCompiler::new(f, emitter);
    compiler.emit_function_intro();
    let blocks = order_blocks(f);
    for (index, &id) in blocks.iter().enumerate() {
        compiler.current_block = id;
        compiler.next_block = blocks.get(index + 1).cloned();
        compiler.emitter.emit(x86::Instruction::Label(compiler.block_labels[&id].clone()));
        compiler.emit_block(&f.blocks[&id]);
    }
}

pub fn compile_program(program: &Program) -> Vec<x86::Instruction> {
    let mut emitter = Emitter::default();
    for (name, f) in &program.functions {
        if f.start_block.is_some() {
            emitter.emit(x86::Instruction::Label(x86::Label::Named(name.0.clone())));
            compile_function(f, &mut emitter);
        }
    }
    emitter.output
}
