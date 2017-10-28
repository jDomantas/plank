use std::collections::HashMap;
use plank_ir::ir;
use ast::cfg;
use struct_layout::LayoutEngine;
use CompileCtx;


struct Builder<'a> {
    layouts: &'a LayoutEngine<'a>,
    ctx: &'a CompileCtx,
    function_name: cfg::Symbol,
    function: &'a cfg::Function,
    type_params: HashMap<cfg::Symbol, cfg::Type>,
    dependencies: HashMap<ir::Symbol, (cfg::Symbol, Vec<cfg::Type>)>,
    registers: HashMap<ir::Reg, ir::Layout>,
}

impl<'a> Builder<'a> {
    fn new(
        function_name: cfg::Symbol,
        function: &'a cfg::Function,
        type_params: HashMap<cfg::Symbol, cfg::Type>,
        ctx: &'a CompileCtx,
        layouts: &'a LayoutEngine<'a>,
    ) -> Self {
        let registers = function
            .registers
            .iter()
            .map(|(&reg, ty)| {
                let ty = ty.replace(&type_params);
                let (s, a) = layouts.size_align(&ty).expect("no layout");
                let layout = ir::Layout { size: s, align: a };
                let reg = ir::Reg(reg.0);
                (reg, layout)
            })
            .collect();
        Builder {
            ctx,
            type_params,
            layouts,
            function_name,
            function,
            dependencies: HashMap::new(),
            registers,
        }
    }

    fn build(&mut self) -> ir::Function {
        let parameters = self.function
            .parameters
            .iter()
            .filter_map(|r| {
                let reg = ir::Reg(r.0);
                if self.registers[&reg].size > 0 {
                    Some(reg)
                } else {
                    None
                }
            })
            .collect();
        let mut blocks = HashMap::new();
        for (&id, block) in &self.function.blocks {
            let ir_id = ir::BlockId(id.0);
            let block = self.build_block(block);
            blocks.insert(ir_id, block);
        }
        let out_type = self.function.out_type.replace(&self.type_params);
        let (s, a) = self.layouts.size_align(&out_type).unwrap();
        let output_layout = if s == 0 {
            None
        } else {
            Some(ir::Layout { size: s, align: a })
        };
        self.registers.retain(|_, layout| layout.size > 0);
        // cheat with size_of and align_of - insert an appropriate implementation
        let start_block = if self.function_name == ::builtins::SIZE_OF {
            debug_assert_eq!(self.type_params.len(), 1);
            let param = self.type_params.values().next().unwrap();
            let size = self.layouts.size_of(param).unwrap() as u64;
            blocks.insert(
                ir::BlockId(0),
                ir::Block {
                    ops: Vec::new(),
                    end: ir::BlockEnd::Return(ir::Value::Int(size, ir::Size::Bit32)),
                },
            );
            Some(ir::BlockId(0))
        } else if self.function_name == ::builtins::ALIGN_OF {
            debug_assert_eq!(self.type_params.len(), 1);
            let param = self.type_params.values().next().unwrap();
            let align = self.layouts.size_of(param).unwrap() as u64;
            blocks.insert(
                ir::BlockId(0),
                ir::Block {
                    ops: Vec::new(),
                    end: ir::BlockEnd::Return(ir::Value::Int(align, ir::Size::Bit32)),
                },
            );
            Some(ir::BlockId(0))
        } else {
            self.function.start_block.map(|b| ir::BlockId(b.0))
        };
        ir::Function {
            blocks,
            output_layout,
            start_block,
            parameters,
            registers: ::std::mem::replace(&mut self.registers, HashMap::new()),
        }
    }

    fn build_block(&mut self, block: &cfg::Block) -> ir::Block {
        let mut ops = Vec::new();
        for op in &block.ops {
            if let Some(op) = self.build_instruction(op) {
                ops.push(op);
            }
        }
        let end = match block.end {
            cfg::BlockEnd::Branch(ref val, a, b) => {
                ir::BlockEnd::Branch(self.convert_value(val), ir::BlockId(a.0), ir::BlockId(b.0))
            }
            cfg::BlockEnd::Error => panic!("cannot build ir with errors"),
            cfg::BlockEnd::Jump(id) => ir::BlockEnd::Jump(ir::BlockId(id.0)),
            cfg::BlockEnd::Return(ref val) => if self.is_zero_sized_value(val) {
                ir::BlockEnd::ReturnProc
            } else {
                ir::BlockEnd::Return(self.convert_value(val))
            },
        };
        ir::Block { ops, end }
    }

    fn build_instruction(&mut self, i: &cfg::Instruction) -> Option<ir::Instruction> {
        match *i {
            cfg::Instruction::Assign(to, ref val) => if self.is_zero_sized(to) {
                None
            } else {
                let val = self.convert_value(val);
                Some(ir::Instruction::Assign(ir::Reg(to.0), val))
            },
            cfg::Instruction::BinaryOp(dest, cfg::BinaryOp::Eq, ref a, _)
                if self.is_zero_sized_value(a) =>
            {
                let value = ir::Value::Int(1, ir::Size::Bit8);
                Some(ir::Instruction::Assign(ir::Reg(dest.0), value))
            }
            cfg::Instruction::BinaryOp(dest, cfg::BinaryOp::Neq, ref a, _)
                if self.is_zero_sized_value(a) =>
            {
                let value = ir::Value::Int(0, ir::Size::Bit8);
                Some(ir::Instruction::Assign(ir::Reg(dest.0), value))
            }
            cfg::Instruction::BinaryOp(dest, op, ref a, ref b) => {
                debug_assert!(!self.is_zero_sized(dest));
                let a = self.convert_value(a);
                let b = self.convert_value(b);
                let op = convert_binop(op);
                Some(ir::Instruction::BinaryOp(ir::Reg(dest.0), op, a, b))
            }
            cfg::Instruction::Call(dest, cfg::Value::Symbol(sym, ref types), ref params) => {
                let f = self.make_symbol(sym, types);
                let params = params
                    .iter()
                    .filter_map(|p| if !self.is_zero_sized_value(p) {
                        Some(self.convert_value(p))
                    } else {
                        None
                    })
                    .collect();
                Some(if self.is_zero_sized(dest) {
                    ir::Instruction::CallProc(f, params)
                } else {
                    ir::Instruction::Call(ir::Reg(dest.0), f, params)
                })
            }
            cfg::Instruction::Call(dest, ref val, ref params) => {
                let f = self.convert_value(val);
                let params = params
                    .iter()
                    .filter_map(|p| if !self.is_zero_sized_value(p) {
                        Some(self.convert_value(p))
                    } else {
                        None
                    })
                    .collect();
                Some(if self.is_zero_sized(dest) {
                    ir::Instruction::CallProcVirt(f, params)
                } else {
                    ir::Instruction::CallVirt(ir::Reg(dest.0), f, params)
                })
            }
            cfg::Instruction::DerefStore(ref address, ref typ, ref fields, ref value) => {
                if self.is_zero_sized_value(value) {
                    None
                } else {
                    let typ = match *typ {
                        cfg::Type::Pointer(ref t) => t,
                        _ => panic!("cannot deref non-pointer"),
                    };
                    let offset = self.find_offset(typ, fields);
                    let address = self.convert_value(address);
                    let value = self.convert_value(value);
                    Some(ir::Instruction::DerefStore(address, offset, value))
                }
            }
            cfg::Instruction::Drop(reg) => if self.is_zero_sized(reg) {
                None
            } else {
                Some(ir::Instruction::Drop(ir::Reg(reg.0)))
            },
            cfg::Instruction::FieldStore(dest, ref fields, ref value) => {
                if self.is_zero_sized_value(value) {
                    None
                } else {
                    let offset = {
                        let dest_ty = &self.function.registers[&dest];
                        self.find_offset(dest_ty, fields)
                    };
                    let dest = ir::Reg(dest.0);
                    let value = self.convert_value(value);
                    Some(ir::Instruction::Store(dest, offset, value))
                }
            }
            cfg::Instruction::StartStatement => None,
            cfg::Instruction::TakeAddress(dest, reg, ref fields) => {
                let dest = ir::Reg(dest.0);
                if self.is_zero_sized(reg) {
                    let value = ir::Value::Int(0, ir::Size::Bit32);
                    Some(ir::Instruction::Assign(dest, value))
                } else {
                    let offset = {
                        let reg_ty = &self.function.registers[&reg];
                        self.find_offset(reg_ty, fields)
                    };
                    let reg = ir::Reg(reg.0);
                    Some(ir::Instruction::TakeAddress(dest, reg, offset))
                }
            }
            cfg::Instruction::UnaryOp(dest, cfg::UnaryOp::Negate(sign, size), ref val) => {
                debug_assert!(!self.is_zero_sized(dest));
                debug_assert!(!self.is_zero_sized_value(val));
                let dest = ir::Reg(dest.0);
                let val = self.convert_value(val);
                let sign = match sign {
                    cfg::Signedness::Signed => ir::Signedness::Signed,
                    cfg::Signedness::Unsigned => ir::Signedness::Unsigned,
                };
                let size = match size {
                    cfg::Size::Bit8 => ir::Size::Bit8,
                    cfg::Size::Bit16 => ir::Size::Bit16,
                    cfg::Size::Bit32 => ir::Size::Bit32,
                };
                Some(ir::Instruction::UnaryOp(
                    dest,
                    ir::UnaryOp::Negate(sign, size),
                    val,
                ))
            }
            cfg::Instruction::UnaryOp(dest, cfg::UnaryOp::DerefLoad, ref val) => {
                if self.is_zero_sized(dest) {
                    None
                } else {
                    let dest = ir::Reg(dest.0);
                    let val = self.convert_value(val);
                    Some(ir::Instruction::DerefLoad(dest, val, 0))
                }
            }
            cfg::Instruction::UnaryOp(
                dest,
                cfg::UnaryOp::FieldLoad(ref typ, ref fields),
                ref val,
            ) => if self.is_zero_sized(dest) {
                None
            } else {
                let dest = ir::Reg(dest.0);
                let offset = self.find_offset(typ, fields);
                let val = match self.convert_value(val) {
                    ir::Value::Reg(reg) => reg,
                    _ => panic!("cannot load field from non-reg"),
                };
                Some(ir::Instruction::Load(dest, val, offset))
            },
            cfg::Instruction::UnaryOp(dest, cfg::UnaryOp::Not, ref val) => {
                debug_assert!(!self.is_zero_sized(dest));
                let dest = ir::Reg(dest.0);
                let val = self.convert_value(val);
                let op = ir::BinaryOp::BitOp(ir::BitOp::Xor, ir::Size::Bit8);
                let arg = ir::Value::Int(1, ir::Size::Bit8);
                Some(ir::Instruction::BinaryOp(dest, op, val, arg))
            }
            cfg::Instruction::UnaryOp(
                dest,
                cfg::UnaryOp::OffsetAddress(ref typ, ref fields),
                ref val,
            ) => {
                let dest = ir::Reg(dest.0);
                let typ = match *typ {
                    cfg::Type::Pointer(ref t) => t,
                    _ => panic!("cannot deref non-pointer"),
                };
                let offset = self.find_offset(typ, fields);
                let val = self.convert_value(val);
                let op =
                    ir::BinaryOp::IntOp(ir::IntOp::Add, ir::Signedness::Unsigned, ir::Size::Bit32);
                let arg = ir::Value::Int(offset as u64, ir::Size::Bit32);
                Some(ir::Instruction::BinaryOp(dest, op, val, arg))
            }
            cfg::Instruction::Error => panic!("cannot build ir with errors"),
        }
    }

    fn is_zero_sized(&self, reg: cfg::Reg) -> bool {
        let ir_reg = ir::Reg(reg.0);
        self.registers[&ir_reg].size == 0
    }

    fn is_zero_sized_value(&self, value: &cfg::Value) -> bool {
        match *value {
            cfg::Value::Bytes(_) => false,
            cfg::Value::Error => panic!("cannot build ir with errors"),
            cfg::Value::Int(_, _) => false,
            cfg::Value::Reg(reg) => self.is_zero_sized(reg),
            cfg::Value::Symbol(_, _) => false,
        }
    }

    fn make_symbol(&mut self, id: cfg::Symbol, type_params: &[cfg::Type]) -> ir::Symbol {
        let type_params = type_params
            .iter()
            .map(|ty| ty.replace(&self.type_params))
            .collect::<Vec<_>>();
        let mut symbol: String = self.ctx.symbols.get_name(id).into();
        if !type_params.is_empty() {
            symbol.push_str("::<");
            let mut first = true;
            for param in type_params.iter() {
                if !first {
                    symbol.push(',');
                }
                first = false;
                self.write_type(&mut symbol, param);
            }
            symbol.push('>');
        }
        let symbol = ir::Symbol(symbol.into());
        self.dependencies.insert(symbol.clone(), (id, type_params));
        symbol
    }

    fn write_type(&self, to: &mut String, typ: &cfg::Type) {
        match *typ {
            cfg::Type::Bool => to.push_str("bool"),
            cfg::Type::Error => panic!("cannot build ir with errors"),
            cfg::Type::Var(_) => panic!("cannot build ir with type vars"),
            cfg::Type::Pointer(ref ty) => {
                to.push('*');
                self.write_type(to, ty);
            }
            cfg::Type::Concrete(name, ref params) => {
                to.push_str(self.ctx.symbols.get_name(name));
                if !params.is_empty() {
                    to.push('<');
                    let mut first = true;
                    for param in params.iter() {
                        if !first {
                            to.push(',');
                        }
                        first = false;
                        self.write_type(to, param);
                    }
                    to.push('>');
                }
            }
            cfg::Type::Function(ref params, ref out) => {
                to.push_str("fn(");
                let mut first = true;
                for param in params.iter() {
                    if !first {
                        to.push(',');
                    }
                    first = false;
                    self.write_type(to, param);
                }
                to.push_str(")->");
                self.write_type(to, out);
            }
            cfg::Type::Int(cfg::Signedness::Unsigned, cfg::Size::Bit8) => to.push_str("u8"),
            cfg::Type::Int(cfg::Signedness::Unsigned, cfg::Size::Bit16) => to.push_str("u16"),
            cfg::Type::Int(cfg::Signedness::Unsigned, cfg::Size::Bit32) => to.push_str("u32"),
            cfg::Type::Int(cfg::Signedness::Signed, cfg::Size::Bit8) => to.push_str("i8"),
            cfg::Type::Int(cfg::Signedness::Signed, cfg::Size::Bit16) => to.push_str("i16"),
            cfg::Type::Int(cfg::Signedness::Signed, cfg::Size::Bit32) => to.push_str("i32"),
        }
    }

    fn convert_value(&mut self, value: &cfg::Value) -> ir::Value {
        match *value {
            cfg::Value::Bytes(ref bytes) => ir::Value::Bytes(bytes.clone()),
            cfg::Value::Int(value, size) => {
                let size = match size {
                    cfg::Size::Bit8 => ir::Size::Bit8,
                    cfg::Size::Bit16 => ir::Size::Bit16,
                    cfg::Size::Bit32 => ir::Size::Bit32,
                };
                ir::Value::Int(value, size)
            }
            cfg::Value::Reg(reg) => ir::Value::Reg(ir::Reg(reg.0)),
            cfg::Value::Symbol(sym, ref types) => ir::Value::Symbol(self.make_symbol(sym, types)),
            cfg::Value::Error => panic!("cannot build ir with errors"),
        }
    }

    fn find_offset(&self, typ: &cfg::Type, fields: &[usize]) -> u32 {
        let mut typ = typ.replace(&self.type_params);
        let mut offset = 0;
        for &field in fields {
            let (off, field_ty) = self.layouts.field_info(&typ, field);
            offset += off;
            typ = field_ty;
        }
        offset
    }
}

fn convert_binop(op: cfg::BinaryOp) -> ir::BinaryOp {
    fn conv_sign(sign: cfg::Signedness) -> ir::Signedness {
        match sign {
            cfg::Signedness::Signed => ir::Signedness::Signed,
            cfg::Signedness::Unsigned => ir::Signedness::Unsigned,
        }
    }
    fn conv_size(size: cfg::Size) -> ir::Size {
        match size {
            cfg::Size::Bit8 => ir::Size::Bit8,
            cfg::Size::Bit16 => ir::Size::Bit16,
            cfg::Size::Bit32 => ir::Size::Bit32,
        }
    }
    match op {
        cfg::BinaryOp::Add(sign, size) => {
            ir::BinaryOp::IntOp(ir::IntOp::Add, conv_sign(sign), conv_size(size))
        }
        cfg::BinaryOp::Div(sign, size) => {
            ir::BinaryOp::IntOp(ir::IntOp::Div, conv_sign(sign), conv_size(size))
        }
        cfg::BinaryOp::Greater(sign, size) => {
            ir::BinaryOp::IntOp(ir::IntOp::Greater, conv_sign(sign), conv_size(size))
        }
        cfg::BinaryOp::GreaterEq(sign, size) => {
            ir::BinaryOp::IntOp(ir::IntOp::GreaterEq, conv_sign(sign), conv_size(size))
        }
        cfg::BinaryOp::Less(sign, size) => {
            ir::BinaryOp::IntOp(ir::IntOp::Less, conv_sign(sign), conv_size(size))
        }
        cfg::BinaryOp::LessEq(sign, size) => {
            ir::BinaryOp::IntOp(ir::IntOp::LessEq, conv_sign(sign), conv_size(size))
        }
        cfg::BinaryOp::Mod(sign, size) => {
            ir::BinaryOp::IntOp(ir::IntOp::Mod, conv_sign(sign), conv_size(size))
        }
        cfg::BinaryOp::Mul(sign, size) => {
            ir::BinaryOp::IntOp(ir::IntOp::Mul, conv_sign(sign), conv_size(size))
        }
        cfg::BinaryOp::Sub(sign, size) => {
            ir::BinaryOp::IntOp(ir::IntOp::Sub, conv_sign(sign), conv_size(size))
        }
        cfg::BinaryOp::Eq => ir::BinaryOp::Eq,
        cfg::BinaryOp::Neq => ir::BinaryOp::Neq,
    }
}

pub(crate) fn build_ir(program: &cfg::Program, ctx: &CompileCtx) -> ir::Program {
    let layout = LayoutEngine::new(&program.structs);
    let mut functions = HashMap::new();
    let mut queue = HashMap::new();
    for (&id, f) in &program.functions {
        if f.type_params.is_empty() {
            let symbol = ir::Symbol(ctx.symbols.get_name(id).to_string().into());
            queue.insert(symbol, (id, Vec::new()));
        }
    }
    loop {
        let (symbol, sym, types) = if let Some(symbol) = queue.keys().next().cloned() {
            let (sym, t) = queue.remove(&symbol).unwrap();
            if functions.contains_key(&symbol) {
                continue;
            }
            (symbol, sym, t)
        } else {
            break;
        };
        let function = &program.functions[&sym];
        debug_assert_eq!(types.len(), function.type_params.len());
        let type_params = function
            .type_params
            .iter()
            .cloned()
            .zip(types.into_iter())
            .collect();
        let mut builder = Builder::new(sym, function, type_params, ctx, &layout);
        let function = builder.build();
        queue.extend(builder.dependencies);
        functions.insert(symbol, function);
    }

    ir::Program { functions }
}
