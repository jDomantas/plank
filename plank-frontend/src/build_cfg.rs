use std::collections::HashMap;
use plank_syntax::position::{Span, Spanned};
use ast::typed::{self as t, Mutability as Mut};
use ast::cfg;
use CompileCtx;


struct LoopDescr {
    start: cfg::BlockId,
    after: cfg::BlockId,
}

#[derive(Debug)]
enum LValue {
    Reg(Mut, cfg::Reg, Vec<usize>),
    Deref(Mut, RValue, t::Type, Vec<usize>),
    Invalid,
    Error,
}

impl LValue {
    fn add_field(&mut self, index: usize) {
        match *self {
            LValue::Reg(_, _, ref mut fields) |
            LValue::Deref(_, _, _, ref mut fields) => {
                fields.push(index)
            }
            LValue::Error | LValue::Invalid => {}
        }
    }
}

#[derive(Debug)]
enum RValue {
    Temp(cfg::Value),
    Var(cfg::Reg),
}

impl RValue {
    fn as_value(&self) -> cfg::Value {
        match *self {
            RValue::Temp(ref value) => value.clone(),
            RValue::Var(reg) => cfg::Value::Reg(reg),
        }
    }
}

struct Builder<'a> {
    parameters: Vec<cfg::Reg>,
    registers: HashMap<cfg::Reg, cfg::Type>,
    blocks: HashMap<cfg::BlockId, cfg::Block>,
    current_loop: Option<LoopDescr>,
    ctx: &'a mut CompileCtx,
    next_block_id: u32,
    next_reg: u32,
    var_registers: HashMap<t::Symbol, cfg::Reg>,
    register_vars: HashMap<cfg::Reg, t::Symbol>,
    current_block: Option<(cfg::BlockId, Vec<Spanned<cfg::Instruction>>)>,
    var_mutability: HashMap<t::Symbol, Mut>,
}

impl<'a> Builder<'a> {
    fn new(ctx: &'a mut CompileCtx) -> Self {
        Builder {
            parameters: Vec::new(),
            registers: HashMap::new(),
            blocks: HashMap::new(),
            current_loop: None,
            ctx,
            next_block_id: 0,
            next_reg: 0,
            var_registers: HashMap::new(),
            register_vars: HashMap::new(),
            current_block: None,
            var_mutability: HashMap::new(),
        }
    }

    fn new_register(&mut self, typ: t::Type) -> cfg::Reg {
        let reg = cfg::Reg(self.next_reg);
        self.registers.insert(reg, typ);
        self.next_reg += 1;
        reg
    }

    fn new_var_register(&mut self, symbol: t::Symbol, typ: t::Type) -> cfg::Reg {
        let reg = self.new_register(typ);
        self.var_registers.insert(symbol, reg);
        self.register_vars.insert(reg, symbol);
        reg
    }

    fn drop_value(&mut self, value: &RValue, span: Span) {
        if let RValue::Temp(cfg::Value::Reg(reg)) = *value {
            self.emit_instruction(cfg::Instruction::Drop(reg), span);
        }
    }

    fn new_block(&mut self) -> cfg::BlockId {
        let block = cfg::BlockId(self.next_block_id);
        self.next_block_id += 1;
        block
    }

    fn emit_instruction(&mut self, op: cfg::Instruction, span: Span) {
        self.current_block
            .as_mut()
            .unwrap()
            .1
            .push(Spanned::new(op, span));
    }

    fn emit_store(&mut self, target: Spanned<LValue>, value: Spanned<cfg::Value>, op_span: Span) {
        let target_span = Spanned::span(&target);
        match Spanned::into_value(target) {
            LValue::Invalid => {
                self.ctx
                    .reporter
                    .error("invalid lvalue", target_span)
                    .span(target_span)
                    .build();
            }
            LValue::Error => {}
            LValue::Deref(Mut::Const, _, _, _) |
            LValue::Reg(Mut::Const, _, _) => {
                self.ctx
                    .reporter
                    .error("cannot modify non-mut value", target_span)
                    .span(target_span)
                    .build();
            }
            LValue::Deref(Mut::Mut, val, typ, fields) => {
                self.emit_instruction(
                    cfg::Instruction::DerefStore(
                        Spanned::new(val.as_value(), target_span),
                        typ,
                        fields,
                        value,
                    ),
                    op_span,
                );
                self.drop_value(&val, target_span);
            }
            LValue::Reg(Mut::Mut, reg, ref fields) if fields.is_empty() => {
                self.emit_instruction(cfg::Instruction::Assign(reg, value), op_span);
            }
            LValue::Reg(Mut::Mut, reg, fields) => {
                self.emit_instruction(
                    cfg::Instruction::FieldStore(
                        Spanned::new(reg, target_span),
                        fields,
                        value,
                    ),
                    op_span,
                );
            }
        }
    }

    fn emit_take_address(&mut self, target: cfg::Reg, value: Spanned<LValue>, op_span: Span, mutable: bool) {
        let value_span = Spanned::span(&value);
        match Spanned::into_value(value) {
            LValue::Invalid => {
                self.ctx
                    .reporter
                    .error("invalid lvalue", value_span)
                    .span(value_span)
                    .build();
            }
            LValue::Error => {}
            LValue::Deref(Mut::Const, _, _, _) |
            LValue::Reg(Mut::Const, _, _) if mutable => {
                let span = op_span.merge(value_span);
                self.ctx
                    .reporter
                    .error("cannot take mutable reference to non-mut value", span)
                    .span(span)
                    .build();
            }
            LValue::Deref(_, ref val, _, ref fields) if fields.is_empty() => {
                self.emit_instruction(
                    cfg::Instruction::Assign(
                        target,
                        Spanned::new(val.as_value(), value_span),
                    ),
                    op_span,
                );
                self.drop_value(val, value_span);
            }
            LValue::Deref(_, val, typ, fields) => {
                self.emit_instruction(
                    cfg::Instruction::UnaryOp(
                        target,
                        cfg::UnaryOp::OffsetAddress(typ, fields),
                        Spanned::new(val.as_value(), value_span),
                    ),
                    op_span,
                );
                self.drop_value(&val, value_span);
            }
            LValue::Reg(_, reg, fields) => {
                self.emit_instruction(
                    cfg::Instruction::TakeAddress(
                        target,
                        Spanned::new(reg, value_span),
                        fields,
                    ),
                    op_span,
                );
            }
        }
    }

    fn start_block(&mut self, id: cfg::BlockId) {
        if self.current_block.is_some() {
            panic!("previous block not finished");
        }
        self.current_block = Some((id, Vec::new()));
    }

    fn end_block(&mut self, end: cfg::BlockEnd, link: cfg::BlockLink) {
        let (id, ops) = self.current_block.take().unwrap();
        let block = cfg::Block { ops, end, link };
        self.blocks.insert(id, block);
    }

    fn build_function(&mut self, f: &t::Function) -> Option<cfg::BlockId> {
        for var in &f.params {
            let param = self.new_var_register(var.name, var.typ.clone());
            self.var_mutability.insert(var.name, var.mutability);
            self.parameters.push(param);
        }
        if let Some(ref body) = f.body {
            let body_block = self.new_block();
            self.start_block(body_block);
            self.build_statement(body);
            if self.current_block.is_some() {
                self.end_block(cfg::BlockEnd::Error, cfg::BlockLink::None);
            }
            Some(body_block)
        } else {
            None
        }
    }

    fn build_statement(&mut self, s: &Spanned<t::Statement>) {
        let span = Spanned::span(s);
        match **s {
            t::Statement::Block(ref stmts) => {
                for stmt in stmts {
                    let span = Spanned::span(stmt);
                    self.emit_instruction(cfg::Instruction::StartStatement, span);
                    self.build_statement(stmt);
                }
                for stmt in stmts {
                    if let t::Statement::Let(_, sym, _, _) = **stmt {
                        let span = Spanned::span(stmt);
                        let reg = self.var_registers[&sym];
                        self.emit_instruction(cfg::Instruction::Drop(reg), span);
                    }
                }
            }
            t::Statement::Break => match self.current_loop {
                Some(LoopDescr { after, .. }) => {
                    let new = self.new_block();
                    let link = cfg::BlockLink::Strong(new);
                    self.end_block(cfg::BlockEnd::Jump(after), link);
                    self.start_block(new);
                }
                None => {
                    self.emit_instruction(cfg::Instruction::Error, span);
                    self.ctx
                        .reporter
                        .error("cannot use `break` outside loop", span)
                        .span(span)
                        .build();
                }
            },
            t::Statement::Continue => match self.current_loop {
                Some(LoopDescr { start, .. }) => {
                    let new = self.new_block();
                    let link = cfg::BlockLink::Strong(new);
                    self.end_block(cfg::BlockEnd::Jump(start), link);
                    self.start_block(new);
                }
                None => {
                    self.emit_instruction(cfg::Instruction::Error, span);
                    self.ctx
                        .reporter
                        .error("cannot use `continue` outside loop", span)
                        .span(span)
                        .build();
                }
            },
            t::Statement::Expr(ref e) => {
                let expr = self.build_expr(e);
                self.drop_value(&expr, e.span);
            }
            t::Statement::If(ref cond, ref then, Some(ref else_)) => {
                let c = self.build_expr(cond);
                let body = self.new_block();
                let else_body = self.new_block();
                let after = self.new_block();
                let link = cfg::BlockLink::Weak(body);
                self.end_block(
                    cfg::BlockEnd::Branch(
                        Spanned::new(c.as_value(), cond.span),
                        body,
                        else_body,
                    ),
                    link,
                );
                self.start_block(body);
                self.drop_value(&c, cond.span);
                self.build_statement(then);
                let link = cfg::BlockLink::Weak(else_body);
                self.end_block(cfg::BlockEnd::Jump(after), link);
                self.start_block(else_body);
                self.drop_value(&c, cond.span);
                self.build_statement(else_);
                let link = cfg::BlockLink::Weak(after);
                self.end_block(cfg::BlockEnd::Jump(after), link);
                self.start_block(after);
            }
            t::Statement::If(ref cond, ref then, None) => {
                let c = self.build_expr(cond);
                let body = self.new_block();
                let after = self.new_block();
                let link = cfg::BlockLink::Weak(body);
                self.end_block(
                    cfg::BlockEnd::Branch(
                        Spanned::new(c.as_value(), cond.span),
                        body,
                        after,
                    ),
                    link,
                );
                self.start_block(body);
                self.drop_value(&c, cond.span);
                self.build_statement(then);
                let link = cfg::BlockLink::Weak(after);
                self.end_block(cfg::BlockEnd::Jump(after), link);
                self.start_block(after);
                self.drop_value(&c, cond.span);
            }
            t::Statement::Let(mutability, name, ref typ, Some(ref value)) => {
                self.var_mutability.insert(*name, mutability);
                let typ = (**typ).clone();
                let var_register = self.new_var_register(*name, typ);
                let built_value = self.build_expr(value);
                self.emit_instruction(
                    cfg::Instruction::Assign(
                        var_register,
                        Spanned::new(built_value.as_value(), value.span),
                    ),
                    span,
                );
                self.drop_value(&built_value, value.span);
            }
            t::Statement::Let(mutability, name, ref typ, None) => {
                self.var_mutability.insert(*name, mutability);
                // give it a register, but don't initialize it
                self.new_var_register(Spanned::into_value(name), (**typ).clone());
            }
            t::Statement::Loop(ref body) => {
                let start = self.new_block();
                let after = self.new_block();
                let outer_loop = self.current_loop.take();
                self.current_loop = Some(LoopDescr { start, after });
                let link = cfg::BlockLink::Weak(start);
                self.end_block(cfg::BlockEnd::Jump(start), link);
                self.start_block(start);
                self.build_statement(body);
                let link = cfg::BlockLink::Weak(after);
                self.end_block(cfg::BlockEnd::Jump(start), link);
                self.current_loop = outer_loop;
                self.start_block(after);
            }
            t::Statement::Return(ref e) => {
                let value = self.build_expr(e);
                let new = self.new_block();
                let link = cfg::BlockLink::Strong(new);
                self.end_block(
                    cfg::BlockEnd::Return(
                        Spanned::new(value.as_value(), e.span),
                    ),
                    link,
                );
                self.start_block(new);
            }
            t::Statement::While(ref cond, ref body) => {
                let start = self.new_block();
                let body_start = self.new_block();
                let after = self.new_block();
                let outer_loop = self.current_loop.take();
                self.current_loop = Some(LoopDescr { start, after });
                let link = cfg::BlockLink::Weak(start);
                self.end_block(cfg::BlockEnd::Jump(start), link);
                self.start_block(start);
                let c = self.build_expr(cond);
                let link = cfg::BlockLink::Weak(body_start);
                self.end_block(
                    cfg::BlockEnd::Branch(
                        Spanned::new(c.as_value(), cond.span),
                        body_start,
                        after,
                    ),
                    link,
                );
                self.start_block(body_start);
                self.drop_value(&c, cond.span);
                self.build_statement(body);
                let link = cfg::BlockLink::Weak(after);
                self.end_block(cfg::BlockEnd::Jump(start), link);
                self.current_loop = outer_loop;
                self.start_block(after);
                self.drop_value(&c, cond.span);
            }
            t::Statement::Error => {
                self.emit_instruction(cfg::Instruction::Error, span);
            }
        }
    }

    fn build_expr(&mut self, e: &t::TypedExpr) -> RValue {
        match *e.expr {
            t::Expr::Binary(ref lhs, op, ref rhs) => match Spanned::into_value(op) {
                t::BinaryOp::Assign => {
                    let target = self.build_expr_lvalue(lhs);
                    let value = self.build_expr(rhs);
                    self.emit_store(
                        Spanned::new(target, lhs.span),
                        Spanned::new(value.as_value(), rhs.span),
                        e.span,
                    );
                    value
                }
                t::BinaryOp::And => self.build_and(lhs, rhs),
                t::BinaryOp::Or => self.build_or(lhs, rhs),
                op => if let Some(op) = binop_to_instruction(op, &lhs.typ) {
                    let built_lhs = self.build_expr(lhs);
                    let built_rhs = self.build_expr(rhs);
                    let result = self.new_register(e.typ.clone());
                    self.emit_instruction(
                        cfg::Instruction::BinaryOp(
                            result,
                            op,
                            Spanned::new(built_lhs.as_value(), lhs.span),
                            Spanned::new(built_rhs.as_value(), rhs.span),
                        ),
                        e.span,
                    );
                    self.drop_value(&built_lhs, lhs.span);
                    self.drop_value(&built_rhs, rhs.span);
                    RValue::Temp(cfg::Value::Reg(result))
                } else {
                    RValue::Temp(cfg::Value::Error)
                },
            },
            t::Expr::Call(ref name, ref params) => {
                let callee = self.build_expr(name);
                let params = params
                    .iter()
                    .map(|p| Spanned::new(self.build_expr(p), p.span))
                    .collect::<Vec<_>>();
                let param_values = params
                    .iter()
                    .map(|p| Spanned::new(p.as_value(), Spanned::span(p)))
                    .collect();
                let target = self.new_register(e.typ.clone());
                let instr = cfg::Instruction::Call(
                    target,
                    Spanned::new(callee.as_value(), name.span),
                    param_values,
                );
                self.emit_instruction(instr, e.span);
                self.drop_value(&callee, name.span);
                for param in &params {
                    self.drop_value(param, name.span);
                }
                RValue::Temp(cfg::Value::Reg(target))
            }
            t::Expr::Error => RValue::Temp(cfg::Value::Error),
            t::Expr::Field(ref expr, index) => {
                let built_expr = self.build_expr(expr);
                let target = self.new_register(e.typ.clone());
                self.emit_instruction(
                    cfg::Instruction::UnaryOp(
                        target,
                        cfg::UnaryOp::FieldLoad(expr.typ.clone(), vec![Spanned::into_value(index)]),
                        Spanned::new(built_expr.as_value(), expr.span),
                    ),
                    e.span,
                );
                self.drop_value(&built_expr, expr.span);
                RValue::Temp(cfg::Value::Reg(target))
            }
            t::Expr::Literal(ref literal) => RValue::Temp(match *literal {
                t::Literal::Unit => cfg::Value::Unit,
                t::Literal::Bool(b) => if b {
                    cfg::Value::Int(1, cfg::Size::Bit8)
                } else {
                    cfg::Value::Int(0, cfg::Size::Bit8)
                },
                t::Literal::Char(c) => cfg::Value::Int(u64::from(c), cfg::Size::Bit8),
                t::Literal::Number(n) => {
                    let size = match e.typ {
                        t::Type::Int(_, size) => size,
                        t::Type::Error => return RValue::Temp(cfg::Value::Error),
                        _ => panic!("bad int type"),
                    };
                    cfg::Value::Int(n.value, size)
                }
                t::Literal::Str(ref bytes) => {
                    let mut bytes = bytes.clone();
                    // add null terminator
                    bytes.push(0);
                    cfg::Value::Bytes(bytes)
                }
            }),
            t::Expr::Name(name, ref type_params) => {
                let name = Spanned::into_value(name);
                if let Some(reg) = self.var_registers.get(&name).cloned() {
                    RValue::Var(reg)
                } else {
                    let type_params = type_params
                        .iter()
                        .map(Spanned::value)
                        .cloned()
                        .collect();
                    RValue::Temp(cfg::Value::Symbol(name, type_params))
                }
            }
            t::Expr::Unary(op, ref expr) => {
                let op = Spanned::into_value(op);
                if op == t::UnaryOp::AddressOf || op == t::UnaryOp::MutAddressOf {
                    let value = self.build_expr_lvalue(expr);
                    let result = self.new_register(e.typ.clone());
                    self.emit_take_address(
                        result,
                        Spanned::new(value, expr.span),
                        expr.span,
                        op == t::UnaryOp::MutAddressOf,
                    );
                    RValue::Temp(cfg::Value::Reg(result))
                } else if let Some(op) = unop_to_instruction(op, &expr.typ) {
                    let built_expr = self.build_expr(expr);
                    let result = self.new_register(e.typ.clone());
                    self.emit_instruction(
                        cfg::Instruction::UnaryOp(
                            result,
                            op,
                            Spanned::new(built_expr.as_value(), expr.span),
                        ),
                        e.span,
                    );
                    self.drop_value(&built_expr, e.span);
                    RValue::Temp(cfg::Value::Reg(result))
                } else {
                    RValue::Temp(cfg::Value::Error)
                }
            }
            t::Expr::Cast(ref expr, ref typ) => {
                let value = self.build_expr(expr);
                let result = self.new_register((**typ).clone());
                self.emit_instruction(
                    cfg::Instruction::CastAssign(
                        result,
                        Spanned::new(value.as_value(), expr.span),
                    ),
                    expr.span,
                );
                RValue::Temp(cfg::Value::Reg(result))
            }
        }
    }

    fn build_expr_lvalue(&mut self, e: &t::TypedExpr) -> LValue {
        match *e.expr {
            t::Expr::Binary(_, _, _) |
            t::Expr::Call(_, _) |
            t::Expr::Literal(_) |
            t::Expr::Cast(_, _) => LValue::Invalid,
            t::Expr::Error => LValue::Error,
            t::Expr::Field(ref expr, index) => {
                let mut lvalue = self.build_expr_lvalue(expr);
                lvalue.add_field(Spanned::into_value(index));
                lvalue
            }
            t::Expr::Name(ref name, _) => {
                let mutability = self.var_mutability[name];
                if let Some(reg) = self.var_registers.get(&**name).cloned() {
                    LValue::Reg(mutability, reg, Vec::new())
                } else {
                    LValue::Invalid
                }
            }
            t::Expr::Unary(op, ref expr) => match Spanned::into_value(op) {
                t::UnaryOp::Deref => {
                    let ptr = self.build_expr(expr);
                    let mutability = match expr.typ {
                        t::Type::Pointer(mutability, _) => mutability,
                        _ => panic!("cannot deref {:?}", expr.typ),
                    };
                    LValue::Deref(mutability, ptr, expr.typ.clone(), Vec::new())
                }
                _ => LValue::Invalid,
            },
        }
    }

    fn build_and(&mut self, lhs: &t::TypedExpr, rhs: &t::TypedExpr) -> RValue {
        let built_lhs = self.build_expr(lhs);
        let rhs_block = self.new_block();
        let reset_block = self.new_block();
        let after_block = self.new_block();
        let result = self.new_register(t::Type::Bool);
        let link = cfg::BlockLink::Strong(rhs_block);
        self.end_block(
            cfg::BlockEnd::Branch(
                Spanned::new(built_lhs.as_value(), lhs.span),
                rhs_block,
                reset_block,
            ),
            link,
        );
        self.start_block(rhs_block);
        self.drop_value(&built_lhs, lhs.span);
        let built_rhs = self.build_expr(rhs);
        self.emit_instruction(
            cfg::Instruction::Assign(
                result,
                Spanned::new(built_rhs.as_value(), rhs.span),
            ),
            rhs.span,
        );
        self.drop_value(&built_rhs, rhs.span);
        self.end_block(
            cfg::BlockEnd::Jump(after_block),
            cfg::BlockLink::Weak(reset_block),
        );
        self.start_block(reset_block);
        self.drop_value(&built_lhs, lhs.span);
        self.emit_instruction(
            cfg::Instruction::Assign(
                result,
                Spanned::new(cfg::Value::Int(0, cfg::Size::Bit8), rhs.span),
            ),
            lhs.span,
        );
        self.end_block(
            cfg::BlockEnd::Jump(after_block),
            cfg::BlockLink::Weak(after_block),
        );
        self.start_block(after_block);
        RValue::Temp(cfg::Value::Reg(result))
    }

    fn build_or(&mut self, lhs: &t::TypedExpr, rhs: &t::TypedExpr) -> RValue {
        let built_lhs = self.build_expr(lhs);
        let rhs_block = self.new_block();
        let reset_block = self.new_block();
        let after_block = self.new_block();
        let result = self.new_register(t::Type::Bool);
        let link = cfg::BlockLink::Strong(rhs_block);
        self.end_block(
            cfg::BlockEnd::Branch(
                Spanned::new(built_lhs.as_value(), lhs.span),
                reset_block,
                rhs_block,
            ),
            link,
        );
        self.start_block(rhs_block);
        self.drop_value(&built_lhs, lhs.span);
        let built_rhs = self.build_expr(rhs);
        self.emit_instruction(
            cfg::Instruction::Assign(
                result,
                Spanned::new(built_rhs.as_value(), rhs.span),
            ),
            rhs.span,
        );
        self.drop_value(&built_rhs, rhs.span);
        self.end_block(
            cfg::BlockEnd::Jump(after_block),
            cfg::BlockLink::Weak(reset_block),
        );
        self.start_block(reset_block);
        self.drop_value(&built_lhs, lhs.span);
        self.emit_instruction(
            cfg::Instruction::Assign(
                result,
                Spanned::new(cfg::Value::Int(1, cfg::Size::Bit8), rhs.span),
            ),
            lhs.span,
        );
        self.end_block(
            cfg::BlockEnd::Jump(after_block),
            cfg::BlockLink::Weak(after_block),
        );
        self.start_block(after_block);
        RValue::Temp(cfg::Value::Reg(result))
    }
}

fn binop_to_instruction(op: t::BinaryOp, arg_type: &t::Type) -> Option<cfg::BinaryOp> {
    let int = match *arg_type {
        t::Type::Int(sign, size) => Some((sign, size)),
        _ => None,
    };
    match op {
        t::BinaryOp::Add => int.map(|(sign, size)| cfg::BinaryOp::Add(sign, size)),
        t::BinaryOp::Divide => int.map(|(sign, size)| cfg::BinaryOp::Div(sign, size)),
        t::BinaryOp::Greater => int.map(|(sign, size)| cfg::BinaryOp::Greater(sign, size)),
        t::BinaryOp::GreaterEqual => int.map(|(sign, size)| cfg::BinaryOp::GreaterEq(sign, size)),
        t::BinaryOp::Less => int.map(|(sign, size)| cfg::BinaryOp::Less(sign, size)),
        t::BinaryOp::LessEqual => int.map(|(sign, size)| cfg::BinaryOp::LessEq(sign, size)),
        t::BinaryOp::Modulo => int.map(|(sign, size)| cfg::BinaryOp::Mod(sign, size)),
        t::BinaryOp::Multiply => int.map(|(sign, size)| cfg::BinaryOp::Mul(sign, size)),
        t::BinaryOp::Subtract => int.map(|(sign, size)| cfg::BinaryOp::Sub(sign, size)),
        t::BinaryOp::Equal => Some(cfg::BinaryOp::Eq),
        t::BinaryOp::NotEqual => Some(cfg::BinaryOp::Neq),
        t::BinaryOp::And | t::BinaryOp::Or | t::BinaryOp::Assign => {
            panic!("invalid binop");
        }
    }
}

fn unop_to_instruction(op: t::UnaryOp, arg_type: &t::Type) -> Option<cfg::UnaryOp> {
    let int = match *arg_type {
        t::Type::Int(sign, size) => Some((sign, size)),
        _ => None,
    };
    match op {
        t::UnaryOp::Deref => Some(cfg::UnaryOp::DerefLoad),
        t::UnaryOp::Minus => int.map(|(sign, size)| cfg::UnaryOp::Negate(sign, size)),
        t::UnaryOp::Not => Some(cfg::UnaryOp::Not),
        t::UnaryOp::Plus | t::UnaryOp::AddressOf | t::UnaryOp::MutAddressOf => panic!("invalid unary op"),
    }
}

fn compile_fn(f: &t::Function, ctx: &mut CompileCtx) -> cfg::Function {
    let mut builder = Builder::new(ctx);
    let start_block = builder.build_function(f);
    debug_assert!(builder.current_block.is_none());
    cfg::Function {
        parameters: builder.parameters,
        complete_span: f.complete_span,
        type_params: f.type_params.clone(),
        registers: builder.registers,
        register_symbols: builder.register_vars,
        out_type: f.return_type.clone(),
        blocks: builder.blocks,
        start_block,
    }
}

pub(crate) fn build_cfg(program: &t::Program, ctx: &mut CompileCtx) -> cfg::Program {
    let functions = program
        .functions
        .iter()
        .map(|f| (f.name, compile_fn(f, ctx)))
        .collect();

    let structs = program.structs.clone();

    cfg::Program { structs, functions }
}
