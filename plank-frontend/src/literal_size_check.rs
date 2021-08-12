use ast::typed::{
    Expr, Literal, Number, Program, Signedness, Size, Statement, Type, TypedExpr, UnaryOp,
};
use plank_errors::position::Span;
use CompileCtx;

struct Context<'a> {
    ctx: &'a mut CompileCtx,
}

impl<'a> Context<'a> {
    fn new(ctx: &'a mut CompileCtx) -> Self {
        Context { ctx }
    }

    fn check_expr(&mut self, expr: &mut TypedExpr) {
        let mut replace_with = None;
        match *expr.expr.as_mut() {
            Expr::Literal(Literal::Number(ref mut n)) => {
                let val = if n.value > ::std::i64::MAX as u64 {
                    ::std::i64::MAX
                } else {
                    n.value as i64
                };
                n.value = check_literal(val, &expr.typ, expr.span, self.ctx) as u64;
            }
            Expr::Unary(op, ref mut value) => match (*op, value.expr.as_mut()) {
                (UnaryOp::Minus, &mut Expr::Literal(Literal::Number(n))) => {
                    let val = if n.value > ::std::i64::MAX as u64 {
                        ::std::i64::MIN
                    } else {
                        -(n.value as i64)
                    };
                    let value = check_literal(val, &expr.typ, expr.span, self.ctx);
                    replace_with = Some(Expr::Literal(Literal::Number(Number {
                        value: value as u64,
                        ..n
                    })));
                }
                _ => self.check_expr(value),
            },
            Expr::Binary(ref mut a, _, ref mut b) => {
                self.check_expr(a);
                self.check_expr(b);
            }
            Expr::Call(ref mut f, ref mut params) => {
                self.check_expr(f);
                for param in params {
                    self.check_expr(param);
                }
            }
            Expr::Cast(ref mut e, _) | Expr::Field(ref mut e, _) => self.check_expr(e),
            Expr::Error | Expr::Name(_, _) | Expr::Literal(_) => {}
        }
        if let Some(new_expr) = replace_with {
            *expr.expr = new_expr
        }
    }

    fn check_statement(&mut self, stmt: &mut Statement) {
        match *stmt {
            Statement::Block(ref mut stmts) => {
                for stmt in stmts {
                    self.check_statement(stmt);
                }
            }
            Statement::Break
            | Statement::Continue
            | Statement::Error
            | Statement::Let(_, _, _, None) => {}
            Statement::Expr(ref mut expr)
            | Statement::Let(_, _, _, Some(ref mut expr))
            | Statement::Return(ref mut expr) => {
                self.check_expr(expr);
            }
            Statement::If(ref mut cond, ref mut then, ref mut else_) => {
                self.check_expr(cond);
                self.check_statement(then);
                if let Some(ref mut stmt) = *else_ {
                    self.check_statement(stmt);
                }
            }
            Statement::Loop(ref mut stmt) => {
                self.check_statement(stmt);
            }
            Statement::While(ref mut expr, ref mut body) => {
                self.check_expr(expr);
                self.check_statement(body);
            }
        }
    }
}

fn check_literal(value: i64, typ: &Type, span: Span, ctx: &mut CompileCtx) -> i64 {
    use self::Signedness::*;
    use self::Size::*;
    use std::{i16, i32, i64, i8, u16, u32, u8};
    let (low, high) = match *typ {
        Type::Int(Signed, Bit8) => (i8::MIN as i64, i8::MAX as i64),
        Type::Int(Signed, Bit16) => (i16::MIN as i64, i16::MAX as i64),
        Type::Int(Signed, Bit32) => (i32::MIN as i64, i32::MAX as i64),
        Type::Int(Unsigned, Bit8) => (u8::MIN as i64, u8::MAX as i64),
        Type::Int(Unsigned, Bit16) => (u16::MIN as i64, u16::MAX as i64),
        Type::Int(Unsigned, Bit32) => (u32::MIN as i64, u32::MAX as i64),
        _ => (i64::MIN, i64::MAX),
    };
    if value < low {
        let msg = format!("should not be below {}", low);
        ctx.reporter
            .error("int literal is out of bounds", span)
            .span_note(msg, span)
            .build();
        0
    } else if value > high {
        let msg = format!("should not be above {}", high);
        ctx.reporter
            .error("int literal is out of bounds", span)
            .span_note(msg, span)
            .build();
        0
    } else {
        value
    }
}

pub(crate) fn check_program(program: &mut Program, ctx: &mut CompileCtx) {
    let mut ctx = Context::new(ctx);
    for f in &mut program.functions {
        if let Some(ref mut body) = f.body {
            ctx.check_statement(body);
        }
    }
}
