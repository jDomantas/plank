use ast::typed::{Expr, Function, Program, Statement, Type, TypedExpr};
use plank_syntax::position::Spanned;
use struct_layout::{LayoutEngine, LayoutResult};
use CompileCtx;

pub(crate) fn check_casts(program: &mut Program, ctx: &mut CompileCtx) {
    let layouts = LayoutEngine::new(&program.structs);
    let mut ctx = Context::new(ctx, layouts);
    for f in &mut program.functions {
        ctx.check_function(f);
    }
}

struct Context<'a> {
    ctx: &'a mut CompileCtx,
    layouts: LayoutEngine<'a>,
}

impl<'a> Context<'a> {
    fn new(ctx: &'a mut CompileCtx, layouts: LayoutEngine<'a>) -> Self {
        Context { ctx, layouts }
    }

    fn check_function(&mut self, fn_: &mut Function) {
        if let Some(ref mut stmt) = fn_.body {
            self.check_statement(stmt);
        }
    }

    fn check_statement(&mut self, stmt: &mut Spanned<Statement>) {
        match **stmt {
            Statement::Block(ref mut stmts) => {
                for stmt in stmts {
                    self.check_statement(stmt);
                }
            }
            Statement::Break | Statement::Continue | Statement::Error => {}
            Statement::Expr(ref mut expr) | Statement::Return(ref mut expr) => {
                self.check_expr(expr)
            }
            Statement::If(ref mut cond, ref mut then, ref mut else_) => {
                self.check_expr(cond);
                self.check_statement(then);
                if let Some(ref mut else_) = *else_ {
                    self.check_statement(else_);
                }
            }
            Statement::Let(_, _, _, Some(ref mut value)) => {
                self.check_expr(value);
            }
            Statement::Let(_, _, _, None) => {}
            Statement::Loop(ref mut body) => self.check_statement(body),
            Statement::While(ref mut cond, ref mut body) => {
                self.check_expr(cond);
                self.check_statement(body);
            }
        }
    }

    fn check_expr(&mut self, expr: &mut TypedExpr) {
        match *expr.expr.as_mut() {
            Expr::Binary(ref mut lhs, _, ref mut rhs) => {
                self.check_expr(lhs);
                self.check_expr(rhs);
                return;
            }
            Expr::Call(ref mut expr, ref mut params) => {
                self.check_expr(expr);
                for param in params {
                    self.check_expr(param);
                }
                return;
            }
            Expr::Field(ref mut expr, _) | Expr::Unary(_, ref mut expr) => {
                self.check_expr(expr);
                return;
            }
            Expr::Error | Expr::Literal(_) | Expr::Name(_, _) => return,
            Expr::Cast(ref mut value, ref typ) => {
                self.check_expr(value);
                let value_layout = self.layouts.size_of(&value.typ);
                let typ_layout = self.layouts.size_of(typ);
                match (value_layout, typ_layout) {
                    (LayoutResult::Ok(a), LayoutResult::Ok(b)) => {
                        if a == b {
                            return;
                        }
                        self.ctx
                            .reporter
                            .error("cannot cast between types of different sizes", expr.span)
                            .span(expr.span)
                            .build();
                    }
                    (LayoutResult::Error, _) | (_, LayoutResult::Error) => {}
                    (LayoutResult::HasTypeParam, _) | (_, LayoutResult::HasTypeParam) => {
                        self.ctx
                            .reporter
                            .error("both types must have known fixed sizes", expr.span)
                            .span(expr.span)
                            .build();
                    }
                }
            }
        }
        *expr.expr = Expr::Error;
        expr.typ = Type::Error;
    }
}
