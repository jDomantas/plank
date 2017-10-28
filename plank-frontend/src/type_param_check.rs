use std::collections::HashMap;
use plank_syntax::position::Spanned;
use ast::resolved::{Expr, Function, FunctionType, Program, Statement, Struct, Symbol, Type};
use CompileCtx;


pub(crate) fn check_type_params(program: &mut Program, ctx: &mut CompileCtx) {
    let mut ctx = Context::new(ctx);
    ctx.check_program(program);
}

struct Context<'a> {
    ctx: &'a mut CompileCtx,
    param_count: HashMap<Symbol, usize>,
}

impl<'a> Context<'a> {
    fn new(ctx: &'a mut CompileCtx) -> Self {
        Context {
            ctx,
            param_count: HashMap::new(),
        }
    }

    fn params_taken(&self, symbol: Symbol) -> usize {
        self.param_count.get(&symbol).cloned().unwrap_or(0)
    }

    fn check_program(&mut self, program: &mut Program) {
        for struct_ in &program.structs {
            self.param_count.insert(
                Spanned::into_value(struct_.name.name),
                struct_.name.type_params.len(),
            );
        }

        for fn_ in &mut program.functions {
            self.param_count.insert(
                Spanned::into_value(fn_.name.name),
                fn_.name.type_params.len(),
            );

            if fn_.fn_type == FunctionType::Extern && !fn_.name.type_params.is_empty() {
                let span = Spanned::span(&fn_.name.name);
                self.ctx
                    .reporter
                    .error("`extern` functions cannot have type parameters", span)
                    .span(span)
                    .build();
            }
        }

        for struct_ in &mut program.structs {
            self.check_struct(struct_);
        }

        for fn_ in &mut program.functions {
            self.check_function(fn_);
        }
    }

    fn check_struct(&mut self, struct_: &mut Struct) {
        for field in &mut struct_.fields {
            self.check_type(&mut field.typ)
        }
    }

    fn check_function(&mut self, fn_: &mut Function) {
        for param in &mut fn_.params {
            self.check_type(&mut param.typ);
        }

        self.check_type(&mut fn_.return_type);

        if let Some(ref mut stmt) = fn_.body {
            self.check_statement(stmt);
        }
    }

    fn check_type(&mut self, typ: &mut Spanned<Type>) {
        match **typ {
            Type::Unit |
            Type::Bool |
            Type::Error |
            Type::I8 |
            Type::I16 |
            Type::I32 |
            Type::U8 |
            Type::U16 |
            Type::U32 |
            Type::Wildcard => return,
            Type::Pointer(ref mut typ) => {
                self.check_type(typ);
                return;
            }
            Type::Function(ref mut params, ref mut out) => {
                for param in params {
                    self.check_type(param);
                }
                self.check_type(out);
                return;
            }
            Type::Concrete(name, ref mut params) => {
                let name_span = Spanned::span(&name);
                let name = Spanned::into_value(name);
                for param in params.iter_mut() {
                    self.check_type(param);
                }
                let params_expected = self.params_taken(name);
                if params.len() != params_expected {
                    let name = self.ctx.symbols.get_name(name);
                    let msg = make_error_message("type", name, params_expected, params.len());
                    let short_msg = make_short_message(params_expected);
                    self.ctx
                        .reporter
                        .error(msg, name_span)
                        .span_note(short_msg, name_span)
                        .build();
                } else {
                    return;
                }
            }
        }
        **typ = Type::Error;
    }

    fn check_statement(&mut self, stmt: &mut Spanned<Statement>) {
        match **stmt {
            Statement::Block(ref mut stmts) => for stmt in stmts {
                self.check_statement(stmt);
            },
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
            Statement::Let(_, ref mut typ, ref mut value) => {
                self.check_type(typ);
                self.check_expr(value);
            }
            Statement::Loop(ref mut body) => self.check_statement(body),
            Statement::While(ref mut cond, ref mut body) => {
                self.check_expr(cond);
                self.check_statement(body);
            }
        }
    }

    fn check_expr(&mut self, expr: &mut Spanned<Expr>) {
        match **expr {
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
            Expr::Error | Expr::Literal(_) => return,
            Expr::Name(name, ref mut params) => {
                let name_span = Spanned::span(&name);
                let name = Spanned::into_value(name);
                for param in params.iter_mut() {
                    self.check_type(param);
                }
                let params_expected = self.params_taken(name);
                if params.is_empty() {
                    for _ in 0..params_expected {
                        params.push(Spanned::new(Type::Wildcard, name_span));
                    }
                    return;
                } else if params_expected != params.len() {
                    let name = self.ctx.symbols.get_name(name);
                    let msg = make_error_message("value", name, params_expected, params.len());
                    let short_msg = make_short_message(params_expected);
                    self.ctx
                        .reporter
                        .error(msg, name_span)
                        .span_note(short_msg, name_span)
                        .build();
                } else {
                    return;
                }
            }
            Expr::Cast(ref mut expr, ref mut typ) => {
                self.check_expr(expr);
                self.check_type(typ);
                return;
            }
        }
        **expr = Expr::Error;
    }
}

fn make_error_message(kind: &str, name: &str, expected: usize, got: usize) -> String {
    if expected == 0 {
        format!("{} `{}` does not take type parameters", kind, name,)
    } else if expected % 10 == 1 && expected % 100 != 11 {
        format!(
            "{} `{}` expects {} type parameter, got {}",
            kind,
            name,
            expected,
            got,
        )
    } else {
        format!(
            "{} `{}` expects {} type parameters, got {}",
            kind,
            name,
            expected,
            got,
        )
    }
}

fn make_short_message(expected: usize) -> String {
    if expected == 0 {
        "did not expect type parameters".into()
    } else if expected % 10 == 1 && expected % 100 != 11 {
        format!("expected {} parameter", expected)
    } else {
        format!("expected {} parameters", expected)
    }
}
