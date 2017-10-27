mod rollback_map;
mod unify;

use std::borrow::{Borrow, Cow};
use std::collections::HashMap;
use std::fmt;
use std::rc::Rc;
use plank_syntax::position::{Span, Spanned};
use ast::resolved::{self as r, BinaryOp, Symbol, UnaryOp};
use ast::typed::{self as t, Type};
use CompileCtx;
use self::unify::UnifyTable;


#[derive(Debug, Clone)]
enum Reason {
    IfCondition(Span),
    WhileCondition(Span),
    LeftOperand(Span),
    RightOperand(Span),
    UnaryOperand(Span),
    FunctionParam(usize, Span),
    Return(Span),
    Assign(Span),
}

#[derive(Debug, Clone)]
struct Scheme {
    vars: Vec<Symbol>,
    typ: Type,
}

impl Scheme {
    fn instantiate<T: Borrow<Type>>(&self, params: &[T]) -> Type {
        fn walk<T: Borrow<Type>>(ty: &mut Type, vars: &[Symbol], params: &[T]) {
            let sym = match *ty {
                Type::Concrete(sym, ref mut p) => {
                    let params = p.iter()
                        .map(|p| {
                            let mut o = p.clone();
                            walk(&mut o, vars, params);
                            o
                        })
                        .collect::<Vec<_>>();
                    *p = params.into();
                    sym
                }
                Type::Bool | Type::Int(_, _) | Type::Error | Type::Var(_) => return,
                Type::Pointer(ref mut t) => {
                    walk(Rc::make_mut(t), vars, params);
                    return;
                }
                Type::Function(ref mut p, ref mut o) => {
                    walk(Rc::make_mut(o), vars, params);
                    let params = p.iter()
                        .map(|p| {
                            let mut o = p.clone();
                            walk(&mut o, vars, params);
                            o
                        })
                        .collect::<Vec<_>>();
                    *p = params.into();
                    return;
                }
            };
            for (index, s) in vars.iter().enumerate() {
                if *s == sym {
                    *ty = params[index].borrow().clone();
                    break;
                }
            }
        }
        debug_assert_eq!(self.vars.len(), params.len());
        if self.vars.len() == 0 {
            return self.typ.clone();
        }
        let mut out_type = self.typ.clone();
        walk(&mut out_type, &self.vars, params);
        out_type
    }
}

struct TypeFormatter<'a> {
    typ: &'a Type,
    inferer: &'a Inferer<'a>,
}

impl<'a> fmt::Display for TypeFormatter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let typ = self.inferer.unifier.shallow_normalize(self.typ);
        match typ {
            Type::Bool => write!(f, "bool"),
            Type::Concrete(sym, ref params) => {
                write!(f, "{}", self.inferer.ctx.symbols.get_name(sym))?;
                if params.len() > 0 {
                    write!(f, "<")?;
                    for (index, param) in params.iter().enumerate() {
                        if index > 0 {
                            write!(f, ", ")?;
                        }
                        write!(
                            f,
                            "{}",
                            TypeFormatter {
                                typ: param,
                                inferer: self.inferer,
                            }
                        )?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            Type::Error => write!(f, "_"),
            Type::Function(ref params, ref out) => {
                write!(f, "fn(")?;
                for (index, param) in params.iter().enumerate() {
                    if index > 0 {
                        write!(f, ", ")?;
                    }
                    write!(
                        f,
                        "{}",
                        TypeFormatter {
                            typ: param,
                            inferer: self.inferer,
                        }
                    )?;
                }
                write!(
                    f,
                    ") -> {}",
                    TypeFormatter {
                        typ: out,
                        inferer: self.inferer,
                    }
                )
            }
            Type::Int(sign, size) => {
                match sign {
                    r::Signedness::Unsigned => write!(f, "u")?,
                    r::Signedness::Signed => write!(f, "i")?,
                }
                match size {
                    r::Size::Bit8 => write!(f, "8"),
                    r::Size::Bit16 => write!(f, "16"),
                    r::Size::Bit32 => write!(f, "32"),
                }
            }
            Type::Pointer(ref typ) => write!(
                f,
                "*{}",
                TypeFormatter {
                    typ,
                    inferer: self.inferer,
                }
            ),
            Type::Var(var) => write!(f, "{}", self.inferer.unifier.describe_var(var)),
        }
    }
}

struct Inferer<'a> {
    ctx: &'a mut CompileCtx,
    unifier: UnifyTable,
    string_type: Type,
    return_type: Option<Type>,
    env: HashMap<Symbol, Scheme>,
    fields: HashMap<Symbol, HashMap<String, (usize, Scheme)>>,
}

impl<'a> Inferer<'a> {
    fn new(ctx: &'a mut CompileCtx) -> Self {
        let char_type = Type::Int(t::Signedness::Unsigned, t::Size::Bit8);
        let string_type = Type::Pointer(Rc::new(char_type));
        Inferer {
            ctx,
            unifier: UnifyTable::new(),
            string_type,
            return_type: None,
            env: HashMap::new(),
            fields: HashMap::new(),
        }
    }

    fn fresh_var(&mut self) -> Type {
        Type::Var(self.unifier.fresh_var())
    }

    fn fresh_int_var(&mut self) -> Type {
        Type::Var(self.unifier.fresh_int_var(None))
    }

    fn unify(&mut self, a: &Type, b: &Type, reason: Reason) -> Type {
        match self.unifier.unify(a, b) {
            Ok(ty) => ty,
            Err(()) => {
                let got = self.format_type(a).to_string();
                let expected = self.format_type(b).to_string();
                let (msg, span) = match reason {
                    Reason::Assign(span) => {
                        let msg = format!("cannot assign `{}` to `{}`", got, expected);
                        (msg, span)
                    }
                    Reason::IfCondition(span) | Reason::WhileCondition(span) => {
                        let msg = format!("condition has type `{}`", got);
                        (msg, span)
                    }
                    Reason::Return(span) => {
                        let msg = format!(
                            "cannot return `{}` from function returning `{}`",
                            got,
                            expected,
                        );
                        (msg, span)
                    }
                    Reason::UnaryOperand(span) => {
                        let msg = format!("operand should be `{}`, but is `{}`", expected, got,);
                        (msg, span)
                    }
                    Reason::LeftOperand(span) => {
                        let msg =
                            format!("left operand should be `{}`, but is `{}`", expected, got,);
                        (msg, span)
                    }
                    Reason::RightOperand(span) => {
                        let msg =
                            format!("right operand should be `{}`, but is `{}`", expected, got,);
                        (msg, span)
                    }
                    Reason::FunctionParam(mut index, span) => {
                        index += 1;
                        let suff = match (index % 100, index % 10) {
                            (11, _) | (12, _) | (13, _) => "th",
                            (_, 1) => "st",
                            (_, 2) => "nd",
                            (_, 3) => "rd",
                            _ => "th",
                        };
                        let msg = format!(
                            "{}{} argument should be `{}`, but is `{}`",
                            index,
                            suff,
                            expected,
                            got,
                        );
                        (msg, span)
                    }
                };
                self.ctx.reporter.error(msg, span).span(span).build();
                Type::Error
            }
        }
    }

    fn format_type<'b>(&'b self, typ: &'b Type) -> TypeFormatter<'b>
    where
        'a: 'b,
    {
        TypeFormatter { typ, inferer: self }
    }

    fn convert_resolved_type(&mut self, typ: &r::Type) -> Type {
        match *typ {
            r::Type::Bool => Type::Bool,
            r::Type::Wildcard => self.fresh_var(),
            r::Type::I8 => Type::Int(t::Signedness::Signed, t::Size::Bit8),
            r::Type::U8 => Type::Int(t::Signedness::Unsigned, t::Size::Bit8),
            r::Type::I16 => Type::Int(t::Signedness::Signed, t::Size::Bit16),
            r::Type::U16 => Type::Int(t::Signedness::Unsigned, t::Size::Bit16),
            r::Type::I32 => Type::Int(t::Signedness::Signed, t::Size::Bit32),
            r::Type::U32 => Type::Int(t::Signedness::Unsigned, t::Size::Bit32),
            r::Type::Pointer(ref typ) => {
                let typ = self.convert_resolved_type(typ);
                t::Type::Pointer(Rc::new(typ))
            }
            r::Type::Concrete(sym, ref params) => {
                let params = params
                    .iter()
                    .map(|t| self.convert_resolved_type(t))
                    .collect::<Vec<_>>();
                t::Type::Concrete(Spanned::into_value(sym), params.into())
            }
            r::Type::Function(ref params, ref out) => {
                let params = params
                    .iter()
                    .map(|t| self.convert_resolved_type(t))
                    .collect::<Vec<_>>();
                let out = self.convert_resolved_type(out);
                t::Type::Function(params.into(), Rc::new(out))
            }
            r::Type::Error => t::Type::Error,
        }
    }

    fn type_name(&self, typ: &Type) -> Cow<'static, str> {
        match *typ {
            Type::Bool => "a bool".into(),
            Type::Concrete(sym, _) => format!("struct `{}`", self.ctx.symbols.get_name(sym)).into(),
            Type::Error => "error".into(),
            Type::Function(_, _) => "a function".into(),
            Type::Int(_, _) => "an int".into(),
            Type::Pointer(_) => "a pointer".into(),
            Type::Var(_) => "type variable".into(),
        }
    }

    fn infer_literal(&mut self, literal: &r::Literal) -> Type {
        match *literal {
            r::Literal::Number(num) => match (num.signedness, num.size) {
                (None, None) => self.fresh_int_var(),
                (Some(sign), None) => Type::Var(self.unifier.fresh_int_var(Some(sign))),
                (Some(sign), Some(size)) => Type::Int(sign, size),
                (None, Some(_)) => panic!("int literal with size but no sign"),
            },
            r::Literal::Bool(_) => Type::Bool,
            r::Literal::Char(_) => Type::Int(t::Signedness::Unsigned, t::Size::Bit8),
            r::Literal::Str(_) => self.string_type.clone(),
        }
    }

    fn infer_symbol(&self, symbol: Symbol, params: &[Spanned<Type>]) -> Type {
        let scheme = &self.env[&symbol];
        debug_assert_eq!(scheme.vars.len(), params.len());
        scheme.instantiate(params)
    }

    fn infer_expr(&mut self, expr: &Spanned<r::Expr>) -> t::TypedExpr {
        let (typed, typ) = match *Spanned::value(expr) {
            r::Expr::Binary(ref lhs, op, ref rhs) => {
                let lhs = self.infer_expr(lhs);
                let rhs = self.infer_expr(rhs);
                let (param_type, out_type) = match Spanned::into_value(op) {
                    BinaryOp::Equal | BinaryOp::NotEqual => (self.fresh_var(), Type::Bool),
                    BinaryOp::Add |
                    BinaryOp::Divide |
                    BinaryOp::Modulo |
                    BinaryOp::Multiply |
                    BinaryOp::Subtract => {
                        let var = self.fresh_int_var();
                        (var.clone(), var)
                    }
                    BinaryOp::And | BinaryOp::Or => (Type::Bool, Type::Bool),
                    BinaryOp::Greater |
                    BinaryOp::GreaterEqual |
                    BinaryOp::Less |
                    BinaryOp::LessEqual => (self.fresh_int_var(), Type::Bool),
                    BinaryOp::Assign => {
                        let reason = Reason::Assign(rhs.span);
                        let typ = self.unify(&rhs.typ, &lhs.typ, reason);
                        let typed = t::Expr::Binary(lhs, op, rhs);
                        return t::TypedExpr {
                            expr: Box::new(typed),
                            typ,
                            span: Spanned::span(expr),
                        };
                    }
                };
                let param_type = self.unify(&lhs.typ, &param_type, Reason::LeftOperand(lhs.span));
                self.unify(&rhs.typ, &param_type, Reason::RightOperand(rhs.span));
                (t::Expr::Binary(lhs, op, rhs), out_type)
            }
            r::Expr::Call(ref expr, ref params) => {
                let expr = self.infer_expr(expr);
                let expr_type = self.unifier.shallow_normalize(&expr.typ);
                match expr_type {
                    Type::Function(param_types, out_type) => if params.len() != param_types.len() {
                        let end = if param_types.len() % 100 == 11 || param_types.len() % 10 != 1 {
                            "s"
                        } else {
                            ""
                        };
                        let msg = format!(
                            "function expects {} parameter{}, got {}",
                            param_types.len(),
                            end,
                            params.len()
                        );
                        let short_msg = format!("expected {} parameter{}", param_types.len(), end);
                        self.ctx
                            .reporter
                            .error(msg, expr.span)
                            .span_note(short_msg, expr.span)
                            .build();
                        (t::Expr::Error, Type::Error)
                    } else {
                        let params = params
                            .iter()
                            .map(|p| self.infer_expr(p))
                            .collect::<Vec<_>>();
                        for i in 0..params.len() {
                            let reason = Reason::FunctionParam(i, params[i].span);
                            self.unify(&params[i].typ, &param_types[i], reason);
                        }
                        let expr = t::Expr::Call(expr, params);
                        (expr, (*out_type).clone())
                    },
                    Type::Error => (t::Expr::Error, Type::Error),
                    Type::Var(_) => {
                        self.ctx
                            .reporter
                            .error("cannot infer the type before call", expr.span)
                            .span(expr.span)
                            .build();
                        (t::Expr::Error, Type::Error)
                    }
                    typ => {
                        let msg = format!("cannot call {}", self.type_name(&typ));
                        self.ctx
                            .reporter
                            .error(msg, expr.span)
                            .span(expr.span)
                            .build();
                        (t::Expr::Error, Type::Error)
                    }
                }
            }
            r::Expr::Error => (t::Expr::Error, t::Type::Error),
            r::Expr::Field(ref expr, ref field) => {
                let expr = self.infer_expr(expr);
                let expr_type = self.unifier.shallow_normalize(&expr.typ);
                match expr_type {
                    Type::Concrete(sym, ref params) => {
                        let res = self.fields.get(&sym).and_then(|f| f.get(&**field));
                        match res {
                            Some(&(index, ref scheme)) => {
                                debug_assert_eq!(scheme.vars.len(), params.len());
                                let typ = scheme.instantiate(params);
                                let expr = t::Expr::Field(expr, Spanned::map_ref(field, |_| index));
                                (expr, typ)
                            }
                            None => {
                                let msg = format!(
                                    "{} does not have field `{}`",
                                    self.type_name(&expr_type),
                                    Spanned::value(field)
                                );
                                self.ctx
                                    .reporter
                                    .error(msg, Spanned::span(field))
                                    .span(Spanned::span(field))
                                    .build();
                                (t::Expr::Error, Type::Error)
                            }
                        }
                    }
                    Type::Error => (t::Expr::Error, Type::Error),
                    Type::Var(_) => {
                        self.ctx
                            .reporter
                            .error("cannot infer the type before field access", expr.span)
                            .span(expr.span)
                            .build();
                        (t::Expr::Error, Type::Error)
                    }
                    typ => {
                        let msg = format!(
                            "no field `{}` on {}",
                            Spanned::value(field),
                            self.type_name(&typ)
                        );
                        self.ctx
                            .reporter
                            .error(msg, expr.span)
                            .span(expr.span)
                            .build();
                        (t::Expr::Error, Type::Error)
                    }
                }
            }
            r::Expr::Literal(ref literal) => {
                let typ = self.infer_literal(literal);
                let expr = t::Expr::Literal(literal.clone());
                (expr, typ)
            }
            r::Expr::Name(name, ref params) => {
                let params = params
                    .iter()
                    .map(|p| Spanned::map_ref(p, |p| self.convert_resolved_type(p)))
                    .collect::<Vec<_>>();
                let typ = self.infer_symbol(Spanned::into_value(name), &params);
                (t::Expr::Name(name, params), typ)
            }
            r::Expr::Unary(op, ref expr) => {
                let expr = self.infer_expr(expr);
                let (param_type, out_type) = match Spanned::into_value(op) {
                    UnaryOp::AddressOf => {
                        let var = self.fresh_var();
                        (var.clone(), Type::Pointer(Rc::new(var)))
                    }
                    UnaryOp::Deref => {
                        let var = self.fresh_var();
                        (Type::Pointer(Rc::new(var.clone())), var)
                    }
                    UnaryOp::Minus | UnaryOp::Plus => {
                        let var = self.fresh_int_var();
                        (var.clone(), var)
                    }
                    UnaryOp::Not => (Type::Bool, Type::Bool),
                };
                self.unify(&expr.typ, &param_type, Reason::UnaryOperand(expr.span));
                (t::Expr::Unary(op, expr), out_type)
            }
        };
        t::TypedExpr {
            expr: Box::new(typed),
            typ,
            span: Spanned::span(expr),
        }
    }

    fn infer_statement(&mut self, stmt: &r::Statement) -> t::Statement {
        match *stmt {
            r::Statement::Block(ref stmts) => t::Statement::Block(
                stmts
                    .iter()
                    .map(|s| Spanned::map_ref(s, |s| self.infer_statement(s)))
                    .collect(),
            ),
            r::Statement::Break => t::Statement::Break,
            r::Statement::Continue => t::Statement::Continue,
            r::Statement::Expr(ref expr) => t::Statement::Expr(self.infer_expr(expr)),
            r::Statement::If(ref cond, ref then, ref else_) => {
                let cond = self.infer_expr(cond);
                let then = Spanned::map_ref(then, |s| self.infer_statement(s));
                let else_ = else_
                    .as_ref()
                    .map(|e| Spanned::map_ref(e, |e| self.infer_statement(e)));
                self.unify(&cond.typ, &Type::Bool, Reason::IfCondition(cond.span));
                t::Statement::If(cond, Box::new(then), else_.map(Box::new))
            }
            r::Statement::Let(sym, ref typ, ref value) => {
                let value = self.infer_expr(value);
                let ty = self.convert_resolved_type(typ);
                let reason = Reason::Assign(value.span);
                let scheme = Scheme {
                    vars: Vec::new(),
                    typ: self.unify(&value.typ, &ty, reason),
                };
                self.env.insert(Spanned::into_value(sym), scheme);
                let typ = Spanned::new(ty, Spanned::span(typ));
                t::Statement::Let(sym, typ, value)
            }
            r::Statement::Loop(ref stmt) => {
                let s = Spanned::map_ref(stmt, |s| self.infer_statement(s));
                t::Statement::Loop(Box::new(s))
            }
            r::Statement::Return(ref expr) => {
                let expr = self.infer_expr(expr);
                let expected = self.return_type.clone().unwrap();
                self.unify(&expr.typ, &expected, Reason::Return(expr.span));
                t::Statement::Return(expr)
            }
            r::Statement::While(ref cond, ref body) => {
                let cond = self.infer_expr(cond);
                let body = Spanned::map_ref(body, |s| self.infer_statement(s));
                self.unify(&cond.typ, &Type::Bool, Reason::WhileCondition(cond.span));
                t::Statement::While(cond, Box::new(body))
            }
        }
    }

    fn infer_function(&mut self, function: &r::Function) -> t::Function {
        let mut params = Vec::new();
        for param in &function.params {
            let typ = self.convert_resolved_type(&param.typ);
            let scheme = Scheme {
                vars: Vec::new(),
                typ: typ.clone(),
            };
            self.env.insert(Spanned::into_value(param.name), scheme);
            params.push(t::Var {
                name: Spanned::into_value(param.name),
                typ,
            });
        }
        let return_type = self.convert_resolved_type(&function.return_type);
        self.return_type = Some(return_type);
        let body = function.body.as_ref().map(|s| {
            Spanned::map_ref(s, |s| {
                let mut stmt = self.infer_statement(s);
                self.normalize_statement(&mut stmt);
                stmt
            })
        });
        t::Function {
            complete_span: function.complete_span,
            fn_type: function.fn_type,
            params,
            return_type: self.return_type.take().unwrap(),
            body,
            name: Spanned::into_value(function.name.name),
            type_params: function.name.type_params.iter().map(|t| **t).collect(),
        }
    }

    fn normalize_statement(&mut self, stmt: &mut t::Statement) {
        match *stmt {
            t::Statement::If(ref mut cond, ref mut then, ref mut else_) => {
                self.normalize_expr(cond);
                self.normalize_statement(then);
                if let Some(ref mut stmt) = *else_ {
                    self.normalize_statement(stmt);
                }
            }
            t::Statement::Break | t::Statement::Continue => {}
            t::Statement::Return(ref mut expr) | t::Statement::Expr(ref mut expr) => {
                self.normalize_expr(expr);
            }
            t::Statement::Block(ref mut stmts) => for stmt in stmts {
                self.normalize_statement(stmt);
            },
            t::Statement::Loop(ref mut stmt) => {
                self.normalize_statement(stmt);
            }
            t::Statement::While(ref mut cond, ref mut stmt) => {
                self.normalize_expr(cond);
                self.normalize_statement(stmt);
            }
            t::Statement::Let(_, ref mut typ, ref mut value) => {
                self.normalize_expr(value);
                match self.unifier.normalize(typ) {
                    Ok(t) => *Spanned::value_mut(typ) = t,
                    Err(()) => {
                        let span = Spanned::span(typ);
                        self.ctx
                            .reporter
                            .error("could not completely infer type", span)
                            .span(span)
                            .build();
                        *Spanned::value_mut(typ) = Type::Error;
                    }
                }
            }
        }
    }

    fn normalize_expr(&mut self, expr: &mut t::TypedExpr) {
        match *expr.expr.as_mut() {
            t::Expr::Binary(ref mut lhs, _, ref mut rhs) => {
                self.normalize_expr(lhs);
                self.normalize_expr(rhs);
            }
            t::Expr::Field(ref mut expr, _) | t::Expr::Unary(_, ref mut expr) => {
                self.normalize_expr(expr);
            }
            t::Expr::Call(ref mut expr, ref mut params) => {
                self.normalize_expr(expr);
                for param in params {
                    self.normalize_expr(param);
                }
            }
            t::Expr::Error => {
                expr.typ = Type::Error;
                return;
            }
            t::Expr::Literal(_) => {}
            t::Expr::Name(_, ref mut params) => for param in params {
                match self.unifier.normalize(param) {
                    Ok(t) => *Spanned::value_mut(param) = t,
                    Err(()) => {
                        let span = Spanned::span(param);
                        self.ctx
                            .reporter
                            .error("could not completely infer type", span)
                            .span(span)
                            .build();
                        *Spanned::value_mut(param) = Type::Error;
                    }
                }
            },
        }
        match self.unifier.normalize(&expr.typ) {
            Ok(typ) => expr.typ = typ,
            Err(()) => {
                expr.typ = t::Type::Error;
                self.ctx
                    .reporter
                    .error("could not completely infer type", expr.span)
                    .span(expr.span)
                    .build();
            }
        }
    }

    fn infer_program(&mut self, program: &r::Program) -> t::Program {
        let mut functions = Vec::new();
        let mut structs = Vec::new();
        for s in &program.structs {
            structs.push(self.convert_struct(s));
        }
        for f in &program.functions {
            self.add_function_to_env(f);
        }
        for f in &program.functions {
            functions.push(self.infer_function(f));
        }
        t::Program { structs, functions }
    }

    fn add_function_to_env(&mut self, f: &r::Function) {
        let param_types = f.params
            .iter()
            .map(|p| self.convert_resolved_type(&p.typ))
            .collect::<Vec<_>>();
        let vars = f.name.type_params.iter().map(|p| **p).collect();
        let return_type = self.convert_resolved_type(&f.return_type);
        let typ = Type::Function(param_types.into(), Rc::new(return_type));
        let scheme = Scheme { vars, typ };
        self.env.insert(Spanned::into_value(f.name.name), scheme);
    }

    fn convert_struct(&mut self, s: &r::Struct) -> t::Struct {
        let param_types = s.fields
            .iter()
            .map(|f| self.convert_resolved_type(&f.typ))
            .collect::<Vec<_>>();
        let vars = s.name.type_params.iter().map(|p| **p).collect::<Vec<_>>();
        let return_type = Type::Concrete(
            Spanned::into_value(s.name.name),
            s.name
                .type_params
                .iter()
                .map(|p| Type::Concrete(**p, Vec::new().into()))
                .collect::<Vec<_>>()
                .into(),
        );
        let typ = Type::Function(param_types.into(), Rc::new(return_type));
        let scheme = Scheme {
            vars: vars.clone(),
            typ,
        };
        self.env.insert(Spanned::into_value(s.name.name), scheme);
        let fields = s.fields
            .iter()
            .map(|f| {
                t::Var {
                    name: Spanned::into_value(f.name),
                    typ: self.convert_resolved_type(&f.typ),
                }
            })
            .collect();
        for (index, field) in s.fields.iter().enumerate() {
            let scheme = Scheme {
                vars: vars.clone(),
                typ: self.convert_resolved_type(&field.typ),
            };
            let name = Spanned::into_value(s.name.name);
            let field_name = self.ctx.symbols.get_name(Spanned::into_value(field.name));
            self.fields
                .entry(name)
                .or_insert_with(HashMap::new)
                .insert(field_name.into(), (index, scheme));
        }
        t::Struct {
            complete_span: s.complete_span,
            name: Spanned::into_value(s.name.name),
            type_params: vars,
            fields,
        }
    }
}

pub(crate) fn type_check(program: &r::Program, ctx: &mut CompileCtx) -> t::Program {
    let mut inferer = Inferer::new(ctx);
    inferer.infer_program(program)
}
