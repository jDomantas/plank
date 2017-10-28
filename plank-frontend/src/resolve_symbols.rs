use std::collections::HashMap;
use std::collections::hash_map::Entry;
use plank_syntax::ast as p;
use plank_syntax::position::{Position, Span, Spanned};
use ast::resolved::{self as r, Symbol};
use CompileCtx;


pub(crate) fn resolve_program(program: &p::Program, ctx: &mut CompileCtx) -> r::Program {
    let mut resolver = Resolver::new(ctx);
    resolver.resolve_program(program)
}

struct Resolver<'a> {
    ctx: &'a mut CompileCtx,
    global_structs: HashMap<String, (Symbol, Span)>,
    global_functions: HashMap<String, Function>,
    type_vars: HashMap<String, Symbol>,
    scopes: Vec<HashMap<String, Symbol>>,
}

impl<'a> Resolver<'a> {
    fn new(ctx: &'a mut CompileCtx) -> Self {
        Resolver {
            ctx,
            global_structs: HashMap::new(),
            global_functions: HashMap::new(),
            type_vars: HashMap::new(),
            scopes: Vec::new(),
        }
    }

    fn resolve_program(&mut self, program: &p::Program) -> r::Program {
        self.collect_globals(program);
        self.add_builtins();

        let structs = program
            .structs
            .iter()
            .map(|s| self.resolve_struct(s))
            .collect();

        let mut functions = program
            .functions
            .iter()
            .map(|f| self.resolve_function(f))
            .collect::<Vec<_>>();

        functions.push(make_builtin_size_of());
        functions.push(make_builtin_align_of());
        functions.push(make_builtin_getc());
        functions.push(make_builtin_putc());

        r::Program { structs, functions }
    }

    fn collect_globals(&mut self, program: &p::Program) {
        for struct_ in &program.structs {
            let name = &struct_.name.name.0;
            let span = Spanned::span(&struct_.name.name);
            // add function only if struct is seen for the first time yet,
            // because otherwise we will get "struct defined multiple times"
            // AND "function defined multiple times" on same positions
            if self.add_struct(name, span).is_ok() {
                let params = struct_.fields.iter().map(|f| f.name.0.clone());
                self.add_function(name, span, params);
            }
        }

        for fn_ in &program.functions {
            let name = &fn_.name.name.0;
            let span = Spanned::span(&fn_.name.name);
            let params = fn_.params.iter().map(|f| f.name.0.clone());
            self.add_function(name, span, params);
        }
    }

    fn add_struct(&mut self, name: &str, mut span: Span) -> Result<(), ()> {
        match self.global_structs.entry(name.into()) {
            Entry::Vacant(entry) => {
                let symbol = self.ctx.symbols.new_symbol(name);
                entry.insert((symbol, span));
                Ok(())
            }
            Entry::Occupied(entry) => {
                let mut prev_span = entry.get().1;
                // always have previous item going before current
                if prev_span.start > span.start {
                    ::std::mem::swap(&mut prev_span, &mut span);
                }
                let msg = format!("struct `{}` is defined multiple times", name);
                let short_msg = format!("struct `{}` is defined here", name);
                self.ctx
                    .reporter
                    .error(msg, span)
                    .span_note("and again defined here", span)
                    .span_note(short_msg, prev_span)
                    .build();
                Err(())
            }
        }
    }

    fn add_function<I>(&mut self, name: &str, mut span: Span, param_names: I)
    where
        I: Iterator<Item = String>,
    {
        if name == "size_of" {
            self.ctx
                .reporter
                .error("`size_of` is a built-in function", span)
                .span(span)
                .build();
            return;
        } else if name == "align_of" {
            self.ctx
                .reporter
                .error("`align_of` is a built-in function", span)
                .span(span)
                .build();
            return;
        } else if name == "putc" {
            self.ctx
                .reporter
                .error("`putc` is a built-in function", span)
                .span(span)
                .build();
            return;
        } else if name == "getc" {
            self.ctx
                .reporter
                .error("`getc` is a built-in function", span)
                .span(span)
                .build();
            return;
        }
        match self.global_functions.entry(name.into()) {
            Entry::Vacant(entry) => {
                // symbol might have already be defined for struct,
                // so check for that
                let symbol = self.global_structs
                    .get(name)
                    .map(|&(s, _)| s)
                    .unwrap_or(self.ctx.symbols.new_symbol(name));
                entry.insert(Function {
                    name: symbol,
                    name_span: span,
                    param_names: param_names.collect(),
                });
            }
            Entry::Occupied(entry) => {
                let mut prev_span = entry.get().name_span;
                // always have previous item going before current
                if prev_span.start > span.start {
                    ::std::mem::swap(&mut prev_span, &mut span);
                }
                let msg = format!("function `{}` is defined multiple times", name);
                let short_msg = format!("function `{}` is defined here", name);
                self.ctx
                    .reporter
                    .error(msg, span)
                    .span_note("and again defined here", span)
                    .span_note(short_msg, prev_span)
                    .build();
            }
        }
    }

    fn add_builtins(&mut self) {
        let dummy_span = Span {
            start: Position { line: 0, column: 0 },
            end: Position { line: 0, column: 0 },
        };
        self.global_functions.insert(
            "size_of".into(),
            Function {
                name: ::builtins::SIZE_OF,
                name_span: dummy_span,
                param_names: Vec::new(),
            },
        );
        self.global_functions.insert(
            "align_of".into(),
            Function {
                name: ::builtins::ALIGN_OF,
                name_span: dummy_span,
                param_names: Vec::new(),
            },
        );
        self.global_functions.insert(
            "putc".into(),
            Function {
                name: ::builtins::PUTC,
                name_span: dummy_span,
                param_names: vec!["ch".into()],
            },
        );
        self.global_functions.insert(
            "getc".into(),
            Function {
                name: ::builtins::GETC,
                name_span: dummy_span,
                param_names: Vec::new(),
            },
        );
    }

    fn resolve_struct(&mut self, struct_: &p::Struct) -> r::Struct {
        let name = self.resolve_item_name(&struct_.name);
        let fields = self.resolve_var_list(&struct_.fields, "field");
        r::Struct {
            name,
            fields,
            complete_span: struct_.complete_span,
        }
    }

    fn resolve_item_name(&mut self, name: &p::ItemName) -> r::ItemName {
        let symbol = self.global_functions[&name.name.0].name;
        let mut type_var_spans = HashMap::new();
        let mut type_params = Vec::new();
        for var in &name.type_params {
            let symbol = if let Some(&span) = type_var_spans.get(&var.0) {
                let msg = format!("type parameter `{}` is listed multiple times", &var.0);
                let short_msg = format!("`{}` is defined here", &var.0);
                self.ctx
                    .reporter
                    .error(msg, Spanned::span(var))
                    .span_note(short_msg, Spanned::span(var))
                    .span_note("and here", span)
                    .build();
                self.ctx.symbols.new_symbol("?")
            } else {
                type_var_spans.insert(&var.0, Spanned::span(var));
                let symbol = self.ctx.symbols.new_symbol(var.0.clone());
                self.type_vars.insert(var.0.clone(), symbol);
                symbol
            };
            type_params.push(Spanned::new(symbol, Spanned::span(var)));
        }
        r::ItemName {
            name: Spanned::new(symbol, Spanned::span(&name.name)),
            type_params,
        }
    }

    fn resolve_var_list(&mut self, vars: &[p::Var], kind: &str) -> Vec<r::Var> {
        let mut result_vars = Vec::new();
        let mut var_spans = HashMap::new();
        for var in vars {
            let name = &var.name.0;
            let span = Spanned::span(&var.name);
            if let Some(&span) = var_spans.get(name) {
                let msg = format!("{} `{}` is listed multiple times", kind, name);
                let short_msg = format!("`{}` is defined here", name);
                self.ctx
                    .reporter
                    .error(msg, span)
                    .span_note("and again here", span)
                    .span_note(short_msg, span)
                    .build();
            } else {
                var_spans.insert(name, span);
            }
            let symbol = self.ctx.symbols.new_symbol(name.clone());
            let field_type = self.resolve_type(&var.typ);
            let field = r::Var {
                name: Spanned::new(symbol, span),
                typ: field_type,
            };
            result_vars.push(field);
        }
        result_vars
    }

    fn resolve_type(&mut self, typ: &Spanned<p::Type>) -> Spanned<r::Type> {
        let span = Spanned::span(typ);
        let typ = match **typ {
            p::Type::Bool => r::Type::Bool,
            p::Type::I8 => r::Type::I8,
            p::Type::U8 => r::Type::U8,
            p::Type::I16 => r::Type::I16,
            p::Type::U16 => r::Type::U16,
            p::Type::I32 => r::Type::I32,
            p::Type::U32 => r::Type::U32,
            p::Type::Wildcard => r::Type::Wildcard,
            p::Type::Pointer(ref typ) => {
                let typ = self.resolve_type(typ);
                r::Type::Pointer(Box::new(typ))
            }
            p::Type::Function(ref params, ref out) => {
                let params = params.iter().map(|typ| self.resolve_type(typ)).collect();
                let out = self.resolve_type(out);
                r::Type::Function(params, Box::new(out))
            }
            p::Type::Concrete(ref name, ref params) => {
                let params = params.iter().map(|typ| self.resolve_type(typ)).collect();
                if let Some(&sym) = self.type_vars.get(&name.0) {
                    let name = Spanned::new(sym, Spanned::span(name));
                    r::Type::Concrete(name, params)
                } else if let Some(sym) = self.global_structs.get(&name.0) {
                    let name = Spanned::new(sym.0, Spanned::span(name));
                    r::Type::Concrete(name, params)
                } else {
                    let msg = format!("unknown type `{}`", &name.0);
                    self.ctx
                        .reporter
                        .error(msg, Spanned::span(name))
                        .span_note("unknown type", Spanned::span(name))
                        .build();
                    r::Type::Error
                }
            }
        };
        Spanned::new(typ, span)
    }

    fn resolve_function(&mut self, f: &p::Function) -> r::Function {
        let name = self.resolve_item_name(&f.name);
        let params = self.resolve_var_list(&f.params, "parameter");
        let return_type = self.resolve_type(&f.return_type);

        debug_assert!(self.scopes.is_empty());
        self.scopes.push(HashMap::new());
        for (name, sym) in f.params.iter().zip(params.iter()) {
            self.add_local(&name.name.0, Spanned::into_value(sym.name));
        }
        let body = f.body.as_ref().map(|s| self.resolve_statement(s));
        self.scopes.clear();

        r::Function {
            complete_span: f.complete_span,
            name,
            params,
            return_type,
            body,
            fn_type: f.fn_type,
        }
    }

    fn resolve_statement(&mut self, s: &Spanned<p::Statement>) -> Spanned<r::Statement> {
        let span = Spanned::span(s);
        let statement: r::Statement = match **s {
            p::Statement::Block(ref statements) => {
                self.scopes.push(HashMap::new());
                let statements = statements
                    .iter()
                    .map(|s| self.resolve_statement(s))
                    .collect();
                self.scopes.pop().expect("missing scope");
                r::Statement::Block(statements)
            }
            p::Statement::Break => r::Statement::Break,
            p::Statement::Continue => r::Statement::Continue,
            p::Statement::Expr(ref expr) => {
                let expr = self.resolve_expr(expr);
                r::Statement::Expr(expr)
            }
            p::Statement::If(ref cond, ref then, ref else_) => {
                let cond = self.resolve_expr(cond);
                let then = self.resolve_statement(then);
                let else_ = else_.as_ref().map(|s| self.resolve_statement(s));
                r::Statement::If(cond, Box::new(then), else_.map(Box::new))
            }
            p::Statement::Let(ref name, ref typ, ref value) => {
                let name_span = Spanned::span(name);
                let typ = typ.as_ref()
                    .map(|t| self.resolve_type(t))
                    .unwrap_or(Spanned::new(r::Type::Wildcard, name_span));
                let value = self.resolve_expr(value);
                let symbol = self.ctx.symbols.new_symbol(name.0.clone());
                self.add_local(&name.0, symbol);
                let symbol = Spanned::new(symbol, name_span);
                r::Statement::Let(symbol, typ, value)
            }
            p::Statement::Loop(ref statement) => {
                let statement = self.resolve_statement(statement);
                r::Statement::Loop(Box::new(statement))
            }
            p::Statement::Return(ref expr) => {
                let expr = self.resolve_expr(expr);
                r::Statement::Return(expr)
            }
            p::Statement::While(ref cond, ref body) => {
                let cond = self.resolve_expr(cond);
                let body = self.resolve_statement(body);
                r::Statement::While(cond, Box::new(body))
            }
            p::Statement::Error => r::Statement::Error,
        };
        Spanned::new(statement, span)
    }

    fn resolve_expr(&mut self, e: &Spanned<p::Expr>) -> Spanned<r::Expr> {
        let span = Spanned::span(e);
        let expr = match **e {
            p::Expr::Binary(ref lhs, op, ref rhs) => {
                let lhs = self.resolve_expr(lhs);
                let rhs = self.resolve_expr(rhs);
                r::Expr::Binary(Box::new(lhs), op, Box::new(rhs))
            }
            p::Expr::Call(ref expr, ref params) => {
                let expr = self.resolve_expr(expr);
                if let Some(params) = self.resolve_call_params(&expr, params) {
                    r::Expr::Call(Box::new(expr), params)
                } else {
                    r::Expr::Error
                }
            }
            p::Expr::Field(ref expr, ref field) => {
                let expr = self.resolve_expr(expr);
                let field = Spanned::new(field.0.clone(), Spanned::span(field));
                r::Expr::Field(Box::new(expr), field)
            }
            p::Expr::Literal(ref lit) => r::Expr::Literal(lit.clone()),
            p::Expr::Name(ref name, ref params) => {
                let params = params.iter().map(|t| self.resolve_type(t)).collect();
                let name_span = Spanned::span(name);
                match self.resolve_var(&name.0, name_span) {
                    Some(symbol) => {
                        let name = Spanned::new(symbol, name_span);
                        r::Expr::Name(name, params)
                    }
                    None => r::Expr::Error,
                }
            }
            p::Expr::Unary(op, ref expr) => {
                let expr = self.resolve_expr(expr);
                r::Expr::Unary(op, Box::new(expr))
            }
            p::Expr::Cast(ref expr, ref typ) => {
                let expr = self.resolve_expr(expr);
                let typ = self.resolve_type(typ);
                r::Expr::Cast(Box::new(expr), typ)
            }
        };
        Spanned::new(expr, span)
    }

    fn resolve_call_params(
        &mut self,
        callee: &r::Expr,
        params: &[p::CallParam],
    ) -> Option<Vec<Spanned<r::Expr>>> {
        match params.get(0) {
            None => Some(Vec::new()),
            Some(&p::CallParam::Named(_, _)) => self.resolve_named_params(callee, params),
            Some(&p::CallParam::Unnamed(_)) => self.resolve_unnamed_params(params),
        }
    }

    fn resolve_named_params(
        &mut self,
        callee: &r::Expr,
        params: &[p::CallParam],
    ) -> Option<Vec<Spanned<r::Expr>>> {
        let name_and_pos = match *callee {
            r::Expr::Name(name, _) => {
                let name = Spanned::into_value(name);
                let name_str = self.ctx.symbols.get_name(name);
                if let Some(f) = self.global_functions.get(name_str) {
                    let positions = f.param_names
                        .iter()
                        .cloned()
                        .enumerate()
                        .map(|(index, name)| (name, index))
                        .collect::<HashMap<_, _>>();
                    Some((name_str.to_string(), positions))
                } else {
                    None
                }
            }
            _ => None,
        };

        if let Some((fn_name, positions)) = name_and_pos {
            self.resolve_and_sort_params(fn_name, params, positions)
        } else {
            let span = param_list_span(params);
            let msg = "named parameters can only be used for global functions";
            self.ctx.reporter.error(msg, span).span(span).build();
            self.resolve_remaining_params(params);
            None
        }
    }

    fn resolve_and_sort_params(
        &mut self,
        fn_name: String,
        params: &[p::CallParam],
        mut positions: HashMap<String, usize>,
    ) -> Option<Vec<Spanned<r::Expr>>> {
        let mut resolved = Vec::new();
        let mut iterator = params.iter();
        while let Some(param) = iterator.next() {
            match *param {
                p::CallParam::Named(ref ident, ref expr) => {
                    let expr = self.resolve_expr(expr);
                    match positions.get_mut(&ident.0) {
                        None => {
                            // bad param name
                            let msg = format!(
                                "`{}` does not have a parameter named `{}`",
                                fn_name,
                                ident.0,
                            );
                            let span = Spanned::span(ident);
                            self.ctx.reporter.error(msg, span).span(span).build();
                            self.resolve_remaining_params(iterator.as_slice());
                            return None;
                        }
                        Some(&mut index) if index == ::std::usize::MAX => {
                            // good name, but already used
                            let msg = format!("parameter `{}` is supplied twice", ident.0,);
                            let span = Spanned::span(ident);
                            self.ctx.reporter.error(msg, span).span(span).build();
                            self.resolve_remaining_params(iterator.as_slice());
                            return None;
                        }
                        Some(index) => {
                            // good name, now we have its real index
                            while resolved.len() <= *index {
                                resolved.push(None);
                            }
                            resolved[*index] = Some(expr);
                            *index = ::std::usize::MAX;
                        }
                    }
                }
                p::CallParam::Unnamed(ref expr) => {
                    let span = param_list_span(params);
                    let msg = "cannot mix named and positional parameters";
                    self.ctx.reporter.error(msg, span).span(span).build();
                    self.resolve_expr(expr);
                    self.resolve_remaining_params(iterator.as_slice());
                    return None;
                }
            }
        }
        for (name, pos) in positions {
            if pos != ::std::usize::MAX {
                let msg = format!("missing parameter `{}`", name);
                let span = param_list_span(params);
                self.ctx.reporter.error(msg, span).span(span).build();
                return None;
            }
        }
        Some(resolved.into_iter().map(Option::unwrap).collect())
    }

    fn resolve_unnamed_params(&mut self, params: &[p::CallParam]) -> Option<Vec<Spanned<r::Expr>>> {
        let mut resolved = Vec::new();
        let mut iterator = params.iter();
        while let Some(param) = iterator.next() {
            match *param {
                p::CallParam::Named(_, ref expr) => {
                    let span = param_list_span(params);
                    let msg = "cannot mix named and positional parameters";
                    self.ctx.reporter.error(msg, span).span(span).build();
                    self.resolve_expr(expr);
                    self.resolve_remaining_params(iterator.as_slice());
                    return None;
                }
                p::CallParam::Unnamed(ref expr) => {
                    resolved.push(self.resolve_expr(expr));
                }
            }
        }
        Some(resolved)
    }

    fn resolve_remaining_params(&mut self, params: &[p::CallParam]) {
        for param in params {
            match *param {
                p::CallParam::Named(_, ref expr) | p::CallParam::Unnamed(ref expr) => {
                    self.resolve_expr(expr);
                }
            }
        }
    }

    fn resolve_var(&mut self, var: &str, span: Span) -> Option<Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(&symbol) = scope.get(var) {
                return Some(symbol);
            }
        }
        if let Some(f) = self.global_functions.get(var) {
            return Some(f.name);
        }
        let msg = format!("unknown value `{}`", var);
        self.ctx.reporter.error(msg, span).span(span).build();
        None
    }

    fn add_local(&mut self, name: &str, symbol: Symbol) {
        let scope = self.scopes.last_mut().expect("missing scope");
        scope.insert(name.into(), symbol);
    }
}

fn make_builtin_size_of() -> r::Function {
    let dummy_span = Span {
        start: Position { line: 0, column: 0 },
        end: Position { line: 0, column: 0 },
    };
    r::Function {
        complete_span: dummy_span,
        name: r::ItemName {
            name: Spanned::new(::builtins::SIZE_OF, dummy_span),
            type_params: vec![Spanned::new(::builtins::SIZE_OF_TYPE_PARAM, dummy_span)],
        },
        params: Vec::new(),
        return_type: Spanned::new(r::Type::U32, dummy_span),
        body: None,
        fn_type: r::FunctionType::Normal,
    }
}

fn make_builtin_align_of() -> r::Function {
    let dummy_span = Span {
        start: Position { line: 0, column: 0 },
        end: Position { line: 0, column: 0 },
    };
    r::Function {
        complete_span: dummy_span,
        name: r::ItemName {
            name: Spanned::new(::builtins::ALIGN_OF, dummy_span),
            type_params: vec![Spanned::new(::builtins::ALIGN_OF_TYPE_PARAM, dummy_span)],
        },
        params: Vec::new(),
        return_type: Spanned::new(r::Type::U32, dummy_span),
        body: None,
        fn_type: r::FunctionType::Normal,
    }
}

fn make_builtin_getc() -> r::Function {
    let dummy_span = Span {
        start: Position { line: 0, column: 0 },
        end: Position { line: 0, column: 0 },
    };
    r::Function {
        complete_span: dummy_span,
        name: r::ItemName {
            name: Spanned::new(::builtins::GETC, dummy_span),
            type_params: Vec::new(),
        },
        params: Vec::new(),
        return_type: Spanned::new(r::Type::I32, dummy_span),
        body: None,
        fn_type: r::FunctionType::Normal,
    }
}

fn make_builtin_putc() -> r::Function {
    let dummy_span = Span {
        start: Position { line: 0, column: 0 },
        end: Position { line: 0, column: 0 },
    };
    r::Function {
        complete_span: dummy_span,
        name: r::ItemName {
            name: Spanned::new(::builtins::PUTC, dummy_span),
            type_params: Vec::new(),
        },
        params: vec![
            r::Var {
                name: Spanned::new(::builtins::PUTC_PARAM, dummy_span),
                typ: Spanned::new(r::Type::U8, dummy_span),
            },
        ],
        return_type: Spanned::new(r::Type::U32, dummy_span),
        body: None,
        fn_type: r::FunctionType::Normal,
    }
}

struct Function {
    name: Symbol,
    name_span: Span,
    param_names: Vec<String>,
}

fn param_list_span(params: &[p::CallParam]) -> Span {
    assert!(!params.is_empty());
    let span = match params[0] {
        p::CallParam::Named(ref ident, ref expr) => Spanned::span(ident).merge(Spanned::span(expr)),
        p::CallParam::Unnamed(ref expr) => Spanned::span(expr),
    };
    match params.last() {
        Some(&p::CallParam::Named(_, ref expr)) | Some(&p::CallParam::Unnamed(ref expr)) => {
            span.merge(Spanned::span(expr))
        }
        None => span,
    }
}
