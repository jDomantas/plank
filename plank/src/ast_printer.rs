use plank_syntax::ast::{Program, Statement, Expr, Function, Struct, Ident, Var, Type, FunctionType, BinaryOp, UnaryOp, CallParam, Literal, Signedness, Size};


#[derive(Copy, Clone)]
enum ListFormat {
    Singleline,
    Multiline(u32),
}

struct LispFormatter {
    output: String,
    list_stack: Vec<ListFormat>,
    at_list_start: bool,
    next_indent: u32,
}

impl LispFormatter {
    fn new() -> LispFormatter {
        LispFormatter {
            output: String::new(),
            list_stack: Vec::new(),
            at_list_start: true,
            next_indent: 1,
        }
    }

    fn start_list(&mut self) {
        self.write_symbol("(");
        self.list_stack.push(ListFormat::Singleline);
        self.at_list_start = true;
    }

    fn end_list(&mut self) {
        // don't use write_str because we want
        // this to be right after last item
        self.output.push(')');
        self.at_list_start = false;
        match self.list_stack.pop() {
            Some(ListFormat::Singleline) => {}
            Some(ListFormat::Multiline(indent)) => {
                self.next_indent = indent;
            }
            None => {
                panic!("not inside a list, so no list to end");
            }
        }
    }

    fn write_symbol(&mut self, symbol: &str) {
        if !self.at_list_start {
            match self.list_stack.last() {
                Some(&ListFormat::Singleline) => {
                    self.output.push(' ');
                }
                Some(&ListFormat::Multiline(indent)) => {
                    self.output.push('\n');
                    for _ in 0..indent {
                        self.output.push_str("  ");
                    }
                }
                None => {
                    // not inside a list - in global scope
                    self.output.push_str("\n\n");
                }
            }
        }
        self.output.push_str(symbol);
        self.at_list_start = false;
    }

    fn make_list_multiline(&mut self) {
        match self.list_stack.last().cloned() {
            Some(ListFormat::Singleline) => {
                self.at_list_start = false;
                self.list_stack.pop();
                let format = ListFormat::Multiline(self.next_indent);
                self.next_indent += 1;
                self.list_stack.push(format);
            }
            Some(ListFormat::Multiline(_)) |
            None => {
                // either in already multiline list,
                // or in global scope which is multiline by default
            }
        }
    }

    fn into_string(self) -> String {
        self.output
    }
}

struct Formatter {
    fmt: LispFormatter,
}

impl Formatter {
    fn new() -> Formatter {
        Formatter {
            fmt: LispFormatter::new(),
        }
    }

    fn format_program(&mut self, program: &Program) {
        for s in &program.structs {
            self.format_struct(s);
        }
        for f in &program.functions {
            self.format_fn(f);
        }
    }

    fn format_struct(&mut self, s: &Struct) {
        self.fmt.start_list();
        self.fmt.write_symbol("def-struct");
        self.format_ident(&s.name.name);
        self.fmt.start_list();
        for type_param in &s.name.type_params {
            self.format_ident(type_param);
        }
        self.fmt.end_list();
        self.format_var_list(&s.fields, true);
        self.fmt.end_list();
    }

    fn format_fn(&mut self, f: &Function) {
        self.fmt.start_list();
        match f.fn_type {
            FunctionType::Extern => {
                self.fmt.write_symbol("def-extern-fn");
            }
            FunctionType::Normal => {
                self.fmt.write_symbol("def-fn");
            }
        }
        self.format_ident(&f.name.name);
        self.fmt.start_list();
        for type_param in &f.name.type_params {
            self.format_ident(type_param);
        }
        self.fmt.end_list();
        self.format_var_list(&f.params, false);
        self.format_type(&f.return_type);
        if let Some(ref body) = f.body {
            self.format_statement(body);
        }
        self.fmt.end_list();
    }

    fn format_ident(&mut self, i: &Ident) {
        self.fmt.write_symbol(&i.0);
    }

    fn format_var_list(&mut self, vars: &[Var], multiline: bool) {
        self.fmt.start_list();
        if multiline {
            self.fmt.make_list_multiline();
        }
        for v in vars {
            self.fmt.start_list();
            self.format_ident(&v.name);
            self.format_type(&v.typ);
            self.fmt.end_list();
        }
        self.fmt.end_list();
    }

    fn format_type(&mut self, t: &Type) {
        match *t {
            Type::Bool => self.fmt.write_symbol("bool"),
            Type::I8 => self.fmt.write_symbol("i8"),
            Type::U8 => self.fmt.write_symbol("u8"),
            Type::I16 => self.fmt.write_symbol("i16"),
            Type::U16 => self.fmt.write_symbol("u16"),
            Type::I32 => self.fmt.write_symbol("i32"),
            Type::U32 => self.fmt.write_symbol("u32"),
            Type::Wildcard => self.fmt.write_symbol("_"),
            Type::Pointer(ref typ) => {
                self.fmt.start_list();
                self.fmt.write_symbol("ptr");
                self.format_type(typ);
                self.fmt.end_list();
            }
            Type::Function(ref params, ref output) => {
                self.fmt.start_list();
                self.fmt.write_symbol("fn");
                self.fmt.start_list();
                for typ in params {
                    self.format_type(typ);
                }
                self.fmt.end_list();
                self.format_type(output);
                self.fmt.end_list();
            }
            Type::Concrete(ref name, ref params) => {
                self.fmt.start_list();
                self.fmt.write_symbol("named");
                self.format_ident(name);
                self.fmt.start_list();
                for param in params {
                    self.format_type(param);
                }
                self.fmt.end_list();
                self.fmt.end_list();
            }
        }
    }

    fn format_statement(&mut self, s: &Statement) {
        match *s {
            Statement::Block(ref statements) => {
                self.fmt.start_list();
                self.fmt.write_symbol("block");
                self.fmt.make_list_multiline();
                for s in statements {
                    self.format_statement(s);
                }
                self.fmt.end_list();
            }
            Statement::Break => {
                self.fmt.write_symbol("break");
            }
            Statement::Continue => {
                self.fmt.write_symbol("continue");
            }
            Statement::Expr(ref expr) => {
                self.fmt.start_list();
                self.fmt.write_symbol("expr");
                self.format_expr(expr);
                self.fmt.end_list();
            }
            Statement::If(ref cond, ref then, ref else_) => {
                self.fmt.start_list();
                if let Some(ref else_) = *else_ {
                    self.fmt.write_symbol("if-then-else");
                    self.fmt.make_list_multiline();
                    self.format_expr(cond);
                    self.format_statement(then);
                    self.format_statement(else_);
                } else {
                    self.fmt.write_symbol("if-then");
                    self.fmt.make_list_multiline();
                    self.format_expr(cond);
                    self.format_statement(then);
                }
                self.fmt.end_list();
            }
            Statement::Let(ref name, ref typ, ref value) => {
                self.fmt.start_list();
                if let Some(ref typ) = *typ {
                    self.fmt.write_symbol("let-typed");
                    self.format_ident(name);
                    self.format_type(typ);
                    self.format_expr(value);
                } else {
                    self.fmt.write_symbol("let");
                    self.format_ident(name);
                    self.format_expr(value);
                }
                self.fmt.end_list();
            }
            Statement::Loop(ref body) => {
                self.fmt.start_list();
                self.fmt.write_symbol("loop");
                self.format_statement(body);
                self.fmt.end_list();
            }
            Statement::Return(ref value) => {
                self.fmt.start_list();
                self.fmt.write_symbol("return");
                self.format_expr(value);
                self.fmt.end_list();
            }
            Statement::While(ref cond, ref body) => {
                self.fmt.start_list();
                self.fmt.write_symbol("while");
                self.fmt.make_list_multiline();
                self.format_expr(cond);
                self.format_statement(body);
                self.fmt.end_list();
            }
            Statement::Error => {
                self.fmt.write_symbol("error");
            }
        }
    }

    fn format_expr(&mut self, e: &Expr) {
        match *e {
            Expr::Binary(ref lhs, ref op, ref rhs) => {
                self.fmt.start_list();
                self.fmt.write_symbol("binary-op");
                self.format_binary_op(op);
                self.format_expr(lhs);
                self.format_expr(rhs);
                self.fmt.end_list();
            }
            Expr::Unary(ref op, ref value) => {
                self.fmt.start_list();
                self.fmt.write_symbol("unary-op");
                self.format_unary_op(op);
                self.format_expr(value);
                self.fmt.end_list();
            }
            Expr::Call(ref value, ref params) => {
                self.fmt.start_list();
                self.fmt.write_symbol("call");
                self.format_expr(value);
                self.fmt.start_list();
                for param in params {
                    self.format_call_param(param);
                }
                self.fmt.end_list();
                self.fmt.end_list();
            }
            Expr::Field(ref value, ref field) => {
                self.fmt.start_list();
                self.fmt.write_symbol("field");
                self.format_expr(value);
                self.format_ident(field);
                self.fmt.end_list();
            }
            Expr::Literal(ref lit) => {
                self.format_literal(lit);
            }
            Expr::Name(ref name, ref type_params) => {
                self.fmt.start_list();
                self.fmt.write_symbol("name");
                self.format_ident(name);
                self.fmt.start_list();
                for param in type_params {
                    self.format_type(param);
                }
                self.fmt.end_list();
                self.fmt.end_list();
            }
        }
    }

    fn format_literal(&mut self, l: &Literal) {
        match *l {
            Literal::Bool(true) => {
                self.fmt.write_symbol("true");
            }
            Literal::Bool(false) => {
                self.fmt.write_symbol("false");
            }
            Literal::Char(value) => {
                self.fmt.write_symbol(&format!("'\\x{:X}'", value));
            }
            Literal::Number(num) => {
                let suffix = match (num.signedness, num.size) {
                    (None, None) => "",
                    (Some(Signedness::Signed), None) => "i",
                    (Some(Signedness::Unsigned), None) => "u",
                    (Some(Signedness::Signed), Some(Size::Bit8)) => "i8",
                    (Some(Signedness::Signed), Some(Size::Bit16)) => "i16",
                    (Some(Signedness::Signed), Some(Size::Bit32)) => "i32",
                    (Some(Signedness::Unsigned), Some(Size::Bit8)) => "u8",
                    (Some(Signedness::Unsigned), Some(Size::Bit16)) => "u16",
                    (Some(Signedness::Unsigned), Some(Size::Bit32)) => "u32",
                    _ => panic!("bad number literal"),
                };
                self.fmt.write_symbol(&format!("{}{}", num.value, suffix));
            }
            Literal::Str(ref values) => {
                self.fmt.start_list();
                self.fmt.write_symbol("string");
                for value in values {
                    self.fmt.write_symbol(&format!("{}", value));
                }
                self.fmt.end_list();
            }
        }
    }

    fn format_call_param(&mut self, p: &CallParam) {
        match *p {
            CallParam::Named(ref name, ref value) => {
                self.fmt.start_list();
                self.fmt.write_symbol("named");
                self.format_ident(name);
                self.format_expr(value);
                self.fmt.end_list();
            }
            CallParam::Unnamed(ref value) => {
                self.fmt.start_list();
                self.fmt.write_symbol("unnamed");
                self.format_expr(value);
                self.fmt.end_list();
            }
        }
    }

    fn format_binary_op(&mut self, op: &BinaryOp) {
        let sym = match *op {
            BinaryOp::Add => "add",
            BinaryOp::And => "and",
            BinaryOp::Assign => "assign",
            BinaryOp::Divide => "divide",
            BinaryOp::Equal => "equal?",
            BinaryOp::Greater => "greater?",
            BinaryOp::GreaterEqual => "greater-or-equal?",
            BinaryOp::Less => "less?",
            BinaryOp::LessEqual => "less-or-equal?",
            BinaryOp::Modulo => "modulo",
            BinaryOp::Multiply => "multiply",
            BinaryOp::NotEqual => "not-equal?",
            BinaryOp::Or => "or",
            BinaryOp::Subtract => "subtract",
        };
        self.fmt.write_symbol(sym);
    }

    fn format_unary_op(&mut self, op: &UnaryOp) {
        let sym = match *op {
            UnaryOp::AddressOf => "take-address",
            UnaryOp::Deref => "deref",
            UnaryOp::Minus => "minus",
            UnaryOp::Not => "not",
            UnaryOp::Plus => "plus",
        };
        self.fmt.write_symbol(sym);
    }

    fn into_string(self) -> String {
        self.fmt.into_string()
    }
}

pub fn format_program(program: &Program) -> String {
    let mut fmt = Formatter::new();
    fmt.format_program(program);
    fmt.into_string()
}
