use ast::resolved::{Function, Program, Struct, Type};
use plank_errors::reporter::Builder;
use plank_syntax::position::{Span, Spanned};
use CompileCtx;

pub(crate) fn check_for_wildcards(program: &Program, ctx: &mut CompileCtx) {
    let mut ctx = Context::new(ctx);
    ctx.check_program(program);
}

struct Context<'a> {
    ctx: &'a mut CompileCtx,
    builder: Option<Builder>,
    item_kind: Option<&'static str>,
}

impl<'a> Context<'a> {
    fn new(ctx: &'a mut CompileCtx) -> Self {
        Context {
            ctx,
            builder: None,
            item_kind: None,
        }
    }

    fn check_program(&mut self, program: &Program) {
        for fn_ in &program.functions {
            self.check_function(fn_);
        }

        for struct_ in program.structs.values() {
            self.check_struct(struct_);
        }
    }

    fn check_struct(&mut self, struct_: &Struct) {
        self.item_kind = Some("struct field");
        for field in &struct_.fields {
            self.check_type(&field.typ);
        }
        self.finish_item_check();
    }

    fn check_function(&mut self, fn_: &Function) {
        self.item_kind = Some("function header");
        for param in &fn_.params {
            self.check_type(&param.typ);
        }
        self.check_type(&fn_.return_type);
        self.finish_item_check();
    }

    fn check_type(&mut self, typ: &Spanned<Type>) {
        match **typ {
            Type::Unit
            | Type::Bool
            | Type::Error
            | Type::I8
            | Type::I16
            | Type::I32
            | Type::U8
            | Type::U16
            | Type::U32 => {}
            Type::Pointer(_, ref typ) => self.check_type(typ),
            Type::Function(ref params, ref out) => {
                for param in params {
                    self.check_type(param);
                }
                self.check_type(out);
            }
            Type::Concrete(_, ref params) => {
                for param in params {
                    self.check_type(param);
                }
            }
            Type::Wildcard => self.report_error(Spanned::span(typ)),
        }
    }

    fn report_error(&mut self, span: Span) {
        self.builder = Some(match self.builder.take() {
            Some(builder) => builder.span(span),
            None => {
                let msg = format!(
                    "wildcard types are not allowed in {}s",
                    self.item_kind.unwrap(),
                );
                self.ctx.reporter.error(msg, span).span(span)
            }
        });
    }

    fn finish_item_check(&mut self) {
        self.item_kind = None;
        if let Some(builder) = self.builder.take() {
            builder.build();
        }
    }
}
