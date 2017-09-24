use std::cell::RefCell;
use std::marker::PhantomData;
use std::rc::Rc;
use position::Span;
use typenum::{Nat, Z, S};


#[derive(Default, Debug, Clone)]
pub struct Reporter {
    diagnostics: Rc<RefCell<Vec<Diagnostic>>>,
}

impl Reporter {
    pub fn new() -> Reporter {
        Default::default()
    }

    pub fn have_errors(&self) -> bool {
        self.diagnostics.borrow().iter().any(|d| {
            d.severity == Severity::Error
        })
    }

    pub fn get_diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.borrow().clone()
    }

    pub fn global_error<T: Into<String>>(&mut self, msg: T) {
        let diagnostic = Diagnostic {
            message: msg.into(),
            primary_span: None,
            notes: Vec::new(),
            severity: Severity::Error,
        };
        self.diagnostics.borrow_mut().push(diagnostic);
    }

    pub fn error<T>(&mut self, msg: T, span: Span) -> Builder<S<S<Z>>>
        where T: Into<String>
    {
        self.diagnostic(Severity::Error, msg, span)
    }

    pub fn warning<T>(&mut self, msg: T, span: Span) -> Builder<S<S<Z>>>
        where T: Into<String>
    {
        self.diagnostic(Severity::Warning, msg, span)
    }

    pub fn diagnostic<T>(&mut self, severity: Severity, msg: T, span: Span) -> Builder<S<S<Z>>>
        where T: Into<String>
    {
        Builder::new(self, severity, msg.into(), span)
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
pub enum Severity {
    Error,
    Warning,
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    pub primary_span: Option<Span>,
    pub severity: Severity,
    pub notes: Vec<Note>,
}

#[derive(Debug, Clone)]
pub struct Note {
    pub span: Span,
    pub message: Option<String>,
}

#[must_use]
pub struct Builder<N> {
    phantom: PhantomData<N>,
    reporter: Reporter,
    diagnostic: Diagnostic,
}

impl<N: Nat> Builder<N> {
    fn new(reporter: &Reporter, severity: Severity, msg: String, primary_span: Span) -> Self {
        Builder {
            phantom: PhantomData,
            reporter: reporter.clone(),
            diagnostic: Diagnostic {
                message: msg,
                severity,
                primary_span: Some(primary_span),
                notes: Vec::new(),
            },
        }
    }

    pub fn build(self) {
        assert!(!self.diagnostic.notes.is_empty(), "built a diagnostic without any notes");
        self.reporter.diagnostics.borrow_mut().push(self.diagnostic);
    }
}

impl<N: Nat> Builder<S<N>> {
    pub fn span(self, span: Span) -> Builder<N> {
        self.note(span, None)
    }

    pub fn span_note<T>(self, span: Span, msg: T) -> Builder<N>
        where T: Into<String>
    {
        self.note(span, Some(msg.into()))
    }

    fn note(mut self, span: Span, msg: Option<String>) -> Builder<N> {
        self.diagnostic.notes.push(Note {
            span,
            message: msg,
        });
        Builder {
            phantom: PhantomData,
            reporter: self.reporter,
            diagnostic: self.diagnostic,
        }
    }
}
