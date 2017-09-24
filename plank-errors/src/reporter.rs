//! Helpers to build and aggregate diagnostics.

use std::cell::RefCell;
use std::rc::Rc;
use position::Span;


/// Reporter aggregates and allows building diagnostics.
///
/// Note that reporters created by cloning will share diagnostic list with the
/// original reporter.
#[derive(Default, Debug, Clone)]
pub struct Reporter {
    diagnostics: Rc<RefCell<Vec<Diagnostic>>>,
}

impl Reporter {
    /// Create a new reporter with no errors.
    pub fn new() -> Reporter {
        Default::default()
    }

    /// Returns if the reporter has any errors.
    ///
    /// This function will return false even if reporter has any warnings.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use plank_errors::reporter::Reporter;
    ///
    /// let mut reporter = Reporter::new();
    /// // empty reporter should not have any errors
    /// assert!(!reporter.has_errors());
    /// ```
    pub fn has_errors(&self) -> bool {
        self.diagnostics.borrow().iter().any(|d| {
            d.severity == Severity::Error
        })
    }

    /// Return the list of diagnostics collected with this reporter.
    ///
    /// The diagnosics are returned in arbitrary order. Depending on how they
    /// will be displayed, you might want to sort them.
    pub fn get_diagnostics(&self) -> Vec<Diagnostic> {
        self.diagnostics.borrow().clone()
    }

    /// Create a new error without associated span.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use plank_errors::reporter::Reporter;
    ///
    /// let mut reporter = Reporter::new();
    /// reporter.global_error("`main` function is missing");
    /// ```
    pub fn global_error<T: Into<String>>(&self, msg: T) {
        let diagnostic = Diagnostic {
            message: msg.into(),
            primary_span: None,
            notes: Vec::new(),
            severity: Severity::Error,
        };
        self.diagnostics.borrow_mut().push(diagnostic);
    }

    /// Create a builder for a new error.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use plank_errors::reporter::Reporter;
    /// use plank_errors::position::{Position, Span};
    ///
    /// let mut reporter = Reporter::new();
    /// # let error_span = Span::new(Position::new(1, 1), Position::new(1, 1));
    /// # let help_span = error_span;
    /// reporter
    ///     .error("error message", error_span)
    ///     .span_note("shorter message", error_span)
    ///     .span_note("helper note", help_span)
    ///     .build();
    /// ```
    pub fn error<T>(&self, msg: T, span: Span) -> Builder
        where T: Into<String>
    {
        self.diagnostic(Severity::Error, msg, span)
    }

    /// Create a builder for a new warning.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use plank_errors::reporter::Reporter;
    /// use plank_errors::position::{Position, Span};
    ///
    /// let mut reporter = Reporter::new();
    /// # let warning_span = Span::new(Position::new(1, 1), Position::new(1, 1));
    /// # let help_span = warning_span;
    /// reporter
    ///     .warning("warning message", warning_span)
    ///     .span_note("shorter message", warning_span)
    ///     .span_note("helper note", help_span)
    ///     .build();
    /// ```
    pub fn warning<T>(&self, msg: T, span: Span) -> Builder
        where T: Into<String>
    {
        self.diagnostic(Severity::Warning, msg, span)
    }

    /// Create a builder for a new diagnostic.
    ///
    /// # Examples
    ///
    /// ```rust
    /// use plank_errors::reporter::{Reporter, Severity};
    /// use plank_errors::position::{Position, Span};
    /// let mut reporter = Reporter::new();
    /// # let error_span = Span::new(Position::new(1, 1), Position::new(1, 1));
    /// reporter
    ///     .diagnostic(Severity::Error, "error message", error_span)
    ///     .span(error_span)
    ///     .build();
    /// ```
    pub fn diagnostic<T>(&self, severity: Severity, msg: T, span: Span) -> Builder
        where T: Into<String>
    {
        Builder::new(self, severity, msg.into(), span)
    }
}

/// Diagnostics severity.
#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
pub enum Severity {
    /// Represents a fatal error.
    Error,
    /// Represents a non-fatal error.
    Warning,
}

#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub message: String,
    pub primary_span: Option<Span>,
    pub severity: Severity,
    pub notes: Vec<Note>,
}

#[allow(missing_docs)]
#[derive(Debug, Clone)]
pub struct Note {
    pub span: Span,
    pub message: Option<String>,
}

/// A helper for building a diagnostic.
#[must_use]
pub struct Builder {
    reporter: Reporter,
    diagnostic: Diagnostic,
}

impl Builder {
    fn new(reporter: &Reporter, severity: Severity, msg: String, primary_span: Span) -> Self {
        Builder {
            reporter: reporter.clone(),
            diagnostic: Diagnostic {
                message: msg,
                severity,
                primary_span: Some(primary_span),
                notes: Vec::new(),
            },
        }
    }

    /// Complete current diagnostic.
    ///
    /// # Panics
    ///
    /// Panics if current diagnostic has no notes. If you want an error without
    /// any notes, use [`Reporter::global_error`]
    /// (struct.Reporter.html#method.global_error) instead.
    pub fn build(self) {
        assert!(!self.diagnostic.notes.is_empty(), "built a diagnostic without any notes");
        self.reporter.diagnostics.borrow_mut().push(self.diagnostic);
    }

    /// Add a new note that has only a span.
    pub fn span(self, span: Span) -> Self {
        self.note(None, span)
    }

    /// Add a new note that has a message and a span.
    pub fn span_note<T>(self, msg: T, span: Span) -> Self
        where T: Into<String>
    {
        self.note(Some(msg.into()), span)
    }

    fn note(mut self, msg: Option<String>, span: Span) -> Self {
        self.diagnostic.notes.push(Note {
            span,
            message: msg,
        });
        self
    }
}
