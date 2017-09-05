use reporter::{Diagnostic, Note, Severity};

struct Printer<'a> {
    lines: Vec<&'a str>,
}

impl<'a> Printer<'a> {
    fn new(source: &'a str) -> Self {
        Printer {
            lines: source.lines().map(str::trim_right).collect(),
        }
    }

    fn pretty_print(&self, diagnostic: &Diagnostic) {
        match diagnostic.severity {
            Severity::Error => {
                println!("error: {}", diagnostic.message);
            }
            Severity::Warning => {
                println!("warning: {}", diagnostic.message);
            }
        }
        assert!(diagnostic.notes.len() > 0, "got diagnostic without notes");
        self.print_notes(&diagnostic.notes);
    }

    fn print_notes(&self, notes: &[Note]) {
        if notes.iter().any(|n| n.span.start.line != n.span.end.line) {
            panic!("cannot format multi-line notes");
        }
        if notes.len() == 1 {
            self.print_simple_note(&notes[0]);
        } else {
            unimplemented!("can only print single-note diagnostics");
        }
    }

    fn print_simple_note(&self, note: &Note) {
        let pad = number_length(note.span.start.line) as usize;
        self.print_line(note.span.start.line, pad);
        self.print_mark(
            pad,
            note.span.start.column as usize,
            note.span.end.column as usize,
            note.message.as_ref().map(|s| s.as_ref()));
    }

    fn print_context(&self, line_number: Option<u32>, number_space: usize) {
        match line_number {
            Some(number) => {
                print!("  {: <width$} | ", number, width=number_space);
            }
            None => {
                print!("{: <width$}| ", "", width=number_space + 3);
            }
        }
    }

    fn print_line(&self, line: u32, pad: usize) {
        self.print_context(Some(line), pad);
        println!("{}", self.lines[(line - 1) as usize]);
    }

    fn print_mark(&self, pad: usize, start: usize, end: usize, msg: Option<&str>) {
        assert!(start < end);
        self.print_context(None, pad);
        let msg = msg.unwrap_or("");
        println!("{0: <left$}{0:~<width$} {1}",
            "",
            msg,
            left = start - 1,
            width = end - start);
    }
}

fn number_length(mut num: u32) -> u32 {
    let mut len = 1;
    while num >= 10 {
        len += 1;
        num /= 10;
    }
    len
}

pub fn print_diagnostic(source: &str, diagnostic: &Diagnostic) {
    Printer::new(source).pretty_print(diagnostic)
}

pub fn print_diagnostics(source: &str, diagnostics: &[Diagnostic]) {
    let printer = Printer::new(source);
    for diagnostic in diagnostics {
        printer.pretty_print(diagnostic);
        println!("");
    }
}
