//! Functions to pretty-print diagnostics.

use std::collections::{BTreeMap, HashSet};
use reporter::{Diagnostic, Note, Severity};


#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
enum MarkerStyle {
    // ~~~~~ or ____~
    Secondary,
    // ^^^^^ or ____^
    Primary,
}

impl MarkerStyle {
    fn from_note_index(index: usize) -> MarkerStyle {
        if index == 0 {
            MarkerStyle::Primary
        } else {
            MarkerStyle::Secondary
        }
    }

    fn to_marker_part(self, is_arrow: bool) -> MarkerPart {
        match self {
            MarkerStyle::Primary => {
                if is_arrow {
                    MarkerPart::ArrowPrimary
                } else {
                    MarkerPart::PrimaryMarker
                }
            }
            MarkerStyle::Secondary => {
                if is_arrow {
                    MarkerPart::ArrowSecondary
                } else {
                    MarkerPart::SecondaryMarker
                }
            }
        }
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
enum MarkerPart {
    None,
    ArrowBottom,
    SecondaryMarker,
    PrimaryMarker,
    ArrowSecondary,
    ArrowPrimary,
    SingleColumn
}

impl MarkerPart {
    fn or(self, other: MarkerPart) -> MarkerPart {
        ::std::cmp::max(self, other)
    }

    fn to_char(self) -> char {
        match self {
            MarkerPart::None => ' ',
            MarkerPart::ArrowBottom => '_',
            MarkerPart::SecondaryMarker |
            MarkerPart::ArrowSecondary => '~',
            MarkerPart::PrimaryMarker |
            MarkerPart::ArrowPrimary => '^',
            MarkerPart::SingleColumn => '|',
        }
    }
}

#[derive(Debug, Clone)]
enum LineMarker<'a> {
    FromStart {
        connect_col: u32,
        arrow_col: u32,
        message: Option<&'a str>,
        style: MarkerStyle,
    },
    FromTo {
        start_col: u32,
        end_col: u32,
        message: Option<&'a str>,
        style: MarkerStyle,
    },
}

impl<'a> LineMarker<'a> {
    fn message(&self) -> Option<&str> {
        match *self {
            LineMarker::FromStart { message, .. } |
            LineMarker::FromTo { message, .. } => message,
        }
    }

    fn end_col(&self) -> u32 {
        match *self {
            LineMarker::FromStart { arrow_col, .. } => arrow_col + 1,
            LineMarker::FromTo { end_col, .. } => end_col,
        }
    }

    fn is_arrow(&self) -> bool {
        match *self {
            LineMarker::FromStart { .. } => true,
            LineMarker::FromTo { .. } => false,
        }
    }
}

struct Printer<'a> {
    lines: Vec<&'a str>,
    // use BTreeMap here so that we can iterate annotated lines in order
    line_markers: BTreeMap<u32, Vec<LineMarker<'a>>>,
    next_connect_col: u32,
    full_connection_cols: HashSet<u32>,
    number_space: usize,
}

impl<'a> Printer<'a> {
    fn new(source: &'a str) -> Self {
        Printer {
            lines: source.lines().map(str::trim_right).collect(),
            line_markers: BTreeMap::new(),
            next_connect_col: 0,
            full_connection_cols: HashSet::new(),
            number_space: 0,
        }
    }

    fn pretty_print(&mut self, diagnostic: &'a Diagnostic) {
        match diagnostic.severity {
            Severity::Error => {
                println!("error: {}", diagnostic.message);
            }
            Severity::Warning => {
                println!("warning: {}", diagnostic.message);
            }
        }
        if !diagnostic.notes.is_empty() {
            self.print_notes(&diagnostic.notes);
        }
    }

    fn print_notes(&mut self, notes: &'a [Note]) {
        assert!(self.line_markers.is_empty());
        assert_eq!(self.next_connect_col, 0);
        assert!(self.full_connection_cols.is_empty());
        for (index, note) in notes.iter().enumerate() {
            self.add_note_markers(note, MarkerStyle::from_note_index(index));
        }
        for markers in self.line_markers.values_mut() {
            markers.sort_by_key(LineMarker::end_col);
        }
        assert!(!self.line_markers.is_empty());
        let last_line = *self.line_markers.keys().next_back().unwrap();
        self.number_space = number_length(last_line) as usize;
        self.print_annotations();
        self.line_markers.clear();
        assert!(self.full_connection_cols.is_empty());
        self.next_connect_col = 0;
    }

    fn add_note_markers(&mut self, note: &'a Note, style: MarkerStyle) {
        if note.span.start.line == note.span.end.line {
            self.add_line_marker(note.span.start.line, LineMarker::FromTo {
                start_col: note.span.start.column,
                end_col: note.span.end.column,
                message: note.message.as_ref().map(AsRef::as_ref),
                style,
            });
        } else {
            let connect_col = self.next_connect_col;
            self.next_connect_col += 2;
            self.add_line_marker(note.span.start.line, LineMarker::FromStart {
                connect_col,
                arrow_col: note.span.start.column,
                message: None,
                style,
            });
            self.add_line_marker(note.span.end.line, LineMarker::FromStart {
                connect_col,
                arrow_col: note.span.end.column - 1,
                message: note.message.as_ref().map(AsRef::as_ref),
                style,
            });
        }
    }

    fn add_line_marker(&mut self, line: u32, marker: LineMarker<'a>) {
        self.line_markers
            .entry(line)
            .or_insert_with(Vec::new)
            .push(marker);
    }

    fn print_annotations(&mut self) {
        let line_markers = ::std::mem::replace(&mut self.line_markers, BTreeMap::new());
        let mut last_printed = None;
        for (line, markers) in line_markers {
            if let Some(prev) = last_printed {
                if !self.full_connection_cols.is_empty() {
                    if line - prev > 3 {
                        self.print_line(prev + 1);
                        self.print_gap_line();
                        self.print_line(line - 1);
                    } else {
                        for line in (prev + 1)..line {
                            self.print_line(line);
                        }
                    }
                } else if line - prev > 1 {
                    self.print_gap_line();
                }
            }
            last_printed = Some(line);
            self.print_line_and_markers(line, markers);
        }
    }

    fn print_line_and_markers(&mut self, line: u32, mut markers: Vec<LineMarker>) {
        assert!(!markers.is_empty(), "line has no markers");
        self.print_line(line);
        self.print_immediate_markers(&markers);
        // last span has its message printed inline
        println!(" {}", markers
            .iter()
            .next_back()
            .unwrap()
            .message()
            .unwrap_or(""));
        markers.pop();
        
        // non-arrow markers without messages don't extend below first line
        markers.retain(|m| m.message().is_some() || m.is_arrow());
        
        // possibly print additional line so that instead of
        //      let x = 1;
        //          ~~~  ~ first note
        //            second note
        // we would get
        //      let x = 1;
        //          ~~~  ~ first note
        //            |
        //            second note
        match markers.iter().next_back().cloned() {
            Some(LineMarker::FromTo { end_col, .. }) => {
                self.print_markers(&markers, end_col);
                println!();
            }
            Some(LineMarker::FromStart { arrow_col, message, .. })
                if message.is_some() =>
            {
                self.print_markers(&markers, arrow_col + 1);
                println!();
            }
            _ => {}
        }
        for i in (1..(markers.len() + 1)).rev() {
            let markers = &markers[0..i];
            match markers[i - 1] {
                LineMarker::FromTo { end_col, message, .. } => {
                    self.print_markers(markers, end_col - 1);
                    println!("{}", message.unwrap_or(""));
                }
                LineMarker::FromStart { arrow_col, message, .. } => {
                    self.print_markers(markers, arrow_col + 2);
                    println!("{}", message.unwrap_or(""));
                }
            }
        }
    }

    fn print_line(&mut self, line: u32) {
        self.print_line_header(Some(line), None);
        for ch in self.lines.get((line - 1) as usize).unwrap_or(&"").chars() {
            match ch {
                // print these chars as one space, otherwise
                // they will break note alignment with code
                '\t' | '\r' => print!(" "),
                ch => print!("{}", ch),
            }
        }
        println!();
    }

    fn print_line_header(&mut self, line: Option<u32>, arrow_from: Option<u32>) {
        if let Some(line) = line {
            print!("{: >width$} |  ", line, width = self.number_space);
        } else {
            print!("{: >width$} |  ", "", width = self.number_space);
        }
        for col in 0..self.next_connect_col {
            if self.full_connection_cols.contains(&col) {
                print!("|");
            } else {
                match arrow_from {
                    Some(c) if c < col => print!("_"),
                    _ => print!(" "),
                }
            }
            if arrow_from == Some(col) {
                if self.full_connection_cols.contains(&col) {
                    self.full_connection_cols.remove(&col);
                } else {
                    self.full_connection_cols.insert(col);
                }
            }
        }
    }

    fn print_gap_line(&mut self) {
        print!("{: <width$}", "...", width = self.number_space + 4);
        for col in 0..self.next_connect_col {
            if self.full_connection_cols.contains(&col) {
                print!("|");
            } else {
                print!(" ");
            }
        }
        println!();
    }

    fn print_immediate_markers(&mut self, markers: &[LineMarker]) {
        let (connect, arrow_end) = match *markers.iter().next_back().unwrap() {
            LineMarker::FromStart { connect_col, arrow_col, .. } => {
                (Some(connect_col), arrow_col.saturating_sub(1))
            }
            _ => {
                (None, 0)
            }
        };
        self.print_line_header(None, connect);
        let last_col = markers.iter().next_back().unwrap().end_col();
        for col in 1..last_col {
            let mut part = if col <= arrow_end {
                MarkerPart::ArrowBottom
            } else {
                MarkerPart::None
            };
            for (index, marker) in markers.iter().rev().enumerate() {
                match *marker {
                    LineMarker::FromStart { arrow_col, style, .. } => {
                        if arrow_col == col {
                            part = part.or(style.to_marker_part(true));
                        }
                    }
                    LineMarker::FromTo { start_col, end_col, style, message } => {
                        if start_col <= col && col < end_col {
                            part = part.or(style.to_marker_part(false));
                        }
                        if start_col == col
                            && end_col == start_col + 1 // marker is one col wide
                            && index > 0 // and not the last one in line
                            && message.is_some() // and has a message
                            && style != MarkerStyle::Primary // and it is not primary
                        {
                            part = part.or(MarkerPart::SingleColumn);
                        }
                    }
                }
            }
            print!("{}", part.to_char());
        }
    }

    fn print_markers(&mut self, markers: &[LineMarker], last_col: u32) {
        let (connect, arrow_end) = match *markers.iter().next_back().unwrap() {
            LineMarker::FromStart { connect_col, arrow_col, .. } => {
                (Some(connect_col), arrow_col - 1)
            }
            _ => {
                (None, 0)
            }
        };
        self.print_line_header(None, connect);
        for col in 1..last_col {
            let mut part = if col <= arrow_end {
                MarkerPart::ArrowBottom
            } else {
                MarkerPart::None
            };
            for marker in markers.iter().rev() {
                match *marker {
                    LineMarker::FromStart { arrow_col, .. } => {
                        if arrow_col == col {
                            part = MarkerPart::SingleColumn;
                        }
                    }
                    LineMarker::FromTo { end_col, message, .. } => {
                        if end_col == col + 1 && message.is_some() {
                            part = MarkerPart::SingleColumn;
                        }
                    }
                }
            }
            print!("{}", part.to_char());
        }
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

/// Print a single diagnostic to stdout.
pub fn print_diagnostic(source: &str, diagnostic: &Diagnostic) {
    Printer::new(source).pretty_print(diagnostic)
}

/// Print all diagnostics to stdout.
///
/// Diagnostics will be printed in the given order.
pub fn print_diagnostics(source: &str, diagnostics: &[Diagnostic]) {
    let mut printer = Printer::new(source);
    for diagnostic in diagnostics {
        printer.pretty_print(diagnostic);
        println!("");
    }
}
