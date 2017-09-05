#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
pub struct Position {
    pub line: u32,
    pub column: u32,
}

impl Position {
    pub fn new(line: u32, column: u32) -> Position {
        Position {
            line,
            column,
        }
    }

    pub fn span_to(self, to: Position) -> Span {
        Span::new(self, to)
    }

    pub fn forward(mut self, amount: u32) -> Position {
        self.column += amount;
        self
    }

    pub fn backwards(mut self, amount: u32) -> Position {
        assert!(self.column > amount, "going back too far");
        self.column -= amount;
        self
    }
}

#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Span {
        debug_assert!(start < end);
        Span {
            start,
            end,
        }
    }

    pub fn merge(self, other: Span) -> Span {
        use std::cmp;
        let start = cmp::min(self.start, other.start);
        let end = cmp::max(self.end, other.end);
        Span::new(start, end)
    }
}
