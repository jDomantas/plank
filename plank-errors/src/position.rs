//! Types to represent positions inside source file.

/// Represents a position inside a source file. Both lines and columns
/// are 0-indexed.
#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
pub struct Position {
    #[allow(missing_docs)]
    pub line: u32,
    #[allow(missing_docs)]
    pub column: u32,
}

impl Position {
    /// Create a new position with given line and column.
    pub fn new(line: u32, column: u32) -> Position {
        Position {
            line,
            column,
        }
    }

    /// Create a span that starts here, and goes to given position. If
    /// `self == to`, then the span is considered empty.
    ///
    /// # Panics
    ///
    /// Panics if `to < self`.
    pub fn span_to(self, to: Position) -> Span {
        Span::new(self, to)
    }

    /// Create a new position that is `amount` columns to the right.
    pub fn forward(mut self, amount: u32) -> Position {
        self.column += amount;
        self
    }

    /// Create a new position that is `amount` columns to the left.
    ///
    /// # Panics
    ///
    /// Panics if resulting column is negative.
    pub fn backwards(mut self, amount: u32) -> Position {
        assert!(self.column >= amount, "going back too far");
        self.column -= amount;
        self
    }
}

/// Represents a range inside source file. You can think of it as a selection
/// inside the editor.
#[derive(PartialEq, PartialOrd, Eq, Ord, Hash, Debug, Copy, Clone)]
pub struct Span {
    #[allow(missing_docs)]
    pub start: Position,
    #[allow(missing_docs)]
    pub end: Position,
}

impl Span {
    /// Create a span that is between given positions.
    ///
    /// # Panics
    ///
    /// Panics if `start > end`.
    pub fn new(start: Position, end: Position) -> Span {
        assert!(start <= end);
        Span {
            start,
            end,
        }
    }

    /// Return the smallest span that contains both `self` and `other` spans.
    pub fn merge(self, other: Span) -> Span {
        use std::cmp;
        let start = cmp::min(self.start, other.start);
        let end = cmp::max(self.end, other.end);
        Span::new(start, end)
    }
}
