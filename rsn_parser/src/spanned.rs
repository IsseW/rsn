use std::{
    borrow::Borrow,
    fmt::{Debug, Display},
    hash::Hash,
    ops::{Deref, DerefMut},
};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Position {
    pub byte_start: usize,
    pub byte_end: usize,
    pub line: usize,
    pub column: usize,
}

impl Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl Position {
    pub fn start() -> Position {
        Self {
            byte_start: 0,
            byte_end: 0,
            line: 0,
            column: 0,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

impl Span {
    pub fn new(start: Position, end: Position) -> Self {
        Self { start, end }
    }

    pub fn empty() -> Self {
        Self::new(Position::start(), Position::start())
    }

    pub fn byte_range(&self) -> std::ops::Range<usize> {
        self.start.byte_start..self.end.byte_end
    }
}

#[derive(Clone)]
pub struct Spanned<T> {
    pub span: Span,
    pub value: T,
}

impl<T: PartialEq> PartialEq for Spanned<T> {
    fn eq(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }
}

impl<T: Eq> Eq for Spanned<T> {}

impl<T: Hash> Hash for Spanned<T> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.value.hash(state);
    }
}

impl Borrow<str> for Spanned<&str> {
    fn borrow(&self) -> &str {
        self.value
    }
}

impl<T> Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.value
    }
}

impl<T> DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.value
    }
}

impl<T> Spanned<T> {
    pub fn new(span: Span, value: T) -> Self {
        Self { span, value }
    }
    pub fn create(value: T) -> Self {
        Self {
            span: Span {
                start: Position::start(),
                end: Position::start(),
            },
            value,
        }
    }
    pub fn start(&self) -> Position {
        self.span.start
    }
    pub fn end(&self) -> Position {
        self.span.end
    }

    pub fn map<U>(self, mut map: impl FnMut(T) -> U) -> Spanned<U> {
        Spanned::new(self.span, map(self.value))
    }

    pub fn inner(self) -> T {
        self.value
    }

    pub fn display_in_src(&self, src: &str) -> String {
        let mut display = String::new();
        for (i, line) in src.lines().enumerate().filter(|(i, _)| {
            (self.start().line.saturating_sub(1)..=self.end().line.saturating_add(1)).contains(i)
        }) {
            display.push_str(line);
            display.push('\n');
            if ((self.start().line + 1)..self.end().line).contains(&i) {
                line.chars().map(|_| '^').collect_into(&mut display);
                display.push('\n');
            } else if i == self.start().line && i == self.end().line {
                line.chars()
                    .enumerate()
                    .map(|(i, _)| {
                        if (self.start().column..=self.end().column).contains(&(i + 1)) {
                            '^'
                        } else {
                            ' '
                        }
                    })
                    .collect_into(&mut display);
                display.push('\n');
            } else if i == self.start().line {
                line.chars()
                    .enumerate()
                    .map(|(i, _)| if self.start().column <= i { '^' } else { ' ' })
                    .collect_into(&mut display);
                display.push('\n');
            } else if i == self.end().line {
                line.chars()
                    .enumerate()
                    .map(|(i, _)| if self.end().column >= i { '^' } else { ' ' })
                    .collect_into(&mut display);
                display.push('\n');
            }
        }
        display
    }
}

impl<T: Debug> Debug for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "From: {}, To: {} ", self.span.start, self.span.end)?;
        self.value.fmt(f)
    }
}

impl<T: Display> Display for Spanned<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "From: {}, To: {} ", self.span.start, self.span.end)?;
        self.value.fmt(f)
    }
}
