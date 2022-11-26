use std::{borrow::Cow, fmt::Display, ops::RangeInclusive, str::CharIndices};

use crate::{Map, Path, Position, Span, Spanned, Value, ValueKind};

#[derive(Clone, Debug)]
pub enum Error {
    UnexpectedSymbol,
    UnexpectedEnd,
    ExpectedEnd,
    Expected(char),
    ExpectedRange(RangeInclusive<char>),
    InclusiveNoEnd,
    InvalidFloat,
    InvalidInteger,
    DuplicateIdent,
    ExpectedFatArrow,
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnexpectedSymbol => write!(f, "Unexpected Symbol"),
            Error::UnexpectedEnd => write!(f, "Unexpected EOF"),
            Error::ExpectedEnd => write!(f, "Expected EOF"),
            Error::Expected(c) => write!(f, "Expected '{c}'"),
            Error::ExpectedRange(c) => write!(f, "Expected '{}'..='{}'", c.start(), c.end()),
            Error::InclusiveNoEnd => write!(f, "Expected end value for inclusive range"),
            Error::InvalidFloat => write!(f, "Invalid float."),
            Error::InvalidInteger => write!(f, "Invalid integer."),
            Error::DuplicateIdent => write!(f, "The same identifier is seen multiple times."),
            Error::ExpectedFatArrow => write!(f, "Expected `=>`."),
        }
    }
}

pub type ParseError = Spanned<Error>;

fn escape(c: char) -> Option<char> {
    Some(match c {
        'n' => '\n',
        'r' => '\r',
        't' => '\t',
        '\'' => '\'',
        '"' => '"',
        '0' => '\0',
        '\\' => '\\',
        _ => return None,
    })
}

#[derive(Clone)]
pub(crate) struct Chars<'a> {
    chars: CharIndices<'a>,
    src: &'a str,
    place: Position,
}

impl<'a> Chars<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            chars: src.char_indices(),
            src,
            place: Position::start(),
        }
    }

    fn spanned<T>(&self, start: Position, value: T) -> Spanned<T> {
        Spanned {
            span: Span::new(start, self.place),
            value,
        }
    }

    fn error_end(&self) -> ParseError {
        self.spanned(self.place, Error::UnexpectedEnd)
    }

    fn error1(&self, error: Error) -> ParseError {
        self.spanned(self.place, error)
    }

    fn error(&self, start: Position, error: Error) -> ParseError {
        self.spanned(start, error)
    }

    fn next_place(&self) -> Position {
        let mut peek = self.clone();
        let _ = peek.next_c();
        peek.place
    }

    fn peek(&mut self, mut f: impl FnMut(&mut Self) -> Result<bool, ParseError>) -> bool {
        let mut peek = self.clone();
        if f(&mut peek).unwrap_or(false) {
            *self = peek;
            true
        } else {
            false
        }
    }

    fn next_nw(&mut self) -> Result<char, ParseError> {
        for (i, c) in &mut self.chars {
            self.place.byte_start = i;
            self.place.byte_end = i + c.len_utf8();
            self.place.column += 1;
            if !c.is_whitespace() {
                return Ok(c);
            } else if c == '\n' {
                self.place.column = 0;
                self.place.line += 1;
            }
        }
        Err(self.error_end())
    }

    fn next_c(&mut self) -> Result<char, ParseError> {
        let res = self.chars.next();
        if let Some((i, c)) = res {
            self.place.byte_start = i;
            self.place.byte_end = i + c.len_utf8();
            self.place.column += 1;
            if c == '\n' {
                self.place.column = 0;
                self.place.line += 1;
            }
        }
        res.map(|(_, c)| c).ok_or_else(|| self.error_end())
    }

    fn next_nw_matches(&mut self, f: impl FnOnce(&char) -> bool) -> Result<char, ParseError> {
        let mut peek = self.clone();
        let res = match peek.next_nw() {
            Ok(c) => {
                if f(&c) {
                    Ok(c)
                } else {
                    Err(peek.error1(Error::UnexpectedSymbol))
                }
            }
            e => e,
        };
        if res.is_ok() {
            *self = peek;
        }
        res
    }

    fn next_c_matches(&mut self, f: impl FnMut(&char) -> bool) -> Result<char, ParseError> {
        let mut peek = self.clone();
        let res = peek
            .next_c()
            .ok()
            .filter(f)
            .ok_or_else(|| peek.error1(Error::UnexpectedSymbol));
        if res.is_ok() {
            *self = peek;
        }
        res
    }

    fn assume_next(&mut self, char: char) -> Result<char, ParseError> {
        self.next_c_matches(|c| *c == char).map_err(|err| {
            err.map(|err| match err {
                Error::UnexpectedSymbol => Error::Expected(char),
                e => e,
            })
        })
    }

    fn assume_next_nw(&mut self, char: char) -> Result<char, ParseError> {
        self.next_nw_matches(|c| *c == char).map_err(|err| {
            err.map(|err| match err {
                Error::UnexpectedSymbol => Error::Expected(char),
                e => e,
            })
        })
    }

    pub fn end<T>(&mut self, value: T) -> Result<T, ParseError> {
        if matches!(
            self.next_nw(),
            Err(ParseError {
                value: Error::UnexpectedEnd,
                ..
            })
        ) {
            Ok(value)
        } else {
            Err(self.error1(Error::ExpectedEnd))
        }
    }

    fn parse_number(&mut self, start: Position, c: char) -> Result<Value<'a>, ParseError> {
        let base = if c == '0' {
            match self.next_c_matches(|c| matches!(c, 'x' | 'X' | 'o' | 'O' | 'b' | 'B')) {
                Ok('x' | 'X') => 16,
                Ok('o' | 'O') => 8,
                Ok('b' | 'B') => 2,
                _ => 10,
            }
        } else {
            10
        };
        let parse_start = if base != 10 { self.next_place() } else { start };
        let mut float = false;
        let mut exp = false;
        let mut can_be_sign = false;

        loop {
            let mut peek = self.clone();
            if peek.assume_next('.').is_ok() {
                if peek.assume_next('.').is_ok() {
                    break;
                } else if !float {
                    float = true;
                } else {
                    break;
                }
            } else {
                let cbs = can_be_sign;
                can_be_sign = false;
                match peek.next_c() {
                    Ok('0'..='9' | '_') => {}
                    Ok('a'..='f' | 'A'..='F') if base == 16 => {}
                    Ok('-') if cbs => {}
                    Ok('e' | 'E') if base == 10 && !exp => {
                        float = true;
                        exp = true;
                        can_be_sign = true;
                    }
                    _ => break,
                }
            }
            *self = peek;
        }

        let number = &self.src[parse_start.byte_start..self.place.byte_end];
        if float {
            let f = number
                .parse()
                .map_err(|_| self.error(start, Error::InvalidFloat))?;

            Ok(self.spanned(start, ValueKind::Float(f)))
        } else {
            let i = i64::from_str_radix(number, base)
                .map_err(|_| self.error(start, Error::InvalidInteger))?;

            Ok(self.spanned(start, ValueKind::Integer(i)))
        }
    }

    fn parse_sequence(&mut self, end: char) -> Result<Vec<Value<'a>>, ParseError> {
        let mut values = Vec::new();
        if self.assume_next_nw(end).is_ok() {
            return Ok(values);
        }
        loop {
            let value = self.parse_value()?;
            values.push(value);

            match (self.assume_next_nw(','), self.assume_next_nw(end)) {
                (_, Ok(_)) => break Ok(values),
                (Ok(_), _) => {}
                (Err(err), Err(_)) => break Err(err),
            }
        }
    }

    fn parse_map(&mut self, start: Position) -> Result<Value<'a>, ParseError> {
        let mut map = Vec::new();

        if self.assume_next_nw('}').is_ok() {
            return Ok(self.spanned(start, ValueKind::Map(map)));
        }
        loop {
            let key = self.parse_value()?;

            self.assume_next_nw('=')
                .map_err(|err| err.map(|_| Error::ExpectedFatArrow))?;
            self.assume_next('>')
                .map_err(|err| err.map(|_| Error::ExpectedFatArrow))?;

            let value = self.parse_value()?;

            map.push((key, value));

            match (self.assume_next_nw(','), self.assume_next_nw('}')) {
                (_, Ok(_)) => break Ok(self.spanned(start, ValueKind::Map(map))),
                (Ok(_), _) => {}
                (Err(err), Err(_)) => break Err(err),
            }
        }
    }

    fn parse_ident(&mut self, c: char, start: Position) -> Result<Spanned<&'a str>, ParseError> {
        if !(c.is_alphabetic() || c == '_') {
            panic!()
        }
        if c == 'r' {
            let _ = self.assume_next('#');
        }
        while self
            .next_c_matches(|c| c.is_alphanumeric() || *c == '_')
            .is_ok()
        {}

        Ok(self.spanned(start, &self.src[start.byte_start..self.place.byte_end]))
    }

    fn parse_struct(&mut self) -> Result<Map<Spanned<&'a str>, Value<'a>>, ParseError> {
        let mut map = Map::new();

        loop {
            let (ident_start, ident) = self
                .next_nw_matches(|c| c.is_alphabetic() || *c == '_')
                .and_then(|c| Ok((self.place, self.parse_ident(c, self.place)?)))?;
            let ident_end = self.place;

            self.assume_next_nw(':')?;

            let value = self.parse_value()?;

            if map.insert(ident, value).is_some() {
                return Err(Spanned {
                    span: Span::new(ident_start, ident_end),
                    value: Error::DuplicateIdent,
                });
            }

            match (self.assume_next_nw(','), self.assume_next_nw('}')) {
                (_, Ok(_)) => break Ok(map),
                (
                    _,
                    Err(
                        err @ ParseError {
                            value: Error::UnexpectedEnd,
                            ..
                        },
                    ),
                )
                | (Err(_), Err(err)) => break Err(err),
                (Ok(_), _) => {}
            }
        }
    }

    fn parse_path(&mut self, c: char, start: Position) -> Result<Path<'a>, ParseError> {
        let leading = c == ':';
        let ident_start = if leading {
            self.assume_next(':')?;
            self.place
        } else {
            start
        };
        let mut peek = self.clone();
        match peek.parse_ident(c, ident_start) {
            Ok(first) => {
                *self = peek;

                let mut idents = vec![first];

                loop {
                    if !self.peek(|this| Ok(this.next_c()? == ':' && this.next_c()? == ':')) {
                        break;
                    }

                    let start = self.next_place();
                    let ident = self
                        .next_nw_matches(|c| c.is_alphabetic() || *c == '_')
                        .and_then(|c| self.parse_ident(c, start))?;
                    idents.push(ident);
                }

                Ok(Path { leading, idents })
            }
            Err(e) => {
                if leading {
                    Ok(Path {
                        leading: true,
                        idents: vec![],
                    })
                } else {
                    Err(e)
                }
            }
        }
    }

    pub fn parse_value(&mut self) -> Result<Value<'a>, ParseError> {
        let c = self.next_nw_matches(|c| {
            matches!(
                c,
                '\'' | '"' | '0'..='9' | '-' | '(' | '[' | '{' | '_' | ':' | '.'
            ) || c.is_alphabetic()
        })?;
        let start = self.place;
        macro_rules! ok {
            ($t:expr) => {
                Ok(self.spanned(start, $t))
            };
        }

        let value = match c {
            '\'' => {
                let c = self.next_c()?;
                let c = match c {
                    '\n' | '\'' => return Err(self.error1(Error::UnexpectedSymbol)),
                    '\\' => escape(self.next_c()?)
                        .ok_or_else(|| self.error(start, Error::UnexpectedSymbol))?,
                    c => c,
                };
                self.assume_next('\'')?;
                ok!(ValueKind::Char(c))
            }
            '"' => {
                let mut value = None;
                let start_index = self.place.byte_end;
                loop {
                    let c = self.next_c()?;
                    let c = match c {
                        '"' => break,
                        '\\' => {
                            let place = self.place;
                            if value.is_none() {
                                value =
                                    Some(self.src[start_index..self.place.byte_start].to_string());
                            }
                            escape(self.next_c()?)
                                .ok_or_else(|| self.error(place, Error::UnexpectedSymbol))?
                        }
                        c => c,
                    };
                    if let Some(ref mut value) = value {
                        value.push(c);
                    }
                }
                let cow = value.map(Cow::Owned).unwrap_or_else(|| {
                    Cow::Borrowed(&self.src[start_index..self.place.byte_start])
                });
                ok!(ValueKind::String(cow))
            }
            c @ '0'..='9' | c @ '-' => self.parse_number(start, c),
            '(' => {
                let seq = self.parse_sequence(')')?;
                ok!(ValueKind::Tuple(seq))
            }
            '[' => {
                let seq = self.parse_sequence(']')?;
                ok!(ValueKind::Array(seq))
            }
            '{' => {
                let mut peek = self.clone();
                match peek.parse_map(start) {
                    Ok(map) => {
                        *self = peek;
                        Ok(map)
                    }
                    Err(Spanned {
                        value: Error::ExpectedFatArrow,
                        ..
                    }) => self
                        .parse_struct()
                        .map(|v| self.spanned(start, ValueKind::Struct(v))),
                    e => e,
                }
            }
            ':' if self.assume_next(':').is_ok() => {
                let path = self.parse_path(c, start)?;
                ok!(ValueKind::Path(path))
            }
            '.' if self.assume_next('.').is_ok() => {
                let inclusive = self.assume_next('=').is_ok();
                let mut peek = self.clone();

                let err = peek.error(start, Error::InclusiveNoEnd);

                let end = peek.parse_value();

                if end.is_err() && inclusive {
                    return Err(err);
                } else {
                    if end.is_ok() {
                        *self = peek;
                    }
                    ok!(ValueKind::Range {
                        min: None,
                        max: end.ok().map(Box::new),
                        inclusive,
                    })
                }
            }
            c if c == '_' || c.is_alphabetic() => {
                let path = self.parse_path(c, start)?;
                let path = self.spanned(start, path);
                if self.assume_next_nw('(').is_ok() {
                    let tuple = self.parse_sequence(')')?;
                    ok!(ValueKind::NamedTuple(path, tuple))
                } else if self.assume_next_nw('{').is_ok() {
                    let value = self.parse_struct()?;
                    ok!(ValueKind::NamedStruct(path, value))
                } else {
                    ok!(ValueKind::Path(path.inner()))
                }
            }
            _ => Err(self.error1(Error::UnexpectedSymbol)),
        };

        let other_start = self.place;
        match (self.assume_next_nw('.'), self.assume_next('.')) {
            (Ok(_), Ok(_)) => {
                let inclusive = self.assume_next('=').is_ok();
                let mut peek = self.clone();

                let err = peek.error(other_start, Error::InclusiveNoEnd);

                let end = peek.parse_value();

                if end.is_err() && inclusive {
                    Err(err)
                } else {
                    if end.is_ok() {
                        *self = peek;
                    }
                    ok!(ValueKind::Range {
                        min: value.ok().map(Box::new),
                        max: end.ok().map(Box::new),
                        inclusive,
                    })
                }
            }
            (Ok(_), Err(e)) => Err(e),
            _ => value,
        }
    }
}
