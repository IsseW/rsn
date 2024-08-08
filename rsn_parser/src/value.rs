use std::fmt::Display;

use crate::{Chars, ParseError, Position, Span, Spanned};

pub type Map<K, V> = indexmap::IndexMap<K, V>;

pub type Fields<'a, C> = Map<Spanned<&'a str>, Value<'a, C>>;

#[derive(Clone, Debug, PartialEq)]
pub struct Path<'a> {
    pub leading: bool,
    pub idents: Vec<Spanned<&'a str>>,
}

fn try_trim(text: &mut &str, pattern: &str) -> bool {
    let t = text.trim_start_matches(pattern);
    if text.len() - t.len() == pattern.len() {
        *text = t;
        true
    } else {
        false
    }
}

fn try_trim_end(text: &mut &str, pattern: &str) -> bool {
    let t = text.trim_end_matches(pattern);
    if text.len() - t.len() == pattern.len() {
        *text = t;
        true
    } else {
        false
    }
}

impl<'a> Path<'a> {
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.leading && self.len() == 0
    }

    #[inline(always)]
    pub fn len(&self) -> usize {
        self.idents.len()
    }

    pub fn is_ident(&self, ident: &'a str) -> bool {
        !self.leading && self.idents.len() == 1 && **self.idents.first().unwrap() == ident
    }

    pub fn is_enum<const UNTAGGED: bool>(&self, full_path: &str) -> bool {
        let mut path = full_path;
        if self.leading {
            self.idents
                .iter()
                .all(|ident| try_trim(&mut path, "::") && try_trim(&mut path, ident))
        } else if self.len() == 2 {
            let [e, v] = &*self.idents else {
                unreachable!()
            };
            try_trim_end(&mut path, v)
                && try_trim_end(&mut path, "::")
                && try_trim_end(&mut path, e)
        } else if UNTAGGED && self.len() == 1 {
            try_trim_end(&mut path, self.idents.last().unwrap())
        } else {
            let mut idents = self.idents.iter();
            idents
                .next()
                .map_or(false, |ident| try_trim(&mut path, ident))
                && idents.all(|ident| try_trim(&mut path, "::") && try_trim(&mut path, ident))
        }
    }

    pub fn is_struct(&self, full_path: &str) -> bool {
        let mut path = full_path;
        if self.leading {
            self.idents
                .iter()
                .all(|ident| try_trim(&mut path, "::") && try_trim(&mut path, ident))
        } else if self.len() == 1 {
            try_trim_end(&mut path, self.idents.last().unwrap())
        } else {
            let mut idents = self.idents.iter();
            idents
                .next()
                .map_or(false, |ident| try_trim(&mut path, ident))
                && idents.all(|ident| try_trim(&mut path, "::") && try_trim(&mut path, ident))
        }
    }
}

impl<'a> Display for Path<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.leading {
            write!(f, "::")?;
        }
        let mut idents = self.idents.iter();
        if let Some(ident) = idents.next() {
            write!(f, "{ident}")?;
        }
        for ident in idents {
            write!(f, "::{ident}")?;
        }
        Ok(())
    }
}

impl<'a> From<&'a str> for Path<'a> {
    fn from(value: &'a str) -> Self {
        Self {
            leading: false,
            idents: vec![Spanned::create(value)],
        }
    }
}
impl<'a> From<Spanned<&'a str>> for Path<'a> {
    fn from(value: Spanned<&'a str>) -> Self {
        Self {
            leading: false,
            idents: vec![value],
        }
    }
}

#[derive(Clone, Debug)]
pub enum ValueKind<'a, C = !> {
    Integer(i128),
    Float(f64),
    Bool(bool),
    String(std::borrow::Cow<'a, str>),
    Char(char),
    Path(Path<'a>),
    Array(Vec<Value<'a, C>>),
    Map(Vec<(Value<'a, C>, Value<'a, C>)>),
    Range {
        min: Option<Box<Value<'a, C>>>,
        max: Option<Box<Value<'a, C>>>,
        inclusive: bool,
    },

    Tuple(Vec<Value<'a, C>>),
    NamedTuple(Spanned<Path<'a>>, Vec<Value<'a, C>>),
    Struct(Fields<'a, C>),
    NamedStruct(Spanned<Path<'a>>, Fields<'a, C>),
    Custom(C),
}

impl<'a, C: PartialEq> PartialEq for ValueKind<'a, C> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Integer(l0), Self::Integer(r0)) => l0 == r0,
            (Self::Float(l0), Self::Float(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::Char(l0), Self::Char(r0)) => l0 == r0,
            (
                Self::Range {
                    min: l_min,
                    max: l_max,
                    inclusive: l_inclusive,
                },
                Self::Range {
                    min: r_min,
                    max: r_max,
                    inclusive: r_inclusive,
                },
            ) => l_min == r_min && l_max == r_max && l_inclusive == r_inclusive,
            (Self::Path(l0), Self::Path(r0)) => l0 == r0,
            (Self::Map(l0), Self::Map(r0)) => l0 == r0,
            (Self::Array(l0), Self::Array(r0)) => l0 == r0,
            (Self::Tuple(l0), Self::Tuple(r0)) => l0 == r0,
            (Self::NamedTuple(l0, l1), Self::NamedTuple(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Struct(l0), Self::Struct(r0)) => l0 == r0,
            (Self::NamedStruct(l0, l1), Self::NamedStruct(r0, r1)) => l0 == r0 && l1 == r1,
            (Self::Custom(c0), Self::Custom(c1)) => c0 == c1,
            _ => false,
        }
    }
}

impl<'a, C: Display> Display for ValueKind<'a, C> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ValueKind::Integer(i) => write!(f, "{i}"),
            ValueKind::Float(i) => write!(f, "{i}"),
            ValueKind::Bool(i) => write!(f, "{i}"),
            ValueKind::String(s) => write!(f, "\"{s}\""),
            ValueKind::Char(c) => write!(f, "'{c}'"),
            ValueKind::Range {
                min,
                max,
                inclusive,
            } => match (min, max, inclusive) {
                (None, None, true) => write!(f, "..="),
                (None, None, false) => write!(f, ".."),
                (None, Some(max), true) => write!(f, "..={max}"),
                (None, Some(max), false) => write!(f, "..{max}"),
                (Some(min), None, true) => write!(f, "{min}..="),
                (Some(min), None, false) => write!(f, "{min}.."),
                (Some(min), Some(max), true) => write!(f, "{min}..={max}"),
                (Some(min), Some(max), false) => write!(f, "{min}..={max}"),
            },
            ValueKind::Path(path) => {
                write!(f, "{path}")
            }
            ValueKind::Map(values) => {
                write!(f, "{{")?;
                values
                    .iter()
                    .try_for_each(|(key, value)| write!(f, "{key} => {value}, "))?;
                write!(f, "}}")
            }
            ValueKind::Array(values) => {
                write!(f, "[")?;
                values.iter().try_for_each(|value| write!(f, "{value}, "))?;
                write!(f, "]")
            }
            ValueKind::Tuple(values) => {
                write!(f, "(")?;
                values.iter().try_for_each(|value| write!(f, "{value}, "))?;
                write!(f, ")")
            }
            ValueKind::NamedTuple(ident, values) => {
                write!(f, "{ident}(")?;
                values.iter().try_for_each(|value| write!(f, "{value}, "))?;
                write!(f, ")")
            }
            ValueKind::Struct(fields) => {
                write!(f, "{{")?;
                fields
                    .iter()
                    .try_for_each(|(field, value)| write!(f, "{field}: {value}, "))?;
                write!(f, "}}")
            }
            ValueKind::NamedStruct(ident, fields) => {
                write!(f, "{ident} {{")?;
                fields
                    .iter()
                    .try_for_each(|(field, value)| write!(f, "{field}: {value}, "))?;
                write!(f, "}}")
            }
            ValueKind::Custom(s) => write!(f, "{s}"),
        }
    }
}

pub type Value<'a, C = !> = Spanned<ValueKind<'a, C>>;

fn no_custom<'a, C>(chars: &mut Chars<'a>) -> Result<Value<'a, C>, ParseError> {
    Err(chars.error1(crate::Error::UnexpectedSymbol))
}

impl<'a, C> Value<'a, C> {
    /// Parses a str to a value.
    /// Returns `Err` if the string is not a valid value.
    pub fn parse_str(src: &'a str) -> Result<Self, ParseError> {
        Self::parse_str_with_custom(src, no_custom)
    }

    /// Parses a str to a value.
    /// Returns `Err` if the string is not a valid value.
    pub fn parse_str_with_custom(
        src: &'a str,
        custom: impl Fn(&mut Chars<'a>) -> Result<Value<'a, C>, crate::ParseError>,
    ) -> Result<Self, ParseError> {
        let mut chars = Chars::new(src);

        let value = chars.parse_value(&custom)?;
        chars.end(value)
    }
}

impl<'a> From<ValueKind<'a>> for Value<'a> {
    fn from(value: ValueKind<'a>) -> Value<'a> {
        Value {
            span: Span::new(Position::start(), Position::start()),
            value,
        }
    }
}
