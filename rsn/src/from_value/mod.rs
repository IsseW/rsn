#[cfg(feature = "impl_hashbrown")]
mod hashbrown;
#[cfg(feature = "impl_vek")]
mod vek;

use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    hash::Hash,
    ops::{Range, RangeBounds, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive},
};

use arrayvec::ArrayVec;

use crate::{Spanned, ValueKind};

use super::Value;

#[derive(Debug)]
pub enum Error {
    MissingField(&'static str),
    ExpectedIdent(&'static str),
    UnexpectedIdent,
    ExpectedAmountOfElements(RangeInclusive<usize>),
    ExpectedType(&'static str),
    ExpectedPattern(&'static [&'static str]),
    Custom(String),
}

impl Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::MissingField(field) => write!(f, "Missing field {field}"),
            Error::ExpectedIdent(ident) => write!(f, "Expected {ident}"),
            Error::UnexpectedIdent => write!(f, "Unexpected ident"),
            Error::ExpectedAmountOfElements(range) => {
                if range.start() == range.end() {
                    write!(f, "Expected {} elements", range.start())
                } else {
                    write!(
                        f,
                        "Expected between {} and {} elements",
                        range.start(),
                        range.end()
                    )
                }
            }
            Error::ExpectedType(ty) => write!(f, "Expected the type {ty}"),
            Error::ExpectedPattern(patterns) => {
                if patterns.len() == 1 {
                    write!(f, "Expected the pattern {}", patterns[0])
                } else {
                    writeln!(f, "Expected one of the following patterns:")?;
                    patterns
                        .iter()
                        .try_for_each(|pattern| writeln!(f, "\t{}", pattern))
                }
            }
            Error::Custom(custom) => write!(f, "{custom}"),
        }
    }
}

pub type FromValueError = Spanned<Error>;

pub trait FromValue: Sized {
    fn from_value(value: Value) -> Result<Self, FromValueError>;
}

impl FromValue for i64 {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        match *value {
            ValueKind::Integer(i) => Ok(i),
            _ => Err(FromValueError {
                span: value.span,
                value: Error::ExpectedType("i64"),
            }),
        }
    }
}

macro_rules! int_from_value {
    ($($ty:ty),*$(,)?) => {
        $(
        impl FromValue for $ty {
            fn from_value(value: Value) -> Result<Self, FromValueError> {
                let span = value.span;
                i64::from_value(value).and_then(|i| i.try_into().map_err(|_| FromValueError::new(span, Error::ExpectedType(stringify!($ty)))))
            }
        }
        )*
    };
}

int_from_value! {
    i8,
    i16,
    i32,
    u8,
    u16,
    u32,
    u64,
}

impl FromValue for f64 {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        match &*value {
            ValueKind::Float(f) => Ok(*f),
            ValueKind::Integer(i) => Ok(*i as f64),
            ValueKind::Path(path) if path.is_ident("INFINITY") => Ok(f64::INFINITY),
            ValueKind::Path(path) if path.is_ident("NEG_INFINITY") => Ok(f64::NEG_INFINITY),
            ValueKind::Path(path) if path.is_ident("NAN") => Ok(f64::NAN),
            _ => Err(FromValueError {
                span: value.span,
                value: Error::ExpectedType("f64"),
            }),
        }
    }
}

impl FromValue for f32 {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        f64::from_value(value)
            .map(|f| f as f32)
            .map_err(|err| FromValueError {
                value: Error::ExpectedType("f32"),
                ..err
            })
    }
}

impl<T: FromValue> FromValue for Range<T> {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Range {
                min: Some(min),
                max: Some(max),
                inclusive: false,
            } => T::from_value(*min).and_then(|a| T::from_value(*max).map(|b| a..b)),
            _ => Err(FromValueError {
                span,
                value: Error::ExpectedPattern(&["<value>..<value>"]),
            }),
        }
    }
}

impl<T: FromValue> FromValue for RangeInclusive<T> {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Range {
                min: Some(min),
                max: Some(max),
                inclusive: true,
            } => T::from_value(*min).and_then(|a| T::from_value(*max).map(|b| a..=b)),
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["<value>..=<value>"]),
            )),
        }
    }
}

impl<T: FromValue> FromValue for RangeFrom<T> {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Range {
                min: Some(min),
                max: None,
                inclusive: false,
            } => T::from_value(*min).map(|a| a..),
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["<value>.."]),
            )),
        }
    }
}

impl<T: FromValue> FromValue for RangeTo<T> {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Range {
                min: None,
                max: Some(max),
                inclusive: false,
            } => T::from_value(*max).map(|a| ..a),
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["..<value>"]),
            )),
        }
    }
}

impl<T: FromValue> FromValue for RangeToInclusive<T> {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Range {
                min: None,
                max: Some(max),
                inclusive: true,
            } => T::from_value(*max).map(|a| ..=a),
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["..=<value>"]),
            )),
        }
    }
}

impl FromValue for RangeFull {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Range {
                min: None,
                max: None,
                inclusive: false,
            } => Ok(..),
            _ => Err(FromValueError::new(span, Error::ExpectedPattern(&[".."]))),
        }
    }
}

pub enum AnyRange<T> {
    Range(Range<T>),
    RangeInclusive(RangeInclusive<T>),
    RangeFrom(RangeFrom<T>),
    RangeTo(RangeTo<T>),
    RangeToInclusive(RangeToInclusive<T>),
    RangeFull,
}

impl<T> RangeBounds<T> for AnyRange<T> {
    fn start_bound(&self) -> std::ops::Bound<&T> {
        match self {
            AnyRange::Range(r) => r.start_bound(),
            AnyRange::RangeInclusive(r) => r.start_bound(),
            AnyRange::RangeFrom(r) => r.start_bound(),
            AnyRange::RangeTo(r) => r.start_bound(),
            AnyRange::RangeToInclusive(r) => r.start_bound(),
            AnyRange::RangeFull => RangeFull.start_bound(),
        }
    }

    fn end_bound(&self) -> std::ops::Bound<&T> {
        match self {
            AnyRange::Range(r) => r.end_bound(),
            AnyRange::RangeInclusive(r) => r.end_bound(),
            AnyRange::RangeFrom(r) => r.end_bound(),
            AnyRange::RangeTo(r) => r.end_bound(),
            AnyRange::RangeToInclusive(r) => r.end_bound(),
            AnyRange::RangeFull => RangeFull.end_bound(),
        }
    }
}

impl<T: FromValue> FromValue for AnyRange<T> {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Range {
                min,
                max,
                inclusive,
            } => match (min, max, inclusive) {
                (Some(min), Some(max), false) => T::from_value(*min)
                    .and_then(|a| T::from_value(*max).map(|b| AnyRange::Range(a..b))),
                (Some(min), Some(max), true) => T::from_value(*min)
                    .and_then(|a| T::from_value(*max).map(|b| AnyRange::RangeInclusive(a..=b))),
                (Some(min), None, false) => T::from_value(*min).map(|a| AnyRange::RangeFrom(a..)),
                (None, Some(max), false) => T::from_value(*max).map(|a| AnyRange::RangeTo(..a)),
                (None, Some(max), true) => {
                    T::from_value(*max).map(|a| AnyRange::RangeToInclusive(..=a))
                }
                (None, None, false) => Ok(AnyRange::RangeFull),
                _ => Err(FromValueError::new(
                    span,
                    Error::ExpectedPattern(&[
                        "..",
                        "<value>..",
                        "..<value>",
                        "..=<value>",
                        "<value>..<value>",
                        "<value>..=<value>",
                    ]),
                )),
            },
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&[
                    "..",
                    "<value>..",
                    "..<value>",
                    "..=<value>",
                    "<value>..<value>",
                    "<value>..=<value>",
                ]),
            )),
        }
    }
}

impl<const N: usize, T: FromValue> FromValue for [T; N] {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Array(s) => {
                if s.len() == N {
                    let t: [Value; N] = s.try_into().unwrap();
                    t.try_map(T::from_value)
                } else {
                    Err(FromValueError::new(
                        span,
                        Error::ExpectedAmountOfElements(N..=N),
                    ))
                }
            }
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["[<value>, <value>, ...]"]),
            )),
        }
    }
}

impl<T: FromValue> FromValue for Vec<T> {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Array(s) => s
                .into_iter()
                .map(|value| T::from_value(value))
                .try_collect(),
            v => T::from_value(Spanned::new(span, v)).map(|t| vec![t]),
        }
    }
}

impl<T: FromValue, const N: usize> FromValue for ArrayVec<T, N> {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Array(s) => {
                if s.len() <= N {
                    s.into_iter()
                        .map(|value| T::from_value(value))
                        .try_collect()
                } else {
                    Err(FromValueError::new(
                        span,
                        Error::ExpectedAmountOfElements(0..=N),
                    ))
                }
            }
            v if N >= 1 => T::from_value(Spanned::new(span, v)).map(|t| {
                let mut vec = ArrayVec::new();
                vec.push(t);
                vec
            }),
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["[<value>, <value>, ...]"]),
            )),
        }
    }
}

impl<K, V> FromValue for HashMap<K, V>
where
    K: FromValue + Hash + Eq,
    V: FromValue,
{
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Map(map) => map
                .into_iter()
                .map(|(k, v)| Ok((K::from_value(k)?, V::from_value(v)?)))
                .try_collect(),
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["{<value> => <value>, <value> => <value>, ...}"]),
            )),
        }
    }
}

#[impl_trait_for_tuples::impl_for_tuples(1, 16)]
impl FromValue for Tuple {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Tuple(seq) => {
                const LEN: usize = for_tuples!( #(1)+* );
                if seq.len() == LEN {
                    let mut seq = seq.into_iter();
                    Ok(for_tuples!((
                    #({
                        Tuple::from_value(seq.next().unwrap())?
                    }),*)))
                } else {
                    Err(FromValueError::new(
                        span,
                        Error::ExpectedAmountOfElements(LEN..=LEN),
                    ))
                }
            }
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["(<value>, <value>, ...)"]),
            )),
        }
    }
}

impl<T: FromValue> FromValue for Option<T> {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Path(ident) => {
                if ident.is_enum::<true>("std::option::Option::None") {
                    Ok(None)
                } else {
                    Err(FromValueError::new(span, Error::ExpectedIdent("None")))
                }
            }
            ValueKind::NamedTuple(ident, inner) => {
                if ident.is_enum::<true>("std::option::Option::Some") {
                    if inner.len() == 1 {
                        T::from_value(inner.into_iter().next().unwrap()).map(Some)
                    } else {
                        Err(FromValueError::new(
                            span,
                            Error::ExpectedAmountOfElements(1..=1),
                        ))
                    }
                } else {
                    Err(FromValueError::new(span, Error::ExpectedIdent("Some")))
                }
            }
            v => T::from_value(Spanned::new(span, v)).map(Some),
        }
    }
}

impl FromValue for bool {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Path(path) if path.is_ident("true") => Ok(true),
            ValueKind::Path(path) if path.is_ident("false") => Ok(false),
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["true", "false"]),
            )),
        }
    }
}

impl FromValue for () {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Tuple(vec) if vec.is_empty() => Ok(()),
            _ => Err(FromValueError::new(span, Error::ExpectedPattern(&["()"]))),
        }
    }
}

impl FromValue for String {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::String(str) => Ok(str.to_string()),
            _ => Err(FromValueError::new(span, Error::ExpectedType("String"))),
        }
    }
}

impl FromValue for char {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Char(c) => Ok(c),
            _ => Err(FromValueError::new(span, Error::ExpectedType("char"))),
        }
    }
}

impl<T: FromValue> FromValue for Box<T> {
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        T::from_value(value).map(Box::new)
    }
}
