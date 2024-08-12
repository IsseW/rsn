pub mod flatten;
#[cfg(feature = "impl_hashbrown")]
mod hashbrown;
#[cfg(feature = "impl_vek")]
mod vek;

use std::{
    collections::{HashMap, HashSet},
    fmt::{Debug, Display},
    hash::Hash,
    ops::{Range, RangeBounds, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive},
};

use arrayvec::ArrayVec;
use rsn_parser::spanned::Span;

use crate::{ParseUnnamedFields, Spanned, UnnamedFields, ValueKind};

use super::Value;

#[derive(Debug)]
pub enum Error {
    MissingField(&'static str),
    ExpectedIdent(&'static str),
    UnexpectedIdent,
    UnexpectedField,
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
            Error::UnexpectedField => write!(f, "Unexpected field"),
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

pub trait FromValue<M, C>: Sized {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError>;
}

macro_rules! int_from_value {
    ($($ty:ty),*$(,)?) => {
        $(
        impl<M, C> FromValue<M, C> for $ty {
            fn from_value(value: Value<C>, _meta: &mut M) -> Result<Self, FromValueError> {
                let span = value.span;
                match value.inner() {
                    ValueKind::Integer(i) => Ok(i.try_into().map_err(|_| FromValueError {
                        span,
                        value: Error::ExpectedType(stringify!($ty)),
                    })?),
                    ValueKind::Path(path) if path.is_ident("MIN") => Ok(<$ty>::MIN),
                    ValueKind::Path(path) if path.is_ident("MAX") => Ok(<$ty>::MAX),
                    _ => Err(FromValueError {
                        span,
                        value: Error::ExpectedType(stringify!($ty)),
                    }),
                }
            }
        }
        )*
    };
}

int_from_value! {
    i8,
    i16,
    i32,
    i64,
    i128,
    isize,
    u8,
    u16,
    u32,
    u64,
    usize,
}

macro_rules! float_from_value {
    ($($ty:ty),*$(,)?) => {
        $(
        impl<M, C> FromValue<M, C> for $ty {
            fn from_value(value: Value<C>, _meta: &mut M) -> Result<Self, FromValueError> {
                let span = value.span;
                match value.inner() {
                    ValueKind::Float(f) => Ok(f as $ty),
                    ValueKind::Integer(i) => Ok(i as $ty),
                    ValueKind::Path(path) if path.is_ident("INFINITY") => Ok(<$ty>::INFINITY),
                    ValueKind::Path(path) if path.is_ident("NEG_INFINITY") => Ok(<$ty>::NEG_INFINITY),
                    ValueKind::Path(path) if path.is_ident("NAN") => Ok(<$ty>::NAN),
                    _ => Err(FromValueError {
                        span,
                        value: Error::ExpectedType("f64"),
                    }),
                }
            }
        }
        )*
    };
}

float_from_value!(f32, f64);

impl<M, C, T: FromValue<M, C>> FromValue<M, C> for Range<T> {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Range {
                min: Some(min),
                max: Some(max),
                inclusive: false,
            } => T::from_value(*min, meta).and_then(|a| T::from_value(*max, meta).map(|b| a..b)),
            _ => Err(FromValueError {
                span,
                value: Error::ExpectedPattern(&["<value>..<value>"]),
            }),
        }
    }
}

impl<M, C, T: FromValue<M, C>> FromValue<M, C> for RangeInclusive<T> {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Range {
                min: Some(min),
                max: Some(max),
                inclusive: true,
            } => T::from_value(*min, meta).and_then(|a| T::from_value(*max, meta).map(|b| a..=b)),
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["<value>..=<value>"]),
            )),
        }
    }
}

impl<M, C, T: FromValue<M, C>> FromValue<M, C> for RangeFrom<T> {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Range {
                min: Some(min),
                max: None,
                inclusive: false,
            } => T::from_value(*min, meta).map(|a| a..),
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["<value>.."]),
            )),
        }
    }
}

impl<M, C, T: FromValue<M, C>> FromValue<M, C> for RangeTo<T> {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Range {
                min: None,
                max: Some(max),
                inclusive: false,
            } => T::from_value(*max, meta).map(|a| ..a),
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["..<value>"]),
            )),
        }
    }
}

impl<M, C, T: FromValue<M, C>> FromValue<M, C> for RangeToInclusive<T> {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Range {
                min: None,
                max: Some(max),
                inclusive: true,
            } => T::from_value(*max, meta).map(|a| ..=a),
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["..=<value>"]),
            )),
        }
    }
}

impl<M, C> FromValue<M, C> for RangeFull {
    fn from_value(value: Value<C>, _: &mut M) -> Result<Self, FromValueError> {
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

impl<M, C, T: FromValue<M, C>> FromValue<M, C> for AnyRange<T> {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Range {
                min,
                max,
                inclusive,
            } => match (min, max, inclusive) {
                (Some(min), Some(max), false) => T::from_value(*min, meta)
                    .and_then(|a| T::from_value(*max, meta).map(|b| AnyRange::Range(a..b))),
                (Some(min), Some(max), true) => T::from_value(*min, meta).and_then(|a| {
                    T::from_value(*max, meta).map(|b| AnyRange::RangeInclusive(a..=b))
                }),
                (Some(min), None, false) => {
                    T::from_value(*min, meta).map(|a| AnyRange::RangeFrom(a..))
                }
                (None, Some(max), false) => {
                    T::from_value(*max, meta).map(|a| AnyRange::RangeTo(..a))
                }
                (None, Some(max), true) => {
                    T::from_value(*max, meta).map(|a| AnyRange::RangeToInclusive(..=a))
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

impl<const N: usize, M, C, T: FromValue<M, C>> FromValue<M, C> for [T; N] {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Array(s) => {
                if s.len() == N {
                    if let Ok(t) = <[Value<C>; N]>::try_from(s) {
                        t.try_map(|t| T::from_value(t, meta))
                    } else {
                        unreachable!("We just checked the length");
                    }
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

impl<M, C, T: FromValue<M, C>> FromValue<M, C> for Vec<T> {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Array(s) => s
                .into_iter()
                .map(|value| T::from_value(value, meta))
                .try_collect(),
            v => T::from_value(Spanned::new(span, v), meta).map(|t| vec![t]),
        }
    }
}

impl<M, C, T: FromValue<M, C>, const N: usize> FromValue<M, C> for ArrayVec<T, N> {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Array(s) => {
                if s.len() <= N {
                    s.into_iter()
                        .map(|value| T::from_value(value, meta))
                        .try_collect()
                } else {
                    Err(FromValueError::new(
                        span,
                        Error::ExpectedAmountOfElements(0..=N),
                    ))
                }
            }
            v if N >= 1 => T::from_value(Spanned::new(span, v), meta).map(|t| {
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

impl<M, C, K, V> FromValue<M, C> for HashMap<K, V>
where
    K: FromValue<M, C> + Hash + Eq,
    V: FromValue<M, C>,
{
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Map(map) => map
                .into_iter()
                .map(|(k, v)| Ok((K::from_value(k, meta)?, V::from_value(v, meta)?)))
                .try_collect(),
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["{<value> => <value>, <value> => <value>, ...}"]),
            )),
        }
    }
}

impl<M, C, K> FromValue<M, C> for HashSet<K>
where
    K: FromValue<M, C> + Hash + Eq,
{
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Array(seq) => seq
                .into_iter()
                .map(|k| (K::from_value(k, meta)))
                .try_collect(),
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["[<value>, <value>, ...]"]),
            )),
        }
    }
}

#[impl_trait_for_tuples::impl_for_tuples(1, 16)]
impl<M, C> FromValue<M, C> for Tuple {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Tuple(seq) => {
                const LEN: usize = for_tuples!( #(1)+* );
                if seq.len() == LEN {
                    let mut seq = seq.into_iter();
                    Ok(for_tuples!((
                    #({
                        Tuple::from_value(seq.next().unwrap(), meta)?
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

#[impl_trait_for_tuples::impl_for_tuples(1, 16)]
impl UnnamedFields for Tuple {
    const MIN_FIELDS: usize = for_tuples!( #(1)+* );
    const MAX_FIELDS: usize = Self::MIN_FIELDS;
}

#[impl_trait_for_tuples::impl_for_tuples(1, 16)]
impl<M, C> ParseUnnamedFields<M, C> for Tuple {
    for_tuples!(where #(Tuple: FromValue<M, C>),*);
    fn parse_fields<'a, I: Iterator<Item = Value<'a, C>>>(
        struct_span: Span,
        fields: &mut I,
        _parse_default: bool,
        meta: &mut M,
    ) -> Result<Self, FromValueError> {
        Ok(for_tuples!(
            (
                #(Tuple::from_value(fields.next().ok_or(FromValueError::new(struct_span, Error::ExpectedAmountOfElements(Self::MIN_FIELDS..=Self::MAX_FIELDS)))?, meta)?),*
            )
        ))
    }
}

impl<M, C, T: FromValue<M, C>> FromValue<M, C> for Option<T> {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
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
                        T::from_value(inner.into_iter().next().unwrap(), meta).map(Some)
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
            v => T::from_value(Spanned::new(span, v), meta).map(Some),
        }
    }
}

impl<M, C> FromValue<M, C> for bool {
    fn from_value(value: Value<C>, _: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Bool(val) => Ok(val),
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["true", "false"]),
            )),
        }
    }
}

impl<M, C> FromValue<M, C> for () {
    fn from_value(value: Value<C>, _: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Tuple(vec) if vec.is_empty() => Ok(()),
            _ => Err(FromValueError::new(span, Error::ExpectedPattern(&["()"]))),
        }
    }
}

impl<M, C> FromValue<M, C> for String {
    fn from_value(value: Value<C>, _: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::String(str) => Ok(str.to_string()),
            _ => Err(FromValueError::new(span, Error::ExpectedType("String"))),
        }
    }
}

impl<M, C> FromValue<M, C> for char {
    fn from_value(value: Value<C>, _: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Char(c) => Ok(c),
            _ => Err(FromValueError::new(span, Error::ExpectedType("char"))),
        }
    }
}

impl<M, C, T: FromValue<M, C>> FromValue<M, C> for Box<T> {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        T::from_value(value, meta).map(Self::new)
    }
}
impl<M, C, T: FromValue<M, C>> FromValue<M, C> for std::rc::Rc<T> {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        T::from_value(value, meta).map(Self::new)
    }
}
impl<M, C, T: FromValue<M, C>> FromValue<M, C> for std::sync::Arc<T> {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        T::from_value(value, meta).map(Self::new)
    }
}
impl<M, C, T: FromValue<M, C>> FromValue<M, C> for std::sync::Mutex<T> {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        T::from_value(value, meta).map(Self::new)
    }
}
impl<M, C, T: FromValue<M, C>> FromValue<M, C> for std::sync::RwLock<T> {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        T::from_value(value, meta).map(Self::new)
    }
}
impl<M, C, T: FromValue<M, C>> FromValue<M, C> for std::cell::RefCell<T> {
    fn from_value(value: Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        T::from_value(value, meta).map(Self::new)
    }
}
