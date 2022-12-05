use crate::{Span, Value, value::Fields};


#[doc(hidden)]
pub mod __types;

pub trait NamedFields: Sized {
    type Fields: __types::Set;

    const REQUIRED_FIELDS: &'static [&'static str];
    const OPTIONAL_FIELDS: &'static [&'static str];
    const MIN_FIELDS: usize;
    const MAX_FIELDS: usize;

    fn parse_fields(struct_span: Span, fields: &mut Fields) -> Result<Self, crate::FromValueError>;
}

pub trait UnnamedFields: Sized {
    const LEN: usize;

    fn parse_fields<'a, I: Iterator<Item = Value<'a>>>(struct_span: Span, fields: &mut I) -> Result<Self, crate::FromValueError>;
}