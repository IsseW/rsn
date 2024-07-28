use crate::{value::Fields, Span, Value};

#[doc(hidden)]
pub mod __types;

pub trait NamedFields: Sized {
    type Fields: __types::Set;

    const REQUIRED_FIELDS: &'static [&'static str];
    const OPTIONAL_FIELDS: &'static [&'static str];
    const MIN_FIELDS: usize;
    const MAX_FIELDS: usize;
}

pub trait ParseNamedFields<M>: NamedFields {
    fn parse_fields(
        struct_span: Span,
        fields: &mut Fields,
        meta: &mut M,
    ) -> Result<Self, crate::FromValueError>;
}

pub trait UnnamedFields: Sized {
    const LEN: usize;
}

pub trait ParseUnnamedFields<M>: UnnamedFields {
    fn parse_fields<'a, I: Iterator<Item = Value<'a>>>(
        struct_span: Span,
        fields: &mut I,
        meta: &mut M,
    ) -> Result<Self, crate::FromValueError>;
}
