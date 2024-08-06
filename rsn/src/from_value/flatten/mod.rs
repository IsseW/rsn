use crate::{Fields, Span, Value};

#[doc(hidden)]
pub mod __types;

pub trait NamedFields: Sized {
    type Fields: __types::Set;

    const REQUIRED_FIELDS: &'static [&'static str];
    const OPTIONAL_FIELDS: &'static [&'static str];
    const MIN_FIELDS: usize;
    const MAX_FIELDS: usize;
}

pub trait ParseNamedFields<M, C>: NamedFields {
    fn parse_fields(
        struct_span: Span,
        fields: &mut Fields<C>,
        meta: &mut M,
    ) -> Result<Self, crate::FromValueError>;
}

pub trait UnnamedFields: Sized {
    const LEN: usize;
}

pub trait ParseUnnamedFields<M, C>: UnnamedFields {
    fn parse_fields<'a, I: Iterator<Item = Value<'a, C>>>(
        struct_span: Span,
        fields: &mut I,
        meta: &mut M,
    ) -> Result<Self, crate::FromValueError>;
}
