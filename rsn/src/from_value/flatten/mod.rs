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

pub trait WriteNamedFields<M, C>: NamedFields {
    fn write_fields(&self, struct_span: Span, fields: &mut Fields<C>, meta: &M);
}

pub trait UnnamedFields: Sized {
    const MIN_FIELDS: usize;
    const MAX_FIELDS: usize;
}

pub trait ParseUnnamedFields<M, C>: UnnamedFields {
    fn parse_fields<'a, I: Iterator<Item = Value<'a, C>>>(
        struct_span: Span,
        fields: &mut I,
        parse_default: bool,
        meta: &mut M,
    ) -> Result<Self, crate::FromValueError>;
}

pub trait WriteUnnamedFields<M, C>: UnnamedFields {
    fn write_fields<'a, I: Iterator<Item = Value<'a, C>>>(
        &self,
        struct_span: Span,
        fields: &mut I,
        write_default: bool,
        meta: &M,
    );
}
