use crate::{Fields, Span, Value};

#[doc(hidden)]
pub mod __types;

/*
#[macro_export]
macro_rules! const_concat {
    ($($s:expr),+) => {{
        const LEN: usize = $( $s.len() + )* 0;
        &{
            let mut arr = [""; LEN];
            let mut base: usize = 0;
            $({
                let mut i = 0;
                while i < $s.len() {
                    arr[base + i] = $s[i];
                    i += 1;
                }
                base += $s.len();
            })*
            if base != LEN { panic!("invalid length"); }
            arr
        }
    }}
}
*/

pub trait NamedFields: Sized {
    type RequiredFields: __types::Set;
    type OptionalFields: __types::Set;

    const MIN_FIELDS: usize;
    const MAX_FIELDS: usize;
}

pub trait ParseNamedFields<M, C>: NamedFields {
    fn parse_required(
        struct_span: Span,
        fields: &mut Fields<C>,
        meta: &mut M,
    ) -> Result<Self, crate::FromValueError>;

    fn parse_optional(
        &mut self,
        struct_span: Span,
        fields: &mut Fields<C>,
        meta: &mut M,
    ) -> Result<(), crate::FromValueError>;

    fn parse_fields(
        struct_span: Span,
        fields: &mut Fields<C>,
        meta: &mut M,
    ) -> Result<Self, crate::FromValueError> {
        let mut this = Self::parse_required(struct_span, fields, meta)?;

        this.parse_optional(struct_span, fields, meta)?;

        Ok(this)
    }
}

pub trait WriteNamedFields<M, C>: NamedFields {
    fn write_fields<'a>(&'a self, fields: &mut Fields<'a, C>, meta: &'a M)
    where
        C: 'a;
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
    fn write_fields<'a>(&'a self, fields: &mut Vec<Value<'a, C>>, write_default: bool, meta: &'a M)
    where
        C: 'a;
}
