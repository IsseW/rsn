use vek::*;

use crate::{FromValue, FromValueError, ValueKind};

use super::Error;

impl<M, C, T: FromValue<M, C>> FromValue<M, C> for Vec2<T> {
    fn from_value(value: crate::Value<C>, meta: &mut M) -> Result<Self, crate::FromValueError> {
        <(T, T)>::from_value(value, meta).map(Self::from)
    }
}

impl<M, C, T: FromValue<M, C>> FromValue<M, C> for Vec3<T> {
    fn from_value(value: crate::Value<C>, meta: &mut M) -> Result<Self, crate::FromValueError> {
        <(T, T, T)>::from_value(value, meta).map(Self::from)
    }
}

impl<M, C, T: FromValue<M, C>> FromValue<M, C> for Vec4<T> {
    fn from_value(value: crate::Value<C>, meta: &mut M) -> Result<Self, crate::FromValueError> {
        <(T, T, T, T)>::from_value(value, meta).map(Self::from)
    }
}

impl<M, C, T: FromValue<M, C>> FromValue<M, C> for Aabr<T> {
    fn from_value(value: crate::Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        macro_rules! from_fields {
            ($fields:expr) => {{
                let mut fields = $fields;
                let correct_len = fields.len() == 2;
                let mut get_field = |field| {
                    fields
                        .swap_remove(field)
                        .ok_or(FromValueError::new(span, Error::MissingField(field)))
                };
                if correct_len {
                    Ok(Aabr {
                        min: FromValue::from_value(get_field("min")?, meta)?,
                        max: FromValue::from_value(get_field("max")?, meta)?,
                    })
                } else {
                    get_field("min")?;
                    get_field("max")?;
                    let (ident, _) = fields.iter().next().unwrap();
                    Err(FromValueError::new(ident.span, Error::UnexpectedIdent))
                }
            }};
        }
        match value.inner() {
            ValueKind::Range {
                min: Some(min),
                max: Some(max),
                inclusive: true,
            } => Ok(Aabr {
                min: FromValue::from_value(*min, meta)?,
                max: FromValue::from_value(*max, meta)?,
            }),
            ValueKind::NamedStruct(ident, fields) => {
                if ident.is_struct("vek::Aabr") {
                    from_fields!(fields)
                } else {
                    Err(FromValueError {
                        span: ident.span,
                        value: Error::ExpectedIdent("Aabr"),
                    })
                }
            }
            ValueKind::Struct(fields) => {
                from_fields!(fields)
            }
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&[
                    "(<value>, <value>)..=(<value>, <value>)",
                    "{ min: (<value>, <value>), max: (<value>, <value>) }",
                    "Aabr { min: (<value>, <value>), max: (<value>, <value>) }",
                ]),
            )),
        }
    }
}

impl<M, C, T: FromValue<M, C>> FromValue<M, C> for Aabb<T> {
    fn from_value(value: crate::Value<C>, meta: &mut M) -> Result<Self, FromValueError> {
        let span = value.span;
        macro_rules! from_fields {
            ($fields:expr) => {{
                let mut fields = $fields;
                let correct_len = fields.len() == 2;
                let mut get_field = |field| {
                    fields
                        .swap_remove(field)
                        .ok_or(FromValueError::new(span, Error::MissingField(field)))
                };
                if correct_len {
                    Ok(Aabb {
                        min: FromValue::from_value(get_field("min")?, meta)?,
                        max: FromValue::from_value(get_field("max")?, meta)?,
                    })
                } else {
                    get_field("min")?;
                    get_field("max")?;
                    let (ident, _) = fields.iter().next().unwrap();
                    Err(FromValueError::new(ident.span, Error::UnexpectedIdent))
                }
            }};
        }
        match value.inner() {
            ValueKind::Range {
                min: Some(min),
                max: Some(max),
                inclusive: true,
            } => Ok(Aabb {
                min: FromValue::from_value(*min, meta)?,
                max: FromValue::from_value(*max, meta)?,
            }),
            ValueKind::NamedStruct(ident, fields) => {
                if ident.is_struct("vek::Aabb") {
                    from_fields!(fields)
                } else {
                    Err(FromValueError {
                        span: ident.span,
                        value: Error::ExpectedIdent("Aabb"),
                    })
                }
            }
            ValueKind::Struct(fields) => {
                from_fields!(fields)
            }
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&[
                    "(<value>, <value>, <value>)..=(<value>, <value>, <value>)",
                    "{ min: (<value>, <value>, <value>), max: (<value>, <value>, <value>) }",
                    "Aabr { min: (<value>, <value>, <value>), max: (<value>, <value>, <value>) }",
                ]),
            )),
        }
    }
}

impl<M, C, T: FromValue<M, C>> FromValue<M, C> for Rgb<T> {
    fn from_value(value: crate::Value<C>, meta: &mut M) -> Result<Self, crate::FromValueError> {
        <(T, T, T)>::from_value(value, meta).map(Self::from)
    }
}

impl<M, C, T: FromValue<M, C>> FromValue<M, C> for Rgba<T> {
    fn from_value(value: crate::Value<C>, meta: &mut M) -> Result<Self, crate::FromValueError> {
        <(T, T, T, T)>::from_value(value, meta).map(Self::from)
    }
}
