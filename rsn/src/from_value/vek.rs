use vek::*;

use crate::{FromValue, FromValueError, ValueKind};

use super::Error;

impl<T: FromValue> FromValue for Vec2<T> {
    fn from_value(value: crate::Value) -> Result<Self, crate::FromValueError> {
        <(T, T)>::from_value(value).map(Self::from)
    }
}

impl<T: FromValue> FromValue for Vec3<T> {
    fn from_value(value: crate::Value) -> Result<Self, crate::FromValueError> {
        <(T, T, T)>::from_value(value).map(Self::from)
    }
}

impl<T: FromValue> FromValue for Vec4<T> {
    fn from_value(value: crate::Value) -> Result<Self, crate::FromValueError> {
        <(T, T, T, T)>::from_value(value).map(Self::from)
    }
}

impl<T: FromValue> FromValue for Aabr<T> {
    fn from_value(value: crate::Value) -> Result<Self, FromValueError> {
        let span = value.span;
        macro_rules! from_fields {
            ($fields:expr) => {{
                let mut fields = $fields;
                let correct_len = fields.len() == 2;
                let mut get_field = |field| {
                    fields
                        .remove(field)
                        .ok_or(FromValueError::new(span, Error::MissingField(field)))
                };
                if correct_len {
                    Ok(Aabr {
                        min: FromValue::from_value(get_field("min")?)?,
                        max: FromValue::from_value(get_field("max")?)?,
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
                min: FromValue::from_value(*min)?,
                max: FromValue::from_value(*max)?,
            }),
            ValueKind::NamedStruct(ident, fields) => {
                if *ident == "Aabb" {
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
                    "(<value>, <value>)..=(<value>, <value>)",
                    "{ min: (<value>, <value>), max: (<value>, <value>) }",
                    "Aabr { min: (<value>, <value>), max: (<value>, <value>) }",
                ]),
            )),
        }
    }
}

impl<T: FromValue> FromValue for Aabb<T> {
    fn from_value(value: crate::Value) -> Result<Self, FromValueError> {
        let span = value.span;
        macro_rules! from_fields {
            ($fields:expr) => {{
                let mut fields = $fields;
                let correct_len = fields.len() == 2;
                let mut get_field = |field| {
                    fields
                        .remove(field)
                        .ok_or(FromValueError::new(span, Error::MissingField(field)))
                };
                if correct_len {
                    Ok(Aabb {
                        min: FromValue::from_value(get_field("min")?)?,
                        max: FromValue::from_value(get_field("max")?)?,
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
                min: FromValue::from_value(*min)?,
                max: FromValue::from_value(*max)?,
            }),
            ValueKind::NamedStruct(ident, fields) => {
                if *ident == "Aabb" {
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
                    "(<value>, <value>)..=(<value>, <value>)",
                    "{ min: (<value>, <value>), max: (<value>, <value>) }",
                    "Aabr { min: (<value>, <value>), max: (<value>, <value>) }",
                ]),
            )),
        }
    }
}

impl<T: FromValue> FromValue for Rgb<T> {
    fn from_value(value: crate::Value) -> Result<Self, crate::FromValueError> {
        <(T, T, T)>::from_value(value).map(Self::from)
    }
}

impl<T: FromValue> FromValue for Rgba<T> {
    fn from_value(value: crate::Value) -> Result<Self, crate::FromValueError> {
        <(T, T, T, T)>::from_value(value).map(Self::from)
    }
}
