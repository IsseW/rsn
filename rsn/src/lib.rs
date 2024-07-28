#![allow(incomplete_features)]
#![feature(
    array_try_map,
    iterator_try_collect,
    iter_collect_into,
    specialization,
    const_trait_impl
)]

mod from_value;
mod parse;
mod spanned;
mod value;

pub use from_value::{AnyRange, Error as FromValueErrorKind, FromValue, FromValueError};
pub use parse::ParseError;
pub use spanned::{Position, Span, Spanned};
pub use value::{Fields, Map, Path, Value, ValueKind};

pub use from_value::flatten::{
    NamedFields, ParseNamedFields, ParseUnnamedFields, UnnamedFields, __types,
};

#[cfg(feature = "derive")]
pub use rsn_derive::FromValue;

use std::fmt::{Debug, Display, Write};

pub struct FullError(String);

impl Debug for FullError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Display for FullError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for FullError {}

pub fn de<T: FromValue<()>>(src: &str) -> Result<T, FullError> {
    Value::parse_str(src)
        .map_err(|e| {
            let mut err = e.display_in_src(src);
            let _ = write!(err, "{}", e.value);
            FullError(err)
        })
        .and_then(|v| {
            T::from_value(v, &mut ()).map_err(|e| {
                let mut err = e.display_in_src(src);
                let _ = write!(err, "{}", e.value);
                FullError(err)
            })
        })
}

#[cfg(test)]
mod tests {
    use std::borrow::Cow;

    use super::from_value::FromValue;
    use super::*;

    #[test]
    fn test_parse() {
        assert_eq!(
            u32::from_value(Value::parse_str("123").unwrap(), &mut ()).unwrap(),
            123
        );
        assert_eq!(
            *Value::parse_str("0x22F").unwrap(),
            ValueKind::Integer(0x22F)
        );

        assert_eq!(
            *Value::parse_str("-0xA2F").unwrap(),
            ValueKind::Integer(-0xA2F)
        );

        assert_eq!(
            *Value::parse_str("0o123567").unwrap(),
            ValueKind::Integer(0o123567)
        );
        assert_eq!(
            *Value::parse_str("0b1001").unwrap(),
            ValueKind::Integer(0b1001)
        );

        assert_eq!(*Value::parse_str("2.25").unwrap(), ValueKind::Float(2.25));
        assert_eq!(*Value::parse_str("92e5").unwrap(), ValueKind::Float(92e5));

        assert_eq!(*Value::parse_str("'c'").unwrap(), ValueKind::Char('c'));
        assert_eq!(*Value::parse_str("'\\n'").unwrap(), ValueKind::Char('\n'));
        assert_eq!(
            *Value::parse_str("\"Hello\\nWorld! 💖\"").unwrap(),
            ValueKind::String("Hello\nWorld! 💖".into())
        );

        assert!(matches!(
            *Value::parse_str("\"Test\"").unwrap(),
            ValueKind::String(Cow::Borrowed("Test"))
        ));

        assert_eq!(
            *Value::parse_str("(1, 2, 3, \"Hello!\")").unwrap(),
            ValueKind::Tuple(vec![
                ValueKind::Integer(1).into(),
                ValueKind::Integer(2).into(),
                ValueKind::Integer(3).into(),
                ValueKind::String("Hello!".into()).into(),
            ])
        );

        assert_eq!(
            *Value::parse_str("Hello").unwrap(),
            ValueKind::Path("Hello".into())
        );

        assert_eq!(
            *Value::parse_str("r#was_sup").unwrap(),
            ValueKind::Path("r#was_sup".into())
        );
        let value = Value::parse_str(
            "
            Foo {
                test_a: 23,
                test_b: \"How are you?\",
                test_c: (1.2, 2, '💖'),
                test_d: [\"wa\", \"wu\", \"we\"]
            }
        ",
        )
        .unwrap()
        .inner();

        assert_eq!(
            value,
            ValueKind::NamedStruct(
                Spanned::create("Foo".into()),
                [
                    (Spanned::create("test_a"), ValueKind::Integer(23).into()),
                    (
                        Spanned::create("test_b"),
                        ValueKind::String("How are you?".into()).into()
                    ),
                    (
                        Spanned::create("test_c"),
                        ValueKind::Tuple(vec![
                            ValueKind::Float(1.2).into(),
                            ValueKind::Integer(2).into(),
                            ValueKind::Char('💖').into(),
                        ])
                        .into()
                    ),
                    (
                        Spanned::create("test_d"),
                        ValueKind::Array(vec![
                            ValueKind::String("wa".into()).into(),
                            ValueKind::String("wu".into()).into(),
                            ValueKind::String("we".into()).into(),
                        ])
                        .into()
                    )
                ]
                .into_iter()
                .collect()
            )
        )
    }
}
