use std::{fmt::Display, marker::PhantomData};

use rsn_parser::{
    spanned::Spanned,
    value::{Fields, Path, Value, ValueKind},
};
use serde::{
    de::{EnumAccess, MapAccess, SeqAccess, VariantAccess},
    ser::{
        SerializeMap, SerializeSeq, SerializeStruct, SerializeStructVariant, SerializeTuple,
        SerializeTupleStruct, SerializeTupleVariant,
    },
    Serializer,
};

use crate::{FromValue, FromValueError, FromValueErrorKind, ToValue};

pub struct Serde<T>(pub T);

impl<T> Serde<T> {
    pub fn from_value<'v, C>(value: Value<'v, C>) -> Result<T, crate::FromValueError>
    where
        T: serde::Deserialize<'v>,
        C: Clone,
    {
        T::deserialize(ValueDeserializer { v: value }).map_err(|err| err.0)
    }

    pub fn to_value<'v, C: 'v>(v: &'v T) -> Value<'v, C>
    where
        T: serde::Serialize,
    {
        T::serialize(v, ValueSerializer::default()).unwrap()
    }
}

impl<'de, T: serde::Deserialize<'de>, M, C: Clone> FromValue<'de, M, C> for Serde<T> {
    fn from_value(value: Value<'de, C>, _meta: &mut M) -> Result<Self, crate::FromValueError> {
        Self::from_value(value).map(Self)
    }
}

impl<T: serde::Serialize, M, C> ToValue<M, C> for Serde<T> {
    fn to_value<'a>(&'a self, _meta: &'a M) -> Value<'a, C>
    where
        C: 'a,
    {
        T::serialize(&self.0, ValueSerializer::default()).unwrap()
    }
}

struct ValueDeserializer<'v, C> {
    v: Value<'v, C>,
}

struct WrappedError(crate::FromValueError);

impl Display for WrappedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl std::fmt::Debug for WrappedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
    }
}

impl std::error::Error for WrappedError {}

impl serde::de::Error for WrappedError {
    fn custom<T>(msg: T) -> Self
    where
        T: Display,
    {
        WrappedError(Spanned::create(crate::FromValueErrorKind::Custom(
            msg.to_string(),
        )))
    }
}

struct ValueSequence<'v, C> {
    v: std::vec::IntoIter<Value<'v, C>>,
}

impl<'de, C: Clone> SeqAccess<'de> for ValueSequence<'de, C> {
    type Error = WrappedError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        self.v
            .next()
            .map(|v| seed.deserialize(ValueDeserializer { v }))
            .transpose()
    }
}

struct FieldDeserializer<'v> {
    s: Spanned<&'v str>,
}

impl<'de> serde::Deserializer<'de> for FieldDeserializer<'de> {
    type Error = WrappedError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_borrowed_str(&self.s)
    }

    fn deserialize_bool<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("bool"),
        )))
    }

    fn deserialize_i8<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("i8"),
        )))
    }

    fn deserialize_i16<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("i8"),
        )))
    }

    fn deserialize_i32<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("i8"),
        )))
    }

    fn deserialize_i64<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("i8"),
        )))
    }

    fn deserialize_u8<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("i8"),
        )))
    }

    fn deserialize_u16<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("i8"),
        )))
    }

    fn deserialize_u32<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("i8"),
        )))
    }

    fn deserialize_u64<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("i8"),
        )))
    }

    fn deserialize_f32<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("i8"),
        )))
    }

    fn deserialize_f64<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("i8"),
        )))
    }

    fn deserialize_char<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("i8"),
        )))
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_borrowed_str(&self.s)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_borrowed_str(&self.s)
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("bytes"),
        )))
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("byte buf"),
        )))
    }

    fn deserialize_option<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("option"),
        )))
    }

    fn deserialize_unit<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("unit"),
        )))
    }

    fn deserialize_unit_struct<V>(
        self,
        _name: &'static str,
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("unit struct"),
        )))
    }

    fn deserialize_newtype_struct<V>(
        self,
        _name: &'static str,
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("newtype struct"),
        )))
    }

    fn deserialize_seq<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("seq"),
        )))
    }

    fn deserialize_tuple<V>(self, _len: usize, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("tuple"),
        )))
    }

    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("tuple struct"),
        )))
    }

    fn deserialize_map<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("map"),
        )))
    }

    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("struct"),
        )))
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        _visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Err(WrappedError(FromValueError::new(
            self.s.span,
            FromValueErrorKind::ExpectedType("enum"),
        )))
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_borrowed_str(&self.s)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        visitor.visit_borrowed_str(&self.s)
    }
}

struct ValueStructMap<'v, C> {
    curr: Option<(Spanned<&'v str>, Value<'v, C>)>,
    v: indexmap::map::IntoIter<Spanned<&'v str>, Value<'v, C>>,
}

impl<'de, C: Clone> MapAccess<'de> for ValueStructMap<'de, C> {
    type Error = WrappedError;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: serde::de::DeserializeSeed<'de>,
    {
        self.curr = self.v.next();

        self.curr
            .as_ref()
            .map(|k| seed.deserialize(FieldDeserializer { s: k.0.clone() }))
            .transpose()
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        if let Some((_, v)) = self.curr.take() {
            seed.deserialize(ValueDeserializer { v })
        } else {
            panic!("`next_key_seed` should always be called first")
        }
    }
}

enum ValueVariant<'v, C> {
    Unit(Path<'v>),
    Unnamed(Spanned<Path<'v>>, Vec<Value<'v, C>>),
    Named(Spanned<Path<'v>>, Fields<'v, C>),
}

impl<'de, C: Clone> VariantAccess<'de> for ValueVariant<'de, C> {
    type Error = WrappedError;

    fn unit_variant(self) -> Result<(), Self::Error> {
        match self {
            ValueVariant::Unit(_) => Ok(()),
            ValueVariant::Unnamed(p, _) | ValueVariant::Named(p, _) => Err(WrappedError(
                FromValueError::new(p.span, FromValueErrorKind::ExpectedType("Unit enum")),
            )),
        }
    }

    fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
    where
        T: serde::de::DeserializeSeed<'de>,
    {
        match self {
            ValueVariant::Unnamed(p, fields) => {
                let mut iter = fields.into_iter();

                if let (Some(v), None) = (iter.next(), iter.next()) {
                    seed.deserialize(ValueDeserializer { v })
                } else {
                    Err(WrappedError(FromValueError::new(
                        p.span,
                        FromValueErrorKind::ExpectedAmountOfElements(1..=1),
                    )))
                }
            }
            ValueVariant::Unit(p) => Err(WrappedError(FromValueError::new(
                p.span(),
                FromValueErrorKind::ExpectedType("Tuple enum"),
            ))),
            ValueVariant::Named(p, _) => Err(WrappedError(FromValueError::new(
                p.span,
                FromValueErrorKind::ExpectedType("Tuple enum"),
            ))),
        }
    }

    fn tuple_variant<V>(self, len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self {
            ValueVariant::Unnamed(p, fields) => {
                if fields.len() != len {
                    return Err(WrappedError(FromValueError::new(
                        p.span,
                        FromValueErrorKind::ExpectedAmountOfElements(len..=len),
                    )));
                }
                visitor.visit_seq(ValueSequence {
                    v: fields.into_iter(),
                })
            }
            ValueVariant::Unit(p) => Err(WrappedError(FromValueError::new(
                p.span(),
                FromValueErrorKind::ExpectedType("Tuple enum"),
            ))),
            ValueVariant::Named(p, _) => Err(WrappedError(FromValueError::new(
                p.span,
                FromValueErrorKind::ExpectedType("Tuple enum"),
            ))),
        }
    }

    fn struct_variant<V>(
        self,
        expected_fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self {
            ValueVariant::Named(p, fields) => {
                let mut fc = 0;

                for f in expected_fields {
                    if fields.contains_key(*f) {
                        fc += 1;
                    }
                }

                if fc != fields.len() {
                    return Err(WrappedError(FromValueError::new(
                        p.span,
                        FromValueErrorKind::Custom(
                            "Contains fields that aren't expected".to_string(),
                        ),
                    )));
                }

                visitor.visit_map(ValueStructMap {
                    curr: None,
                    v: fields.into_iter(),
                })
            }
            ValueVariant::Unit(p) => Err(WrappedError(FromValueError::new(
                p.span(),
                FromValueErrorKind::ExpectedType("Struct enum"),
            ))),
            ValueVariant::Unnamed(p, _) => Err(WrappedError(FromValueError::new(
                p.span,
                FromValueErrorKind::ExpectedType("Struct enum"),
            ))),
        }
    }
}

struct ValueEnum<'v, C> {
    v: Value<'v, C>,
}

impl<'de, C: Clone> EnumAccess<'de> for ValueEnum<'de, C> {
    type Error = WrappedError;

    type Variant = ValueVariant<'de, C>;

    fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        let variant = match &self.v.value {
            ValueKind::Path(p) => ValueVariant::Unit(p.clone()),
            ValueKind::NamedTuple(p, fields) => ValueVariant::Unnamed(p.clone(), fields.clone()),
            ValueKind::NamedStruct(p, fields) => ValueVariant::Named(p.clone(), fields.clone()),
            _ => {
                return Err(WrappedError(FromValueError::new(
                    self.v.span,
                    FromValueErrorKind::ExpectedType("enum"),
                )))
            }
        };
        seed.deserialize(ValueDeserializer { v: self.v })
            .map(|v| (v, variant))
    }
}

struct ValueMap<'v, C> {
    next_value: Option<Value<'v, C>>,
    v: std::vec::IntoIter<(Value<'v, C>, Value<'v, C>)>,
}

impl<'de, C: Clone> MapAccess<'de> for ValueMap<'de, C> {
    type Error = WrappedError;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where
        K: serde::de::DeserializeSeed<'de>,
    {
        self.v
            .next()
            .map(|(k, v)| {
                self.next_value = Some(v);

                seed.deserialize(ValueDeserializer { v: k })
            })
            .transpose()
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::DeserializeSeed<'de>,
    {
        seed.deserialize(ValueDeserializer {
            v: self.next_value.take().unwrap(),
        })
    }
}

impl<'de, C: Clone> serde::Deserializer<'de> for ValueDeserializer<'de, C> {
    type Error = WrappedError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Integer(i) => visitor.visit_i128(i),
            ValueKind::Float(f) => visitor.visit_f64(f),
            ValueKind::Bool(b) => visitor.visit_bool(b),
            ValueKind::String(s) => match s {
                std::borrow::Cow::Borrowed(s) => visitor.visit_borrowed_str(s),
                std::borrow::Cow::Owned(s) => visitor.visit_string(s),
            },
            ValueKind::Char(c) => visitor.visit_char(c),
            ValueKind::Array(seq) => visitor.visit_seq(ValueSequence { v: seq.into_iter() }),
            ValueKind::Map(map) => visitor.visit_map(ValueMap {
                v: map.into_iter(),
                next_value: None,
            }),
            ValueKind::Range { .. } => {
                panic!("Ranges aren't supported with serde");
            }
            ValueKind::Tuple(tuple) => visitor.visit_seq(ValueSequence {
                v: tuple.into_iter(),
            }),
            ValueKind::Path(p) if p.is_enum::<true>("std::option::Option::None") => {
                visitor.visit_none()
            }
            ValueKind::NamedTuple(p, fields)
                if p.is_enum::<true>("std::option::Option::Some") && fields.len() == 1 =>
            {
                visitor.visit_some(ValueDeserializer {
                    v: fields.into_iter().next().expect("We checked the length"),
                })
            }
            ValueKind::Path(_)
            | ValueKind::NamedTuple(_, _)
            | ValueKind::Struct(_)
            | ValueKind::NamedStruct(_, _) => visitor.visit_enum(ValueEnum { v: self.v }),
            ValueKind::Custom(_) => panic!("Custom types aren't supported with serde"),
        }
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Bool(b) => visitor.visit_bool(b),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("bool"),
            ))),
        }
    }

    fn deserialize_i8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Integer(i) => visitor.visit_i8(i as _),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("integer"),
            ))),
        }
    }

    fn deserialize_i16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Integer(i) => visitor.visit_i16(i as _),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("integer"),
            ))),
        }
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Integer(i) => visitor.visit_i32(i as _),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("integer"),
            ))),
        }
    }

    fn deserialize_i64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Integer(i) => visitor.visit_i64(i as _),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("integer"),
            ))),
        }
    }

    fn deserialize_u8<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Integer(i) => visitor.visit_u8(i as _),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("integer"),
            ))),
        }
    }

    fn deserialize_u16<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Integer(i) => visitor.visit_u16(i as _),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("integer"),
            ))),
        }
    }

    fn deserialize_u32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Integer(i) => visitor.visit_u32(i as _),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("integer"),
            ))),
        }
    }

    fn deserialize_u64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Integer(i) => visitor.visit_u64(i as _),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("integer"),
            ))),
        }
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Float(f) => visitor.visit_f32(f as _),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("float"),
            ))),
        }
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Float(f) => visitor.visit_f64(f as _),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("float"),
            ))),
        }
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Char(c) => visitor.visit_char(c),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("char"),
            ))),
        }
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::String(s) => match s {
                std::borrow::Cow::Borrowed(s) => visitor.visit_borrowed_str(s),
                std::borrow::Cow::Owned(s) => visitor.visit_string(s),
            },
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("string"),
            ))),
        }
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        Self::deserialize_str(self, visitor)
    }

    fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_seq(visitor)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Path(path) if path.is_enum::<true>("std::option::Option::None") => {
                visitor.visit_none()
            }
            ValueKind::NamedTuple(path, fields)
                if path.is_enum::<true>("std::option::Option::Some") =>
            {
                if fields.len() == 1 {
                    visitor.visit_some(ValueDeserializer {
                        v: fields
                            .into_iter()
                            .next()
                            .expect("We just checked the length"),
                    })
                } else {
                    Err(WrappedError(FromValueError::new(
                        self.v.span,
                        FromValueErrorKind::ExpectedAmountOfElements(1..=1),
                    )))
                }
            }
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("option"),
            ))),
        }
    }

    fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Tuple(fields) if fields.is_empty() => visitor.visit_unit(),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedPattern(&["()"]),
            ))),
        }
    }

    fn deserialize_unit_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Path(p) if p.is_ident(name) => visitor.visit_unit(),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType(name),
            ))),
        }
    }

    fn deserialize_newtype_struct<V>(
        self,
        name: &'static str,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::NamedTuple(p, fields) if p.is_ident(name) => {
                if fields.len() == 1 {
                    visitor.visit_newtype_struct(ValueDeserializer {
                        v: fields
                            .into_iter()
                            .next()
                            .expect("We just checked the length"),
                    })
                } else {
                    Err(WrappedError(FromValueError::new(
                        self.v.span,
                        FromValueErrorKind::ExpectedAmountOfElements(1..=1),
                    )))
                }
            }
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType(name),
            ))),
        }
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Array(seq) => visitor.visit_seq(ValueSequence { v: seq.into_iter() }),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("array"),
            ))),
        }
    }

    fn deserialize_tuple<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Tuple(seq) => visitor.visit_seq(ValueSequence { v: seq.into_iter() }),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("tuple"),
            ))),
        }
    }

    fn deserialize_tuple_struct<V>(
        self,
        name: &'static str,
        _len: usize,
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::NamedTuple(p, seq) if p.is_ident(name) => {
                visitor.visit_seq(ValueSequence { v: seq.into_iter() })
            }
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType(name),
            ))),
        }
    }

    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::Map(map) => visitor.visit_map(ValueMap {
                v: map.into_iter(),
                next_value: None,
            }),
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType("map"),
            ))),
        }
    }

    fn deserialize_struct<V>(
        self,
        name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match self.v.value {
            ValueKind::NamedStruct(p, fields) if p.is_ident(name) => {
                visitor.visit_map(ValueStructMap {
                    curr: None,
                    v: fields.into_iter(),
                })
            }
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType(name),
            ))),
        }
    }

    fn deserialize_enum<V>(
        self,
        name: &'static str,
        variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        match &self.v.value {
            ValueKind::Path(p)
            | ValueKind::NamedTuple(Spanned { value: p, .. }, _)
            | ValueKind::NamedStruct(Spanned { value: p, .. }, _)
                if p.len() == 2
                    && *p.idents[0] == name
                    && variants.iter().any(|variant| *p.idents[1] == *variant) =>
            {
                visitor.visit_enum(ValueEnum { v: self.v })
            }
            _ => Err(WrappedError(FromValueError::new(
                self.v.span,
                FromValueErrorKind::ExpectedType(name),
            ))),
        }
    }

    fn deserialize_identifier<V>(self, _visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        todo!()
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where
        V: serde::de::Visitor<'de>,
    {
        self.deserialize_any(visitor)
    }
}

#[derive(Debug)]
pub enum SerializeError {
    DuplicateField(&'static str),
    Custom(String),
}

pub struct ValueSerializer<'v, C>(PhantomData<&'v C>);

impl<'v, C> Default for ValueSerializer<'v, C> {
    fn default() -> Self {
        Self(PhantomData)
    }
}

impl Display for SerializeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SerializeError::Custom(s) => f.write_str(s),
            SerializeError::DuplicateField(s) => write!(f, "Duplicate field `{s}`"),
        }
    }
}

impl std::error::Error for SerializeError {}

impl serde::ser::Error for SerializeError {
    fn custom<T>(msg: T) -> Self
    where
        T: Display,
    {
        Self::Custom(msg.to_string())
    }
}

pub struct SerializeValueSeq<'v, C> {
    seq: Vec<Value<'v, C>>,
}

impl<'v, C: 'v> SerializeSeq for SerializeValueSeq<'v, C> {
    type Ok = Value<'v, C>;

    type Error = SerializeError;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.seq.push(value.serialize(ValueSerializer::default())?);

        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Array(self.seq)))
    }
}

impl<'v, C: 'v> SerializeTuple for SerializeValueSeq<'v, C> {
    type Ok = Value<'v, C>;

    type Error = SerializeError;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.seq.push(value.serialize(ValueSerializer::default())?);

        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Tuple(self.seq)))
    }
}

pub struct SerializeNamedTuple<'v, C> {
    path: Path<'v>,
    seq: Vec<Value<'v, C>>,
}

impl<'v, C: 'v> SerializeTupleStruct for SerializeNamedTuple<'v, C> {
    type Ok = Value<'v, C>;

    type Error = SerializeError;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.seq.push(value.serialize(ValueSerializer::default())?);

        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::NamedTuple(
            Spanned::create(self.path),
            self.seq,
        )))
    }
}

impl<'v, C: 'v> SerializeTupleVariant for SerializeNamedTuple<'v, C> {
    type Ok = Value<'v, C>;

    type Error = SerializeError;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.seq.push(value.serialize(ValueSerializer::default())?);

        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::NamedTuple(
            Spanned::create(self.path),
            self.seq,
        )))
    }
}

pub struct SerializeValueMap<'v, C> {
    next_key: Option<Value<'v, C>>,
    map: Vec<(Value<'v, C>, Value<'v, C>)>,
}

impl<'v, C: 'v> SerializeMap for SerializeValueMap<'v, C> {
    type Ok = Value<'v, C>;

    type Error = SerializeError;

    fn serialize_key<T>(&mut self, key: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.next_key = Some(key.serialize(ValueSerializer::default())?);

        Ok(())
    }

    fn serialize_value<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        self.map.push((
            self.next_key.take().unwrap(),
            value.serialize(ValueSerializer::default())?,
        ));

        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Map(self.map)))
    }
}

pub struct SerializeValueStruct<'v, C> {
    path: Path<'v>,
    fields: Fields<'v, C>,
}

impl<'v, C: 'v> SerializeStruct for SerializeValueStruct<'v, C> {
    type Ok = Value<'v, C>;

    type Error = SerializeError;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        if self
            .fields
            .insert(
                Spanned::create(key),
                value.serialize(ValueSerializer::default())?,
            )
            .is_some()
        {
            return Err(SerializeError::DuplicateField(key));
        }

        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::NamedStruct(
            Spanned::create(self.path),
            self.fields,
        )))
    }
}

impl<'v, C: 'v> SerializeStructVariant for SerializeValueStruct<'v, C> {
    type Ok = Value<'v, C>;

    type Error = SerializeError;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        SerializeStruct::serialize_field(self, key, value)
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        SerializeStruct::end(self)
    }
}

impl<'v, C> Serializer for ValueSerializer<'v, C> {
    type Ok = Value<'v, C>;

    type Error = SerializeError;

    type SerializeSeq = SerializeValueSeq<'v, C>;

    type SerializeTuple = SerializeValueSeq<'v, C>;

    type SerializeTupleStruct = SerializeNamedTuple<'v, C>;

    type SerializeTupleVariant = SerializeNamedTuple<'v, C>;

    type SerializeMap = SerializeValueMap<'v, C>;

    type SerializeStruct = SerializeValueStruct<'v, C>;

    type SerializeStructVariant = SerializeValueStruct<'v, C>;

    fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Bool(v)))
    }

    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Integer(v as _)))
    }

    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Integer(v as _)))
    }

    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Integer(v as _)))
    }

    fn serialize_i64(self, v: i64) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Integer(v as _)))
    }

    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Integer(v as _)))
    }

    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Integer(v as _)))
    }

    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Integer(v as _)))
    }

    fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Integer(v as _)))
    }

    fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Float(v as _)))
    }

    fn serialize_f64(self, v: f64) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Float(v)))
    }

    fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Char(v)))
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::String(std::borrow::Cow::Owned(
            v.to_string(),
        ))))
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Array(
            v.iter()
                .map(|v| Value::create(ValueKind::Integer(*v as _)))
                .collect(),
        )))
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Path(Path {
            leading: false,
            idents: vec![Spanned::create("None")],
        })))
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        Ok(Value::create(ValueKind::NamedTuple(
            Spanned::create(Path {
                leading: false,
                idents: vec![Spanned::create("Some")],
            }),
            vec![value.serialize(ValueSerializer::default())?],
        )))
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Tuple(vec![])))
    }

    fn serialize_unit_struct(self, name: &'static str) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Path(Path {
            leading: false,
            idents: vec![Spanned::create(name)],
        })))
    }

    fn serialize_unit_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        Ok(Value::create(ValueKind::Path(Path {
            leading: false,
            idents: vec![Spanned::create(name), Spanned::create(variant)],
        })))
    }

    fn serialize_newtype_struct<T>(
        self,
        name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        Ok(Value::create(ValueKind::NamedTuple(
            Spanned::create(Path {
                leading: false,
                idents: vec![Spanned::create(name)],
            }),
            vec![value.serialize(ValueSerializer::default())?],
        )))
    }

    fn serialize_newtype_variant<T>(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where
        T: ?Sized + serde::Serialize,
    {
        Ok(Value::create(ValueKind::NamedTuple(
            Spanned::create(Path {
                leading: false,
                idents: vec![Spanned::create(name), Spanned::create(variant)],
            }),
            vec![value.serialize(ValueSerializer::default())?],
        )))
    }

    fn serialize_seq(self, len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        Ok(SerializeValueSeq {
            seq: match len {
                Some(l) => Vec::with_capacity(l),
                None => Vec::new(),
            },
        })
    }

    fn serialize_tuple(self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        Ok(SerializeValueSeq {
            seq: Vec::with_capacity(len),
        })
    }

    fn serialize_tuple_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        Ok(SerializeNamedTuple {
            path: Path {
                leading: false,
                idents: vec![Spanned::create(name)],
            },
            seq: Vec::with_capacity(len),
        })
    }

    fn serialize_tuple_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        Ok(SerializeNamedTuple {
            path: Path {
                leading: false,
                idents: vec![Spanned::create(name), Spanned::create(variant)],
            },
            seq: Vec::with_capacity(len),
        })
    }

    fn serialize_map(self, len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        Ok(SerializeValueMap {
            next_key: None,
            map: match len {
                Some(l) => Vec::with_capacity(l),
                None => Vec::new(),
            },
        })
    }

    fn serialize_struct(
        self,
        name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        Ok(SerializeValueStruct {
            path: Path {
                leading: false,
                idents: vec![Spanned::create(name)],
            },
            fields: Fields::with_capacity(len),
        })
    }

    fn serialize_struct_variant(
        self,
        name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        Ok(SerializeValueStruct {
            path: Path {
                leading: false,
                idents: vec![Spanned::create(name), Spanned::create(variant)],
            },
            fields: Fields::with_capacity(len),
        })
    }
}
