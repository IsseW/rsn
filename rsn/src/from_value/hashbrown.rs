use crate::{FromValue, FromValueError, Value, ValueKind};
use std::hash::{BuildHasher, Hash};

use super::Error;

impl<'a, M, C, K, V, S> FromValue<'a, M, C> for hashbrown::HashMap<K, V, S>
where
    K: FromValue<'a, M, C> + Hash + Eq,
    V: FromValue<'a, M, C>,
    S: Default + BuildHasher,
{
    fn from_value(value: Value<'a, C>, meta: &mut M) -> Result<Self, FromValueError> {
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

impl<'a, M, C, K> FromValue<'a, M, C> for hashbrown::HashSet<K>
where
    K: FromValue<'a, M, C> + Hash + Eq,
{
    fn from_value(value: Value<'a, C>, meta: &mut M) -> Result<Self, FromValueError> {
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
