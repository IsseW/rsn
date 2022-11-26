use crate::{FromValue, FromValueError, Value, ValueKind};
use std::hash::{BuildHasher, Hash};

use super::Error;

impl<K, V, S> FromValue for hashbrown::HashMap<K, V, S>
where
    K: FromValue + Hash + Eq,
    V: FromValue,
    S: Default + BuildHasher,
{
    fn from_value(value: Value) -> Result<Self, FromValueError> {
        let span = value.span;
        match value.inner() {
            ValueKind::Map(map) => map
                .into_iter()
                .map(|(k, v)| Ok((K::from_value(k)?, V::from_value(v)?)))
                .try_collect(),
            _ => Err(FromValueError::new(
                span,
                Error::ExpectedPattern(&["{<value> => <value>, <value> => <value>, ...}"]),
            )),
        }
    }
}
