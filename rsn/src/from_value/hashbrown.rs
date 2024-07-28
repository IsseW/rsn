use crate::{FromValue, FromValueError, Value, ValueKind};
use std::hash::{BuildHasher, Hash};

use super::Error;

impl<M, K, V, S> FromValue<M> for hashbrown::HashMap<K, V, S>
where
    K: FromValue<M> + Hash + Eq,
    V: FromValue<M>,
    S: Default + BuildHasher,
{
    fn from_value(value: Value, meta: &mut M) -> Result<Self, FromValueError> {
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
