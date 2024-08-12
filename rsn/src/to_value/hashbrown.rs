use hashbrown::{HashMap, HashSet};
use rsn_derive::rsn;
use rsn_parser::value::Value;

use std::hash::Hash;

use super::ToValue;

impl<M, C, K, V> ToValue<M, C> for HashMap<K, V>
where
    K: ToValue<M, C> + Hash + Eq,
    V: ToValue<M, C>,
{
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        let keys = self.keys();
        let values = self.values();
        rsn! {@(meta) =>
            #{#keys => #values}*
        }
    }
}

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for HashSet<T> {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        let this = self;
        rsn! {@(meta) =>
            #[#this]*
        }
    }
}
