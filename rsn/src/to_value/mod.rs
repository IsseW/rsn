use std::{
    collections::{HashMap, HashSet},
    hash::Hash,
    ops::{Range, RangeFrom, RangeFull, RangeInclusive, RangeTo, RangeToInclusive},
};

use arrayvec::ArrayVec;
use rsn_derive::rsn;

use crate::{AnyRange, Path, Spanned, UnnamedFields, Value, ValueKind, WriteUnnamedFields};

pub mod default;
#[cfg(feature = "hashbrown")]
mod hashbrown;
#[cfg(feature = "vek")]
mod vek;

pub trait ToValue<M, C> {
    fn to_value<'a>(&'a self, meta: &'a M) -> Value<'a, C>
    where
        C: 'a;
}

impl<'a, M, C> ToValue<M, C> for Value<'a, C>
where
    C: Clone,
{
    fn to_value<'v>(&'v self, _meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        self.clone()
    }
}

impl<'a, M, C, T: ToValue<M, C>> ToValue<M, C> for &'a T {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        T::to_value(*self, meta)
    }
}

macro_rules! int_to_value {
    ($($ty:ty),*$(,)?) => {
        $(
        impl<M, C> ToValue<M, C> for $ty {
            #[allow(unused_comparisons)]
            fn to_value<'v>(&'v self, _meta: &'v M) -> Value<'v, C> where C: 'v {
                if *self == Self::MAX {
                    Spanned::create(ValueKind::Path(Path { leading: false, idents: vec![Spanned::create("MAX")] }))
                } else if *self < 0 && *self == Self::MIN {
                    Spanned::create(ValueKind::Path(Path { leading: false, idents: vec![Spanned::create("MIN")] }))
                } else {
                    Spanned::create(ValueKind::Integer(*self as _))
                }
            }
        }
        )*
    };
}

int_to_value! {
    i8,
    i16,
    i32,
    i64,
    i128,
    isize,
    u8,
    u16,
    u32,
    u64,
    usize,
}

macro_rules! float_to_value {
    ($($ty:ty),*$(,)?) => {
        $(
        impl<M, C> ToValue<M, C> for $ty {
            fn to_value<'v>(&'v self, _meta: &'v M) -> Value<'v, C> where C: 'v {
                Spanned::create(ValueKind::Float(*self as _))
            }
        }
        )*
    };
}

float_to_value!(f32, f64);

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for Range<T> {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        let start = &self.start;
        let end = &self.end;

        rsn! {@(meta) =>
            #start..#end
        }
    }
}

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for RangeInclusive<T> {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        let start = self.start();
        let end = self.end();

        rsn! {@(meta) =>
            #start..=#end
        }
    }
}

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for RangeFrom<T> {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        let start = &self.start;

        rsn! {@(meta) =>
            #start..
        }
    }
}

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for RangeTo<T> {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        let end = &self.end;

        rsn! {@(meta) =>
            ..#end
        }
    }
}

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for RangeToInclusive<T> {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        let end = &self.end;

        rsn! {@(meta) =>
            ..=#end
        }
    }
}

impl<M, C> ToValue<M, C> for RangeFull {
    fn to_value<'v>(&'v self, _meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        rsn! {@(meta) =>
            ..
        }
    }
}

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for [T] {
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

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for AnyRange<T> {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        match self {
            AnyRange::Range(r) => r.to_value(meta),
            AnyRange::RangeInclusive(r) => r.to_value(meta),
            AnyRange::RangeFrom(r) => r.to_value(meta),
            AnyRange::RangeTo(r) => r.to_value(meta),
            AnyRange::RangeToInclusive(r) => r.to_value(meta),
            AnyRange::RangeFull => RangeFull.to_value(meta),
        }
    }
}

impl<M, C, T: ToValue<M, C>, const N: usize> ToValue<M, C> for [T; N] {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        self.as_slice().to_value(meta)
    }
}

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for Vec<T> {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        self.as_slice().to_value(meta)
    }
}

impl<M, C, T: ToValue<M, C>, const N: usize> ToValue<M, C> for ArrayVec<T, N> {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        self.as_slice().to_value(meta)
    }
}

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

#[impl_trait_for_tuples::impl_for_tuples(1, 16)]
impl<M, C> ToValue<M, C> for Tuple {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        let values = [for_tuples!(#(Tuple::to_value(&self.Tuple, meta)),*)];
        let values = values.into_iter().collect();

        Spanned::create(ValueKind::Tuple(values))
    }
}

#[impl_trait_for_tuples::impl_for_tuples(1, 16)]
impl<M, C> WriteUnnamedFields<M, C> for Tuple {
    for_tuples!(where #(Tuple: ToValue<M, C>),*);
    fn write_fields<'a>(&'a self, fields: &mut Vec<Value<'a, C>>, _write_default: bool, meta: &'a M)
    where
        C: 'a,
    {
        fields.reserve(<Self as UnnamedFields>::MIN_FIELDS);
        for_tuples!(#(fields.push(Tuple::to_value(&self.Tuple, meta));)*);
    }
}

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for Option<T> {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        match self {
            Some(value) => rsn! {@(meta) =>
                Some(#value)
            },
            None => rsn! {@(meta) => None },
        }
    }
}

impl<M, C> ToValue<M, C> for bool {
    fn to_value<'v>(&'v self, #[allow(unused_variables)] meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        if *self {
            rsn! {@(meta) => true }
        } else {
            rsn! {@(meta) => false }
        }
    }
}

impl<M, C> ToValue<M, C> for () {
    fn to_value<'v>(&'v self, #[allow(unused_variables)] meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        rsn! {@(meta) => () }
    }
}

impl<M, C> ToValue<M, C> for str {
    fn to_value<'v>(&'v self, _meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        Spanned::create(ValueKind::String(std::borrow::Cow::Borrowed(self)))
    }
}

impl<M, C> ToValue<M, C> for String {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        self.as_str().to_value(meta)
    }
}

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for Box<T> {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        T::to_value(&**self, meta)
    }
}
impl<M, C, T: ToValue<M, C>> ToValue<M, C> for std::rc::Rc<T> {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        T::to_value(&**self, meta)
    }
}
impl<M, C, T: ToValue<M, C>> ToValue<M, C> for std::sync::Arc<T> {
    fn to_value<'v>(&'v self, meta: &'v M) -> Value<'v, C>
    where
        C: 'v,
    {
        T::to_value(&**self, meta)
    }
}
