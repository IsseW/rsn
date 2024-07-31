use crate::{Spanned, Value, ValueKind};

pub trait ToValue<M> {
    fn to_value(&self, meta: &M) -> Value;
}

impl<'a, M> ToValue<M> for Value<'a> {
    fn to_value(&self, _meta: &M) -> Value {
        self.clone()
    }
}

macro_rules! int_to_value {
    ($($ty:ty),*$(,)?) => {
        $(
        impl<M> ToValue<M> for $ty {
            fn to_value(&self, _meta: &M) -> Value {
                Spanned::create(ValueKind::Integer(*self as _))
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
        impl<M> ToValue<M> for $ty {
            fn to_value(&self, _meta: &M) -> Value {
                Spanned::create(ValueKind::Float(*self as _))
            }
        }
        )*
    };
}

float_to_value!(f32, f64);
