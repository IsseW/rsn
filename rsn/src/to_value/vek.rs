use rsn_derive::rsn;
use vek::*;

use super::ToValue;

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for Vec2<T> {
    fn to_value<'a>(&'a self, meta: &'a M) -> rsn_parser::value::Value<'a, C>
    where
        C: 'a,
    {
        let x = &self.x;
        let y = &self.y;

        rsn! {@(meta) =>
            (#x, #y)
        }
    }
}

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for Vec3<T> {
    fn to_value<'a>(&'a self, meta: &'a M) -> rsn_parser::value::Value<'a, C>
    where
        C: 'a,
    {
        let x = &self.x;
        let y = &self.y;
        let z = &self.z;

        rsn! {@(meta) =>
            (#x, #y, #z)
        }
    }
}

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for Vec4<T> {
    fn to_value<'a>(&'a self, meta: &'a M) -> rsn_parser::value::Value<'a, C>
    where
        C: 'a,
    {
        let x = &self.x;
        let y = &self.y;
        let z = &self.z;
        let w = &self.w;

        rsn! {@(meta) =>
            (#x, #y, #z, #w)
        }
    }
}

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for Aabr<T> {
    fn to_value<'a>(&'a self, meta: &'a M) -> rsn_parser::value::Value<'a, C>
    where
        C: 'a,
    {
        let min = &self.min;
        let max = &self.max;

        rsn! {@(meta) =>
            #min..=#max
        }
    }
}

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for Aabb<T> {
    fn to_value<'a>(&'a self, meta: &'a M) -> rsn_parser::value::Value<'a, C>
    where
        C: 'a,
    {
        let min = &self.min;
        let max = &self.max;

        rsn! {@(meta) =>
            #min..=#max
        }
    }
}

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for Rgb<T> {
    fn to_value<'a>(&'a self, meta: &'a M) -> rsn_parser::value::Value<'a, C>
    where
        C: 'a,
    {
        let r = &self.r;
        let g = &self.g;
        let b = &self.b;

        rsn! {@(meta) =>
            (#r, #g, #b)
        }
    }
}

impl<M, C, T: ToValue<M, C>> ToValue<M, C> for Rgba<T> {
    fn to_value<'a>(&'a self, meta: &'a M) -> rsn_parser::value::Value<'a, C>
    where
        C: 'a,
    {
        let r = &self.r;
        let g = &self.g;
        let b = &self.b;
        let a = &self.a;

        rsn! {@(meta) =>
            (#r, #g, #b, #a)
        }
    }
}
