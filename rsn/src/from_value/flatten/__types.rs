use core::marker::PhantomData;

#[allow(dead_code)]
pub struct Char<const C: char>(u8);

pub enum Yes {}
pub enum No {}

pub trait Bool {
    const VALUE: bool;

    type SelectSet<A: Set, B: Set>: Set;
    type Or<Other: Bool>: Bool;
}

impl Bool for Yes {
    type Or<Other: Bool> = Yes;

    type SelectSet<A: Set, B: Set> = A;
    const VALUE: bool = true;
}

impl Bool for No {
    type Or<Other: Bool> = Other;
    type SelectSet<A: Set, B: Set> = B;

    const VALUE: bool = false;
}

pub struct TypeEq<A, B>(PhantomData<(A, B)>);

impl<A, B> Bool for TypeEq<A, B> {
    default type Or<Other: Bool> = Other;
    default type SelectSet<X: Set, Y: Set> = Y;

    default const VALUE: bool = false;
}

impl<T> Bool for TypeEq<T, T> {
    type Or<Other: Bool> = Yes;
    type SelectSet<A: Set, B: Set> = A;

    const VALUE: bool = true;
}

#[const_trait]
pub trait Ident {
    /// The length of the ident in bytes in utf8.
    const LEN: usize;
}

pub struct String<R: Ident, const C: char>(PhantomData<R>);

impl<const C: char> const Ident for Char<C> {
    const LEN: usize = C.len_utf8();
}

impl<R: Ident, const C: char> const Ident for String<R, C> {
    const LEN: usize = R::LEN + C.len_utf8();
}

pub trait Set: Sized {
    const IS_VALID: bool;
    const LEN: usize;
    type Contains<T>: Bool;
    type Overlaps<S: Set>: Bool;
    type InsertInto<S: Set>: Set;
}

pub struct EmptySet;

impl Set for EmptySet {
    const IS_VALID: bool = true;

    const LEN: usize = 0;

    type Contains<T> = No;

    type Overlaps<S: Set> = No;
    type InsertInto<S: Set> = S;
}

pub struct Type<T>(PhantomData<T>);

impl<T> Set for Type<T> {
    type Contains<U> = TypeEq<T, U>;
    type Overlaps<S: Set> = S::Contains<T>;
    type InsertInto<S: Set> = Insert<S, T>;

    const LEN: usize = 1;
    const IS_VALID: bool = true;
}

pub struct TypeSet<S: Set, T>(PhantomData<(S, T)>);

type Or<A, B> = <A as Bool>::Or<B>;

impl<S: Set, T> Set for TypeSet<S, T> {
    type Contains<U> = Or<S::Contains<U>, TypeEq<T, U>>;
    type Overlaps<V: Set> = Or<V::Contains<T>, S::Overlaps<V>>;
    type InsertInto<O: Set> = Insert<S::InsertInto<O>, T>;

    const LEN: usize = S::LEN + 1;

    const IS_VALID: bool = !S::Contains::<T>::VALUE;
}

pub struct TypeSetUnion<A: Set, B: Set>(PhantomData<(A, B)>);

impl<A: Set, B: Set> Set for TypeSetUnion<A, B> {
    type Contains<U> = Or<A::Contains<U>, B::Contains<U>>;
    type Overlaps<S: Set> = Or<A::Overlaps<S>, B::Overlaps<S>>;
    type InsertInto<O: Set> = <A::InsertInto<B> as Set>::InsertInto<O>;

    const LEN: usize = A::LEN + B::LEN;

    const IS_VALID: bool = !A::Overlaps::<B>::VALUE;
}

pub type Insert<A, B> = <<A as Set>::Contains<B> as Bool>::SelectSet<A, TypeSet<A, B>>;
pub type Union<A, B> = <A as Set>::InsertInto<B>;
