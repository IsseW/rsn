pub trait IsDefault {
    fn is_default(&self) -> bool;
}

impl<T> IsDefault for T {
    default fn is_default(&self) -> bool {
        false
    }
}

impl<T: PartialEq + Default> IsDefault for T {
    fn is_default(&self) -> bool {
        T::default() == *self
    }
}
