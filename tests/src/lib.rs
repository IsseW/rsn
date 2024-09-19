#![feature(never_type)]

#[cfg(test)]
mod tests;

#[derive(rsn::FromValue)]
struct A {
    #[rsn(default)]
    a: u32,
    #[rsn(default)]
    b: u32,
}

#[derive(rsn::FromValue)]
enum B {
    T {
        #[rsn(flatten)]
        a: A,
    },
}
