#![feature(never_type)]

#[cfg(test)]
mod tests;

#[derive(rsn::FromValue, rsn::ToValue)]
struct Test {
    #[rsn(with_serde)]
    a: u32,
}
