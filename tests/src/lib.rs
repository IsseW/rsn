#[cfg(test)]
mod tests;

struct Meta<'a> {
    test: u32,
    b: &'a u32,
}

#[derive(rsn::FromValue)]
#[rsn(with_meta = Meta<'_>)]
struct Test {
    a: u32,
    #[rsn(with_expr = meta.test)]
    b: u32,
    c: u32,
}
