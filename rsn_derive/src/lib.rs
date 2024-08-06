#![feature(iterator_try_collect, proc_macro_span)]

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

mod from_value;
mod rsn;
mod to_value;
mod type_set;
mod util;

#[proc_macro_derive(FromValue, attributes(rsn))]
pub fn from_value(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let from_value = from_value::from_value(&input);

    match from_value {
        Ok(from_value) => from_value.into(),
        Err(err) => err.into_compile_error().into(),
    }
}

#[proc_macro_derive(ToValue, attributes(rsn))]
pub fn to_value(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let from_value = to_value::to_value(&input);

    match from_value {
        Ok(from_value) => from_value.into(),
        Err(err) => err.into_compile_error().into(),
    }
}

#[proc_macro]
pub fn rsn(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as proc_macro2::TokenStream);
    let rsn = rsn::parse(&input);

    match rsn {
        Ok(rsn) => rsn.into(),
        Err(err) => err.into_compile_error().into(),
    }
}
