#![feature(iterator_try_collect)]

use proc_macro::TokenStream;
use syn::{parse_macro_input, DeriveInput};

mod from_value;
mod type_set;

#[proc_macro_derive(FromValue, attributes(rsn))]
pub fn from_value(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let from_value = from_value::from_value(&input);

    match from_value {
        Ok(from_value) => from_value.into(),
        Err(err) => err.into_compile_error().into(),
    }
}
