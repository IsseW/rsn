use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, DeriveInput};

mod from_value;

#[proc_macro_derive(FromValue, attributes(parse))]
pub fn from_value(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let from_value = from_value::from_value(&input);

    quote!(
        #from_value
    )
    .into()
}
