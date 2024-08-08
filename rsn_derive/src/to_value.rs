use proc_macro2::TokenStream;
use quote::quote;
use syn::spanned::Spanned;

use crate::util::*;

pub fn to_value(
    ValueDeriveInput {
        attrs,
        data,
        ident,
        real_ident,
        ident_str,
        generics,
        modified_generics,
        where_clause,
        meta_type,
        custom_type,
    }: ValueDeriveInput,
) -> syn::Result<TokenStream> {
    let mut extra_impl = quote!();

    Ok(quote! {
        #extra_impl

        #[automatically_derived]
        impl #modified_generics __rsn::ToValue<#meta_type, #custom_type> for #real_ident #generics #where_clause {
            fn to_value(&self, meta: &#meta_type) -> __rsn::Value<#custom_type> {
                todo!()
            }
        }
    })
}
