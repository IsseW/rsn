use proc_macro2::TokenStream;
use quote::quote;
use syn::spanned::Spanned;

use crate::util::*;

fn fields_named(
    fields: &ValueNamedFields,
    generate_write_trait: impl FnOnce(&TokenStream) -> Option<TokenStream>,
    meta_type: &syn::Type,
    custom_type: &syn::Type,
) -> TokenStream {
    let write_fields = fields.iter().map(|(attrs, ident, ty)| {
            let ident_str = ident.to_string();
            match attrs.modifier {
                None => {
                    quote! {
                        fields.insert(
                            __rsn::Spanned::create(#ident_str),
                            <#ty as __rsn::ToValue<#meta_type, #custom_type>>::to_value(&self.#ident, meta),
                        );
                    }
                }
                Some(FieldModifier::Flatten) => {
                    quote! {
                        <#ty as __rsn::WriteNamedFields<#meta_type, #custom_type>>::write_fields(&self.#ident, fields, meta);
                    }
                }
                Some(FieldModifier::Default) => {
                    quote! {
                        if !__rsn::IsDefault::is_default(&self.#ident) {
                            fields.insert(
                                __rsn::Spanned::create(#ident_str),
                                <#ty as __rsn::ToValue<#meta_type, #custom_type>>::to_value(&self.#ident, meta),
                            )
                        }
                    }
                }
                Some(FieldModifier::Skip | FieldModifier::WithExpr(..)) => quote!(),
            }
        }).fold(quote!(), |acc, field| {
            quote! {
                #acc
                #field
            }
        });

    let write_fields = if let Some(write_impl) = generate_write_trait(&write_fields) {
        quote! {
            #write_impl

            <Self as __rsn::WriteNamedFields<#meta_type, #custom_type>>::write_fields(self, fields, meta)
        }
    } else {
        write_fields
    };

    quote! {
        {
            let mut fields_map = __rsn::Fields::with_capacity(<Self as __rsn::UnnamedFields>::MIN_FIELDS);
            let fields = &mut fields_vec;

            #write_fields

            fields_map
        }
    }
}

fn fields_unnamed(
    fields: &ValueUnnamedFields,
    generate_write_trait: impl FnOnce(&TokenStream) -> Option<TokenStream>,
    meta_type: &syn::Type,
    custom_type: &syn::Type,
) -> TokenStream {
    let mut write_fields = quote! {};
    let mut iter = fields.iter().enumerate();
    for (i, (attrs, ty)) in &mut iter {
        let write = match &attrs.modifier {
            None => {
                quote! {
                    fields.push(<#ty as __rsn::ToValue<#meta_type, #custom_type>>::to_value(&self.#i, meta))
                }
            }
            Some(FieldModifier::Flatten) => {
                quote! {
                    <#ty as __rsn::WriteUnnamedFields<#meta_type, #custom_type>>::write_fields(&self.#i, fields, false, meta);
                }
            }
            Some(FieldModifier::Default) => {
                let mut write = quote! {};
                // TODO: We should check if ANY  aren't default and write up to what isn't default...
                for (i, (attrs, ty)) in iter.rev() {
                    match attrs.modifier {
                        Some(FieldModifier::Default) => {
                            write = quote! {
                                if write_default && !__rsn::IsDefault::is_default(&self.#i) {
                                    fields.push(
                                        <#ty as __rsn::ToValue<#meta_type, #custom_type>>::to_value(&self.#i, meta),
                                    );
                                    #write
                                }
                            }
                        }
                        Some(FieldModifier::Skip | FieldModifier::WithExpr(..)) => {}
                        _ => unreachable!(),
                    }
                }

                write = quote! {
                    if write_default && !__rsn::IsDefault::is_default(&self.#i) {
                        fields.push(
                            <#ty as __rsn::ToValue<#meta_type, #custom_type>>::to_value(&self.#i, meta),
                        );
                        #write
                    }
                };

                write_fields = quote! {
                    #write_fields
                    #write
                };
                break;
            }
            Some(FieldModifier::Skip | FieldModifier::WithExpr(..)) => quote! {},
        };

        write_fields = quote! {
            #write_fields
            #write
        };
    }

    let write_fields = if let Some(write_impl) = generate_write_trait(&write_fields) {
        quote! {
            #write_impl

            <Self as __rsn::WriteUnnamedFields<#meta_type, #custom_type>>::write_fields(self, fields, write_default, meta)
        }
    } else {
        write_fields
    };

    quote! {
        {
            let mut fields_vec = ::std::vec::Vec::with_capacity(<Self as __rsn::UnnamedFields>::MIN_FIELDS);
            let fields = &mut fields_vec;

            #write_fields

            fields_vec
        }
    }
}

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

    let value = match data {
        ValueData::Struct(fields) => match fields {
            ValueFields::Unit => {
                if attrs.untagged.is_some() {
                    quote! {
                        __rsn::ValueKind::Tuple(::std::vec::Vec::new())
                    }
                } else {
                    quote! {
                        __rsn::ValueKind::Path(__rsn::value::Path {
                            leading: false,
                            idents: ::std::vec![__rsn::Spanned::create(#ident_str)],
                        })
                    }
                }
            }
            ValueFields::Unnamed(fields) => {
                let fields = fields_unnamed(
                    &fields,
                    |write_fields| {
                        Some(quote! {
                            impl #modified_generics __rsn::WriteUnnamedFields<#meta_type, #custom_type> for #real_ident #generics #where_clause {
                                fn write_fields<'a>(&'a self, fields: &mut ::std::vec::Vec<__rsn::Value<'a, #custom_type>, write_default: bool, meta: &#meta_type) {
                                    #write_fields
                                }
                            }
                        })
                    },
                    &meta_type,
                    &custom_type,
                );

                quote! {
                    __rsn::ValueKind::
                }
            }
            ValueFields::Named(_) => todo!(),
        },
        ValueData::Enum(_) => todo!(),
    };

    Ok(quote! {
        #extra_impl

        #[automatically_derived]
        impl #modified_generics __rsn::ToValue<#meta_type, #custom_type> for #real_ident #generics #where_clause {
            fn to_value(&self, meta: &#meta_type) -> __rsn::Value<#custom_type> {
                let write_default = true;
                let value = #value;

                __rsn::Value::create(value)
            }
        }
    })
}
