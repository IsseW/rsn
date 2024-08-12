use proc_macro2::TokenStream;
use quote::{format_ident, quote};

use crate::{
    fields::{NamedFieldsInfo, UnnamedFieldsInfo},
    util::*,
};

struct FieldsRes {
    pattern: TokenStream,
    fields: TokenStream,
}

fn fields_named(
    fields: &ValueNamedFields,
    generate_write_trait: impl FnOnce(&TokenStream, &TokenStream) -> Option<TokenStream>,
    min_fields: TokenStream,
    meta_type: &syn::Type,
    custom_type: &syn::Type,
) -> FieldsRes {
    let mut pattern = quote!();
    let write_fields = fields.iter().map(|(attrs, ident, ty)| {
            pattern = quote! {
                #pattern
                #ident,
            };
            let ident_str = ident.to_string();
            match attrs.modifier {
                None => {
                    quote! {
                        fields.insert(
                            __rsn::Spanned::create(#ident_str),
                            <#ty as __rsn::ToValue<#meta_type, #custom_type>>::to_value(#ident, meta),
                        );
                    }
                }
                Some(FieldModifier::Flatten) => {
                    quote! {
                        <#ty as __rsn::WriteNamedFields<#meta_type, #custom_type>>::write_fields(#ident, fields, meta);
                    }
                }
                Some(FieldModifier::Default) => {
                    quote! {
                        if !__rsn::IsDefault::is_default(&self.#ident) {
                            fields.insert(
                                __rsn::Spanned::create(#ident_str),
                                <#ty as __rsn::ToValue<#meta_type, #custom_type>>::to_value(#ident, meta),
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

    let pattern = quote! {
        {
            #pattern
        }
    };

    let write_fields = if let Some(write_impl) = generate_write_trait(&write_fields, &pattern) {
        quote! {
            #write_impl

            <Self as __rsn::WriteNamedFields<#meta_type, #custom_type>>::write_fields(self, fields, meta);
        }
    } else {
        write_fields
    };

    let fields = quote! {
        {
            let mut fields_map = __rsn::Fields::with_capacity(#min_fields);
            let fields = &mut fields_map;

            #write_fields

            fields_map
        }
    };

    FieldsRes { pattern, fields }
}

fn fields_unnamed(
    fields: &ValueUnnamedFields,
    generate_write_trait: impl FnOnce(&TokenStream, &TokenStream) -> Option<TokenStream>,
    min_fields: TokenStream,
    meta_type: &syn::Type,
    custom_type: &syn::Type,
) -> FieldsRes {
    let mut pattern = quote!();
    let mut write_fields = quote!();
    for (i, (attrs, ty)) in fields.iter().enumerate() {
        let ident = format_ident!("field{i}");
        pattern = quote! {
            #pattern
            #ident,
        };
        let write = match &attrs.modifier {
            None => {
                quote! {
                    fields.push(<#ty as __rsn::ToValue<#meta_type, #custom_type>>::to_value(#ident, meta));
                }
            }
            Some(FieldModifier::Flatten) => {
                quote! {
                    <#ty as __rsn::WriteUnnamedFields<#meta_type, #custom_type>>::write_fields(#ident, fields, false, meta);
                }
            }
            Some(FieldModifier::Default) => {
                let mut write = None;
                for (j, (attrs, _)) in fields.iter().enumerate().skip(i).rev() {
                    match attrs.modifier {
                        Some(FieldModifier::Default) => {
                            let mut statement = quote!();
                            for (k, (attrs, ty)) in fields.iter().enumerate().take(j + 1).skip(i) {
                                match attrs.modifier {
                                    Some(FieldModifier::Default) => {
                                        let ident = format_ident!("field{k}");
                                        statement = quote! {
                                            #statement
                                            fields.push(
                                                <#ty as __rsn::ToValue<#meta_type, #custom_type>>::to_value(#ident, meta),
                                            );
                                        };
                                    }
                                    Some(FieldModifier::Skip | FieldModifier::WithExpr(..)) => {}
                                    _ => panic!("We shouldn't have any non-default fields here"),
                                }
                            }
                            let ident = format_ident!("field{j}");
                            statement = quote! {
                                if !__rsn::IsDefault::is_default(#ident) {
                                    #statement
                                }
                            };

                            if let Some(write) = write.as_mut() {
                                *write = quote! {
                                    #write else #statement
                                }
                            } else {
                                write = Some(statement)
                            }
                        }
                        Some(FieldModifier::Skip | FieldModifier::WithExpr(..)) => {}
                        _ => panic!("We shouldn't have any non-default fields here"),
                    }
                }

                write_fields = quote! {
                    #write_fields
                    if write_default {
                        #write
                    }
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

    let pattern = quote! {
        (#pattern)
    };

    let write_fields = if let Some(write_impl) = generate_write_trait(&write_fields, &pattern) {
        quote! {
            #write_impl

            <Self as __rsn::WriteUnnamedFields<#meta_type, #custom_type>>::write_fields(self, fields, write_default, meta)
        }
    } else {
        write_fields
    };

    let fields = quote! {
        {
            let mut fields_vec = ::std::vec::Vec::with_capacity(#min_fields);
            let fields = &mut fields_vec;

            #write_fields

            fields_vec
        }
    };

    FieldsRes { pattern, fields }
}

fn ident_path(ident: &[&str]) -> TokenStream {
    quote! {
        __rsn::Path {
            leading: false,
            idents: std::vec![#(__rsn::Spanned::create(#ident)),*],
        }
    }
}

pub fn to_value(
    ValueDeriveInput {
        attrs,
        data,
        real_ident,
        ident_str,
        generics,
        modified_generics,
        where_clause,
        meta_type,
        custom_type,
    }: ValueDeriveInput,
) -> syn::Result<TokenStream> {
    let mut checks = quote!();
    let path = ident_path(&[&ident_str]);
    let value = match data {
        ValueData::Struct(fields) => match fields {
            ValueFields::Unit => {
                if attrs.untagged.is_some() {
                    quote! {
                        __rsn::ValueKind::Tuple(::std::vec::Vec::new())
                    }
                } else {
                    quote! {
                        __rsn::ValueKind::Path(#path)
                    }
                }
            }
            ValueFields::Unnamed(fields) => {
                let FieldsRes { pattern, fields } = fields_unnamed(
                    &fields,
                    |write_fields, pattern| {
                        Some(quote! {
                            #[automatically_derived]
                            impl #modified_generics __rsn::WriteUnnamedFields<#meta_type, #custom_type> for #real_ident #generics #where_clause {
                                fn write_fields<'a>(&'a self, fields: &mut ::std::vec::Vec<__rsn::Value<'a, #custom_type>, write_default: bool, meta: &'a #meta_type)
                                    where #custom_type: 'a
                                {
                                    let #real_ident #pattern = self;
                                    #write_fields
                                }
                            }
                        })
                    },
                    quote!(<Self as __rsn::UnnamedFields>::MIN_FIELDS),
                    &meta_type,
                    &custom_type,
                );

                if attrs.untagged.is_some() {
                    quote! {
                        {
                            let #real_ident #pattern = self;
                            __rsn::ValueKind::Tuple(#fields)
                        }
                    }
                } else {
                    quote! {
                        {
                            let #real_ident #pattern = self;
                            __rsn::ValueKind::NamedTuple(__rsn::Spanned::create(#path), #fields)
                        }
                    }
                }
            }
            ValueFields::Named(fields) => {
                let FieldsRes { pattern, fields } = fields_named(
                    &fields,
                    |write_fields, pattern| {
                        Some(quote! {
                            #[automatically_derived]
                            impl #modified_generics __rsn::WriteNamedFields<#meta_type, #custom_type> for #real_ident #generics #where_clause {
                                fn write_fields<'a>(&'a self, fields: &mut __rsn::Fields<'a, #custom_type>, meta: &'a #meta_type)
                                    where #custom_type: 'a
                                {
                                    let #real_ident #pattern = self;
                                    #write_fields
                                }
                            }
                        })
                    },
                    quote!(<Self as __rsn::NamedFields>::MIN_FIELDS),
                    &meta_type,
                    &custom_type,
                );

                if attrs.untagged.is_some() {
                    quote! {
                        {
                            let #real_ident #pattern = self;
                            __rsn::ValueKind::Struct(#fields)
                        }
                    }
                } else {
                    quote! {
                        {
                            let #real_ident #pattern = self;
                            __rsn::ValueKind::NamedStruct(__rsn::Spanned::create(#path), #fields)
                        }
                    }
                }
            }
        },
        ValueData::Enum(variants) => {
            let variants = variants
                .into_iter()
                .map(|(variant_attrs, variant_ident, fields)| {
                    let variant_ident_str = variant_attrs
                        .rename
                        .as_ref()
                        .unwrap_or(&variant_ident)
                        .to_string();

                    let path = match (attrs.untagged.is_some(), variant_attrs.untagged.is_some()) {
                        (false, false) => Some(ident_path(&[&ident_str, &variant_ident_str])),
                        (true, false) => Some(ident_path(&[&variant_ident_str])),
                        (false, true) => Some(path.clone()),
                        (true, true) => None,
                    };

                    let (pattern, fields) = match (fields, path) {
                        (ValueFields::Unit, None) => (quote!(), quote! {
                            __rsn::ValueKind::Tuple(::std::vec::Vec::new())
                        }),
                        (ValueFields::Unit, Some(path)) => (quote!(), quote! {
                            __rsn::ValueKind::Path(#path)
                        }),
                        (ValueFields::Unnamed(fields), path) => {
                            let info = UnnamedFieldsInfo::new(&fields)?;
                            let FieldsRes { pattern, fields } =
                                fields_unnamed(&fields, |_, _| None, info.required_fields(), &meta_type, &custom_type);

                            let fields = if let Some(path) = path {
                                quote! {
                                    __rsn::ValueKind::NamedTuple(__rsn::Spanned::create(#path), #fields)
                                }
                            } else {
                                quote! {
                                    __rsn::ValueKind::Tuple(#fields)
                                }
                            };

                            (pattern, fields)
                        }
                        (ValueFields::Named(fields), path) => {
                            let info = NamedFieldsInfo::new(&fields);
                            let set = info.required_set();
                            let error_msg = format!("{} contains many of the same rquired ident", variant_ident_str);
                            let check = quote! {
                                if !<#set as __rsn::__types::Set>::IS_VALID {
                                    panic!(#error_msg)
                                }
                            };

                            checks = quote! {
                                #checks
                                #check
                            };
                            let FieldsRes { pattern, fields } = fields_named(&fields, |_, _| None, info.min_fields(), &meta_type, &custom_type);

                            let fields = if let Some(path) = path {
                                quote! {
                                    __rsn::ValueKind::NamedStruct(__rsn::Spanned::create(#path), #fields)
                                }
                            } else {
                                quote! {
                                    __rsn::ValueKind::Struct(#fields)
                                }
                            };

                            (pattern, fields)
                        }
                    };
                    syn::Result::Ok(quote! {
                        Self::#variant_ident #pattern => {
                            #fields
                        }
                    })
                }).try_collect::<Vec<_>>()?;
            quote! {
                match self {
                    #(#variants)*
                }
            }
        }
    };

    Ok(quote! {
        #checks
        #[automatically_derived]
        impl #modified_generics __rsn::ToValue<#meta_type, #custom_type> for #real_ident #generics #where_clause {
            fn to_value<'a>(&'a self, meta: &'a #meta_type) -> __rsn::Value<'a, #custom_type> where #custom_type: 'a {
                let write_default = true;
                let value = #value;

                __rsn::Value::create(value)
            }
        }
    })
}
