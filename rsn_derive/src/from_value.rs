use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::fmt::Write;
use syn::{punctuated::Punctuated, spanned::Spanned};

use crate::util::*;

fn named_help(fields: &ValueNamedFields, path: &str) -> syn::Result<String> {
    let mut msg = String::new();
    let _ = write!(msg, "{} {{", path);

    for (attrs, ident, _) in fields.iter() {
        match attrs.modifier {
            Some(FieldModifier::Default) => {
                write!(msg, "\n\t{}: <value>, <optional>", ident).unwrap();
            }
            None => {
                write!(msg, "\n\t{}: <value>,", ident).unwrap();
            }
            Some(FieldModifier::Flatten) => {
                write!(msg, "\n\t{}: <value>, <flattened>", ident).unwrap();
            }
            Some(FieldModifier::Skip | FieldModifier::WithExpr(_, _)) => {}
        }
    }
    let _ = write!(msg, "\n}}");

    Ok(msg)
}

fn unnamed_help(fields: &ValueUnnamedFields, path: &str) -> syn::Result<String> {
    let mut msg = String::new();
    let _ = write!(msg, "{} (", path);

    for (attrs, _) in fields.iter() {
        match attrs.modifier {
            None => {
                write!(msg, "\n\t<value>,").unwrap();
            }
            Some(FieldModifier::Flatten) => {
                write!(msg, "\n\t<value>, <flattened>").unwrap();
            }
            Some(FieldModifier::Default) => {
                write!(msg, "\n\t<value>, <optional>").unwrap();
            }
            Some(FieldModifier::Skip | FieldModifier::WithExpr(_, _)) => {}
        }
    }
    let _ = write!(msg, "\n)");

    Ok(msg)
}

fn construct_fields_named<'a>(
    fields: &'a ValueNamedFields,
    self_repr: TokenStream,
    mut handle_field: impl FnMut(&syn::Ident, &'a syn::Type, &Option<FieldModifier>),
) -> syn::Result<TokenStream> {
    let error = quote!(__rsn::FromValueErrorKind);
    let field_iter: Vec<_> = fields
        .iter()
        .map(|(attrs, real_ident, ty)| {
            let ident = attrs.rename.as_ref().unwrap_or(real_ident);
            let ident_str = ident.to_string();
            let res = match &attrs.modifier {
                None => {
                    quote! {
                        #real_ident: __rsn::FromValue::from_value(
                            fields.swap_remove(#ident_str).ok_or(__rsn::FromValueError::new(span, #error::MissingField(#ident_str)))?,
                            meta,
                        )?
                    }
                },
                Some(FieldModifier::Flatten) => {
                    // Assumes that this field implements `NamedFields`
                    quote!(
                        #real_ident: __rsn::ParseNamedFields::parse_fields(span, fields, meta)?
                    )
                },
                Some(FieldModifier::Default) => {
                    quote! {
                        #real_ident: if let Some(value) = fields.swap_remove(#ident_str) {
                            __rsn::FromValue::from_value(value, meta)?
                        } else {
                            core::default::Default::default()
                        }
                    }
                },
                Some(FieldModifier::Skip) => {
                    quote! {
                        #real_ident: core::default::Default::default()
                    }
                },
                Some(FieldModifier::WithExpr(_, expr)) => {
                    quote! {
                        #real_ident: #expr
                    }
                }
            };
            handle_field(ident, ty, &attrs.modifier);
            syn::Result::Ok(res)
        })
        .try_collect()?;
    Ok(quote!(
        Ok(#self_repr {
            #(#field_iter),*
        })
    ))
}

fn construct_fields_unnamed<'a>(
    fields: &'a ValueUnnamedFields,
    self_repr: TokenStream,
    mut handle_field: impl FnMut(&'a syn::Type, &Option<FieldModifier>),
) -> syn::Result<TokenStream> {
    let field_iter: Vec<_> = fields
        .iter()
        .map(|(attrs, ty)| {
            let res = match &attrs.modifier {
                None => {
                    quote!(__rsn::FromValue::from_value(iter.next().unwrap(), meta)?)
                }
                Some(FieldModifier::Flatten) => quote!(__rsn::ParseUnnamedFields::parse_fields(
                    span,
                    iter.next().unwrap(),
                    false,
                    meta
                )),
                Some(FieldModifier::Default) => {
                    quote!(if parse_optional {
                        iter.next()
                            .map(|v| __rsn::FromValue::from_value(v, meta).ok())
                            .flatten()
                    } else {
                        None
                    }
                    .unwrap_or_default())
                }
                Some(FieldModifier::Skip) => quote!(core::default::Default::default()),
                Some(FieldModifier::WithExpr(_, expr)) => quote!(#expr),
            };
            handle_field(ty, &attrs.modifier);
            syn::Result::Ok(res)
        })
        .try_collect()?;
    Ok(quote!(
        Ok(#self_repr(#(#field_iter),*))
    ))
}

fn fields_named(
    fields: &ValueNamedFields,
    check_path: Option<TokenStream>,
    self_repr: TokenStream,
    generate_parse_trait: impl FnOnce(&TokenStream) -> Option<TokenStream>,
    meta_type: &syn::Type,
    custom_type: &syn::Type,
) -> syn::Result<TokenStream> {
    let error = quote!(__rsn::FromValueErrorKind);

    let mut min_fields: usize = 0;
    let mut max_fields: usize = 0;

    let mut optional_field_strs = Vec::new();
    let mut needed_field_strs = Vec::new();

    let mut flattened = Vec::new();

    let mut fields_default = Vec::new();

    let field_parse =
        construct_fields_named(fields, self_repr, |ident, ty, modifier| match modifier {
            None => {
                fields_default.push(quote!(
                    #ident: unreachable!()
                ));
                needed_field_strs.push(ident.to_string());
                min_fields += 1;
                max_fields += 1;
            }
            Some(FieldModifier::Flatten) => {
                fields_default.push(quote!(
                    #ident: unreachable!()
                ));
                flattened.push(ty);
            }
            Some(FieldModifier::Default) => {
                fields_default.push(quote!(
                    #ident: core::default::Default::default()
                ));
                optional_field_strs.push(ident.to_string());
                max_fields += 1;
            }
            Some(FieldModifier::Skip) => {
                fields_default.push(quote!(
                    #ident: core::default::Default::default()
                ));
            }
            Some(FieldModifier::WithExpr(_, expr)) => {
                fields_default.push(quote!(#ident: #expr));
            }
        })?;

    let field_parse = if let Some(parse_impl) = generate_parse_trait(&field_parse) {
        quote! {
            #parse_impl

            <Self as __rsn::ParseNamedFields<#meta_type, #custom_type>>::parse_fields(span, fields, meta)
        }
    } else {
        field_parse
    };

    let min_fields = quote!(
        (#min_fields #(+ <#flattened as __rsn::NamedFields>::MIN_FIELDS)*)
    );
    let max_fields = quote!(
        (#max_fields #(+ <#flattened as __rsn::NamedFields>::MAX_FIELDS)*)
    );

    let check_len = quote!(
        if (#min_fields..=#max_fields).contains(&fields.len())
    );

    let field_parse = quote! {
        #check_len {
            let res = {
                let fields = &mut fields;
                #field_parse
            };
            if let Some((ident, _)) = fields.into_iter().next() {
                core::result::Result::Err(__rsn::FromValueError::new(ident.span, #error::UnexpectedIdent))
            } else {
                res
            }
        } else {
            #(
                for field in <#flattened as __rsn::NamedFields>::REQUIRED_FIELDS {
                    fields.swap_remove(*field).ok_or(__rsn::FromValueError::new(span, #error::MissingField(field)))?;
                }
            )*
            #(
                fields.swap_remove(#needed_field_strs).ok_or(__rsn::FromValueError::new(span, #error::MissingField(#needed_field_strs)))?;
            )*
            #(
                for field in <#flattened as __rsn::NamedFields>::OPTIONAL_FIELDS {
                    fields.swap_remove(*field);
                }
            )*
            #(
                fields.swap_remove(#optional_field_strs);
            )*
            let (ident, _) = fields.into_iter().next().unwrap();

            core::result::Result::Err(__rsn::FromValueError::new(ident.span, #error::UnexpectedIdent))
        }
    };

    let all_default = quote!(
        #[allow(unreachable_code)]
        Ok(Self {
            #(#fields_default),*
        })
    );

    let expr = if let Some(check_path) = check_path {
        quote!(
            __rsn::ValueKind::NamedStruct(path, mut fields) if #check_path => {
                #field_parse
            },
            __rsn::ValueKind::Path(path) if #check_path && #min_fields == 0 => {
                #all_default
            },
        )
    } else {
        quote!(
            __rsn::ValueKind::Struct(mut fields) => {
                #field_parse
            },
            __rsn::ValueKind::Map(fields) if #min_fields == 0 && fields.len() == 0 => {
                #all_default
            },
        )
    };

    Ok(expr)
}

fn fields_unnamed(
    fields: &ValueUnnamedFields,
    check_path: Option<TokenStream>,
    self_repr: TokenStream,
    generate_parse_trait: impl FnOnce(&TokenStream) -> Option<TokenStream>,
    meta_type: &syn::Type,
    custom_type: &syn::Type,
) -> syn::Result<TokenStream> {
    let error = quote!(__rsn::FromValueErrorKind);

    let mut required_fields: usize = 0;
    let mut optional_fields: usize = 0;

    let mut flattened = Vec::new();

    let parse_fields =
        construct_fields_unnamed(fields, self_repr, |ty, modifier| match modifier {
            None => {
                required_fields += 1;
            }
            Some(FieldModifier::Flatten) => {
                flattened.push(ty);
            }
            Some(FieldModifier::Default) => {
                optional_fields += 1;
            }
            Some(FieldModifier::Skip | FieldModifier::WithExpr(..)) => {}
        })?;

    let (parse_impl, parse_fields) = if let Some(parse_impl) = generate_parse_trait(&parse_fields) {
        (
            parse_impl,
            quote! {
                <Self as __rsn::ParseUnnamedFields<#meta_type, #custom_type>>::parse_fields(span, &mut iter, true, meta)
            },
        )
    } else {
        (quote!(), parse_fields)
    };

    let required_fields = quote!(
        (#required_fields #(+ <<#flattened> as __rsn::UnnamedFields>::MIN_FIELDS)*)
    );

    let parse_fields2 = quote!(
        #parse_impl
        if fields.len() >= #required_fields && fields.len() <= #required_fields + #optional_fields {
            let mut iter = fields.into_iter();
            #parse_fields
        } else {
            core::result::Result::Err(__rsn::FromValueError::new(span, #error::ExpectedAmountOfElements(#required_fields..=#required_fields + #optional_fields)))
        }
    );

    let expr = if let Some(check_path) = check_path {
        quote!(
            __rsn::ValueKind::NamedTuple(path, mut fields) if #check_path => {
                #parse_fields2
            }
            __rsn::ValueKind::Path(path) if #check_path && #required_fields == 0 => {
                let mut iter = core::iter::empty();
                #parse_fields
            }
        )
    } else {
        quote!(
            __rsn::ValueKind::Tuple(mut fields) => {
                #parse_fields2
            }
            value if #required_fields == 1 => {
                let mut iter = core::iter::once(__rsn::Spanned::new(span, value));
                #parse_fields
            }
        )
    };

    Ok(expr)
}

pub fn from_value(
    ValueDeriveInput {
        attrs,
        data,
        ident,
        real_ident,
        ident_str,
        generics,
        modified_generics,
        mut where_clause,
        meta_type,
        custom_type,
    }: ValueDeriveInput,
) -> syn::Result<TokenStream> {
    let error = quote!(__rsn::FromValueErrorKind);
    let mut added_clone = false;

    let parse = match data {
        ValueData::Struct(fields) => {
            let check_path = attrs.untagged.is_none().then(|| {
                quote!(path.is_struct(std::concat!(std::module_path!(), "::", #ident_str)))
            });
            match fields {
                ValueFields::Named(fields) => {
                    let reprs = fields_named(
                        &fields,
                        check_path,
                        quote!(Self),
                        |field_parse| {
                            Some(quote!(
                                #[automatically_derived]
                                impl #modified_generics  __rsn::ParseNamedFields<#meta_type, #custom_type> for #ident #generics #where_clause {
                                    fn parse_fields(span: __rsn::Span, fields: &mut __rsn::Fields<#custom_type>, meta: &mut #meta_type) -> ::core::result::Result<Self, __rsn::FromValueError> {
                                        #field_parse
                                    }
                                }
                            ))
                        },
                        &meta_type,
                        &custom_type,
                    )?;
                    let help_msg = named_help(&fields, &ident_str)?;

                    quote!(
                        match value.inner() {
                            #reprs
                            value => core::result::Result::Err(__rsn::FromValueError::new(span, #error::ExpectedPattern(&[#help_msg]))),
                        }
                    )
                }
                ValueFields::Unnamed(fields) => {
                    let reprs = fields_unnamed(
                        &fields,
                        check_path,
                        quote!(Self),
                        |parse_fields| {
                            Some(quote!(
                                #[automatically_derived]
                                impl #modified_generics __rsn::ParseUnnamedFields<#meta_type, #custom_type> for #ident #generics #where_clause {
                                    fn parse_fields<'a, I: Iterator<Item = __rsn::Value<'a, #custom_type>>>(struct_span: __rsn::Span, iter: &mut I, parse_optional: bool, meta: &mut #meta_type) -> Result<Self, __rsn::FromValueError> {
                                        #parse_fields
                                    }
                                }
                            ))
                        },
                        &meta_type,
                        &custom_type,
                    )?;
                    let help_msg = unnamed_help(&fields, &ident_str)?;
                    quote!(
                        match value.inner() {
                            #reprs
                            value => core::result::Result::Err(__rsn::FromValueError::new(span, #error::ExpectedPattern(&[#help_msg]))),
                        }
                    )
                }
                ValueFields::Unit => {
                    quote!(
                        match value.inner() {
                            __rsn::ValueKind::Path(path) if #check_path => core::result::Result::Ok(Self),
                            value => core::result::Result::Err(__rsn::FromValueError::new(span, #error::ExpectedIdent(#ident_str))),
                        }
                    )
                }
            }
        }
        ValueData::Enum(variants) => {
            let field_cases = variants.iter().map(|(variant_attrs, variant_ident, fields)| {
                if let Some(ty) = variant_attrs.with_meta.as_ref() {
                    return Err(syn::Error::new(ty.span(), "with_meta is not supported on variants"));
                }
                let variant_str = variant_attrs.rename.as_ref().map_or(variant_ident.to_string(), |ident| ident.to_string());
                let untagged = attrs.untagged.is_some();
                let check_path = if variant_attrs.untagged.is_some() && attrs.untagged.is_some() {
                    None
                } else if variant_attrs.untagged.is_some() {
                    Some(quote!(
                        path.is_enum::<#untagged>(std::concat!(std::module_path!(), "::", #ident_str))
                    ))
                } else {
                    Some(quote!(
                        path.is_enum::<#untagged>(std::concat!(std::module_path!(), "::", #ident_str, "::", #variant_str))
                    ))
                };

                match fields {
                    ValueFields::Named(fields) => {
                        fields_named(fields, check_path, quote!(Self::#variant_ident), |_| None, &meta_type, &custom_type)
                    }
                    ValueFields::Unnamed(fields) => {
                        fields_unnamed(fields, check_path, quote!(Self::#variant_ident), |_| None, &meta_type, &custom_type)
                    }
                    ValueFields::Unit => if let Some(check_path) = check_path {
                        Ok(quote!(
                        __rsn::ValueKind::Path(path) if #check_path => Ok(Self::#variant_ident),
                        ))
                    } else {
                        Ok(quote!(
                            __rsn::ValueKind::Tuple(fields) if fields.len() == 0 => Ok(Self::#variant_ident),
                        ))
                    }
                }
            }).try_collect::<Vec<_>>()?;
            let help_msgs = variants
                .iter()
                .try_fold(quote!(), |acc, (attrs, ident, fields)| {
                    let path = if attrs.untagged.is_some() {
                        ident.to_string()
                    } else {
                        format!("{ident_str}::{}", ident)
                    };
                    let help_msg = match fields {
                        ValueFields::Named(fields) => named_help(fields, &path)?,
                        ValueFields::Unnamed(fields) => unnamed_help(fields, &path)?,
                        ValueFields::Unit => path,
                    };
                    syn::Result::Ok(quote!(
                        #acc
                        #help_msg,
                    ))
                })?;

            if let Some(untagged) = &attrs.untagged {
                let start = quote!(
                    let patterns = &[#help_msgs];
                    core::result::Result::Err(())
                );
                field_cases.into_iter().fold(start, |acc, cases| {
                    if !added_clone {
                        added_clone = true;
                        let where_clause = where_clause.get_or_insert(syn::WhereClause { where_token: syn::Token![where](untagged.span()), predicates: Punctuated::default() });

                        where_clause.predicates.push(syn::WherePredicate::Type(syn::PredicateType {
                            lifetimes: None,
                            bounded_ty: custom_type.clone(),
                            colon_token: syn::Token![:](untagged.span()),
                            bounds: Punctuated::from_iter([syn::TypeParamBound::Trait(syn::TraitBound { paren_token: None, modifier: syn::TraitBoundModifier::None, lifetimes: None, path: syn::Path {
                                leading_colon: Some(syn::Token![::](untagged.span())),
                                segments: Punctuated::from_iter([
                                    syn::PathSegment::from(format_ident!("core")),
                                    syn::PathSegment::from(format_ident!("clone")),
                                    syn::PathSegment::from(format_ident!("Clone")),
                                ]),
                            }  })]),
                        }));
                    }
                    quote!(
                        let parse_optional = true;
                        #acc.or_else(|_| match value.clone().inner() {
                            #cases
                            value => core::result::Result::Err(__rsn::FromValueError::new(span, #error::ExpectedPattern(patterns))),
                        })
                    )
                })
            } else {
                quote!(
                    match value.inner() {
                        #(#field_cases)*
                        value => core::result::Result::Err(__rsn::FromValueError::new(span, #error::ExpectedPattern(&[#help_msgs]))),
                    }
                )
            }
        }
    };

    Ok(quote! {
         #[automatically_derived]
         impl #modified_generics __rsn::FromValue<#meta_type, #custom_type> for #real_ident #generics #where_clause {
             fn from_value(value: __rsn::Value<#custom_type>, meta: &mut #meta_type) -> ::core::result::Result<Self, __rsn::FromValueError> {
                 let span = value.span;
                 #parse
             }
         }
    })
}
