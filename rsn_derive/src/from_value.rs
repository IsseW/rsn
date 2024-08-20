use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::fmt::Write;
use syn::{punctuated::Punctuated, spanned::Spanned};

use crate::{
    fields::{NamedFieldsInfo, UnnamedFieldsInfo},
    util::*,
};

fn named_help(fields: &ValueNamedFields, path: &str) -> syn::Result<String> {
    let mut msg = String::new();
    let _ = write!(msg, "{} {{", path);

    for (attrs, ident, _) in fields.iter() {
        match attrs.modifier {
            Some(FieldModifier::Default) => {
                write!(msg, "\n\t{}: <value>, <optional>", ident).unwrap();
            }
            None | Some(FieldModifier::WithSerde) => {
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
            None | Some(FieldModifier::WithSerde) => {
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

struct ConstructNamed {
    required: TokenStream,
    optional: TokenStream,
}

fn construct_fields_named(
    fields: &ValueNamedFields,
    self_repr: TokenStream,
) -> syn::Result<ConstructNamed> {
    let error = quote!(__rsn::FromValueErrorKind);

    let mut post1 = quote!();
    let mut post2 = quote!();
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
                Some(FieldModifier::WithSerde) => {
                    quote! {
                        #real_ident: __rsn::Serde::<#ty>::from_value(
                            fields.swap_remove(#ident_str)
                                .ok_or(__rsn::FromValueError::new(span, #error::MissingField(#ident_str)))?,
                        )?
                    }
                },
                Some(FieldModifier::Flatten) => {
                    post2 = quote! {
                        #post2
                        __rsn::ParseNamedFields::parse_optional(&mut this.#real_ident, span, fields, meta)?;
                    };
                    // Assumes that this field implements `NamedFields`
                    quote!(
                        #real_ident: __rsn::ParseNamedFields::parse_required(span, fields, meta)?
                    )
                },
                Some(FieldModifier::Default) => {
                    post1 = quote! {
                        #post1
                        if let Some(value) = fields.swap_remove(#ident_str) {
                            this.#real_ident = __rsn::FromValue::from_value(value, meta)?;
                        }
                    };
                    quote! {
                        #real_ident: ::core::default::Default::default()
                    }
                },
                Some(FieldModifier::Skip) => {
                    quote! {
                        #real_ident: ::core::default::Default::default()
                    }
                },
                Some(FieldModifier::WithExpr(_, expr)) => {
                    quote! {
                        #real_ident: #expr
                    }
                }
            };
            syn::Result::Ok(res)
        })
        .try_collect()?;
    Ok(ConstructNamed {
        required: quote! {
            #self_repr {
                #(#field_iter,)*
            }
        },
        optional: quote! {
            #post1
            #post2
        },
    })
}

fn construct_fields_unnamed(
    fields: &ValueUnnamedFields,
    self_repr: TokenStream,
) -> syn::Result<TokenStream> {
    let field_iter: Vec<_> = fields
        .iter()
        .map(|(attrs, ty)| {
            let res = match &attrs.modifier {
                None => {
                    quote!(__rsn::FromValue::from_value(iter.next().unwrap(), meta)?)
                }
                Some(FieldModifier::WithSerde) => {
                    quote!(
                        __rsn::Serde::<#ty>::from_value(iter.next().unwrap())?
                    )
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
    generate_parse_trait: impl FnOnce(&TokenStream, &TokenStream) -> Option<TokenStream>,
    min_fields: TokenStream,
    meta_type: &syn::Type,
    custom_type: &syn::Type,
) -> syn::Result<TokenStream> {
    let error = quote!(__rsn::FromValueErrorKind);

    let ConstructNamed { required, optional } = construct_fields_named(fields, self_repr)?;

    let field_parse = if let Some(parse_impl) = generate_parse_trait(&required, &optional) {
        quote! {
            #parse_impl

            <Self as __rsn::ParseNamedFields<#meta_type, #custom_type>>::parse_fields(span, fields, meta)
        }
    } else {
        quote! {
            {
                let mut this = #required;
                #optional
                Ok(this)
            }
        }
    };

    let field_parse = quote! {
        let res = {
            let fields = &mut fields;
            #field_parse
        };
        if let Some((ident, _)) = fields.into_iter().next() {
            ::core::result::Result::Err(__rsn::FromValueError::new(ident.span, #error::UnexpectedField))
        } else {
            res
        }
    };

    let default_fields = quote! {
        let mut fields = <__rsn::Fields::<#custom_type> as ::core::default::Default>::default();
        let fields = &mut fields;
    };

    let expr = if let Some(check_path) = check_path {
        quote!(
            __rsn::ValueKind::NamedStruct(path, mut fields) if #check_path => {
                #field_parse
            },
            __rsn::ValueKind::Path(path) if #check_path && #min_fields == 0 => {
                #default_fields
                Ok(#required)
            },
        )
    } else {
        quote!(
            __rsn::ValueKind::Struct(mut fields) => {
                #field_parse
            },
            __rsn::ValueKind::Map(fields) if #min_fields == 0 && fields.len() == 0 => {
                #default_fields
                Ok(#required)
            },
        )
    };

    Ok(expr)
}

#[allow(clippy::too_many_arguments)]
fn fields_unnamed(
    fields: &ValueUnnamedFields,
    check_path: Option<TokenStream>,
    self_repr: TokenStream,
    generate_parse_trait: impl FnOnce(&TokenStream) -> Option<TokenStream>,
    min_fields: TokenStream,
    max_fields: TokenStream,
    meta_type: &syn::Type,
    custom_type: &syn::Type,
) -> syn::Result<TokenStream> {
    let error = quote!(__rsn::FromValueErrorKind);

    let parse_fields = construct_fields_unnamed(fields, self_repr)?;

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

    let parse_fields2 = quote!(
        #parse_impl
        if fields.len() >= #min_fields && fields.len() <= #max_fields {
            let mut iter = fields.into_iter();
            #parse_fields
        } else {
            core::result::Result::Err(__rsn::FromValueError::new(span, #error::ExpectedAmountOfElements(#min_fields..=#max_fields)))
        }
    );

    let expr = if let Some(check_path) = check_path {
        quote!(
            __rsn::ValueKind::NamedTuple(path, mut fields) if #check_path => {
                #parse_fields2
            }
            __rsn::ValueKind::Path(path) if #check_path && #min_fields == 0 => {
                let mut iter = core::iter::empty();
                #parse_fields
            }
        )
    } else {
        quote!(
            __rsn::ValueKind::Tuple(mut fields) => {
                #parse_fields2
            }
            value if #min_fields == 1 => {
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
        real_ident,
        ident_str,
        generics,
        modified_generics,
        mut where_clause,
        meta_type,
        custom_type,
        lifetime,
    }: ValueDeriveInput,
) -> syn::Result<TokenStream> {
    let error = quote!(__rsn::FromValueErrorKind);
    let mut added_clone = false;
    let mut add_clone_bound = |span| {
        if !added_clone {
            added_clone = true;
            let where_clause = where_clause.get_or_insert(syn::WhereClause {
                where_token: syn::Token![where](span),
                predicates: Punctuated::default(),
            });

            where_clause
                .predicates
                .push(syn::WherePredicate::Type(syn::PredicateType {
                    lifetimes: None,
                    bounded_ty: custom_type.clone(),
                    colon_token: syn::Token![:](span),
                    bounds: Punctuated::from_iter([syn::TypeParamBound::Trait(syn::TraitBound {
                        paren_token: None,
                        modifier: syn::TraitBoundModifier::None,
                        lifetimes: None,
                        path: syn::Path {
                            leading_colon: Some(syn::Token![::](span)),
                            segments: Punctuated::from_iter([
                                syn::PathSegment::from(format_ident!("core")),
                                syn::PathSegment::from(format_ident!("clone")),
                                syn::PathSegment::from(format_ident!("Clone")),
                            ]),
                        },
                    })]),
                }));
        }
    };

    let mut field_bound = |field_attrs: &FieldAttrs| {
        if matches!(field_attrs.modifier, Some(FieldModifier::WithSerde)) {
            add_clone_bound(field_attrs.modifier_span.unwrap())
        }
    };

    let mut fields_bound = |fields: &_| match fields {
        ValueFields::Unit => {}
        ValueFields::Unnamed(fields) => {
            for (attrs, ..) in fields {
                field_bound(attrs)
            }
        }
        ValueFields::Named(fields) => {
            for (attrs, ..) in fields {
                field_bound(attrs)
            }
        }
    };

    let mut has_untagged = false;
    match &data {
        ValueData::Struct(fields) => fields_bound(fields),
        ValueData::Enum(variants) => {
            for (.., fields) in variants {
                fields_bound(fields)
            }

            for (attrs, ..) in variants {
                if let Some(untagged) = &attrs.untagged {
                    has_untagged = true;
                    add_clone_bound(untagged.span())
                }
            }
        }
    }

    let mut checks = quote!();

    let parse = match data {
        ValueData::Struct(fields) => {
            let check_path = attrs.untagged.is_none().then(|| {
                quote!(path.is_struct(std::concat!(std::module_path!(), "::", #ident_str)))
            });
            match fields {
                ValueFields::Named(fields) => {
                    let min_fields = quote! { <Self as __rsn::NamedFields>::MIN_FIELDS };
                    let reprs = fields_named(
                        &fields,
                        check_path,
                        quote!(Self),
                        |parse_required, parse_optional| {
                            Some(quote!(
                                #[automatically_derived]
                                impl #modified_generics  __rsn::ParseNamedFields<#lifetime, #meta_type, #custom_type> for #real_ident #generics #where_clause {
                                    fn parse_required(span: __rsn::Span, fields: &mut __rsn::Fields<#lifetime, #custom_type>, meta: &mut #meta_type) -> ::core::result::Result<Self, __rsn::FromValueError> {
                                        Ok(#parse_required)
                                    }
                                    fn parse_optional(&mut self, span: __rsn::Span, fields: &mut __rsn::Fields<#lifetime, #custom_type>, meta: &mut #meta_type) -> ::core::result::Result<(), __rsn::FromValueError> {
                                        let this = self;
                                        #parse_optional

                                        Ok(())
                                    }
                                }
                            ))
                        },
                        min_fields,
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
                                impl #modified_generics __rsn::ParseUnnamedFields<#lifetime, #meta_type, #custom_type> for #real_ident #generics #where_clause {
                                    fn parse_fields<I: Iterator<Item = __rsn::Value<#lifetime, #custom_type>>>(struct_span: __rsn::Span, iter: &mut I, parse_optional: bool, meta: &mut #meta_type) -> Result<Self, __rsn::FromValueError> {
                                        #parse_fields
                                    }
                                }
                            ))
                        },
                        quote!(<Self as __rsn::UnnamedFields>::MIN_FIELDS),
                        quote!(<Self as __rsn::UnnamedFields>::MAX_FIELDS),
                        &meta_type,
                        &custom_type,
                    )?;
                    let help_msg = unnamed_help(&fields, &ident_str)?;
                    quote!(
                        match value.inner() {
                            #reprs
                            value => ::core::result::Result::Err(__rsn::FromValueError::new(span, #error::ExpectedPattern(&[#help_msg]))),
                        }
                    )
                }
                ValueFields::Unit => {
                    if attrs.untagged.is_some() {
                        quote! {
                            match value.inner() {
                                __rsn::ValueKind::Tuple(tuple) if tuple.is_empty() => ::core::result::Result::Ok(Self),
                                value => ::core::result::Result::Err(__rsn::FromValueError::new(span, #error::ExpectedPattern(&["()"]))),
                            }
                        }
                    } else {
                        quote!(
                            match value.inner() {
                                __rsn::ValueKind::Path(path) if #check_path => ::core::result::Result::Ok(Self),
                                value => ::core::result::Result::Err(__rsn::FromValueError::new(span, #error::ExpectedIdent(#ident_str))),
                            }
                        )
                    }
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
                        let info = NamedFieldsInfo::new(fields);
                        let error_msg = format!("{} contains many of the same rquired ident", variant_str);
                        let set = info.required_set();
                        let check = quote! {
                            if !<#set as __rsn::__types::Set>::IS_VALID {
                                panic!(#error_msg);
                            }
                        };

                        checks = quote! {
                            #checks
                            #check
                        };

                        fields_named(fields, check_path, quote!(Self::#variant_ident), |_, _| None, info.min_fields(), &meta_type, &custom_type)
                    }
                    ValueFields::Unnamed(fields) => {
                        let info = UnnamedFieldsInfo::new(fields)?;
                        let min_fields = info.required_fields();
                        let optional_fields = info.optional_fields();
                        let max_fields = quote! { #min_fields + #optional_fields };
                        fields_unnamed(fields, check_path, quote!(Self::#variant_ident), |_| None, min_fields, max_fields, &meta_type, &custom_type)
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

            if has_untagged {
                let start = quote!(
                    let patterns = &[#help_msgs];
                    core::result::Result::Err(())
                );
                field_cases.into_iter().fold(start, |acc, cases| {
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
        #checks
         #[automatically_derived]
         impl #modified_generics __rsn::FromValue<#lifetime, #meta_type, #custom_type> for #real_ident #generics #where_clause {
             fn from_value(value: __rsn::Value<#lifetime, #custom_type>, meta: &mut #meta_type) -> ::core::result::Result<Self, __rsn::FromValueError> {
                 let span = value.span;
                 #parse
             }
         }
    })
}
