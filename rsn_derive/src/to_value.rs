use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::spanned::Spanned;

use crate::util::*;

pub fn to_value(input: &syn::DeriveInput) -> syn::Result<TokenStream> {
    let attrs = ContainerAttrs::try_from(&input.attrs)?;

    let real_ident = &input.ident;
    let ident = attrs.rename.as_ref().unwrap_or(real_ident);

    let generics = &input.generics;

    let mut generics_bounds = generics.clone();
    let meta_type = if let Some(ty) = attrs.with_meta.clone() {
        ty
    } else {
        syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments: syn::punctuated::Punctuated::from_iter([syn::PathSegment::from(
                    format_ident!("_Rsn_Meta"),
                )]),
            },
        })
    };
    let custom_type = if let Some(ty) = attrs.with_custom.clone() {
        ty
    } else {
        syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path {
                leading_colon: None,
                segments: syn::punctuated::Punctuated::from_iter([syn::PathSegment::from(
                    format_ident!("_Rsn_CustomType"),
                )]),
            },
        })
    };
    for param in &mut generics_bounds.params {
        if let syn::GenericParam::Type(param) = param {
            param
                .bounds
                .push(syn::TypeParamBound::Trait(syn::TraitBound {
                    paren_token: None,
                    modifier: syn::TraitBoundModifier::None,
                    lifetimes: None,
                    path: syn::Path {
                        leading_colon: None,
                        segments: syn::punctuated::Punctuated::from_iter([
                            syn::PathSegment::from(format_ident!("__rsn")),
                            syn::PathSegment {
                                ident: format_ident!("ToValue"),
                                arguments: syn::PathArguments::AngleBracketed(
                                    syn::AngleBracketedGenericArguments {
                                        colon2_token: None,
                                        lt_token: syn::Token![<](param.span()),
                                        args: syn::punctuated::Punctuated::from_iter([
                                            syn::GenericArgument::Type(meta_type.clone()),
                                            syn::GenericArgument::Type(custom_type.clone()),
                                        ]),
                                        gt_token: syn::Token![>](param.span()),
                                    },
                                ),
                            },
                        ]),
                    },
                }))
        }
    }

    if attrs.with_meta.is_none() || attrs.with_custom.is_none() {
        if attrs.with_meta.is_none() {
            generics_bounds
                .params
                .push(syn::GenericParam::Type(syn::TypeParam {
                    attrs: Vec::new(),
                    ident: format_ident!("_Rsn_Meta"),
                    colon_token: None,
                    bounds: syn::punctuated::Punctuated::new(),
                    eq_token: None,
                    default: None,
                }));
        }
        if attrs.with_custom.is_none() {
            generics_bounds
                .params
                .push(syn::GenericParam::Type(syn::TypeParam {
                    attrs: Vec::new(),
                    ident: format_ident!("_Rsn_CustomType"),
                    colon_token: None,
                    bounds: syn::punctuated::Punctuated::new(),
                    eq_token: None,
                    default: None,
                }));
        }
        let where_clause = generics_bounds.make_where_clause();

        fn skip_bound(ty: &syn::Type, ident: &syn::Ident) -> bool {
            match ty {
                syn::Type::Array(arr) => skip_bound(&arr.elem, ident),
                syn::Type::BareFn(_) => false,
                syn::Type::Group(ty) => skip_bound(&ty.elem, ident),
                syn::Type::ImplTrait(_) => false,
                syn::Type::Infer(_) => false,
                syn::Type::Macro(_) => false,
                syn::Type::Never(_) => false,
                syn::Type::Paren(ty) => skip_bound(&ty.elem, ident),
                syn::Type::Path(path) => {
                    path.path.is_ident(ident)
                        || path
                            .path
                            .segments
                            .last()
                            .map_or(true, |l| match &l.arguments {
                                syn::PathArguments::AngleBracketed(args) => {
                                    args.args.iter().any(|arg| match arg {
                                        syn::GenericArgument::Type(ty) => skip_bound(ty, ident),
                                        _ => false,
                                    })
                                }
                                _ => false,
                            })
                }
                syn::Type::Ptr(_) => false,
                syn::Type::Reference(_) => false,
                syn::Type::Slice(_) => false,
                syn::Type::TraitObject(_) => false,
                syn::Type::Tuple(tuple) => tuple.elems.iter().any(|ty| skip_bound(ty, ident)),
                syn::Type::Verbatim(_) => false,
                _ => todo!(),
            }
        }

        let mut add_type_bound = |ty: &syn::Type| {
            if skip_bound(ty, real_ident) {
                return;
            }
            where_clause
                .predicates
                .push(syn::WherePredicate::Type(syn::PredicateType {
                    lifetimes: None,
                    bounded_ty: ty.clone(),
                    colon_token: syn::Token![:](ty.span()),
                    bounds: syn::punctuated::Punctuated::from_iter([syn::TypeParamBound::Trait(
                        syn::TraitBound {
                            paren_token: None,
                            modifier: syn::TraitBoundModifier::None,
                            lifetimes: None,
                            path: syn::Path {
                                leading_colon: None,
                                segments: syn::punctuated::Punctuated::from_iter([
                                    syn::PathSegment::from(format_ident!("__rsn")),
                                    syn::PathSegment {
                                        ident: format_ident!("ToValue"),
                                        arguments: syn::PathArguments::AngleBracketed(
                                            syn::AngleBracketedGenericArguments {
                                                colon2_token: None,
                                                lt_token: syn::Token![<](ty.span()),
                                                args: syn::punctuated::Punctuated::from_iter([
                                                    syn::GenericArgument::Type(meta_type.clone()),
                                                    syn::GenericArgument::Type(custom_type.clone()),
                                                ]),
                                                gt_token: syn::Token![>](ty.span()),
                                            },
                                        ),
                                    },
                                ]),
                            },
                        },
                    )]),
                }))
        };

        let mut add_field_bounds = |fields: &syn::Fields| match fields {
            syn::Fields::Named(fields) => {
                for field in fields.named.iter() {
                    if FieldAttrs::try_from(&field.attrs).map_or(true, |attrs| !attrs.skip_bound) {
                        add_type_bound(&field.ty)
                    }
                }
            }
            syn::Fields::Unnamed(fields) => {
                for field in fields.unnamed.iter() {
                    if FieldAttrs::try_from(&field.attrs).map_or(true, |attrs| !attrs.skip_bound) {
                        add_type_bound(&field.ty)
                    }
                }
            }
            syn::Fields::Unit => {}
        };

        match &input.data {
            syn::Data::Struct(fields) => add_field_bounds(&fields.fields),
            syn::Data::Enum(variants) => {
                for variant in variants.variants.iter() {
                    add_field_bounds(&variant.fields)
                }
            }
            syn::Data::Union(_) => {}
        }
    }
    let where_clause = generics_bounds.where_clause.clone();

    Ok(quote! {
        const _: () = {
            extern crate rsn as __rsn;
            #[automatically_derived]
            impl #generics_bounds __rsn::ToValue<#meta_type, #custom_type> for #real_ident #generics #where_clause {
                fn to_value(&self, meta: &#meta_type) -> __rsn::Value<#custom_type> {
                }
            }
        };
    })
}
