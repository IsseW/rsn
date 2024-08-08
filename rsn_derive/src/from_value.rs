use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::fmt::Write;
use syn::{spanned::Spanned, DeriveInput};

use crate::{type_set, util::*};

fn named_help(fields: &syn::FieldsNamed, path: String) -> syn::Result<String> {
    let mut msg = String::new();
    let _ = write!(msg, "{} {{", path);

    for field in fields.named.iter() {
        let attrs = FieldAttrs::try_from(&field.attrs)?;
        match attrs.modifier {
            Some(FieldModifier::Default) => {
                write!(
                    msg,
                    "\n\t{}: <value>, <optional>",
                    field.ident.as_ref().unwrap()
                )
                .unwrap();
            }
            None => {
                write!(msg, "\n\t{}: <value>,", field.ident.as_ref().unwrap()).unwrap();
            }
            Some(FieldModifier::Flatten) => {
                write!(
                    msg,
                    "\n\t{}: <value>, <flattened>",
                    field.ident.as_ref().unwrap()
                )
                .unwrap();
            }
            Some(FieldModifier::Skip | FieldModifier::WithExpr(_, _)) => {}
        }
    }
    let _ = write!(msg, "\n}}");

    Ok(msg)
}

fn unnamed_help(fields: &syn::FieldsUnnamed, path: String) -> syn::Result<String> {
    let mut msg = String::new();
    let _ = write!(msg, "{} (", path);

    for field in fields.unnamed.iter() {
        let attrs = FieldAttrs::try_from(&field.attrs)?;
        match attrs.modifier {
            None => {
                write!(msg, "\n\t<value>,").unwrap();
            }
            Some(FieldModifier::Flatten) => {
                write!(msg, "\n\t<value>, <flattened>").unwrap();
            }
            Some(FieldModifier::Default | FieldModifier::Skip | FieldModifier::WithExpr(_, _)) => {}
        }
    }
    let _ = write!(msg, "\n)");

    Ok(msg)
}

fn construct_fields_named<'a>(
    fields: &'a syn::FieldsNamed,
    self_repr: TokenStream,
    mut handle_field: impl FnMut(&syn::Ident, &'a syn::Type, &Option<FieldModifier>),
) -> syn::Result<TokenStream> {
    let error = quote!(__rsn::FromValueErrorKind);
    let field_iter: Vec<_> = fields
        .named
        .iter()
        .map(|field| {
            let attrs = FieldAttrs::try_from(&field.attrs)?;
            let real_ident = field.ident.as_ref().unwrap();
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
            handle_field(ident, &field.ty, &attrs.modifier);
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
    fields: &'a syn::FieldsUnnamed,
    self_repr: TokenStream,
    mut handle_field: impl FnMut(&'a syn::Type, &Option<FieldModifier>),
) -> syn::Result<TokenStream> {
    let field_iter: Vec<_> = fields
        .unnamed
        .iter()
        .map(|field| {
            let attrs = FieldAttrs::try_from(&field.attrs)?;
            let res =
                match &attrs.modifier {
                    None => {
                        quote!(__rsn::FromValue::from_value(iter.next().unwrap(), meta)?)
                    }
                    Some(FieldModifier::Flatten) => quote!(
                        __rsn::ParseUnnamedFields::parse_fields(span, iter.next().unwrap(), meta,)
                    ),
                    Some(FieldModifier::Default) => {
                        panic!("Can't have default fields in a tuple struct")
                    }
                    Some(FieldModifier::Skip) => quote!(core::default::Default::default()),
                    Some(FieldModifier::WithExpr(_, expr)) => quote!(#expr),
                };
            handle_field(&field.ty, &attrs.modifier);
            syn::Result::Ok(res)
        })
        .try_collect()?;
    Ok(quote!(
        Ok(#self_repr(#(#field_iter),*))
    ))
}

fn fields_named(
    fields: &syn::FieldsNamed,
    check_path: Option<TokenStream>,
    self_repr: TokenStream,
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
    fields: &syn::FieldsUnnamed,
    check_path: Option<TokenStream>,
    self_repr: TokenStream,
) -> syn::Result<TokenStream> {
    let error = quote!(__rsn::FromValueErrorKind);

    let mut num_fields: usize = 0;

    let mut flattened = Vec::new();

    let parse_fields =
        construct_fields_unnamed(fields, self_repr, |ty, modifier| match modifier {
            None => num_fields += 1,
            Some(FieldModifier::Flatten) => {
                flattened.push(ty);
            }
            Some(FieldModifier::Skip | FieldModifier::Default | FieldModifier::WithExpr(_, _)) => {}
        })?;

    let num_fields = quote!(
        (#num_fields #(+ <<#flattened> as __rsn::UnnamedFields>::LEN)*)
    );

    let parse_fields2 = quote!(
        if fields.len() == #num_fields {
            let mut iter = fields.into_iter();
            #parse_fields
        } else {
            core::result::Result::Err(__rsn::FromValueError::new(span, #error::ExpectedAmountOfElements(#num_fields..=#num_fields)))
        }
    );

    let expr = if let Some(check_path) = check_path {
        quote!(
            __rsn::ValueKind::NamedTuple(path, mut fields) if #check_path => {
                #parse_fields2
            }
            __rsn::ValueKind::Path(path) if #check_path && #num_fields == 0 => {
                let mut iter = core::iter::empty();
                #parse_fields
            }
        )
    } else {
        quote!(
            __rsn::ValueKind::Tuple(mut fields) => {
                #parse_fields2
            }
            value if #num_fields == 1 => {
                let mut iter = core::iter::once(__rsn::Spanned::new(span, value));
                #parse_fields
            }
        )
    };

    Ok(expr)
}

pub fn from_value(input: &DeriveInput) -> syn::Result<TokenStream> {
    let attrs = ContainerAttrs::try_from(&input.attrs)?;

    let real_ident = &input.ident;
    let ident = attrs.rename.as_ref().unwrap_or(real_ident);
    let ident_str = ident.to_string();
    let error = quote!(__rsn::FromValueErrorKind);
    let mut extra_impl = quote!();
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

    fn lifetime_ident(i: u64) -> syn::Ident {
        format_ident!("_rsn_lifetime{i}")
    }

    fn parameterize_type_lifetimes(ty: &syn::Type, i: &mut u64) -> syn::Result<syn::Type> {
        fn par_return_type(ty: &syn::ReturnType, i: &mut u64) -> syn::Result<syn::ReturnType> {
            Ok(match ty {
                syn::ReturnType::Default => syn::ReturnType::Default,
                syn::ReturnType::Type(arrow, ty) => {
                    syn::ReturnType::Type(*arrow, Box::new(parameterize_type_lifetimes(ty, i)?))
                }
            })
        }
        fn par_lifetime(lifetime: &syn::Lifetime, i: &mut u64) -> syn::Lifetime {
            if lifetime.ident == "_" {
                let ident = lifetime_ident(*i);
                *i += 1;
                syn::Lifetime {
                    ident,
                    ..lifetime.clone()
                }
            } else {
                lifetime.clone()
            }
        }
        fn par_path(path: &syn::Path, i: &mut u64) -> syn::Result<syn::Path> {
            Ok(syn::Path {
                segments: syn::punctuated::Punctuated::from_iter(path.segments.iter().map(|segment| {
                        syn::Result::Ok(syn::PathSegment {
                            arguments: match &segment.arguments {
                                syn::PathArguments::None => syn::PathArguments::None,
                                syn::PathArguments::AngleBracketed(args) => syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                                    args: syn::punctuated::Punctuated::from_iter(args.args.iter().map(|arg| {
                                        syn::Result::Ok(match arg {
                                            syn::GenericArgument::Lifetime(lifetime) => {
                                                syn::GenericArgument::Lifetime(par_lifetime(lifetime, i))
                                            },
                                            syn::GenericArgument::Type(ty) => syn::GenericArgument::Type(
                                                parameterize_type_lifetimes(ty, i)?,
                                            ),
                                            syn::GenericArgument::Const(_) => arg.clone(),
                                            syn::GenericArgument::Binding(binding) => {
                                                syn::GenericArgument::Binding(syn::Binding {
                                                    ty: parameterize_type_lifetimes(&binding.ty, i)?,
                                                    ..binding.clone()
                                                })
                                            },
                                            syn::GenericArgument::Constraint(constraint) => return Err(
                                                syn::Error::new(
                                                    constraint.span(),
                                                    "Can't have type bound constraints in meta type",
                                                ),
                                            ),
                                        })
                                    }).try_collect::<Vec<_>>()?),
                                    ..args.clone()
                                }),
                                syn::PathArguments::Parenthesized(args) => {
                                    syn::PathArguments::Parenthesized(syn::ParenthesizedGenericArguments {
                                        inputs: syn::punctuated::Punctuated::from_iter(
                                            args
                                                .inputs
                                                .iter()
                                                .map(|arg| parameterize_type_lifetimes(arg, i))
                                                .try_collect::<Vec<_>>()?,
                                        ),
                                        output: par_return_type(&args.output, i)?,
                                        ..args.clone()
                                    })
                                },
                            },
                            ..segment.clone()
                        })
                    }).try_collect::<Vec<_>>()?), ..path.clone() })
        }
        match ty {
            syn::Type::Array(array) => Ok(syn::Type::Array(syn::TypeArray {
                elem: Box::new(parameterize_type_lifetimes(&array.elem, i)?),
                ..array.clone()
            })),
            syn::Type::BareFn(function) => Ok(syn::Type::BareFn(syn::TypeBareFn {
                inputs: syn::punctuated::Punctuated::from_iter(
                    function
                        .inputs
                        .iter()
                        .map(|input| {
                            syn::Result::Ok(syn::BareFnArg {
                                ty: parameterize_type_lifetimes(&input.ty, i)?,
                                ..input.clone()
                            })
                        })
                        .try_collect::<Vec<_>>()?,
                ),
                output: par_return_type(&function.output, i)?,
                ..function.clone()
            })),
            syn::Type::Group(group) => Ok(syn::Type::Group(syn::TypeGroup {
                elem: Box::new(parameterize_type_lifetimes(&group.elem, i)?),
                ..group.clone()
            })),
            syn::Type::ImplTrait(_) => Err(syn::Error::new(
                ty.span(),
                "Can't have impl trait in meta type",
            )),
            syn::Type::Infer(_) => Err(syn::Error::new(
                ty.span(),
                "Can't have inferred type in meta type",
            )),
            syn::Type::Macro(_) => Ok(ty.clone()),
            syn::Type::Never(_) => Ok(ty.clone()),
            syn::Type::Paren(paren) => Ok(syn::Type::Paren(syn::TypeParen {
                elem: Box::new(parameterize_type_lifetimes(&paren.elem, i)?),
                ..paren.clone()
            })),
            syn::Type::Path(path) => Ok(syn::Type::Path(syn::TypePath {
                qself: path
                    .qself
                    .as_ref()
                    .map(|qself| {
                        syn::Result::Ok(syn::QSelf {
                            ty: Box::new(parameterize_type_lifetimes(&qself.ty, i)?),
                            ..qself.clone()
                        })
                    })
                    .transpose()?,
                path: par_path(&path.path, i)?,
            })),
            syn::Type::Ptr(ptr) => Ok(syn::Type::Ptr(syn::TypePtr {
                elem: Box::new(parameterize_type_lifetimes(&ptr.elem, i)?),
                ..ptr.clone()
            })),
            syn::Type::Reference(reference) => Ok(syn::Type::Reference(syn::TypeReference {
                elem: Box::new(parameterize_type_lifetimes(&reference.elem, i)?),
                ..reference.clone()
            })),
            syn::Type::Slice(slice) => Ok(syn::Type::Slice(syn::TypeSlice {
                elem: Box::new(parameterize_type_lifetimes(&slice.elem, i)?),
                ..slice.clone()
            })),
            // TODO: This should check for lifetimes in the trait too.
            syn::Type::TraitObject(object) => Ok(syn::Type::TraitObject(syn::TypeTraitObject {
                bounds: syn::punctuated::Punctuated::from_iter(
                    object
                        .bounds
                        .iter()
                        .map(|bound| {
                            syn::Result::Ok(match bound {
                                syn::TypeParamBound::Trait(tra) => {
                                    syn::TypeParamBound::Trait(syn::TraitBound {
                                        path: par_path(&tra.path, i)?,
                                        ..tra.clone()
                                    })
                                }
                                syn::TypeParamBound::Lifetime(lifetime) => {
                                    syn::TypeParamBound::Lifetime(par_lifetime(lifetime, i))
                                }
                            })
                        })
                        .try_collect::<Vec<_>>()?,
                ),
                ..object.clone()
            })),
            syn::Type::Tuple(tuple) => Ok(syn::Type::Tuple(syn::TypeTuple {
                elems: syn::punctuated::Punctuated::from_iter(
                    tuple
                        .elems
                        .iter()
                        .map(|elem| parameterize_type_lifetimes(elem, i))
                        .try_collect::<Vec<_>>()?,
                ),
                ..tuple.clone()
            })),
            syn::Type::Verbatim(_) => todo!(),
            _ => todo!(),
        }
    }

    let mut lifetime_count = 0;
    let parameterized_meta = parameterize_type_lifetimes(&meta_type, &mut lifetime_count)?;
    let lifetimes = (0..lifetime_count)
        .map(|i| syn::Lifetime {
            apostrophe: meta_type.span(),
            ident: lifetime_ident(i),
        })
        .collect::<Vec<_>>();
    let meta_bound_lifetimes = syn::BoundLifetimes {
        for_token: syn::Token![for](meta_type.span()),
        lt_token: syn::Token![<](meta_type.span()),
        lifetimes: syn::punctuated::Punctuated::from_iter(lifetimes.iter().map(|lifetime| {
            syn::LifetimeDef {
                attrs: Vec::new(),
                lifetime: lifetime.clone(),
                colon_token: None,
                bounds: syn::punctuated::Punctuated::new(),
            }
        })),
        gt_token: syn::Token![>](meta_type.span()),
    };

    for param in &mut generics_bounds.params {
        if let syn::GenericParam::Type(param) = param {
            param
                .bounds
                .push(syn::TypeParamBound::Trait(syn::TraitBound {
                    paren_token: None,
                    modifier: syn::TraitBoundModifier::None,
                    lifetimes: Some(meta_bound_lifetimes.clone()),
                    path: syn::Path {
                        leading_colon: None,
                        segments: syn::punctuated::Punctuated::from_iter([
                            syn::PathSegment::from(format_ident!("__rsn")),
                            syn::PathSegment {
                                ident: format_ident!("FromValue"),
                                arguments: syn::PathArguments::AngleBracketed(
                                    syn::AngleBracketedGenericArguments {
                                        colon2_token: None,
                                        lt_token: syn::Token![<](param.span()),
                                        args: syn::punctuated::Punctuated::from_iter([
                                            syn::GenericArgument::Type(parameterized_meta.clone()),
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
                            lifetimes: Some(meta_bound_lifetimes.clone()),
                            path: syn::Path {
                                leading_colon: None,
                                segments: syn::punctuated::Punctuated::from_iter([
                                    syn::PathSegment::from(format_ident!("__rsn")),
                                    syn::PathSegment {
                                        ident: format_ident!("FromValue"),
                                        arguments: syn::PathArguments::AngleBracketed(
                                            syn::AngleBracketedGenericArguments {
                                                colon2_token: None,
                                                lt_token: syn::Token![<](ty.span()),
                                                args: syn::punctuated::Punctuated::from_iter([
                                                    syn::GenericArgument::Type(
                                                        parameterized_meta.clone(),
                                                    ),
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
    let mut where_clause = generics_bounds.where_clause.clone();
    let mut added_clone = false;

    let parse = match &input.data {
        syn::Data::Struct(data) => {
            let check_path = attrs.untagged.is_none().then(|| {
                quote!(path.is_struct(std::concat!(std::module_path!(), "::", #ident_str)))
            });
            match &data.fields {
                syn::Fields::Named(fields) => {
                    let mut required_fields = Vec::new();
                    let mut optional_fields = Vec::new();
                    let mut flattened_fields = Vec::new();

                    let construct =
                        construct_fields_named(fields, quote!(Self), |ident, ty, modifier| {
                            match modifier {
                                None => required_fields.push(ident.clone()),
                                Some(FieldModifier::Flatten) => flattened_fields.push(ty),
                                Some(FieldModifier::Default) => optional_fields.push(ident.clone()),
                                Some(FieldModifier::Skip | FieldModifier::WithExpr(_, _)) => {}
                            }
                        })?;

                    let min_fields = required_fields.len();
                    let min_fields = quote!(
                        (#min_fields #(+ <#flattened_fields as __rsn::NamedFields>::MIN_FIELDS)*)
                    );
                    let max_fields = required_fields.len() + optional_fields.len();
                    let max_fields = quote!(
                        (#max_fields #(+ <#flattened_fields as __rsn::NamedFields>::MAX_FIELDS)*)
                    );

                    let set = type_set::make_ident_set(required_fields.clone().into_iter());
                    let set = type_set::union_all(std::iter::once(set).chain(
                        flattened_fields.into_iter().map(|ty| {
                            let span = ty.span();
                            syn::Type::Path(syn::TypePath {
                                qself: Some(syn::QSelf {
                                    lt_token: syn::token::Lt(span),
                                    ty: Box::new(ty.clone()),
                                    position: 2,
                                    as_token: Some(syn::token::As(span)),
                                    gt_token: syn::token::Gt(span),
                                }),
                                path: syn::Path {
                                    leading_colon: None,
                                    segments: syn::punctuated::Punctuated::from_iter([
                                        syn::PathSegment {
                                            ident: syn::Ident::new("__rsn", ty.span()),
                                            arguments: syn::PathArguments::None,
                                        },
                                        syn::PathSegment {
                                            ident: syn::Ident::new("NamedFields", ty.span()),
                                            arguments: syn::PathArguments::None,
                                        },
                                        syn::PathSegment {
                                            ident: syn::Ident::new("Fields", ty.span()),
                                            arguments: syn::PathArguments::None,
                                        },
                                    ]),
                                },
                            })
                        }),
                    ));
                    let required_fields =
                        required_fields.into_iter().map(|ident| ident.to_string());
                    let optional_fields =
                        optional_fields.into_iter().map(|ident| ident.to_string());

                    extra_impl = quote!(
                        type __Set = #set;
                        #[automatically_derived]
                        impl #generics  __rsn::NamedFields for #ident #generics {
                            type Fields = __Set;

                            const REQUIRED_FIELDS: &'static [&'static str] = &[#(#required_fields),*];
                            const OPTIONAL_FIELDS: &'static [&'static str] = &[#(#optional_fields),*];
                            const MIN_FIELDS: usize = #min_fields;
                            const MAX_FIELDS: usize = #max_fields;
                        }

                        #[automatically_derived]
                        impl #generics_bounds  __rsn::ParseNamedFields<#meta_type, #custom_type> for #ident #generics #where_clause {
                            fn parse_fields(span: __rsn::Span, fields: &mut __rsn::Fields<#custom_type>, meta: &mut #meta_type) -> ::core::result::Result<Self, __rsn::FromValueError> {
                                #construct
                            }
                        }

                        const IS_VALID: bool = <__Set as __rsn::__types::Set>::IS_VALID;

                        if !IS_VALID {
                            panic!("There are required fields with the same identifier");
                        }
                    );

                    let reprs = fields_named(fields, check_path, quote!(Self))?;
                    let help_msg = named_help(fields, ident_str)?;

                    quote!(
                        match value.inner() {
                            #reprs
                            value => core::result::Result::Err(__rsn::FromValueError::new(span, #error::ExpectedPattern(&[#help_msg]))),
                        }
                    )
                }
                syn::Fields::Unnamed(fields) => {
                    let mut flattened_fields = Vec::new();
                    let mut num_fields: usize = 0;

                    let construct = construct_fields_unnamed(
                        fields,
                        quote!(Self),
                        |ty, modifier| match modifier {
                            None => num_fields += 1,
                            Some(FieldModifier::Flatten) => {
                                flattened_fields.push(ty);
                            }
                            Some(
                                FieldModifier::Default
                                | FieldModifier::Skip
                                | FieldModifier::WithExpr(_, _),
                            ) => {}
                        },
                    )?;

                    let num_fields = quote!(
                        (#num_fields #(+ <<#flattened_fields> as __rsn::UnnamedFields>::LEN)*)
                    );

                    extra_impl = quote!(
                        #[automatically_derived]
                        impl #generics __rsn::UnnamedFields for #ident #generics {
                            const LEN: usize  = #num_fields;
                        }

                        #[automatically_derived]
                        impl #generics_bounds __rsn::ParseUnnamedFields<#meta_type, #custom_type> for #ident #generics #where_clause {
                            fn parse_fields<'a, I: Iterator<Item = __rsn::Value<'a, #custom_type>>>(struct_span: __rsn::Span, iter: &mut I, meta: &mut #meta_type) -> Result<Self, __rsn::FromValueError> {
                                #construct
                            }
                        }
                    );

                    let reprs = fields_unnamed(fields, check_path, quote!(Self))?;
                    let help_msg = unnamed_help(fields, ident_str)?;
                    quote!(
                        match value.inner() {
                            #reprs
                            value => core::result::Result::Err(__rsn::FromValueError::new(span, #error::ExpectedPattern(&[#help_msg]))),
                        }
                    )
                }
                syn::Fields::Unit => {
                    quote!(
                        match value.inner() {
                            __rsn::ValueKind::Path(path) if #check_path => core::result::Result::Ok(Self),
                            value => core::result::Result::Err(__rsn::FromValueError::new(span, #error::ExpectedIdent(#ident_str))),
                        }
                    )
                }
            }
        }
        syn::Data::Enum(data) => {
            let field_cases = data.variants.iter().map(|variant| {
                let variant_attrs = ContainerAttrs::try_from(&variant.attrs)?;
                if let Some(ty) = variant_attrs.with_meta {
                    return Err(syn::Error::new(ty.span(), "with_meta is not supported on variants"));
                }
                let variant_ident = &variant.ident;
                let variant_str = variant_attrs.rename.map_or(variant_ident.to_string(), |ident| ident.to_string());
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

                match &variant.fields {
                    syn::Fields::Named(fields) => {
                        fields_named(fields, check_path, quote!(Self::#variant_ident))
                    }
                    syn::Fields::Unnamed(fields) => {
                        fields_unnamed(fields, check_path, quote!(Self::#variant_ident))
                    }
                    syn::Fields::Unit => if let Some(check_path) = check_path {
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
            let help_msgs = data.variants.iter().try_fold(quote!(), |acc, variant| {
                let path = if attrs.untagged.is_some() {
                    variant.ident.to_string()
                } else {
                    format!("{ident_str}::{}", variant.ident)
                };
                let help_msg = match &variant.fields {
                    syn::Fields::Named(fields) => named_help(fields, path)?,
                    syn::Fields::Unnamed(fields) => unnamed_help(fields, path)?,
                    syn::Fields::Unit => path,
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
                        let where_clause = where_clause.get_or_insert(syn::WhereClause { where_token: syn::Token![where](untagged.span()), predicates: syn::punctuated::Punctuated::default() });

                        where_clause.predicates.push(syn::WherePredicate::Type(syn::PredicateType {
                            lifetimes: None,
                            bounded_ty: custom_type.clone(),
                            colon_token: syn::Token![:](untagged.span()),
                            bounds: syn::punctuated::Punctuated::from_iter([syn::TypeParamBound::Trait(syn::TraitBound { paren_token: None, modifier: syn::TraitBoundModifier::None, lifetimes: None, path: syn::Path {
                                leading_colon: Some(syn::Token![::](untagged.span())),
                                segments: syn::punctuated::Punctuated::from_iter([
                                    syn::PathSegment::from(format_ident!("core")),
                                    syn::PathSegment::from(format_ident!("clone")),
                                    syn::PathSegment::from(format_ident!("Clone")),
                                ]),
                            }  })]),
                        }));
                    }
                    quote!(
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
        syn::Data::Union(_) => panic!("Unions aren't supported"),
    };

    Ok(quote! {
        const _: () = {
            extern crate rsn as __rsn;
            #[automatically_derived]
            impl #generics_bounds __rsn::FromValue<#meta_type, #custom_type> for #real_ident #generics #where_clause {
                fn from_value(value: __rsn::Value<#custom_type>, meta: &mut #meta_type) -> ::core::result::Result<Self, __rsn::FromValueError> {
                    let span = value.span;
                    #parse
                }
            }
            #extra_impl
        };
    })
}
