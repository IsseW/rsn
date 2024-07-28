use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::fmt::Write;
use syn::{spanned::Spanned, DeriveInput};

use crate::type_set;

#[allow(dead_code)]
enum ContainerAttr {
    Untagged(syn::Ident),
    Rename(syn::Ident, syn::Token![=], syn::Ident),
    WithMeta(syn::Ident, syn::Token![=], syn::Type),
}

impl syn::parse::Parse for ContainerAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let tag: syn::Ident = input.parse()?;

        Ok(match tag.to_string().as_str() {
            "untagged" => ContainerAttr::Untagged(tag),
            "rename" => ContainerAttr::Rename(tag, input.parse()?, input.parse()?),
            "with_meta" => ContainerAttr::WithMeta(tag, input.parse()?, input.parse()?),

            _ => return Err(syn::Error::new(tag.span(), "Unexpected tag")),
        })
    }
}

#[derive(Default)]
struct ContainerAttrs {
    untagged: bool,
    rename: Option<syn::Ident>,
    with_meta: Option<syn::Type>,
}

impl TryFrom<&Vec<syn::Attribute>> for ContainerAttrs {
    type Error = syn::Error;

    fn try_from(value: &Vec<syn::Attribute>) -> Result<Self, Self::Error> {
        let mut this = Self::default();
        for attr in value {
            if attr.path.is_ident("rsn") {
                let attrs = attr.parse_args_with(|input: syn::parse::ParseStream| {
                    input
                        .parse_terminated::<ContainerAttr, syn::Token![,]>(syn::parse::Parse::parse)
                })?;

                for attr in attrs.into_iter() {
                    match attr {
                        ContainerAttr::Untagged(tag) => {
                            if this.untagged {
                                return Err(syn::Error::new(tag.span(), "Multiple untagged tags"));
                            }
                            this.untagged = true
                        }
                        ContainerAttr::Rename(tag, _, rename) => {
                            if this.rename.is_some() {
                                return Err(syn::Error::new(tag.span(), "Multiple rename tags"));
                            }
                            this.rename = Some(rename);
                        }
                        ContainerAttr::WithMeta(tag, _, with_meta) => {
                            if this.with_meta.is_some() {
                                return Err(syn::Error::new(tag.span(), "Multiple with_meta tags"));
                            }
                            this.with_meta = Some(with_meta);
                        }
                    }
                }
            }
        }
        Ok(this)
    }
}

#[derive(PartialEq, Clone)]
enum FieldModifier {
    Flatten,
    Default,
    Skip,
    WithExpr(syn::Token![=], syn::Expr),
}

impl FieldModifier {
    fn as_str(&self) -> &str {
        match self {
            FieldModifier::Flatten => "flatten",
            FieldModifier::Default => "default",
            FieldModifier::Skip => "skip",
            FieldModifier::WithExpr(_, _) => "from_meta",
        }
    }
}

impl std::fmt::Display for FieldModifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

#[allow(dead_code)]
enum FieldAttr {
    FieldModifier(syn::Ident, FieldModifier),
    Rename(syn::Ident, syn::Token![=], syn::Ident),
}

impl syn::parse::Parse for FieldAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let tag: syn::Ident = input.parse()?;

        Ok(match tag.to_string().as_str() {
            "flatten" => FieldAttr::FieldModifier(tag, FieldModifier::Flatten),
            "default" => FieldAttr::FieldModifier(tag, FieldModifier::Default),
            "skip" => FieldAttr::FieldModifier(tag, FieldModifier::Skip),
            "with_expr" => FieldAttr::FieldModifier(
                tag,
                FieldModifier::WithExpr(input.parse()?, input.parse()?),
            ),
            "rename" => FieldAttr::Rename(tag, input.parse()?, input.parse()?),

            _ => return Err(syn::Error::new(tag.span(), "Unexpected tag")),
        })
    }
}

#[derive(Default)]
struct FieldAttrs {
    modifier: Option<FieldModifier>,
    rename: Option<syn::Ident>,
}

impl TryFrom<&Vec<syn::Attribute>> for FieldAttrs {
    type Error = syn::Error;

    fn try_from(value: &Vec<syn::Attribute>) -> Result<Self, Self::Error> {
        let mut this = Self::default();
        for attr in value {
            if attr.path.is_ident("rsn") {
                let attrs = attr.parse_args_with(|input: syn::parse::ParseStream| {
                    input.parse_terminated::<FieldAttr, syn::Token![,]>(syn::parse::Parse::parse)
                })?;

                for attr in attrs.into_iter() {
                    match attr {
                        FieldAttr::FieldModifier(tag, modifier) => {
                            if let Some(modifier) = &this.modifier {
                                return Err(syn::Error::new(tag.span(), format!("Multiple field modifier tags, this field already has a {} tag", modifier.as_str())));
                            }

                            this.modifier = Some(modifier);
                        }
                        FieldAttr::Rename(tag, _, rename) => {
                            if this.rename.is_some() {
                                return Err(syn::Error::new(tag.span(), "Multiple rename tags"));
                            }
                            this.rename = Some(rename);
                        }
                    }
                }
            }
        }
        Ok(this)
    }
}

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
                        #real_ident: #expr,
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
                                ident: format_ident!("FromValue"),
                                arguments: syn::PathArguments::AngleBracketed(
                                    syn::AngleBracketedGenericArguments {
                                        colon2_token: None,
                                        lt_token: syn::Token![<](param.span()),
                                        args: syn::punctuated::Punctuated::from_iter([
                                            syn::GenericArgument::Type(meta_type.clone()),
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

    let parse = match &input.data {
        syn::Data::Struct(data) => {
            let check_path = (!attrs.untagged).then(|| {
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
                        impl #generics_bounds  __rsn::ParseNamedFields<#meta_type> for #ident #generics {
                            fn parse_fields(span: __rsn::Span, fields: &mut __rsn::Fields, meta: &mut #meta_type) -> ::core::result::Result<Self, __rsn::FromValueError> {
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
                        impl #generics_bounds __rsn::ParseUnnamedFields<#meta_type> for #ident #generics {
                            fn parse_fields<'a, I: Iterator<Item = __rsn::Value<'a>>>(struct_span: __rsn::Span, iter: &mut I, meta: &mut #meta_type) -> Result<Self, __rsn::FromValueError> {
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
                let untagged = attrs.untagged;
                let check_path = if variant_attrs.untagged && attrs.untagged {
                    None
                } else if variant_attrs.untagged {
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
                let path = if attrs.untagged {
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

            if attrs.untagged {
                let start = quote!(
                    let patterns = &[#help_msgs];
                    core::result::Result::Err(())
                );
                field_cases.into_iter().fold(start, |acc, cases| {
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
            impl #generics_bounds __rsn::FromValue<#meta_type> for #real_ident #generics {
                fn from_value(value: __rsn::Value, meta: &mut #meta_type) -> ::core::result::Result<Self, __rsn::FromValueError> {
                    let span = value.span;
                    #parse
                }
            }
            #extra_impl
        };
    })
}
