use std::{
    collections::HashSet,
    ops::Range,
    path::PathBuf,
    sync::{OnceLock, RwLock},
};

use proc_macro2::TokenStream;
use quote::quote;
use syn::{punctuated::Punctuated, spanned::Spanned};

use crate::{type_set, util::*};

fn generated(ident: &syn::Ident) -> bool {
    #[derive(PartialEq, Eq, Hash)]
    struct Place {
        ident: String,
        file: PathBuf,
        span: Range<usize>,
    }
    static GENERATED_PLACES: OnceLock<RwLock<HashSet<Place>>> = OnceLock::new();

    let span = ident.span();
    let file = span.unwrap().source_file().path();
    let span = span.unwrap().byte_range();

    let here = Place {
        ident: ident.to_string(),
        file,
        span,
    };

    let places = GENERATED_PLACES.get_or_init(|| RwLock::new(HashSet::new()));

    let guard = places.read().unwrap();

    if guard.contains(&here) {
        true
    } else {
        drop(guard);
        let mut guard = places.write().unwrap();
        guard.insert(here);
        false
    }
}

pub struct UnnamedFieldsInfo<'a> {
    flattened_fields: Vec<&'a syn::Type>,
    required_fields: usize,
    optional_fields: usize,
}

impl<'a> UnnamedFieldsInfo<'a> {
    pub fn new(fields: &'a ValueUnnamedFields) -> syn::Result<Self> {
        let mut flattened_fields = Vec::new();
        let mut required_fields: usize = 0;
        let mut optional_fields: usize = 0;
        let mut expects_end = None;

        for (attrs, ty) in fields {
            let error = |span| {
                Err(syn::Error::new(
                    span,
                    "The `default` attribute can only be placed after all other required fields in a tuple",
                ))
            };
            match &attrs.modifier {
                None | Some(FieldModifier::WithSerde) => {
                    if let Some(span) = expects_end {
                        return error(span);
                    }
                    required_fields += 1;
                }
                Some(FieldModifier::Flatten) => {
                    if let Some(span) = expects_end {
                        return error(span);
                    }
                    flattened_fields.push(ty);
                }
                Some(FieldModifier::Default) => {
                    optional_fields += 1;
                    expects_end = Some(ty.span());
                }
                Some(FieldModifier::Skip | FieldModifier::WithExpr(..)) => {}
            }
        }

        Ok(Self {
            flattened_fields,
            required_fields,
            optional_fields,
        })
    }

    pub fn required_fields(&self) -> TokenStream {
        let required_fields = self.required_fields;
        let flattened_fields = &self.flattened_fields;
        quote!(#required_fields #(+ <<#flattened_fields> as __rsn::UnnamedFields>::MIN_FIELDS)*)
    }

    pub fn optional_fields(&self) -> usize {
        self.optional_fields
    }
}

pub struct NamedFieldsInfo<'a> {
    required_fields: Vec<syn::Ident>,
    optional_fields: Vec<syn::Ident>,
    flattened_fields: Vec<&'a syn::Type>,
}

impl<'a> NamedFieldsInfo<'a> {
    pub fn new(fields: &'a ValueNamedFields) -> Self {
        let mut required_fields = Vec::new();
        let mut optional_fields = Vec::new();
        let mut flattened_fields = Vec::new();

        for (attrs, ident, ty) in fields {
            let ident = attrs.rename.as_ref().unwrap_or(ident);
            match attrs.modifier {
                None | Some(FieldModifier::WithSerde) => required_fields.push(ident.clone()),
                Some(FieldModifier::Flatten) => flattened_fields.push(ty),
                Some(FieldModifier::Default) => optional_fields.push(ident.clone()),
                Some(FieldModifier::Skip | FieldModifier::WithExpr(..)) => {}
            }
        }

        Self {
            required_fields,
            optional_fields,
            flattened_fields,
        }
    }

    pub fn min_fields(&self) -> TokenStream {
        let flattened_fields = &self.flattened_fields;
        let min_fields = self.required_fields.len();
        quote!(
            (#min_fields #(+ <#flattened_fields as __rsn::NamedFields>::MIN_FIELDS)*)
        )
    }

    pub fn required_set(&self) -> syn::Type {
        let set = type_set::make_ident_set(self.required_fields.clone().into_iter());
        type_set::union_all(
            std::iter::once(set).chain(self.flattened_fields.iter().map(|&ty| {
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
                        segments: Punctuated::from_iter([
                            syn::PathSegment {
                                ident: syn::Ident::new("__rsn", ty.span()),
                                arguments: syn::PathArguments::None,
                            },
                            syn::PathSegment {
                                ident: syn::Ident::new("NamedFields", ty.span()),
                                arguments: syn::PathArguments::None,
                            },
                            syn::PathSegment {
                                ident: syn::Ident::new("RequiredFields", ty.span()),
                                arguments: syn::PathArguments::None,
                            },
                        ]),
                    },
                })
            })),
        )
    }

    pub fn optional_set(&self) -> syn::Type {
        let set = type_set::make_ident_set(self.optional_fields.clone().into_iter());
        type_set::try_union_all(std::iter::once(set).chain(self.flattened_fields.iter().map(
            |&ty| {
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
                        segments: Punctuated::from_iter([
                            syn::PathSegment {
                                ident: syn::Ident::new("__rsn", ty.span()),
                                arguments: syn::PathArguments::None,
                            },
                            syn::PathSegment {
                                ident: syn::Ident::new("NamedFields", ty.span()),
                                arguments: syn::PathArguments::None,
                            },
                            syn::PathSegment {
                                ident: syn::Ident::new("OptionalFields", ty.span()),
                                arguments: syn::PathArguments::None,
                            },
                        ]),
                    },
                })
            },
        )))
    }
}

pub fn derive_fields(input: &ValueDeriveInput) -> syn::Result<TokenStream> {
    if generated(&input.real_ident) {
        return Ok(quote!());
    }

    Ok(if let ValueData::Struct(fields) = &input.data {
        let generics = &input.generics;
        let ident = &input.real_ident;
        match fields {
            ValueFields::Unnamed(fields) => {
                let info = UnnamedFieldsInfo::new(fields)?;

                let optional_fields = info.optional_fields();
                let required_fields = info.required_fields();

                quote! {
                    #[automatically_derived]
                    impl #generics __rsn::UnnamedFields for #ident #generics {
                        const MIN_FIELDS: usize = #required_fields;
                        const MAX_FIELDS: usize = <Self as __rsn::UnnamedFields>::MIN_FIELDS + #optional_fields;
                    }
                }
            }
            ValueFields::Named(fields) => {
                let info = NamedFieldsInfo::new(fields);

                let min_fields = info.min_fields();

                let required_set = info.required_set();
                let optional_set = info.optional_set();

                quote! {
                    type __Set = #required_set;

                    const IS_VALID: bool = <__Set as __rsn::__types::Set>::IS_VALID;

                    if !IS_VALID {
                        panic!("There are required fields with the same identifier");
                    }

                    #[automatically_derived]
                    impl #generics  __rsn::NamedFields for #ident #generics {
                        type RequiredFields = __Set;
                        type OptionalFields = #optional_set;

                        const MIN_FIELDS: usize = #min_fields;
                        const MAX_FIELDS: usize = Self::MIN_FIELDS + <Self::OptionalFields as __rsn::__types::Set>::LEN;
                    }
                }
            }
            ValueFields::Unit => quote!(),
        }
    } else {
        quote!()
    })
}
