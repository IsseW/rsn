use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::fmt::Write;
use syn::{DeriveInput, LitStr};

enum Attribute {
    Tag(syn::Ident),
    Value(syn::Ident, syn::Ident),
}

impl syn::parse::Parse for Attribute {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let tag = input.parse()?;

        let test: Result<syn::Token![=], _> = input.parse();
        if test.is_ok() {
            Ok(Attribute::Value(tag, input.parse()?))
        } else {
            Ok(Attribute::Tag(tag))
        }
    }
}

#[derive(Default)]
struct ContainerAttrs {
    untagged: bool,
    rename: Option<syn::Ident>,
}

impl From<&Vec<syn::Attribute>> for ContainerAttrs {
    fn from(value: &Vec<syn::Attribute>) -> Self {
        let mut this = Self::default();
        for attr in value {
            if attr.path.is_ident("parse") {
                if let Ok(attr) = attr.parse_args::<Attribute>() {
                    match attr {
                        Attribute::Tag(tag) => {
                            if tag == "untagged" {
                                this.untagged = true;
                            }
                        }
                        Attribute::Value(tag, value) => {
                            if tag == "rename" {
                                this.rename = Some(value);
                            }
                        }
                    }
                }
            }
        }
        this
    }
}

#[derive(Default)]
struct FieldAttrs {
    default: bool,
    skip: bool,
    rename: Option<syn::Ident>,
}

impl From<&Vec<syn::Attribute>> for FieldAttrs {
    fn from(value: &Vec<syn::Attribute>) -> Self {
        let mut this = Self::default();
        for attr in value {
            if attr.path.is_ident("parse") {
                if let Ok(attr) = attr.parse_args::<Attribute>() {
                    match attr {
                        Attribute::Tag(tag) => {
                            if tag == "default" {
                                this.default = true;
                            } else if tag == "skip" {
                                this.skip = true;
                            }
                        }
                        Attribute::Value(tag, value) => {
                            if tag == "rename" {
                                this.rename = Some(value);
                            }
                        }
                    }
                }
            }
        }
        this
    }
}

fn named_help(fields: &syn::FieldsNamed, path: String) -> String {
    let mut msg = String::new();
    let _ = write!(msg, "{} {{", path);

    for field in fields.named.iter() {
        let attrs = FieldAttrs::from(&field.attrs);
        if !attrs.skip {
            if attrs.default {
                write!(
                    msg,
                    "\n\t{}: <value>, <optional>",
                    field.ident.as_ref().unwrap()
                )
                .unwrap();
            } else {
                write!(msg, "\n\t{}: <value>,", field.ident.as_ref().unwrap()).unwrap();
            }
        }
    }
    let _ = write!(msg, "\n}}");

    msg
}

fn unnamed_help(fields: &syn::FieldsUnnamed, path: String) -> String {
    let mut msg = String::new();
    let _ = write!(msg, "{} (", path);

    for field in fields.unnamed.iter() {
        let attrs = FieldAttrs::from(&field.attrs);
        if !attrs.skip {
            let _ = write!(msg, "\n\t<value>,");
        }
    }
    let _ = write!(msg, "\n)");

    msg
}

fn fields_named(
    fields: &syn::FieldsNamed,
    check_path: Option<TokenStream>,
    self_repr: TokenStream,
) -> TokenStream {
    let error = quote!(__from_value::FromValueErrorKind);

    let mut min_fields: usize = 0;
    let mut max_fields: usize = 0;

    let mut optional_field_strs = Vec::new();
    let mut needed_field_strs = Vec::new();

    let field_iter: Vec<_> = fields
        .named
        .iter()
        .map(|field| {
            let attrs = FieldAttrs::from(&field.attrs);
            let ident = attrs.rename.as_ref().or(field.ident.as_ref()).unwrap();
            let ident_str = LitStr::new(ident.to_string().as_str(), ident.span());
            if attrs.skip {
                quote! {
                    #ident: core::default::Default::default()
                }
            } else if attrs.default {
                optional_field_strs.push(ident_str.clone());
                max_fields += 1;
                quote! {
                    #ident: if let Some(value) = fields.remove(#ident_str) {
                        __from_value::FromValue::from_value(value)?
                    } else {
                        core::default::Default::default()
                    }
                }
            } else {
                needed_field_strs.push(ident_str.clone());
                min_fields += 1;
                max_fields += 1;
                quote! {
                    #ident: __from_value::FromValue::from_value(fields.remove(#ident_str).ok_or(__from_value::FromValueError::new(span, #error::MissingField(#ident_str)))?)?
                }
            }
        })
        .collect();

    let check_len = quote!(
        if (#min_fields..=#max_fields).contains(&fields.len())
    );
    let field_parse = quote! {
        #check_len {
            let res = Ok(#self_repr {
                #(#field_iter),*
            });
            if let Some((ident, _)) = fields.into_iter().next() {
                core::result::Result::Err(__from_value::FromValueError::new(ident.span, #error::UnexpectedIdent))
            } else {
                res
            }
        } else {
            #(
                fields.remove(#needed_field_strs).ok_or(__from_value::FromValueError::new(span, #error::MissingField(#needed_field_strs)))?;
            )*
            #(
                fields.remove(#optional_field_strs);
            )*
            let (ident, _) = fields.into_iter().next().unwrap();

            core::result::Result::Err(__from_value::FromValueError::new(ident.span, #error::UnexpectedIdent))
        }
    };

    let fields_default = fields.named.iter().map(|field| {
        let ident = field.ident.as_ref().unwrap();
        quote!(
            #ident: core::default::Default::default()
        )
    });
    let all_default = quote!(
        Ok(Self {
            #(#fields_default),*
        })
    );

    if let Some(check_path) = check_path {
        let named_struct_repr = quote!(
            __from_value::ValueKind::NamedStruct(path, mut fields) #check_path => {
                #field_parse
            }
        );
        let unit_repr = if min_fields == 0 {
            quote!(
                __from_value::ValueKind::Path(path) #check_path => {
                    #all_default
                }
            )
        } else {
            quote!()
        };

        quote!(
            #named_struct_repr
            #unit_repr
        )
    } else {
        let struct_repr = quote!(
            __from_value::ValueKind::Struct(mut fields) => {
                #field_parse
            }
        );

        let unit_repr = if min_fields == 0 {
            quote!(
                __from_value::ValueKind::Map(fields) if fields.len() == 0 => {
                    #all_default
                }
            )
        } else {
            quote!()
        };
        quote!(
            #struct_repr
            #unit_repr
        )
    }
}

fn fields_unnamed(
    fields: &syn::FieldsUnnamed,
    check_path: Option<TokenStream>,
    self_repr: TokenStream,
) -> TokenStream {
    let error = quote!(__from_value::FromValueErrorKind);

    let mut num_fields: usize = 0;
    let field_iter: Vec<_> = fields
        .unnamed
        .iter()
        .map(|field| {
            let attrs = FieldAttrs::from(&field.attrs);
            if attrs.default {
                panic!("Can't have default attribute on tuple struct");
            }
            if attrs.skip {
                quote!(core::default::Default::default())
            } else {
                num_fields += 1;
                quote!(__from_value::FromValue::from_value(iter.next().unwrap())?)
            }
        })
        .collect();

    let parse_fields = quote!(
        Ok(#self_repr(
            #(#field_iter),*
        ))
    );
    let parse_fields2 = quote!(
        if fields.len() == #num_fields {
            let mut iter = fields.into_iter();
            #parse_fields
        } else {
            core::result::Result::Err(__from_value::FromValueError::new(span, #error::ExpectedAmountOfElements(#num_fields..=#num_fields)))
        }
    );

    if let Some(check_path) = check_path {
        if num_fields == 0 {
            quote!(
                __from_value::ValueKind::Path(path) #check_path => {
                    #parse_fields
                }
                __from_value::ValueKind::NamedTuple(path, mut fields) #check_path => {
                    #parse_fields2
                }
            )
        } else {
            quote!(
                __from_value::ValueKind::NamedTuple(path, mut fields) #check_path => {
                    #parse_fields2
                }
            )
        }
    } else {
        match num_fields {
            0 => quote!(
                __from_value::ValueKind::Tuple(fields) if fields.len() == 0 => {
                    #parse_fields
                }
            ),
            1 => quote!(
                value => {
                    let mut iter = core::iter::once(__from_value::Spanned::new(span, value));
                    #parse_fields
                }
                __from_value::ValueKind::Tuple(mut fields) => {
                    #parse_fields2
                }
            ),
            _ => quote!(
                __from_value::ValueKind::Tuple(mut fields) => {
                    #parse_fields2
                }
            ),
        }
    }
}

pub fn from_value(input: &DeriveInput) -> TokenStream {
    let attrs = ContainerAttrs::from(&input.attrs);

    let real_ident = &input.ident;
    let ident = attrs.rename.as_ref().unwrap_or(real_ident);
    let ident_str = ident.to_string();
    let error = quote!(__from_value::FromValueErrorKind);

    let parse = match &input.data {
        syn::Data::Struct(data) => {
            let check_path = (!attrs.untagged).then(|| {
                quote!(
                    if path.is_struct(std::concat!(std::module_path!(), "::", #ident_str))
                )
            });
            match &data.fields {
                syn::Fields::Named(fields) => {
                    let reprs = fields_named(fields, check_path, quote!(Self));
                    let help_msg = named_help(fields, ident_str);

                    quote!(
                        match value.inner() {
                            #reprs
                            value => core::result::Result::Err(__from_value::FromValueError::new(span, #error::ExpectedPattern(&[#help_msg]))),
                        }
                    )
                }
                syn::Fields::Unnamed(fields) => {
                    let reprs = fields_unnamed(fields, check_path, quote!(Self));
                    let help_msg = unnamed_help(fields, ident_str);
                    quote!(
                        match value.inner() {
                            #reprs
                            value => core::result::Result::Err(__from_value::FromValueError::new(span, #error::ExpectedPattern(&[#help_msg]))),
                        }
                    )
                }
                syn::Fields::Unit => {
                    quote!(
                        match value.inner() {
                            __from_value::ValueKind::Path(path) #check_path => core::result::Result::Ok(Self),
                            value => core::result::Result::Err(__from_value::FromValueError::new(span, #error::ExpectedIdent(#ident_str))),
                        }
                    )
                }
            }
        }
        syn::Data::Enum(data) => {
            let field_cases = data.variants.iter().map(|variant| {
                let variant_attrs = ContainerAttrs::from(&variant.attrs);
                let variant_ident = &variant.ident;
                let variant_str = variant_attrs.rename.map_or(variant_ident.to_string(), |ident| ident.to_string());
                let untagged = attrs.untagged;
                let check_path = if variant_attrs.untagged && attrs.untagged {
                    None
                } else if variant_attrs.untagged {
                    Some(quote!(
                        if path.is_enum::<#untagged>(std::concat!(std::module_path!(), "::", #ident_str))
                    ))
                } else {
                    Some(quote!(
                        if path.is_enum::<#untagged>(std::concat!(std::module_path!(), "::", #ident_str, "::", #variant_str))
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
                        quote!(
                        __from_value::ValueKind::Path(path) #check_path => Ok(Self::#variant_ident),
                    ) } else {
                        quote!(
                            __from_value::ValueKind::Tuple(fields) if fields.len() == 0 => Ok(Self::#variant_ident),
                        )
                    }
                }
            });
            let help_msgs = data.variants.iter().fold(quote!(), |acc, variant| {
                let path = if attrs.untagged {
                    variant.ident.to_string()
                } else {
                    format!("{ident_str}::{}", variant.ident)
                };
                let help_msg = match &variant.fields {
                    syn::Fields::Named(fields) => named_help(fields, path),
                    syn::Fields::Unnamed(fields) => unnamed_help(fields, path),
                    syn::Fields::Unit => path,
                };
                quote!(
                    #acc
                    #help_msg,
                )
            });

            if attrs.untagged {
                let start = quote!(
                    let patterns = &[#help_msgs];
                    core::result::Result::Err(())
                );
                field_cases.fold(start, |acc, cases| {
                    quote!(
                        #acc.or_else(|_| match value.clone().inner() {
                            #cases
                            value => core::result::Result::Err(__from_value::FromValueError::new(span, #error::ExpectedPattern(patterns))),
                        })
                    )
                })
            } else {
                quote!(
                    match value.inner() {
                        #(#field_cases)*
                        value => core::result::Result::Err(__from_value::FromValueError::new(span, #error::ExpectedPattern(&[#help_msgs]))),
                    }
                )
            }
        }
        syn::Data::Union(_) => panic!("Unions aren't supported"),
    };
    let generics = &input.generics;
    let mut generics_bounds = generics.clone();
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
                            syn::PathSegment::from(format_ident!("__from_value")),
                            syn::PathSegment::from(format_ident!("FromValue")),
                        ]),
                    },
                }))
        }
    }

    quote! {
        const _: () = {
            extern crate rsn as __from_value;
            #[automatically_derived]
            impl #generics_bounds __from_value::FromValue for #real_ident #generics {
                fn from_value(value: __from_value::Value) -> core::result::Result<Self, __from_value::FromValueError> {
                    let span = value.span;
                    #parse
                }
            }
        };
    }
}
