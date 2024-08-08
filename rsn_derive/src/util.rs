use quote::format_ident;
use syn::{punctuated::Punctuated, spanned::Spanned};

#[allow(dead_code)]
pub enum ContainerAttr {
    Untagged(syn::Ident),
    Rename(syn::Ident, syn::Token![=], syn::Ident),
    WithMeta(syn::Ident, syn::Token![=], syn::Type),
    WithCustom(syn::Ident, syn::Token![=], syn::Type),
}

impl syn::parse::Parse for ContainerAttr {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        let tag: syn::Ident = input.parse()?;

        Ok(match tag.to_string().as_str() {
            "untagged" => ContainerAttr::Untagged(tag),
            "rename" => ContainerAttr::Rename(tag, input.parse()?, input.parse()?),
            "with_meta" => ContainerAttr::WithMeta(tag, input.parse()?, input.parse()?),
            "with_custom" => ContainerAttr::WithCustom(tag, input.parse()?, input.parse()?),

            _ => return Err(syn::Error::new(tag.span(), "Unexpected tag")),
        })
    }
}

#[derive(Default)]
pub struct ContainerAttrs {
    pub untagged: Option<syn::Ident>,
    pub rename: Option<syn::Ident>,
    pub with_meta: Option<syn::Type>,
    pub with_custom: Option<syn::Type>,
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
                            if this.untagged.is_some() {
                                return Err(syn::Error::new(tag.span(), "Multiple untagged tags"));
                            }
                            this.untagged = Some(tag);
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
                        ContainerAttr::WithCustom(tag, _, with_custom) => {
                            if this.with_custom.is_some() {
                                return Err(syn::Error::new(
                                    tag.span(),
                                    "Multiple with_custom tags",
                                ));
                            }
                            this.with_custom = Some(with_custom);
                        }
                    }
                }
            }
        }
        Ok(this)
    }
}

#[derive(PartialEq, Clone)]
pub enum FieldModifier {
    Flatten,
    Default,
    Skip,
    WithExpr(syn::Token![=], syn::Expr),
}

impl FieldModifier {
    pub fn as_str(&self) -> &str {
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
    SkipBound(syn::Ident),
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
            "skip_bound" => FieldAttr::SkipBound(tag),

            _ => return Err(syn::Error::new(tag.span(), "Unexpected tag")),
        })
    }
}

#[derive(Default)]
pub struct FieldAttrs {
    pub modifier: Option<FieldModifier>,
    pub rename: Option<syn::Ident>,
    pub skip_bound: bool,
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
                        FieldAttr::SkipBound(tag) => {
                            if this.skip_bound {
                                return Err(syn::Error::new(
                                    tag.span(),
                                    "Multiple skip bound tags",
                                ));
                            }

                            this.skip_bound = true;
                        }
                    }
                }
            }
        }
        Ok(this)
    }
}

pub type ValueNamedFields = Vec<(FieldAttrs, syn::Ident, syn::Type)>;
pub type ValueUnnamedFields = Vec<(FieldAttrs, syn::Type)>;
pub enum ValueFields {
    Unit,
    Unnamed(ValueUnnamedFields),
    Named(ValueNamedFields),
}

impl ValueFields {
    pub fn from_fields(fields: &syn::Fields) -> syn::Result<Self> {
        match fields {
            syn::Fields::Named(fields) => Ok(ValueFields::Named(
                fields
                    .named
                    .iter()
                    .map(|field| {
                        syn::Result::Ok((
                            FieldAttrs::try_from(&field.attrs)?,
                            field.ident.as_ref().unwrap().clone(),
                            field.ty.clone(),
                        ))
                    })
                    .try_collect()?,
            )),
            syn::Fields::Unnamed(fields) => Ok(ValueFields::Unnamed(
                fields
                    .unnamed
                    .iter()
                    .map(|field| {
                        syn::Result::Ok((FieldAttrs::try_from(&field.attrs)?, field.ty.clone()))
                    })
                    .try_collect()?,
            )),
            syn::Fields::Unit => Ok(ValueFields::Unit),
        }
    }
}

pub type ValueVariants = Vec<(ContainerAttrs, syn::Ident, ValueFields)>;
pub enum ValueData {
    Struct(ValueFields),
    Enum(ValueVariants),
}

impl ValueData {
    pub fn from_data(data: &syn::Data) -> syn::Result<Self> {
        match data {
            syn::Data::Struct(data) => {
                Ok(ValueData::Struct(ValueFields::from_fields(&data.fields)?))
            }
            syn::Data::Enum(data) => Ok(ValueData::Enum(
                data.variants
                    .iter()
                    .map(|variant| {
                        syn::Result::Ok((
                            ContainerAttrs::try_from(&variant.attrs)?,
                            variant.ident.clone(),
                            ValueFields::from_fields(&variant.fields)?,
                        ))
                    })
                    .try_collect()?,
            )),
            syn::Data::Union(data) => Err(syn::Error::new(
                data.union_token.span,
                "Unions aren't supported",
            )),
        }
    }
}

pub struct ValueDeriveInput {
    pub attrs: ContainerAttrs,
    pub data: ValueData,
    pub ident: syn::Ident,
    pub real_ident: syn::Ident,
    pub ident_str: String,
    pub generics: syn::Generics,
    pub modified_generics: syn::Generics,
    pub where_clause: Option<syn::WhereClause>,
    pub meta_type: syn::Type,
    pub custom_type: syn::Type,
}

impl ValueDeriveInput {
    pub fn from_input(
        input: &syn::DeriveInput,
        trait_ident: &str,
        flatten_named_trait_ident: &str,
        flatten_unnamed_trait_ident: &str,
    ) -> syn::Result<Self> {
        let attrs = ContainerAttrs::try_from(&input.attrs)?;
        let data = ValueData::from_data(&input.data)?;

        let real_ident = &input.ident;
        let ident = attrs.rename.as_ref().unwrap_or(real_ident);
        let ident_str = ident.to_string();
        let generics = &input.generics;

        let mut modified_generics = generics.clone();
        let meta_type = if let Some(ty) = attrs.with_meta.clone() {
            ty
        } else {
            syn::Type::Path(syn::TypePath {
                qself: None,
                path: syn::Path {
                    leading_colon: None,
                    segments: Punctuated::from_iter([syn::PathSegment::from(format_ident!(
                        "_Rsn_Meta"
                    ))]),
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
                    segments: Punctuated::from_iter([syn::PathSegment::from(format_ident!(
                        "_Rsn_CustomType"
                    ))]),
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
                segments: Punctuated::from_iter(path.segments.iter().map(|segment| {
                        syn::Result::Ok(syn::PathSegment {
                            arguments: match &segment.arguments {
                                syn::PathArguments::None => syn::PathArguments::None,
                                syn::PathArguments::AngleBracketed(args) => syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                                    args: Punctuated::from_iter(args.args.iter().map(|arg| {
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
                                        inputs: Punctuated::from_iter(
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
                    inputs: Punctuated::from_iter(
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
                syn::Type::TraitObject(object) => {
                    Ok(syn::Type::TraitObject(syn::TypeTraitObject {
                        bounds: Punctuated::from_iter(
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
                    }))
                }
                syn::Type::Tuple(tuple) => Ok(syn::Type::Tuple(syn::TypeTuple {
                    elems: Punctuated::from_iter(
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
        let parameterized_meta_type = parameterize_type_lifetimes(&meta_type, &mut lifetime_count)?;
        let lifetimes = (0..lifetime_count)
            .map(|i| syn::Lifetime {
                apostrophe: meta_type.span(),
                ident: lifetime_ident(i),
            })
            .collect::<Vec<_>>();
        let meta_bound_lifetimes = syn::BoundLifetimes {
            for_token: syn::Token![for](meta_type.span()),
            lt_token: syn::Token![<](meta_type.span()),
            lifetimes: Punctuated::from_iter(lifetimes.iter().map(|lifetime| syn::LifetimeDef {
                attrs: Vec::new(),
                lifetime: lifetime.clone(),
                colon_token: None,
                bounds: Punctuated::new(),
            })),
            gt_token: syn::Token![>](meta_type.span()),
        };

        for param in &mut modified_generics.params {
            if let syn::GenericParam::Type(param) = param {
                param
                    .bounds
                    .push(syn::TypeParamBound::Trait(syn::TraitBound {
                        paren_token: None,
                        modifier: syn::TraitBoundModifier::None,
                        lifetimes: Some(meta_bound_lifetimes.clone()),
                        path: syn::Path {
                            leading_colon: None,
                            segments: Punctuated::from_iter([
                                syn::PathSegment::from(format_ident!("__rsn")),
                                syn::PathSegment {
                                    ident: format_ident!("{trait_ident}"),
                                    arguments: syn::PathArguments::AngleBracketed(
                                        syn::AngleBracketedGenericArguments {
                                            colon2_token: None,
                                            lt_token: syn::Token![<](param.span()),
                                            args: Punctuated::from_iter([
                                                syn::GenericArgument::Type(
                                                    parameterized_meta_type.clone(),
                                                ),
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
                modified_generics
                    .params
                    .push(syn::GenericParam::Type(syn::TypeParam {
                        attrs: Vec::new(),
                        ident: format_ident!("_Rsn_Meta"),
                        colon_token: None,
                        bounds: Punctuated::new(),
                        eq_token: None,
                        default: None,
                    }));
            }
            if attrs.with_custom.is_none() {
                modified_generics
                    .params
                    .push(syn::GenericParam::Type(syn::TypeParam {
                        attrs: Vec::new(),
                        ident: format_ident!("_Rsn_CustomType"),
                        colon_token: None,
                        bounds: Punctuated::new(),
                        eq_token: None,
                        default: None,
                    }));
            }
            let where_clause = modified_generics.make_where_clause();

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

            let mut add_type_bound = |ty: &syn::Type, trait_ident: &str| {
                if skip_bound(ty, real_ident) {
                    return;
                }
                where_clause
                    .predicates
                    .push(syn::WherePredicate::Type(syn::PredicateType {
                        lifetimes: None,
                        bounded_ty: ty.clone(),
                        colon_token: syn::Token![:](ty.span()),
                        bounds: Punctuated::from_iter([syn::TypeParamBound::Trait(
                            syn::TraitBound {
                                paren_token: None,
                                modifier: syn::TraitBoundModifier::None,
                                lifetimes: Some(meta_bound_lifetimes.clone()),
                                path: syn::Path {
                                    leading_colon: None,
                                    segments: Punctuated::from_iter([
                                        syn::PathSegment::from(format_ident!("__rsn")),
                                        syn::PathSegment {
                                            ident: format_ident!("{trait_ident}"),
                                            arguments: syn::PathArguments::AngleBracketed(
                                                syn::AngleBracketedGenericArguments {
                                                    colon2_token: None,
                                                    lt_token: syn::Token![<](ty.span()),
                                                    args: Punctuated::from_iter([
                                                        syn::GenericArgument::Type(
                                                            parameterized_meta_type.clone(),
                                                        ),
                                                        syn::GenericArgument::Type(
                                                            custom_type.clone(),
                                                        ),
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

            let mut add_field_bounds = |fields: &ValueFields| match fields {
                ValueFields::Named(fields) => {
                    for (attrs, _, ty) in fields {
                        let trait_ident = match attrs.modifier {
                            None | Some(FieldModifier::Default) => trait_ident,
                            Some(FieldModifier::Flatten) => flatten_named_trait_ident,
                            Some(FieldModifier::Skip | FieldModifier::WithExpr(..)) => continue,
                        };

                        add_type_bound(ty, trait_ident)
                    }
                }
                ValueFields::Unnamed(fields) => {
                    for (attrs, ty) in fields {
                        let trait_ident = match attrs.modifier {
                            None | Some(FieldModifier::Default) => trait_ident,
                            Some(FieldModifier::Flatten) => flatten_unnamed_trait_ident,
                            Some(FieldModifier::Skip | FieldModifier::WithExpr(..)) => continue,
                        };

                        add_type_bound(ty, trait_ident)
                    }
                }
                ValueFields::Unit => {}
            };

            match &data {
                ValueData::Struct(fields) => add_field_bounds(fields),
                ValueData::Enum(variants) => {
                    for (_, _, fields) in variants {
                        add_field_bounds(fields)
                    }
                }
            }
        }
        let where_clause = modified_generics.where_clause.clone();
        Ok(Self {
            ident: ident.clone(),
            data,
            ident_str,
            real_ident: real_ident.clone(),
            generics: generics.clone(),
            modified_generics: modified_generics.clone(),
            where_clause,
            meta_type,
            custom_type,
            attrs,
        })
    }
}
