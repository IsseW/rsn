
use proc_macro2::Span;
use syn::{PathArguments, PathSegment, Ident, Type, TypePath, Path, token::{Lt, Gt}, punctuated::Punctuated, spanned::Spanned, AngleBracketedGenericArguments, GenericArgument, Lit, LitChar, Expr, ExprLit};


pub fn make_ident_set<I: Iterator<Item = Ident>>(idents: I) -> Type {
    let types = idents.map(|s| make_ident_ty(s).unwrap());
    make_type_set(types)
}

fn empty_set() -> Type {
    let span = Span::call_site();
    Type::Path(TypePath {
        qself: None,
        path: Path {
            leading_colon: None,
            segments: Punctuated::from_iter([
                PathSegment {
                    ident: Ident::new("__rsn", span),
                    arguments: PathArguments::None,
                },
                PathSegment {
                    ident: Ident::new("__types", span),
                    arguments: PathArguments::None,
                },
                PathSegment {
                    ident: Ident::new("EmptySet", span),
                    arguments: PathArguments::None,
                },
            ]),
        },
    })
}

pub fn make_type_set<I: Iterator<Item = Type>>(mut types: I) -> Type {
    let Some(first) = types.next() else {
        return empty_set();
    };
    let init = Type::Path(TypePath {
        qself: None,
        path: Path {
            leading_colon: None,
            segments: Punctuated::from_iter([
                PathSegment {
                    ident: Ident::new("__rsn", first.span()),
                    arguments: PathArguments::None,
                },
                PathSegment {
                    ident: Ident::new("__types", first.span()),
                    arguments: PathArguments::None,
                },
                PathSegment {
                    ident: Ident::new("Type", first.span()),
                    arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        colon2_token: None,
                        lt_token: Lt(first.span()),
                        gt_token: Gt(first.span()),
                        args: Punctuated::from_iter([
                            GenericArgument::Type(first),
                        ]),
                    }),
                },
            ]),
        },
    });
    types.fold(
        init,
        |acc, ty| {
            Type::Path(TypePath {
                qself: None,
                path: Path {
                    leading_colon: None,
                    segments: Punctuated::from_iter([
                        PathSegment {
                            ident: Ident::new("__rsn", ty.span()),
                            arguments: PathArguments::None,
                        },
                        PathSegment {
                            ident: Ident::new("__types", ty.span()),
                            arguments: PathArguments::None,
                        },
                        PathSegment {
                            ident: Ident::new("TypeSet", ty.span()),
                            arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                                colon2_token: None,
                                lt_token: Lt(ty.span()),
                                gt_token: Gt(ty.span()),
                                args: Punctuated::from_iter([
                                    GenericArgument::Type(acc),
                                    GenericArgument::Type(ty),
                                ]),
                            }),
                        },
                    ]),
                },
            })
        }
    )
}

pub fn make_ident_ty(input: Ident) -> Option<Type> {
    let span = input.span();
    let input = input.to_string();
    let mut chars = input.chars();
    let first = chars.next()?;
    
    let init = Type::Path(TypePath {
        qself: None,
        path: Path {
            leading_colon: None,
            segments: Punctuated::from_iter([
                PathSegment {
                    ident: Ident::new("__rsn", span),
                    arguments: PathArguments::None,
                },
                PathSegment {
                    ident: Ident::new("__types", span),
                    arguments: PathArguments::None,
                },
                PathSegment {
                    ident: Ident::new("Char", span),
                    arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        colon2_token: None,
                        lt_token: Lt(span),
                        gt_token: Gt(span),
                        args: Punctuated::from_iter([
                            GenericArgument::Const(Expr::Lit(ExprLit {
                                attrs: vec![],
                                lit: Lit::Char(LitChar::new(first, span)),
                            })),
                        ]),
                    }),
                },
            ]),
        },
    });

    let res = chars.fold(
        init,
        |acc, c| {
            Type::Path(TypePath {
                qself: None,
                path: Path {
                    leading_colon: None,
                    segments: Punctuated::from_iter([
                        PathSegment {
                            ident: Ident::new("__rsn", span),
                            arguments: PathArguments::None,
                        },
                        PathSegment {
                            ident: Ident::new("__types", span),
                            arguments: PathArguments::None,
                        },
                        PathSegment {
                            ident: Ident::new("String", span),
                            arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                                colon2_token: None,
                                lt_token: Lt(span),
                                gt_token: Gt(span),
                                args: Punctuated::from_iter([
                                    GenericArgument::Type(acc),
                                    GenericArgument::Const(Expr::Lit(ExprLit {
                                        attrs: vec![],
                                        lit: Lit::Char(LitChar::new(c, span)),
                                    })),
                                ]),
                            }),
                        },
                    ]),
                },
            })
        }
    );

    Some(res)
}

pub fn union_all<I: Iterator<Item = Type>>(mut sets: I) -> Type {
    let Some(init) = sets.next() else {
        return empty_set();
    };
    sets.fold(init, |acc, ty| {
        Type::Path(TypePath {
            qself: None,
            path: Path {
                leading_colon: None,
                segments: Punctuated::from_iter([
                    PathSegment {
                        ident: Ident::new("__rsn", ty.span()),
                        arguments: PathArguments::None,
                    },
                    PathSegment {
                        ident: Ident::new("__types", ty.span()),
                        arguments: PathArguments::None,
                    },
                    PathSegment {
                        ident: Ident::new("TypeSetUnion", ty.span()),
                        arguments: PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                            colon2_token: None,
                            lt_token: Lt(ty.span()),
                            gt_token: Gt(ty.span()),
                            args: Punctuated::from_iter([
                                GenericArgument::Type(acc),
                                GenericArgument::Type(ty),
                            ]),
                        }),
                    },
                ]),
            },
        })
    })
}