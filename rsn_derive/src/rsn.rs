use std::ops::Range;

use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use rsn_parser::{
    value::{Value, ValueKind},
    Chars,
};
use syn::spanned::Spanned;

fn match_span(
    input: &proc_macro2::TokenStream,
    wanted_bytes_local: Range<usize>,
) -> proc_macro2::Span {
    let input_span = input.span();
    let bytes = input_span.unwrap().byte_range();

    let wanted_bytes = bytes.start + wanted_bytes_local.start..bytes.start + wanted_bytes_local.end;
    if bytes == wanted_bytes {
        return input_span;
    }
    let mut input_iter = input.clone().into_iter();

    fn match_span_start(
        input: &mut impl Iterator<Item = proc_macro2::TokenTree>,
        wanted_bytes: &Range<usize>,
    ) -> Option<proc_macro2::TokenTree> {
        let mut last = None;
        for tk in input.by_ref() {
            let span = tk.span();

            let bytes = span.unwrap().byte_range();
            if bytes.start < wanted_bytes.start && wanted_bytes.start <= bytes.end {
                match tk {
                    proc_macro2::TokenTree::Group(group) => {
                        return match_span_start(&mut group.stream().into_iter(), wanted_bytes);
                    }
                    tk => return Some(tk),
                }
            } else if bytes.start == wanted_bytes.start {
                return Some(tk);
            }
            last = Some(tk);
        }

        last
    }

    fn match_span_end(
        input: &mut impl Iterator<Item = proc_macro2::TokenTree>,
        wanted_bytes: &Range<usize>,
    ) -> Option<proc_macro2::TokenTree> {
        let mut last = None;
        for tk in input.by_ref() {
            let span = tk.span();

            let bytes = span.unwrap().byte_range();
            if bytes.start < wanted_bytes.end && wanted_bytes.end <= bytes.end {
                match tk {
                    proc_macro2::TokenTree::Group(group) => {
                        return match_span_start(&mut group.stream().into_iter(), wanted_bytes);
                    }
                    tk => return Some(tk),
                }
            } else if bytes.end == wanted_bytes.end {
                return Some(tk);
            }
            last = Some(tk);
        }

        last
    }

    if let Some(start) = match_span_start(&mut input_iter, &wanted_bytes) {
        let start_span = start.span();
        let bytes = start_span.unwrap().byte_range();
        if bytes.end >= wanted_bytes.end {
            return start_span;
        }

        if let Some(end) = match_span_end(&mut input_iter, &wanted_bytes) {
            let end_span = end.span();

            start_span
                .join(end_span)
                .expect("These are in the same file")
        } else {
            input_span
        }
    } else {
        input_span
    }
}

fn from_parse_error(err: rsn_parser::ParseError, input: &TokenStream) -> syn::Error {
    syn::Error::new(match_span(input, err.span.byte_range()), err.value)
}

enum Expand<'a> {
    Variable(&'a str),
    Array {
        variables: Vec<rsn_parser::spanned::Spanned<&'a str>>,
        expr: rsn_parser::spanned::Spanned<&'a str>,
    },
    Tuple {
        name: Option<rsn_parser::value::Path<'a>>,
        variables: Vec<rsn_parser::spanned::Spanned<&'a str>>,
        expr: rsn_parser::spanned::Spanned<&'a str>,
    },
    Map {
        variables: Vec<rsn_parser::spanned::Spanned<&'a str>>,
        key_expr: rsn_parser::spanned::Spanned<&'a str>,
        value_expr: rsn_parser::spanned::Spanned<&'a str>,
    },
    Struct {
        name: Option<rsn_parser::value::Path<'a>>,
        variables: Vec<rsn_parser::spanned::Spanned<&'a str>>,
        field_expr: rsn_parser::spanned::Spanned<&'a str>,
        value_expr: rsn_parser::spanned::Spanned<&'a str>,
    },
}

fn custom_parser<'a>(
    chars: &mut Chars<'a>,
) -> Result<Value<'a, Expand<'a>>, rsn_parser::ParseError> {
    let start = chars.next_nw()?;
    let name = if start == '#' {
        None
    } else {
        Some(chars.parse_path(start, chars.place)?)
    };

    let start = chars.next_nw()?;
    let start_place = chars.next_place();
    if let Some(end) = match start {
        '(' => Some(')'),
        '[' => Some(']'),
        '{' => Some('}'),
        _ => None,
    } {
        let mut depth = 0;
        let mut center_last = start_place;
        let mut center_start = start_place;
        let mut center_end = start_place;
        let mut center_next = start_place;
        let mut variables = Vec::new();
        let mut last_place = start_place;
        while let Ok(c) = chars.next_c() {
            if depth == 0 {
                if start == '{' {
                    match c {
                        ':' => {
                            center_last = last_place;
                            center_start = chars.place;
                            center_end = chars.place;
                            center_next = chars.next_place();
                        }
                        '=' => {
                            let cs = chars.place;
                            if chars.assume_next('>').is_ok() {
                                center_last = last_place;
                                center_start = cs;
                                center_end = chars.place;
                                center_next = chars.next_place();
                            }
                        }
                        _ => {}
                    }
                }
                if c == '#' {
                    if let Ok(c) = chars.next_nw_matches(|c| *c == '_' || c.is_alphabetic()) {
                        let ident = chars.parse_ident(c, chars.place)?;
                        variables.push(ident);
                    }
                }
            }
            if c == end {
                if depth == 0 {
                    break;
                } else {
                    depth -= 1;
                }
            }
            if c == start {
                depth += 1;
            }
            last_place = chars.place;
        }
        let end_place = last_place;
        chars.assume_next_nw('*')?;

        let p = &chars.src(center_start, center_end);

        match start {
            '(' => Ok(chars.spanned(
                start_place,
                ValueKind::Custom(Expand::Tuple {
                    name,
                    variables,
                    expr: chars.src(start_place, end_place),
                }),
            )),
            '[' if name.is_none() => Ok(chars.spanned(
                start_place,
                ValueKind::Custom(Expand::Array {
                    variables,
                    expr: chars.src(start_place, end_place),
                }),
            )),
            '{' if p.value == ":" => Ok(chars.spanned(
                start_place,
                ValueKind::Custom(Expand::Struct {
                    name,
                    variables,
                    field_expr: chars.src(start_place, center_last),
                    value_expr: chars.src(center_next, end_place),
                }),
            )),
            '{' if p.value == "=>" => Ok(chars.spanned(
                start_place,
                ValueKind::Custom(Expand::Map {
                    variables,
                    key_expr: chars.src(start_place, center_last),
                    value_expr: chars.src(center_next, end_place),
                }),
            )),
            '{' => Err(chars.error(
                start_place,
                rsn_parser::Error::Custom("Expected to find a seperator ':' or '=>'".to_string()),
            )),
            _ => unreachable!("We have matched on everything we enter on here"),
        }
    } else if start.is_alphabetic() || start == '_' {
        chars
            .parse_ident(start, chars.place)
            .map(|value| value.map(|value| ValueKind::Custom(Expand::Variable(value))))
    } else {
        Err(chars.error1(rsn_parser::Error::UnexpectedSymbol))
    }
}

pub fn parse(input: &TokenStream) -> syn::Result<TokenStream> {
    let map_err = |err| from_parse_error(err, input);

    let input_src = input.to_string();
    let mut input_src = input_src.trim();
    let mut meta = quote! { &() };
    let mut import = quote! { extern crate rsn as __rsn; };
    if let Some(src) = input_src.strip_prefix('@') {
        import = quote! { use crate as __rsn; };

        input_src = src.trim_start();
    }
    if let Some(src) = input_src.strip_prefix('(') {
        let src = src.trim_start();
        let src_no_ident = src.trim_start_matches(|c: char| c.is_alphabetic() || c == '_');
        let l = src.len() - src_no_ident.len();
        let ident = &src[..l];
        let src = src_no_ident.trim_start();
        if let Some(src) = src
            .strip_prefix(')')
            .and_then(|src| src.trim_start().strip_prefix("=>"))
        {
            input_src = src.trim_start();
            let ident = format_ident!("{ident}");
            meta = quote! { #ident };
        }
    }
    // return Ok(quote!(#input_src));
    let value = parse_inner(input_src, 0).map_err(map_err)?;

    let value = codify_value(input, value, &meta, true).map_err(map_err)?;

    Ok(quote! {
        {
            #import

            #value
        }
    })
}

fn parse_inner(
    src: &str,
    byte_offset: usize,
) -> Result<rsn_parser::value::Value<'_, Expand>, rsn_parser::ParseError> {
    let map_err = |mut err: rsn_parser::ParseError| {
        err.span.start.byte_start += byte_offset;
        err.span.start.byte_end += byte_offset;
        err.span.end.byte_start += byte_offset;
        err.span.end.byte_end += byte_offset;
        err
    };
    Value::parse_str_with_custom(src, custom_parser).map_err(map_err)
}

fn codify_position(position: rsn_parser::spanned::Position) -> TokenStream {
    let byte_start = position.byte_start;
    let byte_end = position.byte_end;
    let line = position.line;
    let column = position.column;

    quote! {
        __rsn::Position {
            byte_start: #byte_start,
            byte_end: #byte_end,
            line: #line,
            column: #column,
        }
    }
}

fn codify_span(span: rsn_parser::spanned::Span) -> TokenStream {
    let start = codify_position(span.start);
    let end = codify_position(span.end);
    quote! {
        __rsn::Span::new(#start, #end)
    }
}

fn codify_path(path: rsn_parser::value::Path) -> TokenStream {
    let leading = path.leading;

    let idents = path.idents.into_iter().map(|ident| {
        let span = codify_span(ident.span);
        let ident = ident.inner();
        quote! {
            __rsn::Spanned::new(#span, #ident)
        }
    });

    quote! {
        __rsn::Path {
            leading: #leading,
            idents: ::std::vec![#(#idents),*],
        }
    }
}

fn codify_fields(
    input: &TokenStream,
    fields: rsn_parser::value::Fields<Expand>,
    meta: &TokenStream,
    has_ref: bool,
) -> Result<TokenStream, rsn_parser::ParseError> {
    let elems = fields
        .into_iter()
        .map(|(ident, value)| {
            let ident_span = codify_span(ident.span);
            let ident = ident.inner();
            let value = codify_value(input, value, meta, has_ref)?;

            Ok(quote! {
                (__rsn::Spanned::new(#ident_span, #ident), #value)
            })
        })
        .try_collect::<Vec<_>>()?;
    Ok(quote! {
        <__rsn::Fields<_> as ::core::iter::FromIterator<(__rsn::Spanned<&str>, __rsn::Value<_>)>>::from_iter([#(#elems),*])
    })
}

fn codify_value(
    input: &TokenStream,
    value: Value<Expand>,
    meta: &TokenStream,
    has_ref: bool,
) -> Result<TokenStream, rsn_parser::ParseError> {
    let span = codify_span(value.span);
    let v = match value.inner() {
        ValueKind::Integer(i) => {
            quote! {
                __rsn::ValueKind::Integer(#i)
            }
        }
        ValueKind::Float(f) => {
            quote! {
                __rsn::ValueKind::Float(#f)
            }
        }
        ValueKind::Bool(b) => {
            quote! {
                __rsn::ValueKind::Bool(#b)
            }
        }
        ValueKind::String(s) => {
            quote! {
                __rsn::ValueKind::String(::std::borrow::Cow::Borrowed(#s))
            }
        }
        ValueKind::Char(c) => {
            quote! {
                __rsn::ValueKind::Char(#c)
            }
        }
        ValueKind::Path(path) => {
            let path = codify_path(path);
            quote! { __rsn::ValueKind::Path(#path) }
        }
        ValueKind::Array(elems) => {
            let elems = elems
                .into_iter()
                .map(|value| codify_value(input, value, meta, has_ref))
                .try_collect::<Vec<_>>()?;

            quote! {
                __rsn::ValueKind::Array(::std::vec![#(#elems),*])
            }
        }
        ValueKind::Map(elems) => {
            let elems = elems
                .into_iter()
                .map(|(a, b)| {
                    let a = codify_value(input, a, meta, has_ref)?;
                    let b = codify_value(input, b, meta, has_ref)?;
                    Ok(quote! { (#a, #b) })
                })
                .try_collect::<Vec<_>>()?;

            quote! {
                __rsn::ValueKind::Map(::std::vec![#(#elems),*])
            }
        }
        ValueKind::Range {
            min,
            max,
            inclusive,
        } => {
            let min = min
                .map(|value| codify_value(input, *value, meta, has_ref))
                .transpose()?;
            let min = match min {
                Some(s) => quote! {
                    ::core::option::Option::Some(::std::boxed::Box::new(#s))
                },
                None => quote! { ::core::option::Option::None },
            };
            let max = max
                .map(|value| codify_value(input, *value, meta, has_ref))
                .transpose()?;
            let max = match max {
                Some(s) => quote! {
                    ::core::option::Option::Some(::std::boxed::Box::new(#s))
                },
                None => quote! { ::core::option::Option::None },
            };

            quote! {
                __rsn::ValueKind::Range { min: #min, max: #max, inclusive: #inclusive }
            }
        }
        ValueKind::Tuple(elems) => {
            let elems = elems
                .into_iter()
                .map(|value| codify_value(input, value, meta, has_ref))
                .try_collect::<Vec<_>>()?;

            quote! {
                __rsn::ValueKind::Tuple(::std::vec![#(#elems),*])
            }
        }
        ValueKind::NamedTuple(path, elems) => {
            let path_span = codify_span(path.span);
            let path = codify_path(path.inner());
            let elems = elems
                .into_iter()
                .map(|value| codify_value(input, value, meta, has_ref))
                .try_collect::<Vec<_>>()?;

            quote! {
                __rsn::ValueKind::NamedTuple(__rsn::Spanned::new(#path_span, #path), ::std::vec![#(#elems),*])
            }
        }
        ValueKind::Struct(elems) => {
            let fields = codify_fields(input, elems, meta, has_ref)?;

            quote! {
                __rsn::ValueKind::Struct(#fields)
            }
        }
        ValueKind::NamedStruct(path, fields) => {
            let path_span = codify_span(path.span);
            let path = codify_path(path.inner());

            let fields = codify_fields(input, fields, meta, has_ref)?;
            quote! {
                __rsn::ValueKind::NamedStruct(__rsn::Spanned::new(#path_span, #path), #fields)
            }
        }
        ValueKind::Custom(v) => {
            let expand_iter = |variables: Vec<rsn_parser::spanned::Spanned<&str>>| {
                let iter = variables
                    .iter()
                    .map(|v| {
                        let v = format_ident!("{}", v.value);
                        quote! { ::core::iter::IntoIterator::into_iter(#v) }
                    })
                    .fold(quote! { ::core::iter::repeat(()) }, |acc, elem| {
                        quote! {
                            ::core::iter::zip(#acc, #elem)
                        }
                    });
                let iter_args = variables
                    .iter()
                    .map(|v| {
                        let v = format_ident!("{}", v.value);
                        quote! { #v }
                    })
                    .fold(quote! { () }, |acc, elem| {
                        quote! { (#acc, #elem) }
                    });
                (iter, iter_args)
            };
            let expand_expr = |expr: rsn_parser::spanned::Spanned<&str>| {
                let value = parse_inner(expr.value, expr.span.start.byte_start)?;

                codify_value(input, value, meta, true)
            };
            let combine = |value_kind, name, iter, iter_args, expr| {
                if let Some(name) = name {
                    quote! {
                        __rsn::ValueKind::#value_kind(#name, ::core::iter::Iterator::collect(::core::iter::Iterator::map(#iter, |#iter_args| {
                            #expr
                        })))
                    }
                } else {
                    quote! {
                        __rsn::ValueKind::#value_kind(::core::iter::Iterator::collect(::core::iter::Iterator::map(#iter, |#iter_args| {
                            #expr
                        })))
                    }
                }
            };
            match v {
                Expand::Variable(var) => {
                    let ident = format_ident!("{var}");
                    let ident = if has_ref {
                        quote! { #ident }
                    } else {
                        quote! {
                            &#ident
                        }
                    };
                    return Ok(quote::quote! {
                        __rsn::ToValue::<_, _>::to_value(#ident, #meta)
                    });
                }
                Expand::Array { variables, expr } => {
                    // panic!("{variables:?} {expr:?}");
                    let (iter, iter_args) = expand_iter(variables);
                    let expr = expand_expr(expr)?;
                    combine(quote! { Array }, None, iter, iter_args, expr)
                }
                Expand::Tuple {
                    name,
                    variables,
                    expr,
                } => {
                    let (iter, iter_args) = expand_iter(variables);
                    let expr = expand_expr(expr)?;
                    if let Some(name) = name {
                        let name = codify_path(name);
                        combine(quote! { NamedTuple }, Some(name), iter, iter_args, expr)
                    } else {
                        combine(quote! { Tuple }, None, iter, iter_args, expr)
                    }
                }
                Expand::Map {
                    variables,
                    key_expr,
                    value_expr,
                } => {
                    let (iter, iter_args) = expand_iter(variables);
                    let key_expr = expand_expr(key_expr)?;
                    let value_expr = expand_expr(value_expr)?;

                    let expr = quote! { (#key_expr, #value_expr) };

                    combine(quote! { Map }, None, iter, iter_args, expr)
                }
                Expand::Struct {
                    name,
                    variables,
                    field_expr,
                    value_expr,
                } => {
                    let (iter, iter_args) = expand_iter(variables);

                    let field_expr = field_expr.map(|field_expr| {
                        let field_expr = field_expr.trim();
                        if let Some(field_expr) = field_expr.strip_prefix('#') {
                            let ident = format_ident!("{}", field_expr);
                            quote! { #ident }
                        } else {
                            quote! { field_expr }
                        }
                    });
                    let field_span = codify_span(field_expr.span);
                    let field_expr = field_expr.value;
                    let field_expr = quote! { __rsn::Spanned::new(#field_span, #field_expr) };

                    let value_expr = expand_expr(value_expr)?;

                    let expr = quote! { (#field_expr, #value_expr) };

                    if let Some(name) = name {
                        let name = codify_path(name);
                        combine(quote! { NamedStruct }, Some(name), iter, iter_args, expr)
                    } else {
                        combine(quote! { Struct }, None, iter, iter_args, expr)
                    }
                }
            }
        }
    };

    Ok(quote! {
        __rsn::Spanned::new(#span, #v)
    })
}
