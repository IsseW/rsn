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

fn custom_parser<'a>(chars: &mut Chars<'a>) -> Result<Value<'a>, rsn_parser::ParseError> {
    let c = chars.next_nw()?;

    chars
        .parse_ident(c, chars.place)
        .map(|value| value.map(ValueKind::Custom))
}

pub fn parse(input: &TokenStream) -> syn::Result<TokenStream> {
    let map_err = |err| from_parse_error(err, input);

    let src = input.to_string();
    let src = &src;

    let value = Value::parse_str_with_custom(src, custom_parser).map_err(map_err)?;

    let meta = quote! { &() };

    let value = codify_value(input, value, &meta);

    Ok(quote! {
        {
            extern crate rsn as __rsn;

            #value
        }
    })
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
    fields: rsn_parser::value::Fields,
    meta: &TokenStream,
) -> TokenStream {
    let elems = fields.into_iter().map(|(ident, value)| {
        let ident_span = codify_span(ident.span);
        let ident = ident.inner();
        let value = codify_value(input, value, meta);

        quote! {
            (__rsn::Spanned::new(#ident_span, #ident), #value)
        }
    });
    quote! {
        <__rsn::Fields as ::core::iter::FromIterator<(__rsn::Spanned<&str>, __rsn::Value)>>::from_iter([#(#elems),*])
    }
}

fn codify_value(input: &TokenStream, value: Value, meta: &TokenStream) -> TokenStream {
    let byte_span = value.span.byte_range();
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
        ValueKind::Path(path) => codify_path(path),
        ValueKind::Array(elems) => {
            let elems = elems
                .into_iter()
                .map(|value| codify_value(input, value, meta));

            quote! {
                __rsn::ValueKind::Array(::std::vec![#(#elems),*])
            }
        }
        ValueKind::Map(elems) => {
            let elems = elems.into_iter().map(|(a, b)| {
                let a = codify_value(input, a, meta);
                let b = codify_value(input, b, meta);
                quote! { (#a, #b) }
            });

            quote! {
                __rsn::ValueKind::Map(::std::vec![#(#elems),*])
            }
        }
        ValueKind::Range {
            min,
            max,
            inclusive,
        } => {
            let min = min.map(|value| codify_value(input, *value, meta));
            let min = match min {
                Some(s) => quote! {
                    ::core::option::Option::Some(::std::boxed::Box::new(#s))
                },
                None => quote! { ::core::Option::None },
            };
            let max = max.map(|value| codify_value(input, *value, meta));
            let max = match max {
                Some(s) => quote! {
                    ::core::option::Option::Some(::std::boxed::Box::new(#s))
                },
                None => quote! { ::core::Option::None },
            };

            quote! {
                __rsn::ValueKind::Range { min: #min, max: #max, inclusive: #inclusive }
            }
        }
        ValueKind::Tuple(elems) => {
            let elems = elems
                .into_iter()
                .map(|value| codify_value(input, value, meta));

            quote! {
                __rsn::ValueKind::Tuple(::std::vec![#(#elems),*])
            }
        }
        ValueKind::NamedTuple(path, elems) => {
            let path_span = codify_span(path.span);
            let path = codify_path(path.inner());
            let elems = elems
                .into_iter()
                .map(|value| codify_value(input, value, meta));

            quote! {
                __rsn::ValueKind::NamedTuple(__rsn::Spanned::new(#path_span, #path), ::std::vec![#(#elems),*])
            }
        }
        ValueKind::Struct(elems) => {
            let fields = codify_fields(input, elems, meta);

            quote! {
                __rsn::ValueKind::Struct(#fields)
            }
        }
        ValueKind::NamedStruct(path, fields) => {
            let path_span = codify_span(path.span);
            let path = codify_path(path.inner());

            let fields = codify_fields(input, fields, meta);
            quote! {
                __rsn::ValueKind::NamedStruct(__rsn::Spanned::new(#path_span, #path), #fields)
            }
        }
        ValueKind::Custom(v) => {
            let ident = format_ident!("{v}");

            let span = match_span(input, byte_span);

            return quote::quote_spanned! {
                span =>
                __rsn::ToValue::to_value(&#ident, #meta)
            };
        }
    };

    quote! {
        __rsn::Spanned::new(#span, #v)
    }
}
