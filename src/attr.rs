use syn::Type;
use syn::TypePath;
use syn::Path;
use syn::PathSegment;
use syn::PathArguments;
use syn::Ident;
use syn::parse::{ self, Parse, ParseStream };
use syn::punctuated::Punctuated;
use syn::token::Comma;
use proc_macro::Span;
use std::slice::Iter;

#[derive(Default)]
pub struct Attr {
    pub types: Vec<Type>,
}

impl Parse for Attr {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        let list = Punctuated::<Type, Comma>::parse_terminated(input)?;
        if list.is_empty() {
            Span::call_site().warning("lit won't generate any impls").emit()
        }

        Ok(Attr { types: list.into_iter().flat_map(flattern_set).collect() })
    }
}

enum SetIterator {
    Type(Option<Type>),
    Set(proc_macro2::Span, Iter<'static, &'static str>),
}

impl Iterator for SetIterator {
    type Item = Type;
    fn next(&mut self) -> Option<Type> {
        match self {
            SetIterator::Type(ty) => ty.take(),
            SetIterator::Set(span, iter) => iter.next().map(|n|Type::Path(TypePath { qself: None, path: Ident::new(n, *span).into() })),
        }
    }
}

const UINT: [&'static str; 6] = ["u8", "u16", "u32", "u64", "u128", "usize"];
const INT: [&'static str; 6] = ["i8", "i16", "i32", "i64", "i128", "isize"];
const FLOAT: [&'static str; 2] = ["f32", "f64"];

fn flattern_set(ty: Type) -> impl IntoIterator<Item = Type> {
    if let Type::Path(TypePath { qself: None, path: Path { leading_colon: None, segments } }) = &ty {
        if segments.len() == 1 {
            if let PathSegment { ident, arguments: PathArguments::None } = segments.last().unwrap().into_value() {
                if ident == "u_" {
                    return SetIterator::Set(ident.span(), UINT.iter())
                }
                if ident == "i_" {
                    return SetIterator::Set(ident.span(), INT.iter())
                }
                if ident == "f_" {
                    return SetIterator::Set(ident.span(), FLOAT.iter())
                }
            }
        }
    }
    SetIterator::Type(Some(ty))
}
