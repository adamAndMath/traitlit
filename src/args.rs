use syn::Attribute;
use syn::Type;
use syn::TypePath;
use syn::Ident;
use syn::parse::{ self, Parse, ParseStream };
use syn::punctuated::Punctuated;
use syn::Token;
use syn::bracketed;
use syn::token::Bracket;
use syn::punctuated::IntoIter;
use std::slice::Iter;
use proc_macro2::Span;

pub struct WithAttr<T>(pub Vec<Attribute>, pub T);

impl<T: Parse> Parse for WithAttr<T> {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        Ok(WithAttr(input.call(Attribute::parse_outer)?, input.parse()?))
    }
}

enum LitSet {
    Named(Ident),
    Set(Bracket, Punctuated<WithAttr<Type>, Token!(,)>)
}

impl Parse for LitSet {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        if input.peek(Ident) {
            input.parse().map(LitSet::Named)
        } else {
            let content;
            Ok(LitSet::Set(bracketed!(content in input), Punctuated::parse_terminated(&content)?))
        }
    }
}

impl IntoIterator for WithAttr<LitSet> {
    type Item = WithAttr<Type>;
    type IntoIter = SetIterator;
    fn into_iter(self) -> SetIterator {
        match self {
            WithAttr(attrs, LitSet::Named(ident)) => {
                if ident == "u_" {
                    SetIterator::Named(attrs, ident.span(), UINT.iter())
                } else if ident == "i_" {
                    SetIterator::Named(attrs, ident.span(), INT.iter())
                } else if ident == "f_" {
                    SetIterator::Named(attrs, ident.span(), FLOAT.iter())
                } else {
                    ident.span().unwrap().error("Unknown type set").emit();
                    SetIterator::Set(attrs, Punctuated::new().into_iter())
                }
            },
            WithAttr(attrs, LitSet::Set(_, types)) => SetIterator::Set(attrs, types.into_iter()),
        }
    }
}

const UINT: [&'static str; 6] = ["u8", "u16", "u32", "u64", "u128", "usize"];
const INT: [&'static str; 6] = ["i8", "i16", "i32", "i64", "i128", "isize"];
const FLOAT: [&'static str; 2] = ["f32", "f64"];

enum SetIterator {
    Named(Vec<Attribute>, Span, Iter<'static, &'static str>),
    Set(Vec<Attribute>, IntoIter<WithAttr<Type>, Token!(,)>),
}

impl Iterator for SetIterator {
    type Item = WithAttr<Type>;
    fn next(&mut self) -> Option<Self::Item> {
        match self {
            SetIterator::Set(attrs, types) => types.next().map(|WithAttr(a, t)|WithAttr({let mut attrs = attrs.clone(); attrs.extend(a); attrs}, t)),
            SetIterator::Named(attrs, span, iter) => iter.next().map(|n|WithAttr(attrs.clone(), Type::Path(TypePath { qself: None, path: Ident::new(n, *span).into() }))),
        }
    }
}

pub struct Args {
    pub ident: Ident,
    _equal: Token!(=),
    pub types: Vec<WithAttr<Type>>,
}

impl Parse for Args {
    fn parse(input: ParseStream) -> parse::Result<Self> {
        Ok(Args {
            ident: input.parse()?,
            _equal: input.parse()?,
            types: Punctuated::<WithAttr<LitSet>, Token!(+)>::parse_separated_nonempty(input)?.into_iter().flatten().collect()
        })
    }
}
