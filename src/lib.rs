#![feature(proc_macro_diagnostic)]

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;

use syn::Item;
use syn::ItemImpl;
use syn::Type;
use syn::TypePath;
use syn::Ident;
use syn::token::Comma;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;

use quote::ToTokens;
use quote::TokenStreamExt;

const UINT: [&str; 6] = ["u8", "u16", "u32", "u64", "u128", "usize"];
const INT: [&str; 6] = ["i8", "i16", "i32", "i64", "i128", "isize"];
const FLOAT: [&str; 2] = ["f32", "f64"];

#[derive(Default)]
struct Attr {
    uint: bool,
    int: bool,
    float: bool,
}

impl Attr {
    pub fn set(&mut self, ident: Ident) -> syn::parse::Result<()> {
        let p = if ident == "uint" {
            &mut self.uint
        } else if ident == "int" {
            &mut self.int
        } else if ident == "float" {
            &mut self.float
        } else {
            return Err(syn::Error::new(ident.span(), "Unknown type set"))
        };

        if *p {
            ident.span().unwrap().warning("Duplicate type set").emit()
        }
        
        *p = true;
        Ok(())
    }
}

impl syn::parse::Parse for Attr {
    fn parse(input: syn::parse::ParseStream) -> syn::parse::Result<Self> {
        let mut attr = Attr::default();
        let list = Punctuated::<Ident, Comma>::parse_separated_nonempty(input)?;
        for ident in list {
            attr.set(ident)?;
        }
        Ok(attr)
    }
}

fn name_to_type(name: &str, span: Span) -> Type {
    Type::from(TypePath { qself: None, path: Ident::new(name, span).into() })
}

fn build_impl(input: &ItemImpl, ty: Type) -> ItemImpl {
    let mut re = input.clone();
    re.self_ty = Box::new(ty);
    re
}

#[proc_macro_attribute]
pub fn lit(attr: TokenStream, input: TokenStream) -> TokenStream {
    let attr = syn::parse_macro_input!(attr as Attr);
    let input = syn::parse_macro_input!(input as Item);

    match input {
        Item::Impl(input) => {
            let span = match &*input.self_ty {
                Type::Infer(infer) => infer.underscore_token.spans[0],
                ty => {
                    ty.span().unwrap().error(format!("Expected _, found {}", ty.into_token_stream())).emit();
                    return input.into_token_stream().into()
                },
            };

            let mut re = proc_macro2::TokenStream::new();

            if attr.uint {
                re.append_all(UINT.iter().map(|name| name_to_type(name, span)).map(|ty| build_impl(&input, ty)));
            }

            if attr.int {
                re.append_all(INT.iter().map(|name| name_to_type(name, span)).map(|ty| build_impl(&input, ty)));
            }

            if attr.float {
                re.append_all(FLOAT.iter().map(|name| name_to_type(name, span)).map(|ty| build_impl(&input, ty)));
            }

            re.into()
        },
        input => {
            input.span().unwrap().error("lit is only allowed on trait impls").emit();
            input.into_token_stream().into()
        },
    }
}