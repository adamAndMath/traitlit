#![feature(proc_macro_diagnostic)]

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::Span;

use syn::Item;
use syn::ItemImpl;
use syn::ImplItem;
use syn::ImplItemConst;
use syn::ImplItemMethod;
use syn::ImplItemType;
use syn::TraitItem;
use syn::Visibility;
use syn::Type;
use syn::TypePath;
use syn::TypeInfer;
use syn::Ident;
use syn::token::Comma;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::Token;

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

fn separate_impl(mut item: TraitItem) -> (ImplItem, TraitItem) {
    (match &mut item {
        TraitItem::Const(c) => {
            let attrs = ::std::mem::replace(&mut c.attrs, vec![]);
            let (eq_token, expr) = c.default.take().unwrap_or_else(|| { c.semi_token.span().unwrap().error("Expected '=' token").emit(); unreachable!() });
            ImplItem::Const(ImplItemConst {
                attrs,
                vis: Visibility::Inherited,
                defaultness: None,
                const_token: c.const_token.clone(),
                ident: c.ident.clone(),
                colon_token: c.colon_token.clone(),
                ty: c.ty.clone(),
                eq_token,
                expr,
                semi_token: c.semi_token,
            })
        },
        TraitItem::Method(m) => {
            let attrs = ::std::mem::replace(&mut m.attrs, vec![]);
            let block = m.default.take().unwrap_or_else(|| { m.semi_token.unwrap().span().unwrap().error("Expected '{' token").emit(); unreachable!() });
            ImplItem::Method(ImplItemMethod {
                attrs,
                vis: Visibility::Inherited,
                defaultness: None,
                sig: m.sig.clone(),
                block,
            })
        },
        TraitItem::Type(t) => {
            let attrs = ::std::mem::replace(&mut t.attrs, vec![]);
            let (eq_token, ty) = t.default.take().unwrap_or_else(|| { t.semi_token.span().unwrap().error("Expected '=' token").emit(); unreachable!() });
            ImplItem::Type(ImplItemType {
                attrs,
                vis: Visibility::Inherited,
                defaultness: None,
                type_token: t.type_token.clone(),
                ident: t.ident.clone(),
                generics: t.generics.clone(),
                eq_token,
                ty,
                semi_token: t.semi_token.clone(),
            })
        },
        other => {
            other.span().unwrap().error("Unsupported item type").emit();
            unimplemented!()
        },
    }, item)
}

#[proc_macro_attribute]
pub fn lit(attr: TokenStream, input: TokenStream) -> TokenStream {
    let attr = syn::parse_macro_input!(attr as Attr);
    let input = syn::parse_macro_input!(input as Item);

    let (span, item_impl, mut stream) = match input {
        Item::Trait(mut input) => {
            let (items, trait_items) = input.items.into_iter().map(separate_impl).unzip();
            input.items = trait_items;
            let item_impl = ItemImpl {
                attrs: vec![],
                defaultness: None,
                unsafety: input.unsafety.clone(),
                impl_token: Token!(impl)(Span::call_site()),
                generics: input.generics.clone(),
                trait_: Some((None, input.ident.clone().into(), Token!(for)(Span::call_site()))),
                self_ty: Box::new(Type::Infer(TypeInfer { underscore_token: Token!(_)(Span::call_site()) })),
                brace_token: input.brace_token,
                items,
            };
            (input.ident.span(), item_impl, input.into_token_stream())
        },
        Item::Impl(input) => {
            let span = match &*input.self_ty {
                Type::Infer(infer) => infer.underscore_token.spans[0],
                ty => {
                    ty.span().unwrap().error(format!("Expected _, found {}", ty.into_token_stream())).emit();
                    return input.into_token_stream().into()
                },
            };

            (span, input, proc_macro2::TokenStream::new())
        },
        input => {
            input.span().unwrap().error("lit is only allowed on traits and trait impls").emit();
            return input.into_token_stream().into()
        },
    };

    if attr.uint {
        stream.append_all(UINT.iter().map(|name| name_to_type(name, span)).map(|ty| build_impl(&item_impl, ty)));
    }

    if attr.int {
        stream.append_all(INT.iter().map(|name| name_to_type(name, span)).map(|ty| build_impl(&item_impl, ty)));
    }

    if attr.float {
        stream.append_all(FLOAT.iter().map(|name| name_to_type(name, span)).map(|ty| build_impl(&item_impl, ty)));
    }

    stream.into()
}