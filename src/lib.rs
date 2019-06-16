#![feature(proc_macro_diagnostic)]

extern crate proc_macro;

mod attr;
use attr::Attr;

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
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::Token;
use syn::Generics;
use syn::PathArguments;
use syn::AngleBracketedGenericArguments;
use syn::punctuated::Pair;
use syn::GenericParam;
use syn::GenericArgument;
use syn::ReturnType;
use syn::BareFnArg;
use syn::PathSegment;
use syn::TypeParam;
use syn::TypeParamBound;
use syn::Expr;
use syn::ExprPath;

use quote::ToTokens;

fn ident_to_type(ident: Ident) -> Type {
    Type::Path(TypePath { qself: None, path: ident.into() })
}

fn ident_to_expr(ident: Ident) -> Expr {
    Expr::Path(ExprPath { attrs: vec![], qself: None, path: ident.into() })
}

fn build_impl(input: &ItemImpl, ty: Type) -> ItemImpl {
    let mut re = input.clone();
    re.self_ty.infer(&ty);
    if let Some((_, p, _)) = &mut re.trait_ {
        p.segments.infer(&ty);
    }
    re
}

fn separate_impl(item: &mut TraitItem) -> ImplItem {
    match item {
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
    }
}

fn split_gen(gen: &mut Generics) -> Generics {
    Generics {
        lt_token: gen.lt_token.clone(),
        params: gen.params.pairs_mut().map(Pair::into_tuple).map(|(v, p)|Pair::new(split_gen_par(v), p.cloned())).collect(),
        gt_token: gen.gt_token.clone(),
        where_clause: gen.where_clause.clone(),
    }
}

fn split_gen_par(gen: &mut GenericParam) -> GenericParam {
    match gen {
        GenericParam::Type(t) => {
            GenericParam::Type(TypeParam {
                attrs: t.attrs.clone(),
                ident: t.ident.clone(),
                colon_token: t.colon_token.clone(),
                bounds: t.bounds.clone(),
                eq_token: t.eq_token.clone(),
                default:
                    if t.default.as_mut().map(|ty|ty.contains_infer()).unwrap_or(false) {
                        t.default.take()
                    } else {
                        None
                    }
            })
        },
        gen => gen.clone()
    }
}

fn separate_gen(gen: &mut Generics) -> PathArguments {
    match gen {
        Generics {
            lt_token: Some(lt_token),
            params,
            gt_token: Some(gt_token),
            where_clause: _,
        } => {
            let p = ::std::mem::replace(params, Punctuated::new());
            let mut args = Punctuated::new();

            for (v, p) in p.into_pairs().map(Pair::into_tuple) {
                let (arg, param) = separate_gen_par(v);
                params.extend(param.map(|param|Pair::new(param, p.clone())));
                args.extend(::std::iter::once(Pair::new(arg, p)));
            }

            PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                colon2_token: None,
                lt_token: lt_token.clone(),
                args,
                gt_token: gt_token.clone(),
            })
        },
        _ => PathArguments::None,
    }
}

fn separate_gen_par(gen: GenericParam) -> (GenericArgument, Option<GenericParam>) {
    match gen {
        GenericParam::Type(ty) => {
            if let Some(ty) = ty.default {
                (GenericArgument::Type(ty), None)
            } else {
                (GenericArgument::Type(ident_to_type(ty.ident.clone())), Some(GenericParam::Type(ty)))
            }
        },
        GenericParam::Lifetime(l) => {
            (GenericArgument::Lifetime(l.lifetime.clone()), Some(GenericParam::Lifetime(l)))
        },
        GenericParam::Const(c) => {
            if let Some(expr) = c.default {
                (GenericArgument::Const(expr), None)
            } else {
                (GenericArgument::Const(ident_to_expr(c.ident.clone())), Some(GenericParam::Const(c)))
            }
        }, 
    }
}

trait ContainsInfer {
    fn contains_infer(&self) -> bool;

    fn infer(&mut self, val: &Type);
}

impl ContainsInfer for Type {
    fn contains_infer(&self) -> bool {
        let mut ty = self;
        loop {
            match ty {
                Type::Slice(slice) => ty = &slice.elem,
                Type::Array(array) => ty = &array.elem,
                Type::Ptr(ptr) => ty = &ptr.elem,
                Type::Reference(ptr) => ty = &ptr.elem,
                Type::BareFn(fun) => {
                    if fun.inputs.contains_infer() {
                        return true;
                    }

                    match &fun.output {
                        ReturnType::Default => return false,
                        ReturnType::Type(_, re) => ty = &re,
                    }
                },
                Type::Never(_) => return false,
                Type::Tuple(tuple) => return tuple.elems.contains_infer(),
                Type::Path(path) => {
                    if path.path.segments.contains_infer() {
                        return true;
                    }
                    match &path.qself {
                        Some(qself) => ty = &qself.ty,
                        None => return false,
                    }
                },
                Type::TraitObject(obj) => return obj.bounds.contains_infer(),
                Type::ImplTrait(it) => return it.bounds.contains_infer(),
                Type::Paren(paren) => ty = &paren.elem,
                Type::Group(group) => ty = &group.elem,
                Type::Infer(_) => return true,
                Type::Macro(_) => return false,
                Type::Verbatim(_) => return false,
            }
        }
    }
    fn infer(&mut self, val: &Type) {
        let mut ty = self;
        loop {
            match ty {
                Type::Slice(slice) => ty = &mut slice.elem,
                Type::Array(array) => ty = &mut array.elem,
                Type::Ptr(ptr) => ty = &mut ptr.elem,
                Type::Reference(ptr) => ty = &mut ptr.elem,
                Type::BareFn(fun) => {
                    fun.inputs.infer(val);

                    match &mut fun.output {
                        ReturnType::Default => return,
                        ReturnType::Type(_, re) => ty = re,
                    }
                },
                Type::Never(_) => return,
                Type::Tuple(tuple) => return tuple.elems.infer(val),
                Type::Path(path) => {
                    path.path.segments.infer(val);

                    match &mut path.qself {
                        Some(qself) => ty = &mut qself.ty,
                        None => return,
                    }
                },
                Type::TraitObject(obj) => return obj.bounds.infer(val),
                Type::ImplTrait(it) => return it.bounds.infer(val),
                Type::Paren(paren) => ty = &mut paren.elem,
                Type::Group(group) => ty = &mut group.elem,
                Type::Infer(_) => return *ty = val.clone(),
                Type::Macro(_) => return,
                Type::Verbatim(_) => return,
            }
        }
    }
}

impl<T: ContainsInfer, P> ContainsInfer for Punctuated<T, P> {
    fn contains_infer(&self) -> bool {
        self.iter().any(T::contains_infer)
    }

    fn infer(&mut self, val: &Type) {
        self.iter_mut().for_each(|t|t.infer(val))
    }
}

impl ContainsInfer for BareFnArg {
    fn contains_infer(&self) -> bool {
        self.ty.contains_infer()
    }

    fn infer(&mut self, val: &Type) {
        self.ty.infer(val)
    }
}

impl ContainsInfer for PathSegment {
    fn contains_infer(&self) -> bool {
        match &self.arguments {
            PathArguments::None => false,
            PathArguments::AngleBracketed(args) => args.args.contains_infer(),
            PathArguments::Parenthesized(fun) => fun.inputs.contains_infer() ||
                match &fun.output {
                    ReturnType::Default => false,
                    ReturnType::Type(_, ty) => ty.contains_infer(),
                },
        }
    }

    fn infer(&mut self, val: &Type) {
        match &mut self.arguments {
            PathArguments::None => (),
            PathArguments::AngleBracketed(args) => args.args.infer(val),
            PathArguments::Parenthesized(fun) => {
                fun.inputs.infer(val);

                match &mut fun.output {
                    ReturnType::Default => (),
                    ReturnType::Type(_, ty) => ty.infer(val),
                }
            },
        }
    }
}

impl ContainsInfer for GenericArgument {
    fn contains_infer(&self) -> bool {
        match self {
            GenericArgument::Lifetime(_) => false,
            GenericArgument::Type(ty) => ty.contains_infer(),
            GenericArgument::Binding(binding) => binding.ty.contains_infer(),
            GenericArgument::Constraint(constraint) => constraint.bounds.contains_infer(),
            GenericArgument::Const(_) => false,
        }
    }

    fn infer(&mut self, val: &Type) {
        match self {
            GenericArgument::Lifetime(_) => (),
            GenericArgument::Type(ty) => ty.infer(val),
            GenericArgument::Binding(binding) => binding.ty.infer(val),
            GenericArgument::Constraint(constraint) => constraint.bounds.infer(val),
            GenericArgument::Const(_) => (),
        }
    }
}

impl ContainsInfer for TypeParamBound {
    fn contains_infer(&self) -> bool {
        match self {
            TypeParamBound::Trait(bound) => bound.path.segments.contains_infer(),
            TypeParamBound::Lifetime(_) => false,
        }
    }

    fn infer(&mut self, val: &Type) {
        match self {
            TypeParamBound::Trait(bound) => bound.path.segments.infer(val),
            TypeParamBound::Lifetime(_) => (),
        }
    }
}

/// Generates implimentations for a set of literal types.
/// Can be used on either an impl block or directly on the trait definition.
/// The type will be inserted on all '_' in the type or trait arguments.
#[proc_macro_attribute]
pub fn lit(attr: TokenStream, input: TokenStream) -> TokenStream {
    let attr = syn::parse_macro_input!(attr as Attr);
    let input = syn::parse_macro_input!(input as Item);

    let (item_impl, mut stream) = match input {
        Item::Trait(mut input) => {
            let mut generics = split_gen(&mut input.generics);
            let args = separate_gen(&mut generics);
            let item_impl = ItemImpl {
                attrs: vec![],
                defaultness: None,
                unsafety: input.unsafety.clone(),
                impl_token: Token!(impl)(Span::call_site()),
                generics,
                trait_: Some((None, PathSegment { ident: input.ident.clone(), arguments: args }.into(), Token!(for)(Span::call_site()))),
                self_ty: Box::new(Type::Infer(TypeInfer { underscore_token: Token!(_)(Span::call_site()) })),
                brace_token: input.brace_token,
                items: input.items.iter_mut().map(separate_impl).collect(),
            };
            (item_impl, input.into_token_stream())
        },
        Item::Impl(input) => {
            (input, proc_macro2::TokenStream::new())
        },
        input => {
            input.span().unwrap().error("lit is only allowed on traits and trait impls").emit();
            return input.into_token_stream().into()
        },
    };

    for ty in attr.types {
        build_impl(&item_impl, ty).to_tokens(&mut stream)
    }

    stream.into()
}