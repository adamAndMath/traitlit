#![feature(proc_macro_diagnostic)]

extern crate proc_macro;

mod args;
use args::*;

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
use syn::WhereClause;
use syn::WherePredicate;
use syn::MethodTurbofish;
use syn::Stmt;
use syn::Pat;
use syn::FieldPat;
use syn::FieldValue;
use syn::Arm;
use syn::GenericMethodArgument;
use syn::FnArg;
use syn::ForeignItem;
use syn::Variant;
use syn::Fields;
use syn::Field;
use quote::ToTokens;

fn ident_to_type(ident: Ident) -> Type {
    Type::Path(TypePath { qself: None, path: ident.into() })
}

fn ident_to_expr(ident: Ident) -> Expr {
    Expr::Path(ExprPath { attrs: vec![], qself: None, path: ident.into() })
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

fn split_gen(gen: &mut Generics, v: &Ident) -> Generics {
    Generics {
        lt_token: gen.lt_token.clone(),
        params: gen.params.pairs_mut().map(Pair::into_tuple).map(|(t, p)|Pair::new(split_gen_par(t, v), p.cloned())).collect(),
        gt_token: gen.gt_token.clone(),
        where_clause: gen.where_clause.clone(),
    }
}

fn split_gen_par(gen: &mut GenericParam, v: &Ident) -> GenericParam {
    match gen {
        GenericParam::Type(t) => {
            GenericParam::Type(TypeParam {
                attrs: t.attrs.clone(),
                ident: t.ident.clone(),
                colon_token: t.colon_token.clone(),
                bounds: t.bounds.clone(),
                eq_token: t.eq_token.clone(),
                default:
                    if t.default.as_mut().map(|ty|ty.contains_var(v)).unwrap_or(false) {
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
    fn contains_var(&self, v: &Ident) -> bool;

    fn infer(&mut self, v: &Ident, val: &Type);
}

impl<T: ContainsInfer> ContainsInfer for Box<T> {
    #[inline]
    fn contains_var(&self, v: &Ident) -> bool {
        (**self).contains_var(v)
    }

    #[inline]
    fn infer(&mut self, v: &Ident, val: &Type) {
        (**self).infer(v, val)
    }
}

impl<T: ContainsInfer> ContainsInfer for Option<T> {
    fn contains_var(&self, v: &Ident) -> bool {
        match self {
            Some(t) => t.contains_var(v),
            None => false,
        }
    }

    fn infer(&mut self, v: &Ident, val: &Type) {
        if let Some(t) = self {
            t.infer(v, val)
        }
    }
}

impl<T: ContainsInfer> ContainsInfer for Vec<T> {
    fn contains_var(&self, v: &Ident) -> bool {
        self.iter().any(|t|t.contains_var(v))
    }

    fn infer(&mut self, v: &Ident, val: &Type) {
        for t in self {
            t.infer(v, val)
        }
    }
}

impl<T: ContainsInfer, P> ContainsInfer for Punctuated<T, P> {
    fn contains_var(&self, v: &Ident) -> bool {
        self.iter().any(|t|t.contains_var(v))
    }

    fn infer(&mut self, v: &Ident, val: &Type) {
        self.iter_mut().for_each(|t|t.infer(v, val))
    }
}

macro_rules! impl_infer_contains {
    (@anon($i:ident), ) => { false };
    (@named($i:ident), ) => { return false };
    (@anon($i:ident), match ($m:expr) { $($pat:pat => { $($b:tt)* }$(,)?)* }) => {
        match &$m {
            $($pat => impl_infer_contains!(@anon($i), $($b)*)),*
        }
    };
    (@named($i:ident), match ($m:expr) { $($pat:pat => { $($b:tt)* }$(,)?)* }) => {
        match &$m {
            $($pat => impl_infer_contains!(@named($i), $($b)*)),*
        }
    };
    (@anon($i:ident), match ($m:expr) { $($pat:pat => { $($b:tt)* }$(,)?)* } $($rest:tt)+) => {
        match &$m {
            $($pat => impl_infer_contains!(@anon($i), $($b)*)),*
        } ||
        impl_infer_contains!(@anon($i), $($rest)+)
    };
    (@named($i:ident), match ($m:expr) { $($pat:pat => { $($b:tt)* }$(,)?)* } $($rest:tt)+) => {{
        if match &$m {
            $($pat => impl_infer_contains!(@anon($i), $($b)*)),*
        } { return true }
        impl_infer_contains!(@named($i), $($rest)+)
    }};
    (@named($i:ident), yield $e:expr) => { &$e };
    (@anon($i:ident), $e:expr; $($rest:tt)*) => {
        ($e).contains_var($i) ||
        impl_infer_contains!(@anon($i), $($rest)*)
    };
    (@named($i:ident), $e:expr; $($rest:tt)*) => {
        if ($e).contains_var($i) {
            return true
        } else {
            impl_infer_contains!(@named($i), $($rest)*)
        }
    }
}

macro_rules! impl_infer_replace {
    (@anon($i:ident, $ty:ident), ) => { () };
    (@named($i:ident, $ty:ident), ) => { return };
    (@anon($i:ident, $ty:ident), match ($m:expr) { $($pat:pat => { $($b:tt)* }$(,)?)* }) => {{
        match &mut $m {
            $($pat => impl_infer_replace!(@anon($i, $ty), $($b)*)),*
        }
    }};
    (@named($i:ident, $ty:ident), match ($m:expr) { $($pat:pat => { $($b:tt)* }$(,)?)* }) => {{
        match &mut $m {
            $($pat => impl_infer_replace!(@named($i, $ty), $($b)*)),*
        }
    }};
    (@anon($i:ident, $ty:ident), match ($m:expr) { $($pat:pat => { $($b:tt)* }$(,)?)* } $($rest:tt)+) => {{
        match &mut $m {
            $($pat => impl_infer_replace!(@anon($i, $ty), $($b)*)),*
        }
        impl_infer_replace!(@anon($i, $ty), $($rest)+)
    }};
    (@named($i:ident, $ty:ident), match ($m:expr) { $($pat:pat => { $($b:tt)* }$(,)?)* } $($rest:tt)+) => {{
        match &mut $m {
            $($pat => impl_infer_replace!(@anon($i, $ty), $($b)*)),*
        }
        impl_infer_replace!(@named($i, $ty), $($rest)+)
    }};
    (@named($i:ident, $ty:ident), yield $e:expr) => { &mut $e };
    (@anon($i:ident, $ty:ident), $e:expr; $($rest:tt)*) => {{
        ($e).infer($i, $ty);
        impl_infer_replace!(@anon($i, $ty), $($rest)*)
    }};
    (@named($i:ident, $ty:ident), $e:expr; $($rest:tt)*) => {{
        ($e).infer($i, $ty);
        impl_infer_replace!(@named($i, $ty), $($rest)*)
    }};
}

macro_rules! impl_infer {
    () => {};
    (struct $v:ident in $T:ty {$($b:tt)*} $($rest:tt)*) => {
        impl ContainsInfer for $T {
            fn contains_var(&self, i: &Ident) -> bool {
                let $v = self;
                impl_infer_contains!(@anon(i), $($b)*)
            }

            fn infer(&mut self, i: &Ident, val: &Type) {
                let $v = self;
                impl_infer_replace!(@anon(i, val), $($b)*)
            }
        }
        impl_infer!($($rest)*);
    };
    (enum $T:ty { $($pat:pat => {$($b:tt)*}$(,)?)* } $($rest:tt)*) => {
        impl ContainsInfer for $T {
            fn contains_var(&self, i: &Ident) -> bool {
                match self {
                    $($pat => impl_infer_contains!(@anon(i), $($b)*)),*
                }
            }

            fn infer(&mut self, i: &Ident, val: &Type) {
                match self {
                    $($pat => impl_infer_replace!(@anon(i, val), $($b)*)),*
                }
            }
        }
        impl_infer!($($rest)*);
    };
    (enum $v:ident in $T:ty { $($pat:pat => {$($b:tt)*}$(,)?)* } $($rest:tt)*) => {
        impl ContainsInfer for $T {
            fn contains_var(&self, i: &Ident) -> bool {
                let mut $v = self;
                loop {
                    $v = match $v {
                        $($pat => impl_infer_contains!(@named(i), $($b)*)),*
                    }
                }
            }

            fn infer(&mut self, i: &Ident, val: &Type) {
                let mut $v = self;
                loop {
                    $v = match $v {
                        $($pat => impl_infer_replace!(@named(i, val), $($b)*)),*
                    }
                }
            }
        }
        impl_infer!($($rest)*);
    };
}

impl ContainsInfer for Type {
    fn contains_var(&self, v: &Ident) -> bool {
        let mut ty = self;
        loop {
            ty = match ty {
                Type::Slice(slice) => &slice.elem,
                Type::Array(array) => &array.elem,
                Type::Ptr(ptr) => &ptr.elem,
                Type::Reference(ptr) => &ptr.elem,
                Type::BareFn(fun) => {
                    if fun.inputs.contains_var(v) {
                        return true;
                    }

                    match &fun.output {
                        ReturnType::Default => return false,
                        ReturnType::Type(_, re) => &re,
                    }
                },
                Type::Never(_) => return false,
                Type::Tuple(tuple) => return tuple.elems.contains_var(v),
                Type::Path(path) => {
                    if path.path.is_ident(v.clone()) || path.path.segments.contains_var(v) {
                        return true
                    }
                    match &path.qself {
                        Some(qself) => &qself.ty,
                        None => return false,
                    }
                },
                Type::TraitObject(obj) => return obj.bounds.contains_var(v),
                Type::ImplTrait(it) => return it.bounds.contains_var(v),
                Type::Paren(paren) => &paren.elem,
                Type::Group(group) => &group.elem,
                Type::Infer(_) => return false,
                Type::Macro(_) => return false,
                Type::Verbatim(_) => return false,
            }
        }
    }
    fn infer(&mut self, v: &Ident, val: &Type) {
        let mut ty = self;
        loop {
            if let Type::Path(path) = ty {
                if path.qself.is_none() && path.path.is_ident(v.clone()) {
                    return *ty = val.clone()
                }
            }
            ty = match ty {
                Type::Slice(slice) => &mut slice.elem,
                Type::Array(array) => &mut array.elem,
                Type::Ptr(ptr) => &mut ptr.elem,
                Type::Reference(ptr) => &mut ptr.elem,
                Type::BareFn(fun) => {
                    fun.inputs.infer(v, val);

                    match &mut fun.output {
                        ReturnType::Default => return,
                        ReturnType::Type(_, re) => re,
                    }
                },
                Type::Never(_) => return,
                Type::Tuple(tuple) => return tuple.elems.infer(v, val),
                Type::Path(path) => {
                    path.path.segments.infer(v, val);

                    match &mut path.qself {
                        Some(qself) => &mut qself.ty,
                        None => return,
                    }
                },
                Type::TraitObject(obj) => return obj.bounds.infer(v, val),
                Type::ImplTrait(it) => return it.bounds.infer(v, val),
                Type::Paren(paren) => &mut paren.elem,
                Type::Group(group) => &mut group.elem,
                Type::Infer(_) => return,
                Type::Macro(_) => return,
                Type::Verbatim(_) => return,
            }
        }
    }
}

impl_infer! {
    struct gen in Generics {
        gen.params;
        gen.where_clause;
    }

    struct clause in WhereClause {
        clause.predicates;
    }

    enum WherePredicate {
        WherePredicate::Lifetime(_) => { },
        WherePredicate::Type(ty) => {
            ty.bounded_ty;
            ty.bounds;
        },
        WherePredicate::Eq(eq) => {
            eq.lhs_ty;
            eq.rhs_ty;
        },
    }

    enum GenericParam {
        GenericParam::Lifetime(_) => { },
        GenericParam::Type(type_param) => {
            type_param.bounds;
            type_param.default;
        },
        GenericParam::Const(const_param) => { const_param.default; },
    }

    struct arg in BareFnArg {
        arg.ty;
    }

    struct segment in PathSegment {
        match (segment.arguments) {
            PathArguments::None => { },
            PathArguments::AngleBracketed(args) => { args.args; },
            PathArguments::Parenthesized(func) => {
                func.inputs;
                func.output;
            }
        }
    }

    enum GenericArgument {
        GenericArgument::Lifetime(_) => { },
        GenericArgument::Type(ty) => { ty; },
        GenericArgument::Binding(binding) => { binding.ty; },
        GenericArgument::Constraint(constraint) => { constraint.bounds; },
        GenericArgument::Const(expr) => { expr; },
    }

    enum TypeParamBound {
        TypeParamBound::Lifetime(_) => { },
        TypeParamBound::Trait(bound) => { bound.path.segments; },
    }

    enum expr in Expr {
        Expr::Box(expr_box) => { yield expr_box.expr },
        Expr::InPlace(in_place) => {
            in_place.place;
            yield in_place.value
        },
        Expr::Array(arr) => { arr.elems; },
        Expr::Call(call) => {
            call.args;
            yield call.func
        },
        Expr::MethodCall(method) => {
            method.turbofish;
            method.args;
            yield method.receiver
        },
        Expr::Tuple(tuple) => { tuple.elems; },
        Expr::Binary(binary) => {
            binary.left;
            yield binary.right
        },
        Expr::Unary(unary) => { yield unary.expr },
        Expr::Lit(_) => { },
        Expr::Cast(cast) => {
            cast.ty;
            yield cast.expr
        },
        Expr::Type(ty) => {
            ty.ty;
            yield ty.expr
        },
        Expr::Let(guard) => {
            guard.pats;
            yield guard.expr
        },
        Expr::If(branchs) => {
            branchs.then_branch.stmts;
            match (branchs.else_branch) {
                Some((_, expr)) => { expr; },
                None => { },
            }
            yield branchs.cond
        },
        Expr::While(while_loop) => {
            while_loop.body.stmts;
            yield while_loop.cond
        },
        Expr::ForLoop(for_loop) => {
            for_loop.pat;
            for_loop.body.stmts;
            yield for_loop.expr
        },
        Expr::Loop(loop_) => {
            loop_.body.stmts;
        },
        Expr::Match(match_) => {
            match_.arms;
            yield match_.expr
        },
        Expr::Closure(closure) => {
            closure.inputs;
            closure.output;
            yield closure.body
        },
        Expr::Unsafe(block) => { block.block.stmts; },
        Expr::Block(block) => { block.block.stmts; },
        Expr::Assign(assign) => {
            assign.left;
            yield assign.right
        },
        Expr::AssignOp(assign) => {
            assign.left;
            yield assign.right
        },
        Expr::Field(field) => { yield field.base },
        Expr::Index(index) => {
            index.index;
            yield index.expr
        },
        Expr::Range(range) => {
            range.from;
            range.to;
        },
        Expr::Path(path) => {
            path.path.segments;
            match (path.qself) {
                Some(qself) => { qself.ty; },
                None => { },
            }
        },
        Expr::Reference(reference) => { yield reference.expr },
        Expr::Break(break_) => { break_.expr; },
        Expr::Continue(_) => { },
        Expr::Return(return_) => { return_.expr; },
        Expr::Macro(_) => { },
        Expr::Struct(struct_) => {
            struct_.path.segments;
            struct_.fields;
            struct_.rest;
        },
        Expr::Repeat(repeat) => {
            repeat.len;
            yield repeat.expr
        },
        Expr::Paren(paren) => { yield paren.expr },
        Expr::Group(group) => { yield group.expr },
        Expr::Try(try_) => { yield try_.expr },
        Expr::Async(block) => { block.block.stmts; },
        Expr::TryBlock(block) => { block.block.stmts; },
        Expr::Yield(expr) => { expr.expr; },
        Expr::Verbatim(_) => { },
    }

    enum ReturnType {
        ReturnType::Default => { },
        ReturnType::Type(_, ty) => { ty; },
    }

    enum Stmt {
        Stmt::Local(local) => {
            match (local.ty) {
                Some((_, ty)) => { ty; },
                None => { },
            }
            match (local.init) {
                Some((_, expr)) => { expr; },
                None => { },
            }
            local.pats;
        },
        Stmt::Item(item) => { item; },
        Stmt::Expr(expr) => { expr; },
        Stmt::Semi(expr, _) => { expr; },
    }

    struct arm in Arm {
        match (arm.guard) {
            Some((_, expr)) => { expr; },
            None => { },
        }
        arm.pats;
        arm.body;
    }

    enum pat in Pat {
        Pat::Wild(_) => { },
        Pat::Ident(ident) => {
            match (ident.subpat) {
                Some((_, pat)) => { pat; },
                None => { },
            }
        },
        Pat::Struct(struct_) => {
            struct_.path.segments;
            struct_.fields;
        },
        Pat::TupleStruct(struct_) => {
            struct_.path.segments;
            struct_.pat.front;
            struct_.pat.back;
        },
        Pat::Path(path) => {
            match (path.qself) {
                Some(qself) => { qself.ty; },
                None => { },
            }
            path.path.segments;
        },
        Pat::Tuple(tuple) => {
            tuple.front;
            tuple.back;
        },
        Pat::Box(pat) => { yield pat.pat },
        Pat::Ref(pat) => { yield pat.pat },
        Pat::Lit(lit) => { lit.expr; },
        Pat::Range(range) => {
            range.lo;
            range.hi;
        },
        Pat::Slice(slice) => {
            slice.front;
            slice.middle;
            slice.back;
        },
        Pat::Macro(_) => { },
        Pat::Verbatim(_) => { },
    }

    struct field in FieldPat {
        field.pat;
    }

    struct field in FieldValue {
        field.expr;
    }

    struct fish in MethodTurbofish {
        fish.args;
    }

    enum GenericMethodArgument {
        GenericMethodArgument::Type(ty) => { ty; },
        GenericMethodArgument::Const(expr) => { expr; },
    }

    enum FnArg {
        FnArg::SelfRef(_) => { },
        FnArg::SelfValue(_) => { },
        FnArg::Captured(capture) => {
            capture.pat;
            capture.ty;
        },
        FnArg::Inferred(pat) => { pat; },
        FnArg::Ignored(ty) => { ty; },
    }

    enum Item {
        Item::ExternCrate(_) => { },
        Item::Use(_) => { },
        Item::Static(var) => {
            var.ty;
            var.expr;
        },
        Item::Const(var) => {
            var.ty;
            var.expr;
        },
        Item::Fn(func) => {
            func.decl.generics;
            func.decl.inputs;
            func.decl.output;
            func.block.stmts;
        },
        Item::Mod(module) => {
            match (module.content) {
                Some((_, items)) => { items; },
                None => { },
            }
        },
        Item::ForeignMod(module) => { module.items; },
        Item::Type(alias) => {
            alias.generics;
            alias.ty;
        },
        Item::Existential(alias) => {
            alias.generics;
            alias.bounds;
        },
        Item::Struct(struct_) => {
            struct_.generics;
            struct_.fields;
        },
        Item::Enum(enum_) => {
            enum_.generics;
            enum_.variants;
        },
        Item::Union(union_) => {
            union_.generics;
            union_.fields.named;
        },
        Item::Trait(trait_) => {
            trait_.generics;
            trait_.supertraits;
            trait_.items;
        },
        Item::TraitAlias(alias) => {
            alias.generics;
            alias.bounds;
        },
        Item::Impl(impl_) => {
            match (impl_.trait_) {
                Some((_, path, _)) => { path.segments; },
                None => { },
            }
            impl_.generics;
            impl_.self_ty;
            impl_.items;
        },
        Item::Macro(_) => { },
        Item::Macro2(_) => { },
        Item::Verbatim(_) => { },
    }

    enum ForeignItem {
        ForeignItem::Fn(func) => {
            func.decl.generics;
            func.decl.inputs;
            func.decl.output;
        },
        ForeignItem::Static(var) => { var.ty; },
        ForeignItem::Type(_) => { },
        ForeignItem::Macro(_) => { },
        ForeignItem::Verbatim(_) => { },
    }

    enum Fields {
        Fields::Named(fields) => { fields.named; },
        Fields::Unnamed(fields) => { fields.unnamed; },
        Fields::Unit => { },
    }

    struct var in Variant {
        var.fields;
        match (var.discriminant) {
            Some((_, expr)) => { expr; },
            None => { },
        }
    }

    enum TraitItem {
        TraitItem::Const(var) => {
            var.ty;
            match (var.default) {
                Some((_, expr)) => { expr; },
                None => { },
            }
        },
        TraitItem::Method(method) => {
            method.sig.decl.generics;
            method.sig.decl.inputs;
            method.sig.decl.output;
            match (method.default) {
                Some(block) => { block.stmts; },
                None => { },
            }
        }
        TraitItem::Type(alias) => {
            alias.generics;
            alias.bounds;
            match (alias.default) {
                Some((_, expr)) => { expr; },
                None => { },
            }
        },
        TraitItem::Macro(_) => { },
        TraitItem::Verbatim(_) => { },
    }

    struct field in Field {
        field.ty;
    }

    enum ImplItem {
        ImplItem::Const(var) => {
            var.ty;
            var.expr;
        },
        ImplItem::Method(method) => {
            method.sig.decl.generics;
            method.sig.decl.inputs;
            method.sig.decl.output;
            method.block.stmts;
        },
        ImplItem::Type(alias) => {
            alias.generics;
            alias.ty;
        },
        ImplItem::Existential(alias) => {
            alias.generics;
            alias.bounds;
        },
        ImplItem::Macro(_) => { },
        ImplItem::Verbatim(_) => { },
    }
}

/// Generates implimentations for a set of literal types.
/// Can be used on either an impl block or directly on the trait definition.
/// The type will be inserted on all '_' in the type or trait arguments.
#[proc_macro_attribute]
pub fn lit(args: TokenStream, input: TokenStream) -> TokenStream {
    let args = syn::parse_macro_input!(args as Args);
    let input = syn::parse_macro_input!(input as Item);

    let (item, mut stream) = match input {
        Item::Trait(mut input) => {
            let mut generics = split_gen(&mut input.generics, &args.ident);
            let gen = separate_gen(&mut generics);
            let item_impl = ItemImpl {
                attrs: vec![],
                defaultness: None,
                unsafety: input.unsafety.clone(),
                impl_token: Token!(impl)(Span::call_site()),
                generics,
                trait_: Some((None, PathSegment { ident: input.ident.clone(), arguments: gen }.into(), Token!(for)(Span::call_site()))),
                self_ty: Box::new(ident_to_type(args.ident.clone())),
                brace_token: input.brace_token,
                items: input.items.iter_mut().map(separate_impl).collect(),
            };
            (Item::Impl(item_impl), input.into_token_stream())
        },
        item => { (item, proc_macro2::TokenStream::new()) },
    };

    for WithAttr(attrs, ty) in args.types {
        attrs.into_iter().for_each(|attr|attr.to_tokens(&mut stream));
        let mut item = item.clone();
        item.infer(&args.ident, &ty);
        item.to_tokens(&mut stream)
    }

    stream.into()
}