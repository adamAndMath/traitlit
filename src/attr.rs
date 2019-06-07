use syn::Ident;
use syn::parse::{ self, Parse, ParseStream };
use syn::punctuated::Punctuated;
use syn::token::Comma;
use proc_macro::Span;

macro_rules! def_sets {
    ($(set $name:ident { $($ty:ty),*$(,)? })*) => {
        #[derive(Default)]
        pub struct Attr {
            $($name: bool),*
        }
        
        impl Attr {
            fn set(&mut self, ident: Ident) -> syn::parse::Result<()> {
                let p = $(if ident == stringify!($name) {
                    &mut self.$name
                } else)* {
                    return Err(syn::Error::new(ident.span(), "Unknown type set"))
                };

                if *p {
                    ident.span().unwrap().warning("Duplicate type set").emit()
                }

                *p = true;
                Ok(())
            }

            pub fn for_each<F: FnMut(&str)>(self, mut f: F) {
                $(if self.$name {
                    $(f(stringify!($ty));)*
                })*
            }
        }

        impl Parse for Attr {
            fn parse(input: ParseStream) -> parse::Result<Self> {
                let mut attr = Attr::default();
                let list = Punctuated::<Ident, Comma>::parse_terminated(input)?;
                if list.is_empty() {
                    Span::call_site().warning("lit won't generate any impls").emit()
                }

                for ident in list {
                    attr.set(ident)?;
                }
                Ok(attr)
            }
        }

    };
}

def_sets! {
    set uint {
        u8, u16, u32, u64, u128, usize
    }

    set int {
        i8, i16, i32, i64, i128, isize
    }

    set float {
        f32, f64
    }
}
