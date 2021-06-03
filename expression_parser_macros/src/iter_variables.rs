use crate::VariantVisitor;
use proc_macro2::TokenStream;
use quote::{quote, format_ident};
use syn::Ident;

fn generate_left_hand<'ast>(visited: &VariantVisitor<'ast>, func_name: &Ident) -> Vec<TokenStream> {
    let mut left_hand_side = Vec::new();
    for visit in &visited.items {
        let are_vec_arguments =
            crate::generate_tuple_variables(&visit.are_vec_arguments).zip(&visit.are_vec_arguments);
        let mut map_functions: Vec<_> = are_vec_arguments
            .map(|(arg, is_vec)| {
                if *is_vec {
                    quote! {
                        #arg.iter().flat_map(|x| x.#func_name())
                    }
                } else {
                    quote! {
                        #arg.#func_name()
                    }
                }
            })
            .collect();

        let tokens = match map_functions.len() {
            0 => quote! {std::iter::empty::<String>()},
            1 => map_functions.remove(0),
            _ => {
                let mut first_item = true;
                let mut collection = TokenStream::new();
                for item in map_functions {
                    if !first_item {
                        collection = quote! {
                            #collection.chain(#item)
                        }
                    } else {
                        first_item = false;
                        collection = item
                    }
                }
                collection
            }
        };

        left_hand_side.push(quote! {
            Box::new(#tokens)
        });
    }
    left_hand_side
}

pub(crate) fn generate(enum_name: &Ident, visited: &VariantVisitor) -> proc_macro2::TokenStream {
    let func_name = format_ident!("iter_variables");
    let k = crate::generate_right_hand(&visited);
    let v = generate_left_hand(&visited, &func_name);

    quote! {
        pub fn #func_name<'a>(&'a self) -> Box<dyn Iterator<Item = String> + 'a> {
            use #enum_name::*;

            match self {
                #( #k => #v, )*
            }
        }
    }
}
