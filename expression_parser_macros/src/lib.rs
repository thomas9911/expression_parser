use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use syn::punctuated::Punctuated;
use syn::token::Comma;
use syn::visit::{self, Visit};
use syn::{parse_macro_input, DeriveInput};
use syn::{Field, Fields, FieldsUnnamed, Ident};
use syn::{PathSegment, Variant};

mod iter_variables;

#[derive(Debug, Default)]
pub(crate) struct VariantVisitor<'ast> {
    items: Vec<StripedField<'ast>>,
}

#[derive(Debug)]
pub(crate) struct StripedField<'ast> {
    ident: &'ast Ident,
    are_vec_arguments: Vec<bool>,
}

#[derive(Debug, Default)]
pub(crate) struct PathSegmentVisitor {
    is_vec: bool,
}

impl<'ast> Visit<'ast> for PathSegmentVisitor {
    fn visit_path_segment(&mut self, node: &'ast PathSegment) {
        // Vec is the top type so only check if it is not a vec (aka the default).
        if !self.is_vec {
            self.is_vec = !node.arguments.is_empty();
        }

        visit::visit_path_segment(self, node);
    }
}

impl<'ast> Visit<'ast> for VariantVisitor<'ast> {
    fn visit_variant(&mut self, node: &'ast Variant) {
        if let Variant {
            ident,
            fields:
                Fields::Unnamed(FieldsUnnamed {
                    unnamed: fields, ..
                }),
            ..
        } = node
        {
            self.items.push(StripedField {
                ident,
                are_vec_arguments: args_vec_or_not(fields),
            });
        };

        // Delegate to the default impl to visit any nested functions.
        visit::visit_variant(self, node);
    }
}

fn args_vec_or_not(fields: &Punctuated<Field, Comma>) -> Vec<bool> {
    fields.iter().map(vec_or_not).collect::<Vec<_>>()
}

fn vec_or_not(field: &Field) -> bool {
    let mut visitor = PathSegmentVisitor::default();
    visitor.visit_field(field);
    visitor.is_vec
}

fn collect_info<'ast>(input: &'ast DeriveInput) -> VariantVisitor<'ast> {
    let mut visitor = VariantVisitor::default();
    visitor.visit_derive_input(&input);
    visitor
}

pub(crate) fn generate_right_hand<'ast>(visited: &VariantVisitor<'ast>) -> Vec<TokenStream> {
    let mut right_hand_side = Vec::new();
    for visit in &visited.items {
        let ident = visit.ident;
        let args = crate::generate_tuple_variables(&visit.are_vec_arguments);

        let tokens = quote! {
            #ident(#(#args),*)
        };

        right_hand_side.push(tokens);
    }

    right_hand_side
}

pub(crate) fn generate_tuple_variables<'a>(
    args: &'a [bool],
) -> Box<dyn Iterator<Item = Ident> + 'a> {
    Box::new(
        args.iter()
            .enumerate()
            .map(|(i, _)| format_ident!("x_{}", i)),
    )
}

#[proc_macro_derive(DeriveFunctions)]
pub fn my_derive(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let visited = collect_info(&input);
    let enum_name = &input.ident;

    let iter_var = iter_variables::generate(enum_name, &visited);

    let output: proc_macro2::TokenStream = quote! {
        impl #enum_name {
            #iter_var
        }
    };

    proc_macro::TokenStream::from(output)
}
