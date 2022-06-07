use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn;

type VecTok = Vec<TokenStream2>;

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let syn::DeriveInput { ident, data, .. } = syn::parse(input).unwrap();
    let struct_ident = ident;

    if let syn::Data::Struct(data_struct) = data {
        let syn::DataStruct { fields, .. } = data_struct;

        if let syn::Fields::Named(fields) = fields {
            let named_fields = fields.named;

            let expanded = expand_builder(struct_ident, named_fields);

            // panic!("{}", expanded);

            expanded.into()
        } else {
            panic!("Can't derive a builder for un-named fields")
        }
    } else {
        panic!("Can't derive a builder for a union type")
    }
}

fn expand_builder(
    struct_ident: syn::Ident,
    fields: syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> TokenStream2 {
    let builder_ident = format_ident!("{}Builder", struct_ident);

    let (initial_values, declarations, functions, checks, struct_fields) = expand_fields(fields);

    quote! {
        impl #struct_ident {
            pub fn builder() -> #builder_ident {
                #builder_ident {
                    #(#initial_values),*
                }
            }
        }
        pub struct #builder_ident {
            #(#declarations),*
        }
        impl #builder_ident {
            #(#functions)*
            pub fn build(&mut self) -> Result<#struct_ident, Box<dyn std::error::Error>> {
                #(#checks)*

                Ok(#struct_ident {
                    #(#struct_fields),*
                })
            }
        }
    }
}

fn expand_fields(
    fields: syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> (VecTok, VecTok, VecTok, VecTok, VecTok) {
    let mut initial_values: Vec<TokenStream2> = vec![];
    let mut declarations: Vec<TokenStream2> = vec![];
    let mut functions: Vec<TokenStream2> = vec![];
    let mut checks: Vec<TokenStream2> = vec![];
    let mut struct_fields: Vec<TokenStream2> = vec![];

    for field in fields.iter() {
        initial_values.push(expand_initial_value(field));

        declarations.push(expand_declaration(field));

        functions.push(expand_function(field));

        checks.push(expand_check(field));

        struct_fields.push(expand_struct_field(field));
    }

    (
        initial_values,
        declarations,
        functions,
        checks,
        struct_fields,
    )
}

fn expand_initial_value(field: &syn::Field) -> TokenStream2 {
    let syn::Field { ident, .. } = field;

    quote! {
        #ident: None
    }
}

fn expand_declaration(field: &syn::Field) -> TokenStream2 {
    let syn::Field {
        vis,
        ident,
        colon_token,
        ty,
        ..
    } = field;

    quote! {
        #vis #ident #colon_token Option<#ty>
    }
}

fn expand_function(field: &syn::Field) -> TokenStream2 {
    let syn::Field { ident, ty, .. } = field;

    quote! {
        fn #ident(&mut self, #ident: #ty) -> &mut Self {
            self.#ident = Some(#ident);
            self
        }
    }
}

fn expand_check(field: &syn::Field) -> TokenStream2 {
    let syn::Field { ident, .. } = field;

    let error_message = format!("Must provide {}", ident.as_ref().unwrap());

    quote! {
        if let None = self.#ident {
            return Err(Box::from(#error_message));
        }
    }
}

fn expand_struct_field(field: &syn::Field) -> TokenStream2 {
    let syn::Field { ident, .. } = field;

    quote! {
        #ident: self.#ident.take().unwrap()
    }
}
