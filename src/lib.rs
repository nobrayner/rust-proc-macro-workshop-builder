use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn;
use quote::{quote, format_ident};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    let syn::DeriveInput { ident, data, .. } = ast;

    if let syn::Data::Struct(data_struct) = data {
        let builder_ident = format_ident!("{}Builder", ident);
        let syn::DataStruct { fields, .. } = data_struct;

        if let syn::Fields::Named(fields) = fields {
            let named_fields = fields.named;

            let mut initial_values: Vec<TokenStream2> = vec![];
            let mut field_declarations: Vec<TokenStream2> = vec![];
            let mut builder_functions: Vec<TokenStream2> = vec![];

            for field in named_fields.iter() {
                let syn::Field { vis, ident, ty, colon_token, .. } = field;

                initial_values.push(quote! {
                    #ident: None
                });
                field_declarations.push(quote! {
                    #vis #ident #colon_token Option<#ty>
                });
                builder_functions.push(quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = Some(#ident);
                        self
                    }
                });
            }

            let expanded = quote! {
                impl #ident {
                    pub fn builder() -> #builder_ident {
                        #builder_ident {
                            #(#initial_values),*
                        }
                    }
                }
                pub struct #builder_ident {
                    #(#field_declarations),*
                }
                impl #builder_ident {
                    #(#builder_functions)*
                }
            };

            // panic!("{}", expanded);

            expanded.into()
        } else {
            panic!("Can't derive a builder for un-named fields")
        }
    } else {
        panic!("Can't derive a builder for a union type")
    }
}
