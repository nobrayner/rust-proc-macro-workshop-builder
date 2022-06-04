use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use syn;
use quote::{quote, format_ident};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let ast: syn::DeriveInput = syn::parse(input).unwrap();

    let syn::DeriveInput { ident, data, .. } = ast;
    let struct_ident = ident;

    if let syn::Data::Struct(data_struct) = data {
        let builder_ident = format_ident!("{}Builder", struct_ident);
        let syn::DataStruct { fields, .. } = data_struct;

        if let syn::Fields::Named(fields) = fields {
            let named_fields = fields.named;

            let mut initial_values: Vec<TokenStream2> = vec![];
            let mut declarations: Vec<TokenStream2> = vec![];
            let mut functions: Vec<TokenStream2> = vec![];
            let mut checks: Vec<TokenStream2> = vec![];
            let mut struct_fields: Vec<TokenStream2> = vec![];

            for field in named_fields.iter() {
                let syn::Field { vis, ident, ty, colon_token, .. } = field;

                initial_values.push(quote! {
                    #ident: None
                });

                declarations.push(quote! {
                    #vis #ident #colon_token Option<#ty>
                });

                functions.push(quote! {
                    fn #ident(&mut self, #ident: #ty) -> &mut Self {
                        self.#ident = Some(#ident);
                        self
                    }
                });

                // FIXME: Actually add ident as a string, not as the literal "#ident"
                let error_message = format!("Must provide {}", ident.as_ref().unwrap());
                checks.push(quote! {
                    if let None = self.#ident {
                        return Err(Box::from(#error_message));
                    }
                });

                struct_fields.push(quote! {
                    #ident: self.#ident.take().unwrap()
                });
            }

            let expanded = quote! {
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
