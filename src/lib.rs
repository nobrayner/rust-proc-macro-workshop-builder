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

    let (initial_values, declarations, functions, return_fields) = expand_fields(fields);

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
                Ok(#struct_ident {
                    #(#return_fields),*
                })
            }
        }
    }
}

fn expand_fields(
    fields: syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> (VecTok, VecTok, VecTok, VecTok) {
    let mut initial_values: Vec<TokenStream2> = vec![];
    let mut declarations: Vec<TokenStream2> = vec![];
    let mut functions: Vec<TokenStream2> = vec![];
    let mut return_fields: Vec<TokenStream2> = vec![];

    for field in fields.iter() {
        let optional_generic = get_optional_generic(field);

        initial_values.push(expand_initial_value(field));

        declarations.push(expand_declaration(field, optional_generic));

        functions.push(expand_function(field, optional_generic));

        return_fields.push(expand_return_field(field, optional_generic));
    }

    (
        initial_values,
        declarations,
        functions,
        return_fields,
    )
}

fn expand_initial_value(field: &syn::Field) -> TokenStream2 {
    let syn::Field { ident, .. } = field;

    quote! {
        #ident: None
    }
}

fn expand_declaration(field: &syn::Field, optional_generic: Option<&syn::Type>) -> TokenStream2 {
    let syn::Field {
        vis,
        ident,
        colon_token,
        ty,
        ..
    } = field;

    let ty = if let Some(inner_ty) = optional_generic {
        quote! { Option<#inner_ty> }
    } else {
        quote! { Option<#ty> }
    };

    quote! {
        #vis #ident #colon_token #ty
    }
}

fn expand_function(field: &syn::Field, optional_generic: Option<&syn::Type>) -> TokenStream2 {
    let syn::Field { ident, ty, .. } = field;

    let ty = if let Some(inner_ty) = optional_generic {
        quote! { #inner_ty }
    } else {
        quote! { #ty }
    };

    quote! {
        fn #ident(&mut self, #ident: #ty) -> &mut Self {
            self.#ident = Some(#ident);
            self
        }
    }
}

fn expand_return_field(field: &syn::Field, optional_generic: Option<&syn::Type>) -> TokenStream2 {
    let syn::Field { ident, .. } = field;

    if let Some(_) = optional_generic {
        quote! {
            #ident: self.#ident.take(),
        }
    } else {
        let error_message = format!("Must provide {}", ident.as_ref().unwrap());

        quote! {
            #ident: self.#ident.take().ok_or(#error_message)?
        }
    }
}

fn get_optional_generic(field: &syn::Field) -> Option<&syn::Type> {
    match &field.ty {
        syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path { segments, .. },
        }) => match segments.iter().collect::<Vec<_>>().as_slice() {
            [segment] => {
                if segment.ident != "Option" {
                    return None;
                };

                return match &segment.arguments {
                    syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                        args,
                        ..
                    }) => match args.iter().collect::<Vec<_>>().as_slice() {
                        [syn::GenericArgument::Type(ty)] => Some(ty),
                        _ => None,
                    },
                    _ => None,
                };
            }
            _ => None,
        },
        _ => None,
    }
}
