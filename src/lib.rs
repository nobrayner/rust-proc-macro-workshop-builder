use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn;

type VecTok = Vec<TokenStream2>;

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let syn::DeriveInput { ident, data, .. } = syn::parse(input).unwrap();
    let struct_ident = ident;

    if let syn::Data::Struct(data_struct) = data {
        let syn::DataStruct { fields, .. } = data_struct;

        if let syn::Fields::Named(fields) = fields {
            let named_fields = fields.named;

            match expand_builder(struct_ident, named_fields) {
                Ok(stream) => stream.into(),
                Err(stream) => stream.into(),
            }
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
) -> Result<TokenStream2, TokenStream2> {
    let builder_ident = format_ident!("{}Builder", struct_ident);

    let (initial_values, declarations, functions, return_fields) = expand_fields(fields)?;

    Ok(quote! {
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
            pub fn build(&mut self) -> std::result::Result<#struct_ident, std::boxed::Box<dyn std::error::Error>> {
                Ok(#struct_ident {
                    #(#return_fields),*
                })
            }
        }
    })
}

fn expand_fields(
    fields: syn::punctuated::Punctuated<syn::Field, syn::token::Comma>,
) -> Result<(VecTok, VecTok, VecTok, VecTok), TokenStream2> {
    let mut initial_values: Vec<TokenStream2> = vec![];
    let mut declarations: Vec<TokenStream2> = vec![];
    let mut functions: Vec<TokenStream2> = vec![];
    let mut return_fields: Vec<TokenStream2> = vec![];

    for field in fields.iter() {
        let optional_generic = get_option_generic(field);

        initial_values.push(expand_initial_value(field));

        declarations.push(expand_declaration(field, optional_generic));

        functions.push(expand_function(field, optional_generic)?);

        return_fields.push(expand_return_field(field, optional_generic));
    }

    Ok((initial_values, declarations, functions, return_fields))
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
        quote! { std::option::Option<#inner_ty> }
    } else {
        quote! { std::option::Option<#ty> }
    };

    quote! {
        #vis #ident #colon_token #ty
    }
}

fn expand_function(
    field: &syn::Field,
    optional_generic: Option<&syn::Type>,
) -> Result<TokenStream2, TokenStream2> {
    let syn::Field {
        ident, ty, attrs, ..
    } = field;
    let ident = ident.as_ref().unwrap();

    let generate_plural_function = |name: &syn::Ident| {
        let ty = if let Some(inner_ty) = optional_generic {
            inner_ty
        } else {
            ty
        };

        quote! {
            fn #name(&mut self, #ident: #ty) -> &mut Self {
                self.#ident = std::option::Option::Some(#ident);
                self
            }
        }
    };

    if let Some(singular_ident) = &get_each_function_ident(attrs)? {
        let vec_generic = get_vec_generic(field);

        if let None = vec_generic {
            return Err(syn::Error::new_spanned(
                &field.ty,
                "Cannot specify 'each' for a non-vec field",
            )
            .to_compile_error());
        }

        if singular_ident == ident {
            Ok(generate_plural_function(singular_ident))
        } else {
            let plural_function = generate_plural_function(ident);

            Ok(quote! {
                fn #singular_ident(&mut self, #singular_ident: #vec_generic) -> &mut Self {
                    if let std::option::Option::Some(vec) = self.#ident.as_mut() {
                        vec.push(#singular_ident);
                    } else {
                        self.#ident = std::option::Option::Some(vec![#singular_ident]);
                    }

                    self
                }
                #plural_function
            })
        }
    } else {
        Ok(generate_plural_function(ident))
    }
}

fn expand_return_field(field: &syn::Field, optional_generic: Option<&syn::Type>) -> TokenStream2 {
    let syn::Field { ident, .. } = field;

    if let Some(_) = optional_generic {
        quote! {
            #ident: self.#ident.take(),
        }
    } else {
        if let Some(_) = &get_vec_generic(field) {
            quote! {
                #ident: self.#ident.clone().unwrap_or(vec![])
            }
        } else {
            let error_message = format!("Must provide {}", ident.as_ref().unwrap());

            quote! {
                #ident: self.#ident.take().ok_or(#error_message)?
            }
        }
    }
}

fn get_option_generic(field: &syn::Field) -> Option<&syn::Type> {
    get_generic_from_type_if_wrapper_is(&field.ty, "Option")
}

fn get_vec_generic(field: &syn::Field) -> Option<&syn::Type> {
    get_generic_from_type_if_wrapper_is(&field.ty, "Vec")
}

fn get_generic_from_type_if_wrapper_is<'a>(ty: &'a syn::Type, str: &str) -> Option<&'a syn::Type> {
    if let Some((wrapper, inner)) = get_wrapper_and_inner_from_field_type(ty) {
        if wrapper == str {
            if let Some(ty) = inner {
                Some(ty)
            } else {
                None
            }
        } else {
            None
        }
    } else {
        None
    }
}

fn get_wrapper_and_inner_from_field_type(
    ty: &syn::Type,
) -> Option<(&syn::Ident, Option<&syn::Type>)> {
    match ty {
        syn::Type::Path(syn::TypePath {
            qself: None,
            path: syn::Path { segments, .. },
        }) => {
            let segment = segments.iter().next();

            if segment.is_none() {
                return None;
            }

            let segment = segment.unwrap();

            let ident = &segment.ident;

            let inner = match &segment.arguments {
                syn::PathArguments::AngleBracketed(syn::AngleBracketedGenericArguments {
                    args,
                    ..
                }) => match args.iter().collect::<Vec<_>>().as_slice() {
                    [syn::GenericArgument::Type(ty)] => Some(ty),
                    _ => None,
                },
                _ => None,
            };

            Some((ident, inner))
        }
        _ => None,
    }
}

fn get_each_function_ident(
    attributes: &Vec<syn::Attribute>,
) -> Result<Option<syn::Ident>, TokenStream2> {
    if attributes.len() <= 0 {
        return Ok(None);
    }

    let builder_attribute = get_builder_attribute(attributes);

    if builder_attribute.is_none() {
        return Ok(None);
    };

    let builder_attribute = builder_attribute.unwrap();

    let builder_args = match builder_attribute.parse_args() {
        Ok(args) => Some(args),
        _ => None,
    };

    match builder_args.unwrap() {
        syn::Meta::NameValue(syn::MetaNameValue {
            path: syn::Path { segments, .. },
            lit: syn::Lit::Str(singular_str),
            ..
        }) => match &segments.iter().collect::<Vec<_>>().as_slice() {
            [syn::PathSegment { ident, .. }] => {
                if ident == "each" {
                    return Ok(Some(syn::Ident::new(
                        &singular_str.value(),
                        singular_str.span(),
                    )));
                } else {
                    let path = &builder_attribute.path;
                    let tokens = &builder_attribute.tokens;

                    return Err(syn::Error::new_spanned(
                        quote! { #path #tokens },
                        r#"expected `builder(each = "...")"#,
                    )
                    .to_compile_error());
                }
            }
            _ => Ok(None),
        },
        _ => Ok(None),
    }
}

fn get_builder_attribute(attributes: &Vec<syn::Attribute>) -> Option<&syn::Attribute> {
    for attribute in attributes {
        match attribute.parse_meta() {
            Ok(meta) => {
                if let Some(ident) = meta.path().get_ident() {
                    if ident == "builder" {
                        return Some(attribute);
                    }
                }

                return None;
            }
            _ => return None,
        }
    }

    None
}
