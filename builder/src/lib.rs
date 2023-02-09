use proc_macro::{Span, TokenStream};
use quote::quote;
use syn::{parse_macro_input, Data, DataStruct, DeriveInput, Ident, Type};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    impl_builder(&input)
}

fn impl_builder(input: &DeriveInput) -> TokenStream {
    let struct_name = &input.ident;
    let builder_name = Ident::new(&format!("{}Builder", struct_name), Span::call_site().into());

    let fields = match &input.data {
        Data::Struct(DataStruct { fields, .. }) => fields,
        _ => panic!("not a struct"),
    };
    let field_ident = fields
        .iter()
        .map(|field| &field.ident)
        .collect::<Vec<&Option<Ident>>>();
    let field_type = fields.iter().map(|field| &field.ty).collect::<Vec<&Type>>();

    let field_method = fields.iter().map(|field| {
        let ident = &field.ident;
        let ident_type = &field.ty;
        quote!(
            fn #ident(&mut self, #ident: #ident_type) -> &mut Self {
                self.#ident = Some(#ident);
                self
            }
        )
    });

    quote!(
        pub struct #builder_name {
            #(#field_ident: Option<#field_type>),*
        }

        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#field_ident: None,)*
                }
            }
        }

        use std::error::Error;

        impl #builder_name {
            #(#field_method)*

            pub fn build(&mut self) -> Result<#struct_name, Box<dyn Error>> {
                if #(self.#field_ident.is_none()) || * {
                    return Err(Box::<dyn Error>::from("the fields has been explicitly set"))
                }
                Ok(#struct_name {
                    #(#field_ident: self.#field_ident.clone().unwrap()),*
                })
            }
        }
    )
    .into()
}
