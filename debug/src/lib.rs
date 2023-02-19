use proc_macro::TokenStream;
use quote::quote;
use syn::{parse_macro_input, spanned::Spanned, Data, DataStruct, DeriveInput, Ident};

#[proc_macro_derive(CustomDebug)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let fields = match input.data {
        Data::Struct(DataStruct { fields, .. }) => fields,
        _ => {
            let err = syn::Error::new(input.span(), "not a struct");
            return err.to_compile_error().into();
        }
    };

    let impl_struct_name = input.ident;
    let struct_name = impl_struct_name.to_string();
    let struct_fields = fields
        .iter()
        .map(|field| match &field.ident {
            Some(ident) => ident.to_string(),
            None => "".to_string(),
        })
        .collect::<Vec<String>>();
    let self_struct_fields = fields
        .iter()
        .map(|field| &field.ident)
        .collect::<Vec<&Option<Ident>>>();
    let expanded = quote!(
        impl std::fmt::Debug for #impl_struct_name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#struct_name)
                #(.field(#struct_fields, &self.#self_struct_fields))*
                .finish()
            }
        }
    );
    TokenStream::from(expanded)
}
