use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DataStruct, DeriveInput, Fields,
    GenericArgument, Ident, Path, PathArguments, Type, TypePath,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    impl_builder(&input)
}

fn impl_builder(input: &DeriveInput) -> TokenStream {
    let struct_name = &input.ident;
    let builder_name = Ident::new(&format!("{}Builder", struct_name), struct_name.span());

    let fields: &Fields = match &input.data {
        Data::Struct(DataStruct { fields, .. }) => fields,
        _ => panic!("not a struct"),
    };
    let field_ident = field_idents(fields);
    let field_type = field_types(fields);

    let builder_method = builder_methods(fields);
    let check_builder_field = check_builder_field(fields);
    let build_method_return = build_method_return(fields);

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
            #(#builder_method)*

            pub fn build(&mut self) -> Result<#struct_name, Box<dyn Error>> {
                #(#check_builder_field)*

                Ok(#struct_name {
                    #(#build_method_return),*
                })
            }
        }
    )
    .into()
}

fn field_idents(fields: &Fields) -> Vec<&Option<Ident>> {
    fields
        .iter()
        .map(|field| &field.ident)
        .collect::<Vec<&Option<Ident>>>()
}

fn field_types(fields: &Fields) -> Vec<&Type> {
    fields.iter().map(|field| &field.ty).collect::<Vec<&Type>>()
}

fn builder_methods(fields: &Fields) -> Vec<TokenStream2> {
    fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            let (wrap_type, ident_type) = unwrap_type(&field.ty);
            if let WrapType::Option = wrap_type {
                quote!(
                    fn #ident(&mut self, #ident: #ident_type) -> &mut Self {
                        self.#ident = Some(Some(#ident));
                        self
                    }
                )
            } else {
                quote!(
                    fn #ident(&mut self, #ident: #ident_type) -> &mut Self {
                        self.#ident = Some(#ident);
                        self
                    }
                )
            }
        })
        .collect::<Vec<TokenStream2>>()
}

fn check_builder_field(fields: &Fields) -> Vec<TokenStream2> {
    fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            let (wrap_type, _) = unwrap_type(&field.ty);
            if let WrapType::Option = wrap_type {
                quote!()
            } else {
                quote!(
                    if self.#ident.is_none() {
                        return Err(Box::<dyn Error>::from("the fields has been explicitly set"))
                    }
                )
            }
        })
        .collect::<Vec<TokenStream2>>()
}

fn build_method_return(fields: &Fields) -> Vec<TokenStream2> {
    fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            let (wrap_type, _) = unwrap_type(&field.ty);
            if let WrapType::Option = wrap_type {
                quote!(
                    #ident: self.#ident.clone().unwrap_or(None)
                )
            } else {
                quote!(
                    #ident: self.#ident.clone().unwrap()
                )
            }
        })
        .collect::<Vec<TokenStream2>>()
}

enum WrapType {
    Raw,
    Option,
}

fn unwrap_type(ty: &Type) -> (WrapType, Type) {
    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = ty
    {
        if let Some(segment) = segments.last() {
            match segment.ident.to_string().as_str() {
                "Option" => {
                    if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        args,
                        ..
                    }) = &segment.arguments
                    {
                        if let Some(GenericArgument::Type(ident_type)) = args.last() {
                            return (WrapType::Option, ident_type.clone());
                        }
                    }
                }
                _ => (),
            }
        }
    }
    (WrapType::Raw, ty.clone())
}
