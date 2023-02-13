use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    parse_macro_input, AngleBracketedGenericArguments, Data, DataStruct, DeriveInput, Field,
    Fields, GenericArgument, Ident, Lit, Meta, MetaList, MetaNameValue, NestedMeta, Path,
    PathArguments, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
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
            let (wrap_type, ident_type) = unwrap_type(field);
            let attribute = unwrap_attribute(field);

            let mut each_name = quote!(#ident);
            if let Some(ref new_each_name) = attribute {
                each_name = quote!(#new_each_name);
            }

            let set_value;
            let mut new_ident_type = quote!(#ident_type);
            match wrap_type {
                WrapType::Option => {
                    set_value = quote!(
                        self.#ident = Some(Some(#each_name));
                    );
                }
                WrapType::Vec => {
                    if attribute.is_some() {
                        set_value = quote!(
                            let _field = self.#ident.get_or_insert(Vec::new());
                            _field.push(#each_name);
                        );
                    } else {
                        set_value = quote!(
                            self.#ident = Some(#each_name);
                        );
                        new_ident_type = quote!(Vec<#ident_type>);
                    }
                }
                WrapType::Raw => {
                    set_value = quote!(
                        self.#ident = Some(#ident);
                    );
                }
            }

            quote!(
                fn #each_name(&mut self, #each_name: #new_ident_type) -> &mut Self {
                    #set_value
                    self
                }
            )
        })
        .collect::<Vec<TokenStream2>>()
}

fn check_builder_field(fields: &Fields) -> Vec<TokenStream2> {
    fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            let (wrap_type, _) = unwrap_type(field);
            match wrap_type {
                WrapType::Option | WrapType::Vec => quote!(),
                WrapType::Raw => {
                    quote!(
                        if self.#ident.is_none() {
                            return Err(Box::<dyn Error>::from("the fields has been explicitly set"))
                        }
                    )
                }
            }
        })
        .collect::<Vec<TokenStream2>>()
}

fn build_method_return(fields: &Fields) -> Vec<TokenStream2> {
    fields
        .iter()
        .map(|field| {
            let ident = &field.ident;
            let (wrap_type, _) = unwrap_type(field);
            match wrap_type {
                WrapType::Option => {
                    quote!(
                        #ident: self.#ident.clone().unwrap_or(None)
                    )
                }
                WrapType::Vec => {
                    quote!(
                        #ident: self.#ident.clone().unwrap_or(Vec::new())
                    )
                }
                WrapType::Raw => {
                    quote!(
                        #ident: self.#ident.clone().unwrap()
                    )
                }
            }
        })
        .collect::<Vec<TokenStream2>>()
}

fn unwrap_attribute(field: &Field) -> Option<Ident> {
    if let Some(attr) = field.attrs.last() {
        if attr.path.is_ident("builder") {
            if let Ok(Meta::List(MetaList { nested, .. })) = attr.parse_meta() {
                if let Some(NestedMeta::Meta(Meta::NameValue(MetaNameValue {
                    lit: Lit::Str(lit_str),
                    ..
                }))) = nested.last()
                {
                    let ident = Ident::new(&lit_str.value(), lit_str.span());
                    return Some(ident);
                }
            }
        }
    }
    None
}

enum WrapType {
    Raw,
    Option,
    Vec,
}

fn unwrap_type(field: &Field) -> (WrapType, Type) {
    if let Type::Path(TypePath {
        path: Path { segments, .. },
        ..
    }) = &field.ty
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
                "Vec" => {
                    if let PathArguments::AngleBracketed(AngleBracketedGenericArguments {
                        args,
                        ..
                    }) = &segment.arguments
                    {
                        if let Some(GenericArgument::Type(ident_type)) = args.last() {
                            return (WrapType::Vec, ident_type.clone());
                        }
                    }
                }
                _ => {}
            }
        }
    }
    (WrapType::Raw, field.ty.clone())
}
