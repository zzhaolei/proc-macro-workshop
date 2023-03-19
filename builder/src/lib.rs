use proc_macro::TokenStream;
use proc_macro2::{Span, TokenStream as TokenStream2};
use quote::quote;
use syn::{
    parse_macro_input, spanned::Spanned, AngleBracketedGenericArguments, Data, DataStruct,
    DeriveInput, Expr, ExprLit, Field, Fields, GenericArgument, Ident, Lit, MetaNameValue, Path,
    PathArguments, Type, TypePath,
};

#[proc_macro_derive(Builder, attributes(builder))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    match impl_builder(&input) {
        Ok(v) => v,
        Err(e) => e.into(),
    }
}

type ResultTokenStream<T> = Result<T, TokenStream2>;

fn impl_builder(input: &DeriveInput) -> ResultTokenStream<TokenStream> {
    let struct_name = &input.ident;
    let builder_name = Ident::new(&format!("{}Builder", struct_name), struct_name.span());

    let fields: &Fields = match &input.data {
        Data::Struct(DataStruct { fields, .. }) => fields,
        _ => panic!("not a struct"),
    };
    let field_ident = field_idents(fields);
    let field_type = field_types(fields);

    let builder_method = builder_methods(fields)?;
    let check_builder_field = check_builder_field(fields);
    let build_method_return = build_method_return(fields);

    Ok(quote!(
        pub struct #builder_name {
            #(#field_ident: std::option::Option<#field_type>),*
        }

        impl #struct_name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#field_ident: std::option::Option::None,)*
                }
            }
        }

        use std::error::Error;

        impl #builder_name {
            #(#builder_method)*

            pub fn build(&mut self) -> std::result::Result<#struct_name, std::boxed::Box<dyn Error>> {
                #(#check_builder_field)*

                std::result::Result::Ok(#struct_name {
                    #(#build_method_return),*
                })
            }
        }
    )
    .into())
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

fn builder_methods(fields: &Fields) -> ResultTokenStream<Vec<TokenStream2>> {
    let mut methods = Vec::with_capacity(fields.len());
    for field in fields.iter() {
        let ident = &field.ident;
        let (wrap_type, ident_type) = unwrap_type(field);
        let attribute = unwrap_attribute(field)?;

        let mut each_name = quote!(#ident);
        if let Some(ref new_each_name) = attribute {
            each_name = quote!(#new_each_name);
        }

        let set_value;
        let mut new_ident_type = quote!(#ident_type);
        match wrap_type {
            WrapType::Option => {
                set_value = quote!(
                    self.#ident = std::option::Option::Some(std::option::Option::Some(#each_name));
                );
            }
            WrapType::Vec => {
                if attribute.is_some() {
                    set_value = quote!(
                        let _field = self.#ident.get_or_insert(std::vec::Vec::new());
                        _field.push(#each_name);
                    );
                } else {
                    set_value = quote!(
                        self.#ident = std::option::Option::Some(#each_name);
                    );
                    new_ident_type = quote!(std::vec::Vec<#ident_type>);
                }
            }
            WrapType::Raw => {
                set_value = quote!(
                    self.#ident = std::option::Option::Some(#ident);
                );
            }
        }

        methods.push(quote!(
            fn #each_name(&mut self, #each_name: #new_ident_type) -> &mut Self {
                #set_value
                self
            }
        ));
    }
    Ok(methods)
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
                            return std::result::Result::Err(std::boxed::Box::<dyn std::error::Error>::from("the fields has been explicitly set"))
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

fn unwrap_attribute(field: &Field) -> ResultTokenStream<Option<TokenStream2>> {
    if let Some(attr) = field.attrs.last() {
        if attr.path().is_ident("builder") {
            if let Ok(MetaNameValue {
                path,
                eq_token,
                value:
                    Expr::Lit(ExprLit {
                        lit: Lit::Str(lit_str),
                        ..
                    }),
            }) = attr.parse_args::<MetaNameValue>()
            {
                if !path.is_ident("each") {
                    let span = attr
                        .span()
                        .join(path.span())
                        .and_then(|x| x.join(eq_token.span()))
                        .and_then(|x| x.join(lit_str.span()))
                        .unwrap_or(Span::call_site());
                    let err = syn::Error::new(span, r#"expected `builder(each = "...")`"#);
                    return Err(err.into_compile_error());
                }
                let ident = Some(Ident::new(&lit_str.value(), lit_str.span()));
                return Ok(Some(quote!(#ident)));
            }
        }
    }
    Ok(None)
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
