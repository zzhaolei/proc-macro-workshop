use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    parse_macro_input, parse_quote, spanned::Spanned, AngleBracketedGenericArguments, Data,
    DataStruct, DeriveInput, Expr, ExprLit, Field, GenericArgument, GenericParam, Generics, Lit,
    Meta, MetaNameValue, Path, PathArguments, PathSegment, Type, TypePath,
};

#[proc_macro_derive(CustomDebug, attributes(debug))]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let fields = match &input.data {
        Data::Struct(DataStruct { fields, .. }) => fields,
        _ => {
            let err = syn::Error::new(input.span(), "not a struct");
            return err.to_compile_error().into();
        }
    };

    let impl_struct_name = &input.ident;
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
        .map(unwrap_attribute)
        .collect::<Vec<TokenStream2>>();

    let generics = add_trait_bound(fields, input.generics);
    let (impl_generic, type_generic, where_clause) = generics.split_for_impl();

    let expanded = quote!(
        impl #impl_generic std::fmt::Debug for #impl_struct_name #type_generic #where_clause {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                f.debug_struct(#struct_name)
                #(.field(#struct_fields, #self_struct_fields))*
                .finish()
            }
        }
    );
    TokenStream::from(expanded)
}

fn unwrap_attribute(field: &Field) -> TokenStream2 {
    if let Some(attr) = field.attrs.last() {
        if attr.path().is_ident("debug") {
            if let Meta::NameValue(MetaNameValue {
                value:
                    Expr::Lit(ExprLit {
                        lit: Lit::Str(lit_str),
                        ..
                    }),
                ..
            }) = &attr.meta
            {
                if let Some(ref ident) = field.ident {
                    let lit_str = lit_str.token();
                    return quote!(
                        &std::format_args!(#lit_str, self.#ident) as &dyn std::fmt::Debug
                    );
                }
            }
        }
    }
    if let Some(ref ident) = field.ident {
        quote!(&std::format_args!("{:?}", self.#ident) as &dyn std::fmt::Debug)
    } else {
        quote!()
    }
}

fn add_trait_bound(fields: &syn::Fields, mut generics: Generics) -> Generics {
    for param in &mut generics.params {
        if let GenericParam::Type(ref mut ty_param) = *param {
            if fields
                .iter()
                .filter(|field| {
                    if let Type::Path(TypePath {
                        path: Path { segments, .. },
                        ..
                    }) = &field.ty
                    {
                        if let Some(PathSegment {
                            ident,
                            arguments:
                                PathArguments::AngleBracketed(
                                    AngleBracketedGenericArguments { args, .. },
                                    ..,
                                ),
                        }) = segments.last()
                        {
                            if ident == "PhantomData" {
                                if let Some(GenericArgument::Type(Type::Path(TypePath {
                                    path: Path { segments, .. },
                                    ..
                                }))) = args.first()
                                {
                                    if let Some(segment) = segments.last() {
                                        if segment.ident == ty_param.ident {
                                            return true;
                                        }
                                    }
                                }
                            }
                        }
                    };
                    false
                })
                .count()
                == 1
            {
                continue;
            }
            ty_param.bounds.push(parse_quote!(std::fmt::Debug));
        }
    }
    generics
}
