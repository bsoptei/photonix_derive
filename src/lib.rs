extern crate proc_macro;

use proc_macro::{TokenTree, TokenStream};
use quote::*;
use syn::*;
use syn::export::*;

const CRATE_NAME: &str = "photonix_derive";

fn enum_or_struct(item: &TokenStream) -> Option<EnumOrStruct> {
    item.clone().into_iter().fold(None, |temp, token| {
        if temp.is_some() { temp } else {
            match token {
                TokenTree::Ident(ident) => {
                    match ident.to_string().as_str() {
                        "struct" => {
                            // Exit the fold after finding the keyword
                            return Some(EnumOrStruct::Struct);
                        }
                        "enum" => {
                            // Exit the fold after finding the keyword
                            return Some(EnumOrStruct::Enum);
                        }
                        _ => None,
                    }
                }
                _ => None
            }
        }
    })
}

#[proc_macro_derive(Get)]
pub fn get(item: TokenStream) -> TokenStream {
    match enum_or_struct(&item) {
        Some(EnumOrStruct::Struct) => {
            struct_get_impl(parse_macro_input![item as ItemStruct]).into()
        }
        _ => panic![format!("Auto-derive of Get for enums is currently unsupported by {}", CRATE_NAME)]
    }
}

#[allow(unused_variables)]
fn struct_get_impl(struct_definition: ItemStruct) -> TokenStream2 {
    let struct_name = &struct_definition.ident;
    check_generics(WrappedEnumOrStruct::WrappedStruct(&struct_definition), "Get");

    let qs = struct_definition.fields.iter().enumerate().map(|(n, field)| {
        let id = &field.ident;
        let ty = &field.ty;

        match id {
            Some(field_id) => {
                quote![
                    impl Get<#ty> for #struct_name {
                        fn get(self) -> #ty {
                            self.#field_id
                        }
                    }
                ]
            }
            _ => {
                quote![
                    impl Get<#ty> for #struct_name {
                        fn get(self) -> #ty {
                            self.#n
                        }
                    }
                ]
            }
        }
    }).collect::<Vec<TokenStream2>>();

    quote![#(#qs)*]
}

#[proc_macro_derive(GetRef)]
pub fn get_ref(item: TokenStream) -> TokenStream {
    match enum_or_struct(&item) {
        Some(EnumOrStruct::Struct) => {
            struct_get_ref_impl(parse_macro_input![item as ItemStruct]).into()
        }
        _ => panic![format!("Auto-derive of GetRef for enums is currently unsupported by {}", CRATE_NAME)]
    }
}

#[allow(unused_variables)]
fn struct_get_ref_impl(struct_definition: ItemStruct) -> TokenStream2 {
    let struct_name = &struct_definition.ident;
    check_generics(WrappedEnumOrStruct::WrappedStruct(&struct_definition), "GetRef");

    let qs = struct_definition.fields.iter().enumerate().map(|(n, field)| {
        let id = &field.ident;
        let ty = &field.ty;

        match id {
            Some(field_id) => {
                quote![
                    impl GetRef<#ty> for #struct_name {
                        fn get_ref(&self) -> &#ty {
                            &self.#field_id
                        }
                    }
                ]
            }
            _ => {
                quote![
                    impl GetRef<#ty> for #struct_name {
                        fn get_ref(&self) -> &#ty {
                            &self.#n
                        }
                    }
                ]
            }
        }
    }).collect::<Vec<TokenStream2>>();

    quote![#(#qs)*]
}

#[proc_macro_derive(GetOption)]
pub fn get_option(item: TokenStream) -> TokenStream {
    match enum_or_struct(&item) {
        Some(EnumOrStruct::Enum) => {
            get_option_impl(parse_macro_input![item as ItemEnum])
        }
        _ => panic![format!("Auto-derive of GetOption for structs is currently unsupported by {}", CRATE_NAME)]
    }
}

#[allow(unused_variables)]
fn get_option_impl(enum_definition: ItemEnum) -> TokenStream {
    let enum_name = &enum_definition.ident;
    check_generics(WrappedEnumOrStruct::WrappedEnum(&enum_definition), "GetOption");

    let qs = enum_definition.variants.into_iter().map(|variant| {
        let variant_name = &variant.ident;
        let fields = &variant.fields;

        match fields {
            Fields::Named(n) => {
                let types = type_names_for_named(n);
                let field_names = field_names(n);
                let fs = field_names.as_slice();

                let intermediate = types.iter().enumerate().map(|(i, type_name)| {
                    let current_name = nth_item(fs, i).unwrap();
                    let current_type = types.get(i).unwrap();

                    quote![
                        impl GetOption<#current_type> for #enum_name {
                            fn get_option(self) -> Option<#current_type> {
                                match self {
                                    #enum_name::#variant_name{ #(#fs),* } => Some(#current_name),
                                    _ => None
                                }
                            }
                        }
                    ]
                }).collect::<Vec<TokenStream2>>();
                quote![#(#intermediate)*]
            }
            Fields::Unnamed(un) => {
                let types = type_names_for_unnamed(un);
                let get_helper = unnamed_get_helper(types.len());
                let gh = get_helper.as_slice();

                let intermediate = types.into_iter().enumerate().map(|(i, type_name)| {
                    let current_name = nth_item(&get_helper, i).unwrap();

                    quote![
                        impl GetOption<#type_name> for #enum_name {
                            fn get_option(self) -> Option<#type_name> {
                                match self {
                                    #enum_name::#variant_name(#(#gh),*) => Some(#current_name),
                                    _ => None,
                                }
                            }
                        }
                    ]
                }).collect::<Vec<TokenStream2>>();
                quote![#(#intermediate)*]
            }
            _ => quote![]
        }
    }).collect::<Vec<TokenStream2>>();
    quote![#(#qs)*].into()
}

#[proc_macro_derive(Set)]
pub fn set(item: TokenStream) -> TokenStream {
    match enum_or_struct(&item) {
        Some(EnumOrStruct::Struct) => {
            struct_set_impl(parse_macro_input![item as ItemStruct]).into()
        }
        Some(EnumOrStruct::Enum) => {
            enum_set_impl(parse_macro_input![item as ItemEnum])
        }
        _ => panic![format!("Cannot derive Set for {}", item)]
    }
}

fn struct_set_impl(struct_definition: ItemStruct) -> TokenStream2 {
    let struct_name = &struct_definition.ident;
    check_generics(WrappedEnumOrStruct::WrappedStruct(&struct_definition), "Set");

    let qs = struct_definition.fields.iter().enumerate().map(|(n, field)| {
        let id = &field.ident;
        let ty = &field.ty;

        match id {
            Some(field_id) => {
                quote![
                    impl Set<#ty> for #struct_name {
                        fn set(self, new_value: #ty) -> Self {
                            let mut temp_self = self;
                            temp_self.#field_id = new_value;
                            temp_self
                        }
                    }
                ]
            }
            _ => {
                quote![
                    impl Set<#ty> for #struct_name {
                        fn set(self, new_value: #ty) -> Self {
                            let mut temp_self = self;
                            temp_self.#n = new_value;
                            temp_self
                        }
                    }
                ]
            }
        }
    }).collect::<Vec<TokenStream2>>();

    quote![#(#qs)*]
}

#[allow(unused_variables)]
fn enum_set_impl(enum_definition: ItemEnum) -> TokenStream {
    let enum_name = &enum_definition.ident;
    check_generics(WrappedEnumOrStruct::WrappedEnum(&enum_definition), "Set");

    let qs = enum_definition.variants.into_iter().map(|variant| {
        let variant_name = &variant.ident;
        let fields = &variant.fields;

        match fields {
            Fields::Named(n) => {
                let types = type_names_for_named(n);
                let field_names = field_names(n);
                let fs = field_names.as_slice();

                let set_helper = (0..types.len()).map(|n| {
                    let intermediate = field_names.iter().enumerate().map(|(m, field_name)| {
                        if m == n { quote![#field_name: new_value] } else { quote![#field_name] }
                    });
                    quote![#(#intermediate),*]
                }).collect::<Vec<TokenStream2>>();

                let intermediate = types.iter().enumerate().map(|(i, type_name)| {
                    let current = set_helper.get(i).unwrap();

                    quote![
                        impl Set<#type_name> for #enum_name {
                            fn set(self, new_value: #type_name) -> Self {
                                match self {
                                    #enum_name::#variant_name{ #(#fs),* } =>
                                        #enum_name::#variant_name{ #current },
                                    _ => self
                                }
                            }
                        }
                    ]
                }).collect::<Vec<TokenStream2>>();
                quote![#(#intermediate)*]
            }
            Fields::Unnamed(un) => {
                let types = type_names_for_unnamed(un);
                let get_helper = unnamed_get_helper(types.len());
                let gh = get_helper.as_slice();

                let set_helper = (0..types.len()).map(|n| {
                    let intermediate = types.iter().enumerate().map(|(m, type_name)| {
                        let arg_name = gh.get(m);
                        if m == n { quote![new_value] } else { quote![#arg_name] }
                    });
                    quote![#(#intermediate),*]
                }).collect::<Vec<TokenStream2>>();

                let intermediate = types.into_iter().enumerate().map(|(i, type_name)| {
                    let current = nth_item(&set_helper, i).unwrap();

                    quote![
                        impl Set<#type_name> for #enum_name {
                            fn set(self, new_value: #type_name) -> Self {
                                match self {
                                    #enum_name::#variant_name(#(#gh),*) =>
                                        #enum_name::#variant_name(#current),
                                    _ => self
                                }
                            }
                        }
                    ]
                }).collect::<Vec<TokenStream2>>();
                quote![#(#intermediate)*]
            }
            _ => quote![]
        }
    }).collect::<Vec<TokenStream2>>();

    quote![#(#qs)*].into()
}

#[proc_macro_derive(SetOption)]
pub fn set_option(item: TokenStream) -> TokenStream {
    match enum_or_struct(&item) {
        Some(EnumOrStruct::Enum) => {
            set_option_impl(parse_macro_input![item as ItemEnum])
        }
        _ => panic![format!("Auto-derive of SetOption for structs is currently unsupported by {}", CRATE_NAME)]
    }
}

#[allow(unused_variables)]
fn set_option_impl(enum_definition: ItemEnum) -> TokenStream {
    let enum_name = &enum_definition.ident;
    check_generics(WrappedEnumOrStruct::WrappedEnum(&enum_definition), "SetOption");

    let qs = enum_definition.variants.into_iter().map(|variant| {
        let variant_name = &variant.ident;
        let fields = &variant.fields;

        match fields {
            Fields::Named(n) => {
                let types = type_names_for_named(n);
                let field_names = field_names(n);
                let fs = field_names.as_slice();

                let set_helper = (0..types.len()).map(|n| {
                    let intermediate = field_names.iter().enumerate().map(|(m, field_name)| {
                        if m == n { quote![#field_name: new_value] } else { quote![#field_name] }
                    });
                    quote![#(#intermediate),*]
                }).collect::<Vec<TokenStream2>>();

                let intermediate = types.iter().enumerate().map(|(i, type_name)| {
                    let current = set_helper.get(i).unwrap();

                    quote![
                        impl SetOption<#type_name> for #enum_name {
                            fn set_option(self, new_value: #type_name) -> Option<Self> {
                                match self {
                                    #enum_name::#variant_name{ #(#fs),* } =>
                                        Some(#enum_name::#variant_name{ #current }),
                                    _ => None
                                }
                            }
                        }
                    ]
                }).collect::<Vec<TokenStream2>>();
                quote![#(#intermediate)*]
            }
            Fields::Unnamed(un) => {
                let types = type_names_for_unnamed(un);
                let get_helper = unnamed_get_helper(types.len());
                let gh = get_helper.as_slice();

                let set_helper = (0..types.len()).map(|n| {
                    let intermediate = types.iter().enumerate().map(|(m, type_name)| {
                        let arg_name = gh.get(m);
                        if m == n { quote![new_value] } else { quote![#arg_name] }
                    });
                    quote![#(#intermediate),*]
                }).collect::<Vec<TokenStream2>>();

                let intermediate = types.into_iter().enumerate().map(|(i, type_name)| {
                    let current = nth_item(&set_helper, i).unwrap();

                    quote![
                        impl SetOption<#type_name> for #enum_name {
                            fn set_option(self, new_value: #type_name) -> Option<Self> {
                                match self {
                                    #enum_name::#variant_name(#(#gh),*) =>
                                        Some(#enum_name::#variant_name(#current)),
                                    _ => None
                                }
                            }
                        }
                    ]
                }).collect::<Vec<TokenStream2>>();
                quote![#(#intermediate)*]
            }
            _ => quote![]
        }
    }).collect::<Vec<TokenStream2>>();

    quote![#(#qs)*].into()
}

#[proc_macro_derive(Modify)]
pub fn modify(item: TokenStream) -> TokenStream {
    match enum_or_struct(&item) {
        Some(EnumOrStruct::Struct) => {
            struct_modify_impl(parse_macro_input![item as ItemStruct]).into()
        }
        Some(EnumOrStruct::Enum) => {
            enum_modify_impl(parse_macro_input![item as ItemEnum])
        }
        _ => panic![format!("Cannot derive Modify for {}", item)]
    }
}

#[allow(unused_variables)]
fn struct_modify_impl(struct_definition: ItemStruct) -> TokenStream2 {
    let struct_name = &struct_definition.ident;
    check_generics(WrappedEnumOrStruct::WrappedStruct(&struct_definition), "Modify");
    let fields = struct_definition.fields.iter().enumerate();

    let qs = fields.map(|(n, field)| {
        let id = &field.ident;
        let ty = &field.ty;

        match id {
            Some(field_id) => {
                quote![
                    impl Modify<#ty> for #struct_name {
                        fn modify(self, f: impl FnOnce(#ty) -> #ty) -> Self {
                            let mut temp_self = self;
                            temp_self.#field_id = f(temp_self.#field_id);
                            temp_self
                        }
                    }
                ]
            }
            _ => {
                quote![
                    impl Modify<#ty> for #struct_name {
                        fn modify(self, f: impl FnOnce(#ty) -> #ty) -> Self {
                            let mut temp_self = self;
                            temp_self.#n = f(temp_self.#n);
                            temp_self
                        }
                    }
                ]
            }
        }
    }).collect::<Vec<TokenStream2>>();

    quote![#(#qs)*]
}

#[allow(unused_variables)]
fn enum_modify_impl(enum_definition: ItemEnum) -> TokenStream {
    let enum_name = &enum_definition.ident;
    check_generics(WrappedEnumOrStruct::WrappedEnum(&enum_definition), "Modify");

    let qs = enum_definition.variants.into_iter().map(|variant| {
        let variant_name = &variant.ident;
        let fields = &variant.fields;

        match fields {
            Fields::Named(n) => {
                let types = type_names_for_named(n);
                let field_names = field_names(n);
                let fs = field_names.as_slice();

                let modify_helper = (0..types.len()).map(|n| {
                    let intermediate = field_names.iter().enumerate().map(|(m, field_name)| {
                        if m == n { quote![#field_name: f(#field_name)] } else { quote![#field_name] }
                    });
                    quote![#(#intermediate),*]
                }).collect::<Vec<TokenStream2>>();

                let intermediate = types.iter().enumerate().map(|(i, type_name)| {
                    let current = modify_helper.get(i).unwrap();

                    quote![
                        impl Modify<#type_name> for #enum_name {
                            fn modify(self, f: impl FnOnce(#type_name) -> #type_name) -> Self {
                                match self {
                                    #enum_name::#variant_name{ #(#fs),* } =>
                                        #enum_name::#variant_name{ #current },
                                    _ => self
                                }
                            }
                        }
                    ]
                }).collect::<Vec<TokenStream2>>();
                quote![#(#intermediate)*]
            }
            Fields::Unnamed(un) => {
                let types = type_names_for_unnamed(un);
                let get_helper = unnamed_get_helper(types.len());
                let gh = get_helper.as_slice();

                let modify_helper = (0..types.len()).map(|n| {
                    let intermediate = types.iter().enumerate().map(|(m, type_name)| {
                        let arg_name = gh.get(m);
                        if m == n { quote![f(#arg_name)] } else { quote![#arg_name] }
                    });
                    quote![#(#intermediate),*]
                }).collect::<Vec<TokenStream2>>();

                let intermediate = types.into_iter().enumerate().map(|(i, type_name)| {
                    let current = nth_item(&modify_helper, i).unwrap();

                    quote![
                        impl Modify<#type_name> for #enum_name {
                            fn modify(self, f: impl FnOnce(#type_name) -> #type_name) -> Self {
                                match self {
                                    #enum_name::#variant_name(#(#gh),*) =>
                                        #enum_name::#variant_name(#current),
                                    _ => self
                                }
                            }
                        }
                    ]
                }).collect::<Vec<TokenStream2>>();
                quote![#(#intermediate)*]
            }
            _ => quote![]
        }
    }).collect::<Vec<TokenStream2>>();

    quote![#(#qs)*].into()
}

#[proc_macro_derive(ModifyOption)]
pub fn modify_option(item: TokenStream) -> TokenStream {
    match enum_or_struct(&item) {
        Some(EnumOrStruct::Enum) => {
            modify_option_impl(parse_macro_input![item as ItemEnum])
        }
        _ => panic![format!("Auto-derive of ModifyOption for structs is currently unsupported by {}", CRATE_NAME)]
    }
}

#[allow(unused_variables)]
fn modify_option_impl(enum_definition: ItemEnum) -> TokenStream {
    let enum_name = &enum_definition.ident;
    check_generics(WrappedEnumOrStruct::WrappedEnum(&enum_definition), "ModifyOption");

    let qs = enum_definition.variants.into_iter().map(|variant| {
        let variant_name = &variant.ident;
        let fields = &variant.fields;

        match fields {
            Fields::Named(n) => {
                let types = type_names_for_named(n);
                let field_names = field_names(n);
                let fs = field_names.as_slice();

                let modify_helper = (0..types.len()).map(|n| {
                    let intermediate = field_names.iter().enumerate().map(|(m, field_name)| {
                        if m == n { quote![#field_name: f(#field_name)] } else { quote![#field_name] }
                    });
                    quote![#(#intermediate),*]
                }).collect::<Vec<TokenStream2>>();

                let intermediate = types.iter().enumerate().map(|(i, type_name)| {
                    let current = modify_helper.get(i).unwrap();

                    quote![
                        impl ModifyOption<#type_name> for #enum_name {
                            fn modify_option(self, f: impl FnOnce(#type_name) -> #type_name) -> Option<Self> {
                                match self {
                                    #enum_name::#variant_name{ #(#fs),* } =>
                                        Some(#enum_name::#variant_name{ #current }),
                                    _ => None
                                }
                            }
                        }
                    ]
                }).collect::<Vec<TokenStream2>>();
                quote![#(#intermediate)*]
            }
            Fields::Unnamed(un) => {
                let types = type_names_for_unnamed(un);
                let get_helper = unnamed_get_helper(types.len());
                let gh = get_helper.as_slice();

                let modify_helper = (0..types.len()).map(|n| {
                    let intermediate = types.iter().enumerate().map(|(m, type_name)| {
                        let arg_name = gh.get(m);
                        if m == n { quote![f(#arg_name)] } else { quote![#arg_name] }
                    });
                    quote![#(#intermediate),*]
                }).collect::<Vec<TokenStream2>>();

                let intermediate = types.into_iter().enumerate().map(|(i, type_name)| {
                    let current = nth_item(&modify_helper, i).unwrap();

                    quote![
                        impl ModifyOption<#type_name> for #enum_name {
                            fn modify_option(self, f: impl FnOnce(#type_name) -> #type_name) -> Option<Self> {
                                match self {
                                    #enum_name::#variant_name(#(#gh),*) =>
                                        Some(#enum_name::#variant_name(#current)),
                                    _ => None
                                }
                            }
                        }
                    ]
                }).collect::<Vec<TokenStream2>>();
                quote![#(#intermediate)*]
            }
            _ => quote![]
        }
    }).collect::<Vec<TokenStream2>>();

    quote![#(#qs)*].into()
}

#[derive(Debug)]
enum EnumOrStruct {
    Enum,
    Struct,
}

enum WrappedEnumOrStruct<'a> {
    WrappedEnum(&'a ItemEnum),
    WrappedStruct(&'a ItemStruct),
}

fn check_generics(item: WrappedEnumOrStruct, src: &str) {
    let generics = match item {
        WrappedEnumOrStruct::WrappedEnum(item) => &item.generics,
        WrappedEnumOrStruct::WrappedStruct(item) => &item.generics,
    };
    if generics.params.iter().len() > 0 {
        panic!(
            format!(
                "Auto-derive of {} is currently unsupported by {} for items with generic parameters",
                src, CRATE_NAME
            )
        );
    }
}

fn field_names(fields_named: &FieldsNamed) -> Vec<Ident> {
    fields_named.named.iter().map(|field| {
        match &field.ident {
            Some(i) => i.clone(),
            _ => unimplemented!()
        }
    }).collect()
}

fn nth_item<T: Clone>(items: &[T], n: usize) -> Option<T> {
    items.get(n).map(|item| item.clone())
}

fn type_names_for_named(fields_named: &FieldsNamed) -> Vec<Type> {
    fields_named.named.iter().map(|field| field.ty.clone()).collect()
}

fn type_names_for_unnamed(fields_unnamed: &FieldsUnnamed) -> Vec<Type> {
    fields_unnamed.unnamed.iter().map(|field| field.ty.clone()).collect()
}

fn unnamed_get_helper(n: usize) -> Vec<Ident> {
    (0..n).map(|n| Ident::new(&format!("arg{}", n), Span::call_site())).collect()
}
