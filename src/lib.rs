#![cfg_attr(feature = "nightly", feature(proc_macro_diagnostic))]

extern crate proc_macro;

use darling::*;
use darling::util::Flag;
use proc_macro::TokenStream;
use proc_macro2::{TokenStream as SynTokenStream};
use std::result::Result;
use syn::*;
use syn::export::Span;
use syn::spanned::Spanned;
use quote::*;

mod relax_lt;

#[cfg(feature = "nightly")]
fn error(span: Span, data: &str) -> SynTokenStream {
    span.unstable().error(data).emit();
    SynTokenStream::new()
}

#[cfg(not(feature = "nightly"))]
fn error(_: Span, data: &str) -> SynTokenStream {
    quote! { compile_error!(#data); }
}

#[derive(Debug, Clone, FromDeriveInput)]
#[darling(attributes(builder), supports(struct_named))]
struct ContainerAttrs {
    ident: Ident,
    generics: Generics,

    /// Use the core library rather than the std library.
    #[darling(default)]
    no_std: Flag,

    /// Whether to accept any field that converts via `Into` by default.
    #[darling(default)]
    into: bool,

    /// Whether to strip `Option<T>` into `T` in the parameters for the builder method by default.
    #[darling(default)]
    strip_option: bool,

    /// Whether to generate setters for this struct's fields by default. If set to false,
    /// `generate_public` and `generate_private` are ignored.
    #[darling(default)]
    generate: Option<bool>,

    /// Whether to generate setters for this struct's public fields by default.
    #[darling(default)]
    generate_public: Option<bool>,

    /// Whether to generate setters for this struct's private fields by default.
    #[darling(default)]
    generate_private: Option<bool>,
}

#[derive(Debug, Clone, FromField)]
#[darling(attributes(builder), forward_attrs(doc))]
struct FieldAttrs {
    attrs: Vec<Attribute>,

    /// The name of the generated setter method.
    #[darling(default)]
    rename: Option<Ident>,

    /// Whether to accept any field that converts via `Into` to this field's type.
    #[darling(default)]
    into: Option<bool>,

    /// Whether to strip `Option<T>` into `T` in the parameters for the builder method.
    #[darling(default)]
    strip_option: Option<bool>,

    /// Whether to generate a setter for this field regardless of global settings.
    #[darling(default)]
    generate: bool,

    /// Whether to skip this field regardless of global settings. Overwrites `generate`.
    #[darling(default)]
    skip: bool,
}

struct ContainerDef {
    name: Ident,
    ty: Type,
    std: Ident,
    generics: Generics,

    uses_into: bool,
    strip_option: bool,
    generate_public: bool,
    generate_private: bool,
}

struct FieldDef {
    field_name: Ident,
    field_ty: Type,
    field_doc: SynTokenStream,
    setter_name: Ident,
    uses_into: bool,
    strip_option: bool,
}

fn init_container_def(input: &DeriveInput) -> Result<ContainerDef, SynTokenStream> {
    let darling_attrs: ContainerAttrs = match FromDeriveInput::from_derive_input(input) {
        Ok(v) => v,
        Err(e) => return Err(e.write_errors()),
    };

    let ident = &darling_attrs.ident;
    let (_, generics_ty, _) = darling_attrs.generics.split_for_impl();
    let ty = quote! { #ident #generics_ty };

    let generate = darling_attrs.generate.unwrap_or(true);
    Ok(ContainerDef {
        name: darling_attrs.ident,
        ty: parse2(ty).expect("Internal error: failed to parse internally generated type."),
        std: if darling_attrs.no_std.is_some() {
            Ident::new("core", Span::call_site())
        } else {
            Ident::new("std", Span::call_site())
        },
        generics: darling_attrs.generics,
        uses_into: darling_attrs.into,
        strip_option: darling_attrs.strip_option,
        generate_public: generate && darling_attrs.generate_public.unwrap_or(true),
        generate_private: generate && darling_attrs.generate_private.unwrap_or(true),
    })
}

fn init_field_def(
    container: &ContainerDef, field: &Field,
) -> Result<Option<FieldDef>, SynTokenStream> {
    // Decode the various attribute options.
    let darling_attrs: FieldAttrs = match FromField::from_field(field) {
        Ok(v) => v,
        Err(e) => return Err(e.write_errors()),
    };

    // Check whether this field should generate a setter.
    if darling_attrs.skip { return Ok(None) }
    let generates = match field.vis {
        Visibility::Public(_) => container.generate_public,
        _ => container.generate_private,
    };
    if !(darling_attrs.generate || generates) { return Ok(None) }

    // Returns a definition for this field.
    let ident = match &field.ident {
        Some(i) => i.clone(),
        None => panic!("Internal error: init_field_def on wrong item."),
    };
    Ok(Some(FieldDef {
        field_name: ident.clone(),
        field_ty: field.ty.clone(),
        field_doc: if let Visibility::Public(_) = field.vis {
            let doc_str =
                format!("Sets the [`{}`](#structfield.{}) field of this struct.", ident, ident);
            quote! { #[doc = #doc_str] }
        } else {
            let attrs = darling_attrs.attrs;
            quote! { #( #attrs )* }
        },
        setter_name: darling_attrs.rename.unwrap_or_else(|| ident.clone()),
        uses_into: darling_attrs.into.unwrap_or(container.uses_into),
        strip_option: darling_attrs.strip_option.unwrap_or(container.strip_option),
    }))
}


fn generate_builder_method(
    container: &ContainerDef, def: FieldDef,
) -> Result<SynTokenStream, SynTokenStream> {
    let FieldDef {
        field_name, mut field_ty, field_doc, setter_name, ..
    } = def;
    let std = &container.std;

    // Strips `Option<T>` into `T` if the `strip_option` property is set.
    let mut stripped_option = false;
    if def.strip_option {
        if let Type::Path(path) = &field_ty {
            if path.path.segments.len() == 1 {
                let segment = path.path.segments.first().unwrap();
                if segment.ident.to_string() == "Option" {
                    if let PathArguments::AngleBracketed(path) = &segment.arguments {
                        if let GenericArgument::Type(tp) = path.args.first().unwrap() {
                            field_ty = tp.clone();
                            stripped_option = true;
                        }
                    }
                }
            }
        }
    }

    // Relax any lifetimes in the value.
    let (generics, where_generics, value_ty, container_ty) =
        relax_lt::relax_lifetimes(&field_ty, &container.ty)?;

    // The type the builder accepts.
    let value_ty = if def.uses_into {
        quote! { impl ::#std::convert::Into<#value_ty> }
    } else {
        quote! { #value_ty }
    };

    // The expression actually stored into the field.
    let mut expr = quote! { value };
    if def.uses_into { expr = quote! { #expr.into() }; }
    if stripped_option { expr = quote! { Some(#expr) }; }

    // Generates the setter method itself.
    let container_name = &container.name;
    Ok(quote! {
        #field_doc
        pub fn #setter_name #generics (self, value: #value_ty) -> #container_ty #where_generics {
            #container_name { #field_name: #expr, ..self }
        }
    })
}

fn generate_builder(input: &DeriveInput, data: &DataStruct) -> Result<TokenStream, TokenStream> {
    let container_def = init_container_def(&input)?;
    let mut toks = SynTokenStream::new();
    for field in &data.fields {
        if let Some(field_def) = init_field_def(&container_def, field)? {
            let method = generate_builder_method(&container_def, field_def)?;
            toks.extend(method);
        }
    }
    let (generics_bound, _, generics_where) = container_def.generics.split_for_impl();
    let ty = container_def.ty;
    Ok(quote! {
        impl #generics_bound #ty #generics_where {
            #toks
        }
    }.into())
}

#[proc_macro_derive(InherentBuilder, attributes(builder))]
pub fn derive_inherent_builder(input: TokenStream) -> TokenStream {
    let input: DeriveInput = parse_macro_input!(input);
    if let Data::Struct(data) = &input.data {
        match generate_builder(&input, data) {
            Ok(toks) => toks,
            Err(toks) => toks,
        }
    } else {
        error(input.span(), "`#[derive(InherentBuilder)] may only be used on structs.").into()
    }
}