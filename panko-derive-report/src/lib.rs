#![feature(iterator_try_collect)]

use std::borrow::Cow;
use std::collections::HashSet;

use indexmap::IndexMap;
use itertools::Itertools;
use proc_macro2::Ident;
use proc_macro2::Literal;
use proc_macro2::Span;
use proc_macro2::TokenStream;
use quote::quote;
use quote::quote_spanned;
use quote::ToTokens;
use regex::Regex;
use syn::parenthesized;
use syn::parse::Parse;
use syn::parse_macro_input;
use syn::parse_quote;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::visit::Visit;
use syn::Attribute;
use syn::Data;
use syn::DeriveInput;
use syn::Error;
use syn::Expr;
use syn::Lifetime;
use syn::LitStr;
use syn::Pat;
use syn::Result;
use syn::Token;
use syn::Type;

#[derive(Default)]
struct Args(Punctuated<Assign, Token![,]>);

struct Assign(Vec<Attribute>, Pat, Expr);

#[derive(Default)]
struct Diagnostics(IndexMap<Ident, IndexMap<String, Expr>>);

impl Parse for Args {
    fn parse(input: syn::parse::ParseStream) -> syn::Result<Self> {
        Ok(Self(Punctuated::parse_terminated(input)?))
    }
}

impl Parse for Assign {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        let attrs = input.call(Attribute::parse_outer)?;
        let pat = input.call(Pat::parse_multi)?;
        let _: Token![=] = input.parse()?;
        let expr = input.parse()?;
        Ok(Assign(attrs, pat, expr))
    }
}

impl Parse for Diagnostics {
    fn parse(input: syn::parse::ParseStream) -> Result<Self> {
        Ok(Diagnostics(
            Punctuated::<_, Token![,]>::parse_terminated_with(input, |input| {
                let name = input.parse()?;
                let args;
                parenthesized!(args in input);
                let args: Args = args.parse()?;
                Ok((
                    name,
                    args.0
                        .into_iter()
                        .map(|Assign(_, pat, expr)| match pat {
                            Pat::Ident(ident) => Ok((ident.ident.to_string(), expr)),
                            _ => Err(Error::new_spanned(pat, "only ident patterns are allowed")),
                        })
                        .try_collect()?,
                ))
            })?
            .into_iter()
            .collect(),
        ))
    }
}

fn attr_value<T>(attrs: &[Attribute], name: &str) -> Result<Option<T>>
where
    T: Parse,
{
    attrs
        .iter()
        .find_map(|attr| {
            if attr.path().is_ident(name) {
                Some(attr.parse_args())
            }
            else {
                None
            }
        })
        .transpose()
}

#[proc_macro_derive(
    Report,
    attributes(exit_code, error, with, colour, label, help, diagnostics)
)]
pub fn derive_report(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    derive_report_impl(input)
        .unwrap_or_else(|err| err.to_compile_error())
        .into()
}

fn derive_report_impl(input: DeriveInput) -> Result<TokenStream> {
    let name = input.ident;
    let (impl_generics, ty_generics, where_clause) = input.generics.split_for_impl();

    let exit_code = attr_value::<Literal>(&input.attrs, "exit_code")?;

    let Data::Enum(type_) = input.data
    else {
        return Err(Error::new_spanned(name, "must be an enum"));
    };

    let (report_printers, cases, locations): (Vec<_>, Vec<_>, Vec<_>) = type_
        .variants
        .iter()
        .map(|variant| -> Result<_> {
            let diagnostics =
                attr_value::<Diagnostics>(&variant.attrs, "diagnostics")?.unwrap_or_default();

            let mut colours: IndexMap<_, _> = diagnostics
                .0
                .iter()
                .flat_map(|(ident, attrs)| {
                    Some((ident.to_string(), Cow::Borrowed(attrs.get("colour")?)))
                })
                .collect();

            let (fields, field_types): (Vec<_>, Vec<_>) = variant
                .fields
                .iter()
                .map(|field| -> Result<_> {
                    Ok((
                        field.ident.clone().ok_or_else(|| {
                            Error::new_spanned(field, "tuple-like variants not supported")
                        })?,
                        field.ty.clone(),
                    ))
                })
                .process_results(|iter| iter.unzip())?;

            let lifetimes = find_lifetimes(&field_types);

            let extras = attr_value::<Args>(&variant.attrs, "with")?.unwrap_or_default();
            for Assign(attrs, name, value) in &extras.0 {
                if let Pat::Ident(ident) = name {
                    if let Some(colour) = attr_value(attrs, "colour")? {
                        colours.insert(ident.ident.to_string(), Cow::Owned(colour));
                    }
                    else if let Some(colour) =
                        source(value).and_then(|source| colours.get(&source))
                    {
                        colours.insert(ident.ident.to_string(), colour.clone());
                    }
                }
            }

            let error_msg = attr_value(&variant.attrs, "error")?
                .map(|error_msg| gen_format(&colours, error_msg))
                .ok_or_else(|| Error::new_spanned(variant, "`error` attribute missing"))?;
            let help_msg = attr_value(&variant.attrs, "help")?
                .map(|help_msg| gen_format(&colours, help_msg))
                .into_iter();

            let extras = extras
                .0
                .iter()
                .map(|Assign(_, name, value)| quote::quote!(let #name = (|| #value)();));

            let length = diagnostics.0.len();
            let (loc, msg, colour, order): (Vec<_>, Vec<_>, Vec<_>, Vec<_>) =
                itertools::multiunzip(diagnostics.0.iter().enumerate().map(|(i, (name, args))| {
                    let msg = args
                        .get("label")
                        .map(|msg| gen_format(&colours, parse_quote!(#msg)))
                        .into_iter();
                    let msg = quote!(#(.with_message((#msg)()))*);
                    let colour = args.get("colour").into_iter();
                    let colour = quote!(#(.with_color(#colour))*);
                    (
                        quote!(#name.loc()),
                        msg,
                        colour,
                        i32::try_from(length.checked_sub(i).unwrap()).unwrap(),
                    )
                }));

            let name = &variant.ident;
            let print_report = quote_spanned!(name.span() =>
                #[allow(clippy::needless_lifetimes)]
                #[allow(non_snake_case)]
                fn #name<#(#lifetimes,)*>(#(#fields: &#field_types,)* writer: &mut dyn std::io::Write) {
                    #(#extras)*

                    #[allow(clippy::useless_format)]
                    let labels = [
                        #(
                            ::ariadne::Label::new(#loc)
                                #msg
                                #colour
                                .with_order(#order),
                        )*
                    ];

                    #[allow(clippy::double_parens)]
                    #[allow(clippy::redundant_closure_call)]
                    #[allow(clippy::useless_format)]
                    at.loc()
                        .report(::ariadne::ReportKind::Error)
                        .with_message((#error_msg)())
                        .with_labels(labels)
                        .with_config(
                            ::ariadne::Config::default()
                                .with_index_type(::ariadne::IndexType::Byte)
                        )
                        #(.with_help((#help_msg)()))*
                        .finish()
                        .write(at.loc().cache(), writer)
                        .unwrap();
                }
            );
            Ok((
                print_report,
                quote!(Self::#name { #(#fields,)* } => #name(#(#fields,)* writer)),
                quote!(Self::#name { at, .. } => at.loc() ),
            ))
        })
        .process_results(|iter| iter.multiunzip())?;

    let exit_code = quote_spanned!(exit_code.span() =>
        fn exit_code(&self) -> u8 {
            #exit_code
        }
    );

    let expanded = quote::quote!(
        impl #impl_generics ::panko_report::Report for #name #ty_generics #where_clause {
            fn print(&self) {
                self.write(&mut std::io::stderr())
            }

            fn write(&self, writer: &mut dyn std::io::Write) {
                #(#report_printers)*

                match self {
                    #(#cases,)*
                }
            }

            #exit_code

            fn location(&self) -> ::panko_report::Loc {
                match self {
                    #(#locations,)*
                }
            }
        }

        impl #impl_generics ::panko_report::Report for Box<#name #ty_generics> #where_clause {
            fn print(&self) {
                self.as_ref().print()
            }

            fn write(&self, writer: &mut dyn std::io::Write) {
                self.as_ref().write(writer)
            }

            fn exit_code(&self) -> u8 {
                self.as_ref().exit_code()
            }

            fn location(&self) -> ::panko_report::Loc {
                self.as_ref().location()
            }
        }
    );

    Ok(expanded)
}

fn find_lifetimes(field_types: &[Type]) -> impl Iterator<Item = Lifetime> {
    struct CollectLifetimes(HashSet<Lifetime>);
    impl Visit<'_> for CollectLifetimes {
        fn visit_lifetime(&mut self, lifetime: &syn::Lifetime) {
            self.0.insert(lifetime.clone());
        }
    }

    let mut visitor = CollectLifetimes(HashSet::default());
    for ty in field_types {
        visitor.visit_type(ty);
    }
    visitor
        .0
        .into_iter()
        .filter(|lifetime| lifetime.ident != "static")
}

fn gen_format(colours: &IndexMap<String, Cow<Expr>>, format_string: LitStr) -> impl ToTokens {
    let str = format_string.value();
    let placeholders = parse_format_string(&str);
    let format_args = placeholders.into_iter().map(|name| {
        let colour = colours.get(name);
        let name = Ident::new(name, Span::call_site());
        let value = match colour {
            Some(colour) => quote::quote!(::ariadne::Fmt::fg(#name.slice(), #colour)),
            None => quote::quote!(#name.slice()),
        };
        quote::quote!(#name = #value)
    });
    quote_spanned!(format_string.span() => (|| format!(#format_string, #(#format_args,)*)))
}

fn parse_format_string(s: &str) -> HashSet<&str> {
    let regex = Regex::new(r"\{(\w+)\}").unwrap();
    regex
        .captures_iter(s)
        .map(|cap| cap.get(1).unwrap().as_str())
        .collect()
}

fn source(expr: &Expr) -> Option<String> {
    match expr {
        Expr::MethodCall(call) => source(&call.receiver),
        Expr::Path(path) => path.path.get_ident().map(Ident::to_string),
        Expr::Field(field) => source(&field.base),
        _ => None,
    }
}
