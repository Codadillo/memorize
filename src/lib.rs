use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::quote;
use syn::{
    parenthesized,
    parse::{Parse, ParseStream},
    punctuated::Punctuated,
    spanned::Spanned,
    token::Paren,
    Error, Expr, FnArg, Ident, ItemFn, Pat, RangeLimits, Result, ReturnType, Token,
};

fn idents<const N: usize>(bases: [&str; N]) -> [Ident; N] {
    bases.map(|base| {
        syn::parse(
            format!(
                "{base}{}",
                random_string::generate(
                    10,
                    "qwertyuioplkjhgfdsazxcvbnmQWERTYUIOPLKJHGFDSAZXCVBNM1234567890"
                )
            )
            .parse::<TokenStream>()
            .unwrap(),
        )
        .unwrap()
    })
}

fn ident(base: &str) -> Ident {
    let [ident] = idents([base]);
    ident
}

mod kw {
    use syn::custom_keyword;

    custom_keyword!(format);
    custom_keyword!(inline);

    custom_keyword!(domain);
}

enum Format {
    Inline,
}

impl Parse for Format {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::inline) {
            input.parse::<kw::inline>()?;
            Ok(Format::Inline)
        } else {
            Err(lookahead.error())
        }
    }
}

enum Domain {
    Range {
        start: Expr,
        end: Expr,
        extend_end: TokenStream2,
    },
    Expr(Expr),
}

impl Parse for Domain {
    fn parse(input: ParseStream) -> Result<Self> {
        let expr: Expr = input.parse()?;
        Ok(match expr.clone() {
            Expr::Range(range) => {
                let extend_end = match range.limits {
                    RangeLimits::Closed(_) => quote!(true),
                    RangeLimits::HalfOpen(_) => quote!(false),
                };
                match (range.from, range.to) {
                    (Some(start), Some(end)) => Self::Range {
                        start: *start,
                        end: *end,
                        extend_end,
                    },
                    _ => {
                        return Err(Error::new(
                            expr.span(),
                            "memorize: expected a bounded range",
                        ))
                    }
                }
            }
            expr => Self::Expr(expr),
        })
    }
}

enum Argument {
    Format(Format),
    Domain(Vec<Domain>),
}

impl Parse for Argument {
    fn parse(input: ParseStream) -> Result<Self> {
        let lookahead = input.lookahead1();
        if lookahead.peek(kw::format) {
            input.parse::<kw::format>()?;
            input.parse::<Token![=]>()?;
            Ok(Argument::Format(input.parse()?))
        } else if lookahead.peek(kw::domain) {
            input.parse::<kw::domain>()?;
            input.parse::<Token![=]>()?;

            if input.peek(Paren) {
                let content;
                parenthesized!(content in input);

                let subdomains: Punctuated<Domain, Token![,]> =
                    content.parse_terminated(Domain::parse)?;

                Ok(Argument::Domain(subdomains.into_iter().collect()))
            } else {
                Ok(Argument::Domain(vec![input.parse()?]))
            }
        } else {
            Err(lookahead.error())
        }
    }
}

struct Arguments {
    format: Format,
    domain: Vec<Domain>,
}

impl Parse for Arguments {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut format = Format::Inline;
        let mut domain = None;

        let raw_args: Punctuated<Argument, Token![,]> = input.parse_terminated(Argument::parse)?;
        for arg in raw_args {
            match arg {
                Argument::Format(f) => format = f,
                Argument::Domain(d) => domain = Some(d),
            }
        }

        match domain {
            Some(domain) => Ok(Arguments { format, domain }),
            None => Err(Error::new(
                input.span(),
                "memorize: unspecified memorization domain",
            )),
        }
    }
}

#[proc_macro_attribute]
pub fn memorize(attr: TokenStream, item: TokenStream) -> TokenStream {
    let func: ItemFn = syn::parse(item).unwrap();

    if func.sig.constness.is_none() {
        return Error::new(func.sig.span(), "memorize: expected const function")
            .to_compile_error()
            .into();
    }

    let range = match &func.sig.output {
        ReturnType::Type(_, ty) => ty,
        ReturnType::Default => {
            return Error::new(func.sig.span(), "memorize: expected return type")
                .to_compile_error()
                .into();
        }
    };

    let mut true_func_arg_idents = vec![];
    let mut func_arg_idents = vec![];
    let mut func_arg_types = vec![];
    for func_arg in &func.sig.inputs {
        if let FnArg::Typed(func_arg) = func_arg {
            if let Pat::Ident(pat) = func_arg.pat.as_ref() {
                true_func_arg_idents.push(pat.clone());
                func_arg_idents.push(ident(&pat.ident.to_string()));
                func_arg_types.push(*func_arg.ty.clone());
                continue;
            }
        }
        return Error::new(func_arg.span(), "memorize: invalid argument format")
            .to_compile_error()
            .into();
    }

    let name = func.sig.ident.clone();

    let args: Arguments = syn::parse(attr).unwrap();

    let mut domain_func_args = vec![1; args.domain.len()];
    for (arg_count, domain) in domain_func_args.iter_mut().zip(&args.domain) {
        if let Domain::Expr(_) = domain {
            *arg_count = func_arg_types.len() - args.domain.len() + 1;
            break;
        }
    }

    let memorization = match args.format {
        Format::Inline => {
            let mut context = quote!();
            let mut memory = quote!(#name(#(#func_arg_idents),*));
            let mut memory_type = quote!(#range);
            let mut probe_memory = quote!(return *submemory);

            let mut i = 0;
            for (arg_count, subdomain) in domain_func_args.into_iter().zip(args.domain) {
                (context, memory, memory_type, probe_memory) = match subdomain {
                    Domain::Expr(expr) => {
                        let arg_idents = &func_arg_idents[i..i + arg_count];
                        let arg_types = &func_arg_types[i..i + arg_count];

                        let memory_index_ident = ident("MEMORY_INDEX");

                        let arg_indexes: Vec<TokenStream2> = if arg_count != 1 {
                            (0..arg_count)
                                .map(|i| format!(".{i}").parse().unwrap())
                                .collect()
                        } else {
                            vec![quote!()]
                        };

                        (
                            quote! {
                                #context

                                const #memory_index_ident: [(#(#arg_types),*); #expr.len()] = #expr;
                            },
                            // Unfortunately unsafe seems like the only way to do this currently.
                            //
                            // We don't want to require that #range be Copy and Default,
                            // so there can't be a safe default value for MEMORY.
                            //
                            // We also can't just set MEMORY to what it needs to be directly
                            // using macros because there's no way for the macro to extract the
                            // length of the array that #expr evaluates to
                            // (we don't want to force the user to supply it).
                            quote! {
                                {
                                    let mut mem: [std::mem::MaybeUninit<#memory_type>; #memory_index_ident.len()] = unsafe {
                                        std::mem::MaybeUninit::uninit().assume_init()
                                    };

                                    let mut i = 0;
                                    while i < mem.len() {
                                        let (#(#arg_idents),*) = #memory_index_ident[i];

                                        mem[i as usize] = std::mem::MaybeUninit::new(#memory);

                                        i += 1;
                                    }

                                    unsafe { std::mem::transmute(mem) }
                                }
                            },
                            quote!([#memory_type; #memory_index_ident.len() as usize]),
                            quote! {
                                let mut i = 0;
                                while i < #memory_index_ident.len() {
                                    if #(#memory_index_ident[i]#arg_indexes == #func_arg_idents)&&* {
                                        let submemory = &submemory[i];
                                        #probe_memory
                                    }

                                    i += 1;
                                }
                            },
                        )
                    }
                    Domain::Range {
                        start,
                        end,
                        extend_end,
                    } => {
                        let arg_ident = &func_arg_idents[i];
                        let arg_type = &func_arg_types[i];
                        let [start_ident, end_ident, size_ident] = idents(["START", "END", "SIZE"]);
                        (
                            quote! {
                                #context

                                const #start_ident: #arg_type = #start;
                                const #end_ident: #arg_type = #end + if #extend_end { 1 } else { 0 };
                                const #size_ident: #arg_type = #end_ident - #start_ident;
                            },
                            quote! {
                                {
                                    let mut mem: [std::mem::MaybeUninit<#memory_type>; #size_ident as usize] = unsafe {
                                        std::mem::MaybeUninit::uninit().assume_init()
                                    };

                                    let mut i: #arg_type = 0;
                                    while i < #size_ident {
                                        let #arg_ident = #start_ident + i;

                                        mem[i as usize] = std::mem::MaybeUninit::new(#memory);

                                        i += 1;
                                    }

                                    unsafe { std::mem::transmute(mem) }
                                }
                            },
                            quote!([#memory_type; #size_ident as usize]),
                            quote! {
                                if #start_ident <= #arg_ident && #arg_ident < #end_ident {
                                    let submemory = &submemory[(#arg_ident - #start_ident) as usize];
                                    #probe_memory
                                }
                            },
                        )
                    }
                };

                i += arg_count;
            }

            quote! {
                #context

                const MEMORY: #memory_type = #memory;

                let submemory = &MEMORY;
                #probe_memory
            }
        }
    };

    let sig = func.sig.clone();

    let out = quote! {
        #sig {
            #func

            #(let #func_arg_idents = #true_func_arg_idents;)*

            #memorization

            #name(#(#func_arg_idents),*)
        }
    }
    .into();

    println!("{}", out);

    out
}
