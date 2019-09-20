use crate::common_types::*;
use proc_macro2::TokenStream;
use syn::{Ident, Type};
use quote::*;
use std::collections::HashMap;

pub(crate) fn generate_scanner(
    scan_rules: &[ScanRule],
    end_symbol: Terminal,
    symbol_variants: &HashMap<Symbol, (Ident, Type)>,
) -> TokenStream {
    let end_symbol = end_symbol.id();
    // TODO: Rules should be ordered so ignoring e.g. whitespace happens first
    let rules = scan_rules.iter().map(|rule| {
        let return_stmt = match rule.terminal() {
            None => quote! {},
            Some(terminal) if symbol_variants.contains_key(&Symbol::Terminal(terminal)) => {
                let id = terminal.id();
                let variant = symbol_variants[&Symbol::Terminal(terminal)].0.clone();
                let production = match rule.production() {
                    None => quote! { Symbol::#variant },
                    Some(production) => quote! { Symbol::#variant((#production)(token_text)) },
                };
                quote! {
                    return Some(ScanToken::Production(#id, #production));
                }
            }
            Some(terminal) => {
                let id = terminal.id();
                quote! {
                    return Some(ScanToken::Token(#id));
                }
            }
        };
        match rule.token() {
            ScanInput::Char(c) => quote! {
                if self.input.starts_with(#c) {
                    let token_text = &self.input[..1];
                    self.input = &self.input[1..];
                    #return_stmt
                }
            },
            ScanInput::Str(s) => {
                let len = s.len();
                quote! {
                    if self.input.starts_with(#s) {
                        let token_text = &self.input[..#len];
                        self.input = &self.input[#len..];
                        #return_stmt
                    }
                }
            }
            ScanInput::Regex(r) => {
                let regex = format!("^{}", r);
                quote! {
                    if let Some(end) = Regex::new(#regex)
                            .unwrap()
                            .find(self.input)
                            .map(|mat| mat.end()) {
                        let token_text = &self.input[..end];
                        self.input = &self.input[end..];
                        #return_stmt
                    }
                }
            }
        }
    });

    quote! {
        struct ScannedTokens<'a> {
            input: &'a str,
        }

        impl<'a> Iterator for ScannedTokens<'a> {
            type Item = ScanToken;

            fn next(&mut self) -> Option<Self::Item> {
                use lr::Regex;
                if self.input.is_empty() {
                    return Some(ScanToken::Token(#end_symbol));
                }
                #(#rules)*
                return None;
            }
        }
    }
}
