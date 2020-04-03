use crate::common_types::*;
use proc_macro2::TokenStream;
use quote::*;
use std::collections::HashMap;
use syn::{Ident, Type};

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
                    return Some(ScanToken {
                        kind: TokenKind::Production(#id, #production),
                        offset,
                        length,
                    });
                }
            }
            Some(terminal) => {
                let id = terminal.id();
                quote! {
                    return Some(ScanToken {
                        kind: TokenKind::Token(#id),
                        offset,
                        length,
                    });
                }
            }
        };
        match rule.token() {
            ScanInput::Char(c) => quote! {
                if self.input.starts_with(#c) {
                    let token_text = &self.input[..1];
                    self.input = &self.input[1..];

                    let offset = self.offset;
                    let length = 1;
                    self.offset += 1;

                    #return_stmt
                }
            },
            ScanInput::Str(s) => {
                let len = s.len();
                quote! {
                    if self.input.starts_with(#s) {
                        let token_text = &self.input[..#len];
                        self.input = &self.input[#len..];

                        let offset = self.offset;
                        let length = #len;
                        self.offset += #len;

                        #return_stmt
                    }
                }
            }
            ScanInput::Regex(r) => {
                let regex = format!("^({})", r);
                quote! {
                    if let Some(end) = {
                        lazy_static! {
                            static ref REGEX: Regex = Regex::new(#regex).unwrap();
                        }
                        REGEX.find(self.input)
                             .map(|mat| mat.end())
                    } {
                        let token_text = &self.input[..end];
                        self.input = &self.input[end..];

                        let offset = self.offset;
                        let length = end;
                        self.offset += end;

                        #return_stmt
                    }
                }
            }
        }
    });

    quote! {
        struct ScannedTokens<'a> {
            input: &'a str,
            offset: usize,
        }

        impl<'a> ScannedTokens<'a> {
            fn from(input: &'a str) -> Self {
                Self {
                    input,
                    offset: 0,
                }
            }
        }

        impl<'a> Iterator for ScannedTokens<'a> {
            type Item = ScanToken;

            fn next(&mut self) -> Option<Self::Item> {
                use lr::Regex;
                use lr::lazy_static;
                #(#rules)*
                if self.input.is_empty() {
                    Some(ScanToken {
                        kind: TokenKind::Token(#end_symbol),
                        offset: self.offset,
                        length: 0,
                    })
                } else {
                    Some(ScanToken {
                        kind: TokenKind::Invalid,
                        offset: self.offset,
                        length: 1,
                    })
                }
            }
        }
    }
}
