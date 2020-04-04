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
                        line,
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
                        line,
                    });
                }
            }
        };
        match rule.token() {
            ScanInput::Char(c) => {
                let line_counter = if c == &'\n' {
                    quote! {
                        self.offset = 0;
                        self.line += 1;
                    }
                } else {
                    quote! {
                        self.offset += 1;
                    }
                };
                quote! {
                    if self.input.starts_with(#c) {
                        let token_text = &self.input[..1];
                        self.input = &self.input[1..];

                        let offset = self.offset;
                        let length = 1;
                        let line = self.line;

                        #line_counter

                        #return_stmt
                    }
                }
            },
            ScanInput::Str(s) => {
                let len = s.len();
                let line_counter = if let lines @ 1.. = s.chars().filter(|c| c == &'\n').count() {
                    let offset = len - 1 - s.rfind('\n').unwrap();
                    quote! {
                        self.offset = #offset;
                        self.line += #lines;
                    }
                } else {
                    quote! {
                        self.offset += #len;
                    }
                };
                quote! {
                    if self.input.starts_with(#s) {
                        let token_text = &self.input[..#len];
                        self.input = &self.input[#len..];

                        let offset = self.offset;
                        let length = #len;
                        let line = self.line;

                        #line_counter

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
                        let line = self.line;

                        let num_lines = token_text.chars().filter(|c| c == &'\n').count();
                        if num_lines == 0 {
                            self.offset += end;
                        } else {
                            self.line += num_lines;
                            self.offset = end - 1 - token_text.rfind('\n').unwrap();
                        }

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
            line: usize,
        }

        impl<'a> ScannedTokens<'a> {
            fn from(input: &'a str) -> Self {
                Self {
                    input,
                    offset: 0,
                    line: 0,
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
                        line: self.line,
                    })
                } else {
                    Some(ScanToken {
                        kind: TokenKind::Invalid,
                        offset: self.offset,
                        length: 1,
                        line: self.line,
                    })
                }
            }
        }
    }
}
