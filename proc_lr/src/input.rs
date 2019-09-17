use super::rules::{Nonterminal, Symbol, SymbolMap, Terminal};
use std::collections::HashMap;
use syn::parse::*;
use syn::*;

enum ScanInput {
    Char(char),
    Str(String),
}

struct ScanRule {
    token: ScanInput,
    terminal: Option<Terminal>,
    production: Block,
}

struct ParseRule {
    lhs: Nonterminal,
    rhs: Vec<Symbol>,
    production: Block,
}

pub(crate) struct Grammar {
    scan_rules: Vec<ScanRule>,
    parse_rules: Vec<ParseRule>,
    symbol_map: SymbolMap,
}

custom_punctuation!(Def, ::=);

impl Parse for Grammar {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut symbol_map = SymbolMap::new();
        let mut scan_rules = Vec::new();
        while let Some(token) = {
            if let Some(c) = input.parse::<Option<LitChar>>()? {
                Some(ScanInput::Char(c.value()))
            } else {
                input
                    .parse::<Option<LitStr>>()?
                    .map(|s| ScanInput::Str(s.value()))
            }
        } {
            input.parse::<Token![=>]>()?;
            let terminal = input.parse::<Option<Ident>>()?.map(|id| {
                let terminal = id.to_string();
                symbol_map.add_terminal(&terminal);
                symbol_map.get_terminal(&terminal)
            });
            if terminal.is_none() {
                input.parse::<Token![_]>()?;
            }
            scan_rules.push(ScanRule {
                token,
                terminal,
                production: input.parse::<Block>()?,
            });
        }

        let mut parse_rules = Vec::new();
        while let Some(nonterminal) = input.parse::<Option<Ident>>()? {
            let lhs = nonterminal.to_string();
            symbol_map.add_nonterminal(&lhs);
            let lhs = symbol_map.get_nonterminal(&lhs);
            input.parse::<Def>()?;
            let mut rhs = Vec::new();
            while let Some(symbol) = {
                if let Some(id) = input.parse::<Option<Ident>>()? {
                    let id = id.to_string();
                    symbol_map.add_nonterminal(&id);
                    Some(symbol_map.get_symbol(&id))
                } else if let Some(c) = input.parse::<Option<LitChar>>()? {
                    let terminal = format!("{}", c.value());
                    if symbol_map.add_terminal(&terminal) {
                        scan_rules.push(ScanRule {
                            token: ScanInput::Char(c.value()),
                            terminal: Some(symbol_map.get_terminal(&terminal)),
                            production: Block {
                                brace_token: token::Brace { span: c.span() },
                                stmts: Vec::new(),
                            },
                        });
                    }
                    Some(symbol_map.get_symbol(&terminal))
                } else if let Some(s) = input.parse::<Option<LitStr>>()? {
                    let terminal = format!("{}", s.value());
                    if symbol_map.add_terminal(&terminal) {
                        scan_rules.push(ScanRule {
                            token: ScanInput::Str(s.value()),
                            terminal: Some(symbol_map.get_terminal(&terminal)),
                            production: Block {
                                brace_token: token::Brace { span: s.span() },
                                stmts: Vec::new(),
                            },
                        });
                    }
                    Some(symbol_map.get_symbol(&terminal))
                } else {
                    None
                }
            } {
                rhs.push(symbol);
            }
            parse_rules.push(ParseRule {
                lhs,
                rhs,
                production: input.parse::<Block>()?,
            });
        }

        Ok(Self {
            scan_rules,
            parse_rules,
            symbol_map,
        })
    }
}
