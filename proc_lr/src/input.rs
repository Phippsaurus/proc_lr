use crate::common_types::*;
use std::collections::HashMap;
use syn::parse::*;
use syn::*;

pub(crate) struct Grammar {
    scan_rules: Vec<ScanRule>,
    parse_rules: Vec<ParseRule>,
    symbol_map: SymbolMap,
    symbol_variants: HashMap<Symbol, (Ident, Type)>,
}

impl Grammar {
    pub(crate) fn symbols(&self) -> &SymbolMap {
        &self.symbol_map
    }
    pub(crate) fn scan_rules(&self) -> &[ScanRule] {
        self.scan_rules.as_ref()
    }
    pub(crate) fn parse_rules(&self) -> &[ParseRule] {
        self.parse_rules.as_ref()
    }
    pub(crate) fn symbol_map(&self) -> &SymbolMap {
        &self.symbol_map
    }
    pub(crate) fn symbol_variants(&self) -> &HashMap<Symbol, (Ident, Type)> {
        &self.symbol_variants
    }
}

custom_punctuation!(Def, ::=);

impl Parse for Grammar {
    fn parse(input: ParseStream) -> Result<Self> {
        let mut symbol_types = HashMap::new();
        while let Some(ident) = input.parse::<Option<Ident>>()? {
            input.parse::<Token![:]>()?;
            symbol_types.insert(ident.to_string(), (ident, input.parse::<Type>()?));
            input.parse::<Token![;]>()?;
        }
        let mut symbol_map = SymbolMap::new();
        symbol_map.add_terminal("$");
        let mut scan_rules = Vec::new();
        while let Some(token) = {
            if let Some(c) = input.parse::<Option<LitChar>>()? {
                Some(ScanInput::Char(c.value()))
            } else if input.parse::<Option<Token![/]>>()?.is_some() {
                Some(ScanInput::Regex(input.parse::<LitStr>()?.value()))
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

            let production = if input.parse::<Option<Token![;]>>()?.is_none() {
                let production = Some(input.parse::<ExprClosure>()?);
                input.parse::<Token![;]>()?;
                production
            } else {
                None
            };
            scan_rules.push(ScanRule::new(token, terminal, production));
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
                    let terminal = c.value().to_string();
                    if symbol_map.add_terminal(&terminal) {
                        scan_rules.push(ScanRule::new(
                            ScanInput::Char(c.value()),
                            Some(symbol_map.get_terminal(&terminal)),
                            None,
                        ));
                    }
                    Some(symbol_map.get_symbol(&terminal))
                } else if let Some(s) = input.parse::<Option<LitStr>>()? {
                    let terminal = s.value();
                    if symbol_map.add_terminal(&terminal) {
                        scan_rules.push(ScanRule::new(
                            ScanInput::Str(s.value()),
                            Some(symbol_map.get_terminal(&terminal)),
                            None,
                        ));
                    }
                    Some(symbol_map.get_symbol(&terminal))
                } else {
                    None
                }
            } {
                rhs.push(symbol);
            }
            let production = if input.parse::<Option<Token![;]>>()?.is_none() {
                let production = Some(input.parse::<ExprClosure>()?);
                input.parse::<Token![;]>()?;
                production
            } else {
                None
            };
            parse_rules.push(ParseRule::new(
                RuleId(parse_rules.len()),
                lhs,
                rhs,
                production,
            ));
        }

        let symbol_variants = symbol_map
            .symbols
            .iter()
            .filter_map(|(name, symbol)| {
                symbol_types
                    .get(name)
                    .map(|symbol_type| (*symbol, symbol_type.clone()))
            })
            .collect();

        Ok(Self {
            scan_rules,
            parse_rules,
            symbol_map,
            symbol_variants,
        })
    }
}
