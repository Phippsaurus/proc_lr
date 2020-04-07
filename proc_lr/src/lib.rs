#![recursion_limit = "256"]
#![feature(exclusive_range_pattern)]
#![feature(half_open_range_patterns)]
extern crate proc_macro;

mod common_types;
mod debug_output;
mod input;
mod output;
mod rules;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::ToTokens;
use quote::*;
use std::collections::{BTreeSet, HashMap};
use syn::parse_macro_input;

use common_types::*;
use debug_output::generate_debug_output;
use input::*;
use output::*;
use rules::*;

#[proc_macro]
pub fn grammar(tokens: TokenStream) -> TokenStream {
    let grammar = parse_macro_input!(tokens as Grammar);
    let symbols = grammar.symbols();
    let end_symbol = symbols["$"].terminal();
    let mut rev_symbols = HashMap::new();
    for (k, v) in symbols.symbols.iter() {
        rev_symbols.insert(v, k.as_str());
    }
    let mut idx = 0;
    let mut start_lookahead = BTreeSet::new();
    start_lookahead.insert(end_symbol);
    let first_sets = first_sets(grammar.parse_rules());
    let mut states =
        vec![
            LookaheadRule::new(grammar.parse_rules().first().unwrap(), start_lookahead)
                .closure(grammar.parse_rules(), &first_sets),
        ];
    let mut state_ids = HashMap::new();
    let mut table = Vec::new();
    state_ids.insert(states[idx].rules.clone(), StateId::new(idx));
    let symbols = symbols.list();

    while idx < states.len() {
        let mut new_states = Vec::new();
        let reduce_rules = states[idx]
            .rules
            .iter()
            .filter(|rule| rule.is_reducible())
            .map(|rule| (rule.id(), rule.lookahead.clone()));
        let mut reduces = <HashMap<Terminal, Vec<RuleId>>>::new();
        for (id, lookahead) in reduce_rules {
            for terminal in lookahead.iter() {
                reduces.entry(*terminal).or_default().push(id);
            }
        }
        let state_transitions = states[idx]
            .explore(symbols, grammar.parse_rules(), &first_sets)
            .map(|(symbol, state)| {
                if state.is_empty() {
                    if symbol.is_terminal() && reduces.contains_key(&symbol.terminal()) {
                        let rules = &reduces[&symbol.terminal()];
                        if rules.len() == 1 {
                            if rules[0].0 == 0 {
                                Action::Accept
                            } else {
                                Action::Conflict(Conflict::ReduceReduce(rules[0], rules[1]))
                            }
                        } else {
                            Action::Undefined
                        }
                    } else {
                        let id = if let Some(id) = state_ids.get(&state.rules) {
                            *id
                        } else {
                            let id = StateId::new(states.len() + new_states.len());
                            state_ids.insert(state.rules.clone(), id);
                            new_states.push(state);
                            id
                        };
                        if symbol.is_terminal() {
                            let terminal = symbol.terminal();
                            if reduces.contains_key(&terminal) {
                                Action::Conflict(Conflict::ShiftReduce(id, reduces[&terminal][0]))
                            } else {
                                Action::Shift(id)
                            }
                        } else {
                            Action::Goto(id)
                        }
                    }
                })
                .collect::<Vec<_>>(),
        );

        states.append(&mut new_states);
        idx += 1;
    }

    let output = generate_debug_output(&grammar, &table, &symbols, &states, &rev_symbols);

    let num_symbols = symbols.len();
    let num_states = states.len();

    let variants = grammar.symbol_variants();
    let reduce_productions = grammar.parse_rules().iter().map(|rule| {
        let id = rule.id().0;
        let goto = rule.lhs().id();
        let pop = rule.rhs().len();
        if let Some((name, _)) = variants.get(&Symbol::Nonterminal(rule.lhs())) {
            let ident = name.clone();
            match rule.production() {
                None => quote! {
                    #id => {
                        values.push(Symbol::#ident);
                        (#goto, #pop)
                    }
                },
                Some(production) => {
                    let arg_count = rule
                        .rhs()
                        .iter()
                        .filter(|symbol| variants.contains_key(symbol))
                        .count();
                    let args = (0..arg_count).map(|_arg_idx| {
                        quote! {
                            args.next().unwrap().into()
                        }
                    });
                    quote! {
                        #id => {
                            let new_value = {
                                let mut args = values.drain(values.len() - #arg_count..);
                                (#production)(#(#args),*)
                            };
                            values.push(Symbol::#ident(new_value));
                            (#goto, #pop)
                        }
                    }
                }
            }
        } else {
            quote! {
                #id => (#goto, #pop),
            }
        }
    });

    let actions = table.iter().map(|row| {
        let actions = row.iter().map(|action| match action {
            Action::Undefined => quote! { Action::Undefined },
            Action::Accept => quote! { Action::Accept },
            Action::Goto(StateId(id)) => quote! { Action::Goto(#id) },
            Action::Shift(StateId(id)) => quote! { Action::Shift(#id) },
            Action::Reduce(RuleId(id)) => quote! { Action::Reduce(#id) },
            Action::Conflict(_) => {
                panic!("Cannot make a parser from conflicting rules: {:#?}", action)
            }
        });
        quote! {
            [#(#actions),*]
        }
    });

    let scanner = generate_scanner(grammar.scan_rules(), end_symbol, grammar.symbol_variants());
    let variants = grammar.symbol_variants().values().map(|(id, ty)| {
        quote! {
            #id(#ty)
        }
    });
    let mut intos: HashMap<String, (syn::Type, Vec<Ident>)> = HashMap::new();
    for (id, ty) in grammar.symbol_variants().values() {
        let type_name = syn::Type::to_token_stream(ty).to_string();
        intos
            .entry(type_name)
            .or_insert((ty.clone(), Vec::new()))
            .1
            .push(id.clone());
    }
    let intos = intos.iter().map(|(type_name, (ty, idents))| {
        let idents = idents.iter().map(|id| {
            quote! {
                Symbol::#id(value) => value
            }
        });
        quote! {
            impl Into<#ty> for Symbol {
                fn into(self) -> #ty {
                    match self {
                        #(#idents),*,
                        _ => panic!("Cannot cast {:?} to {}", self, #type_name),
                    }
                }
            }
        }
    });

    let parse_result_type =
        &grammar.symbol_variants()[&Symbol::Nonterminal(grammar.parse_rules()[0].lhs())].1;

    let end_id = end_symbol.id();

    TokenStream::from(quote! {
        use lr::*;

        #[derive(Debug)]
        pub enum Symbol {
            #(#variants),*
        }

        #(#intos)*

        #[derive(Debug)]
        pub enum TokenKind {
            Token(usize),
            Production(usize, Symbol),
            Invalid,
        }

        #[derive(Debug)]
        pub struct ScanToken {
            kind: TokenKind,
            offset: usize,
            length: usize,
            line: usize,
        }

        impl ScanToken {
            fn idx(&self) -> usize {
                match self.kind {
                    TokenKind::Token(u) => u,
                    TokenKind::Production(u, _) => u,
                    TokenKind::Invalid => {
                        panic!("Invalid input - no transition table index");
                    }
                }
            }

            fn is_invalid(&self) -> bool {
                if let TokenKind::Invalid = self.kind {
                    true
                } else {
                    false
                }
            }
        }

        #scanner

        pub struct Parser {
            table: [[Action; #num_symbols]; #num_states],
        }

        impl Parser {
            pub fn new() -> Self {
                Self { table: [#(#actions),*] }
            }
            pub fn to_html_debug(&self) -> &'static str {
                #output
            }
            pub fn parse(&self, input: &str) -> Result<#parse_result_type, ParseError> {
                let mut stack = vec![0];
                let mut values: Vec<Symbol> = Vec::new();
                let mut tokens = ScannedTokens::from(input);
                for token in tokens {
                    if token.is_invalid() {
                        return Err(ParseError::InvalidToken {
                            offset: token.offset,
                            line: token.line,
                        });
                    }
                    let idx = token.idx();
                    let mut state = *stack.last().unwrap();

                    while let Action::Reduce(id) = self.table[state][idx] {
                        let (goto, pops) = match id {
                            #(#reduce_productions)*
                            _ => panic!("{} is not a valid rule index.", id),
                        };
                        stack.truncate(stack.len() - pops);
                        state = *stack.last().unwrap();
                        if let Action::Goto(new_state) = self.table[state][goto] {
                            stack.push(new_state);
                            state = new_state
                        } else {
                            panic!("Reduce fail");
                        }
                    }
                    if let Action::Accept = self.table[state][idx] {
                        return values
                            .pop()
                            .map(Symbol::into)
                            .ok_or_else(|| ParseError::IncompleteInput);
                    }
                    if let Action::Shift(new_state) = self.table[state][idx] {
                        if let TokenKind::Production(_, symbol) = token.kind {
                            values.push(symbol);
                        }
                        state = new_state;
                        stack.push(new_state);
                    } else {
                        return Err(if idx == #end_id {
                            ParseError::IncompleteInput
                        } else {ParseError::UnexpectedToken {
                            offset: token.offset,
                            length: token.length,
                            line: token.line,
                        }});
                    }
                }
                Err(ParseError::IncompleteInput)
            }
        }
    })
}
