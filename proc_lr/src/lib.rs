#![recursion_limit = "256"]
extern crate proc_macro;

mod common_types;
mod input;
mod output;
mod rules;
mod dot_string_escape;

use proc_macro::TokenStream;
use proc_macro2::Ident;
use quote::ToTokens;
use quote::*;
use std::collections::{BTreeSet, HashMap};
use syn::parse_macro_input;

use common_types::*;
use input::*;
use output::*;
use rules::*;
use dot_string_escape::Escape;

#[proc_macro]
pub fn grammar(tokens: TokenStream) -> TokenStream {
    let grammar = parse_macro_input!(tokens as Grammar);
    let symbols = grammar.symbols();
    let end_symbol = symbols["$"].terminal();
    let mut rev_symbols = HashMap::new();
    for (k, v) in symbols.symbols.iter() {
        rev_symbols.insert(v, k);
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
            .map(|rule| (rule.id(), rule.lookahead.clone()))
            .collect::<Vec<_>>();
        let mut reduces = <HashMap<Terminal, Vec<RuleId>>>::new();
        for (id, lookahead) in reduce_rules.iter() {
            for terminal in lookahead.iter() {
                reduces.entry(*terminal).or_default().push(*id);
            }
        }
        table.push(
            states[idx]
                .explore(symbols, grammar.parse_rules(), &first_sets)
                .map(|(symbol, state)| {
                    if state.is_empty() {
                        if symbol.is_terminal() && reduces.contains_key(&symbol.terminal()) {
                            let rules = &reduces[&symbol.terminal()];
                            if rules.len() == 1 {
                                if rules[0].0 == 0 {
                                    Action::Accept
                                } else {
                                    Action::Reduce(rules[0])
                                }
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
    let dot = "digraph states {\nnode[shape = record];\n".to_string()
        + &states
            .iter()
            .enumerate()
            .map(|(idx, state)| {
                format!("state{0}[label = \"State {0}:\\n", idx)
                    + &state
                        .rules
                        .iter()
                        .map(|rule| {
                            rev_symbols[&Symbol::Nonterminal(rule.lhs())].escape()
                                + " ⇒"
                                + &rule
                                    .rhs()
                                    .iter()
                                    .enumerate()
                                    .map(|(idx, symbol)| {
                                        let pre = if idx == rule.seen {
                                            "•".to_string()
                                        } else {
                                            " ".to_string()
                                        };
                                        pre + &rev_symbols[symbol].escape()
                                    })
                                    .collect::<String>()
                                + if rule.is_reducible() {
                                    "• ⦃"
                                } else {
                                    "  ⦃"
                                }
                                + &rule
                                    .lookahead
                                    .iter()
                                    .map(|terminal| {
                                        rev_symbols[&Symbol::Terminal(*terminal)].escape()
                                    })
                                    .collect::<String>()
                                + "⦄\\n"
                        })
                        .collect::<String>()
                    + "\"];\n"
            })
            .collect::<String>()
        + &table
            .iter()
            .enumerate()
            .flat_map(|(id, actions)| {
                actions
                    .iter()
                    .zip(symbols.iter())
                    .map(move |(action, symbol)| (id, action, symbol))
            })
            .map(|(state, action, symbol)| match action {
                Action::Goto(StateId(id)) => format!(
                    "state{} -> state{} [label = \"{}\"];\n",
                    state, id, rev_symbols[symbol]
                ),
                Action::Shift(StateId(id)) => format!(
                    "state{} -> state{} [label = \"{}\"];\n",
                    state, id, rev_symbols[symbol]
                ),
                _ => "".to_string(),
            })
            .collect::<String>()
        + "}";

    let echo = std::process::Command::new("echo")
        .arg(&dot)
        .stdout(std::process::Stdio::piped())
        .spawn()
        .expect("Cannot echo");
    let graph = std::process::Command::new("dot")
        .arg("-Tsvg")
        .stdin(echo.stdout.expect("Cannot access echo output"))
        .output()
        .expect("`dot` graph generator failed");

    let output = "<h1>Rules</h1>\n<ol start=\"0\">\n".to_string()
        + &grammar
            .parse_rules()
            .iter()
            .map(|rule| {
                "<li>".to_string()
                    + rev_symbols[&Symbol::Nonterminal(rule.lhs())]
                    + " ⇒"
                    + &rule
                        .rhs()
                        .iter()
                        .map(|symbol| " ".to_string() + &rev_symbols[symbol])
                        .collect::<String>()
                    + "</li>\n"
            })
            .collect::<String>()
        + "</ol>\n<h1>Transition Table</h1>\n<table style=\"width: 100%\">\n<tr><th></th>"
        + &symbols
            .iter()
            .map(|symbol| "<th>".to_string() + rev_symbols[symbol] + "</th>\n")
            .collect::<String>()
        + "</tr>"
        + &table
            .iter()
            .enumerate()
            .map(|(idx, row)| {
                format!("<tr><th>State {}</th>", idx)
                    + &row
                        .iter()
                        .map(|action| match action {
                            Action::Accept => "<td>A</td>".to_string(),
                            Action::Undefined => "<td>-</td>\n".to_string(),
                            Action::Goto(StateId(id)) => format!("<td>G {}</td>\n", id),
                            Action::Shift(StateId(id)) => format!("<td>S {}</td>\n", id),
                            Action::Reduce(RuleId(id)) => format!("<td>R {}</td>\n", id),
                            Action::Conflict(conflict) => match conflict {
                                Conflict::ShiftReduce(StateId(state), RuleId(rule)) => {
                                    format!("<td>S{}/R{}</td>", state, rule)
                                }
                                Conflict::ReduceReduce(RuleId(rule), RuleId(other)) => {
                                    format!("<td>R{}/R{}</td>", rule, other)
                                }
                            },
                        })
                        .collect::<String>()
                    + "</tr>\n"
            })
            .collect::<String>()
        + "</table>\n<br /><h1>Transition Graph</h1>\n<!--\n"
        + &dot + "\n-->"
        + &String::from_utf8(graph.stdout).expect("Command output not in UTF-8");

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
                            // args[#arg_idx].into()
                        }
                    });
                    quote! {
                        #id => {
                            let new_value = {
                                let mut args = values.drain(values.len() - #arg_count..);
                                (#production)(#(#args),*)
                            };
                            // let args = values.split_off(values.len() - #arg_count);
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

    TokenStream::from(quote! {
        #[derive(Debug)]
        pub enum Symbol {
            #(#variants),*
        }

        #(#intos)*

        #[derive(Debug)]
        pub enum ScanToken {
            Token(usize),
            Production(usize, Symbol),
        }

        impl ScanToken {
            fn idx(&self) -> usize {
                *match self {
                    ScanToken::Token(u) => u,
                    ScanToken::Production(u, _) => u,
                }
            }
        }

        #scanner

        #[derive(Debug)]
        pub struct ShiftReduce {
            shift_id: usize,
            goto: usize,
            pop: usize,
            lookahead: Vec<usize>,
        }

        impl ShiftReduce {
            pub fn new(shift_id: usize, goto: usize, pop: usize, lookahead: Vec<usize>) -> Self {
                Self { shift_id, goto, pop, lookahead }
            }
        }

        #[derive(Debug)]
        pub struct LookaheadReduce {
            goto: usize,
            pop: usize,
            lookahead: Vec<usize>,
        }

        impl LookaheadReduce {
            pub fn new(goto: usize, pop: usize, lookahead: Vec<usize>) -> Self {
                Self { goto, pop, lookahead }
            }
        }

        #[derive(Debug)]
        pub enum Action {
            Undefined,
            Shift(usize),
            Reduce(usize),
            Goto(usize),
            Accept,
        }

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
            pub fn parse(&self, input: &str) -> Option<#parse_result_type> {
                let mut stack = vec![0];
                let mut values: Vec<Symbol> = Vec::new();
                let mut tokens = ScannedTokens { input };
                for token in tokens {
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
                        return values.pop().map(|symbol| symbol.into());
                    }
                    if let Action::Shift(new_state) = self.table[state][idx] {
                        if let ScanToken::Production(_, symbol) = token {
                            values.push(symbol);
                        }
                        state = new_state;
                        stack.push(new_state);
                    } else {
                        return None;
                    }
                }
                return None;
            }
        }
    })
}
