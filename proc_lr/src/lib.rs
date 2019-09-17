#![recursion_limit = "256"]
extern crate proc_macro;

mod input;

use proc_macro::TokenStream;
use quote::*;
use std::collections::{BTreeSet, HashMap, HashSet};
use syn::parse_macro_input;

use rules::*;
use input::*;

#[proc_macro]
pub fn grammar(tokens: TokenStream) -> TokenStream {
    let grammar = parse_macro_input!(tokens as Grammar);
    let input = "";
    let mut symbols = SymbolMap::new();
    let (scanner, parser) = input.split_at(
        input
            .find("\n\n")
            .expect("Separator line between scanner and parser not found"),
    );
    let parser = &parser[2..];
    for line in parser.lines() {
        let (nonterminal, _) = line.split_at(line.find("::=").expect("No `::=` in parser rule"));
        symbols.add_nonterminal(nonterminal.trim());
    }
    let scanner = scanner
        .lines()
        .map(|line| {
            let (input, terminal) =
                line.split_at(line.find("=>").expect("No `=>` in scanner production"));
            let terminal = symbols.get_symbol(terminal[2..].trim());
            let input = input.trim();
            let id = terminal.id();
            let len = input.len();
            quote! {
                if self.input.starts_with(#input) {
                    self.input = &self.input[#len..];
                    return Some(#id);
                }
            }
        })
        .collect::<Vec<_>>();
    let rules = Rules::new(
        parser
            .lines()
            .filter_map(|line| line.find("::=").map(|idx| line.split_at(idx)))
            .enumerate()
            .map(|(id, (lhs, rhs))| {
                Rule::new(
                    RuleId(id),
                    symbols.get_nonterminal(lhs.trim()),
                    rhs[3..]
                        .trim()
                        .split_whitespace()
                        .map(|symbol| symbols.get_symbol(symbol))
                        .collect::<Vec<_>>(),
                )
            })
            .collect::<Vec<_>>(),
    );
    let end_symbol = symbols.get_symbol("$").id();
    // let epsilon = symbols.get_symbol("ε").terminal();
    let start_symbol = symbols.get_nonterminal("S");
    let mut rev_symbols = HashMap::new();
    for (k, v) in symbols.symbols.iter() {
        rev_symbols.insert(v, k);
    }
    let mut idx = 0;
    let mut start_lookahead = BTreeSet::new();
    start_lookahead.insert(Terminal(end_symbol));
    // start_lookahead.insert(epsilon);
    // let epsilon = epsilon.0;
    let first_sets = first_sets(rules.rules.as_ref());
    let mut states = vec![rules.rules[idx]
        .lookahead(start_lookahead)
        .closure(&rules, &first_sets)];
    let mut state_ids = HashMap::new();
    let mut table = Vec::new();
    state_ids.insert(states[idx].rules.clone(), StateId::new(idx));
    let symbols = symbols.symbol_list;

    while idx < states.len() {
        let mut new_states = Vec::new();
        let reduce_rules = states[idx]
            .rules
            .iter()
            .filter(|rule| rule.rule.is_reducible())
            .map(|rule| (rule.rule.id, rule.lookahead.clone()))
            .collect::<Vec<_>>();
        let mut reduces = <HashMap<Terminal, Vec<RuleId>>>::new();
        for (id, lookahead) in reduce_rules.iter() {
            for terminal in lookahead.iter() {
                reduces.entry(*terminal).or_default().push(*id);
            }
        }
        table.push(
            states[idx]
                .explore(symbols.as_slice(), &rules, &first_sets)
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
                        /*
                        match &reduce_action {
                            Action::Accept => {
                                Action::Conflict(Conflict::ShiftReduce(id, RuleId(0)))
                            }
                            Action::Reduces(rules) => {
                                let shift_reduce_disjoint =
                                    rules.iter().all(|(_, lookahead)| {
                                        !lookahead.contains(&symbol.terminal())
                                    });
                                if shift_reduce_disjoint {
                                    Action::ShiftReduces(id, rules.clone())
                                } else {
                                    Action::Conflict(Conflict::ShiftReduce(id, rules[0].0))
                                }
                            }
                            Action::Conflict(_) => reduce_action.clone(),
                            _ => Action::Shift(id),
                        }
                        */
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
    let graph = "digraph states {\nnode[shape = record];\n".to_string()
        + &states
            .iter()
            .enumerate()
            .map(|(idx, state)| {
                format!("state{0}[label = \"State {0}:\\n", idx)
                    + &state
                        .rules
                        .iter()
                        .map(|rule| {
                            rev_symbols[&Symbol::Nonterminal(rule.rule.lhs)].to_string()
                                + " ⇒"
                                + &rule
                                    .rule
                                    .rhs
                                    .iter()
                                    .enumerate()
                                    .map(|(idx, symbol)| {
                                        let pre = if idx == rule.rule.seen {
                                            "•".to_string()
                                        } else {
                                            " ".to_string()
                                        };
                                        pre + rev_symbols[symbol]
                                    })
                                    .collect::<String>()
                                + if rule.rule.is_reducible() {
                                    "• ⦃"
                                } else {
                                    "  ⦃"
                                }
                                + &rule
                                    .lookahead
                                    .iter()
                                    .map(|terminal| rev_symbols[&Symbol::Terminal(*terminal)].clone())
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
        .arg(&graph)
        .stdout(std::process::Stdio::piped())
        .spawn()
        .expect("Cannot echo");
    let graph = std::process::Command::new("dot")
        .arg("-Tsvg")
        .stdin(echo.stdout.expect("Cannot access echo output"))
        .output()
        .expect("`dot` graph generator failed");

    let output = "<h1>Rules</h1>\n<ol start=\"0\">\n".to_string()
        + &rules
            .rules
            .iter()
            .map(|rule| {
                "<li>".to_string()
                    + rev_symbols[&Symbol::Nonterminal(rule.lhs)]
                    + " ⇒"
                    + &rule
                        .rhs
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
                            Action::Reduces(rules) => format!("<td>{}</td>\n", {
                                let reduces = rules
                                    .iter()
                                    .map(|(id, _)| format!("R {}", id.0))
                                    .collect::<Vec<_>>();
                                reduces.join(" / ")
                            }),
                            Action::ShiftReduces(StateId(id), rules) => {
                                format!("<td>S {} / {}</td>", id, {
                                    let reduces = rules
                                        .iter()
                                        .map(|(id, _)| format!("R {}", id.0))
                                        .collect::<Vec<_>>();
                                    reduces.join(" / ")
                                })
                            }
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
        + "</table>\n<br /><h1>Transition Graph</h1>"
        + &String::from_utf8(graph.stdout).expect("Command output not in UTF-8");

    let num_symbols = symbols.len();
    let num_states = states.len();

    let rules = rules
        .rules
        .into_iter()
        .map(|rule| (rule.lhs.0, rule.rhs.len()))
        .collect::<Vec<_>>();

    let actions = table.iter().map(|row| {
        let actions = row.iter().map(|action| match action {
            Action::Undefined => quote! { Action::Undefined },
            Action::Accept => quote! { Action::Accept },
            Action::Goto(StateId(id)) => quote! { Action::Goto(#id) },
            Action::Shift(StateId(id)) => quote! { Action::Shift(#id) },
            Action::Reduce(RuleId(id)) => {
                let (goto, pop) = rules[*id];
                quote! { Action::Reduce(#goto, #pop) }
            }
            Action::Reduces(reduce_rules) if reduce_rules.len() == 1 => {
                let rule = reduce_rules[0].0;
                let (goto, pop) = rules[rule.0];
                quote! { Action::Reduce(#goto, #pop) }
            }
            Action::Reduces(reduce_rules) => {
                let reduces = reduce_rules.iter().map(|(id, lookahead)| {
                    let (goto, pop) = rules[id.0];
                    let lookahead = lookahead.iter().map(|terminal| terminal.0);
                    quote! {
                        LookaheadReduce::new(#goto, #pop, vec![#(#lookahead),*])
                    }
                });
                quote! { Action::Reduces(vec![#(#reduces),*]) }
            }
            Action::ShiftReduces(StateId(id), reduce_rules) if reduce_rules.len() == 1 => {
                let rule = reduce_rules[0].0;
                let (goto, pop) = rules[rule.0];
                let lookahead = reduce_rules[0].1.iter().map(|terminal| terminal.0);
                quote! {
                    Action::ShiftReduce(
                        Box::new(ShiftReduce::new(#id, #goto, #pop, vec![#(#lookahead),*]))
                    )
                }
            }
            Action::ShiftReduces(StateId(id), reduce_rules) => {
                let rule = reduce_rules[0].0;
                let (goto, pop) = rules[rule.0];
                let lookahead = reduce_rules[0].1.iter().map(|terminal| terminal.0);
                // TODO: Handle multiple reduce
                quote! {
                    Action::ShiftReduce(
                        Box::new(ShiftReduce::new(#id, #goto, #pop, vec![#(#lookahead),*]))
                    )
                }
            }
            Action::Conflict(_) => {
                panic!("Cannot make a parser from conflicting rules: {:#?}", action)
            }
        });
        quote! {
            [#(#actions),*]
        }
    });

    TokenStream::from(quote! {
        pub struct Scanner {
        }

        pub struct ScannedTokens<'a> {
            input: &'a str,
        }

        impl<'a> Iterator for ScannedTokens<'a> {
            type Item = usize;
            fn next(&mut self) -> Option<Self::Item> {
                if self.input.is_empty() {
                    return Some(#end_symbol);
                }
                #(#scanner)*
                return None;
            }
        }

        impl Scanner {
            pub fn new() -> Self {
                Self {}
            }
            pub fn scan<'a>(&self, input: &'a str) -> ScannedTokens<'a> {
                ScannedTokens { input }
            }
        }

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
            Reduce(usize, usize),
            // Reduces(Vec<LookaheadReduce>),
            // ShiftReduce(Box<ShiftReduce>),
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
            pub fn parse(&self, tokens: ScannedTokens<'_>) -> bool {
                let mut stack = vec![0];
                for token in tokens {
                    let mut state = *stack.last().unwrap();
                    println!("Token {} => {:?}", token, self.table[state][token]);

                    while let Action::Reduce(goto, pops) = self.table[state][token] {
                        println!("{:?}", self.table[state][token]);
                        stack.truncate(stack.len() - pops);
                        state = *stack.last().unwrap();
                        println!("Token {} => {:?}", goto, self.table[state][goto]);
                        if let Action::Goto(new_state) = self.table[state][goto] {
                            stack.push(new_state);
                            state = new_state
                        } else {
                            panic!("Reduce fail");
                        }
                    }
                    if let Action::Accept = self.table[state][token] {
                        println!("Parse success");
                        return true;
                    }
                    if let Action::Shift(new_state) = self.table[state][token] {
                        state = new_state;
                        stack.push(new_state);
                    } else {
                        println!("Parse fail: {:?}", self.table[state][token]);
                        return false;
                    }
                }
                return false;
            }
        }
    })
}

mod rules {
    use std::collections::{BTreeSet, HashMap, HashSet};
    #[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Hash, Eq)]
    pub(crate) struct Nonterminal(pub(crate) usize);
    #[derive(Debug, Copy, PartialOrd, Ord, Clone, PartialEq, Hash, Eq)]
    pub(crate) struct Terminal(pub(crate) usize);
    #[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Hash, Eq)]
    pub(crate) enum Symbol {
        Nonterminal(Nonterminal),
        Terminal(Terminal),
    }
    impl Symbol {
        pub(crate) fn id(&self) -> usize {
            *match self {
                Symbol::Nonterminal(Nonterminal(id)) => id,
                Symbol::Terminal(Terminal(id)) => id,
            }
        }
        fn nonterminal(&self) -> Nonterminal {
            *match self {
                Symbol::Nonterminal(nonterminal) => nonterminal,
                _ => panic!(),
            }
        }
        pub(crate) fn terminal(&self) -> Terminal {
            *match self {
                Symbol::Terminal(terminal) => terminal,
                _ => panic!(),
            }
        }
        pub(crate) fn is_terminal(&self) -> bool {
            if let Symbol::Nonterminal(_) = self {
                false
            } else {
                true
            }
        }
    }
    pub(crate) struct SymbolMap {
        pub(crate) symbols: HashMap<String, Symbol>,
        pub(crate) symbol_list: Vec<Symbol>,
    }
    impl SymbolMap {
        pub(crate) fn new() -> Self {
            Self {
                symbols: HashMap::new(),
                symbol_list: Vec::new(),
            }
        }
        pub(crate) fn get_symbol(&mut self, name: &str) -> Symbol {
            if self.symbols.contains_key(name) {
                self.symbols[name]
            } else {
                let symbol = Symbol::Terminal(Terminal(self.symbol_list.len()));
                self.symbol_list.push(symbol);
                self.symbols.insert(name.to_string(), symbol);
                symbol
            }
        }
        pub(crate) fn add_nonterminal(&mut self, name: &str) -> bool {
            if !self.symbols.contains_key(name) {
                let symbol = Symbol::Nonterminal(Nonterminal(self.symbol_list.len()));
                self.symbol_list.push(symbol);
                self.symbols.insert(name.to_string(), symbol);
                return true;
            }
            false
        }
        pub(crate) fn add_terminal(&mut self, name: &str) -> bool {
            if !self.symbols.contains_key(name) {
                let symbol = Symbol::Terminal(Terminal(self.symbol_list.len()));
                self.symbol_list.push(symbol);
                self.symbols.insert(name.to_string(), symbol);
                return true;
            }
            false
        }
        pub(crate) fn get_nonterminal(&mut self, name: &str) -> Nonterminal {
            self.symbols[name].nonterminal()
        }
        pub(crate) fn get_terminal(&mut self, name: &str) -> Terminal {
            self.symbols[name].terminal()
        }
    }
    #[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
    pub(crate) struct RuleId(pub(crate) usize);

    #[derive(Debug, Eq, Clone)]
    pub(crate) struct Rule {
        pub(crate) id: RuleId,
        pub(crate) lhs: Nonterminal,
        pub(crate) rhs: Vec<Symbol>,
        pub(crate) seen: usize,
    }
    impl PartialEq for Rule {
        fn eq(&self, other: &Self) -> bool {
            self.id == other.id
        }
    }
    impl std::hash::Hash for Rule {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.id.hash(state);
        }
    }
    #[derive(Debug)]
    pub(crate) struct Rules {
        pub(crate) rules: Vec<Rule>,
    }
    impl Rules {
        pub(crate) fn new(rules: Vec<Rule>) -> Self {
            Self { rules }
        }
    }
    #[derive(Clone, Debug, PartialEq, Eq)]
    pub(crate) struct LookaheadRule {
        pub(crate) rule: Rule,
        pub(crate) lookahead: BTreeSet<Terminal>,
    }

    impl LookaheadRule {
        fn shift(&self) -> Self {
            let mut shifted = self.clone();
            shifted.rule.seen += 1;
            shifted
        }
    }

    impl std::hash::Hash for LookaheadRule {
        fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
            self.rule.hash(state);
            for terminal in self.lookahead.iter() {
                terminal.hash(state);
            }
        }
    }
    #[derive(Debug)]
    struct DependencyGraphNode {
        degree: usize,
        neighbors: HashSet<Nonterminal>,
    }
    impl Default for DependencyGraphNode {
        fn default() -> Self {
            Self {
                degree: 0,
                neighbors: HashSet::new(),
            }
        }
    }
    pub(crate) type FirstSets = HashMap<Nonterminal, BTreeSet<Terminal>>;
    pub(crate) fn first_sets(rules: &[Rule]) -> FirstSets {
        let mut dependency_graph: HashMap<Nonterminal, DependencyGraphNode> = HashMap::new();
        let mut first_sets: FirstSets = HashMap::new();
        for rule in rules.iter() {
            match rule.first_rhs() {
                None => {}
                Some(Symbol::Nonterminal(nonterminal)) if nonterminal == rule.lhs => {}
                Some(Symbol::Nonterminal(nonterminal)) => {
                    if dependency_graph
                        .entry(nonterminal)
                        .or_default()
                        .neighbors
                        .insert(rule.lhs)
                    {
                        dependency_graph.entry(rule.lhs).or_default().degree += 1;
                    }
                }
                Some(Symbol::Terminal(terminal)) => {
                    first_sets.entry(rule.lhs).or_default().insert(terminal);
                }
            }
        }
        while !dependency_graph.is_empty() {
            let (symbol, _) = dependency_graph
                .iter()
                .find(|(_, node)| node.degree == 0)
                .expect(&format!(
                    "Rule dependency loop detected: {:?}",
                    dependency_graph
                ));
            let symbol = *symbol;
            let node = dependency_graph
                .remove(&symbol)
                .expect("Symbol missing from dependency graph");
            let terminals = first_sets[&symbol].iter().cloned().collect::<Vec<_>>();
            for neighbor in node.neighbors.iter() {
                dependency_graph
                    .get_mut(&neighbor)
                    .expect("Neighbor missing from dependency graph")
                    .degree -= 1;
                first_sets
                    .entry(*neighbor)
                    .or_default()
                    .extend(terminals.iter().cloned());
            }
        }
        first_sets
    }
    pub(crate) trait Closure {
        fn init(&self) -> Vec<LookaheadRule>;
        fn closure(&self, all_rules: &Rules, first_sets: &FirstSets) -> State {
            let mut lookahead_rules = self.init();
            let mut rules = Vec::new();
            let mut explored_symbols = HashSet::new();
            let mut lookaheads = <HashMap<Nonterminal, BTreeSet<Terminal>>>::new();
            for rule in lookahead_rules.iter() {
                if let Some(Symbol::Nonterminal(first_rhs)) = rule.rule.first_rhs() {
                    lookaheads.entry(first_rhs).or_default().extend(
                        rule.rule
                            .follow(first_sets)
                            .unwrap_or(rule.lookahead.clone())
                            .into_iter(),
                    );
                    if explored_symbols.insert(first_rhs) {
                        rules.extend(
                            all_rules
                                .rules
                                .iter()
                                .filter(|rule| rule.lhs == first_rhs)
                                .cloned(),
                        );
                    }
                }
            }
            let mut idx = 0;
            while idx < rules.len() {
                if let Some(Symbol::Nonterminal(first_rhs)) = rules[idx].first_rhs() {
                    match rules[idx].follow(first_sets) {
                        Some(lookahead) => lookaheads
                            .entry(first_rhs)
                            .or_default()
                            .extend(lookahead.into_iter()),
                        None => {
                            // TODO: handle fixed point iteration
                            let lookahead: Vec<Terminal> = lookaheads
                                .entry(rules[idx].lhs)
                                .or_default()
                                .iter()
                                .cloned()
                                .collect();
                            lookaheads
                                .entry(first_rhs)
                                .or_default()
                                .extend(lookahead.into_iter());
                        }
                    }
                    if explored_symbols.insert(first_rhs) {
                        rules.extend(
                            all_rules
                                .rules
                                .iter()
                                .filter(|rule| rule.lhs == first_rhs)
                                .cloned(),
                        );
                    }
                }
                idx += 1;
            }
            lookahead_rules.extend(rules.into_iter().map(|rule| {
                let lookahead = lookaheads[&rule.lhs].clone();
                LookaheadRule { rule, lookahead }
            }));
            State::new(lookahead_rules)
        }
    }
    impl Rule {
        pub(crate) fn new(id: RuleId, lhs: Nonterminal, rhs: Vec<Symbol>) -> Self {
            Self {
                id,
                lhs,
                rhs,
                seen: 0,
            }
        }
        fn first_rhs(&self) -> Option<Symbol> {
            if self.seen == self.rhs.len() {
                None
            } else {
                Some(self.rhs[self.seen])
            }
        }
        // NOTE: Unsafe if not called after `first_rhs`
        fn shift(&self) -> Self {
            let mut shifted = self.clone();
            shifted.seen += 1;
            shifted
        }
        pub(crate) fn is_reducible(&self) -> bool {
            self.seen == self.rhs.len()
        }
        pub(crate) fn lookahead(&self, lookahead: BTreeSet<Terminal>) -> LookaheadRule {
            LookaheadRule {
                rule: self.clone(),
                lookahead,
            }
        }
        fn follow(&self, first_sets: &FirstSets) -> Option<BTreeSet<Terminal>> {
            if self.seen + 1 < self.rhs.len() {
                Some(match self.rhs[self.seen + 1] {
                    Symbol::Nonterminal(nonterminal) => first_sets[&nonterminal].clone(),
                    Symbol::Terminal(terminal) => {
                        let mut first = BTreeSet::new();
                        first.insert(terminal.clone());
                        first
                    }
                })
            } else {
                None
            }
        }
    }
    impl Closure for LookaheadRule {
        fn init(&self) -> Vec<LookaheadRule> {
            vec![self.clone()]
        }
    }
    impl<'a> Closure for [LookaheadRule] {
        fn init(&self) -> Vec<LookaheadRule> {
            self.to_vec()
        }
    }

    #[derive(Hash, Copy, Clone, Debug)]
    pub(crate) struct StateId(pub(crate) usize);

    impl StateId {
        pub(crate) fn new(id: usize) -> StateId {
            StateId(id)
        }
    }

    #[derive(PartialEq, Eq, Clone)]
    pub(crate) struct State {
        pub(crate) rules: Vec<LookaheadRule>,
    }

    impl State {
        fn new(rules: Vec<LookaheadRule>) -> Self {
            Self { rules }
        }
        pub(crate) fn is_empty(&self) -> bool {
            self.rules.is_empty()
        }
        pub(crate) fn explore<'a>(
            &'a self,
            symbols: &'a [Symbol],
            all_rules: &'a Rules,
            first_sets: &'a FirstSets,
        ) -> impl Iterator<Item = (Symbol, Self)> + 'a {
            symbols.iter().map(move |symbol| {
                (
                    *symbol,
                    self.rules
                        .iter()
                        .filter_map(|rule| {
                            rule.rule.first_rhs().and_then(|first| {
                                if first == *symbol {
                                    Some(rule)
                                } else {
                                    None
                                }
                            })
                        })
                        .map(|rule| rule.shift())
                        .collect::<Vec<_>>()
                        .as_slice()
                        .closure(all_rules, first_sets),
                )
            })
        }
    }
    #[derive(Clone, Debug)]
    pub(crate) enum Conflict {
        ShiftReduce(StateId, RuleId),
        ReduceReduce(RuleId, RuleId),
    }
    #[derive(Clone, Debug)]
    pub(crate) enum Action {
        Undefined,
        Shift(StateId),
        ShiftReduces(StateId, Vec<(RuleId, BTreeSet<Terminal>)>),
        Reduces(Vec<(RuleId, BTreeSet<Terminal>)>),
        Reduce(RuleId),
        Goto(StateId),
        Accept,
        Conflict(Conflict),
    }
}
