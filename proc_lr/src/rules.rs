use crate::common_types::*;
use std::collections::{BTreeSet, HashMap, HashSet};

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub(crate) struct LookaheadRule {
    pub(crate) id: RuleId,
    pub(crate) lhs: Nonterminal,
    pub(crate) rhs: Vec<Symbol>,
    pub(crate) seen: usize,
    pub(crate) lookahead: BTreeSet<Terminal>,
}

impl From<&ParseRule> for LookaheadRule {
    fn from(rule: &ParseRule) -> Self {
        Self {
            id: rule.id(),
            lhs: rule.lhs(),
            rhs: rule.rhs().to_vec(),
            seen: 0,
            lookahead: BTreeSet::new(),
        }
    }
}

impl LookaheadRule {
    pub(crate) fn new(rule: &ParseRule, lookahead: BTreeSet<Terminal>) -> Self {
        Self {
            id: rule.id(),
            lhs: rule.lhs(),
            rhs: rule.rhs().to_vec(),
            seen: 0,
            lookahead,
        }
    }

    pub(crate) fn id(&self) -> RuleId {
        self.id
    }

    pub(crate) fn lhs(&self) -> Nonterminal {
        self.lhs
    }

    pub(crate) fn rhs(&self) -> &[Symbol] {
        self.rhs.as_ref()
    }

    fn shift(&self) -> Self {
        let mut shifted = self.clone();
        shifted.seen += 1;
        shifted
    }

    fn first_rhs(&self) -> Option<Symbol> {
        self.rhs.get(self.seen).cloned()
    }

    pub(crate) fn is_reducible(&self) -> bool {
        self.seen == self.rhs.len()
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

pub(crate) fn first_sets(rules: &[ParseRule]) -> FirstSets {
    let mut dependency_graph: HashMap<Nonterminal, DependencyGraphNode> = HashMap::new();
    let mut first_sets: FirstSets = HashMap::new();
    for rule in rules.iter() {
        let lhs = rule.lhs();
        match rule.first_rhs() {
            None => {}
            Some(Symbol::Nonterminal(nonterminal)) if nonterminal == lhs => {}
            Some(Symbol::Nonterminal(nonterminal)) => {
                if dependency_graph
                    .entry(nonterminal)
                    .or_default()
                    .neighbors
                    .insert(lhs)
                {
                    dependency_graph.entry(lhs).or_default().degree += 1;
                }
            }
            Some(Symbol::Terminal(terminal)) => {
                first_sets.entry(lhs).or_default().insert(terminal);
            }
        }
    }

    while !dependency_graph.is_empty() {
        let (symbol, _) = dependency_graph
            .iter()
            .find(|(_, node)| node.degree == 0)
            .expect("Rule dependency loop detected");
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

    fn closure(&self, all_rules: &[ParseRule], first_sets: &FirstSets) -> State {
        let mut rules = self.init();
        let num_initial_rules = rules.len();
        let mut explored_symbols = HashSet::new();
        let mut lookaheads = <HashMap<Nonterminal, BTreeSet<Terminal>>>::new();
        for rule_idx in 0..num_initial_rules {
            if let Some(Symbol::Nonterminal(first_rhs)) = rules[rule_idx].first_rhs() {
                lookaheads.entry(first_rhs).or_default().extend(
                    rules[rule_idx]
                        .follow(first_sets)
                        .unwrap_or_else(|| rules[rule_idx].lookahead.clone())
                        .into_iter(),
                );
                if explored_symbols.insert(first_rhs) {
                    rules.extend(
                        all_rules
                            .iter()
                            .filter(|rule| rule.lhs() == first_rhs)
                            .map(From::from),
                    );
                }
            }
        }
        let mut idx = num_initial_rules;
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
                            .entry(rules[idx].lhs())
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
                            .iter()
                            .filter(|rule| rule.lhs() == first_rhs)
                            .map(From::from),
                    );
                }
            }
            idx += 1;
        }
        for rule in rules[num_initial_rules..].iter_mut() {
            rule.lookahead.extend(lookaheads[&rule.lhs()].iter());
        }
        State::new(rules)
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

#[derive(Clone, PartialEq, Eq, Debug)]
pub(crate) struct State {
    pub(crate) rules: Vec<LookaheadRule>,
}

impl State {
    fn new(rules: Vec<LookaheadRule>) -> Self {
        Self { rules }
    }

    fn is_empty(&self) -> bool {
        self.rules.is_empty()
    }

    pub(crate) fn explore<'a>(
        &'a self,
        symbols: &'a [Symbol],
        all_rules: &'a [ParseRule],
        first_sets: &'a FirstSets,
    ) -> impl Iterator<Item = Self> + 'a {
        symbols.iter().map(move |symbol| {
            self.rules
                .iter()
                .filter_map(|rule| {
                    rule.first_rhs()
                        .filter(|rhs| rhs == symbol)
                        .map(|_| rule.shift())
                })
                .collect::<Vec<_>>()
                .as_slice()
                .closure(all_rules, first_sets)
        })
    }

    fn explore_transitions<'a>(
        &'a self,
        symbols: &'a [Symbol],
        all_rules: &'a [ParseRule],
        first_sets: &'a FirstSets,
    ) -> impl Iterator<Item = (Self, Symbol)> + 'a {
        symbols.iter().map(move |symbol| {
            let state = self
                .rules
                .iter()
                .filter_map(|rule| {
                    rule.first_rhs()
                        .filter(|rhs| rhs == symbol)
                        .map(|_| rule.shift())
                })
                .collect::<Vec<_>>()
                .as_slice()
                .closure(all_rules, first_sets);
            (state, *symbol)
        })
    }

    pub(crate) fn signature(&self) -> Vec<(RuleId, usize)> {
        self.rules.iter().map(|rule| (rule.id, rule.seen)).collect()
    }

    pub(crate) fn merge(&mut self, other: &Self) -> bool {
        let mut merged = false;
        for (ref mut rule, ref other) in self.rules.iter_mut().zip(other.rules.iter()) {
            for symbol in other.lookahead.iter() {
                if !rule.lookahead.contains(symbol) {
                    rule.lookahead.insert(*symbol);
                    merged = true;
                }
            }
        }
        merged
    }

    fn get_reduce_rules(&self) -> HashMap<Terminal, Vec<RuleId>> {
        let reduce_rules = self
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
        reduces
    }

    pub(crate) fn transitions(
        &self,
        symbols: &[Symbol],
        parse_rules: &[ParseRule],
        first_sets: &FirstSets,
        state_ids: &HashMap<Vec<(RuleId, usize)>, StateId>,
    ) -> Vec<Action> {
        let reduces = self.get_reduce_rules();

        self.explore_transitions(symbols, parse_rules, first_sets)
            .map(|(state, symbol)| {
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
                    let id = state_ids[&state.signature()];
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
            .collect::<Vec<_>>()
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
    Reduce(RuleId),
    Goto(StateId),
    Accept,
    Conflict(Conflict),
}
