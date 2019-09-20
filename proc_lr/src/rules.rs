use crate::common_types::*;
use std::collections::{BTreeSet, HashMap, HashSet};

#[derive(Clone, Debug, Hash, PartialEq, Eq)]
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
        self.rhs.get(self.seen).map(|symbol| *symbol)
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
                        .unwrap_or(rules[rule_idx].lookahead.clone())
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
        all_rules: &'a [ParseRule],
        first_sets: &'a FirstSets,
    ) -> impl Iterator<Item = (Symbol, Self)> + 'a {
        symbols.iter().map(move |symbol| {
            (
                *symbol,
                self.rules
                    .iter()
                    .filter_map(|rule| {
                        rule.first_rhs().and_then(
                            |first| {
                                if first == *symbol {
                                    Some(rule)
                                } else {
                                    None
                                }
                            },
                        )
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
