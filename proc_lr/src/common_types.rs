use std::collections::HashMap;
use syn::ExprClosure;

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Hash, Eq)]
pub(crate) struct Nonterminal(usize);

impl Nonterminal {
    pub(crate) fn id(self) -> usize {
        self.0
    }
}

#[derive(Debug, Copy, PartialOrd, Ord, Clone, PartialEq, Hash, Eq)]
pub(crate) struct Terminal(usize);

impl Terminal {
    pub(crate) fn id(self) -> usize {
        self.0
    }
}

#[derive(Debug, Copy, Clone, PartialOrd, Ord, PartialEq, Hash, Eq)]
pub(crate) enum Symbol {
    Nonterminal(Nonterminal),
    Terminal(Terminal),
}

impl Symbol {
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

    pub(crate) fn list(&self) -> &[Symbol] {
        self.symbol_list.as_ref()
    }
}

impl std::ops::Index<&str> for SymbolMap {
    type Output = Symbol;
    fn index(&self, name: &str) -> &Self::Output {
        &self.symbols[name]
    }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
pub(crate) struct RuleId(pub(crate) usize);

pub(crate) enum ScanInput {
    Char(char),
    Str(String),
    Regex(String),
}

pub(crate) struct ScanRule {
    token: ScanInput,
    terminal: Option<Terminal>,
    production: Option<ExprClosure>,
}

impl ScanRule {
    pub(crate) fn new(
        token: ScanInput,
        terminal: Option<Terminal>,
        production: Option<ExprClosure>,
    ) -> Self {
        Self {
            token,
            terminal,
            production,
        }
    }

    pub(crate) fn token(&self) -> &ScanInput {
        &self.token
    }

    pub(crate) fn terminal(&self) -> Option<Terminal> {
        self.terminal
    }

    pub(crate) fn production(&self) -> &Option<ExprClosure> {
        &self.production
    }
}

pub(crate) struct ParseRule {
    id: RuleId,
    lhs: Nonterminal,
    rhs: Vec<Symbol>,
    production: Option<ExprClosure>,
}

impl ParseRule {
    pub(crate) fn new(
        id: RuleId,
        lhs: Nonterminal,
        rhs: Vec<Symbol>,
        production: Option<ExprClosure>,
    ) -> Self {
        Self {
            id,
            lhs,
            rhs,
            production,
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

    pub(crate) fn first_rhs(&self) -> Option<Symbol> {
        self.rhs.first().cloned()
    }

    pub(crate) fn production(&self) -> &Option<ExprClosure> {
        &self.production
    }
}
