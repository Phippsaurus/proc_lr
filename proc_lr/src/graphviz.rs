use super::*;
use std::process::{Command, Stdio};

pub(super) fn generate_transition_graph_svg(
    table: &[Vec<Action>],
    symbols: &[Symbol],
    states: &[State],
    rev_symbols: &HashMap<&Symbol, &str>,
) -> String {
    let digraph = generate_digraph(table, symbols, states, rev_symbols);
    let echo = Command::new("echo")
        .arg(&digraph)
        .stdout(Stdio::piped())
        .spawn()
        .expect("Cannot echo");
    let graph = Command::new("dot")
        .arg("-Tsvg")
        .stdin(echo.stdout.expect("Cannot access echo output"))
        .output()
        .expect("`dot` graph generator failed");
    String::from_utf8(graph.stdout).expect("Command output not in UTF-8")
}

trait Escape {
    fn escape(&self) -> String;
}

impl Escape for str {
    fn escape(&self) -> String {
        self.replace('"', "\\\"")
            .replace('[', "\\[")
            .replace(']', "\\]")
            .replace('{', "\\{")
            .replace('}', "\\}")
    }
}

fn generate_digraph(
    table: &[Vec<Action>],
    symbols: &[Symbol],
    states: &[State],
    rev_symbols: &HashMap<&Symbol, &str>,
) -> String {
    let nodes = generate_nodes(states, rev_symbols);
    let edges = generate_edges(table, symbols, rev_symbols);
    format!(
        "digraph states {{\nnode[shape = record];\n{}{}}}",
        &nodes, &edges
    )
}

fn generate_nodes(states: &[State], rev_symbols: &HashMap<&Symbol, &str>) -> String {
    states
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
                                .map(|terminal| rev_symbols[&Symbol::Terminal(*terminal)].escape())
                                .collect::<String>()
                            + "⦄\\n"
                    })
                    .collect::<String>()
                + "\"];\n"
        })
        .collect::<String>()
}

fn generate_edges(
    table: &[Vec<Action>],
    symbols: &[Symbol],
    rev_symbols: &HashMap<&Symbol, &str>,
) -> String {
    table
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
}
