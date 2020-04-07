mod graphviz;

use super::*;
use graphviz::generate_transition_graph_svg;

pub(super) fn generate_debug_output(
    grammar: &Grammar,
    table: &[Vec<Action>],
    symbols: &[Symbol],
    states: &[State],
    rev_symbols: &HashMap<&Symbol, &str>,
) -> String {
    let transition_graph_svg = generate_transition_graph_svg(table, symbols, states, rev_symbols);
    let rules_list = generate_rules_list(grammar, rev_symbols);
    let table_header = symbols
        .iter()
        .map(|symbol| format!("<th>{}</th>\n", rev_symbols[symbol]))
        .collect::<String>();

    let table_rows = table
        .iter()
        .enumerate()
        .map(|(idx, row)| generate_table_row(idx, row))
        .collect::<String>();

    format!(
        r#"<h1>Rules</h1>
<ol start="0">
{}
</ol>
<h1>Transition Table</h1>
<table style="width: 100%">
    <tr><th></th>{}</tr>
    {}
</table>
<br/>
<h1>Transition Graph</h1>
{}"#,
        rules_list, table_header, table_rows, transition_graph_svg
    )
}

fn generate_rules_list(grammar: &Grammar, rev_symbols: &HashMap<&Symbol, &str>) -> String {
    grammar
        .parse_rules()
        .iter()
        .map(|rule| {
            format!(
                "<li>{} ⇒{}</li>\n",
                rev_symbols[&Symbol::Nonterminal(rule.lhs())],
                rule.rhs()
                    .iter()
                    .map(|symbol| format!(" {}", rev_symbols[symbol]))
                    .collect::<String>()
            )
        })
        .collect::<String>()
}

fn generate_table_row(idx: usize, row: &[Action]) -> String {
    let row = row
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
        .collect::<String>();

    format!("<tr><th>State {}</th>{}</tr>\n", idx, row)
}
