use lr::grammar;
use std::collections::HashMap;

#[derive(Debug)]
pub enum JsonValue {
    Number(f64),
    Bool(bool),
    Str(String),
    Null,
    List(Vec<JsonValue>),
    Object(HashMap<String, JsonValue>),
}

grammar! {
    S: JsonValue;
    A: JsonValue;
    O: JsonValue;
    St: String;
    Str: JsonValue;
    V: JsonValue;
    ES: Vec<JsonValue>;
    L: JsonValue;
    KV: (String, JsonValue);
    KVS: HashMap<String, JsonValue>;

    /r"[\n\s]+" => _;
    "true" => A |_: &str| JsonValue::Bool(true);
    "false" => A |_: &str| JsonValue::Bool(false);
    "null" => A |_: &str| JsonValue::Null;
    /r"-?\d+(\.\d+)?" => A |input: &str| JsonValue::Number(input.parse::<f64>().unwrap());
    /r#""(\\"|[^"])*""# => St |input: &str|
        input[1..input.len() - 1].to_string();

    S ::= V '$' |obj: JsonValue| obj;
    O ::= '{' '}' || JsonValue::Object(HashMap::new());
    O ::= '{' KVS '}' |map: HashMap<String, JsonValue>| JsonValue::Object(map);
    KVS ::= KVS ',' KV |mut map: HashMap<String, JsonValue>, (name, value): (String, JsonValue)| {
        map.insert(name, value);
        map
    };
    KVS ::= KV |(name, value): (String, JsonValue)| {
        let mut keys_values = HashMap::new();
        keys_values.insert(name, value);
        keys_values
    };
    KV ::= St ':' V |name: String, value: JsonValue| return (name, value);
    Str ::= St |string: String| JsonValue::Str(string);
    V ::= A |atom: JsonValue| atom;
    V ::= Str |string: JsonValue| string;
    V ::= O |obj: JsonValue| obj;
    V ::= L |list: JsonValue| list;
    L ::= '[' ']' || JsonValue::List(Vec::new());
    L ::= '[' ES ']' |vals: Vec<JsonValue>| JsonValue::List(vals);
    ES ::= ES ',' V |mut vals: Vec<JsonValue>, val: JsonValue| {
        vals.push(val);
        vals
    };
    ES ::= V |val: JsonValue| vec![val];
}

fn main() {
    let parser = Parser::new();
    println!("{}", parser.to_html_debug());
    // match parser.parse(&std::env::args().nth(1).expect("No input")) {
    //     Some(result) => println!("Result = {:?}", result),
    //     None => println!("Invalid input."),
    // }
}
