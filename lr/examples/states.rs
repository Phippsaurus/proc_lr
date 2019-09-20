use lr::grammar;

grammar! {
    S: i64;
    E: i64;
    T: i64;
    F: i64;
    Id: i64;

    // /r"\s+" => _;
    /r"-?\d+" => Id |input: &str| input.parse::<i64>().unwrap();

    S ::= E '$' |e: i64| e;
    E ::= E '+' T |e: i64, t: i64| e + t;
    E ::= T |t: i64| t;
    T ::= T '*' F |t: i64, f: i64| t * f;
    T ::= F |f: i64| f;
    F ::= Id |id: i64| id;
    F ::= '(' E ')' |e: i64| e;
}

fn main() {
    let parser = Parser::new();
    match parser.parse(&std::env::args().nth(1).expect("No input")) {
        Some(result) => println!("Result = {:?}", result),
        None => println!("Invalid input."),
    }
    // println!("{}", parser.to_html_debug());
}
