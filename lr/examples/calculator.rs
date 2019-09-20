use lr::grammar;

grammar! {
    S: f64;
    E: f64;
    T: f64;
    F: f64;
    Id: f64;

    /r"[\n\s]+" => _;
    /r"\d+(\.\d+)?" => Id |input: &str| input.parse::<f64>().unwrap();

    S ::= E '$' |e: f64| e;
    E ::= E '+' T |e: f64, t: f64| e + t;
    E ::= E '-' T |e: f64, t: f64| e - t;
    E ::= T |t: f64| t;
    T ::= T '*' F |t: f64, f: f64| t * f;
    T ::= T '/' F |t: f64, f: f64| t / f;
    T ::= F |f: f64| f;
    F ::= Id |id: f64| id;
    F ::= '-' Id |id: f64| -id;
    F ::= '(' E ')' |e: f64| e;
}

fn main() {
    let parser = Parser::new();
    match parser.parse(&std::env::args().nth(1).expect("No input")) {
        Some(result) => println!("Result = {:?}", result),
        None => println!("Invalid input."),
    }
    // println!("{}", parser.to_html_debug());
}
