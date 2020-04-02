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
    let input = std::env::args().nth(1).expect("No input");
    match parser.parse(&input) {
        Ok(result) => println!("Result = {:?}", result),
        Err(error) => match error {
            ParseError::InvalidToken { offset } => println!(
                "Invalid input at column {}:\n{}\n{}^",
                offset,
                input,
                std::iter::repeat(' ').take(offset).collect::<String>()
            ),
            ParseError::UnexpectedToken { offset, length } => println!(
                "Unexpected token at column {}:\n{}\n{}",
                offset,
                input,
                std::iter::repeat(' ')
                    .take(offset)
                    .chain(std::iter::repeat('~').take(length))
                    .collect::<String>(),
            ),
            ParseError::IncompleteInput => println!("Incomplete input"),
        },
    }
    // println!("{}", parser.to_html_debug());
}
