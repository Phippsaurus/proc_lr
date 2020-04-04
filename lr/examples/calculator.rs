use lr::grammar;
use std::{
    env::args,
    io::{self, stdin, stdout, Write},
    iter::repeat,
};

grammar! {
    S: f64;
    E: f64;
    T: f64;
    F: f64;
    Id: f64;

    /r"[\r\n\s]+" => _;
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

fn parse(parser: &Parser, input: &str) {
    match parser.parse(input) {
        Ok(result) => println!("{} = {}", input, result),
        Err(error) => match error {
            ParseError::InvalidToken { offset, line } => {
                let invalid_line = input.lines().nth(line).unwrap();
                println!(
                    "Invalid input \"{}\" at column {} line {}:\n{}\n{}^",
                    &invalid_line[offset..offset + 1],
                    offset,
                    line,
                    invalid_line,
                    repeat(' ').take(offset).collect::<String>()
                );
            }
            ParseError::UnexpectedToken { offset, length, line } => {
                let invalid_line = input.lines().nth(line).unwrap();
                println!(
                    "Unexpected token \"{}\" at column {} line {}:\n{}\n{}",
                    &invalid_line[offset..offset + length],
                    offset,
                    line,
                    invalid_line,
                    repeat(' ')
                        .take(offset)
                        .chain(repeat('~').take(length))
                        .collect::<String>(),
                );
            }
            ParseError::IncompleteInput => println!("Incomplete input"),
        },
    }
}

fn main() -> io::Result<()> {
    let parser = Parser::new();
    match args().nth(1) {
        Some(ref input) => {
            parse(&parser, input);
            Ok(())
        }
        None => {
            let mut input = String::new();
            loop {
                print!("> ");
                stdout().flush()?;
                stdin().read_line(&mut input)?;
                parse(&parser, input.trim());
                input.clear();
            }
        }
    }
    // println!("{}", parser.to_html_debug());
}
