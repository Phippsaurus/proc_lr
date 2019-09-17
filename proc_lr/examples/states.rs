#![feature(proc_macro_hygiene)]
use proc_lr::grammar;

grammar!(
'0' => id {}
'1' => id {}
'2' => id {}
'3' => id {}
'4' => id {}
'5' => id {}
'6' => id {}
'7' => id {}
'8' => id {}
'9' => id {}

S ::= E '$'     { }
E ::= E '+' T   { }
E ::= T         { }
T ::= T '*' F   { }
T ::= F         { }
F ::= id        { }
F ::= '(' E ')' { }
);

fn main() {
    let parser = Parser::new();
    let scanner = Scanner::new();
    // parser.parse(scanner.scan("2+4*(6+5)+8*7"));
    println!("{}", parser.to_html_debug());
}
