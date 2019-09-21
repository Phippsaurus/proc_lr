# LR Parser

This is a hobby project trying to further _rust_ knowledge and understanding of
parsers. It is not meant to replace any established parsing libraries or parser
generators.

Generate a working __LR__-parser with minimum effort and without
external tools through a `proc_macro`.  At this point, a full __LR(1)__-parser
is generated, but the goal is to simplify this further. Features like
associativity are still missing.

## Usage

Only one macro, `grammar` is required. It is called with a set of type
specifications for (non)terminals', a list of scan rules, and a list of parse
rules.

Example call for a simple grammar of mathematical terms:

```rust
grammar! {
	// Type Specifications:
	// ====================
	S: f64;
	E: f64;
	T: f64;
	F: f64;
	Id: f64;

	// Scanner Rules:
	// ==============
	/r"[\n\s]+" => _;
	/r"\d+(\.\d+)?" => Id |input: &str| input.parse::<f64>().unwrap();

	// Parse Rules:
	// ============
	S ::= E '$'     |e: f64| e;
	E ::= E '+' T   |e: f64, t: f64| e + t;
	E ::= E '-' T   |e: f64, t: f64| e - t;
	E ::= T         |t: f64| t;
	T ::= T '*' F   |t: f64, f: f64| t * f;
	T ::= T '/' F   |t: f64, f: f64| t / f;
	T ::= F         |f: f64| f;
	F ::= Id        |id: f64| id;
	F ::= '-' Id    |id: f64| -id;
	F ::= '(' E ')' |e: f64| e;
}
```

As seen in the example, the idea is to infer as much information as possible,
and be readable from experience with rust and theoretical texts about parsers.
As such, scanned input (e.g. whitespace) is simply ignored by producing an `_`
(don't care term), and the first rule's left hand side becomes the start
symbol. Using terminals like the literal character `+` in a parse rule
automatically generates scanner rules. The closures behind rules define how
inputs should be processed and unfortunately type inference for their arguments
does not work.
