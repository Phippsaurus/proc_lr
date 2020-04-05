pub use lazy_static::lazy_static;
pub use proc_lr::*;
pub use regex::Regex;

#[derive(Debug)]
pub struct ShiftReduce {
    shift_id: usize,
    goto: usize,
    pop: usize,
    lookahead: Vec<usize>,
}

impl ShiftReduce {
    pub fn new(shift_id: usize, goto: usize, pop: usize, lookahead: Vec<usize>) -> Self {
        Self {
            shift_id,
            goto,
            pop,
            lookahead,
        }
    }
}

#[derive(Debug)]
pub struct LookaheadReduce {
    goto: usize,
    pop: usize,
    lookahead: Vec<usize>,
}

impl LookaheadReduce {
    pub fn new(goto: usize, pop: usize, lookahead: Vec<usize>) -> Self {
        Self {
            goto,
            pop,
            lookahead,
        }
    }
}

#[derive(Debug)]
pub enum Action {
    Undefined,
    Shift(usize),
    Reduce(usize),
    Goto(usize),
    Accept,
}

pub enum ParseError {
    InvalidToken {
        offset: usize,
        line: usize,
    },
    UnexpectedToken {
        offset: usize,
        length: usize,
        line: usize,
    },
    IncompleteInput,
}
