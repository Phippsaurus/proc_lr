use std::marker::PhantomData;

#[derive(Debug)]
pub struct Rule<Lhs, Rhs> {
    lhs: PhantomData<Lhs>,
    rhs: PhantomData<Rhs>,
}

macro_rules! rule {
    ($lhs:ty => $($rhs:ty),+) => {{
        Rule { lhs: PhantomData::<$lhs>, rhs: PhantomData::<($($rhs),+)> }
    }}
}

#[derive(Debug)]
struct S;
#[derive(Debug)]
struct End;

fn main() {
    let r = rule!(S => End);
    println!("Hello, world! {:?}", r);
}
