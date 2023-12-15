use chumsky::prelude::*;

use crate::Instruction;
use crate::error::CompResult;

trait P<T> = Parser<char, T, Error = Simple<char>>;
trait I = P<Instruction>;

pub fn parse(input: &str) -> CompResult<Vec<Instruction>> {
    todo!()
}

fn integer(input: &str) -> impl I {
    choice((
        decimal,
        hexadecimal,
        binary,
    ))
}

fn decimal(input: &str) -> impl I {
    filter(|ch| ch.is_digit(10))
        .repeated()
        .map(|s| s.parse())
}

fn hexadecimal(input: &str) -> impl I {
    filter(|ch| ch.is_digit(10))
        .repeated()
}

fn binary(input: &str) -> impl I {
    filter(|ch| ch.is_digit(10))
        .repeated()
}

fn comment(input: &str) -> impl P<()> {
    filter(|ch| *ch == ';')
        .then_ignore(none_of(';').repeated())
        .map(|_| ())
}

