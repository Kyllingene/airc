#![doc = include_str!("../README.md")]

use std::{error::Error, fmt::Display};

type CompResult<'a, T> = Result<T, CompError>;

macro_rules! s {
    ( $s:expr ) => {
        String::from($s)
    };
}

#[derive(Debug, Clone)]
enum CompError {
    InvalidOp(usize, String),
    RequireArg(usize, String),
    UnbalancedLoops,
    Other(usize),
}

impl Display for CompError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompError::InvalidOp(i, op) => write!(f, "Invalid op on line {i}: `{op}`"),
            CompError::RequireArg(i, op) => {
                write!(f, "Operator `{op}` on line {i} requires an argument")
            }
            CompError::UnbalancedLoops => write!(f, "Program loop starts != loop ends"),
            CompError::Other(i) => write!(f, "Miscellaneous error on line {i}"),
        }
    }
}

impl Error for CompError {}

#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Op {
    // math
    /// Push pop + pop.
    Add,

    /// Push pop - pop.
    Sub,

    /// Push pop * pop.
    Mul,

    /// Push pop / pop.
    Div,

    // stack
    /// Push a value onto the stack.
    Push(u8),

    /// Set the current stack value.
    Set(u8),

    /// Pop a value off of the stack.
    Pop,

    /// Duplicate the top value.
    Dup,

    /// Swap the top two values.
    Swp,

    /// Rotate the top three values (i.e. 1, 2, 3 => 2, 3, 1).
    Rot,

    /// Swap the top value and value N backwards.
    Swpn(usize),

    // flow control
    /// Skip the next instruction if pop == 0.
    Skip,

    /// Skip the next instruction is pop != 0.
    Pass,

    /// Loop like in brain*
    Loop,
    End,

    // io
    /// Output pop to the output stream (i.e. stdout).
    Out,

    /// Push a byte from the input stream (i.e. stdin).
    In,
}

impl Op {
    fn to_str(&self, start: usize) -> String {
        match self {
            Op::Add => s!("[-<+>]<"),
            Op::Sub => s!("[-<->]<"),
            Op::Mul => s!("<[->>+<<]>[->[->+<<<+>>]>[-<+>]<<]>[-]<<"),
            Op::Div => todo!(),
            Op::Push(arg) => s!(">[-]") + &"+".repeat(*arg as usize),
            Op::Set(arg) => s!("[-]") + &"+".repeat(*arg as usize),
            Op::Pop => s!("[-]<"),
            Op::Dup => s!(">[-]>[-]<<[->+>+<<]>>[-<<+>>]<"),
            // a b b
            Op::Swp => format!("{}<[-]<[->+<]>>[-<<+>>]<", Op::Dup.to_str(start)),
            Op::Rot => format!("<{}>{}", Op::Swp.to_str(start - 1), Op::Swp.to_str(start)),
            Op::Swpn(i) => format!(
                "{}<[-]{1}[-{2}+{1}]>{2}[-<{1}+>{2}]<",
                Op::Dup.to_str(start),
                "<".repeat(*i),
                ">".repeat(*i),
            ),
            Op::Skip => todo!(),
            Op::Pass => todo!(),
            Op::Loop => s!("["),
            Op::End => s!("]"),
            Op::Out => s!(".[-]<"),
            Op::In => s!(">[-],"),
        }
    }

    fn end_pos(&self, start: usize) -> usize {
        match self {
            Op::Add => start - 1,
            Op::Sub => start - 1,
            Op::Mul => start - 1,
            Op::Div => start - 1,
            Op::Push(_) => start + 1,
            Op::Set(_) => start,
            Op::Pop => start - 1,
            Op::Dup => start + 1,
            Op::Swp => start,
            Op::Rot => start,
            Op::Swpn(_) => start,
            Op::Skip => start - 1,
            Op::Pass => start - 1,
            Op::Loop => start,
            Op::End => start,
            Op::Out => start - 1,
            Op::In => start - 1,
        }
    }
}

pub struct Compiler;

impl Compiler {
    fn from_tokens(tokens: &Vec<Op>) -> CompResult<String> {
        if tokens.iter().filter(|op| **op == Op::Loop).count()
            != tokens.iter().filter(|op| **op == Op::End).count()
        {
            return Err(CompError::UnbalancedLoops);
        }

        let mut data = String::new();
        let mut pos = 0;
        for tok in tokens {
            pos = tok.end_pos(pos);
            data.push_str(&tok.to_str(pos));
        }

        Ok(data)
    }

    fn from_str(code: &str) -> CompResult<String> {
        let mut toks = Vec::new();
        for (i, mut line) in code.lines().enumerate() {
            if line.starts_with(';') || line.is_empty() || line.chars().all(|ch| ch.is_whitespace())
            {
                continue;
            }

            if let Some(l) = line.split_once(';') {
                line = l.0;
            }

            let line = line.trim().to_lowercase();
            let (op, arg) = line.split_once(' ').unwrap_or((&line, ""));

            match op {
                "add" => toks.push(Op::Add),
                "sub" => toks.push(Op::Sub),
                "mul" => toks.push(Op::Mul),
                "div" => toks.push(Op::Div),
                "push" => toks.push(Op::Push(
                    arg.parse()
                        .map_err(|_| CompError::RequireArg(i, op.to_owned()))?,
                )),
                "set" => toks.push(Op::Set(
                    arg.parse()
                        .map_err(|_| CompError::RequireArg(i, op.to_owned()))?,
                )),
                "pop" => toks.push(Op::Pop),
                "dup" => toks.push(Op::Dup),
                "swp" => toks.push(Op::Swp),
                "swpn" => toks.push(Op::Swpn(
                    arg.parse()
                        .map_err(|_| CompError::RequireArg(i, op.to_owned()))?,
                )),
                "rot" => toks.push(Op::Rot),
                "skip" => toks.push(Op::Skip),
                "pass" => toks.push(Op::Pass),
                "loop" => toks.push(Op::Loop),
                "end" => toks.push(Op::End),
                "out" => toks.push(Op::Out),
                "in" => toks.push(Op::In),
                _ => return Err(CompError::InvalidOp(i, op.to_owned())),
            }
        }

        Self::from_tokens(&toks)
    }
}

fn main() {
    let sample = include_str!("test.air");
    println!("{}", Compiler::from_str(sample).expect("Failed to compile"));
}
