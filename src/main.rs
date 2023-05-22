#![doc = include_str!("../README.md")]

use std::{error::Error, fmt::Display};

type CompResult<T> = Result<T, CompError>;

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
    // Other(usize),
}

impl Display for CompError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompError::InvalidOp(i, op) => write!(f, "Invalid op on line {i}: `{op}`"),
            CompError::RequireArg(i, op) => {
                write!(f, "Operator `{op}` on line {i} requires an argument")
            }
            CompError::UnbalancedLoops => write!(f, "Program loop starts != loop ends"),
            // CompError::Other(i) => write!(f, "Miscellaneous error on line {i}"),
        }
    }
}

impl Error for CompError {}

fn goto(n: usize) -> String {
    format!(
        "<<<<<[<<<<]>{}",
        ">".repeat(n * 4)
    )
}

fn gotoi(i: u8) -> String {
    format!(
        "<<<<<[<<<<]>>>>>>>{0}[{1}>>>>{0}]{1}<<",
        "-".repeat(i as usize),
        "+".repeat(i as usize)
    )
}

fn goback() -> String {
    s!("<[>>>>]>")
}

fn gohome() -> String {
    s!("<[<<<<]>")
}

// fn find(i: u8) -> String {
//     /* while x != y: [x[-y-x]+y[x-y[-]]x-]
//        clears x* and y

//     x = dup cell.tag
//     y = dup target_tag
//     while x != y {
//         >>>>
//         x = dup cell.tag
//         y = dup target_tag
//     }
    
//      * x may be 1
//     */

//     format!(
//         "",
//         goback(),
//     )
// }

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
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

    /// Swap the top value and the Nth value backwards.
    Swpb(usize),

    /// Swap the top value and the Nth value.
    Swpn(usize),

    /// Assign the tag in the top value (not popped) to the top cell.
    Tag(Option<u8>),

    // TODO: allow dynamic tag finding
    /// Overwrite the top value with the cell at tag.
    Ctg(u8),

    /// Pop and write into cell at tag.
    Mtg(u8),

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

    /// A raw brain* string
    Raw(String),
}

impl Op {
    fn to_str(&self) -> String {
        match self {
            Op::Add => s!("[-<<<<+>>>>]<-<<<"),
            Op::Sub => s!("[-<<<<->>>>]<-<<<"),
            // Op::Mul => s!("<[->>+<<]>[->[->+<<<+>>]>[-<+>]<<]>[-]<<"),
            Op::Mul => todo!(),
            Op::Div => todo!(),
            Op::Push(arg) => s!(">>>+>[-]") + &"+".repeat(*arg as usize),
            Op::Set(arg) => s!("[-]") + &"+".repeat(*arg as usize),
            Op::Pop => s!("[-]<-<<<"),
            Op::Dup => s!(">[-]>>+>[-]<<<<[->+>>>+<<<<]>[-<+>]>>>"),
            Op::Swp => s!(">[-]<[->+<]<<<<[->>>>+<<<<]>>>>>[-<<<<<+>>>>>]<"),
            Op::Rot => format!("<<<<{}>>>>{}", Op::Swp, Op::Swp),
            Op::Swpb(i) => format!(
                "{}<[-]{1}[-{2}+{1}]>{2}[-<{1}+>{2}]<",
                Op::Dup,
                "<".repeat(*i),
                ">".repeat(*i),
            ),
            Op::Swpn(n) => format!(
                "<->>[-]<[->+<]{0}[-{1}+{0}]{1}>[-<{0}+{1}>]<<+>",
                goto(*n),
                goback(),
            ),
            Op::Tag(i) => {
                if let Some(i) = i {
                    format!(">>{}<<", Op::Set(*i))
                } else {
                    format!("[->+>+<<]>[-<+>]<")
                }
            },
            Op::Ctg(i) => format!(
                "<->>[-]<[->+<]{0}[-{1}+{0}]{1}>[-<{0}+{1}>]<<+>",
                gotoi(*i),
                goback(),
            ),
            Op::Mtg(i) => format!(
                "{0}[-]{1}[-{0}+{1}]<-<<<",
                gotoi(*i),
                goback(),
            ),
            Op::Skip => todo!(),
            Op::Pass => todo!(),
            Op::Loop => s!("["),
            Op::End => s!("]"),
            Op::Out => s!(".[-]<-<<<"),
            Op::In => s!(">>>+>[-],"),
            Op::Raw(code) => code.clone(),
        }
    }
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.to_str())
    }
}

pub struct Compiler;

impl Compiler {
    fn from_tokens(tokens: Vec<Op>) -> CompResult<String> {
        if tokens.iter().filter(|op| **op == Op::Loop).count()
            != tokens.iter().filter(|op| **op == Op::End).count()
        {
            return Err(CompError::UnbalancedLoops);
        }

        let mut data = String::from(">");
        for tok in tokens {
            data.push_str(&tok.to_str());
        }

        Ok(data)
    }

    fn from_str(code: &str) -> CompResult<String> {
        let mut toks = Vec::new();
        for (i, line) in code.lines().enumerate() {
            let mut line = line.trim();

            if !line.starts_with("raw") {
                if line.starts_with(';') || line.is_empty() || line.chars().all(|ch| ch.is_whitespace())
                {
                    continue;
                }

                if let Some(l) = line.split_once(';') {
                    line = l.0;
                }
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
                "swpb" => toks.push(Op::Swpb(
                    arg.parse()
                        .map_err(|_| CompError::RequireArg(i, op.to_owned()))?,
                )),
                "swpn" => toks.push(Op::Swpn(
                    arg.parse()
                        .map_err(|_| CompError::RequireArg(i, op.to_owned()))?,
                )),
                "tag" => toks.push(Op::Tag(
                    if arg.is_empty() {
                        None
                    } else {
                        Some(arg.parse()
                            .map_err(|_| CompError::RequireArg(i, op.to_owned()))?)
                    }
                )),
                "ctg" => toks.push(Op::Ctg(
                    arg.parse()
                        .map_err(|_| CompError::RequireArg(i, op.to_owned()))?,
                )),
                "mtg" => toks.push(Op::Mtg(
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
                "raw" => toks.push(Op::Raw(arg.to_owned())),
                _ => return Err(CompError::InvalidOp(i, op.to_owned())),
            }
        }

        Self::from_tokens(toks)
    }
}

fn main() {
    let sample = include_str!("test.air");
    println!("{}", Compiler::from_str(sample).expect("Failed to compile"));
}
