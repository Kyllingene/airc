// TODO: support more kinds of brain* interpreters

use std::{
    error::Error,
    fmt::Display,
    fs::{read_to_string, File},
    io::Write,
    path::Path,
    process::exit,
};

use sarge::prelude::*;

type CompResult<T> = Result<T, CompError>;

macro_rules! s {
    () => {
        String::new()
    };

    ($s:expr) => {
        String::from($s)
    };
}

macro_rules! f {
    ($($s:expr),*) => {
        format!(
            $(
                $s,
            )*
        )
    };

    ($($s:expr),*,) => {
        format!(
            $(
                $s,
            )*
        )
    };
}

#[derive(Debug, Clone)]
enum CompError {
    InvalidOp(usize, String),
    InvalidChar(usize, String),
    InvalidString(usize, String),
    RequiresArg(usize, String),
    UnbalancedIfs,
    UnbalancedLoops,
}

impl Display for CompError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompError::InvalidOp(i, op) => write!(f, "Invalid op on line {i}: `{op}`"),
            CompError::InvalidChar(i, ch) => write!(f, "Invalid character on line {i}: `{ch}`"),
            CompError::InvalidString(i, ch) => write!(f, "Invalid string on line {i}: `{ch}`"),
            CompError::RequiresArg(i, op) => {
                write!(f, "Operator `{op}` on line {i} requires an argument")
            }
            CompError::UnbalancedIfs => write!(f, "Unequal numbers of `if` and `endif`"),
            CompError::UnbalancedLoops => write!(f, "Unequal numbers of `loop` and `end`"),
        }
    }
}

impl Error for CompError {}

fn goto(n: usize) -> String {
    format!("<<<<<[<<<<]>{}", ">".repeat(n * 4))
}

fn gotoi(i: u8) -> String {
    f!(
        "<<<<<[<<<<]>>>>>>>{0}[{1}>>>>{0}]{1}<<",
        "-".repeat(i as usize),
        "+".repeat(i as usize)
    )
}

fn goback() -> String {
    s!("<[>>>>]>")
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
    Add(Option<u8>),

    /// Push pop - pop.
    Sub(Option<u8>),

    /// Push pop * pop.
    Mul(Option<u8>),

    /// Push pop / pop.
    Div(Option<u8>),

    // boolean
    /// Push pop == pop.
    Eq(Option<u8>),
    /// Push pop != pop.
    Neq(Option<u8>),

    /// Push !pop.
    Not(Option<u8>),
    /// Push pop & pop.
    And(Option<u8>),

    // stack
    /// Push a value onto the stack.
    Push(u8),

    /// Set the current stack value.
    Set(u8),

    /// Pop a value off of the stack.
    Pop(u8),

    /// Duplicate the top value.
    Dup(u8),

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

    /// Run code between here and endif if top != 0.
    If,

    // flow control
    // TODO: add some sort of pre-/post-processing to allow more complex flow control
    /// Run code between here and endif if top == 0.
    Ifn,
    /// Close an if statement
    Endif,

    /// Loop like in brain* ([)
    Loop,
    /// Loop like in brain* (])
    End,

    // io
    /// Output pop to the output stream (i.e. stdout).
    Out(Option<u8>),

    /// Push a byte from the input stream (i.e. stdin).
    In,

    // other
    /// A raw brain* string
    Raw(String),

    /// A macro
    Macro(Vec<Op>),
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", match self {
            Op::Add(arg) => if let Some(arg) = arg {
                "+".repeat(*arg as usize)
            } else {
                s!("[-<<<<+>>>>]<-<<<")
            }
            Op::Sub(arg) => if let Some(arg) = arg {
                "-".repeat(*arg as usize)
            } else {
                s!("[-<<<<->>>>]<-<<<")
            }
            Op::Mul(arg) => f!(
                "{}>[-]<<<<[-]<[>>>>>+<<<<<-]>>>>>[<[<<<<+>+>>>-]<<<[>>>+<<<-]>>>>-]<[-]<-<<<",
                match arg {
                    Some(arg) => Op::Push(*arg).to_string(),
                    None => s!(),
                }
            ),
            Op::Div(arg) => f!("{}>[-]<<-<<[-]<<->[>+<-]>[>>>[<<<<<+>>>>>>+<-]>[<+>-]<<<<<<[>>>>>>+<<<<-[>>>>[-]<<+<<-]>>[<<+>>-]>>[<<<<<<-[>-<[-]]+>>>>>>-]<<<<<<-]>+>]<<[-]+>>>>[-]>[-]<<<<",
                match arg {
                    Some(arg) => Op::Push(*arg).to_string(),
                    None => s!(),
                }
            ),
            Op::Eq(arg) => f!("{}<<<<[->>>>-<<<<]+>>>>[<<<<->>>>[-]]<-<<<", match arg {
                Some(arg) => Op::Push(*arg).to_string(),
                None => s!(),
            }),
            Op::Neq(arg) => f!(
                "{}<<<[-]>>>>[-]<<<<<[>>>>>+<<<<<-]>>>>[>-<<<<+>>>-]<<<[>>>+<<<-]>>>>[<<<<<+>>>>>[-]]<<-<<<",
                match arg {
                    Some(arg) => Op::Push(*arg).to_string(),
                    None => s!(),
                }
            ),
            Op::Not(arg) => f!(
                "\n{}>[-]<-[>-<-]>[<+>-]<\n",
                match arg {
                    Some(arg) => Op::Push(*arg).to_string(),
                    None => s!(),
                }
            ),
            Op::And(arg) => f!(
                "{}>[-]<<<<<[>>>>[>+<-]<<<<-]>>>>[-]>[-<<<<<+>>>>>]<<-<<<",
                match arg {
                    Some(arg) => Op::Push(*arg).to_string(),
                    None => s!(),
                }
            ),
            Op::Push(arg) => s!(">>>+>[-]") + &Op::Set(*arg).to_string(),
            Op::Set(arg) => s!("[-]") + &"+".repeat(*arg as usize),
            Op::Pop(arg) => "[-]<-<<<".repeat(*arg as usize),
            Op::Dup(arg) => ">[-]>>+>[-]<<<<[->+>>>+<<<<]>[-<+>]>>>".repeat(*arg as usize),
            Op::Swp => s!(">[-]<[->+<]<<<<[->>>>+<<<<]>>>>>[-<<<<<+>>>>>]<"),
            Op::Rot => format!("<<<<{}>>>>{}", Op::Swp, Op::Swp),
            Op::Swpb(i) => f!(
                "{}<[-]{1}[-{2}+{1}]>{2}[-<{1}+>{2}]<",
                Op::Dup(1),
                "<".repeat(*i),
                ">".repeat(*i)
            ),
            Op::Swpn(n) =>
                f!("<->>[-]<[->+<]{0}[-{1}+{0}]{1}>[-<{0}+{1}>]<<+>", goto(*n), goback()),
            Op::Tag(i) => if let Some(i) = i { f!(">>{}<<", Op::Set(*i)) } else { f!("[->+>+<<]>[-<+>]<") }
            Op::Ctg(i) => f!("<->>[-]<[->+<]{0}[-{1}+{0}]{1}>[-<{0}+{1}>]<<+>", gotoi(*i), goback()),
            Op::Mtg(i) => f!("{0}[-]{1}[-{0}+{1}]<-<<<", gotoi(*i), goback()),
            Op::If => s!("["),
            Op::Ifn => Op::Not(None).to_string() + &Op::If.to_string(),
            Op::Endif => s!(">[-]]<"),
            Op::Loop => s!("["),
            Op::End => s!("]"),
            Op::Out(arg) => match arg {
                    Some(arg) => f!(">[-]{}.<", "+".repeat(*arg as usize)),
                    None => s!(".[-]<-<<<"),
                }
            Op::In => s!(">>>+>[-],"),
            Op::Raw(code) => code.clone(),
            Op::Macro(ops) => ops.iter()
                    .flat_map(|op| op.to_string().chars().collect::<Vec<char>>())
                    .collect(),
        })
    }
}

pub struct Compiler;

impl Compiler {
    fn from_tokens(tokens: Vec<Op>) -> CompResult<String> {
        if tokens.iter().filter(|op| matches!(op, Op::Loop)).count()
            != tokens.iter().filter(|op| matches!(op, Op::End)).count()
        {
            return Err(CompError::UnbalancedLoops);
        }

        if tokens
            .iter()
            .filter(|op| matches!(op, Op::If | Op::Ifn))
            .count()
            != tokens.iter().filter(|op| matches!(op, Op::Endif)).count()
        {
            return Err(CompError::UnbalancedIfs);
        }

        let mut data = s!(">");
        for tok in tokens {
            data.push_str(&tok.to_string());
        }

        while let Some(_) = data.find("<>") {
            data = data.replace("<>", "");
        }

        while let Some(_) = data.find("><") {
            data = data.replace("><", "");
        }

        while let Some(_) = data.find("+-") {
            data = data.replace("+-", "");
        }

        while let Some(_) = data.find("-+") {
            data = data.replace("-+", "");
        }

        Ok(data)
    }

    fn from_str(code: &str) -> CompResult<String> {
        let mut toks = Vec::new();
        for (i, line) in code.lines().enumerate() {
            let mut line = line.trim();

            if line.starts_with(';') || line.is_empty() || line.chars().all(|ch| ch.is_whitespace())
            {
                continue;
            }

            if !line.starts_with("raw") {
                if let Some(l) = line.split_once(';') {
                    line = l.0.trim();
                }
            }

            let (op, mut arg) = line.split_once(' ').unwrap_or((line, ""));
            let op = op.trim().to_lowercase();
            arg = arg.trim();

            let fmt;
            if let Some(Some(ch)) = arg.strip_prefix('\'').map(|s| s.strip_suffix('\'')) {
                if ch.len() != 1 && ch != "\\n" {
                    return Err(CompError::InvalidChar(i, arg.to_string()));
                }

                if ch == "\\n" {
                    arg = "10";
                } else {
                    fmt = format!("{}", ch.as_bytes()[0]);
                    arg = &fmt;
                }
            } else if let Some(Some(string)) = arg.strip_prefix('"').map(|s| s.strip_suffix('"')) {
                fmt = string.replace("\\n", "\n");
                arg = &fmt;
            } else if arg.contains('\'') {
                return Err(CompError::InvalidChar(i, arg.to_string()));
            } else if arg.contains('"') {
                return Err(CompError::InvalidString(i, arg.to_string()));
            }

            match op.as_str() {
                "add" => toks.push(Op::Add(if arg.is_empty() {
                    None
                } else {
                    Some(
                        arg.parse()
                            .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?,
                    )
                })),
                "sub" => toks.push(Op::Sub(if arg.is_empty() {
                    None
                } else {
                    Some(
                        arg.parse()
                            .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?,
                    )
                })),
                "mul" => toks.push(Op::Mul(if arg.is_empty() {
                    None
                } else {
                    Some(
                        arg.parse()
                            .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?,
                    )
                })),
                "div" => toks.push(Op::Div(if arg.is_empty() {
                    None
                } else {
                    Some(
                        arg.parse()
                            .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?,
                    )
                })),
                "eq" => toks.push(Op::Eq(if arg.is_empty() {
                    None
                } else {
                    Some(
                        arg.parse()
                            .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?,
                    )
                })),
                "neq" => toks.push(Op::Neq(if arg.is_empty() {
                    None
                } else {
                    Some(
                        arg.parse()
                            .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?,
                    )
                })),
                "not" => toks.push(Op::Not(if arg.is_empty() {
                    None
                } else {
                    Some(
                        arg.parse()
                            .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?,
                    )
                })),
                "and" => toks.push(Op::And(if arg.is_empty() {
                    None
                } else {
                    Some(
                        arg.parse()
                            .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?,
                    )
                })),
                "push" => toks.push(Op::Push(
                    arg.parse()
                        .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?,
                )),
                "set" => toks.push(Op::Set(
                    arg.parse()
                        .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?,
                )),
                "pop" => toks.push(Op::Pop(if arg.is_empty() {
                    1
                } else {
                    arg.parse()
                        .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?
                })),
                "dup" => toks.push(Op::Dup(if arg.is_empty() {
                    1
                } else {
                    arg.parse()
                        .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?
                })),
                "swp" => toks.push(Op::Swp),
                "swpb" => toks.push(Op::Swpb(
                    arg.parse()
                        .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?,
                )),
                "swpn" => toks.push(Op::Swpn(
                    arg.parse()
                        .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?,
                )),
                "tag" => toks.push(Op::Tag(if arg.is_empty() {
                    None
                } else {
                    Some(
                        arg.parse()
                            .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?,
                    )
                })),
                "ctg" => toks.push(Op::Ctg(
                    arg.parse()
                        .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?,
                )),
                "mtg" => toks.push(Op::Mtg(
                    arg.parse()
                        .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?,
                )),
                "rot" => toks.push(Op::Rot),
                "if" | "{" => toks.push(Op::If),
                "ifn" | "!{" => toks.push(Op::Ifn),
                "endif" | "}" => toks.push(Op::Endif),
                "loop" | "[" => toks.push(Op::Loop),
                "end" | "]" => toks.push(Op::End),
                "out" => toks.push(Op::Out(if arg.is_empty() {
                    None
                } else {
                    Some(
                        arg.parse()
                            .map_err(|_| CompError::RequiresArg(i, op.to_owned()))?,
                    )
                })),
                "in" => toks.push(Op::In),
                "raw" => toks.push(Op::Raw(arg.to_owned())),

                "print" => {
                    if arg.is_empty() {
                        return Err(CompError::RequiresArg(i, op.to_owned()));
                    }

                    let mut ops = Vec::new();
                    for ch in arg.as_bytes() {
                        ops.push(Op::Out(Some(*ch)));
                    }

                    toks.push(Op::Macro(ops));
                }

                _ => {
                    return Err(CompError::InvalidOp(i, op.to_owned()));
                }
            }
        }

        Self::from_tokens(toks)
    }
}

fn print_err(msg: impl Display, e: impl Display) {
    eprintln!("{}[38;5;1m{msg}: {e}{0}[0m", 27 as char);
}

fn print_warn(msg: impl Display) {
    eprintln!("{}[38;5;3m{msg}{0}[0m", 27 as char);
}

fn print_good(msg: impl Display) {
    eprintln!("{}[38;5;2m{msg}{0}[0m", 27 as char);
}

fn print_help() {
    print_warn(include_str!("../usage.txt"));
}

fn main() {
    let mut parser = ArgumentParser::new();
    parser.add(arg!(flag, both, 'h', "help"));
    parser.add(arg!(str, both, 'o', "output"));
    parser.add(arg!(flag, both, 's', "stdout"));

    let input = match parser.parse() {
        Ok(i) => i,
        Err(e) => {
            print_err("failed to parse arguments: ", e);
            exit(1);
        }
    };

    if get_flag!(parser, both, 'h', "help") {
        print_help();
        return;
    }

    let stdout = get_flag!(parser, both, 's', "stdout");
    if stdout && get_arg!(parser, both, 'o', "output").map_or(false, |a| a.val.is_some()) {
        print_err(
            "invalid options",
            "you may only specifiy one of --output or --stdout",
        );
        exit(1);
    }

    if input.is_empty() {
        print_err("invalid arguments", "no input files given");
        print_help();
        exit(1);
    }

    let output_files = get_arg!(parser, both, 'o', "output").unwrap().clone();
    let mut output_files = output_files
        .val
        .map(|v| v.get_str().split(',').map(String::from).collect::<Vec<_>>());

    if !stdout && output_files.is_none() {
        output_files = Some(
            input
                .iter()
                .map(|file| {
                    Path::new(file)
                        .with_extension("bf")
                        .to_str()
                        .unwrap()
                        .to_string()
                })
                .to_owned()
                .collect(),
        );
    }

    if let Some(output_files) = &output_files {
        if output_files.len() != input.len() && output_files.len() != 1 {
            print_err(
                "invalid arguments",
                "must have equal amounts of input and output files, or 1 output file",
            );
            exit(1);
        }
    }

    let mut compiled = Vec::new();
    for file in input {
        let code = match read_to_string(&file) {
            Ok(c) => c,
            Err(e) => {
                print_err(format!("failed to read file {file}"), e);
                exit(1);
            }
        };

        print_warn(format!("compiling {}", file));
        compiled.push(match Compiler::from_str(&code) {
            Ok(c) => c,
            Err(e) => {
                print_err(format!("failed to compile file {file}"), e);
                exit(1);
            }
        });
    }

    if let Some(output_files) = output_files {
        if output_files.len() == 1 {
            print_warn(format!("writing to {}", output_files[0]));
            let mut file = match File::create(&output_files[0]) {
                Ok(f) => f,
                Err(e) => {
                    print_err("failed to open output file", e);
                    exit(1);
                }
            };

            for output in compiled {
                if let Err(e) = file.write_all(output.as_bytes()) {
                    print_err("failed to write to file", e);
                    exit(1);
                }
            }
        } else {
            for (i, filename) in output_files.iter().enumerate() {
                print_warn(format!("writing to {}", filename));
                let mut file = match File::create(&output_files[0]) {
                    Ok(f) => f,
                    Err(e) => {
                        print_err("failed to open output file", e);
                        exit(1);
                    }
                };

                if let Err(e) = file.write_all(compiled[i].as_bytes()) {
                    print_err("failed to write to file", e);
                    exit(1);
                }

                print_good(format!("wrote {filename}"));
            }
        }
    } else {
        print_good("printing output to stdout");
        for output in compiled {
            println!("{output}");
        }
    }

    print_good("done");
}
