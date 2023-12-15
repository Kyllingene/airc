#![feature(trait_alias)]

use std::collections::HashMap;
use std::fmt::Display;
use std::fs::{read_to_string, File};
use std::io::{Read, Write};
use std::path::Path;
use std::process::exit;

use sarge::prelude::*;

// TODO: support more kinds of brain* interpreters

// mod parse;
mod error;

use error::{CompError, CompResult};

macro_rules! ss {
    ($s:expr) => {
        String::from($s)
    };

    () => {
        String::new()
    };
}

#[inline]
fn goto(n: usize) -> String {
    format!("<<<<<[<<<<]>{}", ">".repeat(n * 4))
}

#[inline]
fn goto_tag(i: u8) -> String {
    format!(
        "<<<<<[<<<<]>>>>>>>{0}[{1}>>>>{0}]{1}<<",
        "-".repeat(i as usize),
        "+".repeat(i as usize)
    )
}

#[inline]
fn goback() -> String {
    ss!(">>>[>>>>]>")
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Instruction {
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
    /// Push pop > pop.
    Gt(Option<u8>),
    /// Push pop < pop.
    Lt(Option<u8>),

    /// Push !pop.
    Not(Option<u8>),
    /// Push pop & pop.
    And(Option<u8>),
    /// Push pop | pop.
    Or(Option<u8>),

    // stack
    /// Push a value onto the stack.
    Push(u8),

    /// Push a string onto the stack, with its length.
    Str(String),

    /// Push a null-terminated string onto the stack.
    CStr(String),

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
    Swpb(Option<usize>),

    /// Swap the top value and the Nth value.
    Swpn(usize),

    /// Assign the tag in the top value (not popped) to the top cell.
    Tag(Option<u8>),

    // TODO: allow dynamic tag finding
    /// Overwrite the top value with the cell at tag.
    Ctg(u8),

    /// Pop and write into cell at tag.
    Mtg(u8),

    /// Retrieve the tag of the top cell and push it.
    Ptg,

    // flow control
    /// Run code between here and endif if top != 0.
    If,

    // TODO: add some sort of pre-/post-processing to allow more complex flow control
    /// Close an if statement
    Endif,

    /// Loop like in brain* ([)
    Loop,
    /// Loop like in brain* (])
    End,

    // io
    /// Output top to the output stream (i.e. stdout)
    Out(Option<u8>),

    /// Output pop to the output stream (i.e. stdout).
    Pout(Option<u8>),

    /// Push a byte from the input stream (i.e. stdin).
    In,

    // other
    /// A raw brain* string
    Raw(String),

    /// A macro
    Macro(Vec<Instruction>),
}

impl Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let data = match self {
            Instruction::Add(arg) => if let Some(arg) = arg {
                "+".repeat(*arg as usize)
            } else {
                ss!("[-<<<<+>>>>]<-<<<")
            }
            Instruction::Sub(arg) => if let Some(arg) = arg {
                "-".repeat(*arg as usize)
            } else {
                ss!("[-<<<<->>>>]<-<<<")
            }
            Instruction::Mul(arg) => format!(
                "{}>[-]<<<<[-]<[>>>>>+<<<<<-]>>>>>[<[<<<<+>+>>>-]<<<[>>>+<<<-]>>>>-]<[-]<-<<<",
                match arg {
                    Some(arg) => Instruction::Push(*arg).to_string(),
                    None => ss!(),
                }
            ),
            Instruction::Div(arg) => format!("{}>[-]<<-<<[-]<<->[>+<-]>[>>>[<<<<<+>>>>>>+<-]>[<+>-]<<<<<<[>>>>>>+<<<<-[>>>>[-]<<+<<-]>>[<<+>>-]>>[<<<<<<-[>-<[-]]+>>>>>>-]<<<<<<-]>+>]<<[-]+>>>>[-]>[-]<<<<",
                match arg {
                    Some(arg) => Instruction::Push(*arg).to_string(),
                    None => ss!(),
                }
            ),
            Instruction::Eq(arg) => format!("{}<<<<[->>>>-<<<<]+>>>>[<<<<->>>>[-]]<-<<<", match arg {
                Some(arg) => Instruction::Push(*arg).to_string(),
                None => ss!(),
            }),
            Instruction::Neq(arg) => format!(
                "{}<<<[-]>>>>[-]<<<<<[>>>>>+<<<<<-]>>>>[>-<<<<+>>>-]<<<[>>>+<<<-]>>>>[<<<<<+>>>>>[-]]<<-<<<",
                match arg {
                    Some(arg) => Instruction::Push(*arg).to_string(),
                    None => ss!(),
                }
            ),
            /*
                ty> [-] tx<<<< [-] ux<< -
                x> [tx> +
                    y>>> [- tx<<< [-] ty>>>> + y<]
                tx<<< [- ux<< + tx>>]
                ty>>>> [- y< + ty>]
                y< - x<<<< - ]

                ux< [- x> + ux< ]+ x>
            */
            Instruction::Gt(arg) => format!(
                "{}ty> [-] tx<<<< [-] ux<< -
                x> [tx> +
                    y>>> [- tx<<< [-] ty>>>> + y<]
                tx<<< [- ux<< + tx>>]
                ty>>>> [- y< + ty>]
                y< - x<<<< - ; ]

                ux< [- x> + ux< ]+ x>",
                match arg {
                    Some(arg) => Instruction::Push(*arg).to_string(),
                    None => ss!(),
                }
            ),

            Instruction::Lt(arg) => format!(
                "{}>[-]<[->+<]<<<<[->>>>+<<<<]>>>>>[-<<<<<+>>>>>][-]<<<<[-]<<[-]>[>+>>>[-<<<[-]>>>>+<]<<<[-<<+>>]>>>>[-<+>]<-<<<<-]<[->+<]+>",
                match arg {
                    Some(arg) => Instruction::Push(*arg).to_string(),
                    None => ss!(),
                },
            ),
            Instruction::Not(arg) => format!(
                "{}{}",
                match arg {
                    Some(arg) => Instruction::Push(*arg).to_string(),
                    None => ss!(),
                },
                "+".repeat(244),
            ),
            Instruction::And(arg) => format!(
                "{}>[-]<<<<<[>>>>[>+<-]<<<<-]>>>>[-]>[-<<<<<+>>>>>]<<-<<<",
                match arg {
                    Some(arg) => Instruction::Push(*arg).to_string(),
                    None => ss!(),
                }
            ),
            Instruction::Or(arg) => format!(
                "{}[<<<<[>+<[-]]>>>>[-]][-]<-<<<[-]>[-<+>]<",
                match arg {
                    Some(arg) => Instruction::Push(*arg).to_string(),
                    None => ss!(),
                }
            ),
            Instruction::Push(arg) => ss!(">>>+>") + &Instruction::Set(*arg).to_string(),
            Instruction::Str(string) => string.chars()
                .rev()
                .map(|ch| Instruction::Push(ch as u8).to_string())
                .chain(Some(Instruction::Push(string.len() as u8).to_string()))
                .collect(),
            Instruction::CStr(string) => string.chars()
                .chain(Some('\0'))
                .rev()
                .map(|ch| Instruction::Push(ch as u8).to_string())
                .collect(),
            Instruction::Set(arg) => ss!("[-]") + &"+".repeat(*arg as usize),
            Instruction::Pop(arg) => "[-]<-<<<".repeat(*arg as usize),
            Instruction::Dup(arg) => ">[-]>>+>[-]<<<<[->+>>>+<<<<]>[-<+>]>>>".repeat(*arg as usize),
            Instruction::Swp => ss!(">[-]<[->+<]<<<<[->>>>+<<<<]>>>>>[-<<<<<+>>>>>]<"),
            Instruction::Rot => format!("<<<<{}>>>>{}", Instruction::Swp, Instruction::Swp),
            Instruction::Swpb(i) => if let Some(i) = i {
                format!(
                    ">[-]<[->+<]{0}[-{1}+{0}]{1}>[-{0}<+{1}>]<",
                    "<".repeat(i * 4),
                    ">".repeat(i * 4),
                )
            } else {
                ss!(
                    ">[-]<<-<<<<->[->>>>>+<<<<<]>[-]>>>[-<<<+>>>]<<<[-[-<<<<+>>>>]<<<<]<<->[->>>[>>>>]>+<<<<<[<<<<]>];>>>[>>>>]+>>>>>>[-<<<<<<[<<<<]>+>>>[>>>>]>>]<<<<<<[<<<<]+[>>>>]<<<"
                )
            },
            Instruction::Swpn(n) => format!(
                "<->>[-]<[->+<]{0}[-{1}+{0}]{1}>[-<{0}+{1}>]<<+>",
                goto(*n),
                goback()
            ),
            Instruction::Tag(i) => if let Some(i) = i { format!(">>[-]{}<<", "+".repeat(*i as usize)) } else { ss!("[->+>+<<]>[-<+>]<") },
            Instruction::Ctg(i) => format!("<->>[-]<{0}[->+<{1}+{0}]>[-<+>]<{1}<+>", goto_tag(*i), goback()),
            Instruction::Mtg(i) => format!("{0}[-]{1}[-{0}+{1}]<-<<<", goto_tag(*i), goback()),
            Instruction::Ptg => ss!(">>[->>>+>+<<<<]>>>"),
            Instruction::If => ss!(">[-]<["),
            Instruction::Endif => ss!(">[-]<[->+<]]>[-<+>]<"),
            Instruction::Loop => ss!("["),
            Instruction::End => ss!("]"),
            Instruction::Out(arg) => match arg {
                    Some(arg) => format!(">[-]{}.<", "+".repeat(*arg as usize)),
                    None => ss!("."),
                }
            Instruction::Pout(arg) => match arg {
                    Some(arg) => format!(">[-]{}.<", "+".repeat(*arg as usize)),
                    None => ss!(".[-]<-<<<"),
                }
            Instruction::In => ss!(">>>+>[-],"),
            Instruction::Raw(code) => code.clone(),
            Instruction::Macro(ops) => ops.iter()
                    .flat_map(|op| op.to_string().chars().collect::<Vec<_>>())
                    .collect(),
        };

        write!(f, "{data}")
    }
}

fn compile(tokens: &[Instruction]) -> CompResult<String> {
    if tokens.iter().filter(|op| **op == Instruction::Loop).count()
        != tokens.iter().filter(|op| **op == Instruction::End).count()
    {
        return Err(CompError::UnbalancedLoops);
    }

    if tokens
        .iter()
        .filter(|op| matches!(op, Instruction::If))
        .count()
        != tokens
            .iter()
            .filter(|op| matches!(op, Instruction::Endif))
            .count()
    {
        return Err(CompError::UnbalancedIfs);
    }

    let mut data = ss!(">");
    for tok in tokens {
        data.push_str(&tok.to_string());
    }

    while data.contains("<>") {
        data = data.replace("<>", "");
    }

    while data.contains("><") {
        data = data.replace("><", "");
    }

    while data.contains("+-") {
        data = data.replace("+-", "");
    }

    while data.contains("-+") {
        data = data.replace("-+", "");
    }

    Ok(data)
}

fn tokenize(code: &str) -> CompResult<String> {
    let mut program = Vec::new();

    let mut macros = HashMap::new();
    let mut mac = Vec::new();
    let mut macro_name = None;

    for (i, line) in code.lines().enumerate() {
        let mut line = line.trim();

        let toks = if macro_name.is_some() {
            &mut mac
        } else {
            &mut program
        };

        if line.starts_with(';') || line.is_empty() || line.chars().all(|ch| ch.is_whitespace()) {
            continue;
        }

        if !line.starts_with("raw") {
            let mut split_point = None;
            let mut in_char = false;
            let mut in_str = false;
            for i in 0..line.len() {
                if !(in_char || in_str) {
                    match line.chars().nth(i).unwrap() {
                        '\'' => in_char = true,
                        '"' => in_str = true,
                        ';' => {
                            split_point = Some(i);
                            break;
                        }
                        _ => {}
                    }
                } else if in_char {
                    if line.chars().nth(i).unwrap() == '\'' {
                        in_char = false;
                    }
                } else if line.chars().nth(i).unwrap() == '"' {
                    in_str = false;
                }
            }

            if let Some(i) = split_point {
                line = line.split_at(i).0;
            }
        }

        let (op, mut arg) = line.split_once(' ').unwrap_or((line, ""));
        let op = op.trim().to_lowercase();
        arg = arg.trim();

        let fmt;
        if let Some(ch) = arg.strip_prefix('\'').and_then(|s| s.strip_suffix('\'')) {
            if ch.len() != 1 && ch != "\\n" {
                return Err(CompError::InvalidChar(i, arg.to_string()));
            }

            if ch == "\\n" {
                arg = "10";
            } else {
                fmt = format!("{}", ch.as_bytes()[0]);
                arg = &fmt;
            }
        } else if let Some(string) = arg.strip_prefix('"').and_then(|s| s.strip_suffix('"')) {
            fmt = string.replace("\\n", "\n");
            arg = &fmt;
        } else if arg.contains('\'') {
            return Err(CompError::InvalidChar(i, arg.to_string()));
        } else if arg.contains('"') {
            return Err(CompError::InvalidString(i, arg.to_string()));
        }

        match op.as_str() {
            "add" => toks.push(Instruction::Add(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "sub" => toks.push(Instruction::Sub(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "mul" => toks.push(Instruction::Mul(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "div" => toks.push(Instruction::Div(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "eq" => toks.push(Instruction::Eq(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "neq" => toks.push(Instruction::Neq(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "gt" => toks.push(Instruction::Gt(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "lt" => toks.push(Instruction::Lt(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "not" => toks.push(Instruction::Not(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "and" => toks.push(Instruction::And(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "or" => toks.push(Instruction::Or(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "push" => toks.push(Instruction::Push(
                arg.parse()
                    .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
            )),
            "str" => toks.push(Instruction::Str(arg.to_string())),
            "cstr" => toks.push(Instruction::CStr(arg.to_string())),
            "set" => toks.push(Instruction::Set(
                arg.parse()
                    .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
            )),
            "pop" => toks.push(Instruction::Pop(if arg.is_empty() {
                1
            } else {
                arg.parse()
                    .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?
            })),
            "dup" => toks.push(Instruction::Dup(if arg.is_empty() {
                1
            } else {
                arg.parse()
                    .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?
            })),
            "swp" => toks.push(Instruction::Swp),
            "swpb" => toks.push(Instruction::Swpb(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "swpn" => toks.push(Instruction::Swpn(
                arg.parse()
                    .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
            )),
            "tag" => toks.push(Instruction::Tag(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "ctg" => toks.push(Instruction::Ctg(
                arg.parse()
                    .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
            )),
            "mtg" => toks.push(Instruction::Mtg(
                arg.parse()
                    .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
            )),
            "ptg" => toks.push(Instruction::Ptg),
            "rot" => toks.push(Instruction::Rot),
            "if" => toks.push(Instruction::If),
            "endif" => toks.push(Instruction::Endif),
            "loop" => toks.push(Instruction::Loop),
            "end" => toks.push(Instruction::End),
            "out" => toks.push(Instruction::Out(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "pout" => toks.push(Instruction::Pout(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "in" => toks.push(Instruction::In),
            "raw" => toks.push(Instruction::Raw(arg.to_owned())),

            "def" => {
                let arg = arg.to_string();
                if arg.is_empty() | arg.chars().all(char::is_whitespace) {
                    return Err(CompError::InvalidMacro(i, arg));
                }
                macro_name = Some(arg);
            }
            "endef" => {
                let name = macro_name
                    .take()
                    .expect("Internal error: no name for macro");
                let new = std::mem::take(&mut mac);
                macros.insert(name, Instruction::Macro(new));
            }

            op => {
                if let Some(body) = macros.get(op) {
                    toks.push(body.clone());
                } else {
                    return Err(CompError::InvalidOp(i, op.to_owned()));
                }
            }
        }
    }

    compile(&program)
}

fn print_err(msg: impl Display, e: impl Display) -> ! {
    eprintln!("{}[38;5;1m{msg}: {e}{0}[0m", 27 as char);
    eprintln!("{}[38;5;1maborted due to error{0}[0m", 27 as char);
    exit(1);
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

sarge! {
    Args,

    'h' help: bool,
    's' stdout: bool,
    #ok 'o' output: String,
}

fn main() {
    let (args, mut input) = match Args::parse() {
        Ok(i) => i,
        Err(e) => {
            print_err("failed to parse arguments: ", e);
        }
    };

    if args.help {
        print_help();
        return;
    }

    if args.stdout && args.output.is_some() {
        print_err(
            "invalid options",
            "you may only specifiy one of --output or --stdout",
        );
    }

    input.remove(0);
    if input.is_empty() {
        print_help();
        exit(1);
    }

    let default_name = if input.len() == 1 {
        Path::new(&input[0])
            .with_extension("bf")
            .display()
            .to_string()
    } else {
        "out.bf".to_string()
    };

    let output_files = args
        .output
        .map(|v| v.split(',').map(String::from).collect::<Vec<_>>())
        .unwrap_or_else(|| vec![default_name]);

    if output_files.len() != input.len() && output_files.len() != 1 {
        print_err(
            "invalid arguments",
            "must have equal amounts of input and output files, or 1 output file",
        );
    }

    let mut compiled = Vec::new();
    if output_files.len() != 1 {
        for file in input {
            let code = match read_to_string(&file) {
                Ok(c) => c,
                Err(e) => {
                    print_err(format!("failed to read file {file}"), e);
                }
            };

            print_warn(format!("compiling {}", file));
            compiled.push(match tokenize(&code) {
                Ok(c) => c,
                Err(e) => {
                    print_err(format!("failed to parse file {file}"), e);
                }
            });
        }
    } else {
        let mut code = String::new();
        for filename in input {
            match File::open(&filename) {
                Ok(mut file) => {
                    if let Err(e) = file.read_to_string(&mut code) {
                        print_err("failed to read file {filename}", e);
                    }
                }
                Err(e) => {
                    print_err(format!("failed to open file {filename}"), e);
                }
            }
        }

        print_warn("compiling all files");
        compiled.push(match tokenize(&code) {
            Ok(c) => c,
            Err(e) => {
                print_err("failed to parse files", e);
            }
        });
    }

    if !args.stdout {
        if output_files.len() == 1 {
            print_warn(format!("writing to {}", output_files[0]));
            let mut file = match File::create(&output_files[0]) {
                Ok(f) => f,
                Err(e) => {
                    print_err("failed to open output file", e);
                }
            };

            for output in compiled {
                if let Err(e) = file.write_all(output.as_bytes()) {
                    print_err("failed to write to file", e);
                }
            }
        } else {
            for (i, filename) in output_files.iter().enumerate() {
                print_warn(format!("writing to {}", filename));
                let mut file = match File::create(filename) {
                    Ok(f) => f,
                    Err(e) => {
                        print_err("failed to open output file", e);
                    }
                };

                if let Err(e) = file.write_all(compiled[i].as_bytes()) {
                    print_err("failed to write to file", e);
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
