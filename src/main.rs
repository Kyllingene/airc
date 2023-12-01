// TODO: support more kinds of brain* interpreters
use std::collections::HashMap;
use std::error::Error;
use std::fmt::Display;
use std::path::Path;
use std::fs::{read_to_string, File};
use std::io::{Read, Write};
use std::process::exit;

use sarge::prelude::*;

// TODO: implement pact
// mod pact;

type CompResult<T> = Result<T, CompError>;

macro_rules! ss {
    ($s:expr) => {
        String::from($s)
    };

    () => {
        String::new()
    };
}

#[derive(Debug, Clone)]
#[allow(unused)]
#[non_exhaustive]
enum CompError {
    InvalidOp(usize, String),
    InvalidChar(usize, String),
    InvalidString(usize, String),
    InvalidArgument(usize, String),
    InvalidMacro(usize, String),
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
            CompError::InvalidArgument(i, ch) => write!(f, "Invalid argument on line {i}: `{ch}`"),
            CompError::RequiresArg(i, op) => {
                write!(f, "Operator `{op}` on line {i} requires an argument")
            }
            CompError::InvalidMacro(i, name) => {
                write!(f, "Invalid macro name on line {i}: `{name}`")
            }
            CompError::UnbalancedIfs => write!(f, "Unequal numbers of `if` and `endif`"),
            CompError::UnbalancedLoops => write!(f, "Unequal numbers of `loop` and `end`"),
        }
    }
}

impl Error for CompError {}

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
    Macro(Vec<Op>),
}

impl Display for Op {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let data = match self {
            Op::Add(arg) => if let Some(arg) = arg {
                "+".repeat(*arg as usize)
            } else {
                ss!("[-<<<<+>>>>]<-<<<")
            }
            Op::Sub(arg) => if let Some(arg) = arg {
                "-".repeat(*arg as usize)
            } else {
                ss!("[-<<<<->>>>]<-<<<")
            }
            Op::Mul(arg) => format!(
                "{}>[-]<<<<[-]<[>>>>>+<<<<<-]>>>>>[<[<<<<+>+>>>-]<<<[>>>+<<<-]>>>>-]<[-]<-<<<",
                match arg {
                    Some(arg) => Op::Push(*arg).to_string(),
                    None => ss!(),
                }
            ),
            Op::Div(arg) => format!("{}>[-]<<-<<[-]<<->[>+<-]>[>>>[<<<<<+>>>>>>+<-]>[<+>-]<<<<<<[>>>>>>+<<<<-[>>>>[-]<<+<<-]>>[<<+>>-]>>[<<<<<<-[>-<[-]]+>>>>>>-]<<<<<<-]>+>]<<[-]+>>>>[-]>[-]<<<<",
                match arg {
                    Some(arg) => Op::Push(*arg).to_string(),
                    None => ss!(),
                }
            ),
            Op::Eq(arg) => format!("{}<<<<[->>>>-<<<<]+>>>>[<<<<->>>>[-]]<-<<<", match arg {
                Some(arg) => Op::Push(*arg).to_string(),
                None => ss!(),
            }),
            Op::Neq(arg) => format!(
                "{}<<<[-]>>>>[-]<<<<<[>>>>>+<<<<<-]>>>>[>-<<<<+>>>-]<<<[>>>+<<<-]>>>>[<<<<<+>>>>>[-]]<<-<<<",
                match arg {
                    Some(arg) => Op::Push(*arg).to_string(),
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
            Op::Gt(arg) => format!(
                "{}ty> [-] tx<<<< [-] ux<< -
                x> [tx> +
                    y>>> [- tx<<< [-] ty>>>> + y<]
                tx<<< [- ux<< + tx>>]
                ty>>>> [- y< + ty>]
                y< - x<<<< - ; ]

                ux< [- x> + ux< ]+ x>",
                match arg {
                    Some(arg) => Op::Push(*arg).to_string(),
                    None => ss!(),
                }
            ),

            Op::Lt(arg) => format!(
                "{}>[-]<[->+<]<<<<[->>>>+<<<<]>>>>>[-<<<<<+>>>>>][-]<<<<[-]<<[-]>[>+>>>[-<<<[-]>>>>+<]<<<[-<<+>>]>>>>[-<+>]<-<<<<-]<[->+<]+>",
                match arg {
                    Some(arg) => Op::Push(*arg).to_string(),
                    None => ss!(),
                },
            ),
            Op::Not(arg) => format!(
                "{}>[-]<-[>-<-]>[<+>-]<",
                match arg {
                    Some(arg) => Op::Push(*arg).to_string(),
                    None => ss!(),
                }
            ),
            Op::And(arg) => format!(
                "{}>[-]<<<<<[>>>>[>+<-]<<<<-]>>>>[-]>[-<<<<<+>>>>>]<<-<<<",
                match arg {
                    Some(arg) => Op::Push(*arg).to_string(),
                    None => ss!(),
                }
            ),
            Op::Or(arg) => format!(
                "{}[<<<<[>+<[-]]>>>>[-]][-]<-<<<[-]>[-<+>]<",
                match arg {
                    Some(arg) => Op::Push(*arg).to_string(),
                    None => ss!(),
                }
            ),
            Op::Push(arg) => ss!(">>>+>") + &Op::Set(*arg).to_string(),
            Op::Str(string) => string.chars()
                .rev()
                .map(|ch| Op::Push(ch as u8).to_string())
                .chain(Some(Op::Push(string.len() as u8).to_string()))
                .collect(),
            Op::CStr(string) => string.chars()
                .chain(Some('\0'))
                .rev()
                .map(|ch| Op::Push(ch as u8).to_string())
                .collect(),
            Op::Set(arg) => ss!("[-]") + &"+".repeat(*arg as usize),
            Op::Pop(arg) => "[-]<-<<<".repeat(*arg as usize),
            Op::Dup(arg) => ">[-]>>+>[-]<<<<[->+>>>+<<<<]>[-<+>]>>>".repeat(*arg as usize),
            Op::Swp => ss!(">[-]<[->+<]<<<<[->>>>+<<<<]>>>>>[-<<<<<+>>>>>]<"),
            Op::Rot => format!("<<<<{}>>>>{}", Op::Swp, Op::Swp),
            Op::Swpb(i) => if let Some(i) = i {
                    format!(
                    "[->+<]{0}[-{1}+{0}]{1}>[-{0}<+{1}>]<",
                    "<".repeat(i * 4),
                    ">".repeat(i * 4),
                )
            } else {
                ss!(
                    "<->[->+<]>[-<<<<[-]>>>>[-<<<<+>>>>]<<<<]<<->[->+>>[>>>>]>+<<<<<[<<<<]>]>[-<+>]<<+>>>>[>>>>]+>"
                )
            },
            Op::Swpn(n) => format!(
                "<->>[-]<[->+<]{0}[-{1}+{0}]{1}>[-<{0}+{1}>]<<+>",
                goto(*n),
                goback()
            ),
            Op::Tag(i) => if let Some(i) = i { format!(">>{}<<", Op::Set(*i)) } else { ss!("[->+>+<<]>[-<+>]<") },
            Op::Ctg(i) => format!("<->>[-]<[->+<]{0}[-{1}+{0}]{1}>[-<{0}+{1}>]<<+>", goto_tag(*i), goback()),
            Op::Mtg(i) => format!("{0}[-]{1}[-{0}+{1}]<-<<<", goto_tag(*i), goback()),
            Op::Ptg => ss!(">>[->>>+>+<<<<]>>>"),
            Op::If => ss!(">[-]<["),
            Op::Ifn => Op::Not(None).to_string() + &Op::If.to_string(),
            Op::Endif => ss!(">[-]<[->+<]]>[-<+>]<"),
            Op::Loop => format!("[{}", Op::Pop(1)),
            Op::End => format!("]{}", Op::Pop(1)),
            Op::Out(arg) => match arg {
                    Some(arg) => format!(">[-]{}.<", "+".repeat(*arg as usize)),
                    None => ss!("."),
                }
            Op::Pout(arg) => match arg {
                    Some(arg) => format!(">[-]{}.<", "+".repeat(*arg as usize)),
                    None => ss!(".[-]<-<<<"),
                }
            Op::In => ss!(">>>+>[-],"),
            Op::Raw(code) => code.clone(),
            Op::Macro(ops) => ops.iter()
                    .flat_map(|op| op.to_string().chars().collect::<Vec<_>>())
                    .collect(),
        };

        write!(f, "{data}")
    }
}

fn compile(tokens: &[Op]) -> CompResult<String> {
    if tokens.iter().filter(|op| **op == Op::Loop).count()
        != tokens.iter().filter(|op| **op == Op::End).count()
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
            "add" => toks.push(Op::Add(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "sub" => toks.push(Op::Sub(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "mul" => toks.push(Op::Mul(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "div" => toks.push(Op::Div(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "eq" => toks.push(Op::Eq(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "neq" => toks.push(Op::Neq(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "gt" => toks.push(Op::Gt(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "lt" => toks.push(Op::Lt(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "not" => toks.push(Op::Not(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "and" => toks.push(Op::And(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "or" => toks.push(Op::Or(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "push" => toks.push(Op::Push(
                arg.parse()
                    .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
            )),
            "str" => toks.push(Op::Str(arg.to_string())),
            "cstr" => toks.push(Op::CStr(arg.to_string())),
            "set" => toks.push(Op::Set(
                arg.parse()
                    .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
            )),
            "pop" => toks.push(Op::Pop(if arg.is_empty() {
                1
            } else {
                arg.parse()
                    .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?
            })),
            "dup" => toks.push(Op::Dup(if arg.is_empty() {
                1
            } else {
                arg.parse()
                    .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?
            })),
            "swp" => toks.push(Op::Swp),
            "swpb" => toks.push(Op::Swpb(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "swpn" => toks.push(Op::Swpn(
                arg.parse()
                    .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
            )),
            "tag" => toks.push(Op::Tag(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "ctg" => toks.push(Op::Ctg(
                arg.parse()
                    .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
            )),
            "mtg" => toks.push(Op::Mtg(
                arg.parse()
                    .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
            )),
            "ptg" => toks.push(Op::Ptg),
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
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "pout" => toks.push(Op::Pout(if arg.is_empty() {
                None
            } else {
                Some(
                    arg.parse()
                        .map_err(|_| CompError::InvalidArgument(i, arg.to_owned()))?,
                )
            })),
            "in" => toks.push(Op::In),
            "raw" => toks.push(Op::Raw(arg.to_owned())),

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
                macros.insert(name, Op::Macro(new));
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
        Path::new(&input[0]).with_extension("bf").display().to_string()
    } else {
        "out.bf".to_string()
    };

    let output_files = args.output
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
