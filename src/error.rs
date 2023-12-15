use thiserror::Error;

#[derive(Debug, Clone, Error)]
#[allow(unused)]
#[non_exhaustive]
pub enum CompError {
    #[error("Invalid instruction (line {0}): `{1}`")]
    InvalidOp(usize, String),
    #[error("Invalid character (line {0}): `{1}`")]
    InvalidChar(usize, String),
    #[error("Invalid string (line {0}): `{1}`")]
    InvalidString(usize, String),
    #[error("Invalid argument (line {0}): `{1}`")]
    InvalidArgument(usize, String),
    #[error("Invalid macro (line {0}): `{1}`")]
    InvalidMacro(usize, String),
    #[error("Instruction `{1}` requires an argument (line {0})")]
    RequiresArg(usize, String),
    #[error("`if/ifn`s and `endif`s aren't balanced")]
    UnbalancedIfs,
    #[error("`loop`s and `end`s aren't balanced")]
    UnbalancedLoops,
}

pub type CompResult<T> = Result<T, CompError>;
