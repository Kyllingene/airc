//! A module for compiling airc to pact assembly.
//! 
//! This is because airc assembly is infinitely better.

use crate::Op;

// memory layout:  0-256: stack

fn inc_sp() -> &'static str {
    r#"
    ioi cpu 3

    "#
}

impl Op {
    fn to_pact(self) -> String {
        match self {
            Op::Add(_) => todo!(),
            Op::Sub(_) => todo!(),
            Op::Mul(_) => todo!(),
            Op::Div(_) => todo!(),
            Op::Eq(_) => todo!(),
            Op::Neq(_) => todo!(),
            Op::Gt(_) => todo!(),
            Op::Lt(_) => todo!(),
            Op::Not(_) => todo!(),
            Op::And(_) => todo!(),
            Op::Or(_) => todo!(),
            Op::Push(_) => todo!(),
            Op::Set(_) => todo!(),
            Op::Pop(_) => todo!(),
            Op::Dup(_) => todo!(),
            Op::Swp => todo!(),
            Op::Rot => todo!(),
            Op::Swpb(_) => todo!(),
            Op::Swpn(_) => todo!(),
            Op::Tag(_) => todo!(),
            Op::Ctg(_) => todo!(),
            Op::Mtg(_) => todo!(),
            Op::Ptg => todo!(),
            Op::If => todo!(),
            Op::Ifn => todo!(),
            Op::Endif => todo!(),
            Op::Loop => todo!(),
            Op::End => todo!(),
            Op::Out(_) => todo!(),
            Op::Pout(_) => todo!(),
            Op::In => todo!(),
            Op::Raw(_) => todo!(),
            Op::Macro(_) => todo!(),
        }
    }
}
