use ast::{Literal, Pattern};
use std::rc::Rc;

#[derive(Clone, Eq, Debug, PartialEq, PartialOrd, Ord)]
pub enum Op {
    Mul,
    Div,
    Add,
    Sub,
}

#[derive(Clone, Eq, Debug, PartialEq, PartialOrd, Ord)]
pub enum Instruction {
    Push(Literal),
    Fetch(String),
    LocalAssign(String),
    Assign(String),
    Call(usize),
    MakeMap(usize),
    Rescue(Rc<Pattern>, Rc<InstructionSequence>),
    IndexAccess,
    IndexAssign,
    Raise,
    BinOp(Op),
    Import(String),
    Nop,
}

pub type InstructionSequence = Vec<Instruction>;
