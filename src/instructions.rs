use ast::{Literal, Pattern};
use std::cmp::Ordering;
use std::fmt;
use std::mem::transmute;
use std::rc::Rc;
use vm::Vm;

#[derive(Clone, Eq, Debug, PartialEq, PartialOrd, Ord)]
pub enum Op {
    Mul,
    Div,
    Add,
    Sub,
}

pub type NativeCode = *const fn(&mut Vm) -> InstructionSequence;

#[derive(Clone)]
pub struct NativeFunction {
    function: NativeCode,
}

impl NativeFunction {
    pub fn new(f: NativeCode) -> Self {
        NativeFunction { function: f }
    }

    pub fn call(&self, vm: &mut Vm) -> InstructionSequence {
        unsafe {
            let func = transmute::<NativeCode, fn(&mut Vm) -> InstructionSequence>(self.function);
            func(vm)
        }
    }
}

impl PartialEq for NativeFunction {
    fn eq(&self, other: &NativeFunction) -> bool {
        self.function as *const _ == other.function as *const _
    }
}

impl Eq for NativeFunction {}

impl Ord for NativeFunction {
    fn cmp(&self, other: &NativeFunction) -> Ordering {
        (self.function as *const _).cmp(&(other.function as *const _))
    }
}

impl PartialOrd for NativeFunction {
    fn partial_cmp(&self, other: &NativeFunction) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl fmt::Debug for NativeFunction {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "NativeFunction")
    }
}

#[derive(Clone, Eq, Debug, PartialEq, PartialOrd, Ord)]
pub enum Instruction {
    Clear,
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
    Import,
    Native(NativeFunction),
    Nop,
}

pub type InstructionSequence = Vec<Instruction>;

#[cfg(test)]
mod test {
    use super::*;

    fn mock(_: &mut Vm) -> InstructionSequence {
        vec![]
    }
    fn other_mock(_: &mut Vm) -> InstructionSequence {
        vec![]
    }

    #[test]
    fn compare_native_functions() {
        assert_eq!(
            NativeFunction::new(mock as NativeCode),
            NativeFunction::new(mock as NativeCode)
        );
        assert!(
            NativeFunction::new(mock as NativeCode)
                != NativeFunction::new(other_mock as NativeCode)
        );
    }

    #[test]
    fn call_native_function() {
        let mut vm = Vm::new(&"");
        assert_eq!(
            vec![] as InstructionSequence,
            NativeFunction::new(mock as NativeCode).call(&mut vm)
        );
    }
}
