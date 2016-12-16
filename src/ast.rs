use std::collections::BTreeMap;
use num::rational::{BigRational};

#[derive(Clone, Eq, Debug, Hash, Ord, PartialEq, PartialOrd)]
pub enum Value {
    // Nil,
    // Bool(bool),
    Number(BigRational),
    CharString(String),
    // Vec(Vec<Value>),
    // HashMap(BTreeMap<Rc<Value>, Rc<Value>>),
    // Fn(Box<(String, Vec<(String, Type)>, Vec<Expression>)>),

    // PrimitiveFn(fn(Vec<Rc<Value>>) -> Value),
}

#[derive(Clone, Eq, Debug, Hash, Ord, PartialEq, PartialOrd)]
pub enum Statement {
    Assign(bool, String, Box<Expression>),
}

#[derive(Clone, Eq, Debug, Hash, Ord, PartialEq, PartialOrd)]
pub enum Expression {
    BinOp(String, Box<Expression>, Box<Expression>),
    Value(Value),
}
