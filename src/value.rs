use closure::Closure;

use std::collections::BTreeMap;
use std::rc::Rc;
use std::cell::RefCell;
use num::rational::BigRational;

#[derive(Clone, Eq, Debug, PartialEq, PartialOrd, Ord)]
pub enum Value {
    Number(BigRational),
    CharString(String),
    Boolean(bool),
    Map(Rc<RefCell<BTreeMap<Value, Value>>>),
    Closure(Rc<Box<Vec<String>>>, Rc<Closure>),
}
