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
    None,
}

pub type BinopResult = Result<Value, String>;

impl Value {
    pub fn sub(&self, other: Value) -> BinopResult {
        match (self, other) {
            (&Value::Number(ref lratio), Value::Number(ref rratio)) => {
                Ok(Value::Number(lratio - rratio))
            }
            (&Value::CharString(ref lstr), Value::CharString(ref rstr)) => {
                if let Some(index) = lstr.rfind(rstr) {
                    Ok(Value::CharString(lstr[0..index].to_owned() + &lstr[(index + rstr.len())..]))
                } else {
                    Ok(Value::CharString("".to_owned()))
                }
            }
            (&Value::Closure(_, _), Value::Closure(_, _)) => {
                Err(format!("Subtraction of closures is not supported"))
            }
            (&Value::Boolean(ref lbool), Value::Boolean(ref rbool)) => {
                Ok(Value::Boolean(lbool ^ rbool))
            }
            (&Value::Map(ref lmap), Value::Map(ref rmap)) => {
                let result = lmap.borrow()
                    .clone()
                    .into_iter()
                    .filter(|&(ref key, ref value)| if let Some(rvalue) = rmap.borrow().get(key) {
                        rvalue != value
                    } else {
                        true
                    })
                    .collect();
                Ok(Value::Map(Rc::new(RefCell::new(result))))
            }
            (l, r) => Err(format!("Unsupported operation - for {:?} and {:?}", l, r)),
        }
    }
}

#[cfg(test)]
mod test {
    use test_helpers::*;

    #[test]
    fn sub() {
        // Numbers
        assert_eq!(Ok(v_number(15, 2)), v_number(8, 1).sub(v_number(1, 2)));
        assert_err!(v_number(8, 1).sub(v_string("toto")));
        // Strings
        assert_eq!(Ok(v_string("hello ")),
                   v_string("hello world").sub(v_string("world")));
        assert_eq!(Ok(v_string("helld")),
                   v_string("hello world").sub(v_string("lo wor")));
        assert_eq!(Ok(v_string("hello world")),
                   v_string("hello world").sub(v_string("")));
        assert_err!(v_string("toto").sub(v_number(1, 1)));
        // Boolean
        assert_eq!(Ok(v_bool(false)), v_bool(true).sub(v_bool(true)));
        assert_eq!(Ok(v_bool(true)), v_bool(true).sub(v_bool(false)));
        assert_eq!(Ok(v_bool(true)), v_bool(false).sub(v_bool(true)));
        assert_eq!(Ok(v_bool(false)), v_bool(false).sub(v_bool(false)));
        assert_err!(v_bool(false).sub(v_number(1, 1)));
        // Map
        assert_eq!(Ok(v_map(vec![(v_number(1, 1), v_number(1, 1))])),
                   v_map(vec![(v_number(1, 1), v_number(1, 1)), (v_number(2, 1), v_number(2, 1))])
                       .sub(v_map(vec![(v_number(2, 1), v_number(2, 1))])));
        // Map
        assert_eq!(Ok(v_map(vec![(v_number(1, 1), v_number(1, 1)),
                                 (v_number(2, 1), v_number(2, 1))])),
                   v_map(vec![(v_number(1, 1), v_number(1, 1)), (v_number(2, 1), v_number(2, 1))])
                       .sub(v_map(vec![(v_number(2, 1), v_number(2, 2))])));
        assert_err!(v_map(vec![(v_number(1, 1), v_number(1, 1))]).sub(v_number(1, 1)));
    }
}
