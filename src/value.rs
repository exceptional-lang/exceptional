use closure::Closure;

use std::collections::BTreeMap;
use std::rc::Rc;
use std::cell::RefCell;
use num::rational::{BigRational, Ratio};
use num::{range, ToPrimitive, Zero};
use num::bigint::{BigInt, ToBigInt};

#[derive(Clone, Eq, Debug, PartialEq, PartialOrd, Ord)]
pub enum Value {
    Number(BigRational),
    CharString(String),
    Boolean(bool),
    Map(Rc<RefCell<BTreeMap<Value, Value>>>),
    Closure(Rc<Box<Vec<String>>>, Rc<Closure>),
}

type BinopResult = Result<Value, String>;

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

    pub fn mul(&self, right: Value) -> BinopResult {
        match (self, right) {
            (&Value::Number(ref lratio), Value::Number(ref rratio)) => {
                Ok(Value::Number(lratio * rratio))
            }
            (&Value::CharString(ref str), Value::Number(ref ratio)) => {
                let extended: String = range(BigInt::from(0), ratio.ceil().to_integer())
                    .map(|_| str.clone())
                    .collect::<Vec<String>>()
                    .join("");
                let truncation = (ratio.ceil() - ratio.clone()) *
                                 Ratio::from_integer((str.len() as i64).to_bigint().unwrap());
                let truncation_index = extended.len().to_bigint().unwrap() -
                                       truncation.to_integer();
                Ok(Value::CharString(extended[..truncation_index.to_usize().unwrap()].to_owned()))
            }
            (&Value::Boolean(ref lbool), Value::Boolean(ref rbool)) => {
                Ok(Value::Boolean(lbool & rbool))
            }
            (l, r) => Err(format!("Unsupported operation * for {:?} and {:?}", l, r)),
        }
    }

    pub fn div(&self, right: Value) -> BinopResult {
        match (self, right) {
            (&Value::Number(ref lratio), Value::Number(ref rratio)) => {
                if rratio.is_zero() {
                    Err("Can't divide by zero".to_owned())
                } else {
                    Ok(Value::Number(lratio / rratio))
                }
            }
            (&Value::CharString(_), Value::Number(ref ratio)) => {
                if ratio.is_zero() {
                    Err("Can't divide by zero".to_owned())
                } else {
                    self.mul(Value::Number(Ratio::from_integer(BigInt::from(1)) / ratio))
                }
            }

            (l, r) => Err(format!("Unsupported operation / for {:?} and {:?}", l, r)),
        }
    }

    pub fn add(&self, right: Value) -> BinopResult {
        match (self, right) {
            (&Value::Number(ref lratio), Value::Number(ref rratio)) => {
                Ok(Value::Number(lratio + rratio))
            }
            (&Value::CharString(ref lstr), Value::CharString(ref rstr)) => {
                Ok(Value::CharString((*lstr).clone() + rstr))
            }
            (&Value::Closure(_, _), Value::Closure(_, _)) => {
                Err("Addition of closures is not supported".to_owned())
            }
            (&Value::Boolean(lbool), Value::Boolean(rbool)) => Ok(Value::Boolean(lbool || rbool)),
            (&Value::Map(ref lmap), Value::Map(ref rmap)) => {
                let mut result = (*lmap.borrow()).clone();
                let mut rclone = (*rmap.borrow()).clone();
                result.append(&mut rclone);
                Ok(Value::Map(Rc::new(RefCell::new(result))))
            }
            (l, r) => Err(format!("Unsupported operation + for {:?} and {:?}", l, r)),
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

    #[test]
    fn mul() {
        // Numbers
        assert_eq!(Ok(v_number(4, 1)), v_number(8, 1).mul(v_number(1, 2)));
        assert_err!(v_number(8, 1).mul(v_string("toto")));
        // Strings
        assert_err!(v_string("hello ").mul(v_string("world")));
        assert_eq!(Ok(v_string("totototo")), v_string("to").mul(v_number(4, 1)));
        assert_eq!(Ok(v_string("tototot")), v_string("to").mul(v_number(7, 2)));
        assert_eq!(Ok(v_string("tot")), v_string("toto").mul(v_number(3, 4)));
        assert_eq!(Ok(v_string("")), v_string("toto").mul(v_number(0, 1)));
        // Boolean
        assert_eq!(Ok(v_bool(true)), v_bool(true).mul(v_bool(true)));
        assert_eq!(Ok(v_bool(false)), v_bool(true).mul(v_bool(false)));
        assert_eq!(Ok(v_bool(false)), v_bool(false).mul(v_bool(true)));
        assert_eq!(Ok(v_bool(false)), v_bool(false).mul(v_bool(false)));
        assert_err!(v_bool(false).mul(v_number(1, 1)));
        // Map can't be multiplied?
        assert_err!(v_map(vec![]).mul(v_map(vec![])));
    }

    #[test]
    fn div() {
        // Numbers
        assert_eq!(Ok(v_number(16, 1)), v_number(8, 1).div(v_number(1, 2)));
        assert_err!(v_number(8, 1).div(v_number(0, 1)));
        assert_err!(v_number(8, 1).div(v_string("toto")));
        // Strings
        assert_err!(v_string("hello ").div(v_string("world")));
        assert_eq!(Ok(v_string("t")), v_string("to").div(v_number(2, 1)));
        assert_eq!(Ok(v_string("tototot")), v_string("to").div(v_number(2, 7)));
        assert_eq!(Ok(v_string("tot")), v_string("toto").div(v_number(4, 3)));
        assert_err!(v_string("toto").div(v_number(0, 1)));
        // Boolean can't be divided
        assert_err!(v_bool(true).div(v_bool(true)));
        // Map can't be div?
        assert_err!(v_map(vec![]).div(v_map(vec![])));
    }

    #[test]
    fn add() {
        // Numbers
        assert_eq!(Ok(v_number(17, 2)), v_number(8, 1).add(v_number(1, 2)));
        assert_err!(v_number(8, 1).add(v_string("toto")));
        // Strings
        assert_eq!(Ok(v_string("hello world")),
                   v_string("hello ").add(v_string("world")));
        assert_err!(v_string("toto").add(v_number(1, 1)));
        // Boolean
        assert_eq!(Ok(v_bool(true)), v_bool(true).add(v_bool(true)));
        assert_eq!(Ok(v_bool(true)), v_bool(true).add(v_bool(false)));
        assert_eq!(Ok(v_bool(true)), v_bool(false).add(v_bool(true)));
        assert_eq!(Ok(v_bool(false)), v_bool(false).add(v_bool(false)));
        assert_err!(v_bool(false).add(v_number(1, 1)));
        // Map
        assert_eq!(Ok(v_map(vec![(v_number(1, 1), v_number(1, 1)),
                                 (v_number(2, 1), v_number(2, 1))])),
                   v_map(vec![(v_number(1, 1), v_number(1, 1))])
                       .add(v_map(vec![(v_number(2, 1), v_number(2, 1))])));
        assert_err!(v_map(vec![(v_number(1, 1), v_number(1, 1))]).add(v_number(1, 1)));
    }
}
