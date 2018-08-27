use closure::Closure;

use num::bigint::{BigInt, ToBigInt};
use num::rational::{BigRational, Ratio};
use num::{range, One, ToPrimitive, Zero};
use std::cell::RefCell;
use std::collections::BTreeMap;
use std::rc::Rc;

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
                    Ok(Value::CharString(
                        lstr[0..index].to_owned() + &lstr[(index + rstr.len())..],
                    ))
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
                let result = lmap
                    .borrow()
                    .clone()
                    .into_iter()
                    .filter(|&(ref key, ref value)| {
                        if let Some(rvalue) = rmap.borrow().get(key) {
                            rvalue != value
                        } else {
                            true
                        }
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
            (&Value::CharString(ref str), Value::Number(ref ratio))
            | (&Value::Number(ref ratio), Value::CharString(ref str)) => {
                let extended: String = range(BigInt::from(0), ratio.ceil().to_integer())
                    .map(|_| str.clone())
                    .collect::<Vec<String>>()
                    .join("");
                let truncation = (ratio.ceil() - ratio.clone())
                    * Ratio::from_integer((str.len() as i64).to_bigint().unwrap());
                let truncation_index =
                    extended.len().to_bigint().unwrap() - truncation.to_integer();
                Ok(Value::CharString(
                    extended[..truncation_index.to_usize().unwrap()].to_owned(),
                ))
            }
            (&Value::Map(ref map), Value::Number(ref ratio))
            | (&Value::Number(ref ratio), Value::Map(ref map)) => {
                let result = map
                    .borrow()
                    .clone()
                    .into_iter()
                    .map(|(ref key, ref value)| {
                        (
                            key.mul(Value::Number(ratio.clone()))
                                .expect("everything is multipliable by a number"),
                            value
                                .mul(Value::Number(ratio.clone()))
                                .expect("everything is multipliable by a number"),
                        )
                    })
                    .collect();

                Ok(Value::Map(Rc::new(RefCell::new(result))))
            }
            (&Value::Boolean(ref boolean), Value::Number(ref ratio))
            | (&Value::Number(ref ratio), Value::Boolean(ref boolean)) => match boolean {
                &true => Ok(Value::Number(ratio.clone())),
                &false => Ok(Value::Number(Ratio::new(BigInt::zero(), BigInt::one()))),
            },
            (&Value::CharString(ref str), Value::Boolean(ref boolean))
            | (&Value::Boolean(ref boolean), Value::CharString(ref str)) => match boolean {
                &true => Ok(Value::CharString(str.clone())),
                &false => Ok(Value::CharString("".to_owned())),
            },
            (&Value::Boolean(ref lbool), Value::Boolean(ref rbool)) => {
                Ok(Value::Boolean(lbool & rbool))
            }
            (&Value::Boolean(ref boolean), Value::Map(ref map))
            | (&Value::Map(ref map), Value::Boolean(ref boolean)) => {
                let result = map
                    .borrow()
                    .clone()
                    .into_iter()
                    .map(|(ref key, ref value)| {
                        (
                            key.mul(Value::Boolean(*boolean))
                                .expect("everything is multipliable by a boolean"),
                            value
                                .mul(Value::Boolean(*boolean))
                                .expect("everything is multipliable by a boolean"),
                        )
                    })
                    .collect();

                Ok(Value::Map(Rc::new(RefCell::new(result))))
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

    pub fn val_eq(&self, right: &Value) -> BinopResult {
        Ok(Value::Boolean(self == right))
    }

    pub fn val_gt(&self, right: &Value) -> BinopResult {
        Ok(Value::Boolean(self > right))
    }

    pub fn val_lt(&self, right: &Value) -> BinopResult {
        Ok(Value::Boolean(self < right))
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
        assert_eq!(
            Ok(v_string("hello ")),
            v_string("hello world").sub(v_string("world"))
        );
        assert_eq!(
            Ok(v_string("helld")),
            v_string("hello world").sub(v_string("lo wor"))
        );
        assert_eq!(
            Ok(v_string("hello world")),
            v_string("hello world").sub(v_string(""))
        );
        assert_err!(v_string("toto").sub(v_number(1, 1)));
        // Boolean
        assert_eq!(Ok(v_bool(false)), v_bool(true).sub(v_bool(true)));
        assert_eq!(Ok(v_bool(true)), v_bool(true).sub(v_bool(false)));
        assert_eq!(Ok(v_bool(true)), v_bool(false).sub(v_bool(true)));
        assert_eq!(Ok(v_bool(false)), v_bool(false).sub(v_bool(false)));
        assert_err!(v_bool(false).sub(v_number(1, 1)));
        // Map
        assert_eq!(
            Ok(v_map(vec![(v_number(1, 1), v_number(1, 1))])),
            v_map(vec![
                (v_number(1, 1), v_number(1, 1)),
                (v_number(2, 1), v_number(2, 1)),
            ]).sub(v_map(vec![(v_number(2, 1), v_number(2, 1))]))
        );
        // Map
        assert_eq!(
            Ok(v_map(vec![
                (v_number(1, 1), v_number(1, 1)),
                (v_number(2, 1), v_number(2, 1)),
            ])),
            v_map(vec![
                (v_number(1, 1), v_number(1, 1)),
                (v_number(2, 1), v_number(2, 1)),
            ]).sub(v_map(vec![(v_number(2, 1), v_number(2, 2))]))
        );
        assert_err!(v_map(vec![(v_number(1, 1), v_number(1, 1))]).sub(v_number(1, 1)));
    }

    #[test]
    fn mul() {
        // Numbers * Number
        assert_eq!(Ok(v_number(4, 1)), v_number(8, 1).mul(v_number(1, 2)));
        // Number * CharString
        assert_eq!(Ok(v_string("totototo")), v_number(4, 1).mul(v_string("to")));
        assert_eq!(Ok(v_string("tototot")), v_number(7, 2).mul(v_string("to")));
        assert_eq!(Ok(v_string("tot")), v_number(3, 4).mul(v_string("toto")));
        assert_eq!(Ok(v_string("")), v_number(0, 1).mul(v_string("toto")));
        // CharString * Number
        assert_eq!(Ok(v_string("totototo")), v_string("to").mul(v_number(4, 1)));
        assert_eq!(Ok(v_string("tototot")), v_string("to").mul(v_number(7, 2)));
        assert_eq!(Ok(v_string("tot")), v_string("toto").mul(v_number(3, 4)));
        assert_eq!(Ok(v_string("")), v_string("toto").mul(v_number(0, 1)));
        // Number * Boolean
        assert_eq!(Ok(v_number(0, 1)), v_number(1, 1).mul(v_bool(false)));
        assert_eq!(Ok(v_number(1, 1)), v_number(1, 1).mul(v_bool(true)));
        // Boolean * Number
        assert_eq!(Ok(v_number(0, 1)), v_bool(false).mul(v_number(1, 1)));
        assert_eq!(Ok(v_number(1, 1)), v_bool(true).mul(v_number(1, 1)));
        // Number * Map
        assert_eq!(
            Ok(v_map(vec![(v_string("tototo"), v_number(3, 1))])),
            v_number(3, 1).mul(v_map(vec![(v_string("to"), v_number(1, 1))]))
        );
        // Map * Number
        assert_eq!(
            Ok(v_map(vec![(v_string("tototo"), v_number(3, 1))])),
            v_map(vec![(v_string("to"), v_number(1, 1))]).mul(v_number(3, 1))
        );
        // TODO: Number * Closure
        // TODO: Closure * Number

        // CharString * CharString
        assert_err!(v_string("hello ").mul(v_string("world")));
        // CharString * Boolean
        assert_eq!(Ok(v_string("")), v_string("toto").mul(v_bool(false)));
        assert_eq!(Ok(v_string("toto")), v_string("toto").mul(v_bool(true)));
        // Boolean * CharString
        assert_eq!(Ok(v_string("")), v_bool(false).mul(v_string("toto")));
        assert_eq!(Ok(v_string("toto")), v_bool(true).mul(v_string("toto")));
        // TODO: CharString * Map
        // TODO: Map * CharString

        // Boolean * Boolean
        assert_eq!(Ok(v_bool(true)), v_bool(true).mul(v_bool(true)));
        assert_eq!(Ok(v_bool(false)), v_bool(true).mul(v_bool(false)));
        assert_eq!(Ok(v_bool(false)), v_bool(false).mul(v_bool(true)));
        assert_eq!(Ok(v_bool(false)), v_bool(false).mul(v_bool(false)));

        // Boolean * Map
        assert_eq!(
            Ok(v_map(vec![(v_string("toto"), v_number(1, 1))])),
            v_bool(true).mul(v_map(vec![(v_string("toto"), v_number(1, 1))]))
        );
        assert_eq!(
            Ok(v_map(vec![(v_string(""), v_number(0, 1))])),
            v_bool(false).mul(v_map(vec![(v_string("toto"), v_number(1, 1))]))
        );
        // Map * Boolean
        assert_eq!(
            Ok(v_map(vec![(v_string("toto"), v_number(1, 1))])),
            v_map(vec![(v_string("toto"), v_number(1, 1))]).mul(v_bool(true))
        );
        assert_eq!(
            Ok(v_map(vec![(v_string(""), v_number(0, 1))])),
            v_map(vec![(v_string("toto"), v_number(1, 1))]).mul(v_bool(false))
        );
        // TODO: Boolean * Closure
        // TODO: Closure * Boolean

        // Map * Map
    }

    #[test]
    fn div() {
        // Number / Number
        assert_eq!(Ok(v_number(16, 1)), v_number(8, 1).div(v_number(1, 2)));

        // Number / CharString
        assert_err!(v_number(8, 1).div(v_number(0, 1)));
        assert_err!(v_number(8, 1).div(v_string("toto")));
        // TODO: Number / Boolean
        // TODO: Number / Map

        // CharString / Number
        assert_eq!(Ok(v_string("t")), v_string("to").div(v_number(2, 1)));
        assert_eq!(Ok(v_string("tototot")), v_string("to").div(v_number(2, 7)));
        assert_eq!(Ok(v_string("tot")), v_string("toto").div(v_number(4, 3)));
        assert_err!(v_string("toto").div(v_number(0, 1)));
        // CharString / CharString
        assert_err!(v_string("hello ").div(v_string("world")));
        // TODO: CharString / Boolean
        // TODO: CharString / Map

        // TODO: Boolean / Number
        // TODO: Boolean / CharStrings
        // Boolean / Boolean
        assert_err!(v_bool(true).div(v_bool(true)));
        // Map / Map
        assert_err!(v_map(vec![]).div(v_map(vec![])));
    }

    #[test]
    fn add() {
        // Numbers
        assert_eq!(Ok(v_number(17, 2)), v_number(8, 1).add(v_number(1, 2)));
        assert_err!(v_number(8, 1).add(v_string("toto")));
        // Strings
        assert_eq!(
            Ok(v_string("hello world")),
            v_string("hello ").add(v_string("world"))
        );
        assert_err!(v_string("toto").add(v_number(1, 1)));
        // Boolean
        assert_eq!(Ok(v_bool(true)), v_bool(true).add(v_bool(true)));
        assert_eq!(Ok(v_bool(true)), v_bool(true).add(v_bool(false)));
        assert_eq!(Ok(v_bool(true)), v_bool(false).add(v_bool(true)));
        assert_eq!(Ok(v_bool(false)), v_bool(false).add(v_bool(false)));
        assert_err!(v_bool(false).add(v_number(1, 1)));
        // Map
        assert_eq!(
            Ok(v_map(vec![
                (v_number(1, 1), v_number(1, 1)),
                (v_number(2, 1), v_number(2, 1)),
            ])),
            v_map(vec![(v_number(1, 1), v_number(1, 1))])
                .add(v_map(vec![(v_number(2, 1), v_number(2, 1))]))
        );
        assert_err!(v_map(vec![(v_number(1, 1), v_number(1, 1))]).add(v_number(1, 1)));
    }

    #[test]
    fn eq() {
        // Number == Number
        assert_eq!(Ok(v_bool(true)), v_number(1, 1).val_eq(&v_number(1, 1)));
        assert_eq!(Ok(v_bool(false)), v_number(2, 1).val_eq(&v_number(1, 1)));
        // Number == String
        assert_eq!(Ok(v_bool(false)), v_number(2, 1).val_eq(&v_string("toto")));
        // Number == Boolean
        assert_eq!(Ok(v_bool(false)), v_number(2, 1).val_eq(&v_bool(false)));
        // Number == Map
        assert_eq!(Ok(v_bool(false)), v_number(2, 1).val_eq(&v_map(vec![])));

        // CharString == CharString
        assert_eq!(Ok(v_bool(true)), v_string("toto").val_eq(&v_string("toto")));
        assert_eq!(Ok(v_bool(false)), v_string("toto").val_eq(&v_string("titi")));
        // CharString == Boolean
        assert_eq!(Ok(v_bool(false)), v_string("toto").val_eq(&v_bool(false)));
        // CharString == Map
        assert_eq!(Ok(v_bool(false)), v_string("toto").val_eq(&v_map(vec![])));

        // Boolean == Boolean
        assert_eq!(Ok(v_bool(true)), v_bool(true).val_eq(&v_bool(true)));
        assert_eq!(Ok(v_bool(false)), v_bool(false).val_eq(&v_bool(true)));
        // Boolean == Map
        assert_eq!(Ok(v_bool(false)), v_bool(true).val_eq(&v_map(vec![])));

        // Map == Map
        assert_eq!(
            Ok(v_bool(true)),
            v_map(vec![(v_string("a"), v_number(1, 1))])
                .val_eq(&v_map(vec![(v_string("a"), v_number(1, 1))]))
        );
        assert_eq!(
            Ok(v_bool(false)),
            v_map(vec![(v_string("not-a"), v_number(1, 1))])
                .val_eq(&v_map(vec![(v_string("a"), v_number(1, 1))]))
        );
    }

    #[test]
    fn gt_and_lt() {
        // Number > Number
        assert_eq!(Ok(v_bool(true)), v_number(2, 1).val_gt(&v_number(1, 1)));
        assert_eq!(Ok(v_bool(false)), v_number(1, 1).val_gt(&v_number(1, 1)));
        assert_eq!(Ok(v_bool(false)), v_number(0, 1).val_gt(&v_number(1, 1)));
        // Number < Number
        assert_eq!(Ok(v_bool(true)), v_number(1, 1).val_lt(&v_number(2, 1)));
        assert_eq!(Ok(v_bool(false)), v_number(1, 1).val_lt(&v_number(1, 1)));
        assert_eq!(Ok(v_bool(false)), v_number(1, 1).val_lt(&v_number(0, 1)));
        // Number > CharString
        assert_eq!(Ok(v_bool(false)), v_number(2, 1).val_gt(&v_string("toto")));
        // Number < CharString
        assert_eq!(Ok(v_bool(false)), v_string("toto").val_lt(&v_number(2, 1)));
        // Number > Boolean
        assert_eq!(Ok(v_bool(false)), v_number(2, 1).val_gt(&v_bool(false)));
        assert_eq!(Ok(v_bool(false)), v_number(2, 1).val_gt(&v_bool(true)));
        // Number < Boolean
        assert_eq!(Ok(v_bool(false)), v_bool(false).val_lt(&v_number(2, 1)));
        assert_eq!(Ok(v_bool(false)), v_bool(true).val_lt(&v_number(2, 1)));
        // Number > Map
        assert_eq!(Ok(v_bool(false)), v_number(2, 1).val_gt(&v_map(vec![])));
        // Number < Map
        assert_eq!(Ok(v_bool(false)), v_map(vec![]).val_lt(&v_number(2, 1)));

        // CharString > CharString
        assert_eq!(Ok(v_bool(true)), v_string("toto").val_gt(&v_string("tot")));
        assert_eq!(Ok(v_bool(false)), v_string("toto").val_gt(&v_string("toto")));
        // CharString < CharString
        assert_eq!(Ok(v_bool(true)), v_string("tot").val_lt(&v_string("toto")));
        assert_eq!(Ok(v_bool(false)), v_string("toto").val_lt(&v_string("toto")));
        // CharString > Number
        assert_eq!(Ok(v_bool(true)), v_string("toto").val_gt(&v_number(2, 1)));
        // CharString < Number
        assert_eq!(Ok(v_bool(true)), v_number(2, 1).val_lt(&v_string("toto")));
        // CharString > Boolean
        assert_eq!(Ok(v_bool(false)), v_string("toto").val_gt(&v_bool(false)));
        // CharString < Boolean
        assert_eq!(Ok(v_bool(false)), v_bool(false).val_lt(&v_string("toto")));
        // CharString > Map
        assert_eq!(Ok(v_bool(false)), v_string("toto").val_gt(&v_map(vec![])));
        // CharString < Map
        assert_eq!(Ok(v_bool(false)), v_map(vec![]).val_lt(&v_string("toto")));

        // Boolean > Boolean
        assert_eq!(Ok(v_bool(true)), v_bool(true).val_gt(&v_bool(false)));
        assert_eq!(Ok(v_bool(false)), v_bool(true).val_gt(&v_bool(true)));
        assert_eq!(Ok(v_bool(false)), v_bool(false).val_gt(&v_bool(true)));
        // Boolean < Boolean
        assert_eq!(Ok(v_bool(true)), v_bool(false).val_lt(&v_bool(true)));
        assert_eq!(Ok(v_bool(false)), v_bool(true).val_lt(&v_bool(true)));
        assert_eq!(Ok(v_bool(false)), v_bool(true).val_lt(&v_bool(false)));
        // Boolean > Number
        assert_eq!(Ok(v_bool(true)), v_bool(true).val_gt(&v_number(1, 1)));
        // Boolean < Number
        assert_eq!(Ok(v_bool(true)), v_number(1, 1).val_lt(&v_bool(true)));
        // Boolean > CharString
        assert_eq!(Ok(v_bool(true)), v_bool(true).val_gt(&v_string("toto")));
        // Boolean < CharString
        assert_eq!(Ok(v_bool(true)), v_string("toto").val_lt(&v_bool(true)));
        // Boolean > Map
        assert_eq!(Ok(v_bool(false)), v_bool(true).val_gt(&v_map(vec![])));
        // Boolean < Map
        assert_eq!(Ok(v_bool(false)), v_map(vec![]).val_lt(&v_bool(true)));

        // Map > Map
        assert_eq!(
            Ok(v_bool(false)),
            v_map(vec![(v_string("a"), v_number(1, 1))])
                .val_gt(&v_map(vec![(v_string("a"), v_number(2, 1))]))
        );
        assert_eq!(
            Ok(v_bool(false)),
            v_map(vec![(v_string("a"), v_number(1, 1))])
                .val_gt(&v_map(vec![(v_string("b"), v_number(1, 1))]))
        );
        assert_eq!(
            Ok(v_bool(true)),
            v_map(vec![(v_string("b"), v_number(1, 1))])
                .val_gt(&v_map(vec![(v_string("a"), v_number(2, 1))]))
        );
        assert_eq!(
            Ok(v_bool(true)),
            v_map(vec![(v_string("b"), v_number(2, 1))])
                .val_gt(&v_map(vec![(v_string("b"), v_number(1, 1))]))
        );
        // Map < Map
        assert_eq!(
            Ok(v_bool(false)),
            v_map(vec![(v_string("a"), v_number(2, 1))])
                .val_lt(&v_map(vec![(v_string("a"), v_number(1, 1))]))
        );
        assert_eq!(
            Ok(v_bool(false)),
            v_map(vec![(v_string("b"), v_number(1, 1))])
                .val_lt(&v_map(vec![(v_string("a"), v_number(1, 1))]))
        );
        assert_eq!(
            Ok(v_bool(true)),
            v_map(vec![(v_string("a"), v_number(2, 1))])
                .val_lt(&v_map(vec![(v_string("b"), v_number(1, 1))]))
        );
        assert_eq!(
            Ok(v_bool(true)),
            v_map(vec![(v_string("b"), v_number(1, 1))])
                .val_lt(&v_map(vec![(v_string("b"), v_number(2, 1))]))
        );
    }
}
