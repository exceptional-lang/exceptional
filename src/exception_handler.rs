use ast::*;
use closure::Closure;
use value::Value;

use num::rational::BigRational;
use regex::Regex;
use std::collections::btree_map::Entry;
use std::collections::BTreeMap;
use std::rc::Rc;

pub type MatchedBindings = Option<BTreeMap<String, Value>>;

#[derive(Clone, Eq, Debug, PartialEq)]
pub struct ExceptionHandler {
    pub closure: Closure,
    pattern: Rc<Pattern>,
}

impl ExceptionHandler {
    pub fn new(pattern: Rc<Pattern>, closure: Closure) -> ExceptionHandler {
        ExceptionHandler {
            pattern: pattern,
            closure: closure,
        }
    }
    pub fn matches(&self, value: Value) -> MatchedBindings {
        ExceptionHandler::match_pattern(&*self.pattern, &value)
    }

    fn match_pattern(pattern: &Pattern, value: &Value) -> MatchedBindings {
        match pattern {
            &Pattern::Number(ref ratio) => ExceptionHandler::match_number(ratio, value),
            &Pattern::CharString(ref string) => ExceptionHandler::match_string(string, value),
            &Pattern::Boolean(bool) => ExceptionHandler::match_bool(bool, value),
            &Pattern::Map(ref pairs) => ExceptionHandler::match_map(pairs, value),
            &Pattern::Identifier(ref name) => ExceptionHandler::match_identifier(name, value),
            &Pattern::StringMatch(ref bindings, ref matcher) => {
                ExceptionHandler::match_string_match(bindings, &matcher.regex, value)
            }
        }
    }

    fn match_string_match(bindings: &Vec<String>, regex: &Regex, value: &Value) -> MatchedBindings {
        if let &Value::CharString(ref char_str) = value {
            if let Some(mat) = regex.captures(char_str) {
                let results = bindings
                    .iter()
                    .enumerate()
                    .map(|(index, ref name)| {
                        (
                            name.to_string(),
                            Value::CharString(mat.get(index + 1).unwrap().as_str().to_owned()),
                        )
                    })
                    .collect();

                Some(results)
            } else {
                None
            }
        } else {
            None
        }
    }

    fn match_number(ratio: &BigRational, value: &Value) -> MatchedBindings {
        match value {
            &Value::Number(ref number) => match ratio.eq(number) {
                true => Some(BTreeMap::new()),
                _ => None,
            },
            _ => None,
        }
    }

    fn match_string(string: &String, value: &Value) -> MatchedBindings {
        match value {
            &Value::CharString(ref str) => match str.eq(string) {
                true => Some(BTreeMap::new()),
                _ => None,
            },
            _ => None,
        }
    }

    fn match_bool(b: bool, value: &Value) -> MatchedBindings {
        match value {
            &Value::Boolean(other_bool) => match b == other_bool {
                true => Some(BTreeMap::new()),
                _ => None,
            },
            _ => None,
        }
    }

    fn match_map(pairs: &Vec<(Pattern, Pattern)>, value: &Value) -> MatchedBindings {
        match value {
            &Value::Map(ref btreemap) => {
                let mut bindings: BTreeMap<String, Value> = BTreeMap::new();
                for &(ref key, ref pattern_value) in pairs.iter() {
                    let key_as_value = ExceptionHandler::pattern_key_to_value(key);

                    let maybe_nested_bindings = btreemap
                        .borrow()
                        .get(&Rc::new(key_as_value))
                        .and_then(|value| ExceptionHandler::match_pattern(pattern_value, value));

                    if let None = maybe_nested_bindings {
                        return None;
                    }

                    for (key, value) in maybe_nested_bindings.unwrap().iter() {
                        match bindings.entry(key.to_owned()) {
                            Entry::Occupied(entry) => {
                                if entry.get() != value {
                                    return None;
                                }
                            }
                            Entry::Vacant(v) => {
                                v.insert(value.to_owned());
                            }
                        }
                    }
                }
                Some(bindings)
            }
            _ => None,
        }
    }

    fn match_identifier(name: &String, value: &Value) -> MatchedBindings {
        let bindings: BTreeMap<_, _> = vec![(name.to_owned(), value.clone())].into_iter().collect();
        Some(bindings)
    }

    fn pattern_key_to_value(pattern: &Pattern) -> Value {
        match pattern {
            &Pattern::Number(ref ratio) => Value::Number(ratio.to_owned()),
            &Pattern::CharString(ref str) => Value::CharString(str.to_owned()),
            &Pattern::Boolean(bool) => Value::Boolean(bool),
            ref pat => panic!("pattern cannot be used as key ({:?})", pat),
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use std::rc::Rc;
    use test_helpers::*;

    #[test]
    fn matches_numbers() {
        let handler = ExceptionHandler::new(
            Rc::new(Pattern::Number(build_ratio(1, 1))),
            Closure::blank(),
        );
        assert_eq!(Some(BTreeMap::new()), handler.matches(v_number(1, 1)));
        assert_eq!(None, handler.matches(v_number(2, 1)));
        assert_eq!(None, handler.matches(v_string("toto")))
    }

    #[test]
    fn matches_strings() {
        let handler = ExceptionHandler::new(
            Rc::new(Pattern::CharString("toto".to_owned())),
            Closure::blank(),
        );
        assert_eq!(Some(BTreeMap::new()), handler.matches(v_string("toto")));
        assert_eq!(None, handler.matches(v_string("titi")));
        assert_eq!(None, handler.matches(v_number(1, 1)))
    }

    #[test]
    fn matches_bools() {
        let handler = ExceptionHandler::new(Rc::new(Pattern::Boolean(false)), Closure::blank());
        assert_eq!(Some(BTreeMap::new()), handler.matches(v_bool(false)));
        assert_eq!(None, handler.matches(v_bool(true)));
        assert_eq!(None, handler.matches(v_string("toto")));
    }

    #[test]
    fn matches_simple_map() {
        let handler = ExceptionHandler::new(
            Rc::new(Pattern::Map(vec![(
                Pattern::Number(build_ratio(1, 1)),
                Pattern::Identifier("toto".to_owned()),
            )])),
            Closure::blank(),
        );
        assert_eq!(
            Some(
                vec![("toto".to_owned(), v_string("titi"))]
                    .into_iter()
                    .collect(),
            ),
            handler.matches(v_map(vec![(v_number(1, 1), v_string("titi"))]))
        );
        assert_eq!(
            None,
            handler.matches(v_map(vec![(v_string("titi"), v_number(2, 1))]))
        );
        assert_eq!(None, handler.matches(v_bool(false)));
    }

    #[test]
    fn matches_maps_with_multiple_bindings_of_equal_values() {
        let handler = ExceptionHandler::new(
            Rc::new(Pattern::Map(vec![
                (
                    Pattern::Number(build_ratio(1, 1)),
                    Pattern::Identifier("toto".to_owned()),
                ),
                (
                    Pattern::Number(build_ratio(2, 1)),
                    Pattern::Identifier("toto".to_owned()),
                ),
            ])),
            Closure::blank(),
        );
        assert_eq!(
            Some(
                vec![("toto".to_owned(), v_string("titi"))]
                    .into_iter()
                    .collect(),
            ),
            handler.matches(v_map(vec![
                (v_number(1, 1), v_string("titi")),
                (v_number(2, 1), v_string("titi")),
            ]))
        );
        assert_eq!(
            None,
            handler.matches(v_map(vec![
                (v_number(1, 1), v_string("titi")),
                (v_number(2, 1), v_string("foo")),
            ]))
        );
    }

    #[test]
    fn matches_recursive_maps() {
        let pattern = Pattern::Map(vec![(
            Pattern::Number(build_ratio(1, 1)),
            Pattern::Map(vec![(
                Pattern::Number(build_ratio(2, 1)),
                Pattern::Identifier("toto".to_owned()),
            )]),
        )]);
        let handler = ExceptionHandler::new(Rc::new(pattern), Closure::blank());
        assert_eq!(
            Some(
                vec![("toto".to_owned(), v_string("titi"))]
                    .into_iter()
                    .collect(),
            ),
            handler.matches(v_map(vec![(
                v_number(1, 1),
                v_map(vec![(v_number(2, 1), v_string("titi"))]),
            )]))
        );
    }

    #[test]
    fn matches_identifier() {
        let handler = ExceptionHandler::new(
            Rc::new(Pattern::Identifier("toto".to_owned())),
            Closure::blank(),
        );
        assert_eq!(
            Some(
                vec![("toto".to_owned(), v_string("titi"))]
                    .into_iter()
                    .collect(),
            ),
            handler.matches(v_string("titi"))
        );
        assert_eq!(
            Some(
                vec![("toto".to_owned(), v_number(1, 1))]
                    .into_iter()
                    .collect(),
            ),
            handler.matches(v_number(1, 1))
        )
    }

    #[test]
    fn matches_string_match() {
        let handler = ExceptionHandler::new(
            Rc::new(p_string_match(
                vec!["toto", "titi"],
                r#"(?s)\Ahello (.*?) foo (.*?)\z"#,
            )),
            Closure::blank(),
        );
        assert_eq!(
            Some(
                vec![
                    ("toto".to_owned(), v_string("world")),
                    ("titi".to_owned(), v_string("bar")),
                ].into_iter()
                    .collect(),
            ),
            handler.matches(v_string("hello world foo bar"))
        );

        let handler = ExceptionHandler::new(
            Rc::new(p_string_match(vec!["toto"], r#"(?s)\Ahello 1 (.*?)\z"#)),
            Closure::blank(),
        );
        assert_eq!(
            Some(
                vec![("toto".to_owned(), v_string("world"))]
                    .into_iter()
                    .collect(),
            ),
            handler.matches(v_string("hello 1 world"))
        );
    }
}
