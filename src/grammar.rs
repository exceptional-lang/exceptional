
include!(concat!(env!("OUT_DIR"), "/exceptional-grammar.rs"));

#[cfg(test)]
mod test_helpers {
    use super::*;
    use ast::*;
    use num::rational::{Ratio};
    use num::{BigInt};

    pub fn v_string(name: &str) -> Value {
        Value::CharString(
           name.to_owned()
        )
    }

    pub fn v_ratio(num: i64, denom: i64) -> Value {
        Value::Number(
            Ratio::new(
                BigInt::from(num),
                BigInt::from(denom)
            )
        )
    }

    pub fn v_bool(b: bool) -> Value {
        match b {
            true => Value::Boolean(true),
            false => Value::Boolean(false)
        }
    }

    pub fn v_function(args: Vec<String>, statements: Vec<Statement>) -> Value {
        Value::Fn(Box::new((args, statements)))
    }

    pub fn s_assign(name: &str, value: Value) -> Statement {
        Statement::Assign(
            true,
            name.to_owned(),
            Box::new(
                e_value(
                    value
                )
            )
        )
    }

    pub fn s_call(name: &str, args: Vec<Expression>) -> Statement {
        Statement::Call(
            name.to_owned(),
            Box::new(args),
        )
    }

    pub fn e_value(value: Value) -> Expression {
        Expression::Value(value)
    }

    pub fn parse_expression(input: &str) -> Expression {
        expression(input).unwrap()
    }

    pub fn parse_statements(input: &str) -> Vec<Statement> {
        statements(input).unwrap()
    }

    pub fn parse_value(input: &str) -> Value {
        value(input).unwrap()
    }
}

#[cfg(test)]
mod test_values {
    use super::test_helpers::*;

    #[test]
    fn parses_number() {
        assert_eq!(
            parse_value(&"1234"),
            v_ratio(1234, 1)
        );

        assert_eq!(
            parse_value(&"0011"),
            v_ratio(11, 1)
        );
    }

    #[test]
    fn parses_strings() {
        assert_eq!(
            parse_value(&"\"\""),
            v_string(&"")
        );

        assert_eq!(
            parse_value(&"\"string with more words\""),
            v_string(&"string with more words")
        );
    }

    #[test]
    fn parses_booleans() {
        assert_eq!(
            parse_value(&"true"),
            v_bool(true)
        );

        assert_eq!(
            parse_value(&"false"),
            v_bool(false)
        );
    }
}

#[cfg(test)]
mod test_expressions {
    use super::test_helpers::*;

    #[test]
    fn parses_simple_functions() {
        assert_eq!(
            parse_expression(&"def() do end"),
            e_value(
                v_function(vec![], vec![])
            )
        )
    }

    #[test]
    fn parses_functions_with_args() {
        assert_eq!(
            parse_expression(&"def(a, b) do end"),
            e_value(
                v_function(vec!["a".to_owned(), "b".to_owned()], vec![])
            )
        )
    }

    #[test]
    fn prases_functions_with_bodies() {
        assert_eq!(
            parse_expression(&"def(a, b) do\nlet c = 1\nend"),
            e_value(
                v_function(
                    vec!["a".to_owned(), "b".to_owned()],
                    vec![s_assign(&"c", v_ratio(1, 1))]
                )
            )
        )
    }
}

#[cfg(test)]
mod test_statements {
    use super::test_helpers::*;

    #[test]
    fn parses_assigns() {
        assert_eq!(
            parse_statements(&"let a = 1"),
            [s_assign(&"a", v_ratio(1, 1))]
        )
    }

    #[test]
    fn parses_calls() {
        assert_eq!(
            parse_statements("a()"),
            [s_call(&"a", vec![])]
        )
    }

    #[test]
    fn parses_calls_with_simple_args() {
        assert_eq!(
            parse_statements("a(1)"),
            [s_call(&"a", vec![e_value(v_ratio(1, 1))])]
        )
    }
}
