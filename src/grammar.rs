
include!(concat!(env!("OUT_DIR"), "/exceptional-grammar.rs"));

#[cfg(test)]
mod tests {
    use super::*;
    use ast::*;
    use num::rational::{Ratio};
    use num::{BigInt};

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

    #[test]
    fn parses_functions() {
        assert_eq!(
            parse_expression(&"def() do end"),
            Expression::Value(
                v_function(vec![], vec![])
            )
        );

        assert_eq!(
            parse_expression(&"def(a, b) do end"),
            Expression::Value(
                v_function(vec!["a".to_owned(), "b".to_owned()], vec![])
            )
        );

        assert_eq!(
            parse_expression(&"def(a, b) do\nlet c = 1\nend"),
            Expression::Value(
                v_function(
                    vec!["a".to_owned(), "b".to_owned()],
                    vec![s_assign(&"c", v_ratio(1, 1))]
                )
            )
        )
    }

    #[test]
    fn parses_assigns() {
        assert_eq!(
            parse_statements(&"let a = 1"),
            [s_assign(&"a", v_ratio(1, 1))]
        )
    }

    fn v_string(name: &str) -> Value {
        Value::CharString(
           name.to_owned()
        )
    }

    fn v_ratio(num: i64, denom: i64) -> Value {
        Value::Number(
            Ratio::new(
                BigInt::from(num),
                BigInt::from(denom)
            )
        )
    }

    fn v_bool(b: bool) -> Value {
        match b {
            true => Value::Boolean(true),
            false => Value::Boolean(false)
        }
    }

    fn v_function(args: Vec<String>, statements: Vec<Statement>) -> Value {
        Value::Fn(Box::new((args, statements)))
    }

    fn s_assign(name: &str, value: Value) -> Statement {
        Statement::Assign(
            true,
            name.to_owned(),
            Box::new(
                Expression::Value(
                    value
                )
            )
        )
    }

    fn parse_expression(input: &str) -> Expression {
        expression(input).unwrap()
    }

    fn parse_statements(input: &str) -> Vec<Statement> {
        statements(input).unwrap()
    }

    fn parse_value(input: &str) -> Value {
        value(input).unwrap()
    }
}
