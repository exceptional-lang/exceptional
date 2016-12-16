
include!(concat!(env!("OUT_DIR"), "/exceptional-grammar.rs"));

#[cfg(test)]
mod tests {
    use super::*;
    use ast::*;
    use num::rational::{Ratio};
    use num::{BigInt};

    #[test]
    fn parses_assign_with_numbers() {
        assert_eq!(
            parse(&"let a = 1234"),
            [s_assign(&"a", v_ratio(1234, 1))]
        );

        assert_eq!(
            parse(&"let a = 0011"),
            [s_assign(&"a", v_ratio(11, 1))]
        );
    }

    #[test]
    fn parses_assign_with_strings() {
        assert_eq!(
            parse(&"let a = \"\""),
            [s_assign(&"a", v_string(&""))]
        );

        assert_eq!(
            parse(&"let a = \"string with more words\""),
            [s_assign(&"a", v_string(&"string with more words"))]
        );
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

    fn parse(input: &str) -> Vec<Statement> {
        statements(input).unwrap()
    }
}
