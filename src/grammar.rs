
include!(concat!(env!("OUT_DIR"), "/exceptional-grammar.rs"));

#[cfg(test)]
mod test_helpers {
    use super::*;
    use ast::*;
    use num::rational::{Ratio};
    use num::{BigInt};

    pub fn l_string(name: &str) -> Literal {
        Literal::CharString(
           name.to_owned()
        )
    }

    pub fn l_number(num: i64, denom: i64) -> Literal {
        Literal::Number(
            Ratio::new(
                BigInt::from(num),
                BigInt::from(denom)
            )
        )
    }

    pub fn l_bool(b: bool) -> Literal {
        match b {
            true => Literal::Boolean(true),
            false => Literal::Boolean(false)
        }
    }

    pub fn l_function(args: Vec<String>, statements: Vec<Statement>) -> Literal {
        Literal::Fn(Box::new((args, statements)))
    }

    pub fn s_assign(name: &str, literal: Literal) -> Statement {
        Statement::Assign(
            true,
            name.to_owned(),
            Box::new(
                e_literal(
                    literal
                )
            )
        )
    }

    pub fn s_call(name: &str, args: Vec<Expression>) -> Statement {
        Statement::Call(name.to_owned(), args)
    }

    pub fn e_literal(literal: Literal) -> Expression {
        Expression::Literal(literal)
    }

    pub fn parse_expression(input: &str) -> Expression {
        expression(input).unwrap()
    }

    pub fn parse_statements(input: &str) -> Vec<Statement> {
        statements(input).unwrap()
    }

    pub fn parse_literal(input: &str) -> Literal {
        literal(input).unwrap()
    }
}

#[cfg(test)]
mod test_literals {
    use super::test_helpers::*;

    #[test]
    fn parses_number() {
        assert_eq!(
            parse_literal(&"1234"),
            l_number(1234, 1)
        );

        assert_eq!(
            parse_literal(&"0011"),
            l_number(11, 1)
        );
    }

    #[test]
    fn parses_strings() {
        assert_eq!(
            parse_literal(&"\"\""),
            l_string(&"")
        );

        assert_eq!(
            parse_literal(&"\"string with more words\""),
            l_string(&"string with more words")
        );
    }

    #[test]
    fn parses_booleans() {
        assert_eq!(
            parse_literal(&"true"),
            l_bool(true)
        );

        assert_eq!(
            parse_literal(&"false"),
            l_bool(false)
        );
    }
}

#[cfg(test)]
mod test_expressions {
    use super::test_helpers::*;

    #[test]
    fn parses_simple_literal_expressions() {
        assert_eq!(
            parse_expression(&"1"),
            e_literal(l_number(1, 1))
        );

        assert_eq!(
            parse_expression(&"\"\""),
            e_literal(l_string(&""))
        )
    }

    #[test]
    fn parses_simple_functions() {
        assert_eq!(
            parse_expression(&"def() do end"),
            e_literal(
                l_function(vec![], vec![])
            )
        )
    }

    #[test]
    fn parses_functions_with_args() {
        assert_eq!(
            parse_expression(&"def(a, b) do end"),
            e_literal(
                l_function(vec!["a".to_owned(), "b".to_owned()], vec![])
            )
        )
    }

    #[test]
    fn prases_functions_with_bodies() {
        assert_eq!(
            parse_expression(&"def(a, b) do\nlet c = 1\nend"),
            e_literal(
                l_function(
                    vec!["a".to_owned(), "b".to_owned()],
                    vec![s_assign(&"c", l_number(1, 1))]
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
            [s_assign(&"a", l_number(1, 1))]
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
            [s_call(&"a", vec![e_literal(v_ratio(1, 1))])]
        )
    }
}
