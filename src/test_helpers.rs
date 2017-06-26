#[cfg(test)]

use ast::*;
use binding_map::BindingMap;
use grammar::*;
use value::Value;
use closure::Closure;
use instructions::*;
use num::rational::Ratio;
use num::BigInt;
use std::rc::*;
use std::cell::RefCell;
use std::collections::BTreeMap;
use regex::Regex;

pub fn l_string(string: &str) -> Literal {
    Literal::CharString(string.to_owned())
}

pub fn l_number(num: i64, denom: i64) -> Literal {
    Literal::Number(build_ratio(num, denom))
}

pub fn l_bool(b: bool) -> Literal {
    match b {
        true => Literal::Boolean(true),
        false => Literal::Boolean(false),
    }
}

pub fn l_function(args: Vec<String>, statements: Vec<Statement>) -> Literal {
    Literal::Fn(Box::new(args), Box::new(statements))
}

pub fn l_map(pairs: Vec<(Expression, Expression)>) -> Literal {
    Literal::Map(pairs)
}

pub fn p_map(pairs: Vec<(Pattern, Pattern)>) -> Pattern {
    Pattern::Map(pairs)
}

pub fn p_bool(b: bool) -> Pattern {
    Pattern::Boolean(b)
}

pub fn p_number(num: i64, denom: i64) -> Pattern {
    Pattern::Number(build_ratio(num, denom))
}

pub fn p_string(string: &str) -> Pattern {
    Pattern::CharString(string.to_owned())
}

pub fn p_ident(name: &str) -> Pattern {
    Pattern::Identifier(name.to_owned())
}

pub fn p_string_match(bindings: Vec<&str>, regex: &str) -> Pattern {
    Pattern::StringMatch(
        bindings.into_iter().map(|b| b.to_owned()).collect(),
        StringMatcher { regex: Regex::new(&(r#"(?s)\A"#.to_string() + regex + r#"\z"#)).unwrap() },
    )
}

pub fn s_assign(name: &str, literal: Literal) -> Statement {
    Statement::Assign(true, name.to_owned(), Box::new(e_literal(literal)))
}

pub fn s_index_assign(target: Expression, property: Expression, value: Expression) -> Statement {
    Statement::IndexAssign(Box::new(target), Box::new(property), Box::new(value))
}

pub fn s_call(target: Expression, args: Vec<Expression>) -> Statement {
    Statement::Call(Box::new(target), args)
}

pub fn s_raise(exp: Expression) -> Statement {
    Statement::Raise(exp)
}

pub fn s_rescue(map: Pattern, statements: Vec<Statement>) -> Statement {
    Statement::Rescue(map, Box::new(statements))
}

pub fn e_literal(literal: Literal) -> Expression {
    Expression::Literal(literal)
}

pub fn e_identifier(name: &str) -> Expression {
    Expression::Identifier(name.to_owned())
}

pub fn e_binop(op: &str, left: Expression, right: Expression) -> Expression {
    Expression::BinOp(op.to_owned(), Box::new(left), Box::new(right))
}

pub fn e_index_access(target: Expression, property: Expression) -> Expression {
    Expression::IndexAccess(Box::new(target), Box::new(property))
}

pub fn e_import(name: Expression) -> Expression {
    Expression::Import(Box::new(name))
}

pub fn build_ratio(num: i64, denom: i64) -> Ratio<BigInt> {
    Ratio::new(BigInt::from(num), BigInt::from(denom))
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

pub fn v_bool(bool: bool) -> Value {
    Value::Boolean(bool)
}

pub fn v_string(str: &str) -> Value {
    Value::CharString(str.to_owned())
}

pub fn v_number(num: i64, denom: i64) -> Value {
    Value::Number(build_ratio(num, denom))
}

pub fn v_map(pairs: Vec<(Value, Value)>) -> Value {
    let map: BTreeMap<_, _> = pairs
        .into_iter()
        .map(|(key, value)| (key, value))
        .collect::<BTreeMap<_, _>>();
    Value::Map(Rc::new(RefCell::new(map)))
}

pub fn v_closure(
    args: Vec<String>,
    insns: InstructionSequence,
    parent_bindings: Option<&BindingMap>,
) -> Value {
    let bindings = BindingMap::new(parent_bindings);
    let closure = Closure::new(Rc::new(insns), &bindings);
    Value::Closure(Rc::new(Box::new(args)), Rc::new(closure))
}

macro_rules! assert_err {
    ($e:expr) => {
        match $e {
            Err(_) => {}
            res => {
                panic!("assertion failed: expected Err, got: {:?}", res)
            }
        }
    }
}
