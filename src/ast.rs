use num::rational::BigRational;
use regex::Regex;
use std::cmp::Ordering;
use std::hash::{Hash, Hasher};

#[derive(Clone, Eq, Debug, Hash, Ord, PartialEq, PartialOrd)]
pub enum Literal {
    Number(BigRational),
    CharString(String),
    Boolean(bool),
    Map(Vec<(Expression, Expression)>),
    Fn(Box<Vec<String>>, Box<Vec<Statement>>),
}
// TODO: Vec(Vec<Value>)

#[derive(Clone, Eq, Debug, Hash, Ord, PartialEq, PartialOrd)]
pub enum Statement {
    Assign(bool, String, Box<Expression>),
    IndexAssign(Box<Expression>, Box<Expression>, Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Raise(Expression),
    Rescue(Pattern, Box<Vec<Statement>>),
}

#[derive(Clone, Eq, Debug, Hash, Ord, PartialEq, PartialOrd)]
pub enum Expression {
    BinOp(String, Box<Expression>, Box<Expression>),
    Literal(Literal),
    Identifier(String),
    IndexAccess(Box<Expression>, Box<Expression>),
    Import(Box<Expression>),
}

#[derive(Clone, Debug)]
pub struct StringMatcher {
    pub regex: Regex,
}

impl PartialEq for StringMatcher {
    fn eq(&self, other: &StringMatcher) -> bool {
        self.regex.as_str().eq(other.regex.as_str())
    }
}

impl Eq for StringMatcher {}

impl Ord for StringMatcher {
    fn cmp(&self, other: &StringMatcher) -> Ordering {
        self.regex.as_str().cmp(other.regex.as_str())
    }
}

impl PartialOrd for StringMatcher {
    fn partial_cmp(&self, other: &StringMatcher) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Hash for StringMatcher {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.regex.as_str().hash(state);
    }
}

#[derive(Clone, Eq, Debug, Hash, Ord, PartialEq, PartialOrd)]
pub enum Pattern {
    Number(BigRational),
    CharString(String),
    Boolean(bool),
    Map(Vec<(Pattern, Pattern)>),
    Identifier(String),
    StringMatch(Vec<String>, StringMatcher),
}

#[cfg(test)]
mod test {
    use super::*;
    use test_helpers::*;
    use std::collections::hash_map::DefaultHasher;

    fn string_matcher(str: &str) -> StringMatcher {
        StringMatcher { regex: Regex::new(str).unwrap() }
    }

    #[test]
    fn string_matcher_eq() {
        assert_eq!(string_matcher("abcd"), string_matcher("abcd"));
        assert_ne!(string_matcher("abcd"), string_matcher("efgh"));
        assert!(string_matcher("abcd") < string_matcher("efgh"));
        assert!(string_matcher("efgh") > string_matcher("abcd"));

        let mut hasher = DefaultHasher::new();
        assert_eq!(
            string_matcher("abcd").hash(&mut hasher),
            string_matcher("abcd").hash(&mut hasher)
        );
    }
}
