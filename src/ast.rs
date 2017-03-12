use num::rational::BigRational;

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
    Call(String, Vec<Expression>),
    Raise(Expression),
    Rescue(Pattern, Box<Vec<Statement>>),
}

#[derive(Clone, Eq, Debug, Hash, Ord, PartialEq, PartialOrd)]
pub enum Expression {
    BinOp(String, Box<Expression>, Box<Expression>),
    Literal(Literal),
    Identifier(String),
    IndexAccess(Box<Expression>, Box<Expression>),
}

#[derive(Clone, Eq, Debug, Hash, Ord, PartialEq, PartialOrd)]
pub enum Pattern {
    Number(BigRational),
    CharString(String),
    Boolean(bool),
    Map(Vec<(Pattern, Pattern)>),
    Identifier(String),
}
