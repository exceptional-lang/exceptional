use grammar::*;
use ast::*;
use std::collections::BTreeMap;
use std::rc::Rc;
use num::rational::BigRational;

#[derive(Clone, Eq, Debug, PartialEq)]
pub enum Value {
    Number(BigRational),
    CharString(String),
    Boolean(bool),
    Map(BTreeMap<Rc<Value>, Rc<Value>>),
    Fn((Vec<String>, InstructionSequence)),
}

#[derive(Clone, Eq, Debug, PartialEq)]
pub enum Instruction {
    Push(Value),
    Fetch(String),
    LocalAssign(String)
    // Set(String, Value),
}

pub type InstructionSequence = Vec<Instruction>;

#[derive(Clone, Eq, Debug, PartialEq)]
pub struct Vm {
    instructions: InstructionSequence,
    pc: i64,
    stack: Vec<Value>,
}

fn compile_statement<'a>(statement: &'a Statement) -> Vec<Instruction> {
    match statement {
        &Statement::Assign(local, ref binding_name, ref expression) => {
            let mut instructions = compile_expression(expression);
            instructions.push(Instruction::LocalAssign(binding_name.to_owned()));
            instructions
        },
        &Statement::Call(ref binding_name, ref expressions) => vec![],
    }
}

fn compile_expression<'a>(expression: &'a Expression) -> Vec<Instruction> {
    match expression {
        &Expression::Literal(ref literal) => {
            vec![Instruction::Push(compile_literal(literal))]
        },
        &Expression::Identifier(_) => { vec![] },
        &Expression::BinOp(_, _, _) => { vec![] },
    }
}

fn compile_literal<'a>(literal: &'a Literal) -> Value {
    match literal {
        &Literal::Number(ref num) => Value::Number(num.to_owned()),
        _ => Value::CharString("not implemented".to_owned()),
    }
}

fn compile(statements: &Vec<Statement>) -> InstructionSequence {
    let mut instructions = InstructionSequence::new();

    for statement in statements.iter() {
        instructions.extend(compile_statement(&statement).iter().cloned());
    }

    instructions
}

impl Vm {
    pub fn new(source: &str) -> Vm {
        let stmts = statements(source);
        println!("{:?}", stmts);
        let instructions = compile(&stmts.unwrap());
        println!("{:?}", instructions);
        let mut vm = Vm{
            instructions: instructions,
            pc: 0,
            stack: Vec::new(),
        };
        vm
    }

    pub fn run(&mut self) -> Result<(),()> {
        Ok(())
    }
}

#[cfg(test)]
mod test_vm {
    use vm::*;
    use grammar::test_helpers::*;

    #[test]
    fn test_new() {
        let vm = Vm::new("let a = 1");
        assert_eq!(
            vec![
                Instruction::Push(Value::Number(build_ratio(1, 1))),
                Instruction::LocalAssign("a".to_owned()),
            ],
            vm.instructions
        )
    }
}
