use grammar::*;
use ast::*;
use std::collections::BTreeMap;
use std::rc::Rc;
use num::rational::BigRational;

#[derive(Clone, Eq, Debug, PartialEq)]
pub enum Value<'a> {
    Number(BigRational),
    CharString(String),
    Boolean(bool),
    Map(BTreeMap<Rc<Value<'a>>, Rc<Value<'a>>>),
    Fn(u8, Closure<'a>),
}

#[derive(Clone, Eq, Debug, PartialEq)]
pub enum Instruction<'a> {
    Push(Value<'a>),
    Fetch(String),
    LocalAssign(String),
    Assign(String),
    Call(u8),
    // Set(String, Value),
}

pub type InstructionSequence<'a> = Vec<Instruction<'a>>;

#[derive(Clone, Eq, Debug, PartialEq)]
pub struct Closure<'a> {
    instructions: Box<InstructionSequence<'a>>,
    parent_activation: &'a Activation<'a>,
}

impl<'a> Closure<'a> {
    pub fn new(instructions: InstructionSequence<'a>, parent_activation: &'a Activation) -> Closure<'a> {
        Closure {
            instructions: Box::new(instructions),
            parent_activation: parent_activation,
        }
    }
}

#[derive(Clone, Eq, Debug, PartialEq)]
pub struct Activation<'a> {
    // <String> sucks because I'm copying all the memory all the time.
    // Figure this shit out
    local_bindings: BTreeMap<String, Rc<Value<'a>>>,
}

impl<'a> Activation<'a> {
    pub fn new() -> Activation<'a> {
        Activation {
            local_bindings: BTreeMap::new(),
        }
    }
}

#[derive(Clone, Eq, Debug, PartialEq)]
struct Frame<'a> {
    activation: Activation<'a>,
    exception_handlers: Vec<()>,
}

impl<'a> Frame<'a> {
    pub fn new() -> Frame<'a> {
        Frame {
            activation: Activation::new(),
            exception_handlers: Vec::new(),
        }
    }

    pub fn local_assign(&mut self, binding_name: &str, value: Value<'a>) {
        self.activation.local_bindings.insert(binding_name.to_owned(), Rc::new(value));
    }
}

fn compile_statement<'a, 'b>(statement: &'a Statement) -> InstructionSequence<'b> {
    match statement {
        &Statement::Assign(local, ref binding_name, ref expression) => {
            let mut instructions = compile_expression(expression);
            let instruction = match local {
                true => Instruction::LocalAssign(binding_name.to_owned()),
                false => Instruction::Assign(binding_name.to_owned()),
            };
            instructions.push(instruction);
            instructions
        },
        &Statement::Call(ref binding_name, ref expressions) => {
            let mut instructions = expressions
                .iter()
                .flat_map(|exp| compile_expression(exp))
                .collect::<InstructionSequence>();
            instructions.push(Instruction::Fetch(binding_name.to_owned()));
            instructions.push(Instruction::Call(expressions.len() as u8));
            instructions
        },
        // _ => panic!("not implemented"),
    }
}

fn compile_expression<'a, 'b>(expression: &'a Expression) -> InstructionSequence<'b> {
    match expression {
        &Expression::Literal(ref literal) => {
            vec![Instruction::Push(compile_literal(literal))]
        },
        &Expression::Identifier(ref binding_name) => {
            vec![Instruction::Fetch(binding_name.to_owned())]
        }
        _ => panic!("not implemented"),
        // &Expression::Identifier(_) => { vec![] },
        // &Expression::BinOp(_, _, _) => { vec![] },
    }
}

fn compile_literal<'a, 'b>(literal: &'a Literal) -> Value<'b> {
    match literal {
        &Literal::Number(ref num) => Value::Number(num.to_owned()),
        &Literal::CharString(ref str) => Value::CharString(str.to_string()),
        // &Literal::Fn(ref args, ref statements) => {
        //     let closure = Closure::new(compile(&statements));
        //     Value::Fn(args.len() as u8, closure)
        // },
        _ => panic!("not implemented"),
    }
}

fn compile<'a, 'b>(statements: &'a Vec<Statement>) -> InstructionSequence<'b> {
    let mut instructions = InstructionSequence::new();

    for statement in statements.iter() {
        instructions.extend(compile_statement(&statement).iter().cloned());
    }

    instructions
}

#[derive(Clone, Eq, Debug, PartialEq)]
pub struct Vm<'a> {
    instructions: Rc<InstructionSequence<'a>>,
    pc: usize,
    stack: Vec<Value<'a>>,
    frames: Vec<Frame<'a>>,
}

impl<'a> Vm<'a> {
    pub fn new(source: &str) -> Vm<'a> {
        let stmts = statements(source);
        // println!("{:?}", stmts);
        let instructions = compile(&stmts.unwrap());
        // println!("{:?}", instructions);
        let mut frame = Frame::new();

        let mut vm = Vm{
            instructions: Rc::new(instructions),
            pc: 0,
            stack: Vec::new(),
            frames: vec![frame],
        };
        vm
    }

    pub fn run(&mut self) {
        for instruction in self.instructions.iter() {
            match instruction {
                &Instruction::Push(ref value) => {
                    self.stack.push(value.to_owned())
                },
                &Instruction::LocalAssign(ref binding_name) => {
                    let value = self.stack.pop().unwrap();
                    self.frames.last_mut().unwrap().local_assign(binding_name, value)
                },
                &Instruction::Call(arg_size) => {
                    let (closure_arg_size, closure) = match self.stack.pop() {
                        Some(Value::Fn(arg_size, closure)) => (arg_size, closure),
                        _ => panic!("expected a closure"),
                    };
                    // if arg_size != closure_arg_size {
                    //     panic!("wrong number of arguments")
                    // };
                    // make sure closure
                    // verify args
                    // push frame
                    // make new activation
                    // assign values
                    // change iseq
                },
                _ => ()
            };
        };
    }
}

#[cfg(test)]
mod test_vm {
    use super::*;
    use std::rc::Rc;
    use grammar::test_helpers::*;

    #[test]
    fn test_new_populates_basic_instructions() {
        let vm = Vm::new("let a = 1\nlet b = \"\"\n");
        assert_eq!(
            Rc::new(vec![
                Instruction::Push(Value::Number(build_ratio(1, 1))),
                Instruction::LocalAssign("a".to_owned()),
                Instruction::Push(Value::CharString("".to_string())),
                Instruction::LocalAssign("b".to_owned()),
            ]),
            vm.instructions
        )
    }

    fn test_new_populates_function_instructions() {
        let vm = Vm::new("let b = def(x)\nend\nb(1)");
        let activation = Activation::new();
        let closure = Closure::new(vec![], &activation);
        assert_eq!(
            Rc::new(vec![
                Instruction::Push(Value::Fn(1, closure)),
                Instruction::LocalAssign("b".to_owned()),
                Instruction::Push(Value::Number(build_ratio(1, 1))),
                Instruction::Fetch("b".to_owned()),
                Instruction::Call(1),
            ]),
            vm.instructions
        )
    }

    #[test]
    fn test_run_simple() {
        let mut vm = Vm::new("let a = 1");
        vm.run();
        assert_eq!(
            Rc::new(Value::Number(build_ratio(1, 1))),
            vm.frames.last().unwrap().activation.local_bindings.get("a").unwrap().to_owned()
        )
    }

    #[test]
    #[ignore]
    fn test_function_call() {
        let source =
            "let a = \"\"\
            let x = def() do
              a = 1
            end\
            x()";

        let mut vm = Vm::new(source);
        vm.run();
        assert_eq!(
            Rc::new(Value::Number(build_ratio(1, 1))),
            vm.frames.last().unwrap().activation.local_bindings.get("a").unwrap().to_owned()
        )
    }

    #[test]
    #[should_panic]
    fn test_calling_non_function() {
        let source =
            "let x = \"\"\
            x()";

        let mut vm = Vm::new(source);
        vm.run();
    }

    #[test]
    #[should_panic]
    fn test_function_with_wrong_arg_count() {
        let source =
            "let x = def(a, b) do
              a = 1
            end\
            x(1, 1)";

        let mut vm = Vm::new(source);
        vm.run();
    }
}
