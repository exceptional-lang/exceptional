use grammar::*;
use ast::*;
use std::collections::BTreeMap;
use std::rc::Rc;
use std::cell::RefCell;
use num::rational::BigRational;

#[derive(Clone, Eq, Debug, PartialEq)]
pub enum Value {
    Number(BigRational),
    CharString(String),
    Boolean(bool),
    Map(BTreeMap<Rc<Value>, Rc<Value>>),
    Closure(u8, Closure),
    Fn(u8, Closure),
}

#[derive(Clone, Eq, Debug, PartialEq)]
pub enum Instruction {
    Push(Literal),
    Fetch(String),
    LocalAssign(String),
    Assign(String),
    Call(u8),
    // Set(String, Value),
}

pub type InstructionSequence = Vec<Instruction>;
pub type BindingMap = Rc<RefCell<BTreeMap<String, Value>>>;

#[derive(Clone, Eq, Debug, PartialEq)]
pub struct Closure {
    instructions: Box<InstructionSequence>,
    parent_bindings: BindingMap,
}

impl Closure {
    pub fn new(instructions: InstructionSequence, parent_bindings: &BindingMap) -> Closure {
        Closure {
            instructions: Box::new(instructions),
            parent_bindings: parent_bindings.clone(),
        }
    }
}

#[derive(Clone, Eq, Debug, PartialEq)]
struct Frame {
    bindings: BindingMap,
    exception_handlers: Vec<()>,
}

impl Frame {
    pub fn new() -> Frame {
        Frame {
            bindings: Rc::new(RefCell::new(BTreeMap::new())),
            exception_handlers: Vec::new(),
        }
    }

    pub fn local_assign(&mut self, binding_name: &str, value: Value) {
        self.bindings.borrow_mut().insert(binding_name.to_owned(), value);
    }
}

fn compile_statement<'a>(statement: &'a Statement) -> InstructionSequence {
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

fn compile_expression<'a>(expression: &'a Expression) -> InstructionSequence {
    match expression {
        &Expression::Literal(ref literal) => {
            vec![Instruction::Push(literal.to_owned())]
        },
        &Expression::Identifier(ref binding_name) => {
            vec![Instruction::Fetch(binding_name.to_owned())]
        }
        _ => panic!("not implemented"),
        // &Expression::Identifier(_) => { vec![] },
        // &Expression::BinOp(_, _, _) => { vec![] },
    }
}

fn compile<'a>(statements: &'a Vec<Statement>) -> InstructionSequence {
    let mut instructions = InstructionSequence::new();

    for statement in statements.iter() {
        instructions.extend(compile_statement(&statement).iter().cloned());
    }

    instructions
}

#[derive(Clone, Eq, Debug, PartialEq)]
pub struct Vm {
    instructions: Rc<InstructionSequence>,
    pc: usize,
    stack: Vec<Value>,
    frames: Vec<Frame>,
}

impl Vm {
    pub fn new(source: &str) -> Vm {
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

    pub fn run<'b, 'c>(&'b mut self) {
        loop {
            let insn_result = Vm::next_instruction(self);
            let instruction;

            match insn_result {
                Some(i) => instruction = i,
                None => break,
            }

            match instruction {
                Instruction::Push(ref value) => {
                    let top_bindings = &mut self.frames.last_mut().unwrap().bindings;
                    self.stack.push(Vm::literal_to_value(value, top_bindings))
                    // Vm::push_value(&mut self.stack, top_activation, value)
                },
                Instruction::LocalAssign(ref binding_name) => {
                    let value = self.stack.pop().unwrap();
                    self.frames.last_mut().unwrap().local_assign(binding_name, value)
                },
                Instruction::Call(arg_size) => {
                    let (closure_arg_size, closure) = match self.stack.pop() {
                        Some(Value::Closure(arg_size, closure)) => (arg_size, closure),
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
                _ => ()//panic!("unknown instruction {:?}", instruction)
            };
        };
    }

    fn next_instruction(vm: &mut Vm) -> Option<Instruction> {
        let instruction = match vm.instructions.get(vm.pc) {
            Some(i) => Some(i.clone()),
            None => None,
        };

        vm.pc += 1;

        instruction
    }

    fn literal_to_value<'b>(literal: &'b Literal, top_bindings: &BindingMap) -> Value {
        match literal {
            &Literal::Number(ref num) => Value::Number(num.to_owned()),
            &Literal::CharString(ref str) => Value::CharString(str.to_string()),
            &Literal::Fn(ref args, ref statements) => {
                // Statements should be compiled ahead of time
                let closure = Closure::new(compile(&statements), top_bindings);
                Value::Closure(args.len() as u8, closure)
            },
            _ => panic!("not implemented"),
        }
    }
}

#[cfg(test)]
mod test_vm {
    use super::*;
    use ast::*;
    use std::rc::Rc;
    use grammar::test_helpers::*;

    #[test]
    fn test_new_populates_basic_instructions() {
        let vm = Vm::new("let a = 1\nlet b = \"\"\n");
        assert_eq!(
            Rc::new(vec![
                Instruction::Push(Literal::Number(build_ratio(1, 1))),
                Instruction::LocalAssign("a".to_owned()),
                Instruction::Push(Literal::CharString("".to_string())),
                Instruction::LocalAssign("b".to_owned()),
            ]),
            vm.instructions
        )
    }

    fn test_new_populates_function_instructions() {
        let vm = Vm::new("let b = def(x)\nend\nb(1)");
        assert_eq!(
            Rc::new(vec![
                Instruction::Push(Literal::Fn(Box::new(vec!["x".to_owned()]), Box::new(vec![]))),
                Instruction::LocalAssign("b".to_owned()),
                Instruction::Push(Literal::Number(build_ratio(1, 1))),
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
            Value::Number(build_ratio(1, 1)),
            vm.frames.last().unwrap().bindings.borrow().get("a").unwrap().to_owned()
        );
        ()
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
            Value::Number(build_ratio(1, 1)),
            vm.frames.last().unwrap().bindings.borrow().get("a").unwrap().to_owned()
        );
        ()
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
