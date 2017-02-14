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
    Closure(Box<Vec<String>>, Closure),
}

#[derive(Clone, Eq, Debug, PartialEq)]
pub enum Instruction {
    Push(Literal),
    Fetch(String),
    LocalAssign(String),
    Assign(String),
    Call(usize),
    Nop,
    // Set(String, Value),
}

pub type InstructionSequence = Vec<Instruction>;

#[derive(Clone, Eq, Debug, PartialEq)]
pub struct BindingMap {
    map: Rc<RefCell<BTreeMap<String, Value>>>,
    parent: Option<Box<BindingMap>>,
}

impl BindingMap {
    fn new(parent: Option<&BindingMap>) -> BindingMap {
        let parent = match parent {
            Some(map) => Some(Box::new(map.clone())),
            None => None,
        };
        BindingMap {
            map: Rc::new(RefCell::new(BTreeMap::new())),
            parent: parent,
        }
    }

    pub fn fetch(&self, binding_name: &String) -> Option<Value> {
        match self.map.borrow().get(binding_name) {
            Some(value) => Some(value.clone()),
            None => {
                match self.parent {
                    Some(ref parent) => parent.fetch(binding_name),
                    None => None,
                }
            },
        }
    }

    pub fn local_assign(&mut self, binding_name: &String, value: Value) {
        self.map.borrow_mut().insert(binding_name.to_owned(), value);
    }

    pub fn assign(&mut self, binding_name: &String, value: Value) {
        match self.has_binding(binding_name) {
            true => self.local_assign(binding_name, value),
            false => match self.parent {
                Some(ref mut parent) => parent.assign(binding_name, value),
                None => panic!("no such binding"),
            }
        }
    }

    pub fn has_binding(&self, binding_name: &String) -> bool {
        self.map.borrow().contains_key(binding_name)
    }
}

#[derive(Clone, Eq, Debug, PartialEq)]
pub struct Closure {
    instructions: Rc<InstructionSequence>,
    parent_bindings: BindingMap,
}

impl Closure {
    pub fn new(instructions: InstructionSequence, parent_bindings: &BindingMap) -> Closure {
        Closure {
            instructions: Rc::new(instructions),
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
    pub fn new(bindings: BindingMap) -> Frame {
        Frame {
            bindings: bindings,
            exception_handlers: Vec::new(),
        }
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
            instructions.push(Instruction::Call(expressions.len()));
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
        let instructions = compile(&stmts.unwrap());
        let mut map = BindingMap::new(None);
        let mut frame = Frame::new(map);

        let mut vm = Vm {
            instructions: Rc::new(instructions),
            pc: 0,
            stack: Vec::new(),
            frames: vec![frame],
        };
        vm
    }

    pub fn run<'b>(&'b mut self) {
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
                },
                Instruction::Assign(ref binding_name) => {
                    let value = self.stack.pop().unwrap();
                    self.frames.last_mut().unwrap().bindings.assign(binding_name, value)
                },
                Instruction::LocalAssign(ref binding_name) => {
                    let value = self.stack.pop().unwrap();
                    self.frames.last_mut().unwrap().bindings.local_assign(binding_name, value)
                },
                Instruction::Call(arg_size) => {
                    let (closure_args, closure) = match self.stack.pop() {
                        Some(Value::Closure(arg_names, closure)) => (arg_names, closure),
                        Some(x) => panic!("expected a closure, got {:?}", x),
                        None => panic!("expected a closure, got None"),
                    };
                    if arg_size != closure_args.len() {
                        panic!(
                            "wrong number of arguments, expected {:?}, got {:?}",
                            closure_args.len(),
                            arg_size
                        )
                    };
                    let mut map = BindingMap::new(Some(&closure.parent_bindings));
                    for arg_name in closure_args.iter().rev() {
                        map.local_assign(arg_name, self.stack.pop().unwrap());
                    }
                    self.frames.push(Frame::new(map));
                    self.instructions = closure.instructions.clone();
                    self.pc = 0;
                },
                Instruction::Fetch(ref binding_name) => {
                    let value = self.frames.last().unwrap().bindings.fetch(binding_name).unwrap();
                    self.stack.push(value);
                },
                _ => panic!("unknown instruction {:?}", instruction),
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
                Value::Closure(args.clone(), closure)
            },
            _ => panic!("not implemented"),
        }
    }
}

#[cfg(test)]
mod test_binding_map {
    use super::*;
    use std::collections::BTreeMap;
    use std::rc::Rc;
    use std::cell::RefCell;

    #[test]
    fn test_new() {
        assert_eq!(
            Rc::new(RefCell::new(BTreeMap::new())),
            BindingMap::new(None).map
        )
    }

    #[test]
    fn test_has_binding() {
        let mut map = BindingMap::new(None);
        assert_eq!(
            false,
            map.has_binding(&"toto".to_owned())
        )
    }

    #[test]
    fn test_local_assign() {
        let mut map = BindingMap::new(None);
        map.local_assign(&"toto".to_owned(), Value::CharString("value".to_owned()));
        assert!(map.has_binding(&"toto".to_owned()))
    }

    #[test]
    fn test_fetch() {
        let mut map = BindingMap::new(None);
        map.local_assign(&"toto".to_owned(), Value::CharString("value".to_owned()));
        assert_eq!(
            Some(Value::CharString("value".to_owned())),
            map.fetch(&"toto".to_owned())
        )
    }

    #[test]
    fn test_delegates_fetch() {
        let mut parent = BindingMap::new(None);
        parent.local_assign(&"toto".to_owned(), Value::CharString("value".to_owned()));
        let map = BindingMap::new(Some(&parent));

        assert_eq!(
            Some(Value::CharString("value".to_owned())),
            map.fetch(&"toto".to_owned())
        )
    }

    #[test]
    fn test_delegates_assign() {
        let mut parent = BindingMap::new(None);
        parent.local_assign(&"toto".to_owned(), Value::CharString("value".to_owned()));
        let mut map = BindingMap::new(Some(&parent));
        map.assign(&"toto".to_owned(), Value::CharString("new_value".to_owned()));

        assert_eq!(
            Some(Value::CharString("new_value".to_owned())),
            parent.fetch(&"toto".to_owned())
        )
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

    #[test]
    fn test_new_populates_function_instructions() {
        let vm = Vm::new("let b = def(x) do\nend\nb(1)");
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
            vm.frames.last().unwrap().bindings.fetch(&"a".to_owned()).unwrap().to_owned()
        )
    }

    #[test]
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
        vm.frames.last().unwrap().bindings.fetch(&"a".to_owned()).unwrap().to_owned()
        )
    }

    #[test]
    fn test_function_call_with_args() {
        let source =
            "\
            let a = \"\"\
            let b = \"\"\
            let x = def(c, d) do
              a = c
              b = d
            end\
            x(1, 2)";

        let mut vm = Vm::new(source);
        vm.run();
        assert_eq!(
            Value::Number(build_ratio(1, 1)),
            vm.frames.last().unwrap().bindings.fetch(&"a".to_owned()).unwrap().to_owned()
        );
        assert_eq!(
            Value::Number(build_ratio(2, 1)),
            vm.frames.last().unwrap().bindings.fetch(&"b".to_owned()).unwrap().to_owned()
        )
    }

    #[test]
    #[should_panic(expected="expected a closure")]
    fn test_calling_non_function() {
        let source =
            "let x = \"\"\
            x()";

        let mut vm = Vm::new(source);
        vm.run();
    }

    #[test]
    #[should_panic(expected="wrong number of arguments")]
    fn test_function_with_wrong_arg_count() {
        let source =
            "let x = def(a, b) do
            end\
            x(1)";

        let mut vm = Vm::new(source);
        vm.run();
    }
}
