use grammar::*;
use ast::*;
use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::rc::Rc;
use std::cell::RefCell;
use num::rational::BigRational;

#[derive(Clone, Eq, Debug, PartialEq, PartialOrd, Ord)]
pub enum Value {
    Number(BigRational),
    CharString(String),
    Boolean(bool),
    Map(BTreeMap<Rc<Value>, Rc<Value>>),
    Closure(Box<Vec<String>>, Closure),
}

#[derive(Clone, Eq, Debug, PartialEq, PartialOrd, Ord)]
pub enum Instruction {
    Push(Literal),
    Fetch(String),
    LocalAssign(String),
    Assign(String),
    Call(usize),
    MakeMap(usize),
    Rescue(Rc<Pattern>, Rc<InstructionSequence>),
    Raise,
    Nop,
}

pub type InstructionSequence = Vec<Instruction>;

#[derive(Clone, Eq, Debug, PartialEq, PartialOrd, Ord)]
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
            }
        }
    }

    pub fn local_assign(&mut self, binding_name: &String, value: Value) {
        self.map.borrow_mut().insert(binding_name.to_owned(), value);
    }

    pub fn assign(&mut self, binding_name: &String, value: Value) {
        match self.has_binding(binding_name) {
            true => self.local_assign(binding_name, value),
            false => {
                match self.parent {
                    Some(ref mut parent) => parent.assign(binding_name, value),
                    None => panic!("no such binding"),
                }
            }
        }
    }

    pub fn has_binding(&self, binding_name: &String) -> bool {
        self.map.borrow().contains_key(binding_name)
    }
}

#[derive(Clone, Eq, Debug, PartialEq, PartialOrd, Ord)]
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
struct ExceptionHandler {
    pattern: Rc<Pattern>,
    instructions: Rc<InstructionSequence>,
}

type MatchedBindings = Option<BTreeMap<String, Value>>;

impl ExceptionHandler {
    pub fn new(pattern: Rc<Pattern>, instructions: Rc<InstructionSequence>) -> ExceptionHandler {
        ExceptionHandler {
            pattern: pattern,
            instructions: instructions,
        }
    }
    pub fn matches(&self, value: Value) -> MatchedBindings {
        ExceptionHandler::match_pattern(&*self.pattern, &value)
    }

    fn match_pattern(pattern: &Pattern, value: &Value) -> MatchedBindings {
        match pattern {
            &Pattern::Number(ref ratio) => ExceptionHandler::match_number(ratio, value),
            &Pattern::CharString(ref string) => ExceptionHandler::match_string(string, value),
            &Pattern::Boolean(bool) => ExceptionHandler::match_bool(bool, value),
            &Pattern::Map(ref pairs) => ExceptionHandler::match_map(pairs, value),
            &Pattern::Identifier(ref name) => ExceptionHandler::match_identifier(name, value),
        }
    }

    fn match_number(ratio: &BigRational, value: &Value) -> MatchedBindings {
        match value {
            &Value::Number(ref number) => {
                match ratio.eq(number) {
                    true => Some(BTreeMap::new()),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn match_string(string: &String, value: &Value) -> MatchedBindings {
        match value {
            &Value::CharString(ref str) => {
                match str.eq(string) {
                    true => Some(BTreeMap::new()),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn match_bool(b: bool, value: &Value) -> MatchedBindings {
        match value {
            &Value::Boolean(other_bool) => {
                match b == other_bool {
                    true => Some(BTreeMap::new()),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    fn match_map(pairs: &Vec<(Pattern, Pattern)>, value: &Value) -> MatchedBindings {
        match value {
            &Value::Map(ref btreemap) => {
                let mut bindings: BTreeMap<String, Value> = BTreeMap::new();
                for &(ref key, ref pattern_value) in pairs.iter() {
                    let key_as_value = ExceptionHandler::pattern_key_to_value(key);

                    let maybe_nested_bindings = btreemap.get(&Rc::new(key_as_value))
                        .and_then(|value| ExceptionHandler::match_pattern(pattern_value, &**value));

                    if let None = maybe_nested_bindings {
                        return None;
                    }

                    for (key, value) in maybe_nested_bindings.unwrap().iter() {
                        match bindings.entry(key.to_owned()) {
                            Entry::Occupied(entry) => {
                                if entry.get() != value {
                                    return None;
                                }
                            }
                            Entry::Vacant(v) => {
                                v.insert(value.to_owned());
                            }
                        }
                    }
                }
                Some(bindings)
            }
            _ => None,
        }
    }

    fn match_identifier(name: &String, value: &Value) -> MatchedBindings {
        let bindings: BTreeMap<_, _> = vec![(name.to_owned(), value.clone())].into_iter().collect();
        Some(bindings)
    }

    fn pattern_key_to_value(pattern: &Pattern) -> Value {
        match pattern {
            &Pattern::Number(ref ratio) => Value::Number(ratio.to_owned()),
            &Pattern::CharString(ref str) => Value::CharString(str.to_owned()),
            &Pattern::Boolean(bool) => Value::Boolean(bool),
            ref pat => panic!("pattern cannot be used as key ({:?})", pat),
        }
    }
}

#[derive(Clone, Eq, Debug, PartialEq)]
struct Frame {
    bindings: BindingMap,
    exception_handlers: Vec<ExceptionHandler>,
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
        }
        &Statement::Call(ref binding_name, ref expressions) => {
            let mut instructions = expressions.iter()
                .flat_map(|exp| compile_expression(exp))
                .collect::<InstructionSequence>();
            instructions.push(Instruction::Fetch(binding_name.to_owned()));
            instructions.push(Instruction::Call(expressions.len()));
            instructions
        }
        &Statement::Rescue(ref pattern, ref statements) => {
            let mut instructions = statements.iter()
                .flat_map(|stmt| compile_statement(stmt))
                .collect::<InstructionSequence>();

            vec![Instruction::Rescue(Rc::new(pattern.clone()), Rc::new(instructions))]
        }
        &Statement::Raise(ref expression) => {
            let mut instructions = compile_expression(expression);
            instructions.push(Instruction::Raise);
            instructions
        }
        // s => panic!("not implemented: {:?}", s),
    }
}

fn compile_expression<'a>(expression: &'a Expression) -> InstructionSequence {
    match expression {
        &Expression::Literal(ref literal) => {
            match literal {
                &Literal::Map(ref pairs) => {
                    let mut map_instructions = pairs.iter()
                        .flat_map(|&(ref key, ref value)| {
                            let mut insns = compile_expression(key);
                            insns.extend(compile_expression(value).iter().cloned());
                            insns
                        })
                        .collect::<InstructionSequence>();

                    map_instructions.push(Instruction::MakeMap(pairs.len()));
                    map_instructions
                }
                _ => vec![Instruction::Push(literal.to_owned())],
            }
        }
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
                }
                Instruction::Assign(ref binding_name) => {
                    let value = self.stack.pop().unwrap();
                    self.frames.last_mut().unwrap().bindings.assign(binding_name, value)
                }
                Instruction::LocalAssign(ref binding_name) => {
                    let value = self.stack.pop().unwrap();
                    self.frames.last_mut().unwrap().bindings.local_assign(binding_name, value)
                }
                Instruction::Call(arg_size) => {
                    let closure_info = match self.stack.pop() {
                        Some(Value::Closure(arg_names, closure)) => Ok((arg_names, closure)),
                        Some(x) => Err(format!("expected a closure, got {:?}", x)),
                        None => Err(format!("expected a closure, got None")),
                    };

                    let new_stack_length = {
                        self.stack.len() - arg_size
                    };
                    let mut args = self.stack.split_off(new_stack_length);
                    let (closure_args, closure) = match closure_info {
                        Ok(info) => info,
                        Err(m) => panic!(m),
                    };
                    if arg_size != closure_args.len() {
                        panic!("wrong number of arguments, expected {:?}, got {:?}",
                               closure_args.len(),
                               arg_size)
                    };
                    let mut map = BindingMap::new(Some(&closure.parent_bindings));
                    for arg_name in closure_args.iter().rev() {
                        map.local_assign(arg_name, args.pop().unwrap());
                    }
                    self.frames.push(Frame::new(map));
                    self.instructions = closure.instructions.clone();
                    self.pc = 0;
                }
                Instruction::Fetch(ref binding_name) => {
                    let value = self.frames.last().unwrap().bindings.fetch(binding_name).unwrap();
                    self.stack.push(value);
                }
                Instruction::MakeMap(size) => {
                    let map = (0..size)
                        .into_iter()
                        .map(|_| {
                            let value = self.stack.pop().unwrap();
                            let key = self.stack.pop().unwrap();
                            (Rc::new(key), Rc::new(value))
                        })
                        .collect();
                    self.stack.push(Value::Map(map))
                }
                Instruction::Rescue(ref pattern, ref iseq) => {
                    self.frames
                        .last_mut()
                        .unwrap()
                        .exception_handlers
                        .push(ExceptionHandler::new(pattern.clone(), iseq.clone()))
                }
                Instruction::Raise => {
                    let raised_value = self.stack.pop().unwrap();
                    for frame in self.frames.iter().rev() {
                        let handlers = frame.exception_handlers
                            .iter()
                            .filter_map(|handler| match handler.matches(raised_value.clone()) {
                                Some(bindings) => Some((handler, bindings)),
                                None => None,
                            });
                        println!("{:?}", handlers);
                    }
                }
                _ => panic!("unknown instruction {:?}", instruction),
            };
        }
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
            }
            _ => panic!("not implemented"),
        }
    }
}

#[cfg(test)]
mod test_exception_handler {
    use super::*;
    use std::rc::Rc;
    use grammar::test_helpers::*;

    #[test]
    fn matches_numbers() {
        let handler = ExceptionHandler::new(Rc::new(Pattern::Number(build_ratio(1, 1))),
                                            Rc::new(vec![]));
        assert_eq!(Some(BTreeMap::new()),
                   handler.matches(Value::Number(build_ratio(1, 1))));
        assert_eq!(None, handler.matches(Value::Number(build_ratio(2, 1))));
        assert_eq!(None, handler.matches(Value::CharString("toto".to_owned())))
    }

    #[test]
    fn matches_strings() {
        let handler = ExceptionHandler::new(Rc::new(Pattern::CharString("toto".to_owned())),
                                            Rc::new(vec![]));
        assert_eq!(Some(BTreeMap::new()),
                   handler.matches(Value::CharString("toto".to_owned())));
        assert_eq!(None, handler.matches(Value::CharString("titi".to_owned())));
        assert_eq!(None, handler.matches(Value::Number(build_ratio(1, 1))))
    }

    #[test]
    fn matches_bools() {
        let handler = ExceptionHandler::new(Rc::new(Pattern::Boolean(false)), Rc::new(vec![]));
        assert_eq!(Some(BTreeMap::new()),
                   handler.matches(Value::Boolean(false)));
        assert_eq!(None, handler.matches(Value::Boolean(true)));
        assert_eq!(None, handler.matches(Value::CharString("toto".to_owned())));
    }

    #[test]
    fn matches_simple_map() {
        let handler =
            ExceptionHandler::new(Rc::new(Pattern::Map(vec![(Pattern::Number(build_ratio(1,
                                                                                         1)),
                                                             Pattern::Identifier("toto"
                                                                 .to_owned()))])),
                                  Rc::new(vec![]));
        assert_eq!(Some(vec![("toto".to_owned(), Value::CharString("titi".to_owned()))]
                       .into_iter()
                       .collect()),
                   handler.matches(Value::Map(vec![(Rc::new(Value::Number(build_ratio(1, 1))),
                                                    Rc::new(Value::CharString("titi"
                                                        .to_owned())))]
                       .into_iter()
                       .collect())));
        assert_eq!(None,
                   handler.matches(Value::Map(vec![(Rc::new(Value::CharString("titi"
                                                        .to_owned())),
                                                    Rc::new(Value::Number(build_ratio(2, 1))))]
                       .into_iter()
                       .collect())));
        assert_eq!(None, handler.matches(Value::Boolean(false)));
    }

    #[test]
    fn matches_maps_with_multiple_bindings_of_equal_values() {
        let handler =
            ExceptionHandler::new(Rc::new(Pattern::Map(vec![(Pattern::Number(build_ratio(1,
                                                                                         1)),
                                                             Pattern::Identifier("toto"
                                                                 .to_owned())),
                                                            (Pattern::Number(build_ratio(2,
                                                                                         1)),
                                                             Pattern::Identifier("toto"
                                                                 .to_owned()))])),
                                  Rc::new(vec![]));
        assert_eq!(Some(vec![("toto".to_owned(), Value::CharString("titi".to_owned()))]
                       .into_iter()
                       .collect()),
                   handler.matches(Value::Map(vec![(Rc::new(Value::Number(build_ratio(1, 1))),
                                                    Rc::new(Value::CharString("titi"
                                                        .to_owned()))),
                                                   (Rc::new(Value::Number(build_ratio(2, 1))),
                                                    Rc::new(Value::CharString("titi"
                                                        .to_owned())))]
                       .into_iter()
                       .collect())));
        assert_eq!(None,
                   handler.matches(Value::Map(vec![(Rc::new(Value::Number(build_ratio(1, 1))),
                                                    Rc::new(Value::CharString("titi"
                                                        .to_owned()))),
                                                   (Rc::new(Value::Number(build_ratio(2, 1))),
                                                    Rc::new(Value::CharString("foo"
                                                        .to_owned())))]
                       .into_iter()
                       .collect())));
    }

    #[test]
    fn matches_recursive_maps() {
        let handler =
            ExceptionHandler::new(Rc::new(Pattern::Map(vec![(Pattern::Number(build_ratio(1,
                                                                                         1)),
                                                             Pattern::Map(vec![
                        (
                            Pattern::Number(build_ratio(2, 1)),
                            Pattern::Identifier("toto".to_owned())
                        )
                    ]))])),
                                  Rc::new(vec![]));
        assert_eq!(Some(vec![("toto".to_owned(), Value::CharString("titi".to_owned()))]
                       .into_iter()
                       .collect()),
                   handler.matches(Value::Map(vec![(Rc::new(Value::Number(build_ratio(1, 1))),
                                                    Rc::new(Value::Map(vec![
                                (
                                    Rc::new(Value::Number(build_ratio(2, 1))),
                                    Rc::new(Value::CharString("titi".to_owned()))
                                )
                            ]
                                                        .into_iter()
                                                        .collect())))]
                       .into_iter()
                       .collect())));
    }

    #[test]
    fn matches_identifier() {
        let handler = ExceptionHandler::new(Rc::new(Pattern::Identifier("toto".to_owned())),
                                            Rc::new(vec![]));
        assert_eq!(Some(vec![("toto".to_owned(), Value::CharString("titi".to_owned()))]
                       .into_iter()
                       .collect()),
                   handler.matches(Value::CharString("titi".to_owned())));
        assert_eq!(Some(vec![("toto".to_owned(), Value::Number(build_ratio(1, 1)))]
                       .into_iter()
                       .collect()),
                   handler.matches(Value::Number(build_ratio(1, 1))))
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
        assert_eq!(Rc::new(RefCell::new(BTreeMap::new())),
                   BindingMap::new(None).map)
    }

    #[test]
    fn test_has_binding() {
        let mut map = BindingMap::new(None);
        assert_eq!(false, map.has_binding(&"toto".to_owned()))
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
        assert_eq!(Some(Value::CharString("value".to_owned())),
                   map.fetch(&"toto".to_owned()))
    }

    #[test]
    fn test_delegates_fetch() {
        let mut parent = BindingMap::new(None);
        parent.local_assign(&"toto".to_owned(), Value::CharString("value".to_owned()));
        let map = BindingMap::new(Some(&parent));

        assert_eq!(Some(Value::CharString("value".to_owned())),
                   map.fetch(&"toto".to_owned()))
    }

    #[test]
    fn test_delegates_assign() {
        let mut parent = BindingMap::new(None);
        parent.local_assign(&"toto".to_owned(), Value::CharString("value".to_owned()));
        let mut map = BindingMap::new(Some(&parent));
        map.assign(&"toto".to_owned(),
                   Value::CharString("new_value".to_owned()));

        assert_eq!(Some(Value::CharString("new_value".to_owned())),
                   parent.fetch(&"toto".to_owned()))
    }
}

#[cfg(test)]
mod test_vm {
    use super::*;
    use std::rc::Rc;
    use grammar::test_helpers::*;

    #[test]
    fn test_new_populates_basic_instructions() {
        let source = "let a = 1
        let b = \"\"
        rescue(id) do
            b = 1
        end
        raise(\"a\")
        ";
        let vm = Vm::new(source);
        assert_eq!(vm.instructions,
                   Rc::new(vec![Instruction::Push(l_number(1, 1)),
                                Instruction::LocalAssign("a".to_owned()),
                                Instruction::Push(l_string("")),
                                Instruction::LocalAssign("b".to_owned()),
                                Instruction::Rescue(Rc::new(p_ident("id")),
                                                    Rc::new(vec![
                        Instruction::Push(l_number(1, 1)),
                        Instruction::Assign("b".to_owned()),
                    ])),
                                Instruction::Push(l_string("a")),
                                Instruction::Raise]))
    }

    #[test]
    fn test_new_populates_function_instructions() {
        let source =
            "\
            let a = { 1 => 2 }
            let b = def(x) doendb(1)";
        let vm = Vm::new(source);
        assert_eq!(Rc::new(vec![Instruction::Push(Literal::Number(build_ratio(1, 1))),
                                Instruction::Push(Literal::Number(build_ratio(2, 1))),
                                Instruction::MakeMap(1),
                                Instruction::LocalAssign("a".to_owned()),
                                Instruction::Push(Literal::Fn(Box::new(vec!["x".to_owned()]),
                                                              Box::new(vec![]))),
                                Instruction::LocalAssign("b".to_owned()),
                                Instruction::Push(Literal::Number(build_ratio(1, 1))),
                                Instruction::Fetch("b".to_owned()),
                                Instruction::Call(1)]),
                   vm.instructions)
    }

    #[test]
    fn test_run_simple() {
        let mut vm = Vm::new("let a = 1\
        let b = { \"a\" => 1 }");
        vm.run();
        assert_eq!(Value::Number(build_ratio(1, 1)),
                   vm.frames.last().unwrap().bindings.fetch(&"a".to_owned()).unwrap().to_owned());
        let expected_map = {
            let mut map = BTreeMap::new();
            map.insert(Rc::new(Value::CharString("a".to_owned())),
                       Rc::new(Value::Number(build_ratio(1, 1))));
            map
        };
        assert_eq!(Value::Map(expected_map),
                   vm.frames.last().unwrap().bindings.fetch(&"b".to_owned()).unwrap().to_owned())
    }

    #[test]
    fn test_function_call() {
        let source = "let a = \"\"
            let x = def() do
              a = 1
            end
            x()";

        let mut vm = Vm::new(source);
        vm.run();
        assert_eq!(Value::Number(build_ratio(1, 1)),
                   vm.frames.last().unwrap().bindings.fetch(&"a".to_owned()).unwrap().to_owned())
    }

    #[test]
    fn test_function_call_with_args() {
        let source = "
            let a = \"\"
            let b = \"\"
            let x = def(c, d) do
              a = c
              b = d
            end
            x(1, 2)";

        let mut vm = Vm::new(source);
        vm.run();
        assert_eq!(Value::Number(build_ratio(1, 1)),
                   vm.frames.last().unwrap().bindings.fetch(&"a".to_owned()).unwrap().to_owned());
        assert_eq!(Value::Number(build_ratio(2, 1)),
                   vm.frames.last().unwrap().bindings.fetch(&"b".to_owned()).unwrap().to_owned())
    }

    #[test]
    #[should_panic(expected="expected a closure")]
    fn test_calling_non_function() {
        let source = "
        let x = \"\"
        x()";

        let mut vm = Vm::new(source);
        vm.run();
    }

    #[test]
    #[should_panic(expected="wrong number of arguments")]
    fn test_function_with_wrong_arg_count() {
        let source = "
            let x = def(a, b) do
            end
            x(1)";

        let mut vm = Vm::new(source);
        vm.run();
    }

    #[test]
    fn test_basic_rescue() {
        let source =
            "let a = \"\"
            rescue(id) do
                a = id
            end
            let x = def(b) do
                raise(b)
            end
            x(1)";

        let mut vm = Vm::new(source);
        vm.run();
        assert_eq!(Value::Number(build_ratio(1, 1)),
                   vm.frames.last().unwrap().bindings.fetch(&"a".to_owned()).unwrap().to_owned())
    }
}
