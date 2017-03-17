use grammar::*;
use ast::*;
use compiler::*;
use instructions::*;
use binding_map::BindingMap;
use value::Value;
use closure::Closure;

use std::collections::BTreeMap;
use std::collections::btree_map::Entry;
use std::rc::Rc;
use std::cell::RefCell;
use num::rational::{BigRational, Ratio};
use num::bigint::{BigInt, ToBigInt};
use num::{range, ToPrimitive, Zero};

pub type BinopResult = Result<Value, String>;

#[derive(Clone, Eq, Debug, PartialEq)]
struct ExceptionHandler {
    pattern: Rc<Pattern>,
    closure: Closure,
}

type MatchedBindings = Option<BTreeMap<String, Value>>;

impl ExceptionHandler {
    pub fn new(pattern: Rc<Pattern>, closure: Closure) -> ExceptionHandler {
        ExceptionHandler {
            pattern: pattern,
            closure: closure,
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

                    let maybe_nested_bindings = btreemap.borrow()
                        .get(&Rc::new(key_as_value))
                        .and_then(|value| ExceptionHandler::match_pattern(pattern_value, value));

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
        let map = BindingMap::new(None);
        let frame = Frame::new(map);

        let vm = Vm {
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

            instruction = if let Some(i) = insn_result {
                println!("next instruction: {:?}", i);
                i
            } else {
                println!("instruction not found, terminating");
                break;
            };

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
                    let local_bindings = (*closure_args)
                        .clone()
                        .into_iter()
                        .rev()
                        .map(|arg_name| (arg_name, args.pop().unwrap()))
                        .collect();
                    let mut map = closure.init_map(local_bindings);
                    self.reset_instructions(closure.instructions.clone(), map)
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
                            (key, value)
                        })
                        .collect();
                    self.stack.push(Value::Map(Rc::new(RefCell::new(map))))
                }
                Instruction::Rescue(ref pattern, ref iseq) => {
                    let top_bindings = {
                        &mut self.frames.last_mut().unwrap().bindings.clone()
                    };
                    let closure = Closure::new(iseq.clone(), top_bindings);
                    self.frames
                        .last_mut()
                        .unwrap()
                        .exception_handlers
                        .push(ExceptionHandler::new(pattern.clone(), closure))
                }
                Instruction::Raise => {
                    let raised_value = self.stack.pop().unwrap();
                    self.raise(raised_value);
                }
                Instruction::Add => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();

                    if let Ok(result) = Vm::add(left, right) {
                        self.stack.push(result);
                    } else {
                        // TODO: Raise
                    }
                }
                Instruction::Sub => {
                    let right = self.stack.pop().unwrap();
                    let left = self.stack.pop().unwrap();

                    if let Ok(result) = Vm::sub(left, right) {
                        self.stack.push(result);
                    } else {
                        // TODO: Raise
                    }
                }
                Instruction::IndexAccess => {
                    let property = self.stack.pop().unwrap();
                    let target = self.stack.pop().unwrap();

                    match target {
                        Value::Map(ref map) => {
                            if let Some(value) = map.borrow().get(&property) {
                                self.stack.push((*value).clone());
                            } else {
                                panic!("no value for {:?}", target); // TODO: Raise
                            }
                        }
                        v => panic!("can't use index access for {:?}", v), // TODO: Raise
                    };
                }
                Instruction::IndexAssign => {
                    let value = self.stack.pop().unwrap();
                    let property = self.stack.pop().unwrap();
                    let mut target = self.stack.pop().unwrap();

                    match target {
                        Value::Map(ref mut map) => {
                            map.borrow_mut().insert(property, value);
                        }
                        v => panic!("can't use index access for {:?}", v), // TODO: Raise
                    };
                }
                _ => panic!("unknown instruction {:?}", instruction),
            };
        }
    }

    fn add(left: Value, right: Value) -> BinopResult {
        match (left, right) {
            (Value::Number(lratio), Value::Number(rratio)) => Ok(Value::Number(lratio + rratio)),
            (Value::CharString(lstr), Value::CharString(rstr)) => {
                Ok(Value::CharString(lstr + &rstr))
            }
            (Value::Closure(_, _), Value::Closure(_, _)) => {
                Err("Addition of closures is not supported".to_owned())
            }
            (Value::Boolean(lbool), Value::Boolean(rbool)) => Ok(Value::Boolean(lbool || rbool)),
            (Value::Map(lmap), Value::Map(rmap)) => {
                let mut result = (*lmap.borrow()).clone();
                let mut rclone = (*rmap.borrow()).clone();
                result.append(&mut rclone);
                Ok(Value::Map(Rc::new(RefCell::new(result))))
            }
            (l, r) => Err(format!("Unsupported operation + for {:?} and {:?}", l, r)),
        }
    }

    fn sub(left: Value, right: Value) -> BinopResult {
        match (left, right) {
            (Value::Number(lratio), Value::Number(rratio)) => Ok(Value::Number(lratio - rratio)),
            (Value::CharString(lstr), Value::CharString(rstr)) => {
                if let Some(index) = (&lstr).rfind(&rstr) {
                    Ok(Value::CharString(lstr[0..index].to_owned() + &lstr[(index + rstr.len())..]))
                } else {
                    Ok(Value::CharString("".to_owned()))
                }
            }
            (Value::Closure(_, _), Value::Closure(_, _)) => {
                Err(format!("Subtraction of closures is not supported"))
            }
            (Value::Boolean(lbool), Value::Boolean(rbool)) => Ok(Value::Boolean(lbool ^ rbool)),
            (Value::Map(ref lmap), Value::Map(ref rmap)) => {
                let result = lmap.borrow()
                    .clone()
                    .into_iter()
                    .filter(|&(ref key, ref value)| if let Some(rvalue) = rmap.borrow().get(key) {
                        rvalue != value
                    } else {
                        true
                    })
                    .collect::<BTreeMap<_, _>>();
                Ok(Value::Map(Rc::new(RefCell::new(result))))
            }
            (l, r) => Err(format!("Unsupported operation - for {:?} and {:?}", l, r)),
        }
    }

    fn mul(left: Value, right: Value) -> BinopResult {
        match (left, right) {
            (Value::Number(lratio), Value::Number(rratio)) => Ok(Value::Number(lratio * rratio)),
            (Value::CharString(str), Value::Number(ratio)) => {
                let extended: String = range(BigInt::from(0), ratio.ceil().to_integer())
                    .map(|_| &*str)
                    .collect::<Vec<&str>>()
                    .join("");
                let truncation = (ratio.ceil() - ratio.clone()) *
                                 Ratio::from_integer((str.len() as i64).to_bigint().unwrap());
                let truncation_index = extended.len().to_bigint().unwrap() -
                                       truncation.to_integer();
                Ok(Value::CharString(extended[..truncation_index.to_usize().unwrap()].to_owned()))
            }
            (Value::Boolean(lbool), Value::Boolean(rbool)) => Ok(Value::Boolean(lbool & rbool)),
            (l, r) => Err(format!("Unsupported operation * for {:?} and {:?}", l, r)),
        }
    }

    fn div(left: Value, right: Value) -> BinopResult {
        match (left, right) {
            (Value::Number(lratio), Value::Number(rratio)) => {
                if rratio.is_zero() {
                    Err("Can't divide by zero".to_owned())
                } else {
                    Ok(Value::Number(lratio / rratio))
                }
            }
            (Value::CharString(str), Value::Number(ratio)) => {
                if ratio.is_zero() {
                    Err("Can't divide by zero".to_owned())
                } else {
                    Vm::mul(Value::CharString(str.clone()),
                            Value::Number(Ratio::from_integer(BigInt::from(1)) / ratio))
                }
            }

            (l, r) => Err(format!("Unsupported operation / for {:?} and {:?}", l, r)),
        }
    }

    fn raise(&mut self, value: Value) {
        let matched_handler = self.frames
            .iter()
            .rev()
            .filter_map(|frame| {
                let handlers = frame.exception_handlers
                    .iter()
                    .filter_map(|handler| match handler.matches(value.clone()) {
                        Some(bindings) => Some((handler, bindings)),
                        None => None,
                    })
                    .collect::<Vec<_>>();

                if handlers.is_empty() {
                    return None;
                }

                println!("found handlers: {:?}", handlers.len());
                Some(handlers.first().unwrap().clone())
            })
            .take(1)
            .collect::<Vec<_>>()
            .first()
            .map(|&(ref handler, ref bindings)| {
                let mut map = BindingMap::new(Some(&handler.closure.parent_bindings));
                for (key, value) in bindings.iter() {
                    map.local_assign(key, value.to_owned());
                }
                println!("bindings: {:?}", bindings);
                (handler.closure.instructions.clone(), map)
            });

        if let Some((instructions, map)) = matched_handler {
            println!("insns: {:?}", instructions);
            self.reset_instructions(instructions, map);
        } else {
            println!("Uncaught exception ignored: {:?}", value);
        }
    }

    fn reset_instructions(&mut self, instructions: Rc<InstructionSequence>, map: BindingMap) {
        self.frames.push(Frame::new(map));
        self.instructions = instructions.clone();
        self.pc = 0;
        println!("instructions have been reset!");
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
                let closure = Closure::new(Rc::new(compile(&statements)), top_bindings);
                Value::Closure(Rc::new(args.clone()), Rc::new(closure))
            }
            _ => panic!("not implemented"),
        }
    }
}

#[cfg(test)]
mod test_exception_handler {
    use test_helpers::*;
    use super::*;
    use std::rc::Rc;

    #[test]
    fn matches_numbers() {
        let handler = ExceptionHandler::new(Rc::new(Pattern::Number(build_ratio(1, 1))),
                                            Closure::blank());
        assert_eq!(Some(BTreeMap::new()), handler.matches(v_number(1, 1)));
        assert_eq!(None, handler.matches(v_number(2, 1)));
        assert_eq!(None, handler.matches(v_string("toto")))
    }

    #[test]
    fn matches_strings() {
        let handler = ExceptionHandler::new(Rc::new(Pattern::CharString("toto".to_owned())),
                                            Closure::blank());
        assert_eq!(Some(BTreeMap::new()), handler.matches(v_string("toto")));
        assert_eq!(None, handler.matches(v_string("titi")));
        assert_eq!(None, handler.matches(v_number(1, 1)))
    }

    #[test]
    fn matches_bools() {
        let handler = ExceptionHandler::new(Rc::new(Pattern::Boolean(false)), Closure::blank());
        assert_eq!(Some(BTreeMap::new()), handler.matches(v_bool(false)));
        assert_eq!(None, handler.matches(v_bool(true)));
        assert_eq!(None, handler.matches(v_string("toto")));
    }

    #[test]
    fn matches_simple_map() {
        let handler =
            ExceptionHandler::new(Rc::new(Pattern::Map(vec![(Pattern::Number(build_ratio(1,
                                                                                         1)),
                                                             Pattern::Identifier("toto"
                                                                 .to_owned()))])),
                                  Closure::blank());
        assert_eq!(Some(vec![("toto".to_owned(), v_string("titi"))]
                       .into_iter()
                       .collect()),
                   handler.matches(v_map(vec![(v_number(1, 1), v_string("titi"))])));
        assert_eq!(None,
                   handler.matches(v_map(vec![(v_string("titi"), v_number(2, 1))])));
        assert_eq!(None, handler.matches(v_bool(false)));
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
                                  Closure::blank());
        assert_eq!(Some(vec![("toto".to_owned(), v_string("titi"))]
                       .into_iter()
                       .collect()),
                   handler.matches(v_map(vec![(v_number(1, 1), v_string("titi")),
                                              (v_number(2, 1), v_string("titi"))])));
        assert_eq!(None,
                   handler.matches(v_map(vec![(v_number(1, 1), v_string("titi")),
                                              (v_number(2, 1), v_string("foo"))])));
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
                                  Closure::blank());
        assert_eq!(Some(vec![("toto".to_owned(), v_string("titi"))]
                       .into_iter()
                       .collect()),
                   handler.matches(v_map(vec![(v_number(1, 1),
                                               v_map(vec![(v_number(2, 1), v_string("titi"))]))])));
    }

    #[test]
    fn matches_identifier() {
        let handler = ExceptionHandler::new(Rc::new(Pattern::Identifier("toto".to_owned())),
                                            Closure::blank());
        assert_eq!(Some(vec![("toto".to_owned(), v_string("titi"))]
                       .into_iter()
                       .collect()),
                   handler.matches(v_string("titi")));
        assert_eq!(Some(vec![("toto".to_owned(), v_number(1, 1))]
                       .into_iter()
                       .collect()),
                   handler.matches(v_number(1, 1)))
    }
}


#[cfg(test)]
mod test_vm {
    use super::*;
    use test_helpers::*;
    use std::rc::Rc;

    #[test]
    fn test_new_compiles_basic_instructions() {
        let source = "let a = 1
            let b = \"\"
            rescue(id) do
              b = 1 + 2 * 1 + a[c]
              c[d] = 1
            end
            raise(\"a\")";
        let vm = Vm::new(source);
        let expected = Rc::new(vec![Instruction::Push(l_number(1, 1)),
                         Instruction::LocalAssign("a".to_owned()),
                         Instruction::Push(l_string("")),
                         Instruction::LocalAssign("b".to_owned()),
                         Instruction::Rescue(Rc::new(p_ident("id")),
                                             Rc::new(vec![Instruction::Push(l_number(1, 1)),
                                                          Instruction::Push(l_number(2, 1)),
                                                          Instruction::Push(l_number(1, 1)),
                                                          Instruction::Mul,
                                                          Instruction::Fetch("a".to_owned()),
                                                          Instruction::Fetch("c".to_owned()),
                                                          Instruction::IndexAccess,
                                                          Instruction::Add,
                                                          Instruction::Add,
                                                          Instruction::Assign("b".to_owned()),
                                                          Instruction::Fetch("c".to_owned()),
                                                          Instruction::Fetch("d".to_owned()),
                                                          Instruction::Push(l_number(1, 1)),
                                                          Instruction::IndexAssign])),
                         Instruction::Push(l_string("a")),
                         Instruction::Raise]);
        assert_eq!(vm.instructions, expected)
    }

    #[test]
    fn test_new_compiles_function_instructions() {
        let source = "let a = { 1 => 2 }
            let b = def(x) do
            end
            b(1)";
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
        assert_eq!(v_number(1, 1),
                   vm.frames.last().unwrap().bindings.fetch(&"a".to_owned()).unwrap().to_owned());
        let expected_map = v_map(vec![(v_string("a"), v_number(1, 1))]);
        assert_eq!(expected_map,
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
        assert_eq!(v_number(1, 1),
                   vm.frames.last().unwrap().bindings.fetch(&"a".to_owned()).unwrap().to_owned())
    }

    #[test]
    fn test_function_call_with_args() {
        let source = "let a = \"\"
            let b = \"\"
            let x = def(c, d) do
              a = c
              b = d
            end
            x(1, 2)";

        let mut vm = Vm::new(source);
        vm.run();
        assert_eq!(v_number(1, 1),
                   vm.frames.last().unwrap().bindings.fetch(&"a".to_owned()).unwrap().to_owned());
        assert_eq!(v_number(2, 1),
                   vm.frames.last().unwrap().bindings.fetch(&"b".to_owned()).unwrap().to_owned())
    }

    #[test]
    #[should_panic(expected="expected a closure")]
    fn test_calling_non_function() {
        let source = "let x = \"\"
            x()";
        let mut vm = Vm::new(source);
        vm.run();
    }

    #[test]
    #[should_panic(expected="wrong number of arguments")]
    fn test_function_with_wrong_arg_count() {
        let source = "let x = def(a, b) do
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
        assert_eq!(v_number(1, 1),
                   vm.frames.last().unwrap().bindings.fetch(&"a".to_owned()).unwrap().to_owned())
    }

    #[test]
    fn test_rescue_map() {
        let source =
            "let a = \"\"
            rescue({\"b\" => id}) do
                a = id
            end
            let x = def(a, b) do
                raise({\"a\" => 1, \"b\" => b})
            end
            x(2, 1)";

        let mut vm = Vm::new(source);
        vm.run();
        assert_eq!(v_number(1, 1),
                   vm.frames.last().unwrap().bindings.fetch(&"a".to_owned()).unwrap().to_owned())
    }

    #[test]
    fn test_maps() {
        let source = "let a = { \"c\" => 1 }
            a[\"b\"] = 2
            let b = a[\"b\"]
            let c = a[\"c\"]\
            let d = a + { \"e\" => 3 }";

        let mut vm = Vm::new(source);
        vm.run();

        assert_eq!(v_number(2, 1),
                   vm.frames.last().unwrap().bindings.fetch(&"b".to_owned()).unwrap().to_owned());
        assert_eq!(v_number(1, 1),
                   vm.frames.last().unwrap().bindings.fetch(&"c".to_owned()).unwrap().to_owned());
        assert_eq!(v_map(vec![(v_string("c"), v_number(1, 1)), (v_string("b"), v_number(2, 1))]),
                   vm.frames.last().unwrap().bindings.fetch(&"a".to_owned()).unwrap().to_owned());
        assert_eq!(v_map(vec![(v_string("c"), v_number(1, 1)),
                              (v_string("b"), v_number(2, 1)),
                              (v_string("e"), v_number(3, 1))]),
                   vm.frames.last().unwrap().bindings.fetch(&"d".to_owned()).unwrap().to_owned());
    }

    #[test]
    fn test_fibonacci() {
        let source = "let fib = def(k) do
          rescue({ \"m\" => m, \"k\" => 0 }) do
            raise({ \"result\" => m })
          end
          rescue({ \"m\" => m, \"n\" => n, \"k\" => k }) do
            raise({ \"m\" => n, \"n\" => m + n, \"k\" => k - 1 })
          end
          raise({ \"m\" => 0, \"n\" => 1, \"k\" => k })
        end
        let res = \"\"
        let setup = def() do
          rescue({ \"result\" => r }) do
            res = r
          end
          fib(6)
        end
        setup()";

        let mut vm = Vm::new(source);
        vm.run();
        assert_eq!(v_number(8, 1),
                   vm.frames.last().unwrap().bindings.fetch(&"res".to_owned()).unwrap().to_owned())
    }
}

#[cfg(test)]
mod test_binop {
    use super::*;
    use test_helpers::*;

    #[test]
    fn test_add() {
        // Numbers
        assert_eq!(Ok(v_number(17, 2)), Vm::add(v_number(8, 1), v_number(1, 2)));
        assert_err!(Vm::add(v_number(8, 1), v_string("toto")));
        // Strings
        assert_eq!(Ok(v_string("hello world")),
                   Vm::add(v_string("hello "), v_string("world")));
        assert_err!(Vm::add(v_string("toto"), v_number(1, 1)));
        // Boolean
        assert_eq!(Ok(v_bool(true)), Vm::add(v_bool(true), v_bool(true)));
        assert_eq!(Ok(v_bool(true)), Vm::add(v_bool(true), v_bool(false)));
        assert_eq!(Ok(v_bool(true)), Vm::add(v_bool(false), v_bool(true)));
        assert_eq!(Ok(v_bool(false)), Vm::add(v_bool(false), v_bool(false)));
        assert_err!(Vm::add(v_bool(false), v_number(1, 1)));
        // Map
        assert_eq!(Ok(v_map(vec![(v_number(1, 1), v_number(1, 1)),
                                 (v_number(2, 1), v_number(2, 1))])),
                   Vm::add(v_map(vec![(v_number(1, 1), v_number(1, 1))]),
                           v_map(vec![(v_number(2, 1), v_number(2, 1))])));
        assert_err!(Vm::add(v_map(vec![(v_number(1, 1), v_number(1, 1))]),
                            v_number(1, 1)));
    }

    #[test]
    fn test_sub() {
        // Numbers
        assert_eq!(Ok(v_number(15, 2)), Vm::sub(v_number(8, 1), v_number(1, 2)));
        assert_err!(Vm::sub(v_number(8, 1), v_string("toto")));
        // Strings
        assert_eq!(Ok(v_string("hello ")),
                   Vm::sub(v_string("hello world"), v_string("world")));
        assert_eq!(Ok(v_string("helld")),
                   Vm::sub(v_string("hello world"), v_string("lo wor")));
        assert_eq!(Ok(v_string("hello world")),
                   Vm::sub(v_string("hello world"), v_string("")));
        assert_err!(Vm::sub(v_string("toto"), v_number(1, 1)));
        // Boolean
        assert_eq!(Ok(v_bool(false)), Vm::sub(v_bool(true), v_bool(true)));
        assert_eq!(Ok(v_bool(true)), Vm::sub(v_bool(true), v_bool(false)));
        assert_eq!(Ok(v_bool(true)), Vm::sub(v_bool(false), v_bool(true)));
        assert_eq!(Ok(v_bool(false)), Vm::sub(v_bool(false), v_bool(false)));
        assert_err!(Vm::sub(v_bool(false), v_number(1, 1)));
        // Map
        assert_eq!(Ok(v_map(vec![(v_number(1, 1), v_number(1, 1))])),
                   Vm::sub(v_map(vec![(v_number(1, 1), v_number(1, 1)),
                                      (v_number(2, 1), v_number(2, 1))]),
                           v_map(vec![(v_number(2, 1), v_number(2, 1))])));
        // Map
        assert_eq!(Ok(v_map(vec![(v_number(1, 1), v_number(1, 1)),
                                 (v_number(2, 1), v_number(2, 1))])),
                   Vm::sub(v_map(vec![(v_number(1, 1), v_number(1, 1)),
                                      (v_number(2, 1), v_number(2, 1))]),
                           v_map(vec![(v_number(2, 1), v_number(2, 2))])));
        assert_err!(Vm::sub(v_map(vec![(v_number(1, 1), v_number(1, 1))]),
                            v_number(1, 1)));
    }

    #[test]
    fn test_mul() {
        // Numbers
        assert_eq!(Ok(v_number(4, 1)), Vm::mul(v_number(8, 1), v_number(1, 2)));
        assert_err!(Vm::mul(v_number(8, 1), v_string("toto")));
        // Strings
        assert_err!(Vm::mul(v_string("hello "), v_string("world")));
        assert_eq!(Ok(v_string("totototo")),
                   Vm::mul(v_string("to"), v_number(4, 1)));
        assert_eq!(Ok(v_string("tototot")),
                   Vm::mul(v_string("to"), v_number(7, 2)));
        assert_eq!(Ok(v_string("tot")),
                   Vm::mul(v_string("toto"), v_number(3, 4)));
        assert_eq!(Ok(v_string("")), Vm::mul(v_string("toto"), v_number(0, 1)));
        // Boolean
        assert_eq!(Ok(v_bool(true)), Vm::mul(v_bool(true), v_bool(true)));
        assert_eq!(Ok(v_bool(false)), Vm::mul(v_bool(true), v_bool(false)));
        assert_eq!(Ok(v_bool(false)), Vm::mul(v_bool(false), v_bool(true)));
        assert_eq!(Ok(v_bool(false)), Vm::mul(v_bool(false), v_bool(false)));
        assert_err!(Vm::mul(v_bool(false), v_number(1, 1)));
        // Map can't be multiplied?
        assert_err!(Vm::mul(v_map(vec![]), v_map(vec![])));
    }

    #[test]
    fn test_div() {
        // Numbers
        assert_eq!(Ok(v_number(16, 1)), Vm::div(v_number(8, 1), v_number(1, 2)));
        assert_err!(Vm::div(v_number(8, 1), v_number(0, 1)));
        assert_err!(Vm::div(v_number(8, 1), v_string("toto")));
        // Strings
        assert_err!(Vm::div(v_string("hello "), v_string("world")));
        assert_eq!(Ok(v_string("t")), Vm::div(v_string("to"), v_number(2, 1)));
        assert_eq!(Ok(v_string("tototot")),
                   Vm::div(v_string("to"), v_number(2, 7)));
        assert_eq!(Ok(v_string("tot")),
                   Vm::div(v_string("toto"), v_number(4, 3)));
        assert_err!(Vm::div(v_string("toto"), v_number(0, 1)));
        // Boolean can't be divided
        assert_err!(Vm::div(v_bool(true), v_bool(true)));
        // Map can't be div?
        assert_err!(Vm::div(v_map(vec![]), v_map(vec![])));
    }
}
