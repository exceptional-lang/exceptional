use value::Value;
use instructions::*;
// TODO: Make Vm a trait?
use vm::Vm;
use closure::Closure;
use binding_map::BindingMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::fs::File;
use std::io::prelude::*;
use std::error::Error;

fn read_file(vm: &mut Vm) -> InstructionSequence {
    let btreemap = vec![
        (Value::CharString("io.result".to_owned()), Value::CharString("".to_owned()))
    ].into_iter().collect();
    let result = Value::Map(Rc::new(RefCell::new(btreemap)));
    vm.push(result);
    vec![Instruction::Raise]
}

fn read_file_contents(name: String) -> Result<String, String> {
    let mut file = match File::open(name) {
        Ok(f) => f,
        Err(m) => return Err(m.description().to_owned()),
    };

    let mut contents = String::new();
    if let Ok(_) = file.read_to_string(&mut contents) {
        return Ok(contents);
    } else {
        Err("Can't read file".to_owned())
    }
}

fn wrap_native_code(args: Vec<String>, f: NativeCode) -> Value {
    let parent_bindings = BindingMap::new(None);
    let closure = Closure::new(Rc::new(vec![Instruction::Native(NativeFunction::new(f))]),
                               &parent_bindings);
    Value::Closure(Rc::new(Box::new(args)), Rc::new(closure))
}

fn io() -> Value {
    let map = vec![(Value::CharString("read".to_owned()),
                    wrap_native_code(vec!["name".to_owned()], read_file as NativeCode))]
        .into_iter()
        .collect();

    Value::Map(Rc::new(RefCell::new(map)))
}

pub fn find_lib(name: &str) -> Option<Value> {
    match name {
        "io" => Some(io()),
        _ => None,
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use test_helpers::*;
    use vm::Vm;

    #[test]
    fn find_lib_returns_none_when_not_found() {
        assert_eq!(None, find_lib("oops"));
    }

    #[test]
    fn find_lib_returns_io() {
        let closure = v_closure(vec!["name".to_owned()],
                                vec![Instruction::Native(NativeFunction::new(read_file as
                                                                             NativeCode))],
                                None);
        let lib = v_map(vec![(v_string("read"), closure)]);
        assert_eq!(Some(lib), find_lib("io"));
    }

    #[test]
    fn read_file_contents_returns_result() {
        assert!(read_file_contents("/dev/null".to_owned()).is_ok())
    }

    #[test]
    fn read_file_contents_returns_error() {
        assert_eq!(Err("entity not found".to_owned()),
                   read_file_contents("/does/not/exist".to_owned()))
    }

    #[test]
    fn read_file_uses_the_file_name_argument() {
        let mut vm = Vm::empty();
        read_file(&mut vm);
    }
}
