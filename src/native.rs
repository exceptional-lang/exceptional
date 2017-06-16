use value::Value;
use instructions::*;
// TODO: Make Vm a trait?
use vm::Vm;
use closure::Closure;
use binding_map::BindingMap;
use std::rc::Rc;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fs::File;
use std::os::unix::io::RawFd;
use std::os::unix::io::AsRawFd;
use std::os::unix::io::FromRawFd;
use std::io::prelude::*;
use std::error::Error;

#[derive(Debug)]
pub enum FileDescriptor {
    File(File),
}

impl PartialEq for FileDescriptor {
    fn eq(&self, other: &FileDescriptor) -> bool {
        match (self, other) {
            (&FileDescriptor::File(ref s), &FileDescriptor::File(ref o)) => {
                s.as_raw_fd() == o.as_raw_fd()
            }
        }
    }
}

impl Eq for FileDescriptor {}

impl Clone for FileDescriptor {
    fn clone(&self) -> Self {
        let &FileDescriptor::File(ref f) = self;
        unsafe { FileDescriptor::File(File::from_raw_fd(f.as_raw_fd())) }
    }
}

pub type FileDescriptorMap = HashMap<RawFd, FileDescriptor>;

//fn native_file_open(vm: &mut Vm) -> InstructionSequence {
//    vec![]
//}

fn io_result(key: &str, value: Value) -> Value {
    let map = vec![(Value::CharString(key.to_owned()), value)]
        .into_iter()
        .collect();
    (Value::Map(Rc::new(RefCell::new(map))))
}

fn read_file_contents(path: String) -> Result<String, String> {
    let mut file = match File::open(path) {
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

fn native_file_read(vm: &mut Vm) -> InstructionSequence {
    let path = match vm.fetch(&"path".to_owned()) {
        Some(Value::CharString(str)) => str,
        Some(_) => panic!("unexpected value in path parameter"), // TODO: Raise
        None => panic!("how did we get here?!"),
    };

    let result = match read_file_contents(path) {
        Ok(content) => io_result("file.result", Value::CharString(content)),
        Err(err) => io_result("file.error", Value::CharString(err)),
    };

    vm.push(result);
    vec![Instruction::Raise]
}

fn write_file_contents(path: &String, content: &String) -> Result<(), String> {
    let mut file = match File::create(path) {
        Ok(f) => f,
        Err(m) => return Err(m.description().to_owned()),
    };

    if let Ok(_) = file.write_all(content.as_bytes()) {
        Ok(())
    } else {
        Err("Can't write file".to_owned())
    }
}

fn native_file_write(vm: &mut Vm) -> InstructionSequence {
    let path = match vm.fetch(&"path".to_owned()) {
        Some(Value::CharString(str)) => str,
        Some(_) => panic!("unexpected value in path parameter"), // TODO: Raise
        None => panic!("how did we get here?!"),
    };
    let content = match vm.fetch(&"content".to_owned()) {
        Some(Value::CharString(str)) => str,
        Some(_) => panic!("unexpected value in content parameter"), // TODO: Raise
        None => panic!("how did we get here?!"),
    };
    let result = match write_file_contents(&path, &content) {
        Ok(_) => io_result("file.result", Value::Boolean(true)),
        Err(err) => io_result("file.error", Value::CharString(err)),
    };
    vm.push(result);
    vec![Instruction::Raise]
}

//fn native_file_close(vm: &mut Vm) -> InstructionSequence {
//    vec![]
//}

//fn socket(vm: &mut Vm) -> InstructionSequence {
//    vec![]
//}

fn wrap_native_code(args: Vec<String>, f: NativeCode) -> Value {
    let parent_bindings = BindingMap::new(None);
    let closure = Closure::new(Rc::new(vec![Instruction::Native(NativeFunction::new(f))]),
                               &parent_bindings);
    Value::Closure(Rc::new(Box::new(args)), Rc::new(closure))
}

fn file_lib() -> Value {
    let map = vec![(Value::CharString("read".to_owned()),
                    wrap_native_code(vec!["path".to_owned()], native_file_read as NativeCode)),
                   (Value::CharString("write".to_owned()),
                    wrap_native_code(vec!["path".to_owned(), "content".to_owned()],
                                     native_file_write as NativeCode))]
            .into_iter()
            .collect();

    Value::Map(Rc::new(RefCell::new(map)))
}

pub fn find_lib(name: &str) -> Option<Value> {
    match name {
        "file" => Some(file_lib()),
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
    fn find_lib_returns_file() {
        let read_closure =
            v_closure(vec!["path".to_owned()],
                      vec![Instruction::Native(NativeFunction::new(native_file_read as
                                                                   NativeCode))],
                      None);
        let write_closure =
            v_closure(vec!["path".to_owned(), "content".to_owned()],
                      vec![Instruction::Native(NativeFunction::new(native_file_write as
                                                                   NativeCode))],
                      None);
        let lib = v_map(vec![(v_string("read"), read_closure),
                             (v_string("write"), write_closure)]);
        assert_eq!(Some(lib), find_lib("file"));
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
        vm.local_assign(&"path".to_owned(), v_string("/dev/null"));

        assert_eq!(vec![Instruction::Raise], native_file_read(&mut vm));
    }

    #[test]
    fn read_file_pushes_a_value_and_returns_a_raise_instruction() {
        let mut vm = Vm::empty();
        vm.local_assign(&"path".to_owned(), v_string("/dev/null"));
        let result = native_file_read(&mut vm);
        assert_eq!(vec![Instruction::Raise], result);
        assert_eq!(Some(v_map(vec![(v_string("file.result"), v_string(""))])),
                   vm.pop());
    }

    #[test]
    fn read_file_does_not_exist() {
        let mut vm = Vm::empty();
        vm.local_assign(&"path".to_owned(), v_string("/does/not/exist"));
        let result = native_file_read(&mut vm);
        assert_eq!(Some(v_map(vec![(v_string("file.error"), v_string("entity not found"))])),
                   vm.pop());
        assert_eq!(vec![Instruction::Raise], result);
    }

    #[test]
    fn write_file_writes_the_content() {
        let mut vm = Vm::empty();
        vm.local_assign(&"path".to_owned(), v_string("/tmp/test.exceptional"));
        vm.local_assign(&"content".to_owned(), v_string("testing write file"));

        let result = native_file_write(&mut vm);

        assert_eq!(Some(v_map(vec![(v_string("file.result"), v_bool(true))])),
                   vm.pop());
        assert_eq!(vec![Instruction::Raise], result);
    }
}
