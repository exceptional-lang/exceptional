use instructions::*;
use value::Value;
// TODO: Make Vm a trait?
use binding_map::BindingMap;
use closure::Closure;
use num::bigint::{BigInt, ToBigInt};
use num::rational::{BigRational, Ratio};
use num::ToPrimitive;
use std::cell::RefCell;
use std::collections::HashMap;
use std::error::Error;
use std::fs::File;
use std::io::prelude::*;
use std::net::{Shutdown, TcpListener, TcpStream};
use std::os::unix::io::AsRawFd;
use std::os::unix::io::FromRawFd;
use std::os::unix::io::RawFd;
use std::rc::Rc;
use vm::Vm;

#[derive(Debug)]
pub enum FileDescriptor {
    File(File),
    TcpStream(TcpStream),
    TcpListener(TcpListener),
}

impl FileDescriptor {
    fn read_to_string(&mut self) -> Result<String, String> {
        let mut buffer = String::new();
        match self {
            &mut FileDescriptor::TcpStream(ref mut s) => {
                s.read_to_string(&mut buffer);
                Ok(buffer)
            }
            _ => Err("can't read on this file descriptor".to_owned()),
        }
    }

    fn write(&mut self, string: String) -> Result<usize, String> {
        match self {
            &mut FileDescriptor::TcpStream(ref mut s) => s
                .write(string.as_bytes())
                .or_else(|e| Err(format!("couldn't write to file descriptor: {:?}", e))),
            _ => Err("can't write to this file descriptor".to_owned()),
        }
    }
}

impl PartialEq for FileDescriptor {
    fn eq(&self, other: &FileDescriptor) -> bool {
        match (self, other) {
            (&FileDescriptor::File(ref s), &FileDescriptor::File(ref o)) => {
                s.as_raw_fd() == o.as_raw_fd()
            }
            (&FileDescriptor::TcpStream(ref s), &FileDescriptor::TcpStream(ref o)) => {
                s.as_raw_fd() == o.as_raw_fd()
            }
            (&FileDescriptor::TcpListener(ref s), &FileDescriptor::TcpListener(ref o)) => {
                s.as_raw_fd() == o.as_raw_fd()
            }
            _ => false,
        }
    }
}

impl Eq for FileDescriptor {}

impl Clone for FileDescriptor {
    // TODO: Oh crap, cloning a VM will cause errors if an FD is closed
    fn clone(&self) -> Self {
        match self {
            &FileDescriptor::File(ref f) => unsafe {
                FileDescriptor::File(File::from_raw_fd(f.as_raw_fd()))
            },
            &FileDescriptor::TcpStream(ref t) => unsafe {
                FileDescriptor::TcpStream(TcpStream::from_raw_fd(t.as_raw_fd()))
            },
            &FileDescriptor::TcpListener(ref t) => unsafe {
                FileDescriptor::TcpListener(TcpListener::from_raw_fd(t.as_raw_fd()))
            },
        }
    }
}

pub type FileDescriptorMap = HashMap<RawFd, FileDescriptor>;

fn io_result(key: &str, value: Value) -> Value {
    let map = vec![(Value::CharString(key.to_owned()), value)]
        .into_iter()
        .collect();
    (Value::Map(Rc::new(RefCell::new(map))))
}

fn fd_to_number<T: AsRawFd>(fd: &T) -> Value {
    Value::Number(Ratio::new(BigInt::from(fd.as_raw_fd()), BigInt::from(1)))
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
//fn native_file_open(vm: &mut Vm) -> InstructionSequence {
//    vec![]
//}

//fn native_file_close(vm: &mut Vm) -> InstructionSequence {
//    vec![]
//}

fn native_socket_tcp_connect(vm: &mut Vm) -> InstructionSequence {
    let address = match vm.fetch(&"address".to_owned()) {
        Some(Value::CharString(str)) => str,
        _ => {
            vm.push(io_result(
                "socket.error",
                Value::CharString("address must be a address:port string".to_owned()),
            ));
            return vec![Instruction::Raise];
        }
    };

    let mut stream = match TcpStream::connect(address) {
        Ok(stream) => stream,
        Err(e) => {
            vm.push(io_result(
                "socket.error",
                Value::CharString(e.description().to_owned()),
            ));
            return vec![Instruction::Raise];
        }
    };

    let number = Ratio::new(BigInt::from(stream.as_raw_fd()), BigInt::from(1));
    vm.file_descriptors
        .insert(stream.as_raw_fd(), FileDescriptor::TcpStream(stream));

    vm.push(io_result("socket.result", Value::Number(number)));

    vec![Instruction::Raise]
}

fn native_socket_tcp_listen(vm: &mut Vm) -> InstructionSequence {
    let address = match vm.fetch(&"address".to_owned()) {
        Some(Value::CharString(str)) => str,
        _ => {
            vm.push(io_result(
                "socket.error",
                Value::CharString("address must be a address:port string".to_owned()),
            ));
            return vec![Instruction::Raise];
        }
    };

    let mut listener = match TcpListener::bind(address) {
        Ok(listener) => listener,
        Err(e) => {
            vm.push(io_result(
                "socket.error",
                Value::CharString(e.description().to_owned()),
            ));
            return vec![Instruction::Raise];
        }
    };

    let number = fd_to_number(&listener);
    vm.file_descriptors
        .insert(listener.as_raw_fd(), FileDescriptor::TcpListener(listener));

    vm.push(io_result("socket.result", number));

    vec![Instruction::Raise]
}

fn native_socket_tcp_accept(vm: &mut Vm) -> InstructionSequence {
    let result = match vm
        .fetch(&"fn".to_owned())
        .expect("expected callback argument")
    {
        closure @ Value::Closure(_, _) => Ok(closure),
        _ => Err("callback must be a function".to_owned()),
    }.and_then(|closure| {
        match vm
            .fetch(&"socket".to_owned())
            .expect("expected socket argument")
        {
            Value::Number(ratio) => ratio
                .to_integer()
                .to_i32()
                .and_then(|fd| vm.file_descriptors.get(&fd))
                .ok_or("socket not found".to_owned())
                .and_then(|descriptor| {
                    if let &FileDescriptor::TcpListener(ref l) = descriptor {
                        Ok(l)
                    } else {
                        Err("socket is not a socket".to_owned())
                    }
                })
                .and_then(|listener| match listener.accept() {
                    Ok((socket, _)) => Ok((closure, socket)),
                    Err(e) => Err(format!("could not connect to the client: {}", e)),
                }),
            x => Err(format!("socket argument is not a socket: {:?}", x)),
        }
    });

    let (callback, socket) = match result {
        Ok((c, s)) => (c, s),
        Err(e) => {
            vm.push(io_result("socket.error", Value::CharString(e)));
            return vec![Instruction::Raise];
        }
    };

    let number = fd_to_number(&socket);
    // TODO: We're holding onto sockets forever here
    vm.file_descriptors
        .insert(socket.as_raw_fd(), FileDescriptor::TcpStream(socket));
    vm.push(number);
    vm.push(callback);

    vec![Instruction::Call(1)]
}

fn native_io_read_all(vm: &mut Vm) -> InstructionSequence {
    let result = {
        match vm.fetch(&"fd".to_owned()) {
            Some(Value::Number(ratio)) => ratio
                .to_integer()
                .to_i32()
                .and_then(|fd| vm.file_descriptors.get_mut(&fd))
                .ok_or("file descriptor not found".to_owned())
                .and_then(|descriptor| descriptor.read_to_string()),
            x => Err(format!("fd argument is not a file descriptor: {:?}", x)),
        }
    };

    let string = match result {
        Ok(str) => str,
        Err(e) => {
            vm.push(io_result("io.error", Value::CharString(e)));
            return vec![Instruction::Raise];
        }
    };

    vm.push(io_result("io.result", Value::CharString(string)));
    vec![Instruction::Raise]
}

fn native_io_write(vm: &mut Vm) -> InstructionSequence {
    let result = {
        match vm.fetch(&"string".to_owned()) {
            Some(Value::CharString(string)) => Ok(string),
            x => Err(format!("string argument is not a string: {:?}", x)),
        }.and_then(|string| match vm.fetch(&"fd".to_owned()) {
            Some(Value::Number(ratio)) => ratio
                .to_integer()
                .to_i32()
                .and_then(|fd| vm.file_descriptors.get_mut(&fd))
                .ok_or("file descriptor not found".to_owned())
                .and_then(|descriptor| descriptor.write(string)),
            x => Err(format!("fd argument is not a file descriptor: {:?}", x)),
        })
    };

    let bytes = match result {
        Ok(bytes) => bytes,
        Err(e) => {
            vm.push(io_result("io.error", Value::CharString(e)));
            return vec![Instruction::Raise];
        }
    };

    let number = Value::Number(Ratio::new(BigInt::from(bytes), BigInt::from(1)));
    vm.push(io_result("io.result", number));
    vec![Instruction::Raise]
}

fn wrap_native_code(args: Vec<String>, f: NativeCode) -> Value {
    let parent_bindings = BindingMap::new(None);
    let closure = Closure::new(
        Rc::new(vec![Instruction::Native(NativeFunction::new(f))]),
        &parent_bindings,
    );
    Value::Closure(Rc::new(Box::new(args)), Rc::new(closure))
}

fn socket_lib() -> Value {
    let map = vec![
        (
            Value::CharString("tcp_connect".to_owned()),
            wrap_native_code(
                vec!["address".to_owned()],
                native_socket_tcp_connect as NativeCode,
            ),
        ),
        (
            Value::CharString("tcp_listen".to_owned()),
            wrap_native_code(
                vec!["address".to_owned()],
                native_socket_tcp_listen as NativeCode,
            ),
        ),
        (
            Value::CharString("tcp_accept".to_owned()),
            wrap_native_code(
                vec!["socket".to_owned(), "fn".to_owned()],
                native_socket_tcp_accept as NativeCode,
            ),
        ),
    ].into_iter()
        .collect();

    Value::Map(Rc::new(RefCell::new(map)))
}

fn io_lib() -> Value {
    let map = vec![
        (
            Value::CharString("read_all".to_owned()),
            wrap_native_code(vec!["fd".to_owned()], native_io_read_all as NativeCode),
        ),
        (
            Value::CharString("write".to_owned()),
            wrap_native_code(
                vec!["fd".to_owned(), "string".to_owned()],
                native_io_write as NativeCode,
            ),
        ),
    ].into_iter()
        .collect();

    Value::Map(Rc::new(RefCell::new(map)))
}

fn file_lib() -> Value {
    let map = vec![
        (
            Value::CharString("read".to_owned()),
            wrap_native_code(vec!["path".to_owned()], native_file_read as NativeCode),
        ),
        (
            Value::CharString("write".to_owned()),
            wrap_native_code(
                vec!["path".to_owned(), "content".to_owned()],
                native_file_write as NativeCode,
            ),
        ),
    ].into_iter()
        .collect();

    Value::Map(Rc::new(RefCell::new(map)))
}

pub fn find_lib(name: &str) -> Option<Value> {
    match name {
        "file" => Some(file_lib()),
        "socket" => Some(socket_lib()),
        "io" => Some(io_lib()),
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
        let read_closure = v_closure(
            vec!["path".to_owned()],
            vec![Instruction::Native(NativeFunction::new(
                native_file_read as NativeCode,
            ))],
            None,
        );
        let write_closure = v_closure(
            vec!["path".to_owned(), "content".to_owned()],
            vec![Instruction::Native(NativeFunction::new(
                native_file_write as NativeCode,
            ))],
            None,
        );
        let lib = v_map(vec![
            (v_string("read"), read_closure),
            (v_string("write"), write_closure),
        ]);
        assert_eq!(Some(lib), find_lib("file"));
    }

    #[test]
    fn find_lib_returns_socket() {
        let tcp_connect_closure = v_closure(
            vec!["address".to_owned()],
            vec![i_native_fn(native_socket_tcp_connect as NativeCode)],
            None,
        );
        let tcp_listen_closure = v_closure(
            vec!["address".to_owned()],
            vec![i_native_fn(native_socket_tcp_listen as NativeCode)],
            None,
        );
        let tcp_accept_closure = v_closure(
            vec!["socket".to_owned(), "fn".to_owned()],
            vec![i_native_fn(native_socket_tcp_accept as NativeCode)],
            None,
        );

        let lib = v_map(vec![
            (v_string("tcp_connect"), tcp_connect_closure),
            (v_string("tcp_listen"), tcp_listen_closure),
            (v_string("tcp_accept"), tcp_accept_closure),
        ]);
        assert_eq!(Some(lib), find_lib("socket"));
    }

    #[test]
    fn find_lib_returns_io() {
        let read_all_closure = v_closure(
            vec!["fd".to_owned()],
            vec![i_native_fn(native_io_read_all as NativeCode)],
            None,
        );
        let write_closure = v_closure(
            vec!["fd".to_owned(), "string".to_owned()],
            vec![i_native_fn(native_io_write as NativeCode)],
            None,
        );

        let lib = v_map(vec![
            (v_string("read_all"), read_all_closure),
            (v_string("write"), write_closure),
        ]);
        assert_eq!(Some(lib), find_lib("io"));
    }

    #[test]
    fn read_file_contents_returns_result() {
        assert!(read_file_contents("/dev/null".to_owned()).is_ok())
    }

    #[test]
    fn read_file_contents_returns_error() {
        assert_eq!(
            Err("entity not found".to_owned()),
            read_file_contents("/does/not/exist".to_owned())
        )
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
        assert_eq!(
            Some(v_map(vec![(v_string("file.result"), v_string(""))])),
            vm.pop()
        );
    }

    #[test]
    fn read_file_does_not_exist() {
        let mut vm = Vm::empty();
        vm.local_assign(&"path".to_owned(), v_string("/does/not/exist"));
        let result = native_file_read(&mut vm);
        assert_eq!(
            Some(v_map(vec![(
                v_string("file.error"),
                v_string("entity not found"),
            )],)),
            vm.pop()
        );
        assert_eq!(vec![Instruction::Raise], result);
    }

    #[test]
    fn write_file_writes_the_content() {
        let mut vm = Vm::empty();
        vm.local_assign(&"path".to_owned(), v_string("/tmp/test.exceptional"));
        vm.local_assign(&"content".to_owned(), v_string("testing write file"));

        let result = native_file_write(&mut vm);

        assert_eq!(
            Some(v_map(vec![(v_string("file.result"), v_bool(true))])),
            vm.pop()
        );
        assert_eq!(vec![Instruction::Raise], result);
    }

    #[test]
    fn native_tcp_connect_opens_a_tcp_stream() {
        let listener = TcpListener::bind("127.0.0.1:8080").unwrap();

        let mut vm = Vm::empty();
        vm.local_assign(&"address".to_owned(), v_string("127.0.0.1:8080"));

        let result = native_socket_tcp_connect(&mut vm);
        assert_eq!(vec![Instruction::Raise], result);

        let socket_fd = match vm.pop() {
            Some(Value::Map(map)) => {
                match map.borrow().get(&v_string("socket.result")) {
                    Some(&Value::Number(_)) => {} // All good
                    x => panic!("expected result to be an number, got {:?} in {:?}", x, map),
                }
            }
            _ => panic!("expected result to be a map"),
        };
    }

    #[test]
    fn native_tcp_listen_opens_a_tcp_listener() {
        let mut vm = Vm::empty();
        vm.local_assign(&"address".to_owned(), v_string("127.0.0.1:8081"));

        let result = native_socket_tcp_listen(&mut vm);
        assert_eq!(vec![Instruction::Raise], result);

        let socket_fd = match vm.pop() {
            Some(Value::Map(map)) => {
                match map.borrow().get(&v_string("socket.result")) {
                    Some(&Value::Number(_)) => {} // All good
                    x => panic!("expected result to be an number, got {:?} in {:?}", x, map),
                }
            }
            _ => panic!("expected result to be a map"),
        };

        TcpStream::connect("127.0.0.1:8081").unwrap();
    }

    #[test]
    fn native_tcp_accept_calls_a_function_with_the_socket() {
        let mut vm = Vm::empty();
        let listener = TcpListener::bind("127.0.0.1:8082").unwrap();
        let socket = TcpStream::connect("127.0.0.1:8082").unwrap();

        let callback = v_closure(vec!["socket".to_owned()], vec![], None);

        vm.local_assign(
            &"socket".to_owned(),
            v_number(listener.as_raw_fd() as i64, 1),
        );
        vm.local_assign(&"fn".to_owned(), callback.clone());
        vm.file_descriptors
            .insert(listener.as_raw_fd(), FileDescriptor::TcpListener(listener));

        let result = native_socket_tcp_accept(&mut vm);
        assert_eq!(vec![Instruction::Call(1)], result);
        assert_eq!(callback, vm.pop().unwrap())
    }

    #[test]
    fn native_io_read_all_reads_from_a_file_descriptor_and_returns_a_string() {
        let mut vm = Vm::empty();
        let listener = TcpListener::bind("127.0.0.1:8083").unwrap();
        let mut connector = TcpStream::connect("127.0.0.1:8083").unwrap();
        connector.write("foo".as_bytes());
        connector.flush();
        connector
            .shutdown(Shutdown::Both)
            .expect("expect shutdown of connected tcp stream");
        let (socket, _) = listener.accept().unwrap();

        vm.local_assign(&"fd".to_owned(), v_number(socket.as_raw_fd() as i64, 1));
        vm.file_descriptors
            .insert(socket.as_raw_fd(), FileDescriptor::TcpStream(socket));

        let result = native_io_read_all(&mut vm);
        assert_eq!(vec![Instruction::Raise], result);
        assert_eq!(
            v_map(vec![(v_string("io.result"), v_string("foo"))]),
            vm.pop().unwrap()
        )
    }

    #[test]
    fn native_io_write_writes_to_a_file_descriptor_and_raises() {
        let mut vm = Vm::empty();
        let listener = TcpListener::bind("127.0.0.1:8084").unwrap();
        let mut connector = TcpStream::connect("127.0.0.1:8084").unwrap();

        let (socket, _) = listener.accept().unwrap();

        vm.local_assign(&"fd".to_owned(), v_number(socket.as_raw_fd() as i64, 1));
        vm.local_assign(&"string".to_owned(), v_string("foo bar"));
        vm.file_descriptors
            .insert(socket.as_raw_fd(), FileDescriptor::TcpStream(socket));

        let result = native_io_write(&mut vm);
        assert_eq!(vec![Instruction::Raise], result);
        assert_eq!(
            v_map(vec![(v_string("io.result"), v_number(7, 1))]),
            vm.pop().unwrap()
        )
    }
}
