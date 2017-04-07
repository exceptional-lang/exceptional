extern crate num;

#[cfg(test)]
#[macro_use]
mod test_helpers;

mod ast;
mod native;
mod instructions;
mod exception_handler;
mod value;
mod grammar;
mod vm;
mod closure;
mod compiler;
mod binding_map;
use std::env;
use vm::Vm;

fn exec(source: &String) {
    let mut vm = Vm::new(source);
    vm.run();
}

fn main() {
    if let Some(source) = env::args().take(2).last() {
        println!("ARGV contains what could be source");
        println!("Starting VM with ARGV");
        println!("{}", source);
        exec(&source);
    } else {
        println!("No args found, stopping");
    }
}
