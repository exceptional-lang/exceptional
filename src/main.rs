extern crate num;
extern crate regex;
#[macro_use]
extern crate log;
extern crate fern;

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
use std::fs::File;
use std::io::Read;


fn exec(source: &String) {
    let mut vm = Vm::new(source);
    vm.run();
}

fn main() {
    fern::Dispatch::new()
        .level(log::LogLevelFilter::Trace)
        .chain(std::io::stdout())
        .apply()
        .expect("failed to setup logging");

    let mut source = String::new();
    let file_read = env::args()
        .nth(1)
        .ok_or("No path given, stopping".to_string())
        .and_then(|path| File::open(path).map_err(|err| err.to_string()))
        .and_then(|mut file| {
            file.read_to_string(&mut source).map_err(
                |err| err.to_string(),
            )
        });
    match file_read {
        Ok(_) => {
            info!("Starting VM with contents from ARGV file");
            trace!("{}", source);
            exec(&source);
        }
        Err(e) => error!("{}", e),
    }
}
