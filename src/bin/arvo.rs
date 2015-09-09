#![feature(convert)]

extern crate arvo;
extern crate libc;

use libc::{c_int, c_char};

use std::env;
use std::ffi::CString;
use std::path::Path;
use std::process::exit;

#[link(name = "arvo")]
extern {
    fn setup_printing();
    fn initialize_arvo_parsers();
    fn vernac_init(working_directory: *const c_char);
    fn process_stream_with_stdin() -> c_int;
    fn process_file(filename: *const c_char) -> c_int;
    fn cleanup_arvo_parsers();
}

fn main() {
    unsafe {
        setup_printing();
        initialize_arvo_parsers();
    }

    let mut ans = 0;

    let args = env::args().collect::<Vec<_>>();

    if args.len() > 1 {
        let filename = Path::new(&args[1]);

        let parent = match filename.parent() {
            None => panic!("{:?} has no parent path", filename.display()),
            Some(path) => path
        };

        // Run the Rust parsing code in parallel for now.
        let parser = arvo::parser::Parser::from_file(filename).unwrap(); // may fail opening file
        let program = match parser.parse_program() {
            Err(e) => panic!("rust parser failed with: {:?}", e),
            Ok(p)  => p
        };

        // All interaction with external code is inhernetly unsafe, eventually if this code
        // is written in Rust we can just remove it.
        unsafe {
            // Conversion here can fail because of UTF-8 so to_cstring returns an Option
            // which must be unwrapped or dealt with.
            vernac_init(parent.as_os_str().to_cstring().unwrap().as_ptr());
            ans = process_file(filename.as_os_str().to_cstring().unwrap().as_ptr());
        }
    } else {
        unsafe {
            vernac_init(CString::new(".").unwrap().as_ptr());
            ans = process_stream_with_stdin();
        }
    }

    unsafe { cleanup_arvo_parsers() };

    // Rust's main fn doesn't return integer error codes, so call process::exit to emulate the
    // behavior of the C main.
    exit(ans)
}
