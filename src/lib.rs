#![crate_type = "staticlib"]
extern crate libc;
mod term;
mod ffi;

#[no_mangle]
pub extern "C" fn rust_test(t: *mut ffi::term) -> *mut ffi::term {
    let rt = unsafe { ffi::cterm_to_term(t) }; 
    println!("{:?}", rt);
    ffi::term_to_cterm(&rt)
}
