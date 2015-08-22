#![crate_type = "staticlib"]
extern crate libc;
mod term;
mod ffi;
mod prettyprint;

#[no_mangle]
pub extern "C" fn C_prettyprint(t: *mut ffi::term) -> *const libc::c_char{
    let rt = unsafe { ffi::cterm_to_term(t) }; 
    let s = prettyprint::prettyprint(&*rt);
    ffi::string_to_cstring(&s)
}
