use libc;
use term::Term;
use term::Term::*;
use term::Variable;
use std::ffi::{CStr, CString};
use std::str;
use std::ptr;

#[repr(C)]
#[derive(Clone, Copy)]
enum term_tag {
  VAR,
  LAM,
  PI, 
  APP,
  TYPE,
  INTRO,
  ELIM,
  DATATYPE,
  HOLE
}

#[repr(C)]
struct variable {
    name: *mut libc::c_char
}

#[repr(C)]
pub struct term {
    tag: term_tag,
    var: *mut variable,
    left: *mut term,
    right: *mut term,
    
    num_args: libc::c_int,
    args: *mut *mut term
}

extern "C" {
    fn strdup(s: *const libc::c_char) -> *mut libc::c_char;

    fn make_variable(name: *mut libc::c_char) -> *mut variable;

    fn make_pi(x: *mut variable, A: *mut term, B: *mut term) -> *mut term;
    fn make_lambda(x: *mut variable, a: *mut term, B: *mut term) -> *mut term;
    fn make_app(a: *mut term, b: *mut term) -> *mut term;
    fn make_var(a: *mut variable) -> *mut term;
    fn make_type() -> *mut term;
    fn make_hole() -> *mut term;
    fn make_intro(name: *mut variable, ty: *mut term, num_args: libc::c_int) -> *mut term;
    fn make_elim(name: *mut variable, num_args: libc::c_int) -> *mut term;
    fn make_datatype_term(name: *mut variable) -> *mut term;
}

unsafe fn charstar_to_string(s: *const libc::c_char) -> String {
    let bytes = CStr::from_ptr(s).to_bytes(); 
    str::from_utf8(bytes).unwrap().to_owned()
}

unsafe fn cvar_to_var(v: *const variable) -> Variable {
    Variable { name: charstar_to_string((*v).name) }
}

unsafe fn carray_to_vec_term(n: libc::c_int, a: *mut *mut term) -> Vec<Term> {
    let mut ans = Vec::with_capacity(n as usize);
    for i in 0..n {
        ans.push(*cterm_to_term(*a.offset(i as isize)));
    }
    ans
}


pub unsafe fn cterm_to_term(t: *mut term) -> Box<Term> {
    use self::term_tag::*;
    Box::new(
        match (*t).tag {
            TYPE => Type,
            HOLE => Hole,
            VAR => Var(cvar_to_var((*t).var)),
            LAM => Lambda(cvar_to_var((*t).var), 
                          if (*t).left.is_null() { None } else { Some(cterm_to_term((*t).left)) }, 
                          cterm_to_term((*t).right)),
            PI => Pi(cvar_to_var((*t).var),
                     cterm_to_term((*t).left), 
                     cterm_to_term((*t).right)),
            APP => App(cterm_to_term((*t).left), 
                       cterm_to_term((*t).right)),
            INTRO => Intro(cvar_to_var((*t).var),
                           cterm_to_term((*t).left),
                           carray_to_vec_term((*t).num_args, (*t).args)),
            ELIM => Elim(cvar_to_var((*t).var),
                         carray_to_vec_term((*t).num_args, (*t).args)),
            DATATYPE => Data(cvar_to_var((*t).var)),
        }
    )
}

fn string_to_cstring(s: &String) -> *mut libc::c_char {
    unsafe { 
        strdup(CString::new(s.clone()).unwrap().as_ptr())
    }
}

fn var_to_cvar(x: &Variable) -> *mut variable {
    unsafe { 
        make_variable(string_to_cstring(&x.name))
    }
}

unsafe fn fill_out_carray(dest: *mut *mut term, src: &Vec<Term>) {
    for (i, t) in src.iter().enumerate() {
        *dest.offset(i as isize) = term_to_cterm(t);
    }
}

pub fn term_to_cterm(t: &Term) -> *mut term {
    unsafe {
        match *t {
            Type => make_type(),
            Hole => make_hole(),
            Var(ref name) => make_var(var_to_cvar(name)),
            Lambda(ref x, ref a, ref b) => make_lambda(var_to_cvar(x),
                                                   match *a { None => ptr::null_mut(), Some(ref t) => term_to_cterm(t) }, 
                                                   term_to_cterm(b)),
            Pi(ref x, ref a, ref b) => make_pi(var_to_cvar(x),
                                               term_to_cterm(a), 
                                               term_to_cterm(b)),
            App(ref a, ref b) => make_app(term_to_cterm(a), 
                                          term_to_cterm(b)),
            Intro(ref name, ref a, ref v) => {
                let t = make_intro(var_to_cvar(name), 
                                   term_to_cterm(a),
                                   v.len() as libc::c_int);
                fill_out_carray((*t).args, v);
                t
            },
            Elim(ref name, ref v) => {
                let t = make_elim(var_to_cvar(name), 
                                  v.len() as libc::c_int);
                fill_out_carray((*t).args, v);
                t
            },
            Data(ref name) => make_datatype_term(var_to_cvar(name)),
        }
    }
}
