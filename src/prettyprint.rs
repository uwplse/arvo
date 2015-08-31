use term::Term;
use term::Term::*;

#[derive(Clone,Copy,Eq,PartialEq,PartialOrd,Ord,Debug)]
enum Prec {
    BOT,
    APP,
    LAM,
    TOP,
}

use prettyprint::Prec::*;

#[derive(Clone,Copy,Eq,PartialEq,PartialOrd,Ord)]
enum Side {
    LEFT,
    RIGHT,
    NONE
}

use prettyprint::Side::*;

pub fn prettyprint(t: &Term) -> String {
    let mut buf = String::new();
    go(&mut buf, t, Prec::TOP, NONE);
    buf
}

fn term_prec(t: &Term) -> Prec {
    match *t {
        Lambda(_,_,_) => LAM,
        Pi(_,_,_) => LAM,
        App(_,_) => APP,
        _ => BOT
    }
}

fn prec_assoc(p: Prec) -> Side {
    match p {
        LAM => RIGHT,
        APP => LEFT,
        TOP => NONE,
        BOT => NONE
    }
}

fn no_parens(ip: Prec, op: Prec, s: Side) -> bool {
    ip < op || (ip == op && s == prec_assoc(ip))
}

fn go(buf: &mut String, t: &Term, p: Prec, s: Side) {
    if (!no_parens(term_prec(t), p, s)) {
        buf.push_str("(");
        go(buf, t, TOP, NONE);
        buf.push_str(")");
        return;
    }
    match *t {
        Type => buf.push_str("Type"),
        Hole => buf.push_str("?"),
        Var(ref v) => buf.push_str(&v.name),
        Lambda(ref v, ref a, ref b) => {
            buf.push_str("\\");
            buf.push_str(&v.name);
            match *a {
                Some(ref ty) => {
                    buf.push_str(" : ");
                    go(buf, ty, Prec::TOP, NONE)
                }
                None => {}
            }
            buf.push_str(". ");
            go(buf, b, LAM, RIGHT)
        }
        Pi(ref v, ref a, ref b) => {
            if v.name != "_" {
                buf.push_str("(");
                buf.push_str(&v.name);
                buf.push_str(" : ");
                go(buf, a, TOP, NONE);
                buf.push_str(")");
            } else {
                go(buf, a, LAM, LEFT);
            }
            buf.push_str(" -> ");
            go(buf, b, LAM, RIGHT);

        }
        App(ref l, ref r) => {
            go(buf, l, APP, LEFT);
            buf.push_str(" ");
            go(buf, r, APP, RIGHT);
        }

        Data(ref v, ref params, ref indices) => {
            buf.push_str(&v.name);
            buf.push_str("(");
            let mut started = false;
            for param in params {
                if started {
                    buf.push_str("; ");
                }
                started = true;
                go(buf, param, TOP, NONE);
            }
            buf.push_str(")(");
            started = false;
            for index in indices {
                if started {
                    buf.push_str("; ");
                }
                started = true;
                go(buf, index, TOP, NONE);
            }
            buf.push_str(")");
        }
        Intro(ref v, _, ref args, ref params, ref indices) |
        Elim (ref v,    ref args, ref params, ref indices) => {
            buf.push_str(&v.name);
            buf.push_str("(");
            let mut started = false;
            for param in params {
                if started {
                    buf.push_str("; ");
                }
                started = true;
                go(buf, param, TOP, NONE);
            }
            buf.push_str(")(");
            started = false;
            for index in indices {
                if started {
                    buf.push_str("; ");
                }
                started = true;
                go(buf, index, TOP, NONE);
            }
            buf.push_str(")(");
            started = false;
            for arg in args {
                if started {
                    buf.push_str("; ");
                }
                started = true;
                go(buf, arg, TOP, NONE);
            }
            buf.push_str(")");


        }
    }
}
