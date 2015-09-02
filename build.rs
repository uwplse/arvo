extern crate gcc;
extern crate lalrpop;

fn main() {
    lalrpop::process_root().unwrap();
    gcc::compile_library("libarvo.a", &[
        "term.c",
        "typecheck.c",
        "telescope.c",
        "parser.c",
        "printing.c",
        "vernac.c",
        "context.c",
        "typing_context.c",
        "normalize.c",
        "mpc/mpc.c",
        "dbg.c"
    ]);
}
