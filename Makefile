default: arvo
clean :
	rm -rf arvo

arvo: main.c term.c typecheck.c telescope.c parser.c printing.c vernac.c context.c typing_context.c normalize.c  mpc/mpc.c dbg.c target/debug/libarvo.a
	gcc -Wall -Wextra -Wno-format -g -pthread main.c term.c typecheck.c telescope.c parser.c vernac.c context.c typing_context.c normalize.c mpc/mpc.c printing.c dbg.c ./target/debug/libarvo.a -o arvo -lm -ldl -lrt

target/debug/libarvo.a: src/ffi.rs src/lib.rs src/prettyprint.rs src/term.rs
	cargo build

.PHONY : default clean
