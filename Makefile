default: arvo
clean :
	rm -rf arvo

FILES = main.c term.c typecheck.c telescope.c parser.c printing.c vernac.c context.c typing_context.c normalize.c mpc/mpc.c dbg.c

CFLAGS = -Wall -Wextra -Wno-format -g

LFLAGS = -lm -lm -ldl

UNAME := $(shell sh -c 'uname -s 2>/dev/null')


ifeq ($(UNAME),Linux)
	LFLAGS += -lrt
endif

arvo: $(FILES) target/debug/libarvo.a
	gcc $(CFLAGS) -pthread $(FILES) ./target/debug/libarvo.a -o arvo $(LFLAGS)

target/debug/libarvo.a: src/ffi.rs src/lib.rs src/prettyprint.rs src/term.rs
	cargo build

.PHONY : default clean
