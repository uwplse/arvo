#define _GNU_SOURCE

#include "printing.h"
#include "dbg.h"
#include "term.h"
#include "telescope.h"
#include "typecheck.h"
#include "parser.h"

int main(int argc, char **argv) {
  setup_printing();

  command *c;
  check(argc > 1, "Need a filename.");
  check(parse(argv[1]), "Parse error");

  vernac_init();
  while((c = next_command())) {
    vernac_run(c);
    free_command(c);
  }
  free_ast();
    
  return 0;
 error:
  return 1;
}
