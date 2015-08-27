#define _GNU_SOURCE

#include "printing.h"
#include "dbg.h"
#include "term.h"
#include "telescope.h"
#include "typecheck.h"
#include "parser.h"

#include <libgen.h>
#include <stdlib.h>

int main(int argc, char **argv) {
  setup_printing();
  initialize_arvo_parsers();

  int ans = 0;

  if (argc > 1) {
    char* filename = strdup(argv[1]);
    vernac_init(dirname(filename));
    free(filename);
    filename = NULL;

    ans = process_file(argv[1]);
  } else {
    vernac_init(".");
    ans = process_stream("standard input", stdin);
  }

  cleanup_arvo_parsers();

  return ans;
}
