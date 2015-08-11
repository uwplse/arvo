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

  check(argc > 1, "Need a filename.");

  char* filename = strdup(argv[1]);
  vernac_init(dirname(filename));
  free(filename);
  filename = NULL;

  int ans = process_file(argv[1]);

  return ans;
 error:
  return 1;
}
