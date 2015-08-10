#include "term.h"
#include "telescope.h"
#include "printing.h"
#include <stdlib.h>

telescope* telescope_empty() {
  return NULL;
}

telescope* telescope_add(variable* var, term* ty, telescope* rest) {
  telescope *new = malloc(sizeof(telescope));
  new->var = var;
  new->ty = ty;
  new->rest = rest;
  return new;
}

term* telescope_lookup(variable* var, telescope* Gamma) {
  while(Gamma) {
    if (variable_equal(var, Gamma->var)) {
      return Gamma->ty;
    }
    Gamma = Gamma->rest;
  }
  return NULL;
}

int print_telescope(FILE* stream, telescope* Gamma) {
  if (Gamma) {
    return fprintf(stream, "%W, (%W : %W)", Gamma->rest, print_telescope, Gamma->var, print_variable, Gamma->ty, print_term);
  }
  else {
    return fprintf(stream, ".");
  }
}

telescope* telescope_pop(telescope* Gamma) {
  if (Gamma == NULL) return NULL;

  free_variable(Gamma->var);
  Gamma->var = NULL;
  free_term(Gamma->ty);
  Gamma->ty = NULL;

  telescope* ans = Gamma->rest;
  Gamma->rest = NULL;

  free(Gamma);

  return ans;
}
