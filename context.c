#include "telescope.h"
#include "context.h"

#include <stdlib.h>

// Note: these are represented in reverse, so that rest is always well formed by itself.

context* context_empty() {
  return telescope_empty();
}

context* context_add(variable* var, term* t, context* rest) {
  return telescope_add(var, t, rest);
}

term* context_lookup(variable* var, context* Sigma) {
  return telescope_lookup(var, Sigma);
}


context* context_pop(context* Sigma) {
  if (Sigma == NULL) return NULL;

  free_variable(Sigma->var);
  Sigma->var = NULL;
  free_term(Sigma->ty);
  Sigma->ty = NULL;

  telescope* ans = Sigma->rest;
  Sigma->rest = NULL;

  free(Sigma);

  return ans;
}


int print_context(FILE* stream, context* Sigma) {
  return print_telescope(stream, Sigma);
}
