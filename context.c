#include "term.h"
#include "context.h"
#include "printing.h"
#include <stdlib.h>

context* context_empty() {
  return NULL;
}

context* context_add(variable* var, term* t, context* rest) {
  context *new = malloc(sizeof(context));
  new->var = var;
  new->t = t;
  new->rest = rest;
  return new;
}

term* context_lookup(variable* var, context* Sigma) {
  while(Sigma) {
    if (variable_equal(var, Sigma->var)) {
      return Sigma->t;
    }
    Sigma = Sigma->rest;
  }
  return NULL;
}

int print_context(FILE* stream, context* Sigma) {
  if (Sigma) {
    return fprintf(stream, "%W, (%W : %W)", Sigma->rest, print_context, Sigma->var, print_variable, Sigma->t, print_term);
  }
  else {
    return fprintf(stream, ".");
  }
}

context* context_pop(context* Sigma) {
  if (Sigma == NULL) return NULL;

  free_variable(Sigma->var);
  Sigma->var = NULL;
  free_term(Sigma->t);
  Sigma->t = NULL;

  context* ans = Sigma->rest;
  Sigma->rest = NULL;

  free(Sigma);

  return ans;
}
