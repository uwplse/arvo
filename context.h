#ifndef CONTEXT_H
#define CONTEXT_H

#include "term.h"

// Note: these are represented in reverse, so that rest is always well formed by itself.
typedef struct context {
  variable* var;
  term* t;
  struct context* rest;
} context;

context* context_empty();

context* context_add(variable* var, term* t, context* rest);

term* context_lookup(variable* var, context* Sigma);

context* context_pop(context* Sigma);

int print_context(FILE* stream, context* Sigma);

#endif  // CONTEXT_H
