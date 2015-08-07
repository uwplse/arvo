#ifndef TELESCOPE_H
#define TELESCOPE_H

#include "term.h"

// Note: these are represented in reverse, so that rest is always well formed by itself.
typedef struct telescope {
  variable* var;
  term* ty;
  struct telescope* rest;
} telescope;

telescope* telescope_empty();

telescope* telescope_add(variable* var, term* ty, telescope* rest);

term* telescope_lookup(variable* var, telescope* Gamma);

char* telescope_to_string(telescope* Gamma);

#endif  // TELESCOPE_H
