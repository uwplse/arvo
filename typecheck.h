#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "term.h"
#include "telescope.h"
#include "context.h"
#include "typing_context.h"

term* typecheck(telescope* Gamma, context *Sigma, typing_context* Delta, term* t);
int has_type(telescope *Gamma, context *Sigma, typing_context* Delta, term *t, term *ty);
#endif  // TYPECHECK_H
