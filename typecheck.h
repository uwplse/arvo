#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "term.h"
#include "telescope.h"
#include "context.h"
#include "typing_context.h"

int typecheck_check(telescope* Gamma, context *Sigma, typing_context* Delta, term* t, term* ty);

term* typecheck_infer(telescope* Gamma, context *Sigma, typing_context* Delta, term* t);

#define typecheck(G,S,D,t) typecheck_infer(G,S,D,t)

int has_type(telescope *Gamma, context *Sigma, typing_context* Delta, term *t, term *ty);
#endif  // TYPECHECK_H
