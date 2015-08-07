#ifndef NORMALIZE_H
#define NORMALIZE_H

#include "term.h"
#include "context.h"
#include "typing_context.h"

term* normalize(context *Sigma, typing_context* Delta, term* t);
int definitionally_equal(context *Sigma, typing_context* Delta, term* a, term* b);
#endif  // NORMALIZE_H
