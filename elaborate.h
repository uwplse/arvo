#ifndef ELABORATE_H
#define ELABORATE_H

#include "term.h"
#include "telescope.h"
#include "context.h"
#include "typing_context.h"

term* elaborate(telescope* Gamma, context* Sigma, typing_context* Delta, term* t, term* ty);

#endif  // ELABORATE_H
