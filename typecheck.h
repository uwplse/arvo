#ifndef TYPECHECK_H
#define TYPECHECK_H

#include "term.h"
#include "telescope.h"

term* typecheck(telescope* Gamma, term* t);
#endif  // TYPECHECK_H
