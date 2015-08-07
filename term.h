#ifndef TERM_H
#define TERM_H

#include "dbg.h"

// based on pi-forall: https://github.com/sweirich/pi-forall
/*
 * e ::= x
 *     | \ x : e . e
 *     | (x : e) -> e
 *     | e e 
 *     | Type
 */

typedef enum {
  VAR,
  LAM,
  PI, 
  APP,
  TYPE
} term_tag;

const char* term_tag_names[TYPE+1] = {"VAR", "LAM", "PI", "APP", "TYPE"};

const char* term_tag_to_string(term_tag tag) {
  check(0 <= tag && tag <= TYPE, "Bad tag %d", tag);
  return term_tag_names[tag];
 error:
  return NULL;
}

typedef struct {
  char* name;
} variable;

typedef struct term {
  term_tag tag;
  variable* var;       // Valid for VAR, LAM, PI, otherwise NULL
  struct term* left;   // Valid for LAM, PI, APP, otherwise NULL
  struct term* right;  // Valid for LAM, PI, APP, otherwise NULL
} term;

int term_locally_well_formed(term* t) {
  if (t == NULL) return 1;

  switch (t->tag) {
  case VAR:
    return (t->var != NULL);
  case LAM:
  case PI:
    return (t->var != NULL) && (t->left != NULL) && (t->right != NULL);
  case APP:
    return (t->left != NULL) && (t->right != NULL);
  case TYPE:
    return 1;
  default:
    return 0;
  }
}

char* term_to_string(term* t);

term* make_pi(variable* x, term* A, term* B);

term* make_type();

term* normalize(term* t);

int definitionally_equal(term* a, term* b);

term* substitute(variable* from, term* to, term* haystack);

#endif  // TERM_H
