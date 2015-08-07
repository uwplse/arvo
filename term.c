#include "term.h"
#include <stdlib.h>

char* term_to_string(term* t) {
  return "<term>";
}

static term* make_term() {
  term* ans = malloc(sizeof(term));
  ans->var = NULL;
  ans->left = NULL;
  ans->right = NULL;

  return ans;
}

term* make_pi(variable* x, term* A, term* B) {
  term* ans = make_term();
  ans->tag = PI;
  ans->var = x;
  ans->left = A;
  ans->right = B;

  return ans;
}

term* make_type() {
  term* ans = make_term();
  ans->tag = TYPE;

  return ans;
}

static term* make_lambda(variable* var, term* A, term* b) {
  term* ans = make_term();
  ans->tag = LAM;
  ans->var = var;
  ans->left = A;
  ans->right = b;

  return ans;
}

static term* make_app(term* a, term* b) {
  term* ans = make_term();
  ans->tag = APP;
  ans->left = a;
  ans->right = b;
  
  return ans;
}

term* normalize(term* t) {
  return t;
}

static int variable_equal(variable* x, variable* y) {
  return (x == NULL && y == NULL) || (strcmp(x->name, y->name) == 0);
}

static int syntactically_identical(term* a, term* b) {
  check(a != NULL && b != NULL, "alpha_equiv requires non-NULL arguments");
  check(term_locally_well_formed(a) && term_locally_well_formed(b), 
        "alpha equiv requires well-formed arguments");

  if (a->tag != b-> tag) return 0;

  switch (a->tag) {
  case VAR:
    return variable_equal(a->var, b->var);
  case LAM:
    return 
      variable_equal(a->var, b->var) &&
      syntactically_identical(a->left, b->left) &&
      syntactically_identical(a->right, b->right);
  case PI:
    return 
      variable_equal(a->var, b->var) &&
      syntactically_identical(a->left, b->left) &&
      syntactically_identical(a->right, b->right);
  case APP:
    return 
      syntactically_identical(a->left, b->left) &&
      syntactically_identical(a->right, b->right);
  case TYPE:
    return 1;
  default:
    sentinel("malformed term");
  }
  
 error:
  return 0;
}

int definitionally_equal(term* a, term* b) {
  return syntactically_identical(normalize(a), normalize(b));
}



term* substitute(variable* from, term* to, term* haystack) {
  check(from != NULL && to != NULL && haystack != NULL, "substitute requires non-NULL arguments");
  check(term_locally_well_formed(to) && term_locally_well_formed(haystack), 
        "substitute requires locally well-formed arguments");

  switch(haystack->tag) {
  case VAR:
    if (variable_equal(from, haystack->var)) {
      return to;
    } else {
      return haystack;
    }
  case LAM:
    if (variable_equal(from, haystack->var)) {
      return make_lambda(haystack->var, substitute(from, to, haystack->left), haystack->right);
    } else {
      return make_lambda(haystack->var, 
                         substitute(from, to, haystack->left), 
                         substitute(from, to, haystack->right));
    }
  case PI:
    if (variable_equal(from, haystack->var)) {
      return make_pi(haystack->var, substitute(from, to, haystack->left), haystack->right);
    } else {
      return make_pi(haystack->var, 
                     substitute(from, to, haystack->left), 
                     substitute(from, to, haystack->right));
    }
  case APP:
    return make_app(substitute(from, to, haystack->left),
                    substitute(from, to, haystack->right));
  case TYPE:
  default:
    sentinel("malformed term");
  }

 error:
  return NULL;
}

