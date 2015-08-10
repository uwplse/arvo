#include "term.h"
#include "typecheck.h"
#include "context.h"
#include "normalize.h"
#include "printing.h"

static term* typecheck_app(telescope* Gamma, context* Sigma, typing_context* Delta, term* fun, term* arg) {
  term* tyArg = NULL;

  term* tyFun = typecheck(Gamma, Sigma, Delta, fun);
  check(tyFun != NULL, "Bad function %W", fun, print_term);
  check(tyFun->tag == PI, "Function %W has type %W but expected to have pi-type",
        fun, print_term, tyFun, print_term);

  tyArg = typecheck(Gamma, Sigma, Delta, arg);
  check(tyArg != NULL, "Bad argument %W", arg, print_term);
  check(definitionally_equal(Sigma, Delta, tyFun->left, tyArg),
        "Type mismatch in function application: function has domain %W but argument has type %W",
        tyFun->left, print_term, tyArg, print_term);

  term* ans = substitute(tyFun->var, arg, tyFun->right);

  free_term(tyArg);
  free_term(tyFun);

  return ans;
 error:
  free_term(tyFun);
  free_term(tyArg);
  return NULL;
}

static term* typecheck_lam(telescope* Gamma, context* Sigma, typing_context* Delta, variable* v, term* A, term* body) {
  term* tyBody = NULL;
  term* tyA = typecheck(Gamma, Sigma, Delta, A);
  check(tyA != NULL, "Bad type annotation %W", A, print_term);
  check(tyA->tag == TYPE, "Annotation has type %W, but expected to have type Type",
        tyA, print_term);
  free_term(tyA);
  tyA = NULL;

  if (!variable_equal(v, &ignore)) Gamma = telescope_add(variable_dup(v), term_dup(A), Gamma);
  tyBody = typecheck(Gamma, Sigma, Delta, body);
  check(tyBody != NULL, "Bad body %W", body, print_term);
  if (!variable_equal(v, &ignore)) telescope_pop(Gamma);
  return make_pi(variable_dup(v), term_dup(A), tyBody);
 error:
  free_term(tyA);
  free_term(tyBody);
  if (!variable_equal(v, &ignore)) telescope_pop(Gamma);
  return NULL;
}

static term* typecheck_pi(telescope* Gamma, context* Sigma, typing_context* Delta, variable* v, term* A, term* B) {
  term* tyB = NULL;
  term* tyA = typecheck(Gamma, Sigma, Delta, A);
  check(tyA != NULL, "Bad domain %W", A, print_term);
  check(tyA->tag == TYPE, "Domain %W has type %W, but expected to have type Type",
        A, print_term, tyA, print_term);
  free_term(tyA);
  tyA = NULL;

  if (!variable_equal(v, &ignore)) Gamma = telescope_add(v, A, Gamma);
  tyB = typecheck(Gamma, Sigma, Delta, B);
  check(tyB != NULL, "Bad codomain %W", B, print_term);
  check(tyB->tag == TYPE, "Codomain %W has type %W, but expected to have type Type",
        B, print_term, tyB, print_term);
  free_term(tyB);

  return make_type();
 error:
  free_term(tyA);
  free_term(tyB);
  return NULL;
}



/*
  Caller should free result.
 */
term* typecheck(telescope* Gamma, context *Sigma, typing_context* Delta, term* t) {
  check(t, "term must be non-NULL");
  check(term_locally_well_formed(t), "term must be well formed");

  switch (t->tag) {
  case VAR:
    {
      term* ty = telescope_lookup(t->var, Gamma);
      check(ty != NULL, "variable %s unbound in context %W", t->var->name, Gamma, print_telescope);
      return term_dup(ty);
    }
  case LAM:
    return typecheck_lam(Gamma, Sigma, Delta, t->var, t->left, t->right);
  case PI:
    return typecheck_pi(Gamma, Sigma, Delta, t->var, t->left, t->right);
  case APP:
    return typecheck_app(Gamma, Sigma, Delta, t->left, t->right);
  case TYPE:
    return make_type();
  default:
    sentinel("Bad tag %d", t->tag);
  }

 error:
  return NULL;
}

int has_type(telescope *Gamma, context *Sigma, typing_context* Delta, term *t, term *ty) {
  term* tty = typecheck(Gamma, Sigma, Delta, t);
  int result = definitionally_equal(Sigma, Delta, tty, ty);
  free_term(tty);
  return result;
}
