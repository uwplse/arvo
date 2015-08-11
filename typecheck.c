#include "term.h"
#include "typecheck.h"
#include "context.h"
#include "normalize.h"
#include "printing.h"

static term* typecheck_app(telescope* Gamma, context* Sigma, typing_context* Delta, term* fun, term* arg) {
  term* tyArg = NULL;

  term* tyFun = normalize_and_free(Sigma, Delta, typecheck(Gamma, Sigma, Delta, fun));
  check(tyFun != NULL, "Bad function %W", fun, print_term);
  check(tyFun->tag == PI, "Function %W has type %W but expected to have pi-type",
        fun, print_term, tyFun, print_term);

  check(typecheck_check(Gamma, Sigma, Delta, arg, tyFun->left),
        "Type mismatch in function application: argument expected to have type %W",
        tyFun->left, print_term);

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

  if (!variable_equal(v, &ignore)) Gamma = telescope_add(variable_dup(v), term_dup(A), Gamma);
  tyB = typecheck(Gamma, Sigma, Delta, B);
  if (!variable_equal(v, &ignore)) telescope_pop(Gamma);
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

int typecheck_check(telescope* Gamma, context *Sigma, typing_context* Delta, term* t, term* ty) {
  check(t, "term must be non-NULL");
  check(term_locally_well_formed(t), "term must be well formed");
  check(ty, "type must be non-NULL");
  check(term_locally_well_formed(ty), "type must be well formed");

  switch (t->tag) {
  case LAM:
    if (t->left == NULL) {
      check(ty->tag == PI, "checking lambda against non-pi %W", ty, print_term);
      if (variable_equal(t->var, &ignore)) {
        return typecheck_check(Gamma, Sigma, Delta, t->right, ty->right);
      }
      term* tvar = make_var(variable_dup(t->var));
      Gamma = telescope_add(variable_dup(t->var), substitute(ty->var, tvar, ty->left), Gamma);
      term* codomain = substitute(ty->var, tvar, ty->right);
      int ans = typecheck_check(Gamma, Sigma, Delta, t->right, codomain);
      telescope_pop(Gamma);
      free_term(tvar);
      free_term(codomain);
      return ans;
    }
  default:
    {
      term* inferred = typecheck_infer(Gamma, Sigma, Delta, t);
      int ans = definitionally_equal(Sigma, Delta, ty, inferred);
      if (!ans)
        log_err("in context %W\n%W expected to have type %W but has type %W", Gamma, print_telescope, t, print_term, ty, print_term, inferred, print_term);
      free_term(inferred);
      return ans;
    }
  }

 error:
  return 0;
}

term* typecheck_infer(telescope* Gamma, context *Sigma, typing_context* Delta, term* t) {
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
    check(t->left != NULL, "Cannot infer type ");
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
