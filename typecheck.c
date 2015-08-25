#include "term.h"
#include "typecheck.h"
#include "context.h"
#include "normalize.h"
#include "printing.h"

static term* typecheck_app(telescope* Gamma, context* Sigma, typing_context* Delta, term* fun, term* arg) {
  term* tyArg = NULL;

  term* tyFun = whnf_and_free(Sigma, Delta, typecheck(Gamma, Sigma, Delta, fun));
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

int typecheck_check(telescope* Gamma, context *Sigma, typing_context* Delta, term* t, term* ty);

int typecheck_check_and_free_type(telescope* Gamma, context *Sigma, typing_context* Delta, term* t, term* ty) {
  int ans = typecheck_check(Gamma, Sigma, Delta, t, ty);
  free_term(ty);
  return ans;
}

int typecheck_check(telescope* Gamma, context *Sigma, typing_context* Delta, term* t, term* ty) {
  check(t, "term must be non-NULL");
  check(term_locally_well_formed(t), "term must be well formed");
  check(ty, "type must be non-NULL");
  check(term_locally_well_formed(ty), "type must be well formed");

  switch (t->tag) {
  case HOLE:
    {
      term* nty = whnf_no_unfold(Sigma, Delta, ty);
      log_info("Hole has type %W", nty, print_term);
      free_term(nty);
      return 1;
    }
  case IMPLICIT:
    sentinel("Unexpected implicit variable during typechecking. Elaborate first.");
  case LAM:
    {
      term* nty = whnf(Sigma, Delta, ty);
      check(nty->tag == PI, "checking lambda term %W against non-pi %W", t, print_term, nty, print_term);
      if (t->left != NULL) {
        check(typecheck_check_and_free_type(Gamma, Sigma, Delta, t->left, make_type()), "Bad annotation %W", t->left, print_term);
        check(definitionally_equal(Sigma, Delta, t->left, nty->left),
              "annotation %W does not match type %W",
              t->left, print_term, nty->left, print_term);
      }
      if (variable_equal(t->var, &ignore)) {
        int ans = typecheck_check(Gamma, Sigma, Delta, t->right, nty->right);
        free_term(nty);
        return ans;
      }
      term* tvar = make_var(variable_dup(t->var));
      Gamma = telescope_add(variable_dup(t->var), substitute(nty->var, tvar, nty->left), Gamma);
      term* codomain = substitute(nty->var, tvar, nty->right);

      int ans = typecheck_check(Gamma, Sigma, Delta, t->right, codomain);
      telescope_pop(Gamma);
      free_term(tvar);
      free_term(codomain);
      free_term(nty);
      return ans;
    }
  default:
    {
      term* inferred = typecheck_infer(Gamma, Sigma, Delta, t);
      int ans = definitionally_equal(Sigma, Delta, ty, inferred);
      if (!ans) {
        term* nty = whnf(Sigma, Delta, ty);
        term* ninf = whnf(Sigma, Delta, inferred);
        log_err("%W expected to have type %W but has type %W", t, print_term, nty, print_term, ninf, print_term);
        free_term(nty);
        free_term(ninf);
      }
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
      check(ty != NULL, "variable %s unbound", t->var->name);
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
  case DATATYPE:
    return make_type();
  case INTRO:
    return term_dup(t->left);
  case ELIM:
    return make_app(term_dup(t->args[0]), term_dup(t->args[t->num_args-1]));
  case IMPLICIT:
    sentinel("Cannot infer type of implicit.");
  case HOLE:
    sentinel("Cannot infer type of hole.");
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
