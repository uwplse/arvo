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


static term* typecheck_nat_ind(telescope* Gamma, context* Sigma, typing_context* Delta, term* P, term* z, term* s, term* n) {
  term* tyZ = NULL;
  term* pi_nat_to_type = NULL;
  term* P_O = NULL;
  term* tyS = NULL;
  term* succ_type = NULL;
  term* tyN = NULL;

  term *tyP = typecheck(Gamma, Sigma, Delta, P);
  check(tyP != NULL, "bad motive %W", P, print_term);
  pi_nat_to_type = make_pi(variable_dup(&ignore), make_nat(), make_type());
  check(definitionally_equal(Sigma, Delta, tyP, pi_nat_to_type),
        "bad motive for nat-ind; has type %W instead of %W",
        tyP, print_term,
        pi_nat_to_type, print_term);
  free_term(tyP);
  tyP = NULL;
  free_term(pi_nat_to_type);
  pi_nat_to_type = NULL;

  tyZ = typecheck(Gamma, Sigma, Delta, z);
  check(tyZ != NULL, "bad zero-case %W", z, print_term);
  P_O = make_app(term_dup(P), make_o());
  check(definitionally_equal(Sigma, Delta, tyZ, P_O),
        "bad zero-case for nat-ind; has type %W", tyZ, print_term);
  free_term(tyZ);
  tyZ = NULL;
  free_term(P_O);
  P_O = NULL;

  tyS = typecheck(Gamma, Sigma, Delta, s);
  check(tyS != NULL, "bad zero-case %W", s, print_term);
  // FIXME: I think we need to use gensym here instead of "n". --jrw
  term *tn = make_var(make_variable(strdup("n")));
  succ_type = make_pi(variable_dup(tn->var), make_nat(),
                            make_pi(variable_dup(&ignore), make_app(term_dup(P), tn),
                                    make_app(term_dup(P), make_app(make_s(), term_dup(tn)))));


  check(definitionally_equal(Sigma, Delta, tyS, succ_type),
        "bad succ-case for nat-ind; has type %W, should be %W", tyS, print_term,
        normalize(Sigma, Delta, succ_type), print_term_and_free);
  free_term(tyS);
  tyS = NULL;
  free_term(succ_type);
  succ_type = NULL;

  tyN = typecheck(Gamma, Sigma, Delta, n);
  check(tyN != NULL, "bad argument %W", n, print_term);
  check(tyN->tag == NAT, "bad argument for nat-ind; has type %W", tyN, print_term);
  free_term(tyN);
  tyN = NULL;

  return make_app(term_dup(P), term_dup(n));
 error:
  free_term(tyZ);
  free_term(pi_nat_to_type);
  free_term(P_O);
  free_term(tyS);
  free_term(succ_type);
  free_term(tyN);
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
  case NAT:
    return make_type();
  case NAT_IND:
    return typecheck_nat_ind(Gamma, Sigma, Delta, t->args[0], t->args[1], t->args[2], t->args[3]);
  case O:
    return make_nat();
  case S:
    return make_pi(variable_dup(&ignore), make_nat(), make_nat());
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
