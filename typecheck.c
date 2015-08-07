#include "term.h"
#include "typecheck.h"

term* typecheck(telescope* Gamma, term* t) {
  check(t, "term must be non-NULL");
  check(term_locally_well_formed(t), "term must be well formed");

  switch (t->tag) {
  case VAR:
    { 
      term* ty = telescope_lookup(t->var, Gamma);
      check(ty != NULL, "variable %s unbound in context %s", t->var->name, telescope_to_string(Gamma));
      return ty;
    }
  case LAM:
    {
      term* A = t->left;
      term* tyA = typecheck(Gamma, A);
      check(tyA != NULL, "Bad type annotation %s", term_to_string(A));
      check(tyA->tag == TYPE, "Annotation has type %s, but expected to have type Type", 
            term_to_string(tyA));
      
      term* body = t->right;
      term* tyBody = typecheck(telescope_add(t->var, A, Gamma), body);
      check(tyBody != NULL, "Bad body %s", term_to_string(body));
      return make_pi(t->var, A, tyBody);
    }
  case PI:
    {
      term* A = t->left;
      term* tyA = typecheck(Gamma, A);
      check(tyA != NULL, "Bad domain %s", term_to_string(A));
      check(tyA->tag == TYPE, "Domain %s has type %s, but expected to have type Type", 
            term_to_string(A), term_to_string(tyA));
      
      term* B = t->right;
      term* tyB = typecheck(telescope_add(t->var, A, Gamma), B);
      check(tyB != NULL, "Bad codomain %s", term_to_string(B));
      check(tyB->tag == TYPE, "Codomain %s has type %s, but expected to have type Type", 
            term_to_string(B), term_to_string(tyB));

      return make_type();

    }
  case APP:
    {
      term* fun = t->left;
      term* tyFun = typecheck(Gamma, fun);
      check(tyFun != NULL, "Bad function %s", term_to_string(fun));
      check(tyFun->tag == PI, "Function %s has type %s but expected to have pi-type", 
            term_to_string(fun), term_to_string(tyFun));

      term* arg = t->right;
      term* tyArg = typecheck(Gamma, arg);
      check(tyArg != NULL, "Bad argument %s", term_to_string(arg));
      check(definitionally_equal(tyFun->left, tyArg), 
            "Type mismatch in function application %s: function has domain %s but argument has type %s",
            term_to_string(t), term_to_string(tyFun->left), term_to_string(tyArg));

    }
  case TYPE:
    return make_type();
  default:
    sentinel("Bad tag %d", t->tag);
  }

 error: 
  return NULL;
}
