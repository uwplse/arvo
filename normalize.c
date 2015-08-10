#include "term.h"
#include "normalize.h"
#include "printing.h"


term* normalize(context *Sigma, typing_context* Delta, term* t) {
  check(t, "t must be non-NULL");
  check(term_locally_well_formed(t), "t must be locally well formed");

  switch (t->tag) {
  case VAR:
    {
      term* defn = context_lookup(t->var, Sigma);
      if (defn == NULL) {
        return term_dup(t);
      }
      return normalize(Sigma, Delta, defn);
    }
  case APP:
    {
      term *f = normalize(Sigma, Delta, t->left);
      term *x = normalize(Sigma, Delta, t->right);
      if (f->tag == LAM) {
        term* subs = substitute(f->var, x, f->right);
        free_term(f);
        free_term(x);
        term* ans = normalize(Sigma, Delta, subs);
        free_term(subs);
        return ans;
      }
      return make_app(f, x);
    }
  case LAM:
    {
      term* A = normalize(Sigma, Delta, t->left);
      context* extend = context_add(variable_dup(t->var), NULL, Sigma);
      term* b = normalize(extend, Delta, t->right);
      context_pop(extend);
      return make_lambda(variable_dup(t->var), A, b);
    }
  case PI:
    {
      term* A = normalize(Sigma, Delta, t->left);
      context* extend = context_add(variable_dup(t->var), NULL, Sigma);
      term* B = normalize(extend, Delta, t->right);
      context_pop(extend);
      return make_pi(variable_dup(t->var), A, B);
    }
  case ELIM:
    {
      term* last = normalize(Sigma, Delta, t->args[t->num_args - 1]);
      if (last->tag == INTRO) {
        datatype* T = elim_to_datatype(t->var, Delta);
        int index = datatype_intro_index(last->var, T);
        free_term(last);
        last = NULL;
        return normalize(Sigma, Delta, t->args[index + 1]);
      } else {
        term* ans = make_elim(variable_dup(t->var), t->num_args);
        int i;
        for (i = 0; i < t->num_args - 1; i++) {
          ans->args[i] = normalize(Sigma, Delta, t->args[i]);
        }
        ans->args[t->num_args-1] = last;
        return ans;
      }
    }
  case INTRO:
    {
      term* ans = make_intro(variable_dup(t->var), t->num_args);
      int i;
      for (i = 0; i < t->num_args; i++) {
        ans->args[i] = normalize(Sigma, Delta, t->args[i]);
      }
      return ans;
    }
  default:
    return term_dup(t);
  }
 error:
  return NULL;
}

int definitionally_equal(context *Sigma, typing_context* Delta, term* a, term* b) {
  term* na = normalize(Sigma, Delta, a);
  term* nb = normalize(Sigma, Delta, b);
  int ans = syntactically_identical(na, nb);
  free_term(na);
  free_term(nb);
  return ans;
}

int is_pi_returning(context *Sigma, typing_context *Delta, term *t, term *val) {
  if (definitionally_equal(Sigma, Delta, t, val)) {
    return 1;
  }
  if (t->tag == PI) {
    return is_pi_returning(Sigma, Delta, t->right, val);
  }
  return 0;
}

