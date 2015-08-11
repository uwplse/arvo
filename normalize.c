#include "term.h"
#include "normalize.h"
#include "printing.h"

#define FUEL 1000

static term * normalize_fuel(context *Sigma, typing_context* Delta, term* t, int fuel);

static term* normalize_and_free_fuel(context *Sigma, typing_context* Delta, term* t, int fuel) {
  term* ans = normalize_fuel(Sigma, Delta, t, fuel);
  free_term(t);
  return ans;
}

term *normalize_and_free(context *Sigma, typing_context* Delta, term* t) {
  return normalize_and_free_fuel(Sigma, Delta, t, FUEL);
}

term* normalize_fuel(context *Sigma, typing_context* Delta, term* t, int fuel) {
  check(t, "t must be non-NULL");
  check(term_locally_well_formed(t), "t must be locally well formed");

  switch (t->tag) {
  case VAR:
    {
      term* defn = context_lookup(t->var, Sigma);
      if (defn == NULL) {
        return term_dup(t);
      }
      return normalize_fuel(Sigma, Delta, defn, fuel-1);
    }
  case APP:
    {
      term *f = normalize_fuel(Sigma, Delta, t->left, fuel-1);
      term *x = normalize_fuel(Sigma, Delta, t->right, fuel-1);
      if (f->tag == LAM) {
        term* subs = substitute(f->var, x, f->right);
        free_term(f);
        free_term(x);
        term* ans = normalize_fuel(Sigma, Delta, subs, fuel-1);
        free_term(subs);
        return ans;
      }
      return make_app(f, x);
    }
  case LAM:
    {
      term* A = normalize_fuel(Sigma, Delta, t->left, fuel-1);
      context* extend = context_add(variable_dup(t->var), NULL, Sigma);
      term* b = normalize_fuel(extend, Delta, t->right, fuel-1);
      context_pop(extend);
      return make_lambda(variable_dup(t->var), A, b);
    }
  case PI:
    {
      term* A = normalize_fuel(Sigma, Delta, t->left, fuel-1);
      context* extend = context_add(variable_dup(t->var), NULL, Sigma);
      term* B = normalize_fuel(extend, Delta, t->right, fuel-1);
      context_pop(extend);
      return make_pi(variable_dup(t->var), A, B);
    }
  case ELIM:
    {
      term* last = normalize_fuel(Sigma, Delta, t->args[t->num_args - 1], fuel-1);
      if (last->tag == INTRO) {
        datatype* T = elim_to_datatype(t->var, Delta);
        int index = datatype_intro_index(last->var, T);
        term *app = term_dup(t->args[index + 1]);
        int i;
        for (i = 0; i < last->num_args; i++) {
          app = make_app(app, term_dup(last->args[i]));
          if (constructor_arg_is_inductive(T, last->var, i)) {
            term *inductive = term_dup(t);
            free_term(inductive->args[inductive->num_args - 1]);
            inductive->args[inductive->num_args - 1] = term_dup(last->args[i]);
            app = make_app(app, inductive);
          }
        }
        free_term(last);
        last = NULL;
        return normalize_and_free_fuel(Sigma, Delta, app, fuel-1);
      } else {
        term* ans = make_elim(variable_dup(t->var), t->num_args);
        int i;
        for (i = 0; i < t->num_args - 1; i++) {
          ans->args[i] = normalize_fuel(Sigma, Delta, t->args[i], fuel-1);
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
        ans->args[i] = normalize_fuel(Sigma, Delta, t->args[i], fuel-1);
      }
      return ans;
    }
  default:
    return term_dup(t);
  }
 error:
  return NULL;
}

term* normalize(context *Sigma, typing_context* Delta, term* t) {
  return normalize_fuel(Sigma, Delta, t, FUEL);
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

