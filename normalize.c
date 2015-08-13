#include "term.h"
#include "normalize.h"
#include "printing.h"

#define FUEL 1000

static term* elim_over_intro(typing_context* Delta, term* t);

term* whnf_and_free(context *Sigma, typing_context* Delta, term* t) {
  term* ans = whnf(Sigma, Delta, t);
  free_term(t);
  return ans;
}

term* whnf(context *Sigma, typing_context* Delta, term* t) {
  if (t == NULL) return NULL;

  switch (t->tag) {
  case VAR:
    {
      term* defn = context_lookup(t->var, Sigma);
      if (defn == NULL) {
        return term_dup(t);
      }
      return whnf(Sigma, Delta, defn);
    }
  case APP:
    {
      term* l = whnf(Sigma, Delta, t->left);
      if (l->tag == LAM) {
        term* subs = substitute(l->var, t->right, l->right);
        free_term(l);
        return whnf_and_free(Sigma, Delta, subs);
      }
      return make_app(l, term_dup(t->right));
    }
  case ELIM:
    {
      term* last = t->args[t->num_args - 1];
      term* nlast = whnf(Sigma, Delta, last);
      term* c = term_dup(t);
      free_term(c->args[c->num_args - 1]);
      c->args[c->num_args - 1] = nlast;
      return whnf_and_free(Sigma, Delta, c);
    }
  case HOLE:
  case DATATYPE:
  case TYPE:
  case LAM:
  case INTRO:
  case PI:
    return term_dup(t);
  }
}

static term * normalize_fuel(context *Sigma, typing_context* Delta, term* t, int fuel);

static term* normalize_and_free_fuel(context *Sigma, typing_context* Delta, term* t, int fuel) {
  term* ans = normalize_fuel(Sigma, Delta, t, fuel);
  free_term(t);
  return ans;
}

term *normalize_and_free(context *Sigma, typing_context* Delta, term* t) {
  return normalize_and_free_fuel(Sigma, Delta, t, FUEL);
}

static term* normalize_fuel_app(context *Sigma, typing_context* Delta, term* t, int fuel) {
  term *f = normalize_fuel(Sigma, Delta, t->left, fuel-1);
  term *x = normalize_fuel(Sigma, Delta, t->right, fuel-1);
  if (!f || !x) goto error;
  if (f->tag == LAM) {
    term* subs = substitute(f->var, x, f->right);
    free_term(f);
    free_term(x);
    term* ans = normalize_fuel(Sigma, Delta, subs, fuel-1);
    free_term(subs);
    return ans;
  }
  return make_app(f, x);
 error:
  free_term(f);
  free_term(x);
  return NULL;
}

term* normalize_fuel_lambda(context *Sigma, typing_context* Delta, term* t, int fuel) {
  term* b = NULL;
  term* A = normalize_fuel(Sigma, Delta, t->left, fuel-1);

  context* extend = context_add(variable_dup(t->var), NULL, Sigma);
  b = normalize_fuel(extend, Delta, t->right, fuel-1);
  context_pop(extend);
  if (!b) goto error;
  return make_lambda(variable_dup(t->var), A, b);
 error:
  free_term(A);
  free_term(b);
  return NULL;
}

term* normalize_fuel_pi(context *Sigma, typing_context* Delta, term* t, int fuel) {
  term* B = NULL;
  term* A = normalize_fuel(Sigma, Delta, t->left, fuel-1);
  if (!A) goto error;
  context* extend = context_add(variable_dup(t->var), NULL, Sigma);
  B = normalize_fuel(extend, Delta, t->right, fuel-1);
  context_pop(extend);
  if (!B) goto error;
  return make_pi(variable_dup(t->var), A, B);
 error:
  free_term(A);
  free_term(B);
  return NULL;
}

static term* elim_over_intro(typing_context* Delta, term* t) {
  check(t && t->args && t->args[t->num_args - 1], "ill formed term");
  term* last = t->args[t->num_args - 1];
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
  free_term(t);
  t = NULL;
  return app;
 error:
  return NULL;
}

term* normalize_fuel_elim(context *Sigma, typing_context* Delta, term* t, int fuel) {
  term* ans = NULL;
  term* last = normalize_fuel(Sigma, Delta, t->args[t->num_args - 1], fuel-1);
  if (!last) goto error;
  if (last->tag == INTRO) {
    term* c = term_dup(t);
    free_term(c->args[c->num_args - 1]);
    c->args[c->num_args - 1] = last;
    return normalize_and_free_fuel(Sigma, Delta, elim_over_intro(Delta, c), fuel-1);
  } else {
    ans = make_elim(variable_dup(t->var), t->num_args);
    int i;
    for (i = 0; i < t->num_args - 1; i++) {
      ans->args[i] = normalize_fuel(Sigma, Delta, t->args[i], fuel-1);
      if (!ans->args[i]) goto error;
    }
    ans->args[t->num_args-1] = last;
    return ans;
  }
 error:
  free_term(last);
  free_term(ans);
  return NULL;
}

term* normalize_fuel_intro(context *Sigma, typing_context* Delta, term* t, int fuel) {
  term* ans = make_intro(variable_dup(t->var), term_dup(t->left), t->num_args);
  int i;
  for (i = 0; i < t->num_args; i++) {
    // FIXME: this leaks on error --jrw
    ans->args[i] = normalize_fuel(Sigma, Delta, t->args[i], fuel-1);
    if (!ans->args[i]) goto error;
  }
  return ans;
 error:
  free_term(ans);
  return NULL;
}

term* normalize_fuel_var(context *Sigma, typing_context* Delta, term* t, int fuel) {
  term* defn = context_lookup(t->var, Sigma);
  if (defn == NULL) {
    return term_dup(t);
  }
  return normalize_fuel(Sigma, Delta, defn, fuel-1);
}

term* normalize_fuel(context *Sigma, typing_context* Delta, term* t, int fuel) {
  if (t == NULL) return NULL;
  check(term_locally_well_formed(t), "t must be locally well formed");
  check(fuel >= 0, "Stack depth exceeded")

  switch (t->tag) {
  case VAR:
    return normalize_fuel_var(Sigma, Delta, t, fuel);
  case APP:
    return normalize_fuel_app(Sigma, Delta, t, fuel);
  case LAM:
    return normalize_fuel_lambda(Sigma, Delta, t, fuel);
  case PI:
    return normalize_fuel_pi(Sigma, Delta, t, fuel);
  case ELIM:
    return normalize_fuel_elim(Sigma, Delta, t, fuel);
  case INTRO:
    return normalize_fuel_intro(Sigma, Delta, t, fuel);
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
