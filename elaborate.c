#include "printing.h"
#include "elaborate.h"
#include "normalize.h"

#include <stdlib.h>

typedef struct equation {
  term* left;
  term* right;
} equation;

typedef struct equation_set {
  equation* here;
  struct equation_set* rest;
} equation_set;

int print_equation(FILE* stream, equation* e) {
  return fprintf(stream, "%W = %W", e->left, print_term, e->right, print_term);
}

int print_equation_set(FILE* stream, equation_set* es) {
  int len = 0;
  while (es) {
    len += fprintf(stream, "%W\n", es->here, print_equation);
    es = es->rest;
  }
  return len;
}

equation_set* make_equation_set(equation* eq, equation_set* rest) {
  equation_set* ans = malloc(sizeof(equation_set));
  ans->here = eq;
  ans->rest = rest;
  return ans;
}

void equation_set_add(equation_set** es, equation* e) {
  *es = make_equation_set(e, *es);
}

void free_equation(equation* e) {
  if (e == NULL) return;
  free_term(e->left);
  e->left = NULL;
  free_term(e->right);
  e->right = NULL;
  free(e);
}

void pop_equation_set(equation_set** es) {
  equation* e = (*es)->here;
  equation_set* rest = (*es)->rest;
  (*es)->here = NULL;
  (*es)->rest = NULL;
  free(*es);
  *es = rest;
  free_equation(e);
}

void free_equation_set(equation_set* es) {
  while (es) {
    free_equation(es->here);
    es->here = NULL;
    equation_set* rest = es->rest;
    es->rest = NULL;
    free(es);
    es = rest;
  }
}

int occurs(term* x, term* t) {
  check(x->tag == IMPLICIT, "bad var");
  check(t, "bad term");

  switch (t->tag) {
  case APP:
    return occurs(x, t->left) || occurs(x, t->right);
  case PI:
  case LAM:
    return occurs(x, t->left) || occurs(x, t->right);
  case VAR:
  case TYPE:
    return 0;
  case IMPLICIT:
    return variable_equal(x->right->var, t->right->var);
  default:
    sentinel("bad tag %d", t->tag);
  }

 error:
  return 1;
}

void swap_term(term* a, term* b) {
  term tmp = *a;
  *a = *b;
  *b = tmp;
}

void substitute_implicit(term* from, term* to, term* haystack) {
  check(from->tag == IMPLICIT, "bad var");

  if (haystack == NULL) return;

  switch (haystack->tag) {
  case LAM:
  case PI:
  case APP:
    substitute_implicit(from, to, haystack->left);
    substitute_implicit(from, to, haystack->right);
    break;
  case HOLE:
  case VAR:
  case TYPE:
    break;
  case IMPLICIT:
    if (variable_equal(from->right->var, haystack->right->var)) {
      term* copy = term_dup(to);
      swap_term(haystack, copy);
      free_term(copy);
    }
    break;
  default:
    sentinel("bad tag %d", haystack->tag);
  }
  return;
 error:
  return;
}

void swap_ptr(void** a, void** b) {
  void* t = *a;
  *a = *b;
  *b = t;
}

void substitute_set(term* from, term* to, equation_set* es) {
  while (es) {
    substitute_implicit(from, to, es->here->left);
    substitute_implicit(from, to, es->here->right);
    es = es->rest;
  }
}

void substitute_set_rights(term* from, term* to, equation_set* es) {
  while (es) {
    substitute_implicit(from, to, es->here->right);
    es = es->rest;
  }
}

equation* make_equation(term* left, term* right) {
  equation* ans = malloc(sizeof(struct equation));
  ans->left = left;
  ans->right = right;
  return ans;
}

int unify(typing_context* Delta, equation_set** es, equation_set** ans) {
  equation_set** t = es;
  while (*t) {
    equation* e = (*t)->here;
    e->left = normalize_no_unfold_and_free(Delta, e->left);
    e->right = normalize_no_unfold_and_free(Delta, e->right);

    if (syntactically_identical(e->left, e->right)) {
      //log_info("deleting equation %W", e, print_equation);
      pop_equation_set(t);
      continue;
    }

    if (e->left->tag != IMPLICIT && e->right->tag == IMPLICIT) {
      //log_info("swapping %W", e, print_equation);
      term* tmp = e->right;
      e->right = e->left;
      e->left = tmp;
    }
    if (e->left->tag == IMPLICIT) {
      if (occurs(e->left, e->right)) {
        log_warn("equation %W fails occurs check", e, print_equation);
        return 0;
      } else {
        //log_info("substituting %W -> %W", e->left, print_term, e->right, print_term);
        (*t)->here = NULL;
        pop_equation_set(t);
        substitute_set(e->left, e->right, *es);
        substitute_set_rights(e->left, e->right, *ans);
        equation_set_add(ans, e);
        continue;
      }
    } else if (e->left->tag == APP && e->right->tag == APP) {
      term* l = term_dup(e->left);
      term* r = term_dup(e->right);
      equation* el = make_equation(term_dup(l->left), term_dup(r->left));
      equation* er = make_equation(term_dup(l->right), term_dup(r->right));
      //log_info("splitting %W to %W and %W", e, print_equation, el, print_equation, er, print_equation);
      free_term(l); l = NULL;
      free_term(r); r = NULL;

      pop_equation_set(t);

      equation_set_add(t, el);
      equation_set_add(t, er);
      continue;
    } else if (e->left->tag == PI && e->right->tag == PI) {
      term* l = term_dup(e->left);
      term* r = term_dup(e->right);
      equation* el = make_equation(term_dup(l->left), term_dup(r->left));
      equation* er = make_equation(term_dup(l->right), term_dup(r->right));

      (*t)->here = NULL;
      pop_equation_set(t);

      equation_set_add(t, el);
      if ((variable_equal(e->left->var, &ignore) ||
           !is_free(e->left->var, e->left->right)) &&
          (variable_equal(e->right->var, &ignore) ||
           !is_free(e->right->var, e->right->right))) {
        //log_info("splitting %W to %W and %W", e, print_equation, el, print_equation, er, print_equation);
        equation_set_add(t, er);
      } else {
        //log_warn("dropping equation %W because it's too hard", er, print_equation);
        free_equation(er); er = NULL;
      }
      free_term(l); l = NULL;
      free_term(r); r = NULL;
      free_equation(e); e = NULL;

      continue;
    } else {
      //log_info("not unifiable: %W", e, print_equation);
      pop_equation_set(t);
      continue;
    }
    sentinel("bad");
  }
  return 1;
 error:
  return 0;
}

term* generate_constraints(telescope* Gamma, typing_context* Delta, equation_set** es, term* t);

void climb(telescope* Gamma, typing_context* Delta, equation_set** es, term* t) {
  while (t->tag != TYPE && t->tag != IMPLICIT) {
    t = generate_constraints(Gamma, Delta, es , t);
  }
}

term* generate_constraints(telescope* Gamma, typing_context* Delta, equation_set** es, term* t) {
  //log_info("gc %W", t, print_term);
  if (t == NULL) return NULL;

  switch (t->tag) {
  case APP:
    {
      term* tyL = generate_constraints(Gamma, Delta, es, t->left);
      term* tyR = generate_constraints(Gamma, Delta, es, t->right);
      //climb(Gamma, Sigma, Delta, es, tyL);
      //climb(Gamma, Sigma, Delta, es, tyR);

      if (!tyL || !tyR) goto error;

      if (tyL->tag == PI) {
        equation_set_add(es, make_equation(term_dup(tyL->left), term_dup(tyR)));

        term* ans = substitute(tyL->var, t->right, tyL->right);
        //climb(Gamma, Sigma, Delta, es, ans);
        //log_info("%W has type %W", t, print_term, ans, print_term);
        free_term(tyL);
        free_term(tyR);
        return ans;
      } else {
        term* tau = make_implicit(fresh("tau"));
        term* pi = make_pi(variable_dup(&ignore), tyR, term_dup(tau));
        equation_set_add(es, make_equation(tyL, pi));
        return tau;
      }
    }
  case LAM:
    {
      term* tau = NULL;
      check(t->left != NULL, "bad annotation");

      free_term(generate_constraints(Gamma, Delta, es, t->left));

      tau = term_dup(t->left);

      Gamma = telescope_add(variable_dup(t->var), term_dup(tau), Gamma);
      term* result = generate_constraints(Gamma, Delta, es, t->right);

      telescope_pop(Gamma);


      term* ans = make_pi(variable_dup(t->var), tau, result);
      //log_info("%W has type %W", t, print_term, ans, print_term);
      return ans;
    }
  case PI:
    {
      free_term(generate_constraints(Gamma, Delta, es, t->left));
      //climb(Gamma, Sigma, Delta, es, tyL);

      if (!variable_equal(t->var, &ignore)) {
        Gamma = telescope_add(variable_dup(t->var), term_dup(t->left), Gamma);
      }
      free_term(generate_constraints(Gamma, Delta, es, t->right));
      //climb(Gamma, Sigma, Delta, es, tyR);
      if (!variable_equal(t->var, &ignore)) {
        telescope_pop(Gamma);
      }

      //log_info("%W has type Type", t, print_term);
      return make_type();
    }

  case IMPLICIT:
    return make_implicit(fresh("tau"));
  case HOLE:
    return make_implicit(fresh("type_of_hole"));
  case TYPE:
    return make_type();
  case VAR:
    {
      term* ty = telescope_lookup(t->var, Gamma);
      check(ty != NULL, "Unbound variable %W", t->var, print_variable);
      //log_info("%W has type %W", t, print_term, ty, print_term);
      return term_dup(ty);
    }
  default:
    sentinel("Unexpected tag %d", t->tag)
  }
 error:
  return NULL;
}

int no_implicits(term* t) {
  if (t == NULL) return 1;

  switch (t->tag) {
  case APP:
  case LAM:
  case PI:
    return no_implicits(t->left) && no_implicits(t->right);
  case IMPLICIT:
    return 0;
  case VAR:
  case TYPE:
  case HOLE:
    return 1;
  default:
    sentinel("bad tag %d")
  }
 error:
  return 0;
}

void apply_solution(equation_set* es, term* t) {
  while (es) {
    equation* e = es->here;
    if (no_implicits(e->right)) {
      substitute_implicit(e->left, e->right, t);
    } else {
      //log_warn("bad solution %W\n", es, print_equation_set);
    }

    es = es->rest;
  }
  return;
}

void erase_bad_annotations(term* t) {
  check(t, "t must be non-NULL");

  switch (t->tag) {
  case LAM:
    if (t->num_args != 0) {
      t->num_args = 0;
      free_term(t->left);
      t->left = NULL;
    }
    erase_bad_annotations(t->right);
    return;
  case APP:
  case PI:
    erase_bad_annotations(t->left);
    erase_bad_annotations(t->right);
    return;
  case VAR:
  case TYPE:
  case HOLE:
  case INTRO:
  case ELIM:
  case DATATYPE:
    return;
  case IMPLICIT:
    sentinel("unexpected implicit");

  }
 error:
  return;
}

term* elaborate(telescope* Gamma, typing_context* Delta, term* t, term* check) {
  if (t == NULL) return NULL;

  equation_set* s = NULL;

  //log_info("elaborating %W", t, print_term);
  t = normalize_no_unfold(Delta, t);
  term* ty = generate_constraints(Gamma, Delta, &s, t);
  if (check != NULL) {
    equation_set_add(&s, make_equation(ty, term_dup(check)));
  } else {
    free_term(ty);
    ty = NULL;
  }

  //log_info("constraint generation returned type %W", ty, print_term);
  //log_info("...and equation set:\n%W", s, print_equation_set);


  equation_set* sol = NULL;
  check(unify(Delta, &s, &sol), "unification failed");
  //log_info("after unification:\n%W", sol, print_equation_set);
  //log_info("leftover:\n%W", s, print_equation_set);

  term* ans = t;
  apply_solution(sol, ans);
  erase_bad_annotations(ans);
  //log_info("result: %W", ans, print_term);
  free_equation_set(sol);
  return ans;
 error:
  return NULL;
}
