#include "term.h"
#include "printing.h"
#include <stdlib.h>

variable ignore = { .name = "_"};

int print_variable(FILE* stream, variable* v) {
  if (v == NULL) {
    return fprintf(stream, "NULL");
  }
  return fprintf(stream, "%s", v->name);
}

extern char* C_prettyprint(term* t);

int print_term(FILE* stream, term* t) {
  char* s = C_prettyprint(t);
  int ans = fprintf(stream, "%s", s);
  free(s);
  return ans;
}

int print_term_and_free(FILE* stream, term* t) {
  int ans = print_term(stream, t);
  free_term(t);
  return ans;
}

static term* make_term() {
  term* ans = malloc(sizeof(term));
  ans->var = NULL;
  ans->left = NULL;
  ans->right = NULL;
  ans->num_args = 0;
  ans->args = NULL;
  ans->num_params = 0;
  ans->params = NULL;

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

term* make_hole() {
  term* ans = make_term();
  ans->tag = HOLE;

  return ans;
}

term* make_implicit(variable* name) {
  term* ans = make_term();
  ans->tag = IMPLICIT;
  ans->right = make_var(name);

  return ans;
}

term* make_lambda(variable* var, term* A, term* b) {
  term* ans = make_term();
  ans->tag = LAM;
  ans->var = var;
  ans->left = A;
  ans->right = b;

  return ans;
}

term* make_app(term* a, term* b) {
  term* ans = make_term();
  ans->tag = APP;
  ans->left = a;
  ans->right = b;
  
  return ans;
}

variable *make_variable(char *c) {
  variable *ans = malloc(sizeof(variable));
  ans->name = c;
  return ans;
}

term *make_var(variable *var) {
  term *ans = make_term();
  ans->tag = VAR;
  ans->var = var;
  return ans;
}

int variable_equal(variable* x, variable* y) {
  return (x == NULL && y == NULL) || (x != NULL && y != NULL && strcmp(x->name, y->name) == 0);
}

int syntactically_identical(term* a, term* b) {
  if (a == NULL || b == NULL) return a == b;
  check(term_locally_well_formed(a) && term_locally_well_formed(b), 
        "alpha equiv requires well-formed arguments");

  if (a->tag == HOLE) {
    log_info("Hole should unify with %W", b, print_term);
    return 1;
  }

  if (b->tag == HOLE) {
    log_info("Hole should unify with %W", a, print_term);
    return 1;
  }

  if (a->tag != b-> tag) return 0;

  switch (a->tag) {
  case VAR:
    return variable_equal(a->var, b->var);
  case LAM:
    {
      if (a->left != NULL && b->left != NULL && !syntactically_identical(a->left, b->left))
        return 0;
      if (variable_equal(a->var, b->var))
        return syntactically_identical(a->right, b->right);

      term* va = make_var(variable_dup(a->var));
      term* bsubs = substitute(b->var, va, b->right);
      free_term(va);

      term* c = make_lambda(variable_dup(a->var), term_dup(b->left), bsubs);
      int ans = syntactically_identical(a, c);
      free_term(c);
      return ans;

    }
  case PI:
    {
      if (!syntactically_identical(a->left, b->left))
        return 0;
      if (variable_equal(a->var, b->var))
        return syntactically_identical(a->right, b->right);


      term* va = make_var(variable_dup(a->var));
      term* bsubs = substitute(b->var, va, b->right);
      free_term(va);

      term* c = make_pi(variable_dup(a->var), term_dup(b->left), bsubs);
      int ans = syntactically_identical(a, c);
      free_term(c);
      return ans;
    }
  case APP:
    return 
      syntactically_identical(a->left, b->left) &&
      syntactically_identical(a->right, b->right);
  case DATATYPE:
    {
      if (!variable_equal(a->var, b->var)) {
        return 0;
      }
#define EQ_VEC(a, an, b, bn) do {                                       \
        if (an != bn) return 0;                                         \
        int __i;                                                        \
        for (__i = 0; __i < an; __i++) {                                \
          if (!syntactically_identical(a[__i], b[__i])) return 0;       \
        }                                                               \
      } while(0)

      EQ_VEC(a->params, a->num_params, b->params, b->num_params);

      return 1;
    }
  case INTRO:
  case ELIM:
    {
      if (!variable_equal(a->var, b->var)) {
        return 0;
      }

      EQ_VEC(a->args, a->num_args, b->args, b->num_args);
      EQ_VEC(a->params, a->num_params, b->params, b->num_params);
      EQ_VEC(a->indices, a->num_indices, b->indices, b->num_indices);
      return 1;
    }
  case TYPE:
    return 1;
  case IMPLICIT:
    return syntactically_identical(a->right, b->right);
  default:
    sentinel("malformed term");
  }
  
 error:
  return 0;
}

int has_holes(term* t) {
  if (t == NULL) return 0;
  if (t->tag == HOLE) return 1;

  if (has_holes(t->left)) return 1;
  if (has_holes(t->right)) return 1;

#define HAS_HOLES_VEC(n, a) do {                \
    int __i;                                    \
    for (__i = 0; __i < n; __i++) {             \
      if (has_holes(a[__i])) return 1;          \
    }                                           \
  } while (0)

  HAS_HOLES_VEC(t->num_args, t->args);
  HAS_HOLES_VEC(t->num_params, t->params);

  return 0;
}

static int counter = 0;
variable *gensym(char *name) {
  char *new;
  asprintf(&new, "_%s%d", name, counter++);
  return make_variable(new);
}

int is_free(variable *var, term *haystack) {
  if (haystack == NULL) return 0;

  switch(haystack->tag) {
  case VAR:
    if (variable_equal(var, haystack->var)) {
      return 1;
    }
    return 0;
  case HOLE:
    return 0;
  case IMPLICIT:
    return 0;
  case LAM:
  case PI:
    if (variable_equal(var, haystack->var)) {
      return 0;
    }
    // fall-through
  case APP:
    return is_free(var, haystack->left) || is_free(var, haystack->right);
  case DATATYPE:
    {
      if (variable_equal(var, haystack->var)) {
        return 1;
      }
#define IS_FREE_VEC(n, a) do {                  \
        int __i;                                \
        for (__i = 0; __i < n; __i++) {         \
          if (is_free(var, a[__i])) return 1;   \
        }                                       \
      } while (0)

      IS_FREE_VEC(haystack->num_params, haystack->params);
      return 0;
    }
  case INTRO:
  case ELIM:
    {
      if (variable_equal(var, haystack->var)) {
        return 1;
      }
      IS_FREE_VEC(haystack->num_args, haystack->args);
      IS_FREE_VEC(haystack->num_params, haystack->params);
      return 0;
    }
  case TYPE:
    return 0;

  default:
    sentinel("bad term");
  }
 error:
  return 0;
}

/*
  invariant: no sharing between returned term and *any* arguments.
  the caller must free the result.
 */
term* substitute(variable* from, term* to, term* haystack) {
  if (haystack == NULL) return NULL;

  check(from != NULL && to != NULL, "substitute requires non-NULL arguments");
  check(term_locally_well_formed(to), "substitute requires %W to be locally well-formed", to, print_term);
  check(term_locally_well_formed(haystack),"substitute requires %W to be locally well-formed", haystack, print_term);



  switch(haystack->tag) {
  case VAR:
    if (variable_equal(from, haystack->var)) {
      return term_dup(to);
    } else {
      return term_dup(haystack);
    }
  case HOLE:
    return term_dup(haystack);
  case LAM:
    if (variable_equal(from, haystack->var)) {
      return make_lambda(variable_dup(haystack->var),
                         substitute(from, to, haystack->left),
                         term_dup(haystack->right));
    } else {
      if (is_free(haystack->var, to)) {
        variable *g = gensym(haystack->var->name);
        term *tg = make_var(g);
        term* new_haystack = make_lambda(variable_dup(g), term_dup(haystack->left),
                                         substitute(haystack->var, tg, haystack->right));
        free_term(tg);
        term* ans = substitute(from, to, new_haystack);
        free_term(new_haystack);
        return ans;
      }
      return make_lambda(variable_dup(haystack->var),
                         substitute(from, to, haystack->left),
                         substitute(from, to, haystack->right));
    }
  case PI:
    if (variable_equal(from, haystack->var)) {
      return make_pi(variable_dup(haystack->var),
                     substitute(from, to, haystack->left),
                     term_dup(haystack->right));
    } else {
      if (is_free(haystack->var, to)) {
        variable *g = gensym(haystack->var->name);
        term *tg = make_var(g);
        term* new_haystack = make_pi(variable_dup(g), term_dup(haystack->left),
                                     substitute(haystack->var, tg, haystack->right));
        free_term(tg);
        term* ans = substitute(from, to, new_haystack);
        free_term(new_haystack);
        return ans;
      }
      return make_pi(variable_dup(haystack->var),
                     substitute(from, to, haystack->left),
                     substitute(from, to, haystack->right));
    }
  case APP:
    return make_app(substitute(from, to, haystack->left),
                    substitute(from, to, haystack->right));
  case TYPE:
    return term_dup(haystack);
  case DATATYPE:
    {
      term* ans = make_datatype_term(variable_dup(haystack->var),
                                     haystack->num_params);
#define SUB_VEC(dst, src, n) do {                       \
        int __i;                                        \
        for (__i = 0; __i < n; __i++) {                 \
          dst[__i] = substitute(from, to, src[__i]);     \
        }                                               \
      } while(0)

      SUB_VEC(ans->params, haystack->params, haystack->num_params);

      return ans;
    }

  case INTRO:
    {
      term* ans = make_intro(variable_dup(haystack->var), term_dup(haystack->left), haystack->num_args, haystack->num_params, haystack->num_indices);

      SUB_VEC(ans->args, haystack->args, haystack->num_args);
      SUB_VEC(ans->params, haystack->params, haystack->num_params);
      return ans;
    }
  case ELIM:
    {
      term* ans = make_elim(variable_dup(haystack->var), haystack->num_args, haystack->num_params, haystack->num_indices);

      SUB_VEC(ans->args, haystack->args, haystack->num_args);
      SUB_VEC(ans->params, haystack->params, haystack->num_params);
      return ans;
    }
  case IMPLICIT:
    return term_dup(haystack);
  default:
    sentinel("malformed term with tag %d", haystack->tag);
  }

 error:
  return NULL;
}

int term_locally_well_formed(term* t) {
  if (t == NULL) return 1;

  switch (t->tag) {
  case VAR:
    return (t->var != NULL);
  case LAM:
    return (t->var != NULL) && (t->right != NULL);
  case PI:
    return (t->var != NULL) && (t->left != NULL) && (t->right != NULL);
  case APP:
    return (t->left != NULL) && (t->right != NULL);
  case TYPE:
  case INTRO:
  case ELIM:
  case DATATYPE:
  case HOLE:
  case IMPLICIT:
    return 1;

  default:
    return 0;

  }
}


void free_variable(variable* v) {
  if (v == NULL) return;

  free(v->name);
  v->name = NULL;
  free(v);
}

void free_term(term* t) {
  if (t == NULL) return;

  free_variable(t->var);
  t->var = NULL;

  free_term(t->left);
  t->left = NULL;

  free_term(t->right);
  t->right = NULL;

#define FREE_VEC(n, a) do {                     \
    int __i;                                    \
    for (__i = 0; __i < n; __i++) {             \
      free_term(a[__i]);                        \
      a[__i] = NULL;                            \
    }                                           \
    free(a);                                    \
    a = NULL;                                   \
  } while (0)

  FREE_VEC(t->num_args, t->args);
  FREE_VEC(t->num_params, t->params);

  free(t);
}

variable* variable_dup(variable* v) {
  if (v == NULL) return NULL;

  return make_variable(strdup(v->name));
}

#define DUP_VEC(dstn, dst, srcn, src, dup_f, ty) do {   \
    dstn = srcn;                                        \
    dst = NULL;                                         \
    if (dstn > 0) {                                     \
      dst = malloc((dstn) * sizeof(ty));                \
      int __i;                                          \
      for (__i = 0; __i < dstn; __i++) {                \
        dst[__i] = dup_f(src[__i]);                     \
      }                                                 \
    }                                                   \
  } while (0)

term* term_dup(term* t) {
  if (t == NULL) return NULL;

  term* ans = make_term();
  ans->tag = t->tag;
  ans->var = variable_dup(t->var);
  ans->left = term_dup(t->left);
  ans->right = term_dup(t->right);

  DUP_VEC(ans->num_args, ans->args,
          t->num_args, t->args,
          term_dup, struct term*);

  DUP_VEC(ans->num_params, ans->params,
          t->num_params, t->params,
          term_dup, struct term*);


  return ans;
}

term* make_intro(variable* name, term *type, int num_args, int num_params) {
  term* ans = make_term();
  ans->tag = INTRO;
  ans->var = name;
  ans->left = type;
  ans->num_args = num_args;
  ans->args = calloc(num_args, sizeof(term*));
  ans->num_params = num_params;
  ans->params = calloc(num_params, sizeof(term*));
  return ans;
}

term* make_elim(variable* name, int num_args, int num_params) {
  term* ans = make_term();
  ans->tag = ELIM;
  ans->var = name;
  ans->num_args = num_args;
  ans->args = calloc(num_args, sizeof(term*));
  ans->num_params = num_params;
  ans->params = calloc(num_params, sizeof(term*));
  return ans;
}

term* make_datatype_term(variable* name, int num_params) {
  term* ans = make_term();
  ans->tag = DATATYPE;
  ans->var = name;
  ans->num_params = num_params;
  ans->params = calloc(num_params, sizeof(term*));
  return ans;
}

static int _fresh = 0;

variable* fresh(char* prefix) {
  char* meta;
  asprintf(&meta, "_%s_%d", prefix, _fresh++);
  return make_variable(meta);
}

