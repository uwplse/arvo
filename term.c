#include "term.h"
#include "printing.h"
#include <stdlib.h>

variable ignore = { .name = "_"};

int print_variable(FILE* stream, variable* v) {
  return fprintf(stream, "%s", v->name);
}

int print_term(FILE* stream, term* t) {
  if (t == NULL) return fprintf(stream, "NULL");

  switch (t->tag) {
  case VAR:
    return print_variable(stream, t->var);
  case LAM:
    return fprintf(stream, "\\%W : %W. %W", t->var, print_variable, t->left, print_term, t->right, print_term);
  case PI:
    if (variable_equal(t->var, &ignore)) {
      return fprintf(stream, "%W -> %W", t->left, print_term, t->right, print_term);
    } else {
      return fprintf(stream, "(%W : %W) -> %W", t->var, print_variable, t->left, print_term, t->right, print_term);
    }
  case APP:
    return fprintf(stream, "(%W %W)", t->left, print_term, t->right, print_term);
  case TYPE:
    return fprintf(stream, "Type");
  case NAT:
    return fprintf(stream, "nat");
  case NAT_IND:
    return fprintf(stream, "nat-ind(%W; %W; %W; %W)", t->args[0], print_term, t->args[1], print_term, t->args[2], print_term, t->args[3], print_term);
  case O:
    return fprintf(stream, "O");
  case S:
    return fprintf(stream, "S");
  default:
    sentinel("Bad tag %d", t->tag);
  }
    
 error:
  return -1;
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

term* make_nat() {
  term* ans = make_term();
  ans->tag = NAT;

  return ans;
}

term* make_nat_ind(term* motive, term* Z, term* S, term* n) {
  term* ans = make_term();
  ans->tag = NAT_IND;
  ans->num_args = 4;
  ans->args = malloc(4 * sizeof(term*));
  ans->args[0] = motive;
  ans->args[1] = Z;
  ans->args[2] = S;
  ans->args[3] = n;

  return ans;
}

term* make_o() {
  term* ans = make_term();
  ans->tag = O;

  return ans;
}

term* make_s() {
  term* ans = make_term();
  ans->tag = S;

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
  return (x == NULL && y == NULL) || (strcmp(x->name, y->name) == 0);
}

int syntactically_identical(term* a, term* b) {
  check(a != NULL && b != NULL, "alpha_equiv requires non-NULL arguments");
  check(term_locally_well_formed(a) && term_locally_well_formed(b), 
        "alpha equiv requires well-formed arguments");

  if (a->tag != b-> tag) return 0;

  switch (a->tag) {
  case VAR:
    return variable_equal(a->var, b->var);
  case LAM:
    {
      if (!syntactically_identical(a->left, b->left))
        return 0;
      if (variable_equal(a->var, b->var))
        return syntactically_identical(a->left, b->left) &&
          syntactically_identical(a->right, b->right);

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
        return syntactically_identical(a->left, b->left) &&
          syntactically_identical(a->right, b->right);


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
  case TYPE:
  case NAT:
  case NAT_IND:
  case O:
  case S:
    return 1;
  default:
    sentinel("malformed term");
  }
  
 error:
  return 0;
}

static int counter = 0;
variable *gensym(variable *var) {
  char *new;
  asprintf(&new, "_%s%d", var->name, counter++);
  return make_variable(new);
}

int is_free(variable *var, term *haystack) {
  switch(haystack->tag) {
  case VAR:
    if (variable_equal(var, haystack->var)) {
      return 1;
    }
    return 0;
  case LAM:
  case PI:
    if (variable_equal(var, haystack->var)) {
      return 0;
    }
  case APP:
    return is_free(var, haystack->left) || is_free(var, haystack->right);
  case TYPE:
  case NAT:
  case NAT_IND:
  case O:
  case S:
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
  check(from != NULL && to != NULL && haystack != NULL, "substitute requires non-NULL arguments");
  check(term_locally_well_formed(to) && term_locally_well_formed(haystack),
        "substitute requires locally well-formed arguments");

  switch(haystack->tag) {
  case VAR:
    if (variable_equal(from, haystack->var)) {
      return term_dup(to);
    } else {
      return term_dup(haystack);
    }
  case LAM:
    if (variable_equal(from, haystack->var)) {
      return make_lambda(variable_dup(haystack->var),
                         substitute(from, to, haystack->left),
                         term_dup(haystack->right));
    } else {
      if (is_free(haystack->var, to)) {
        variable *g = gensym(haystack->var);
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
        variable *g = gensym(haystack->var);
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
  case NAT_IND:
    return make_nat_ind(substitute(from, to, haystack->args[0]),
                        substitute(from, to, haystack->args[1]),
                        substitute(from, to, haystack->args[2]),
                        substitute(from, to, haystack->args[3]));
  case TYPE:
  case NAT:
  case O:
  case S:
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
  case PI:
    return (t->var != NULL) && (t->left != NULL) && (t->right != NULL);
  case APP:
    return (t->left != NULL) && (t->right != NULL);
  case TYPE:
  case NAT:
  case NAT_IND:
  case O:
  case S:
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

  int i;
  for (i = 0; i < t->num_args; i++) {
    free_term(t->args[i]);
    t->args[i] = NULL;
  }
  free(t->args);
  t->args = NULL;

  free(t);
}

static const char* term_tag_names[S+1] = {"VAR", "LAM", "PI", "APP", "TYPE", "NAT", "NAT_IND" "O", "S"};

const char* term_tag_to_string(term_tag tag) {
  check(0 <= tag && tag <= TYPE, "Bad tag %d", tag);
  return term_tag_names[tag];
 error:
  return NULL;
}

variable* variable_dup(variable* v) {
  if (v == NULL) return NULL;

  return make_variable(strdup(v->name));
}

term* term_dup(term* t) {
  if (t == NULL) return NULL;

  term* ans = make_term();
  ans->tag = t->tag;
  ans->var = variable_dup(t->var);
  ans->left = term_dup(t->left);
  ans->right = term_dup(t->right);
  ans->num_args = t->num_args;
  ans->args = NULL;
  if (ans->num_args > 0) {
    ans->args = malloc((ans->num_args) * sizeof(struct term*));
    int i;
    for (i = 0; i < ans->num_args; i++) {
      ans->args[i] = term_dup(t->args[i]);
    }
  }

  return ans;
}

term* make_intro(variable* name) {
  term* ans = make_term();
  ans->tag = INTRO;
  ans->var = name;
  return ans;
}

term* make_elim(variable* name, int num_args) {
  term* ans = make_term();
  ans->tag = ELIM;
  ans->var = name;
  ans->num_args = num_args;
  ans->args = malloc(num_args * sizeof(term*));
  return ans;
}

term* make_datatype_term(variable* name) {
  term* ans = make_term();
  ans->tag = DATATYPE;
  ans->var = name;
  return ans;
}
