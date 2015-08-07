#include "typing_context.h"

#include <stdlib.h>

datatype* make_datatype(variable* name, int num_intros, term* elim) {
  datatype* ans = malloc(sizeof(datatype));
  ans->name = name;
  ans->num_intros = num_intros;
  ans->intros = malloc(num_intros * sizeof(term*));
  ans->elim = elim;
  return ans;
}

int datatype_intro_index(variable* needle, datatype* T) {
  check(T, "T must be non-NULL");
  int i;
  for (i = 0; i < T->num_intros; i++) {
    if (variable_equal(T->intros[i]->var, needle)) {
      return i;
    }
  }
  sentinel("T has no constructor named %s", needle->name);
 error:
  return -1;
}

typing_context* typing_context_empty() {
  return NULL;
}

typing_context* typing_context_add(datatype* t, typing_context* rest) {
  typing_context* ans = malloc(sizeof(typing_context));
  ans->here = t;
  ans->rest = rest;
  return ans;
}

datatype* typing_context_lookup(variable* var, typing_context* Delta) {
  while (Delta) {
    if (variable_equal(Delta->here->name, var)) {
      return Delta->here;
    }
    Delta = Delta->rest;
  }
  return NULL;
}

int print_typing_context(FILE* stream, typing_context* Delta) {
  if (Delta == NULL) {
    return fprintf(stream, ".");
  }

  int len =print_typing_context(stream, Delta->rest);
  len += fprintf(stream, "data %s :=");
  int i;
  datatype* t = Delta->here;
  if (t->num_intros > 0) {
    len += fprintf(stream, "%s", t->intros[0]->var->name);
  }
  for (i = 1; i < Delta->here->num_intros; i++) {
    len += fprintf(stream, "| %s", t->intros[i]->var->name);
  }
  len += fprintf(stream, ".");
  return len;
}

datatype* elim_to_datatype(variable* needle, typing_context* Delta) {
  while (Delta) {
    if (variable_equal(Delta->here->elim->var, needle)) {
      return Delta->here;
    }
    Delta = Delta->rest;
  }
  return NULL;
}
