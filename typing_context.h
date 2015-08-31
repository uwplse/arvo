#ifndef TYPING_CONTEXT_H
#define TYPING_CONTEXT_H

#include "term.h"
#include "telescope.h"

typedef struct {
  variable* name;
  int num_params;
  variable** param_names;
  term** param_types;
  int num_indices;

  term* kind;  // the type of the type constructor. eg, for lists: Type -> Type.
  term* type_constructor;  // the lambda wrapped type constructor. eg, \A : Type. list(A), where list(.) is the raw datatype term
  term* applied_type;  // the fully applied type constructor to the declared params. eg, (list A), where list is just a variable.

  int num_intros;
  term** intros;
  term* elim;
  int *inductive_args;
} datatype;

datatype* make_datatype(variable* name, int num_intros, int num_params, int num_indices);

int datatype_intro_index(variable* needle, datatype* T);

typedef struct typing_context {
  datatype* here;
  struct typing_context* rest;
} typing_context;

typing_context* typing_context_empty();

typing_context* typing_context_add(datatype* t, typing_context* rest);

datatype* typing_context_lookup(variable* var, typing_context* Delta);

datatype* elim_to_datatype(variable* needle, typing_context* Delta);

int print_typing_context(FILE* stream, typing_context* Delta);

int constructor_arg_is_inductive(datatype *T, variable *c, int arg);

#endif  // TYPING_CONTEXT_H
