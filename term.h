#ifndef TERM_H
#define TERM_H

#include "dbg.h"

// loosely based on pi-forall: https://github.com/sweirich/pi-forall

typedef enum {
  VAR,                          /* variables or names of constants */
  LAM,
  PI,
  APP,
  TYPE,                         /* the universe */
  INTRO,                        /* constructors */
  ELIM,                         /* eliminators */
  DATATYPE,                     /* user-defined types */
  HOLE,                         /* hole to be filled by user later; for interactive use */
  IMPLICIT                      /* to be filled by unification; currently unused/disabled */
} term_tag;

// constructors, eliminators, and user-defined types do not appear in the concrete syntax,
// instead, they are all parsed as constants, which are then looked up and replaced
// during evaluation by their "raw" forms.
// raw forms can be recognized in printed terms because of extra pairs of parens,
// eg after declaring a type nat, "simpl nat." prints "nat ==> nat()()", meaning
// that the variable nat reduces to the raw term nat()() (of tag DATATYPE)

typedef struct {
  char* name;
} variable;

extern variable ignore;

// terms are represented by a very hacky struct whose
// fields have different interpretations depending on the tag.
// unless otherwise noted, unused fields are NULL/0
// VAR:
//   var is the name of the referenced variable or constant.
//   note: this tag is also used to represent constructors,
//     in which case left contains a possibly NULL type.
//     such a term will never be encountered in the wild, but only
//     as a child of a data command.
// LAM:
//   var is the bound variable;
//   left is the type annotation;
//   right is the body of the lambda.
// PI:
//   var is the bound variable;
//   left is the type annotation;
//   right is the body of the pi.
// APP: left applied to right.
// TYPE: all fields unused.
// INTRO:
//   var is the name of the constructor;
//   left is its type;
//   args is the list of arguments (of length num_args);
//   params is the list of parameters (of length num_params);
//   indices is the list of indices (of length indices).
// ELIM:
//   var is the name of the eliminator;
//   args is the list of arguments, of length num_args,
//     which is equal to the number of constructors for the type, plus 2 extras:
//     one for the motive (first argument)
//     and one for the discriminee (last argument);
//   params is the list of parameters (of length num_params);
//   indices is the list of indices (of length num_indices).
// DATATYPE:
//   var is the name of the type;
//   params is the list of parameters (of length num_params)
//   indices is the list of indices (of length num_indices)
typedef struct term {
  term_tag tag;
  variable* var;
  struct term* left;
  struct term* right;

  int num_args;
  struct term** args;

  int num_params;
  struct term** params;

  int num_indices;
  struct term** indices;
} term;

int term_locally_well_formed(term* t);
int variable_equal(variable* x, variable* y);

int print_term(FILE* stream, term* t);

int print_term_and_free(FILE* stream, term* t);

int print_variable(FILE* stream, variable* v);

/* caller gives up ownership of c */
variable* make_variable(char *c);

variable *gensym(char *name);

term* make_pi(variable* x, term* A, term* B);
term* make_lambda(variable* x, term* A, term* B);
term* make_app(term* a, term* b);
term* make_var(variable* var);

term* make_type();
term* make_hole();
term* make_implicit();

term* make_intro(variable* name, term *type, int num_args, int num_params, int num_indices);
term* make_elim(variable* name, int num_args, int num_params, int num_indices);
term* make_datatype_term(variable* name, int num_params, int num_indices);

void free_variable(variable* v);

void free_term(term* t);

int syntactically_identical(term* a, term* b);

term* term_dup(term* t);
variable* variable_dup(variable* v);

term* substitute(variable* from, term* to, term* haystack);

variable* fresh(char* prefix);

int is_free(variable *var, term *haystack);

int has_holes(term* t);

#endif  // TERM_H
