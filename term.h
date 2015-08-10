#ifndef TERM_H
#define TERM_H

#include "dbg.h"

// based on pi-forall: https://github.com/sweirich/pi-forall
/*
 * e ::= x
 *     | \ x : e . e
 *     | (x : e) -> e
 *     | e e 
 *     | Type
 *     | nat
 *     | nat-ind
 *     | O
 *     | S
 */

typedef enum {
  VAR,
  LAM,
  PI, 
  APP,
  TYPE,
  NAT,
  NAT_IND,
  O,
  S,
  INTRO,
  ELIM,
  DATATYPE
} term_tag;



const char* term_tag_to_string(term_tag tag);

typedef struct {
  char* name;
} variable;

extern variable ignore;

typedef struct term {
  term_tag tag;
  variable* var;       // Valid for VAR, LAM, PI, otherwise NULL
  struct term* left;   // Valid for LAM, PI, APP, otherwise NULL
  struct term* right;  // Valid for LAM, PI, APP, otherwise NULL

  int num_args;        // Valid for NAT_IND only
  struct term** args;  // Valid for NAT_IND only
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
term* make_nat();
term* make_nat_ind(term* motive, term* Z, term* S, term* n);
term* make_o();
term* make_s();

term* make_intro(variable* name, int num_args);
term* make_elim(variable* name, int num_args);
term* make_datatype_term(variable* name);

void free_variable(variable* v);

void free_term(term* t);

int syntactically_identical(term* a, term* b);

term* term_dup(term* t);
variable* variable_dup(variable* v);

term* substitute(variable* from, term* to, term* haystack);

#endif  // TERM_H
