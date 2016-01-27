#ifndef VERNAC_H
#define VERNAC_H

#include "term.h"
#include "telescope.h"

typedef enum command_tag {
  DEF,              /* define a new constant */
  PRINT,            /* print definition of a constant */
  CHECK,            /* print type of term */
  SIMPL,            /* normalize a term */
  DATA,             /* declare a new inductive type */
  RECORD,           /* declare a new record */
  AXIOM,            /* declare a new axiom */
  IMPORT            /* process another file */
} command_tag;

// unless otherwise noted, unused fields are NULL/0
// DEF:
//   var is the name of the constant to be defined
//   left is its type
//   right is its definition
// PRINT: var is the name of the constant to print
// CHECK: left is the term to typecheck
// SIMPL: left is the term to normalize
// DATA:
//   var is the name of the new type
//   args is the list of constructors, of length num_args
//     each constructor is represented as a term with tag VAR
//     where the field var contains the name of the constructor
//     and the field left contains the type (or NULL if no annotation
//     was given, which is only allowed if the constructor represents a constant)
//   param_names is the list of parameter names, of length num_params
//   param_types is the list of parameter types, of length num_params
//   indices is a term representing the indices of the type
//     it is either a Pi returning Type, with one argument for each index,
//     or just Type, if the type has no indices
// RECORD:
//   var is the name of the new type
//   field_names is the list of field names, of length num_fields
//   field_types is the list of field types, of length num_fields
//   num_params, param_names, and param_types are just like the DATA case
// AXIOM:
//   var is the name of the axiom
//   left is its type
// IMPORT: var is the name of the file to import
typedef struct command {
  command_tag tag;
  variable *var;
  term *left;
  term *right;
  int num_args;
  term** args;
  int num_params;
  variable** param_names;
  term** param_types;
  int num_fields;
  variable** field_names;
  term** field_types;

  term* indices;  // pi-type returning Type
} command;

void vernac_init(char* working_directory);
void vernac_run(command *c);

command *make_def(variable *var, term *ty, term *t);
command *make_print(variable *t);
command *make_check(term *t);
command *make_simpl(term *t);
command *make_data(variable* name, int num_constructors, int num_params);
command *make_record(variable* name, int num_fields, int num_params);
command *make_axiom(variable* name, term* ty);
command *make_import(variable* name);

int process_stream(char* filename, FILE* stream);
int process_file(char* filename);

int print_command(FILE* stream, command* c);
void free_command(command* c);

#endif  // VERNAC_H
