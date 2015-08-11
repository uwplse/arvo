#ifndef VERNAC_H
#define VERNAC_H

#include "term.h"
#include "telescope.h"

typedef enum command_tag {
  DEF,
  PRINT,
  CHECK,
  SIMPL,
  DATA,
  AXIOM,
  IMPORT
} command_tag;

typedef struct command {
  command_tag tag;
  variable *var;
  term *left;
  term *right;
  int num_args;
  term** args;
} command;

void vernac_init(char* working_directory);
void vernac_run(command *c);

command *make_def(variable *var, term *ty, term *t);
command *make_print(variable *t);
command *make_check(term *t);
command *make_simpl(term *t);
command *make_data(variable* name, int num_constructors);
command *make_axiom(variable* name, term* ty);
command *make_import(variable* name);

int process_file(char* filename);

int print_command(FILE* stream, command* c);
void free_command(command* c);

#endif  // VERNAC_H
