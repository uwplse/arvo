#ifndef PARSER_H
#define PARSER_H

#include "term.h"
#include "vernac.h"

int parse(char* filename);

command *next_command(void);
void free_ast(void);

#endif  // PARSER_H
