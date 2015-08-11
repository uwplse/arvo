#ifndef PARSER_H
#define PARSER_H

#include "term.h"
#include "vernac.h"

typedef struct parsing_context parsing_context;

parsing_context* parse(char* filename);

command *next_command(parsing_context* pc);
void free_parsing_context(parsing_context* pc);

#endif  // PARSER_H
