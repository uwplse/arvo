#ifndef PARSER_H
#define PARSER_H

#include "term.h"
#include "vernac.h"

typedef struct parsing_context parsing_context;

void initialize_arvo_parsers();
void cleanup_arvo_parsers();

parsing_context* make_parsing_context(char* filename, FILE* stream);
void free_parsing_context(parsing_context* pc);

command *next_command(parsing_context* pc);

#endif  // PARSER_H
