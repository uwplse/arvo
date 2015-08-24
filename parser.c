#include "parser.h"
#include "vernac.h"

#include "mpc/mpc.h"

#include <stdlib.h>

static int prefix(char* s, char* t) {
  check(s != NULL && t != NULL, "prefix requires non-NULL arguments");

  while (*s) {
    if (*t) {
      if (*s == *t) {
        s++;
        t++;
      } else {
        return 0;
      }
    } else {
      return 0;
    }
  }
  return 1;
 error:
  return 0;
}

struct parsing_context {
  mpc_result_t result;
  int command_index;
};

static parsing_context* make_parsing_context() {
  parsing_context* ans = malloc(sizeof(parsing_context));
  ans->command_index = 0;
  return ans;
}

void free_parsing_context(parsing_context* pc) {
  mpc_ast_delete(pc->result.output);
  pc->result.output = NULL;
  free(pc);
}


term* ast_to_term(mpc_ast_t* ast) {
  check(ast, "cannot convert NULL to term");

  if (strstr(ast->tag, "var")) {
    return make_var(make_variable(strdup(ast->contents)));
  } else if (strstr(ast->tag, "type")) {
    return make_type();
  } else if (strstr(ast->tag, "hole")) {
    return make_hole();
  } else if (prefix("lambda", ast->tag)) {
    if (ast->children_num == 4) {
      term* ans =  make_lambda(make_variable(strdup(ast->children[1]->contents)),
                               NULL,
                               ast_to_term(ast->children[3]));
      ans->num_args = -1;
      return ans;
    } else {
      check(ast->children_num == 6, "malformed lambda node");
      return make_lambda(make_variable(strdup(ast->children[1]->contents)),
                         ast_to_term(ast->children[3]),
                         ast_to_term(ast->children[5]));
    }
  } else if (prefix("pi", ast->tag)) {
    if (ast->children[0]->contents[0] == '(') {
      check(ast->children_num >= 7, "malformed pi node");
      int n = ast->children_num;
      term* ty = ast_to_term(ast->children[n-4]);
      term* ans = make_pi(make_variable(strdup(ast->children[n-6]->contents)),
                          ty,
                          ast_to_term(ast->children[n-1]));

      int i;
      for (i = n-7; i > 0; i--) {
        ans = make_pi(make_variable(strdup(ast->children[i]->contents)),
                      term_dup(ty),
                      ans);
      }
      return ans;
    } else {
      check(ast->children_num == 3, "malformed pi node");
      return make_pi(variable_dup(&ignore),
                     ast_to_term(ast->children[0]),
                     ast_to_term(ast->children[2]));
    }
  } else if (prefix("app", ast->tag)) {
    check(ast->children_num > 0, "malformed app node");
    term *ans = ast_to_term(ast->children[0]);
    int i;
    for (i = 1; i < ast->children_num; i++) {
      ans = make_app(ans, ast_to_term(ast->children[i]));
    }

    return ans;
  } else if (prefix("base", ast->tag)) {
    check(ast->children_num == 3, "malformed base/parens node");
    return ast_to_term(ast->children[1]);
  } else if (prefix(">", ast->tag)) {
    check(ast->children_num == 3, "malformed root node");
    return ast_to_term(ast->children[1]);
  } else {
    log_err("Unknown tag %s", ast->tag);
  }

 error:
  return NULL;
}

command *ast_to_command(mpc_ast_t *ast) {
  check(ast, "cannot convert NULL to command");
  if (prefix("def", ast->tag)) {
    check(ast->children_num == 7, "malformed def");

    return make_def(make_variable(strdup(ast->children[1]->contents)),
                    ast_to_term(ast->children[3]),
                    ast_to_term(ast->children[5]));
  }
  else if (prefix("axiom", ast->tag)) {
    check(ast->children_num == 5, "malformed def");

    return make_axiom(make_variable(strdup(ast->children[1]->contents)),
                      ast_to_term(ast->children[3]));
  }
  else if (prefix("import", ast->tag)) {
    check(ast->children_num == 3, "malformed import");
    return make_import(make_variable(strdup(ast->children[1]->contents)));
  }
  else if (prefix("print", ast->tag)) {
    check(ast->children_num == 3, "malformed print");

    return make_print(make_variable(strdup(ast->children[1]->contents)));
  }
  else if (prefix("check", ast->tag)) {
    if (ast->children_num == 3) {
      return make_check(ast_to_term(ast->children[1]));
    } else {
      check(ast->children_num == 5, "malformed check");
      command* ans = make_check(ast_to_term(ast->children[1]));
      ans->right = ast_to_term(ast->children[3]);
      return ans;
    }
  }
  else if (prefix("simpl", ast->tag)) {
    check(ast->children_num == 3, "malformed simpl");

    return make_simpl(ast_to_term(ast->children[1]));
  }
  else if (prefix("data", ast->tag)) {
    int i, ic, ip, num_constructors=0, num_params=0;
    command *data;
    for (i=0; i < ast->children_num; i++) {
      if (prefix("constructor", ast->children[i]->tag)) {
        num_constructors++;
      } else if (prefix("param", ast->children[i]->tag)) {
        num_params++;
      }
    }
    data = make_data(make_variable(strdup(ast->children[1]->contents)), num_constructors, num_params);
    ic = 0; ip = 0;
    for (i = 0; i < ast->children_num; i++) {
      if (prefix("constructor", ast->children[i]->tag)) {
        if (ast->children[i]->children_num == 0) {
          check(num_params == 0, "parametrized data types require explicit return types on all constructors");
          data->args[ic++] = make_var(make_variable(strdup(ast->children[i]->contents)));
        }
        else {
          check(ast->children[i]->children_num == 3, "malformed constructor");
          term *c = make_var(make_variable(strdup(ast->children[i]->children[0]->contents)));
          c->left = ast_to_term(ast->children[i]->children[2]);
          data->args[ic++] = c;
        }
      } else if (prefix("param", ast->children[i]->tag)) {
        check(ast->children[i]->children_num == 5, "malformed parameter");
        data->param_names[ip] = make_variable(strdup(ast->children[i]->children[1]->contents));
        data->param_types[ip] = ast_to_term(ast->children[i]->children[3]);
        ip++;
      }
    }
    return data;
  }
 error:
  return NULL;
}

static mpc_parser_t* pComment;
static mpc_parser_t* pVar;
static mpc_parser_t* pHole;
static mpc_parser_t* pBound;
static mpc_parser_t* pLambda;
static mpc_parser_t* pPi;
static mpc_parser_t* pApp;
static mpc_parser_t* pBase;
static mpc_parser_t* pType;
static mpc_parser_t* pTerm;
static mpc_parser_t* pCommand;
static mpc_parser_t* pDef;
static mpc_parser_t* pAxiom;
static mpc_parser_t* pImport;
static mpc_parser_t* pPrint;
static mpc_parser_t* pCheck;
static mpc_parser_t* pSimpl;
static mpc_parser_t* pConstructor;
static mpc_parser_t* pParam;
static mpc_parser_t* pData;
static mpc_parser_t* pProgram;

parsing_context* parse(char* filename) {
  pComment = mpc_new("comment");
  pVar = mpc_new("var");
  pHole = mpc_new("hole");
  pBound = mpc_new("bound");
  pLambda = mpc_new("lambda");
  pPi = mpc_new("pi");
  pApp = mpc_new("app");
  pBase = mpc_new("base");
  pType = mpc_new("type");
  pTerm = mpc_new("term");
  pCommand = mpc_new("command");
  pDef = mpc_new("def");
  pAxiom = mpc_new("axiom");
  pImport = mpc_new("import");
  pPrint = mpc_new("print");
  pCheck = mpc_new("check");
  pSimpl = mpc_new("simpl");
  pConstructor = mpc_new("constructor");
  pParam = mpc_new("param");
  pData = mpc_new("data");
  pProgram = mpc_new("program");

  mpc_err_t* err =
    mpca_lang(MPCA_LANG_DEFAULT,
              " comment : /@[^@]*@/                              ; \n"
              " var     : /[a-zA-Z][a-zA-Z0-9_]*/ ;                \n"
              " hole    : \"\?\" ; \n"
              " bound   : \"_\" | <var> ;                          \n"
              " lambda  : \"\\\\\" <bound> (':' <term>)? '.' <term> ; \n"
              " pi      : '(' <bound>* ':' <term> ')' \"->\" <term> \n"
              "         |  <app> \"->\" <term> ; \n"
              " base    : <type> | <hole> | <var> | '(' <term> ')' ; \n"
              " app     : <base> <base>* ;\n"
              " type    : \"Type\" ;\n"
              " term    : <lambda> | <pi> | <app>;\n"
              " def     : \"def\" <var> ':' <term> \":=\" <term> '.' ;\n"
              " axiom   : \"axiom\" <var> ':' <term> '.' ;\n"
              " import  : \"import\" <var> '.' ;\n"
              " print   : \"print\" <var> '.' ;\n"
              " check   : \"check\" <term> (':' <term>)? '.' ;\n"
              " simpl   : \"simpl\" <term> '.' ;\n"
              " constructor : <var> (':' <term>)? ;\n"
              " param  : '(' <var> ':' <term> ')' ;\n"
              " data    : \"data\" <var> <param>* \":=\" <constructor>? ('|' <constructor>)* '.' ;\n"
              " command : <def> | <print> | <check> | <simpl> | <data> | <axiom> | <import> | <comment>;\n"
              " program  : /^/ <command> * /$/ ;\n",
              pComment, pVar, pHole, pBound, pLambda, pPi, pBase, pApp, pType,
              pTerm,
              pDef, pAxiom, pImport, pPrint, pCheck, pSimpl, pConstructor, pParam, pData, pCommand, pProgram, NULL);

  if (err != NULL) {
    mpc_err_print(err);
    mpc_err_delete(err);
    goto error;
  }

  parsing_context* ans = make_parsing_context();

  if (!mpc_parse_contents(filename, pProgram, &ans->result)) {
    printf ("error: \n");
    mpc_err_print(ans->result.error);
    mpc_err_delete(ans->result.error);
    goto error;
  }
  mpc_cleanup(21, pComment, pVar, pHole, pBound, pLambda, pPi, pApp, pBase, pType,
              pTerm, pCommand, pDef, pAxiom, pImport, pPrint, pCheck, pSimpl,
              pConstructor, pParam, pData, pProgram);
  //mpc_ast_print(ans->result.output);
  ans->command_index = 1;
  return ans;
 error:
  mpc_cleanup(21, pComment, pVar, pHole, pBound, pLambda, pPi, pApp, pBase, pType,
              pTerm, pCommand, pDef, pAxiom, pImport, pPrint, pCheck, pSimpl,
              pConstructor, pParam, pData, pProgram);
  return NULL;
}

command *next_command(parsing_context* pc) {
  mpc_ast_t *ast = pc->result.output;
  while (pc->command_index < ast->children_num && strstr(ast->children[pc->command_index]->tag, "comment")) {
    pc->command_index++;
  }
  if (pc->command_index >= ast->children_num-1) {
    return NULL;
  }
  return ast_to_command(ast->children[pc->command_index++]);
}



