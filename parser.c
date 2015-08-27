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
  char* filename;
  FILE* stream;
  int expect_sep;
};

parsing_context* make_parsing_context(char* filename, FILE* stream) {
  parsing_context* ans = malloc(sizeof(parsing_context));
  ans->filename = filename;
  ans->stream = stream;
  ans->expect_sep = 0;
  return ans;
}

void free_parsing_context(parsing_context* pc) {
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
  } else if (prefix("factor", ast->tag)) {
    check(ast->children_num != 0, "malformed factor: zero children");
    if (ast->children_num == 1) {
      return ast_to_term(ast->children[0]);
    }
    check(ast->children_num % 2 == 1, "malformed factor: even number of children");
    term* ans = ast_to_term(ast->children[ast->children_num - 1]);
    int i;
    for (i = ast->children_num - 3; i >= 0; i -= 2) {
      mpc_ast_t* child = ast->children[i];
      if (prefix("expr", child->tag) && child->children_num >= 5) {
        check(child->children[child->children_num - 3]->contents[0] == ':', "malformed pi");
        term* ty = ast_to_term(child->children[child->children_num - 2]);
        int j;
        for (j = child->children_num - 4; j > 0; j--) {
          ans = make_pi(make_variable(strdup(child->children[j]->contents)),
                        term_dup(ty),
                        ans);
        }
        free_term(ty); ty = NULL;
      } else if ((prefix("expr", child->tag) && child->children_num == 0) || prefix("app", child->tag) || prefix("base", child->tag)) {
        ans = make_pi(variable_dup(&ignore),
                      ast_to_term(child),
                      ans);
      } else {
        mpc_ast_print_to(child, stderr);
        sentinel("malformed factor: unknown child tag %s with %d children", child->tag, child->children_num);
      }
    }
    return ans;
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
    check(ast->children_num == 6, "malformed def");

    return make_def(make_variable(strdup(ast->children[1]->contents)),
                    ast_to_term(ast->children[3]),
                    ast_to_term(ast->children[5]));
  }
  else if (prefix("axiom", ast->tag)) {
    check(ast->children_num == 4, "malformed def");

    return make_axiom(make_variable(strdup(ast->children[1]->contents)),
                      ast_to_term(ast->children[3]));
  }
  else if (prefix("import", ast->tag)) {
    check(ast->children_num == 2, "malformed import");
    return make_import(make_variable(strdup(ast->children[1]->contents)));
  }
  else if (prefix("print", ast->tag)) {
    check(ast->children_num == 2, "malformed print");

    return make_print(make_variable(strdup(ast->children[1]->contents)));
  }
  else if (prefix("check", ast->tag)) {
    if (ast->children_num == 2) {
      return make_check(ast_to_term(ast->children[1]));
    } else {
      check(ast->children_num == 4, "malformed check");
      command* ans = make_check(ast_to_term(ast->children[1]));
      ans->right = ast_to_term(ast->children[3]);
      return ans;
    }
  }
  else if (prefix("simpl", ast->tag)) {
    check(ast->children_num == 2, "malformed simpl");

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
  } else {
    sentinel("unknown tag %s", ast->tag);
  }
 error:
  return NULL;
}

static mpc_parser_t* pComment;
static mpc_parser_t* pVar;
static mpc_parser_t* pHole;
static mpc_parser_t* pBound;
static mpc_parser_t* pType;
static mpc_parser_t* pBase;
static mpc_parser_t* pApp;
static mpc_parser_t* pExpr;
static mpc_parser_t* pFactor;
static mpc_parser_t* pLambda;
static mpc_parser_t* pTerm;
static mpc_parser_t* pDef;
static mpc_parser_t* pAxiom;
static mpc_parser_t* pImport;
static mpc_parser_t* pPrint;
static mpc_parser_t* pCheck;
static mpc_parser_t* pSimpl;
static mpc_parser_t* pConstructor;
static mpc_parser_t* pParam;
static mpc_parser_t* pData;
static mpc_parser_t* pCommand;
static mpc_parser_t* pSep;
static mpc_parser_t* pProgram;
static int initialized = 0;

void initialize_arvo_parsers() {
  if (initialized) return;
  initialized = 1;
  pComment     = mpc_new("comment");
  pVar         = mpc_new("var");
  pHole        = mpc_new("hole");
  pBound       = mpc_new("bound");
  pType        = mpc_new("type");
  pBase        = mpc_new("base");
  pApp         = mpc_new("app");
  pExpr        = mpc_new("expr");
  pFactor      = mpc_new("factor");
  pLambda      = mpc_new("lambda");
  pTerm        = mpc_new("term");
  pDef         = mpc_new("def");
  pAxiom       = mpc_new("axiom");
  pImport      = mpc_new("import");
  pPrint       = mpc_new("print");
  pCheck       = mpc_new("check");
  pSimpl       = mpc_new("simpl");
  pConstructor = mpc_new("constructor");
  pParam       = mpc_new("param");
  pData        = mpc_new("data");
  pCommand     = mpc_new("command");
  pProgram     = mpc_new("program");
  pSep         = mpc_new("sep");

#define PARSERS pComment, pVar, pHole, pBound, pType, pBase, pApp, pExpr, pFactor, \
                pLambda, pTerm, pDef, pAxiom, pImport, pPrint, pCheck, pSimpl, \
                pConstructor, pParam, pData, pCommand, pProgram, pSep

  mpc_err_t* err =
    mpca_lang(MPCA_LANG_DEFAULT,
              " comment : /@[^@]*@/                              ; \n"
              " var     : /[a-zA-Z][a-zA-Z0-9_]*/ ;                \n"
              " hole    : \"\?\" ; \n"
              " bound   : \"_\" | <var> ;                          \n"
              " type    : \"Type\" ;\n"
              " base    : <type> | <hole> | <var> | '(' <term> ')' ; \n"
              " app     : <base> <base>* ;\n"
              " expr    : '(' <bound>* ':' <term> ')' \n"
              "         | <app> ; \n "
              " factor  : <expr> ( \"->\" <expr> )* ; \n"
              " lambda  : \"\\\\\" <bound> (':' <term>)? '.' <term> ; \n"
              " term    : <lambda> | <factor> ;\n"
              " def     : \"def\" <var> ':' <term> \":=\" <term> ;\n"
              " axiom   : \"axiom\" <var> ':' <term> ;\n"
              " import  : \"import\" <var>  ;\n"
              " print   : \"print\" <var>  ;\n"
              " check   : \"check\" <term> (':' <term>)?  ;\n"
              " simpl   : \"simpl\" <term>  ;\n"
              " constructor : <var> (':' <term>)? ;\n"
              " param  : '(' <var> ':' <term> ')' ;\n"
              " data    : \"data\" <var> <param>* \":=\" <constructor>? ('|' <constructor>)*  ;\n"
              " command : /^/ (<def> | <print> | <check> | <simpl> | <data> | <axiom> | <import> | <comment>) ;\n"
              " sep     : '.' ; \n"
              " program  : /^/ <command> * /$/ ;\n",
              PARSERS, NULL);

  if (err != NULL) {
    mpc_err_print(err);
    mpc_err_delete(err);
    exit(1);
  }
}

void cleanup_arvo_parsers() {
  if (!initialized) return;
  initialized = 0;
  mpc_cleanup(22, PARSERS);
}


command *next_command(parsing_context* pc) {
  check(initialized, "parsers should be initialized first!");

  mpc_ast_t* ast = NULL;

  if (pc->expect_sep) {
    pc->expect_sep = 0;
    if (!mpc_parse_pipe(pc->filename, pc->stream, pSep, &pc->result)) {
      printf ("error: \n");
      mpc_err_print(pc->result.error);
      mpc_err_delete(pc->result.error);
      return NULL;
    }
  }

  if (feof(pc->stream)) return NULL;

  do {
    if (!mpc_parse_pipe(pc->filename, pc->stream, pCommand, &pc->result)) {
      printf ("error: \n");
      mpc_err_print(pc->result.error);
      mpc_err_delete(pc->result.error);
      return NULL;
    }
    ast = (mpc_ast_t *)(pc->result.output);
  } while (strstr(ast->children[1]->tag, "comment"));

  pc->expect_sep = 1;

  check(strcmp(ast->tag, ">") == 0 && ast->children_num == 2, "malformed command node with tag %s and %d children", ast->tag, ast->children_num);

  //mpc_ast_print(ast);

  return ast_to_command(ast->children[1]);
 error:
  return NULL;
}
