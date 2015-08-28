#include "vernac.h"
#include "telescope.h"
#include "context.h"
#include "typing_context.h"
#include "typecheck.h"
#include "term.h"
#include "normalize.h"
#include "parser.h"
#include "printing.h"

#include <stdlib.h>

static telescope *Gamma;
static context *Sigma;
static typing_context *Delta;
static char* wd;

void vernac_init(char* working_directory) {
  Gamma = telescope_empty();
  Sigma = context_empty();
  Delta = typing_context_empty();
  wd = strdup(working_directory);
}

int print_command(FILE* stream, command* c) {
  if (c == NULL) return fprintf(stream, "NULL");

  switch (c->tag) {
  case DEF:
    return fprintf(stream, "def %W : %W := %W.",
                   c->var, print_variable,
                   c->left, print_term,
                   c->right, print_term);
  case SIMPL:
    return fprintf(stream, "simpl %W.", c->left, print_term);
  case AXIOM:
    return fprintf(stream, "axiom %W : %W.", c->var, print_variable, c->left, print_term);
  case DATA: {
    int ans = fprintf(stream, "data %W ", c->var, print_variable);
    int i;
    for (i = 0; i < c->num_params; i++) {
      ans += fprintf(stream, "(%W : %W) ", c->param_names[i], print_variable, c->param_types[i], print_term);
    }
    ans += fprintf(stream, ":= <declaration>.");
    return ans;
  }
  default:
    return fprintf(stream, "<command>");
  }
}

static void vernac_run_def(command* c) {
  term* Type = make_type();
  check(typecheck_check(Gamma, Sigma, Delta, c->left, Type), "%W is not well typed.", c->left, print_term);
  free_term(Type);
  Type = NULL;

  check(typecheck_check(Gamma, Sigma, Delta, c->right, c->left), "Term %W\n failed to have type %W\n",
        c->right, print_term,
        c->right, print_term);

  if (!has_holes(c->right)) {
    Gamma = telescope_add(variable_dup(c->var), term_dup(c->left), Gamma);
    Sigma = context_add(variable_dup(c->var), term_dup(c->right), Sigma);
    printf("%W defined\n", c->var, print_variable);
  } else {
    printf("%W not defined because of remaining holes\n", c->var, print_variable);
  }
  return;
 error:
  free_term(Type);
}

static void vernac_run_check(command* c) {
  term* Type = make_type();
  if (c->right == NULL) {
    term* ty = typecheck(Gamma, Sigma, Delta, c->left);
    printf("%W : %W\n", c->left, print_term, ty, print_term);
    free_term(ty);
  } else {
    check(typecheck_check(Gamma, Sigma, Delta, c->right, Type), "RHS of check is ill-typed");
    check(typecheck_check(Gamma, Sigma, Delta, c->left, c->right), "Check failed.");
    printf("check succeeded.\n");
  }
 error:
  free_term(Type);
}

static int check_datatype(command *c, term** out_kind, term** out_type_constructor, term** out_applied_type) {
  int res = 0;
  term* type = make_type();
  telescope *Gamma_prime = Gamma;
  term* type_constructor = make_datatype_term(variable_dup(c->var), c->num_params);
  term* kind = make_type();
  term* A = NULL;

  term* k = NULL;
  term* tyC = NULL;
  int i; term* pk; term* pTyC;
  for (i = 0, pk = NULL, pTyC = NULL; i < c->num_params; i++) {
    check(typecheck_check(Gamma_prime, Sigma, Delta, c->param_types[i], type),
          "parameter %W has type %W, which is ill formed",
          c->param_names[i], c->param_types[i]);
    variable* x = c->param_names[i];
    term* ty = c->param_types[i];
    type_constructor->args[i] = make_var(variable_dup(x));
    term* new_kind = make_pi(variable_dup(x), term_dup(ty), NULL);
    term* new_tyC = make_lambda(variable_dup(x), term_dup(ty), NULL);
    if (pk == NULL) {
      pk = new_kind;
      pTyC = new_tyC;
      k = pk;
      tyC = pTyC;
    } else {
      pk->right = new_kind;
      pTyC->right = new_tyC;
      pk = pk->right;
      pTyC = pTyC->right;
    }
    Gamma_prime = telescope_add(variable_dup(x), term_dup(ty), Gamma_prime);
  }
  if (pk != NULL) {
    pk->right = kind;
    kind = k;
    pTyC->right = type_constructor;
    type_constructor = tyC;
  }

  Gamma_prime = telescope_add(variable_dup(c->var), term_dup(kind), Gamma_prime);

  context* Sigma_prime = context_add(variable_dup(c->var), term_dup(type_constructor), Sigma);

  A = make_var(variable_dup(c->var));
  for (i = 0; i < c->num_params; i++) {
    A = make_app(A, make_var(variable_dup(c->param_names[i])));
  }
  int num_constructors = c->num_args;
  for (i = 0; i < num_constructors; i++) {
    term *constructor = c->args[i];
    if (constructor->left == NULL) {
      constructor->left = make_var(variable_dup(c->var));
    }

    check(has_type(Gamma_prime, Sigma_prime, Delta, constructor->left, type),
          "constructor %W has type %W instead of Type", constructor->var,
          print_variable,
          typecheck(Gamma_prime, Sigma_prime, Delta, constructor->left),
          print_term);
    check(is_pi_returning(Sigma_prime, Delta,
                          constructor->left, A), "constructor %W does not return %W",
          constructor->var, print_variable, A, print_term);
    // todo: positivity
    // todo: allow constructor types to normalize to pi-types
  }
  free_term(type);
  type = NULL;

  *out_kind = kind;
  *out_type_constructor = type_constructor;
  *out_applied_type = A;

  res = 1;
 error:
  Gamma_prime = telescope_pop(Gamma_prime);
  for (i = 0; i < c->num_params; i++) {
    Gamma_prime = telescope_pop(Gamma_prime);
  }
  Gamma_prime = NULL;

  Sigma_prime = context_pop(Sigma_prime);
  Sigma_prime = NULL;

  free_term(type);
  type = NULL;


  return res;
}

static void add_datatype_to_context(command *c, term* kind, term* type_constructor) {
  Gamma = telescope_add(variable_dup(c->var), term_dup(kind), Gamma);
  Sigma = context_add(variable_dup(c->var), term_dup(type_constructor), Sigma);
}

static void build_constructors(command *c, datatype *T) {
  int i;
  int total_args = 0;
  for (i = 0; i < T->num_intros; i++) {
    term *constructor_type = c->args[i]->left;
    while (constructor_type->tag == PI) {
      total_args++;
      constructor_type = constructor_type->right;
    }
  }
  T->inductive_args = calloc(total_args, sizeof(int));
  int total_arg_index = 0;
  T->intros = malloc(T->num_intros * sizeof(term*));

  for (i = 0; i < c->num_args; i++) {
    term *constructor_type = c->args[i]->left;
    int num_args = 0;
    while (constructor_type->tag == PI) {
      num_args++;
      constructor_type = constructor_type->right;
    }
    term *intro = make_intro(variable_dup(c->args[i]->var), term_dup(T->applied_type), num_args, c->num_params);
    T->intros[i] = intro;
    term *lambda_wrapped_intro = intro;
    term *prev = NULL;
    int j;
    for (j = 0; j < c->num_params; j++) {
      term* new_lambda = make_lambda(variable_dup(c->param_names[j]), term_dup(c->param_types[j]), intro);
      if (prev == NULL) {
        lambda_wrapped_intro = new_lambda;
      } else {
        prev->right = new_lambda;
      }
      intro->params[j] = make_var(variable_dup(c->param_names[j]));
      prev = new_lambda;
    }

    constructor_type = c->args[i]->left;
    for (j = 0; j < num_args; j++) {
      variable *x = gensym("x");
      term *new_lambda = make_lambda(x, term_dup(constructor_type->left), intro);
      if (prev == NULL) {
        lambda_wrapped_intro = new_lambda;
      }
      else {
        prev->right = new_lambda;
      }
      term *arg = make_var(variable_dup(x));
      if (definitionally_equal(Sigma, Delta, constructor_type->left, T->applied_type)) {
        T->inductive_args[total_arg_index] = 1;
      }
      total_arg_index++;
      intro->args[j] = arg;
      prev = new_lambda;
      if (!variable_equal(&ignore, constructor_type->var)) {
        term* to = make_var(variable_dup(x));
        term* new = substitute(constructor_type->var, to, constructor_type->right);
        free_term(to);
        free_term(constructor_type->right);
        constructor_type->right = new;
        free_variable(constructor_type->var);
        constructor_type->var = variable_dup(x);
      }
      constructor_type = constructor_type->right;
    }
    Sigma = context_add(variable_dup(c->args[i]->var), lambda_wrapped_intro, Sigma);
    Gamma = telescope_add(variable_dup(c->args[i]->var),
                          typecheck(Gamma, Sigma, Delta, lambda_wrapped_intro),
                          Gamma);
  }
}

static void build_eliminator(command *c, datatype *T) {
  variable **vars = malloc((c->num_args+2) * sizeof(variable*));

  vars[0] = gensym("M");
  vars[c->num_args+1] = gensym("a");
  int i;
  for (i = 0; i < c->num_args; i++) {
    vars[i+1] = gensym("c");
  }
  char *elim_name;
  asprintf(&elim_name, "%s_elim", c->var->name, print_term);
  term *wrapped_eliminator;
  T->elim = make_elim(make_variable(strdup(elim_name)), c->num_args + 2, c->num_params);
  for (i = 0; i < c->num_args+2; i++) {
    T->elim->args[i] = make_var(variable_dup(vars[i]));
  }
  wrapped_eliminator = T->elim;
  wrapped_eliminator = make_lambda(variable_dup(vars[c->num_args+1]),
                                   term_dup(T->applied_type), wrapped_eliminator);
  for (i = c->num_args-1; i >= 0; i--) {
    term *constructor_type = c->args[i]->left;
    term *app = make_var(variable_dup(c->args[i]->var));
    int j;
    for (j = 0; j < c->num_params; j++) {
      app = make_app(app, make_var(variable_dup(c->param_names[j])));
    }

    app = make_app(make_var(variable_dup(vars[0])), app);

    term *prev = NULL;
    term *wrapped = app;
    while (constructor_type->tag == PI) {
      term *new_wrapper;
      variable *x = gensym("x");
      new_wrapper = make_pi(variable_dup(x), term_dup(constructor_type->left), app);
      if (prev == NULL) {
        wrapped = new_wrapper;
      }
      else
      {
        prev->right = new_wrapper;
      }
      // check to see if this is an inductive case
      if (definitionally_equal(Sigma, Delta, constructor_type->left, T->applied_type)) {
        new_wrapper->right = make_pi(variable_dup(&ignore),
                                     make_app(make_var(variable_dup(vars[0])),
                                              make_var(variable_dup(x))),
                                     app);
        new_wrapper = new_wrapper->right;
      }          
      app->right = make_app(app->right, make_var(variable_dup(x)));
      prev = new_wrapper;
      if (!variable_equal(&ignore, constructor_type->var)) {
        term* to = make_var(variable_dup(x));
        term* new = substitute(constructor_type->var, to, constructor_type->right);
        free_term(to);
        free_term(constructor_type->right);
        constructor_type->right = new;
        free_variable(constructor_type->var);
        constructor_type->var = variable_dup(x);
      }
      constructor_type = constructor_type->right;
      free_variable(x);
      x = NULL;
    }
    wrapped_eliminator = make_lambda(variable_dup(vars[i+1]),
                       wrapped,
                       wrapped_eliminator);
  }
  wrapped_eliminator = make_lambda(variable_dup(vars[0]),
                                   make_pi(variable_dup(&ignore), term_dup(T->applied_type), make_type()),
                     wrapped_eliminator);
  for (i = c->num_params - 1; i >= 0; i--) {
    wrapped_eliminator = make_lambda(variable_dup(c->param_names[i]),
                                     term_dup(c->param_types[i]),
                                     wrapped_eliminator);
    T->elim->params[i] = make_var(variable_dup(c->param_names[i]));
  }

  for (i = 0; i < c->num_args+2; i++) {
    free_variable(vars[i]);
    vars[i] = NULL;
  }
  free(vars);
  vars = NULL;
  Sigma = context_add(make_variable(strdup(elim_name)), wrapped_eliminator, Sigma);
  Gamma = telescope_add(make_variable(strdup(elim_name)),
                        typecheck(Gamma, Sigma, Delta, wrapped_eliminator),
                        Gamma);
  free(elim_name);
  elim_name = NULL;
}

static void build_params(command* c, datatype* T) {
  T->param_names = malloc(T->num_params * sizeof(variable*));
  T->param_types = malloc(T->num_params * sizeof(term*));
  int i;
  for (i = 0; i < T->num_params; i++) {
    T->param_names[i] = variable_dup(c->param_names[i]);
    T->param_types[i] = term_dup(c->param_types[i]);
  }
}

static void vernac_run_data(command *c) {
  term* kind = NULL;
  term* type_constructor = NULL;
  term* applied_type = NULL;
  if (!check_datatype(c, &kind, &type_constructor, &applied_type)) {
    return;
  }

  add_datatype_to_context(c, kind, type_constructor);
  datatype *T = make_datatype(variable_dup(c->var), c->num_args, c->num_params);
  T->kind = kind;
  T->type_constructor = type_constructor;
  T->applied_type = applied_type;

  build_params(c, T);
  build_constructors(c, T);
  build_eliminator(c, T);
  Delta = typing_context_add(T, Delta);
  printf("added datatype %W\n", T->name, print_variable);
}

void vernac_run(command *c) {
  switch (c->tag) {
  case DEF:
    vernac_run_def(c);
    break;
  case PRINT:
    printf("%W\n", context_lookup(c->var, Sigma), print_term);
    break;
  case CHECK:
    vernac_run_check(c);
    break;
  case SIMPL:
    {
      term* t = normalize(Sigma, Delta, c->left);
      printf("%W ==> %W\n", c->left, print_term, t, print_term);
      free_term(t);
      break;
    }
  case DATA:
    {
      vernac_run_data(c);
      break;
    }
  case AXIOM:
    {
      term* Type = make_type();
      term* ty = normalize_and_free(Sigma, Delta, typecheck(Gamma, Sigma, Delta, c->left));
      check(ty != NULL, "Axiom %W's type does not typecheck", c->left, print_term);
      check(ty->tag == TYPE, "Axiom %W's type has type %W instead of Type", c->left, print_term, ty, print_term);
      Gamma = telescope_add(variable_dup(c->var), term_dup(c->left), Gamma);
      Sigma = context_add(variable_dup(c->var), NULL, Sigma);
      printf("%W added as axiom\n", c->var, print_variable);
      free_term(ty);
      free_term(Type);
      break;
    }
  case IMPORT:
    {
      log_info("importing file %s", c->var->name);
      char* filename;
      asprintf(&filename, "%s/%s%s", wd, c->var->name, ".arvo");
      log_info("path = %s", filename);
      fflush(stdout);
      check(process_file(filename) == 0, "Failed to import file %s.", c->var->name);
      free(filename);
      break;
    }
  }

 error:
  return;
}

static command* make_command() {
  command* ans = malloc(sizeof(command));
  ans->var = NULL;
  ans->left = NULL;
  ans->right = NULL;
  ans->num_args = 0;
  ans->args = NULL;
  ans->num_params = 0;
  ans->param_names = NULL;
  ans->param_types = NULL;
  return ans;
}

void free_command(command* c) {
  if (c == NULL) return;
  free_variable(c->var);
  c->var = NULL;
  free_term(c->left);
  c->left = NULL;
  free_term(c->right);
  c->right = NULL;
  int i;
  for (i = 0; i < c->num_args; i++) {
    free_term(c->args[i]);
    c->args[i] = NULL;
  }
  free(c->args);
  c->args = NULL;

  for (i = 0; i < c->num_params; i++) {
    free_term(c->param_types[i]);
    c->param_types[i] = NULL;
    free_variable(c->param_names[i]);
    c->param_names[i] = NULL;
  }
  free(c->param_types);
  c->param_types = NULL;
  free(c->param_names);
  free(c);
}


command *make_def(variable *var, term *ty, term *t) {
  command *ans = make_command();
  ans->tag = DEF;
  ans->var = var;
  ans->left = ty;
  ans->right = t;
  return ans;
}
command *make_print(variable *t) {
  command *ans = make_command();
  ans->tag = PRINT;
  ans->var = t;
  return ans;
}

command *make_check(term *t) {
  command *ans = make_command();
  ans->tag = CHECK;
  ans->left = t;
  return ans;
}
command *make_simpl(term *t) {
  command *ans = make_command();
  ans->tag = SIMPL;
  ans->left = t;
  return ans;
}

command *make_data(variable* name, int num_constructors, int num_params) {
  command* ans = make_command();
  ans->tag = DATA;
  ans->var = name;
  ans->num_args = num_constructors;
  ans->args = malloc(num_constructors * sizeof(term*));
  ans->num_params = num_params;
  ans->param_names = malloc(num_params * sizeof(variable*));
  ans->param_types = malloc(num_params * sizeof(term*));
  return ans;
}

command *make_axiom(variable *var, term *ty) {
  command *ans = make_command();
  ans->tag = AXIOM;
  ans->var = var;
  ans->left = ty;
  return ans;
}

command *make_import(variable* name) {
  command *ans = make_command();
  ans->tag = IMPORT;
  ans->var = name;
  return ans;
}


int process_stream(char* filename, FILE* stream) {
  command *c;

  parsing_context* pc = make_parsing_context(filename, stream);

  while(!feof(stream) && (c = next_command(pc))) {
    vernac_run(c);
    free_command(c);
  }

  free_parsing_context(pc);

  return 0;
}

int process_file(char* filename) {
  FILE* stream = fopen(filename, "r");
  int ans = process_stream(filename, stream);
  fclose(stream);
  return ans;
}
