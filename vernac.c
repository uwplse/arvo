#include "vernac.h"
#include "telescope.h"
#include "context.h"
#include "typing_context.h"
#include "typecheck.h"
#include "term.h"
#include "normalize.h"
#include "printing.h"

#include <stdlib.h>

static telescope *Gamma;
static context *Sigma;
static typing_context *Delta;

void vernac_init() {
  Gamma = telescope_empty();
  Sigma = context_empty();
  Delta = typing_context_empty();
}

void vernac_run(command *c) {
  switch (c->tag) {
  case DEF:
    {
      term* ty = typecheck(Gamma, Sigma, Delta, c->right);
      check(definitionally_equal(Sigma, Delta, ty, c->left), "Term %W has type %W <> %W",
            c->right, print_term,
            ty, print_term,
            c->left, print_term);
      free_term(ty);

      Gamma = telescope_add(variable_dup(c->var), term_dup(c->left), Gamma);
      Sigma = context_add(variable_dup(c->var), term_dup(c->right), Sigma);
      printf("%W defined\n", c->var, print_variable);
      break;
    }
  case PRINT:
    printf("%W\n", context_lookup(c->var, Sigma), print_term);
    break;
  case CHECK:
    {
      term* ty = typecheck(Gamma, Sigma, Delta, c->left);
      printf("%W : %W\n", c->left, print_term, ty, print_term);
      free_term(ty);
      break;
    }
  case SIMPL:
    {
      term* t = normalize(Sigma, Delta, c->left);
      printf("%W ==> %W\n", c->left, print_term, t, print_term);
      free_term(t);
      break;
    }
  case DATA:
    {
      // check that this is a reasonable type definition
      telescope *Gamma_prime = telescope_add(variable_dup(c->var), make_type(), Gamma);
      term *tA = make_datatype_term(variable_dup(c->var));
      context *Sigma_prime = context_add(variable_dup(c->var), tA, Sigma);
      
      term *A = make_var(variable_dup(c->var));
      int num_constructors = c->num_args;
      int i;
      for (i = 0; i < num_constructors; i++) {
        term *constructor = c->args[i];
        if (constructor->left == NULL) {
          constructor->left = term_dup(A);
        }
        term *type = make_type();
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
      
      
      /* Modify Gamma. For a datatype A with constructors a1,...,an and
         eliminator E, we need terms with types: 
         - A : Type
         - a1,...,an : A
         - E : (M : A -> Type) -> M a1 -> ... -> M an -> (a : A) -> M a
      */
      Gamma = Gamma_prime;
      for (i = 0; i < c->num_args; i++) {
        Gamma = telescope_add(variable_dup(c->args[i]->var), term_dup(c->args[i]->left), Gamma);
      }
      
      term *tyMotive = make_pi(variable_dup(&ignore), term_dup(A), make_type());
      variable *M = gensym("M");
      term *tM = make_var(M);
      variable *a = gensym("a");
      term *ta = make_var(a);
      term *elim = make_pi(variable_dup(a), term_dup(A), make_app(term_dup(tM), term_dup(ta)));
      free_term(ta);
      ta = NULL;
      for (i = c->num_args-1; i >= 0; i--) {
        term *constructor_type = c->args[i]->left;
        term *argument_to_motive = make_var(variable_dup(c->args[i]->var));
        term *type_of_evidence = make_app(term_dup(tM), argument_to_motive);
        term *app = type_of_evidence;
        while (constructor_type->tag == PI) {
          variable *x = gensym(make_variable("x"));
          // check to see if this is an inductive case
          if (definitionally_equal(Sigma, Delta, constructor_type->left, A)) {
            type_of_evidence = make_pi(variable_dup(&ignore), make_app(term_dup(tM), make_var(x)), type_of_evidence);
          }
          type_of_evidence = make_pi(x, term_dup(constructor_type->left), type_of_evidence);
          app->right = make_app(app->right, make_var(x));
          constructor_type = constructor_type->right;
        }
        elim = make_pi(variable_dup(&ignore),
                       type_of_evidence,
                       elim);
      }
      elim = make_pi(variable_dup(M), tyMotive, elim);
      free_term(tM);
      tM = NULL;
      char *elim_name;
      asprintf(&elim_name, "%W_elim", A, print_term);
      Gamma = telescope_add(make_variable(strdup(elim_name)), elim, Gamma);
      
      /* Modify Sigma. For a datatype A with constructors a1,...,an and
         eliminator E, we need terms with tags:
         - A : DATATYPE
         - a1,...,an : INTRO
         - elim : \M : (A -> Type) . \x1 : M a1 . ... \xn : M an . \a : A . ELIM(M; x1; ...; xn; a)
      */

      Sigma = Sigma_prime;
      for (i = 0; i < c->num_args; i++) {
        /*
        constructor_type = c->args[i]->left;
        while (constructor_type->tag == PI) {
          
        }*/
        Sigma = context_add(variable_dup(c->args[i]->var), term_dup(c->args[i]), Sigma);
      }
      variable **vars = malloc((c->num_args+2) * sizeof(variable*));
      vars[0] = gensym("M");
      vars[c->num_args+1] = gensym("a");
      for (i = 0; i < c->num_args; i++) {
        vars[i+1] = gensym("c");
      }
      term *eliminator;
      eliminator = make_elim(make_variable(strdup(elim_name)), c->num_args + 2);
      for (i = 0; i < c->num_args+2; i++) {
        eliminator->args[i] = make_var(vars[i]);
      }
      elim = eliminator;
      elim = make_lambda(vars[c->num_args+1], term_dup(A), elim);
      free_term(A);
      A = NULL;
      for (i = c->num_args-1; i >= 0; i--) {
        elim = make_lambda(vars[i+1],
                       make_app(make_var(vars[0]), make_var(variable_dup(c->args[i]->var))),
                       elim);
      }
      elim = make_lambda(variable_dup(vars[0]), make_pi(variable_dup(&ignore), term_dup(A), make_type()),
                         elim);
      free(vars);
      vars = NULL;
      Sigma = context_add(make_variable(elim_name), elim, Sigma);

      // Modify Delta

      datatype *d = make_datatype(variable_dup(c->var), c->num_args, term_dup(eliminator));
      for (i = 0; i < c->num_args; i++) {
        d->intros[i] = term_dup(c->args[i]);
      }
      Delta = typing_context_add(d, Delta);
      printf("added datatype %W\n", A, print_term);
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

command *make_data(variable* name, int num_constructors) {
  command* ans = make_command();
  ans->tag = DATA;
  ans->var = name;
  ans->num_args = num_constructors;
  ans->args = malloc(num_constructors * sizeof(term*));
  return ans;
}
