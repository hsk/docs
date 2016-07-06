#include "syntax.h"
#include "parser.h"
#include <stdio.h>
#include <string.h>

syntax*
lookup(syntax* env, const char* id) {
  if(env==syntax_nil) return syntax_nil;
  if(env->tag==syntax_BIN && ((syntax_bin*)env)->op==CONS) {
    syntax* e1 = ((syntax_bin*)env)->l;
    if(e1->tag==syntax_BIN && ((syntax_bin*)e1)->op==CONS) {
      syntax* k = ((syntax_bin*)e1)->l;
      if(k->tag==syntax_VAR && strcmp(((syntax_var*)k)->id,id)==0)
        return ((syntax_bin*)e1)->r;
    }
    return lookup(((syntax_bin*)env)->r, id);
  }
  return syntax_nil;
}

static syntax* z = NULL;

syntax*
eval(syntax* env, syntax* t) {
  int rec;
  syntax* e1;
  syntax* e2;

  if (t == NULL) return 0;
  switch (t->tag) {
  case syntax_PRE:
    switch (((syntax_pre*)t)->op) {
    case '+':
      return eval(env, ((syntax_pre*)t)->e);
    case '-':
      e1 = eval(env, ((syntax_pre*)t)->e);
      if (e1->tag == syntax_DOUBLE)
        return syntax_double_new(- ((syntax_double*)e1)->value);
      break;
    }
    break;
  case syntax_BIN:
    switch (((syntax_bin*)t)->op) {
    case '+':
      e1 = eval(env, ((syntax_bin*)t)->l);
      e2 = eval(env, ((syntax_bin*)t)->r);
      if (e1->tag == syntax_DOUBLE && e2->tag == syntax_DOUBLE)
        return syntax_double_new(((syntax_double*)e1)->value + ((syntax_double*)e2)->value);
      break;
    case '-':
      e1 = eval(env, ((syntax_bin*)t)->l);
      e2 = eval(env, ((syntax_bin*)t)->r);
      if (e1->tag == syntax_DOUBLE && e2->tag == syntax_DOUBLE)
        return syntax_double_new(((syntax_double*)e1)->value - ((syntax_double*)e2)->value);
      break;
    case '*':
      e1 = eval(env, ((syntax_bin*)t)->l);
      e2 = eval(env, ((syntax_bin*)t)->r);
      if (e1->tag == syntax_DOUBLE && e2->tag == syntax_DOUBLE)
        return syntax_double_new(((syntax_double*)e1)->value * ((syntax_double*)e2)->value);
      break;
    case '/':
      e1 = eval(env, ((syntax_bin*)t)->l);
      e2 = eval(env, ((syntax_bin*)t)->r);
      if (e1->tag == syntax_DOUBLE && e2->tag == syntax_DOUBLE)
        return syntax_double_new(((syntax_double*)e1)->value / ((syntax_double*)e2)->value);
      break;
    case LE:
      e1 = eval(env, ((syntax_bin*)t)->l);
      e2 = eval(env, ((syntax_bin*)t)->r);
      if (e1->tag == syntax_DOUBLE && e2->tag == syntax_DOUBLE) {
        return syntax_bool_new(((syntax_double*)e1)->value <= ((syntax_double*)e2)->value ? -1 : 0);
      }
      break;
    case CONS:
      e1 = eval(env, ((syntax_bin*)t)->l);
      e2 = eval(env, ((syntax_bin*)t)->r);
      return syntax_bin_new(e1, CONS, e2);
    case ARROW:
      e1 = ((syntax_bin*)t)->l;
      e2 = ((syntax_bin*)t)->r;
      if (e1->tag == syntax_VAR) {
        return syntax_lambda_new(env, ((syntax_var*)e1)->id, e2);
      }
      break;
    case APP:
      e1 = eval(env, ((syntax_bin*)t)->l);
      e2 = eval(env, ((syntax_bin*)t)->r);
      if (e1->tag == syntax_LAMBDA) {
        env = syntax_bin_new(
          syntax_bin_new(
            syntax_var_new(((syntax_lambda*)e1)->id),
            CONS,
            e2
          ),
          CONS,
          ((syntax_lambda*)e1)->env);
        return eval(env,((syntax_lambda*)e1)->e1);
      }
      break;
    }
    break;
  case syntax_DOUBLE: return t;
  case syntax_BOOL: return t;
  case syntax_VAR: return lookup(env, ((syntax_var*)t)->id);
  case syntax_LAMBDA: return t;
  case syntax_LET:
      rec = ((syntax_let*)t)->rec;
      if(rec==0) {
        e1 = eval(env, ((syntax_let*)t)->e1);
        env = syntax_bin_new(
          syntax_bin_new(
            syntax_var_new(((syntax_let*)t)->id),
            CONS,
            e1),
          CONS,
          env);
        return eval(env,((syntax_let*)t)->e2);
      } else {
        e1 = ((syntax_let*)t)->e1;
        e2 = syntax_var_new(((syntax_let*)t)->id);
        env = syntax_bin_new(
          syntax_bin_new(
            e2,
            CONS,
            eval(env,syntax_bin_new(z,APP,syntax_bin_new(e2,ARROW,e1)))),
          CONS,
          env);
        return eval(env,((syntax_let*)t)->e2);
      }
      break;

  case syntax_IF:
      e1 = eval(env, ((syntax_if*)t)->e1);
      if(e1->tag==syntax_BOOL && ((syntax_bool*)e1)->value != 0)
        return eval(env, ((syntax_if*)t)->e2);
      return eval(env, ((syntax_if*)t)->e3);
  case syntax_NIL: return t;
  }
  return syntax_nil;
}

void
eval_init() {
  if(z==NULL) {
    syntax* f = syntax_var_new("f");
    syntax* x = syntax_var_new("x");
    syntax* y = syntax_var_new("y");
    z = eval(syntax_nil,
      syntax_bin_new(f, ARROW,
        syntax_bin_new(
          syntax_bin_new(x, ARROW,
            syntax_bin_new(f, APP, syntax_bin_new(y, ARROW,
              syntax_bin_new(syntax_bin_new(x,APP,x),APP,y)
            ))
          ),APP,
          syntax_bin_new(x, ARROW,
            syntax_bin_new(f, APP, syntax_bin_new(y, ARROW,
              syntax_bin_new(syntax_bin_new(x,APP,x),APP,y)
            ))
          )
        )
      ));
  }
}
