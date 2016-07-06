#include "syntax.h"
#include "parser.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

syntax syntax_nil_v = {syntax_NIL};

syntax_string
syntax_str_new(const char* s, int len)
{
  syntax_string t = malloc(sizeof(struct syntax_string)+len+1);
  t->len = len;
  memcpy(t->buf, s, len);
  t->buf[len] = '\0';
  return t;
}

syntax*
syntax_pre_new(int op, syntax* e)
{
  syntax_pre* t = malloc(sizeof(syntax_pre));
  t->tag = syntax_PRE;
  t->op = op;
  t->e = e;
  return (syntax*)t;
}

syntax*
syntax_bin_new(syntax* l, int op, syntax* r)
{
  syntax_bin* t = malloc(sizeof(syntax_bin));
  t->tag = syntax_BIN;
  t->l = l;
  t->op = op;
  t->r = r;
  return (syntax*)t;
}

syntax*
syntax_if_new(syntax* e1, syntax* e2, syntax* e3)
{
  syntax_if* t = malloc(sizeof(syntax_if));
  t->tag = syntax_IF;
  t->e1 = e1;
  t->e2 = e2;
  t->e3 = e3;
  return (syntax*)t;
}

syntax*
syntax_let_new(int rec, const char* id, syntax* e1, syntax* e2)
{
  int len = strlen(id);
  syntax_let* t = malloc(sizeof(syntax_let)+len+1);
  t->tag = syntax_LET;
  t->rec = rec;
  t->e1 = e1;
  t->e2 = e2;
  t->len = len;
  memcpy(t->id, id, len);
  t->id[len] = '\0';
  return (syntax*)t;
}

syntax*
syntax_lambda_new(syntax* env, const char* id, syntax* e1)
{
  int len = strlen(id);
  syntax_lambda* t = malloc(sizeof(syntax_lambda)+len+1);
  t->tag = syntax_LAMBDA;
  t->env = env;
  t->e1 = e1;
  t->len = len;
  memcpy(t->id, id, len);
  t->id[len] = '\0';
  return (syntax*)t;
}

syntax*
syntax_double_new(double d)
{
  syntax_double* t = malloc(sizeof(syntax_double));
  t->tag = syntax_DOUBLE;
  t->value = d;
  return (syntax*)t;
}

syntax*
syntax_bool_new(int b)
{
  syntax_bool* t = malloc(sizeof(syntax_bool));
  t->tag = syntax_BOOL;
  t->value = b;
  return (syntax*)t;
}

syntax*
syntax_var_new(const char* s)
{
  int len = strlen(s);
  syntax_var* t = malloc(sizeof(struct syntax_var)+len+1);
  t->tag = syntax_VAR;
  t->len = len;
  memcpy(t->id, s, len);
  t->id[len] = '\0';
  return (syntax*)t;
}

static void
print_str(syntax_string str)
{
  printf("%.*s", (int)str->len, str->buf);
}

void
syntax_p(syntax* t, int indent)
{
  int i;
  for (i = 0; i < indent; i++) putchar(' ');
  if(t==NULL) printf("NULL\n"); else
  switch (t->tag) {
  case syntax_PRE:
    printf("PRE(\n");
    if(((syntax_pre*)t)->op < 256)
      printf("'%c'\n", ((syntax_pre*)t)->op);
    else
    printf("\"unknown\"\n"); break;
    syntax_p(((syntax_pre*)t)->e, indent+1);
    for (i = 0; i < indent; i++) putchar(' ');
    printf(")\n");
    break;
  case syntax_BIN:
    printf("BIN(\n");
    syntax_p(((syntax_bin*)t)->l, indent+1);
    for (i = 0; i < indent+1; i++) putchar(' ');
    if(((syntax_bin*)t)->op < 256)
      printf("'%c'\n", ((syntax_bin*)t)->op);
    else
    switch(((syntax_bin*)t)->op) {
      case ARROW: printf("\"->\"\n"); break;
      case LE: printf("\"<=\"\n"); break;
      case APP: printf("\"app\"\n"); break;
      case CONS: printf("\"cons\"\n"); break;
      default: printf("\"unknown\"\n"); break;
    }
    syntax_p(((syntax_bin*)t)->r, indent+1);
    for (i = 0; i < indent; i++) putchar(' ');
    printf(")\n");
    break;
  case syntax_IF:
    printf("IF(\n");
    syntax_p(((syntax_if*)t)->e1, indent+1);
    syntax_p(((syntax_if*)t)->e2, indent+1);
    syntax_p(((syntax_if*)t)->e3, indent+1);
    for (i = 0; i < indent; i++) putchar(' ');
    printf(")\n");
    break;
  case syntax_LET:
    printf("LET(\n");
    for (i = 0; i < indent+1; i++) putchar(' ');
    printf("%d\n", (int)((syntax_let*)t)->rec);
    for (i = 0; i < indent+1; i++) putchar(' ');
    printf("%.*s\n", (int)((syntax_let*)t)->len, ((syntax_let*)t)->id);
    syntax_p(((syntax_let*)t)->e1, indent+1);
    syntax_p(((syntax_let*)t)->e2, indent+1);
    for (i = 0; i < indent; i++) putchar(' ');
    printf(")\n");
    break;
  case syntax_LAMBDA:
    printf("LAMBDA(\n");
    for (i = 0; i < indent+1; i++) putchar(' ');
    printf("%.*s\n", (int)((syntax_lambda*)t)->len, ((syntax_lambda*)t)->id);
    syntax_p(((syntax_lambda*)t)->e1, indent+1);
    for (i = 0; i < indent; i++) putchar(' ');
    printf(")\n");
    break;
  case syntax_DOUBLE:
    printf("DOUBLE(%f)\n", ((syntax_double*)t)->value);
    break;
  case syntax_BOOL:
    printf("BOOL(%s)\n", ((syntax_double*)t)->value ? "true" : "false");
    break;
  case syntax_VAR:
    printf("VAR");
    printf("(%.*s)\n", (int)((syntax_var*)t)->len, ((syntax_var*)t)->id);
    break;
  case syntax_NIL:
    printf("NIL\n");
    break;
  default:
    printf("UNKNOWN(%d)\n", t->tag);
    break;
  }
}

void
syntax_free(syntax* t)
{
  if(t==NULL)return;
  switch (t->tag) {
  case syntax_NIL:
    return;
  case syntax_BIN:
    syntax_free(((syntax_bin*)t)->l);
    syntax_free(((syntax_bin*)t)->r);
    break;
  case syntax_IF:
    syntax_free(((syntax_if*)t)->e1);
    syntax_free(((syntax_if*)t)->e2);
    syntax_free(((syntax_if*)t)->e3);
    break;
  case syntax_LET:
    syntax_free(((syntax_let*)t)->e1);
    syntax_free(((syntax_let*)t)->e2);
    break;
  case syntax_LAMBDA:
    syntax_free(((syntax_lambda*)t)->env);
    syntax_free(((syntax_lambda*)t)->e1);
    break;
  case syntax_DOUBLE:
  case syntax_BOOL:
  case syntax_VAR:
  case syntax_PRE:
    break;
  }
  free(t);
}