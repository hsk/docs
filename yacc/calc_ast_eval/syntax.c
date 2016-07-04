#include "syntax.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

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
syntax_double_new(double d)
{
  syntax_double* t = malloc(sizeof(syntax_double));
  t->tag = syntax_DOUBLE;
  t->value = d;
  return (syntax*)t;
}

static void
print_str(syntax_string str)
{
  printf("%.*s", (int)str->len, str->buf);
}

void
syntax_pp(syntax* t, int indent)
{
  int i;
  for (i = 0; i < indent; i++) putchar(' ');
  if(t==NULL) printf("NULL\n"); else
  switch (t->tag) {
  case syntax_BIN:
    printf("BIN(\n");
    syntax_pp(((syntax_bin*)t)->l, indent+1);
    for (i = 0; i < indent+1; i++) putchar(' ');
    printf("'%c'\n", ((syntax_bin*)t)->op);
    syntax_pp(((syntax_bin*)t)->r, indent+1);
    for (i = 0; i < indent; i++) putchar(' ');
    printf(")\n");
    break;
  case syntax_DOUBLE:
    printf("DOUBLE(%f)\n", ((syntax_double*)t)->value);
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
  case syntax_BIN:
    syntax_free(((syntax_bin*)t)->l);
    syntax_free(((syntax_bin*)t)->r);
    break;
  default:
    break;
  }
  free(t);
}

