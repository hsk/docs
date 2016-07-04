#include "syntax.h"
#include "parser.h"
#include "lexer.h"
#include "eval.h"
#include <stdio.h>

extern int yyparse(parser_state* p);

void
parse_init(parser_state* p)
{
  p->lval = NULL;
}

int
parse_input(parser_state* p, FILE* f)
{
  parse_init(p);
  yyrestart(f);
  return yyparse(p);
}

int
parse_string(parser_state* p, const char* prog)
{
  parse_init(p);
  yy_scan_string(prog);
  return yyparse(p);
}


int
main(int argc, const char**argv)
{
  parser_state state;
  parse_string(&state, "1+2*3");
  syntax_pp(state.lval, 0);
  printf("eval %lf\n", eval(state.lval));
  syntax_free(state.lval);
  parse_string(&state, "(1+2) * ");
  syntax_pp(state.lval, 0);
  eval(state.lval);
  printf("eval %lf\n", eval(state.lval));
  syntax_free(state.lval);
  return 0;
}
