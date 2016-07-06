#include "syntax.h"
#include "parser.h"
#include "lexer.h"
#include <stdio.h>

extern int yyparse();

void
parse_init()
{
  yylineno = 1;
  state.lval = NULL;
}

int
parse_input(FILE* f)
{
  parse_init();
  yyrestart(f);
  return yyparse();
}

int
parse_string(const char* prog)
{
  parse_init();
  yy_scan_string(prog);
  return yyparse();
}


int
main(int argc, const char**argv)
{
  parse_string("1+2*3");
  syntax_pp(state.lval, 0);
  syntax_free(state.lval);
  parse_string("(1+2) * ");
  syntax_pp(state.lval, 0);
  syntax_free(state.lval);
  return 0;
}
