#include "syntax.h"
#include "parser.h"
#include "lexer.h"
#include <stdio.h>

extern int yyparse();

void
parse_init()
{
  state.lval = 0;
  yylineno=1;
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
  if(!parse_string("\n\n1+2*\n3")) printf("result>%lf\n", state.lval);
  if(!parse_string("1+2*10")) printf("result>%lf\n", state.lval);
  return 0;
}
