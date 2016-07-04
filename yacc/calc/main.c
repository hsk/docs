#include "syntax.h"
#include "parser.h"
#include "lexer.h"
#include <stdio.h>

extern int yyparse(parser_state* p);

void
parse_init(parser_state* p)
{
  p->lval = 0;
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
  if(!parse_string(&state, "1+2*3")) printf("result>%lf\n", state.lval);
  if(!parse_string(&state, "1+2*10")) printf("result>%lf\n", state.lval);
  return 0;
}
