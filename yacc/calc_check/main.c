#include "parser.h"
#include "lexer.h"
#include <stdio.h>

extern int yyparse();

int
parse_input(FILE* f)
{
  yyrestart(f);
  return yyparse();
}

int
parse_string(const char* prog)
{
  yy_scan_string(prog);
  return yyparse();
}


int
main(int argc, const char**argv)
{
  if(!parse_string("1+2*3"))printf("ok\n");
  if(!parse_string("(1+2) * "))printf("ok\n");
  if(!parse_string("(1+2) * a"))printf("ok\n");
  if(!parse_string("(1+2) * a"))printf("ok\n");
  return 0;
}
