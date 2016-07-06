#include <stdio.h>
#include "arc.hpp"
#include "syntax.h"
#include "parser.h"
#include "lexer.h"

extern int yyparse();

parser_state state;

void
parse_init()
{
  state.lval=NULL;
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

double
eval(E* e)
{
  arc_case(e,EDbl*,e1) return e1->v;
  arc_case(e,EAdd*,e1) return eval(e1->l()) + eval(e1->r());
  arc_case(e,ESub*,e1) return eval(e1->l()) - eval(e1->r());
  arc_case(e,EMul*,e1) return eval(e1->l()) * eval(e1->r());
  arc_case(e,EDiv*,e1) return eval(e1->l()) / eval(e1->r());
  return 0;
}



arc_instance

int
main()
{
  arc::AutoReleasePool ap;

  { arc::LocalPool p;
    if(!parse_string("1+2*3")) {
      printf("%lf\n", eval(state.lval));
    }
  }

  { arc::LocalPool p;
    if(!parse_string("(1+2*3)/2")) {
      printf("%lf\n", eval(state.lval));
    }
  }

	return 0;
}
