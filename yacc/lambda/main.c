#include "syntax.h"
#include "parser.h"
#include "lexer.h"
#include "eval.h"
#include <stdio.h>

extern int yyparse(parser_state* p);

void
parse_init(parser_state* p)
{
  p->lval = syntax_nil;
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
  eval_init();
  parser_state state;
  parse_string(&state, "1+2*3");
  syntax_p(state.lval, 0);
  syntax_p(eval(syntax_nil, state.lval), 0);
  syntax_free(state.lval);

  parse_string(&state, "1");
  syntax_p(state.lval, 0);
  //printf("eval %lf\n", eval(state.lval));
  syntax_free(state.lval);

  parse_string(&state, "abc");
  syntax_p(state.lval, 0);
  //printf("eval %lf\n", eval(state.lval));
  syntax_free(state.lval);

  parse_string(&state, "1::2+3::()");
  syntax_p(state.lval, 0);
  syntax_p(eval(syntax_nil, state.lval), 0);
  syntax_free(state.lval);

  parse_string(&state, "(a::2)::(b::3)::()");
  syntax* env = state.lval;
  syntax_p(state.lval, 0);

  syntax_p(lookup(env, "a"), 0);
  syntax_p(lookup(env, "b"), 0);
  syntax_p(lookup(env, "c"), 0);
  syntax_free(env);

  parse_string(&state, "abc -> abc");
  syntax_p(state.lval, 0);
  syntax_p(eval(syntax_nil, state.lval), 0);
  syntax_free(state.lval);

  parse_string(&state, "(abc -> abc)10");
  syntax_p(state.lval, 0);
  syntax_p(eval(syntax_nil, state.lval), 0);
  syntax_free(state.lval);

  parse_string(&state, "let abc=1 in abc");
  syntax_p(state.lval, 0);
  syntax_p(eval(syntax_nil, state.lval), 0);
  syntax_free(state.lval);

  parse_string(&state, "let abc=2 in if abc<=1 then 1 else 2");
  syntax_p(state.lval, 0);
  syntax_p(eval(syntax_nil, state.lval), 0);
  syntax_free(state.lval);

  parse_string(&state, "let abc=0 in if abc<=1 then 1 else 2");
  syntax_p(state.lval, 0);
  syntax_p(eval(syntax_nil, state.lval), 0);
  syntax_free(state.lval);

  parse_string(&state, "let abc=0 in abc<=1");
  syntax_p(state.lval, 0);
  syntax_p(eval(syntax_nil, state.lval), 0);
  syntax_free(state.lval);

  parse_string(&state, "let abc=2 in abc<=1");
  syntax_p(state.lval, 0);
  syntax_p(eval(syntax_nil, state.lval), 0);
  syntax_free(state.lval);

  parse_string(&state, "(x -> x + 1)1");
  syntax_p(state.lval, 0);
  syntax_p(eval(syntax_nil, state.lval), 0);
  syntax_free(state.lval);

  parse_string(&state, "let rec abc=1 in abc");
  syntax_p(state.lval, 0);
  syntax_p(eval(syntax_nil, state.lval), 0);
  syntax_free(state.lval);

  parse_string(&state,
    "let rec sum = \n"
    "  x -> if x <= 0 then 0 else x + sum (x-1) \n"
    "in \n"
    "sum 10\n");
  syntax_p(state.lval, 0);
  syntax_p(eval(syntax_nil, state.lval), 0);
  syntax_free(state.lval);

  parse_string(&state, "(1+2) * ");
  syntax_p(state.lval, 0);
  syntax_p(eval(syntax_nil, state.lval), 0);
  syntax_free(state.lval);

  parse_string(&state, "-1");
  syntax_p(state.lval, 0);
  syntax_p(eval(syntax_nil, state.lval), 0);
  syntax_free(state.lval);

  parse_string(&state, "(x-> -x)(+1)");
  syntax_p(state.lval, 0);
  syntax_p(eval(syntax_nil, state.lval), 0);
  syntax_free(state.lval);
  return 0;
}
