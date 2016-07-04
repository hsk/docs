#include "syntax.h"
#include <stdio.h>

double
eval(const syntax* t) {
  if (t == NULL) return 0;
  switch (t->tag) {
  case syntax_BIN:
    switch (((syntax_bin*)t)->op) {
    case '+': return eval(((syntax_bin*)t)->l) + eval(((syntax_bin*)t)->r);
    case '-': return eval(((syntax_bin*)t)->l) - eval(((syntax_bin*)t)->r);
    case '*': return eval(((syntax_bin*)t)->l) * eval(((syntax_bin*)t)->r);
    case '/': return eval(((syntax_bin*)t)->l) / eval(((syntax_bin*)t)->r);
    }
    break;
  case syntax_DOUBLE: return ((syntax_double*)t)->value;
  }
  return 0;
}
