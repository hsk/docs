#ifndef SYNTAX_H
#define SYNTAX_H

#include "arc.hpp"


struct E : arc::object {};
struct EDbl : E {
  double v;
  EDbl(double v):v(v){}
};

struct EAdd : E {
  arc::ptr<E> l;
  arc::ptr<E> r;
  EAdd(E* l, E* r):l(l),r(r){}
};

struct ESub : E {
  arc::ptr<E> l;
  arc::ptr<E> r;
  ESub(E* l, E* r):l(l),r(r){}
};

struct EMul : E {
  arc::ptr<E> l;
  arc::ptr<E> r;
  EMul(E* l, E* r):l(l),r(r){}
};

struct EDiv : E {
  arc::ptr<E> l;
  arc::ptr<E> r;
  EDiv(E* l, E* r):l(l),r(r){}
};

struct parser_state;
extern parser_state state;

struct parser_state {
  E* lval;
};

#endif
