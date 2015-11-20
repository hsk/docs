#ifndef AST_H
#define AST_H

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

typedef enum { EINT, ESTR, ESYM, EMSG, ELST, EFUN, ECLO, EMAC } ETag;

typedef struct E {
  ETag tag;
  union {
   int intv;
   char* strv;
   char* symv;
   struct E* hd;
   struct E* (*fun)(struct E*,struct E*);
  };
  struct E* tl;
} E;

E* ENew(ETag tag);
E* EInt(int i);
E* EFun(E* (*f)(E*,E*));
E* EMac(E* (*f)(E*,E*));
E* EStr(char* s);
E* ESym(char* s);
extern E* EUni;
E* EPair(E* e1, E*e2);
E* EMsg0(E* e1, E*e2);
E* ELam(E* e1, E*e2);
E* EDef(char* name, E* e1, E*e2);
E* EClo(E* e1, E*env, E*e2);
void printe(E* e);
void printes(char* sep, E* e);
void printeln(E* e);
char* to_s(E* e);
E* EList(E* e1, ...);
E* EMsg(E* e1, ...);
E* EBin(char* msg, E* e1, E* e2);
E* EIf(E* e1, E* e2, E* e3);

void setE(E *);

int eq(E* e1, E* e2);
E* init();
E* List_assoc(E* e, E* ls);
E* eval(E* env, E* e);
E* parse(char* e);
E* fparse(FILE* fp);
#endif /*AST_H*/
