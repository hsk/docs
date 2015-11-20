#include "ast.h"

E* ENew(ETag tag) {
  E* e = (E*)malloc(sizeof(E));
  e->tag = tag;
  return e;
}

E* EInt(int i) {
  E* e = ENew(EINT);
  e->intv = i;
  return e;
}
E* EFun(E* (*f)(E*,E*)) {
  E* e = ENew(EFUN);
  e->fun = f;
  return e;
}
E* EMac(E* (*f)(E*,E*)) {
  E* e = ENew(EMAC);
  e->fun = f;
  return e;
}
E* EStr(char* s) {
  E* e = ENew(ESTR);
  e->strv = s;
  return e;
}

E* ESym(char* s) {
  E* e = ENew(ESYM);
  e->symv = s;
  return e;
}

static E uni = (E){EUNI, 0};
E* EUni = &uni;

E* EPair(E* e1, E*e2) {
  E* e = ENew(ELST);
  e->hd = e1;
  e->tl = e2;
  return e;
}
E* EMsg0(E* e1, E*e2) {
  E* e = ENew(EMSG);
  e->hd = e1;
  e->tl = e2;
  return e;
}

E* EClo(E* e1, E*env, E*e2) {
  E* e = ENew(ECLO);
  e->hd = e1;
  e->tl = ENew(ELST);
  e->tl->hd = env;
  e->tl->tl = e2;
  return e;
}

#include <stdarg.h>

void printe(E* e);

void printes(E* e) {
  switch(e->tag) {
    case EUNI: return;
    case ELST:
      switch(e->tl->tag){
        case EUNI: printe(e->hd); return;
        case ELST: printe(e->hd);printf("; ");printes(e->tl); return;
        default: printe(e->hd);printf(". ");printe(e->tl); return;
      }
    default: printe(e);
  }
}

void printe(E* e) {
  switch(e->tag) {
    case EINT: printf("%d", e->intv); break;
    case ESTR: printf("\"%s\"", e->strv); break;
    case ESYM: printf("%s", e->symv); break;
    case ELST: printf("("); printes(e); printf(")"); break;
    case EUNI: printf("()"); break;
    case EMSG: printe(e->hd);printe(e->tl); break;
    case ECLO: printf("clo "); printe(e->hd);printf(" -> ");printe(e->tl->tl); break;
    case EFUN: printf("(native %p)", e->fun); break;
    case EMAC: printf("(mac %p)", e->fun); break;
    default: printf("(tag%d)", e->tag); break;
  }
}

#include <unistd.h>
char* to_s(E* e) {
  char fname[L_tmpnam];
  tmpnam(fname);

  int o = dup(fileno(stdout));
  freopen(fname,"w",stdout);
  printe(e);
  int len = ftell(stdout);
  dup2(o,fileno(stdout));
  close(o);

  char* data = malloc(len+1);
  data[len] = 0;
  FILE* fp = fopen(fname,"r");
  fread(data, 1, len, fp);
  fclose(fp);
  remove(fname);
  return data;
}

void printeln(E* e) {
  printe(e);
  printf("\n");
}

E* EList(E* e1, ...) {
  if(e1==NULL) return EUni;

  va_list list;
  va_start( list ,  e1);

  E* e = EPair(e1,EUni);
  E* tail = e;
  while((e1 = va_arg(list, E*))!=NULL) {
    tail->tl = EPair(e1, EUni);
    tail = tail->tl;
  }
  va_end( list );
  return e;
}

E* EMsg(E* e1, ...) {
  va_list list;
  va_start( list ,  e1);

  E* e = EPair(e1,EUni);
  e->tag = EMSG;
  E* tail = e;
  while((e1 = va_arg(list, E*))!=NULL) {
    tail->tl = EPair(e1, EUni);
    tail = tail->tl;
  }
  va_end( list );
  return e;
}
E* EBin(char* msg, E* e1, E* e2) {
  return EMsg(ESym(msg), e1, e2, NULL);
}
E* EIf(E* e1, E* e2, E* e3) {
  return EMsg(ESym("if"), e1, e2, e3, NULL);
}

E* EDef(char* name, E* e1, E*e2) {
  return EMsg(ESym("def"), ESym(name), e1, e2, NULL);
}

E* ELam(E* e1, E*e2) {
  return EMsg(ESym("lam"), e1, e2, NULL);
}
