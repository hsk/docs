#include "ast.h"

int eq(E* e1, E* e2) {
  if (e1==e2) return 1; 
  if (e1->tag != e2->tag) return 0;
  switch(e1->tag) {
    case EINT: return e1->intv == e2->intv;
    case ESTR: return strcmp(e1->strv, e2->strv)==0;
    case ESYM: return strcmp(e1->symv, e2->symv)==0;
    case EMSG:
    case ECLO:
    case ELST: if(eq(e1->hd, e2->hd)) return eq(e1->tl, e2->tl);
    case EFUN: return e1->fun == e2->fun;
    default: return 0;
  }
}

E* List_assoc(E* e, E* ls) {
  while(ls != EUni && ls->tag == ELST) {
    E* hd = ls->hd;
    if (hd->tag == ELST && eq(hd->hd, e)) return hd->tl;
    ls = ls->tl;
  }
  return EUni;
}

E* eval(E* env, E* e) {
  switch(e->tag) {
  default: return EPair(env, e);
  case ESYM: return EPair(env, List_assoc(e, env));
  case ELST:{
      if(e==EUni) return EPair(env, e);
      E* env_e1 = eval(env, e->hd);
      E* env_e = eval(env_e1->hd, e->tl);
      return EPair(env_e->hd, EPair(env_e1->tl, env_e->tl));
    }
  case EMSG:{
      E* msg = e->hd;
      msg = eval(env, msg)->tl;
      switch(msg->tag) {
        default: return EPair(env, EMsg(msg, eval(env, e->tl)->tl));
        case EMAC: return msg->fun(env, e->tl);
        case EFUN: return msg->fun(env, eval(env, e->tl)->tl);
        case ECLO: {
            E* tl = eval(env, e->tl)->tl;
            E* nenv = msg->tl->hd;
            E* pp = msg->hd;
            while(tl != EUni || pp != EUni) {
              nenv = EPair(EPair(pp->hd, tl->hd), nenv);
              tl = tl->tl;
              pp = pp->tl;
            }
            E* r = eval(nenv, msg->tl->tl)->tl;
            return EPair(env, r);
          }
      }
    }
  }
}

static E* eif(E* env, E* e) {
  if(e->tag==ELST && e->tl->tag == ELST && e->tl->tl->tag == ELST) {
    E* a = e->hd;
    E* b = e->tl->hd;
    E* c = e->tl->tl->hd;
    if (eval(env, a)->tl != EUni) return eval(env, b);
    return eval(env, c);
  }
  return EPair(env, e);
}

static E* block(E* env, E* e) {
  if(e->tag!=ELST) return EPair(env, e);
  E* env_e1 = eval(env, e->hd);
  if(e->tl == EUni) return env_e1;
  return block(env_e1->hd, e->tl);
}

static E* let(E* env, E* e) {
  if(e->tag==ELST && e->tl->tag == ELST) {
    E* a = e->hd;
    E* b = e->tl->hd;
    if(a->tag==ESYM) {
      b = eval(env, b)->tl;
      return EPair(EPair(EPair(a, b),env), b);
    }
  }
  return EPair(env, e);
}

static E* lam(E* env, E* e) {
  E* a = e->hd;
  E* b = e->tl->hd;
  if(a->tag==ELST) return EPair(env, EClo(a, env, b));
  return EPair(env, e);
}

static E* def(E* env, E* e) {
  E* nam = e->hd;
  E* a = e->tl->hd;
  E* b = e->tl->tl->hd;
//  printf("def------ %s a %s b %s\n", to_s(nam), to_s(a), to_s(b));
  E* clo = EClo(a, env, b);
  clo->tl->hd = EPair(EPair(nam, clo), clo->tl->hd);
  return EPair(clo->tl->hd, clo);
}


static E* add(E* env, E* e) {
  if(e->tag==ELST && e->tl->tag == ELST) {
    E* a = e->hd;
    E* b = e->tl->hd;
    if(a->tag==EINT && b->tag==EINT)
      return EPair(env, EInt(a->intv + b->intv));
    if(a->tag==ESTR && b->tag==ESTR) {
      int lena = strlen(a->strv);
      int lenb = strlen(b->strv);
      char* c = (char*)malloc(lena+lenb+1);
      memcpy(c,a->strv,lena);
      memcpy(&c[lena],b->strv,lenb+1);
      return EPair(env, EStr(c));
    }
    return EPair(env, e);
  }
  return EPair(env, e);
}

static E* sub(E* env, E* e) {
  if(e->tag==ELST && e->tl->tag == ELST) {
    E* a = e->hd;
    E* b = e->tl->hd;
    if(a->tag==EINT && b->tag==EINT)
      return EPair(env, EInt(a->intv - b->intv));
    return EPair(env, EMsg(e->hd, EList(a,b,NULL),NULL));
  }
  return EPair(env, e);
}

static E* mul(E* env, E* e) {
  if(e->tag==ELST && e->tl->tag == ELST) {
    E* a = e->hd;
    E* b = e->tl->hd;
    if(a->tag==EINT && b->tag==EINT)
      return EPair(env, EInt(a->intv * b->intv));
    return EPair(env, EMsg(e->hd, EList(a,b,NULL),NULL));
  }
  return EPair(env, e);
}

static E* lt(E* env, E* e) {
  if(e->tag==ELST && e->tl->tag == ELST) {
    E* a = e->hd;
    E* b = e->tl->hd;
    if(a->tag==EINT && b->tag==EINT)
      return EPair(env, a->intv < b->intv ? EInt(1) : EUni);
    return EPair(env, EMsg(e->hd, EList(a,b,NULL),NULL));
  }
  return EPair(env, e);
}

static E* gt(E* env, E* e) {
  if(e->tag==ELST && e->tl->tag == ELST) {
    E* a = e->hd;
    E* b = e->tl->hd;
    if(a->tag==EINT && b->tag==EINT)
      return EPair(env, a->intv > b->intv ? EInt(1) : EUni);
    return EPair(env, EMsg(e->hd, EList(a,b,NULL),NULL));
  }
  return EPair(env, e);
}

static E* le(E* env, E* e) {
  if(e->tag==ELST && e->tl->tag == ELST) {
    E* a = e->hd;
    E* b = e->tl->hd;
    if(a->tag==EINT && b->tag==EINT)
      return EPair(env, a->intv <= b->intv ? EInt(1) : EUni);
    return EPair(env, EMsg(e->hd, EList(a,b,NULL),NULL));
  }
  return EPair(env, e);
}

static E* ge(E* env, E* e) {
  if(e->tag==ELST && e->tl->tag == ELST) {
    E* a = e->hd;
    E* b = e->tl->hd;
    if(a->tag==EINT && b->tag==EINT)
      return EPair(env, a->intv >= b->intv ? EInt(1) : EUni);
    return EPair(env, EMsg(e->hd, EList(a,b,NULL),NULL));
  }
  return EPair(env, e);
}

static E* equ(E* env, E* e) {
  if(e->tag==ELST && e->tl->tag == ELST) {
    E* a = e->hd;
    E* b = e->tl->hd;
    return EPair(env, eq(a,b) ? EInt(1) : EUni);
  }
  return EPair(env, e);
}

int yyparse(void);
extern FILE *yyin;
extern int yydebug; /* デバッグ用 */

static E* e_root;

void setE(E* e) {
  e_root = e;
}

E* fparse(FILE* fp) {
  yyin = fp;
  setE(EUni);
  yyparse();
  return e_root;  
}

E* parse(char* str) {
  FILE* fp = tmpfile();
  fprintf(fp, "%s", str);
  fseek(fp, 0, SEEK_SET);
  return fparse(fp);
}

static E* parse_e(E* env, E*e) {
  if(e->hd->tag==ESTR)
    return EPair(env, parse(e->hd->strv));    
  return EPair(env, e);
}

static E* eval_e(E* env, E* e) {
  return eval(env, e->hd);
}

static E* read(E* env, E* e) {
  if(e->hd->tag==ESTR) {
    FILE* fp = fopen(e->hd->strv,"r");
    if(fp==NULL) {
      return EPair(env, e);
    }
    fseek(fp,0,SEEK_END);
    int len = ftell(fp);
    fseek(fp,0,SEEK_SET);
    
    char* str = (char*)malloc(len+1);
    str[len] = 0;
    fread(str,1,len,fp);
    fclose(fp);
    return EPair(env, EStr(str));
  }
  return EPair(env, e);
}

E* init() {
  EUni = EPair(EUni,EUni); EUni->hd=EUni;EUni->tl =EUni;

  return EList(
    EPair(ESym("mul"),EFun(mul)),
    EPair(ESym("add"),EFun(add)),
    EPair(ESym("sub"),EFun(sub)),
    EPair(ESym("lt"),EFun(lt)),
    EPair(ESym("gt"),EFun(gt)),
    EPair(ESym("le"),EFun(le)),
    EPair(ESym("ge"),EFun(ge)),
    EPair(ESym("eq"),EFun(equ)),
    EPair(ESym("if"),EMac(eif)),
    EPair(ESym("parse"),EFun(parse_e)),
    EPair(ESym("eval"),EFun(eval_e)),
    EPair(ESym("read"),EFun(read)),
    EPair(ESym("lam"),EMac(lam)),
    EPair(ESym("def"),EMac(def)),
    EPair(ESym("let"),EMac(let)),
    EPair(ESym("block"),EMac(block)),
    NULL
  );
}
