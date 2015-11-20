#include <stdio.h>
#include "ast.h"
#include <assert.h>

E* env;

void test(E* e) {
  printe(e);
  printf(" => ");
  printeln(eval(env, e)->tl);
}

int test_eval(char* s1, char* s2) {
  E* e1 = parse(s1);
  printf("test ");
  printeln(e1);
  e1 = eval(env, e1)->tl;
  E* e2 = parse(s2);
  return eq(e1, e2);
}

void run(char* filename) {
  test(fparse(fopen(filename,"r")));
}


int main(int argc, char ** argv){

  env = init();
  printf("koko\n");
  assert(eq(parse("1"),EInt(1)));
  assert(strcmp(to_s(parse("1")),"1")==0);
  printf("koko1\n");
  assert(eq(parse("1,2"),EPair(EInt(1),EInt(2))));
  printf("kokoa\n");
  assert(strcmp(to_s(EPair(EInt(1),EInt(2))),"(1. 2)")==0);
  printf("kokog\n");
  assert(eq(parse("()"),EUni));
  printf("koko2\n");
  assert(eq(parse("[]"),EList(NULL)));
  assert(eq(parse("[1]"),EList(EInt(1),NULL)));
  assert(eq(parse("[1;2]"),EList(EInt(1),EInt(2),NULL)));
  assert(eq(parse("a(1;2)"),EBin("a",EInt(1),EInt(2))));
  assert(eq(parse("a(1;2)(b;c)"),EMsg(EBin("a",EInt(1),EInt(2)),ESym("b"),ESym("c"),NULL)));
  assert(test_eval("if(();1;2)", "2"));
  assert(test_eval("if(1;1;2)", "1"));
  assert(test_eval("if(0;1;2)", "1"));
  assert(test_eval("1", "1"));
  assert(test_eval("1+1", "2"));
  assert(test_eval("1+10*20", "201"));
  assert(test_eval("[2+1;20*10]", "[3;200]"));
  printf("koko2\n");

  printeln(EPair(EInt(1),EPair(EInt(2),EInt(3))));
  printeln(EList(NULL));
  printeln(EList(EInt(1),NULL));
  printeln(EList(EInt(1),EInt(2),NULL));
  printeln(EMsg(EBin("abc",EInt(1),EInt(2)), ESym("bb"), NULL));

  test(EInt(1));  
  test(EList(EInt(1),EInt(2),NULL));  
  test(EBin("add",EInt(2),EInt(1)));
  test(EList(EBin("add",EInt(2),EInt(1)),EBin("mul",EInt(20),EInt(10)),NULL));
  test(EMsg(ESym("if"),EUni,EInt(1),EInt(2),NULL));
  test(EMsg(ESym("if"),EInt(0),EInt(1),EInt(2),NULL));
  test(EMsg(ESym("if"),EInt(1),EInt(1),EInt(2),NULL));
  test(EIf(EUni,EInt(1),EInt(2)));

  test(EBin("let",ESym("x"),EInt(1)));
  assert(test_eval("let(x;1)", "1"));
  assert(test_eval("block(let(x;1);x)", "1"));
  test(EMsg(ESym("block"),
    EBin("let",ESym("x"),EInt(1)),
    ESym("x"),
    NULL)
  );
  assert(test_eval("block(20*10)", "200"));
  test(EMsg(ESym("block"),
    EBin("mul",EInt(20),EInt(10)),NULL));

  assert(test_eval("block(let(x;20); x * 10)", "200"));
  test(EMsg(ESym("block"),
    EBin("let",ESym("x"),EInt(20)),
    EBin("mul",ESym("x"),EInt(10)),NULL));

  test(parse("block(let(a;fun(x)->x))"));
  assert(test_eval("block(let(a;fun(x)->x); a(10))", "10"));
  test(EMsg(ESym("block"),
    EBin("let", ESym("a"), ELam(EList(ESym("x"),NULL),ESym("x"))),
    EMsg(ESym("a"),EInt(10),NULL),
    NULL));

  assert(test_eval("block(let(a;fun(x)->x+1); a(10))", "11"));
  test(EMsg(ESym("block"),
    EBin("let", ESym("a"), ELam(EList(ESym("x"),NULL),EBin("add",ESym("x"),EInt(1)))),
    EMsg(ESym("a"),EInt(10),NULL),
    NULL));

  assert(test_eval("block(let(a;fun(x;y)->x+y); a(10;20))", "30"));
  test(EMsg(ESym("block"),
    EBin("let", ESym("a"), ELam(EList(ESym("x"),ESym("y"),NULL),EBin("add",ESym("x"),ESym("y")))),
    EBin("a",EInt(10),EInt(20)),
    NULL));

  assert(test_eval("block(let(x;50);let(a;fun(y)->x+y); a(20))", "70"));
  assert(test_eval("block(x=50;a=fun(y)->x+y; x=10;a(20))", "70"));
  test(parse("block(x=50;a=fun(y)->x+y;x=10; a(20))"));
  test(parse("block((fun(n)->if(lt(n;1);1;2))(10) )"));

  test(parse("block(\"aaa\" )"));
  printf("----------\n");

  assert(test_eval("block(def f(n)=if(n<0;0;n+f(n-1)); f(10))","55"));
  assert(test_eval("\"abc\"","\"abc\""));
  test(parse("\"abc\"+\"d\""));
  assert(test_eval("\"abc\"+\"d\"","\"abcd\""));
  assert(test_eval("\"abc\"==\"d\"","()"));
  assert(test_eval("\"abc\"==\"abc\"","1"));

  test(parse("parse(\"1\")"));
  test(parse("eval(parse(\"1+1\"))"));
  test(parse("read(\"a.txt\")"));
  test(parse("parse(read(\"a.txt\"))"));
  test(parse("eval(parse(read(\"a.txt\")))"));

  to_s(EMsg(ESym("block"),
    EBin("let", ESym("a"), ELam(EList(ESym("x"),NULL),EBin("add",ESym("x"),EInt(1)))),
    EMsg(ESym("a"),EInt(10),NULL),
    NULL));

  run(argv[1]);

  return 0;
}

