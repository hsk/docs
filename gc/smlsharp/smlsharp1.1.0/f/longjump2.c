#include <stdio.h>
#include <setjmp.h>
#include <unistd.h>
#include <stdlib.h>

typedef struct exn_stack {
  jmp_buf jb;
  struct exn_stack *next;
} exn_stack;

exn_stack *exn;

#define try(r) int r;{ \
  exn_stack *st = (exn_stack*)malloc(sizeof(exn_stack)); \
  st->next = exn; \
  exn = st; \
  r = setjmp(st->jb); \
} if(r == 0) \

#define catch catchf(); } else { catchf();

void catchf() {
  if(exn == NULL) return;
  exn_stack *st = exn;
  exn = exn->next;
  free(st);
}
void throw(int r) {
  longjmp(exn->jb, r);
}

int test1() {
  try(e){
    printf("test1\n");
    throw(112);
  catch
    printf("catch %d\n", e);
  }
  throw(222);
  return 0;
}

int main() {
  int x = 777;
  try(e) {
    printf( "before: %d\n", x );
    x = 666;
    test1();
    printf("aaa");
  catch
    printf( "after : %d %d\n", x, e );
  }
  return 0;
}

