#include "gc.h"
#include <stdio.h>

void _main(){
  enum {FRAME_START, FRAME_SIZE, tmp2,tmp1, FRAME_END};
  ENTER_FRAME_ENUM();
  frame[tmp1]=gc_alloc_int((1));
  frame[tmp2]=gc_alloc_int((2));
  printf("%d\n", (frame[tmp1]->intv+frame[tmp2]->intv));
  LEAVE_FRAME();
}

int main() {
  gc_init();
  _main();
  gc_free();
  return 0;
}
