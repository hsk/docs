#include "gc.h"
#include <stdio.h>

void _main(){
  enum {FRAME_START, FRAME_SIZE, A, FRAME_END};
  ENTER_FRAME_ENUM();
  frame[A]= gc_alloc_int(((1)+(2)));
  printf("%d\n", frame[A]->ints[0]);
  LEAVE_FRAME();
}

int main() {
  gc_init();
  _main();
  gc_free();
  return 0;
}
