#include "gc.h"

void get_action() {
  printf("HTTP/1.0 200 OK\n");
  printf("text/html\n");
  printf("Cache-Control: max-age=0\n\n");

  printf("<html>\n");
  printf("<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />\n");
  printf("<body>\n");
  printf("<h1>test gc</h1>");

  printf("start test\n");
  enum {frame_START, frame_SIZE, VM1, VM2, A, B, C, frame_END};
  ENTER_FRAME_ENUM(frame);
  frame[A] = gc_alloc_int(1);

  printf("test\n");

  assert(vm->heap_num==1);
  VM* tmp_vm = vm;
  frame[VM1] = (Object*)vm_new();// 世界を作る
  frame[VM2] = (Object*)vm_new();// 世界を作る
  assert(vm->heap_num==3);

  vm = (VM*)frame[VM1];// 世界を移動
    assert(vm->heap_num==0);
    vm->record = gc_alloc_int(frame[A]->intv);// 計算する
    printf("vm = %d\n", vm->record->intv);
    assert(vm->heap_num==1);

  vm = (VM*)frame[VM2];// 世界を移動
    assert(vm->heap_num==0);
    vm->record = gc_alloc_int(frame[A]->intv);// 計算する
    assert(vm->heap_num==1);

  vm = tmp_vm;// 元に戻る
  assert(vm->heap_num==3);

    printf("vm = %d\n", ((VM*)frame[VM1])->record->intv);

  frame[B] = vm_get_record((VM*)frame[VM1]);// コピーとる
  frame[C] = vm_get_record((VM*)frame[VM2]);// コピーとる
  assert(vm->heap_num==5);
  frame[VM1] = NULL; // 世界を消す
  frame[VM2] = NULL; // 世界を消す
  //gc_collect();
  //assert(vm->heap_num==3);
  printf("test888 %d %d\n", frame[A]->intv, frame[B]->intv+frame[C]->intv);
  LEAVE_FRAME(frame);
  printf("<hr/>\n");
  printf("<a href=\"javascript:history.back()\">back</a>\n");
  printf("</body>\n");
  printf("</html>\n");
}
