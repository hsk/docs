/*

C だけで使える簡単な完全なGCをするためのサンプルプログラム

*/

#include "gc.h"

void test() {
  void* frame[2+1];
  frame[0] = (void*)frame_list;
  frame[1] = (void*)1;
  frame_list = (Frame*)frame;
  frame[2] = gc_alloc(OBJ_BOXED_ARRAY,sizeof(long)*2);
  gc_collect();
  frame_list = frame_list->frame_prev;
}

void test2() {
  ENTER_FRAME(1);
  frame[2] = gc_alloc(OBJ_BOXED_ARRAY,sizeof(long)*2);
  gc_collect();
  LEAVE_FRAME();
}

void test3() {
  enum {FRAME_START, FRAME_SIZE, A, B, unboxed, FRAME_END};
  ENTER_FRAME_ENUM();

  // ペア
  frame[A] = gc_alloc_pair();
  frame[A]->pair.fst = gc_alloc_int(10);
  frame[A]->pair.snd = gc_alloc_int(20);

  // オブジェクト配列
  frame[B] = gc_alloc_boxed_array(2);
  frame[B]->field[0] = gc_alloc_int(30);
  frame[B]->field[1] = gc_alloc_int(40);

  // int配列
  frame[unboxed] = gc_alloc_unboxed_array(sizeof(int)*2);
  frame[unboxed]->ints[0] = 50;
  frame[unboxed]->ints[1] = 60;

  printf("data1 = %p %d\n", frame[A]->pair.fst, frame[A]->pair.fst->intv);
  printf("data2 = %p %d\n", frame[A]->pair.snd, frame[A]->pair.snd->intv);

  printf("data3 = %p %d\n", frame[B]->field[0], frame[B]->field[0]->intv);
  printf("data4 = %p %d\n", frame[B]->field[1], frame[B]->field[1]->intv);

  printf("data5 = %p %d\n", &frame[unboxed]->ints[0], frame[unboxed]->ints[0]);
  printf("data6 = %p %d\n", &frame[unboxed]->ints[1], frame[unboxed]->ints[1]);
  gc_collect();
  LEAVE_FRAME();
}

Object* test_int(int n) {
  enum {FRAME_START, FRAME_SIZE, A, FRAME_END};
  ENTER_FRAME_ENUM();
  frame[A] = gc_alloc_int(n);
  LEAVE_FRAME();
  return frame[A];
}

void test_record() {
  enum {FRAME_START, FRAME_SIZE, A, FRAME_END};
  ENTER_FRAME_ENUM();

  // レコード
  enum {RECORD_SIZE=3,RECORD_BITMAP=BIT(1)|BIT(2)};
  frame[A] = gc_alloc_record(RECORD_SIZE);
  frame[A]->longs[0] = 10; // undata
  frame[A]->field[1] = gc_alloc_int(20);
  frame[A]->field[2] = test_int(30);
  frame[A]->longs[RECORD_SIZE] = RECORD_BITMAP;// レコードのビットマップ(cpuビット数分でアラインする。ビットマップもcpu bit数)

  gc_collect();
  LEAVE_FRAME();
}

int main() {
  gc_init();
  test();
  gc_free();
  printf("---\n");
  gc_init();
  test2();
  gc_free();

  printf("---\n");
  gc_init();
  test3();
  gc_free();

  printf("---\n");
  gc_init();
  test_record();
  gc_free();
  printf("sizeof type %ld header %ld\n", sizeof(ObjectType), sizeof(ObjectHeader));
  return 0;
}
