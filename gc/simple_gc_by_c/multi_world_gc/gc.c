/*

multi world gc

*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <memory.h>

#define DEBUG
//#define DEBUG2
void noprintf(char* str, ...){}
#ifdef DEBUG
#define debug printf
#else
#define debug noprintf
#endif
#ifdef DEBUG2
#define debug2 printf
#else
#define debug2 noprintf
#endif

typedef enum {
  OBJ_BOXED_ARRAY,
  OBJ_UNBOXED_ARRAY,
  OBJ_PAIR,
  OBJ_RECORD,
  OBJ_VM,
} ObjectType;

struct VM;

typedef struct ObjectHeader {
  struct ObjectHeader* next;
  unsigned int size;
  struct VM* vm;
  unsigned char type;
  unsigned char marked;
} ObjectHeader;

typedef union Object {
  struct {
    union Object *fst;
    union Object *snd;
  }pair;
  union Object* field[0];

  char charv;
  short shortv;
  int intv;
  long longv;
  long long longlongv;
  char chars[0];
  short shorts[0];
  int ints[0];
  long longs[0];
  long long longlongs[0];

  unsigned char ucharv;
  unsigned short ushortv;
  unsigned int uintv;
  unsigned long ulongv;
  unsigned long long ulonglongv;
  unsigned char uchars[0];
  unsigned short ushorts[0];
  unsigned int uints[0];
  unsigned long ulongs[0];
  unsigned long long ulonglongs[0];
} Object;

typedef struct Frame {
  struct Frame* frame_prev;
  unsigned long frame_size;
  Object* frame_data[0];
} Frame;

typedef struct VM {
  Object* record;
  ObjectHeader* heap_list;
  long heap_num;
  long heap_max;
} VM;

VM* vm;
Frame* frame_list;
Frame* frame_bottom;

int heap_find(ObjectHeader* o) {
  ObjectHeader* object = vm->heap_list;
  while (object) {
    if(object == o) return 1;
    object = object->next;
  }
  return 0;
}

void gc_mark_object(Object* object) {
  ObjectHeader* head = &((ObjectHeader*)object)[-1];
  debug("mark %p\n",head);
  long size;
  if (!heap_find(head)) {
    debug2("unfind\n");
    return;
  }
  debug2("find\n");
  if (head->marked || head->vm != vm) return;
  long* bitmap;
  head->marked = 1;
  switch(head->type) {
    case OBJ_BOXED_ARRAY:
      debug("BOXED_ARRAY\n");
      size = ((int)head->size) / sizeof(long);
      debug2("size=%ld\n",size);
      for(int i = 0; i < size; i++) 
          gc_mark_object(object->field[i]);
      debug2("END\n");
      break;
    case OBJ_PAIR:
      debug("PAIR\n");
      gc_mark_object(object->pair.fst);
      gc_mark_object(object->pair.snd);
      break;
    case OBJ_UNBOXED_ARRAY:
      debug("UNBOXED ARRAY\n");
      break;
    case OBJ_VM:
      debug("VM\n");
      break;
    case OBJ_RECORD:
      size = ((int)head->size) / sizeof(long);
      debug("RECORD size=%ld\n", size);
      bitmap = &object->longs[size];
      debug2("size=%ld\n",size);
      for(int i = 0; i < size; i++) {
        if(bitmap[i/sizeof(long)] & (1 << (i % sizeof(long))))
          gc_mark_object(object->field[i]);
        else {
          debug2("skip %d\n", i);
        }
      }
      break;
  }
}

void gc_mark() {
  Frame* frame = frame_list;
  while(frame != frame_bottom) {
    debug2("gc mark %p size %ld\n", frame, frame->frame_size);
    for(int i = 0; i < frame->frame_size; i++) {
      gc_mark_object(frame->frame_data[i]);
      debug2("done\n");
    }
    debug2("next %p\n", frame);
    debug2("next prev %p %p\n", frame->frame_prev, frame_bottom);
    frame = frame->frame_prev;
  }
  debug2("gc mark done\n");
}

void vm_finalize(VM* _vm);

void gc_sweep(VM* vm) {
  ObjectHeader** object = &(vm->heap_list);
  debug2("object =%p\n", object);
  while (*object) {
    if((*object)->vm != vm) break;
    if (!(*object)->marked) {
      ObjectHeader* unreached = *object;
      *object = unreached->next;

      if(unreached->type == OBJ_VM) vm_finalize((VM*)&unreached[1]);
      
      free(unreached);

      vm->heap_num--;
    } else {
      if(vm) {
        debug2("id change\n");
        debug2("id change %p -> %p\n", (*object)->vm, vm);
        (*object)->vm = vm;
      }
      (*object)->marked = 0;
      object = &(*object)->next;
    }
  }
}

void gc_collect() {
  long prev_num = vm->heap_num;

  debug2("gc mark\n");
  gc_mark();
  debug2("gc sweep\n");
  gc_sweep(vm);

  vm->heap_max = prev_num * 2;

  debug("Collected %ld objects, %ld remaining.\n", prev_num - vm->heap_num,
         vm->heap_num);
}

void gc_collect_end_world(Object* data, VM* vm) {
  long prev_num = vm->heap_num;
  debug2("gc mark\n");
  gc_mark_object(data);
  debug2("gc sweep\n");
  gc_sweep(vm);

  vm->heap_max = prev_num * 2;

  debug("Collected %ld objects, %ld remaining.\n", prev_num - vm->heap_num,
         vm->heap_num);
}

void gc_collect_pipe(Object* data) {
  long prev_num = vm->heap_num;
  gc_mark_object(data);
  gc_sweep(vm);

  vm->heap_max = prev_num * 2;

  debug("Collected %ld objects, %ld remaining.\n", prev_num - vm->heap_num,
         vm->heap_num);
}

#define NEW_WORLD(frame) \
  VM* frame##_vm = vm; \
  vm = vm_new(); \
  Frame* frame##_tmp_bottom = frame_bottom; \

#define END_WORLD(frame,root) \
  frame_bottom = frame##_tmp_bottom; \
  vm = frame##_vm; \
  gc_collect_end_world(root,frame##_vm); \

void* gc_alloc(ObjectType type, int size) {
  debug2("gc alloc\n");
  debug2("vm=%p\n",vm);
  if (vm->heap_num == vm->heap_max) gc_collect();

  ObjectHeader* head = (ObjectHeader*)malloc(sizeof(ObjectHeader)+size);

  debug("gc_alloc %p\n", head);
  head->vm = vm;
  head->type = type;
  head->next = vm->heap_list;
  vm->heap_list = head;
  head->marked = 0;
  head->size=size;
  vm->heap_num++;

  return &head[1];
}

#define gc_alloc_pair() (gc_alloc(OBJ_PAIR, sizeof(Object*)*2))
#define gc_alloc_boxed_array(size) (gc_alloc(OBJ_BOXED_ARRAY, sizeof(Object*)*size))
#define gc_alloc_unboxed_array(size) (gc_alloc(OBJ_UNBOXED_ARRAY, size))
#define gc_alloc_record(n) (gc_alloc(OBJ_RECORD, sizeof(Object*)*n+RECORD_BITMAP_NUM(n)))
#define RECORD_BITMAP_NUM(n) (((n)+sizeof(long)*8-1) / (sizeof(long)*8) )
#define BIT(n) (1 << n)

void* gc_alloc_int(int n) {
  int* data = gc_alloc(OBJ_UNBOXED_ARRAY, sizeof(int)*1);

  debug("int ptr %p\n", data);
  *data = n;
  return data;
}


Object* gc_copy(Object* object) {
  ObjectHeader* head = &((ObjectHeader*)object)[-1];
  debug("gc copy %p\n",head);
  long size;
  if (!heap_find(head)) return NULL;
  long* bitmap;
  Object* new;
  switch(head->type) {
    case OBJ_BOXED_ARRAY:
      new = gc_alloc(head->type, head->size);
      size = ((int)head->size) / sizeof(long);
      debug("size=%ld\n",size);
      for(int i = 0; i < size; i++)
          new->field[i] = gc_copy(object->field[i]);
      break;
    case OBJ_PAIR:
      new = gc_alloc(head->type, head->size);
      new->pair.fst = gc_copy(object->pair.fst);
      new->pair.snd = gc_copy(object->pair.snd);
      break;
    case OBJ_UNBOXED_ARRAY:
    case OBJ_VM:
      new = gc_alloc(head->type, head->size);
      memcpy(object, new, head->size);
      break;
    case OBJ_RECORD:
      size = ((int)head->size) / sizeof(long);
      new = gc_alloc(head->type, head->size);
      memcpy(object, new, head->size);
      bitmap = &object->longs[size];
      for(int i = 0; i < size; i++) {
        if(bitmap[i/sizeof(long)] & (1 << (i % sizeof(long))))
          new->field[i] = gc_copy(object->field[i]);
      }
      break;
  }
  return new;
}

#define ENTER_FRAME(frame, SIZE) \
  Object* frame[SIZE+2]; \
  ((Frame*)frame)->frame_prev = frame_list; \
  ((Frame*)frame)->frame_size = SIZE; \
  frame_list = (Frame*)frame; \

#define ENTER_FRAME_ENUM(frame) ENTER_FRAME(frame, (frame##_END-2))

#define LEAVE_FRAME(frame) \
  frame_list = frame_list->frame_prev;

Object* vm_get_record(VM* _vm) {
  VM* tmp_vm = vm;
  vm = _vm;
  Object* record = gc_copy(vm->record);
  vm = tmp_vm;
  return record;
}

void vm_finalize(VM* _vm) {
  VM* tmp_vm = vm;

  vm = _vm;
  gc_collect();
  vm = tmp_vm;
}

void vm_end(Object* o, VM* vm) {
  gc_collect_end_world(o, vm);
}

Object* vm_end_record(VM* vm) {
  gc_collect_end_world(vm->record, vm);
  return vm->record;
}

VM* vm_new() {
  debug("vm_new\n");
  VM* vm = gc_alloc(OBJ_VM, sizeof(VM));
  debug("gc alloc ok\n");
  vm->record = NULL;
  vm->heap_list = NULL;
  vm->heap_num = 0;
  vm->heap_max = 8;
  return vm;
}

void gc_init() {
  vm = malloc(sizeof(VM));
  vm->record = NULL;
  vm->heap_list = NULL;
  vm->heap_num = 0;
  vm->heap_max = 8;
  frame_list = NULL;
  frame_bottom = NULL;
}

void gc_free() {
  gc_collect();
  assert(vm->heap_num==0);
  free(vm);
}

void test() {
  void* frame[2+1];
  frame[0] = (void*)frame_list;
  frame[1] = (void*)1;
  frame_list = (Frame*)frame;
  frame[2] = gc_alloc(OBJ_BOXED_ARRAY,sizeof(long)*2);

  assert(vm->heap_num==1);
  gc_collect();
  assert(vm->heap_num==1);

  frame_list = frame_list->frame_prev;
}

void test2() {
  ENTER_FRAME(frame, 1);
  frame[2] = gc_alloc(OBJ_BOXED_ARRAY,sizeof(long)*2);
  assert(vm->heap_num==1);
  gc_collect();
  assert(vm->heap_num==1);
  LEAVE_FRAME(frame);
}

void test3() {
  enum {frame_START, frame_SIZE, A, B, unboxed, frame_END};
  ENTER_FRAME_ENUM(frame);

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
  assert(vm->heap_num==7);
  gc_collect();
  assert(vm->heap_num==7);
  LEAVE_FRAME(frame);
}

Object* test_int(int n) {
  enum {frame_START, frame_SIZE, A, frame_END};
  ENTER_FRAME_ENUM(frame);
  frame[A] = gc_alloc_int(n+10);
  gc_collect();
  LEAVE_FRAME(frame);
  return frame[A];
}

void test_record() {
  enum {frame_START, frame_SIZE, A, frame_END};
  ENTER_FRAME_ENUM(frame);

  // レコード
  enum {RECORD_SIZE=3,RECORD_BITMAP=BIT(1)|BIT(2)};
  frame[A] = gc_alloc_record(RECORD_SIZE);
  frame[A]->longs[RECORD_SIZE] = RECORD_BITMAP;// レコードのビットマップ(cpuビット数分でアラインする。ビットマップもcpu bit数)
  frame[A]->longs[0] = 10; // undata
  frame[A]->field[1] = gc_alloc_int(20);
  frame[A]->field[2] = test_int(30);

  assert(vm->heap_num==3);
  gc_collect();
  assert(vm->heap_num==3);
  LEAVE_FRAME(frame);
}

Object* test_new_world2(Object* data) {
  enum {frame_START, frame_SIZE, A, B, C, frame_END};
  ENTER_FRAME_ENUM(frame);

  // レコード
  enum {RECORD_SIZE=3,RECORD_BITMAP=BIT(1)|BIT(2)};
  frame[A] = gc_alloc_record(RECORD_SIZE); // 4
  frame[A]->longs[RECORD_SIZE] = RECORD_BITMAP;// レコードのビットマップ(cpuビット数分でアラインする。ビットマップもcpu bit数)
  frame[A]->longs[0] = 100; // undata
  frame[A]->field[1] = gc_alloc_int(200); // 5
  frame[A]->field[2] = data;

  frame[B] = gc_alloc_int(3); // 6
  frame[C] = gc_alloc_int(5); // 7

  gc_collect();
  gc_collect();

  LEAVE_FRAME(frame);
  return frame[A];
}

void test_new_world() {
  enum {frame_START, frame_SIZE, A, B, frame_END};
  ENTER_FRAME_ENUM(frame);

  // レコード
  frame[A] = gc_alloc_int(1); // 1

  assert(vm->heap_num==1);

  NEW_WORLD(frame1);
    enum {frame1_START, frame1_SIZE, C, frame1_END};
    ENTER_FRAME_ENUM(frame1);

    assert(vm->heap_num==0);

    NEW_WORLD(frame2);
      enum {frame2_START, frame2_SIZE, D, frame2_END};
      ENTER_FRAME_ENUM(frame2);

      assert(vm->heap_num==0);
      frame1[C] = test_new_world2(frame[A]);
      assert(vm->heap_num==4);

      LEAVE_FRAME(frame2);
    END_WORLD(frame2, frame1[C]);// 6と7が消える。
    assert(vm->heap_num==0);
    frame[B] = frame1[C];
    LEAVE_FRAME(frame1);
  printf("id change check.........\n");
  END_WORLD(frame1,frame[B]);
  assert(vm->heap_num==0);
  printf("id change check.........\n");
  gc_collect();
  assert(vm->heap_num==0);
  LEAVE_FRAME(frame);
}

void test_pipes1() {
  enum {frame_START, frame_SIZE, A, B, C, frame_END};
  ENTER_FRAME_ENUM(frame);

  frame[A] = gc_alloc_int(1); // 1

  assert(vm->heap_num==1);
  NEW_WORLD(frame1);
    assert(vm->heap_num==0);
    frame[B] = test_new_world2(frame[A]);
    assert(vm->heap_num==4);
    frame[B] = test_new_world2(frame[B]);
    assert(vm->heap_num==8);
    frame[B] = test_new_world2(frame[B]);
    assert(vm->heap_num==12);

  printf("id change check.........\n");
  END_WORLD(frame1,frame[B]);
  assert(vm->heap_num==0);
  printf("id change check.........\n");
  gc_collect();
  assert(vm->heap_num==0);
  LEAVE_FRAME(frame);
}

void test_pipes2() {
  enum {frame_START, frame_SIZE, A, B, C, frame_END};
  ENTER_FRAME_ENUM(frame);

  frame[A] = gc_alloc_int(1); // 1

  assert(vm->heap_num==1);
  NEW_WORLD(frame1);
    assert(vm->heap_num==0);
    frame[B] = test_new_world2(frame[A]);
    assert(vm->heap_num==4);
    gc_collect_pipe(frame[B]);
    assert(vm->heap_num==4);
    frame[B] = test_new_world2(frame[B]);
    assert(vm->heap_num==8);
    gc_collect_pipe(frame[B]);
    assert(vm->heap_num==4);
    frame[B] = test_new_world2(frame[B]);
    assert(vm->heap_num==8);

  printf("id change check.........\n");
  END_WORLD(frame1, frame[B]);
  assert(vm->heap_num==0);
  printf("id change check.........\n");
  gc_collect();
  assert(vm->heap_num==0);
  LEAVE_FRAME(frame);
}

void test_multi_world() {
  enum {frame_START, frame_SIZE, A, B, C, D, frame_END};
  ENTER_FRAME_ENUM(frame);

  frame[A] = gc_alloc_int(1);

  NEW_WORLD(frame1);
    frame[B] = test_int(frame[A]->intv);
  END_WORLD(frame1, frame[B]);

  NEW_WORLD(frame2);
    frame[C] = test_int(frame[B]->intv);
  END_WORLD(frame2, frame[C]);

  NEW_WORLD(frame3);
    frame[D] = test_int(frame[C]->intv);
  END_WORLD(frame3, frame[D]);

  printf("id change check.........\n");
  gc_collect();
  LEAVE_FRAME(frame);
}

void test_multi_world2() {
  enum {frame_START, frame_SIZE, VM1,VM2,A, B, C, frame_END};
  ENTER_FRAME_ENUM(frame);
  frame[A] = gc_alloc_int(1);

  VM* tmp_vm = vm;
  frame[VM1] = (Object*)vm_new();
  frame[VM2] = (Object*)vm_new();
  vm = (VM*)frame[VM1];
    vm->record = test_int(frame[A]->intv);
  vm = (VM*)frame[VM2];
    vm->record = test_int(frame[A]->intv);
  vm = tmp_vm;

  frame[B] = vm_get_record((VM*)frame[VM1]);// コピーとる
  frame[C] = vm_get_record((VM*)frame[VM2]);// コピーとる

  printf("id change check.........\n");
  gc_collect();
  LEAVE_FRAME(frame);
}

int main() {

  printf("------------- test\n");
  gc_init();
  test();
  gc_free();

  printf("------------- test2\n");
  gc_init();
  test2();
  gc_free();

  printf("------------- test3\n");
  gc_init();
  test3();
  gc_free();

  printf("------------- test4\n");
  gc_init();
  test_record();
  gc_free();

  printf("------------- test new world\n");
  gc_init();
  test_new_world();
  gc_free();

  printf("------------- test multi world\n");
  gc_init();
  test_multi_world();
  gc_free();

  printf("------------- test multi world2\n");
  gc_init();
  test_multi_world2();
  gc_free();

  return 0;
}
