/*

ARC的、GC

*/

#include <stdio.h>
#include <stdlib.h>
#include <setjmp.h>

#define DEBUG

#ifdef DEBUG
#define debug printf
#else
#define debug noprintf
void noprintf(char* str, ...){}
#endif

typedef enum {
  OBJ_BOXED_ARRAY,
  OBJ_UNBOXED_ARRAY,
  OBJ_PAIR,
  OBJ_RECORD,
} ObjectType;

typedef struct ObjectHeader {
  struct ObjectHeader* next;
  unsigned int size;
  unsigned int level;
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

int heap_level;
Frame* frame_list;
Frame* frame_bottom;
ObjectHeader* heap_list;
int heap_num;
int heap_max;

int heap_find(ObjectHeader* o) {
  ObjectHeader* object = heap_list;
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
  if (!heap_find(head)) return;
  if (head->marked || head->level < heap_level) return;
  long* bitmap;
  head->marked = 1;
  switch(head->type) {
    case OBJ_BOXED_ARRAY:
      size = ((int)head->size) / sizeof(long);
      debug("size=%ld\n",size);
      for(int i = 0; i < size; i++)
          gc_mark_object(object->field[i]);
      break;
    case OBJ_PAIR:
      debug("PAIR\n");
      gc_mark_object(object->pair.fst);
      gc_mark_object(object->pair.snd);
      break;
    case OBJ_UNBOXED_ARRAY:
      break;
    case OBJ_RECORD:
      size = ((int)head->size) / sizeof(long);
      debug("RECORD size=%ld\n", size);
      bitmap = &object->longs[size];
      debug("size=%ld\n",size);
      for(int i = 0; i < size; i++) {
        if(bitmap[i/sizeof(long)] & (1 << (i % sizeof(long))))
          gc_mark_object(object->field[i]);
        else
          debug("skip %d\n", i);
      }
      break;
  }
}

void gc_mark() {
  Frame* frame = frame_list;
  while(frame != frame_bottom) {
    for(int i = 0; i < frame->frame_size; i++)
      gc_mark_object(frame->frame_data[i]);
    frame = frame->frame_prev;
  }
}

void gc_sweep(int level) {
  ObjectHeader** object = &heap_list;
  while (*object) {
  	if((*object)->level < heap_level) break;
    if (!(*object)->marked) {
      ObjectHeader* unreached = *object;
      *object = unreached->next;
      free(unreached);

      heap_num--;
    } else {
      if(level) {
        printf("level change\n");
        printf("level change %d -> %d\n", (*object)->level, level);
      	(*object)->level = level;
      }
      (*object)->marked = 0;
      object = &(*object)->next;
    }
  }
}

void gc_end_world(Object* data) {
  int prev_num = heap_num;
  gc_mark_object(data);
  gc_sweep(heap_level-1);

  heap_max = prev_num * 2;

  debug("Collected %d objects, %d remaining.\n", prev_num - heap_num,
         heap_num);
}

void gc_pipe(Object* data) {
  int prev_num = heap_num;
  gc_mark_object(data);
  gc_sweep(0);

  heap_max = prev_num * 2;

  debug("Collected %d objects, %d remaining.\n", prev_num - heap_num,
         heap_num);
}

Object* gc_new_world(Object*(*f)(void*data), void* data) {
	heap_level++;
	Frame* frame_bottom_temp = frame_bottom;
	Object* rc = f(data);
	frame_bottom = frame_bottom_temp;
	heap_level--;
	return rc;
}

#define NEW_WORLD(tmp) \
  heap_level++; \
  Frame* tmp = frame_bottom;

#define END_WORLD(tmp,root) \
  gc_end_world(root); \
  frame_bottom = tmp; \
  heap_level--;

void gc_collect() {
  int prev_num = heap_num;

  gc_mark();
  gc_sweep(0);

  heap_max = prev_num * 2;

  debug("Collected %d objects, %d remaining.\n", prev_num - heap_num,
         heap_num);
}

void* gc_alloc(ObjectType type, int size) {
  if (heap_num == heap_max) gc_collect();

  ObjectHeader* head = (ObjectHeader*)malloc(sizeof(ObjectHeader)+size);

  debug("gc_alloc %p\n", head);
  head->level = heap_level;
  head->type = type;
  head->next = heap_list;
  heap_list = head;
  head->marked = 0;
  head->size=size;
  heap_num++;

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

#define ENTER_FRAME(SIZE) \
  Object* frame[SIZE+2]; \
  ((Frame*)frame)->frame_prev = frame_list; \
  ((Frame*)frame)->frame_size = SIZE; \
  frame_list = (Frame*)frame; \

#define ENTER_FRAME_ENUM() ENTER_FRAME((FRAME_END-2))

#define LEAVE_FRAME() \
  frame_list = frame_list->frame_prev;

void gc_init() {
  heap_level = 1;
  frame_list = NULL;
  frame_bottom = NULL;
  heap_list = NULL;
  heap_num = 0;
  heap_max = 8;
}

void gc_free() {
  frame_list = NULL;
  gc_collect();
}

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

Object* test_new_world2(Object* data) {
  enum {FRAME_START, FRAME_SIZE, A, B, C, FRAME_END};
  ENTER_FRAME_ENUM();

  // レコード
  enum {RECORD_SIZE=3,RECORD_BITMAP=BIT(1)|BIT(2)};
  frame[A] = gc_alloc_record(RECORD_SIZE); // 4
  frame[A]->longs[0] = 100; // undata
  frame[A]->field[1] = gc_alloc_int(200); // 5
  frame[A]->field[2] = data;
  frame[A]->longs[RECORD_SIZE] = RECORD_BITMAP;// レコードのビットマップ(cpuビット数分でアラインする。ビットマップもcpu bit数)

  frame[B] = gc_alloc_int(3); // 6
  frame[C] = gc_alloc_int(5); // 7
  gc_collect();
  gc_collect();

  LEAVE_FRAME();
  return frame[A];
}

void test_new_world() {
  enum {FRAME_START, FRAME_SIZE, A, B, FRAME_END};
  ENTER_FRAME_ENUM();

  // レコード
  enum {RECORD_SIZE=3,RECORD_BITMAP=BIT(1)|BIT(2)};
  frame[A] = gc_alloc_record(RECORD_SIZE); // 1
  frame[A]->longs[0] = 10; // undata
  frame[A]->field[1] = gc_alloc_int(20); // 2
  frame[A]->field[2] = test_int(30); // 3
  frame[A]->longs[RECORD_SIZE] = RECORD_BITMAP;// レコードのビットマップ(cpuビット数分でアラインする。ビットマップもcpu bit数)

  NEW_WORLD(frame_tmp1);

    NEW_WORLD(frame_tmp2);
    frame[B] = test_new_world2(frame[A]);
    END_WORLD(frame_tmp2, frame[B]);// 6と7が消える。

  printf("level change check.........\n");
  END_WORLD(frame_tmp1,frame[B]);// 6と7が消える。
  printf("level change check.........\n");
  gc_collect();
  LEAVE_FRAME();
}

void test_pipes1() {
  enum {FRAME_START, FRAME_SIZE, A, B, C, FRAME_END};
  ENTER_FRAME_ENUM();

  // レコード
  enum {RECORD_SIZE=3,RECORD_BITMAP=BIT(1)|BIT(2)};
  frame[A] = gc_alloc_record(RECORD_SIZE); // 1
  frame[A]->longs[0] = 10; // undata
  frame[A]->field[1] = gc_alloc_int(20); // 2
  frame[A]->field[2] = test_int(30); // 3
  frame[A]->longs[RECORD_SIZE] = RECORD_BITMAP;// レコードのビットマップ(cpuビット数分でアラインする。ビットマップもcpu bit数)

  NEW_WORLD(frame_tmp1);

    frame[B] = test_new_world2(frame[A]);
    frame[B] = test_new_world2(frame[B]);
    frame[B] = test_new_world2(frame[B]);

  printf("level change check.........\n");
  END_WORLD(frame_tmp1,frame[B]);
  printf("level change check.........\n");
  gc_collect();
  LEAVE_FRAME();
}

void test_pipes2() {
  enum {FRAME_START, FRAME_SIZE, A, B, C, FRAME_END};
  ENTER_FRAME_ENUM();

  // レコード
  enum {RECORD_SIZE=3,RECORD_BITMAP=BIT(1)|BIT(2)};
  frame[A] = gc_alloc_record(RECORD_SIZE); // 1
  frame[A]->longs[0] = 10; // undata
  frame[A]->field[1] = gc_alloc_int(20); // 2
  frame[A]->field[2] = test_int(30); // 3
  frame[A]->longs[RECORD_SIZE] = RECORD_BITMAP;// レコードのビットマップ(cpuビット数分でアラインする。ビットマップもcpu bit数)

  NEW_WORLD(frame_tmp1);

    frame[B] = test_new_world2(frame[A]);
    gc_pipe(frame[B]);
    frame[B] = test_new_world2(frame[B]);
    gc_pipe(frame[B]);
    frame[B] = test_new_world2(frame[B]);

  printf("level change check.........\n");
  END_WORLD(frame_tmp1,frame[B]);
  printf("level change check.........\n");
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

  printf("---\n");
  gc_init();
  test_new_world();
  gc_free();

  printf("sizeof type %ld header %ld\n", sizeof(ObjectType), sizeof(ObjectHeader));
  return 0;
}
