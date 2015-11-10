/*

C だけで使える簡単な完全なスタックマップ付きGCをするためのサンプルプログラム
for osx x86_64

*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

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
  if (head->marked) return;
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

typedef struct Frame {
  void* lbl;
  unsigned short bitmap_size;
  int* bitmap;
} Frame;

typedef struct StackMap {
  unsigned short frame_size;
  void* start;
  void* end;
  Frame* frames;
  struct StackMap* next;
} StackMap;

StackMap* gc_stack_map_list;

void gc_add_stack_map(StackMap* stack_map) {
  if(stack_map->frame_size % 2 != 0) {
    printf("frame size error %d\n", stack_map->frame_size);
    exit(0);
  }
  stack_map->next = gc_stack_map_list;
  gc_stack_map_list = stack_map;
}

void** gc_top_ptr;

void** get_stack_top() {
  void* data;
  void** ptr = &data;
  ptr = (void**)&(ptr[1]);
  return (void**)ptr[0];
}

StackMap* gc_mark_find_stack_map(void* addr) {
  StackMap* stack_map = gc_stack_map_list;
  while (stack_map) {
    if (stack_map->start <= addr && addr <= stack_map->end)
      return stack_map;      
    stack_map = stack_map->next;
  }
  return NULL;
}

Frame* gc_mark_find_frame(StackMap* f, void* addr) {
  Frame* frames = f->frames;
  while (frames) {
    if (frames[1].lbl > addr) return frames;
    
    frames++;
  }
  return NULL;
}

void gc_mark_frame(Frame *frame, Object** objects) {
  for(int i = 0; i < frame->bitmap_size; i++) {
    unsigned int bitmap = frame->bitmap[i];
    int n = i * 32;
    while (bitmap) {
      if(bitmap & 1) {
        gc_mark_object(objects[n]);
      }
      bitmap = bitmap >> 1;
      n++;
    }
  }  
}

void gc_mark(void** ptr, void* addr) {
  do {
    addr = ptr[1];
    ptr = (void**)(ptr[0]);

    StackMap* stack_map = gc_mark_find_stack_map(addr);
    if (!stack_map) continue;

    Frame* frame = gc_mark_find_frame(stack_map, addr);
    if (!frame) continue;

    printf("find frame bitmap_size=%d\n", frame->bitmap_size);

#ifdef __x86_64__
    Object** objects = (Object**)&ptr[-2 - stack_map->frame_size];
#else
    Object** objects = (Object**)&ptr[1 - stack_map->frame_size*2];
#endif

    gc_mark_frame(frame, objects);

  } while(ptr < gc_top_ptr);
}

void gc_sweep() {
  ObjectHeader** object = &heap_list;
  while (*object) {
    if (!(*object)->marked) {
      ObjectHeader* unreached = *object;
      *object = unreached->next;
      printf("free %p\n", &unreached[1]);
      free(unreached);

      heap_num--;
    } else {
      (*object)->marked = 0;
      object = &(*object)->next;
    }
  }
}

void gc_collect() {
  void* data;
  int prev_num = heap_num;
  //printf("gc %p ptr %p\n", &prev_num+1, ptr);// スタックトップ

  gc_mark(get_stack_top(), NULL);
  gc_sweep();

  heap_max = prev_num * 2;

  debug("Collected %d objects, %d remaining.\n", prev_num - heap_num,
         heap_num);
}

void* gc_alloc(ObjectType type, int size) {
  if (heap_num == heap_max) gc_collect();

  ObjectHeader* head = malloc(sizeof(ObjectHeader)+size);

  debug("gc_alloc %p\n", head);
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

  debug("int ptr %p %d\n", data, n);
  *data = n;
  return data;
}

void gc_init() {
  gc_top_ptr = get_stack_top();
  heap_list = NULL;
  heap_num = 0;
  heap_max = 8;
}

void gc_free() {
  gc_collect();
  assert(heap_num==0);
}

void test() {
  void* frame[2];

  static void* start_ptr = &&end; goto *start_ptr; start:;

  printf("frame[1]=%p\n", frame);

  frame[0] = gc_alloc(OBJ_BOXED_ARRAY,sizeof(long)*2);
lbl1:;
  assert(heap_num==1);
  gc_collect();
  assert(heap_num==1);
lbl2:;
  gc_collect();
  assert(heap_num==1);
lbl3:;
  gc_collect();
  assert(heap_num==0);
  return;
end:;
  static int bitmap[] = {1,1};
  static Frame frames[] = {
    {&&lbl1,1,&bitmap[0]},
    {&&lbl2,1,&bitmap[1]},
    {&&lbl3,0,NULL},
    {&&end,0,NULL},
  };
  static StackMap f = {2, (void*)test,&&end, frames, NULL};
  gc_add_stack_map(&f); start_ptr=&&start; goto start;
}

void test2() {
  enum {A,B,SIZE};
  void* frame[SIZE];
  static void* start_ptr = &&end; goto *start_ptr; start:;
  frame[A] = gc_alloc(OBJ_BOXED_ARRAY,sizeof(long)*2);
  frame[B] = gc_alloc(OBJ_BOXED_ARRAY,sizeof(long)*2);
lbl1:;
  assert(heap_num==2);
  gc_collect();
lbl2:;
  assert(heap_num==1);
  gc_collect();
  assert(heap_num==1);
  return;
end:;
  static int bitmap[] = {1};
  static Frame frames[] = {
    {&&lbl1,1,&bitmap[0]},
    {&&lbl2,1,&bitmap[0]},
    {&&end,0,NULL},
  };
  static StackMap f = {SIZE, (void*)test2,&&end, frames, NULL};
  gc_add_stack_map(&f); start_ptr=&&start; goto start;
}

void test3() {
  enum {A, B, unboxed,DUMMY,SIZE};
  Object* frame[SIZE];
  static void* start_ptr = &&end; goto *start_ptr; start:;

  printf("test frame=%p\n", frame);

  // ペア
  frame[A] = gc_alloc_pair();
  frame[A]->pair.fst = gc_alloc_int(10);
  frame[A]->pair.snd = gc_alloc_int(20);
lbl1:;
  assert(heap_num==3);

  // オブジェクト配列
  frame[B] = gc_alloc_boxed_array(2);
  frame[B]->field[0] = gc_alloc_int(30);
  frame[B]->field[1] = gc_alloc_int(40);
lbl2:;
  assert(heap_num==6);

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

  assert(heap_num==7);
  gc_collect();
  assert(heap_num==6);

lbl3:;
  gc_collect();
  printf("%d\n", heap_num);
  assert(heap_num==3);
  return;
end:;
  static int bitmap[] = {1,3,6};
  static Frame frames[] = {
    {&&lbl1,1,&bitmap[A]},
    {&&lbl2,1,&bitmap[B]},
    {&&lbl3,1,&bitmap[unboxed]},
    {&&end,0,NULL},
  };
  static StackMap f = {SIZE, (void*)test3,&&end, frames, NULL};
  gc_add_stack_map(&f); start_ptr=&&start; goto start;
}

Object* test_int(int n) {
  enum {A, DUMMY,SIZE};
  Object* frame[SIZE];
  static void* start_ptr = &&end; goto *start_ptr; start:;
  frame[A] = gc_alloc_int(n);
  Object* a = frame[A];
  gc_collect();
  assert(heap_num==3);
lbl1:;
  return a;
end:;
  static int bitmap[] = {1,1};
  static Frame frames[] = {
    {&&lbl1,1,&bitmap[A]},
    {&&end,1,&bitmap[SIZE]},
  };
  static StackMap f = {SIZE, (void*)test_int,&&end, frames, NULL};
  gc_add_stack_map(&f); start_ptr=&&start; goto *start_ptr;
}

void test_record() {
  enum {A, DUMMY,SIZE};
  Object* frame[SIZE];
  static void* start_ptr = &&end; goto *start_ptr; start:;
  // レコード
  enum {RECORD_SIZE=3,RECORD_BITMAP=BIT(1)|BIT(2)};
  frame[A] = gc_alloc_record(RECORD_SIZE);
  frame[A]->longs[RECORD_SIZE] = RECORD_BITMAP;// レコードのビットマップ(cpuビット数分でアラインする。ビットマップもcpu bit数)
  frame[A]->longs[0] = 10; // undata
  frame[A]->field[1] = gc_alloc_int(20);
  frame[A]->field[2] = test_int(30);

  assert(heap_num==3);
  gc_collect();
  assert(heap_num==3);
lbl1:;
  gc_collect();
  assert(heap_num==3);
  return;
end:;
  static int bitmap[] = {1,1};
  static Frame frames[] = {
    {&&lbl1,1,&bitmap[A]},
    {&&end,1,&bitmap[SIZE]},
  };
  static StackMap f = {SIZE, (void*)test_record,&&end, frames, NULL};
  gc_add_stack_map(&f); start_ptr=&&start; goto *start_ptr;
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
