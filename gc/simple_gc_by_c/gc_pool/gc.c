/*

C だけで使える簡単な完全なGCをするためのサンプルプログラム

*/
//#define DEBUG

//#define NOGC
#define NDEBUG

#ifdef NOGC
#define NDEBUG
#endif

#ifdef NDEBUG
  #define gc_collect1() 
#else
  #define gc_collect1 gc_collect
#endif

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>

#ifdef __cplusplus
#include <set>
using namespace std;
#endif



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
#ifndef __cplusplus
  struct ObjectHeader* next;
#endif
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

typedef struct Frame {
  struct Frame* frame_prev;
  short frame_size;
  short frame_pos;
  Object* frame_data[0];
} Frame;

Frame* frame_list;

#ifdef __cplusplus
    set<ObjectHeader*> heap_list;       // ローカル変数として、mp を生成
#else
  ObjectHeader* heap_list;
#endif
int heap_num;
int heap_max;

int heap_find(ObjectHeader* o) {
#ifdef __cplusplus
    return heap_list.find(o) != heap_list.end();
#else
  ObjectHeader* object = heap_list;
  while (object) {
    if(object == o) return 1;
    object = object->next;
  }
  return 0;
#endif
}

void gc_mark_object(Object* object) {
  ObjectHeader* head = &((ObjectHeader*)object)[-1];
  //debug("mark %p\n",head);
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

void gc_mark() {
  Frame* frame = frame_list;
  while(frame) {
    int pos = frame->frame_size;
    if(pos > frame->frame_pos) pos = frame->frame_pos;
    for(int i = 0; i < pos; i++)
      gc_mark_object(frame->frame_data[i]);
    frame = frame->frame_prev;
  }
}

void gc_sweep() {

#ifdef __cplusplus
  for (set<ObjectHeader*>::iterator object = heap_list.begin(); object != heap_list.end();) {
    if (!(*object)->marked) {
      ObjectHeader* unreached = *object;
      heap_list.erase(object++);
      //debug("sweep %p\n", unreached);
      free(unreached);
      heap_num--;
    } else {

      (*object)->marked = 0;
      object++;
    }
  }
#else
  ObjectHeader** object = &heap_list;
  while (*object) {
    if (!(*object)->marked) {
      ObjectHeader* unreached = *object;
      *object = unreached->next;
      //debug("sweep %p\n", unreached);
      free(unreached);

      heap_num--;
    } else {
      (*object)->marked = 0;
      object = &(*object)->next;
    }
  }
#endif
}

void gc_collect() {
  #ifdef NOGC
    return;
  #endif
  int prev_num = heap_num;

  gc_mark();
  gc_sweep();

  heap_max = prev_num * 2;

  debug("Collected %d objects, %d remaining.\n", prev_num - heap_num,
         heap_num);
}

Object* gc_new0(ObjectType type, int size) {

  ObjectHeader* head = (ObjectHeader*)malloc(sizeof(ObjectHeader)+size);
  //debug("gc_new %p\n", head);
  head->type = type;
#ifdef __cplusplus
  heap_list.insert(head);
#else
  head->next = heap_list;
  heap_list = head;
#endif
  head->marked = 0;
  head->size=size;
  heap_num++;

  return (Object*)&head[1];
}

#define gc_oarray0(size) (gc_new0(OBJ_BOXED_ARRAY, sizeof(Object*)*size))

Object* gc_add_pool(Frame* frame_list, Object* head) {

  int frame_pos = frame_list->frame_pos;
  int frame_size = frame_list->frame_size-1;
  Object** frame_data = frame_list->frame_data;
  
  if (frame_pos < frame_size) {// フレームサイズ内ならそのまま使用する
    frame_data += frame_list->frame_pos;
  } else {// 足りなくなったら追加領域を使う
    frame_data += frame_size;
    frame_pos -= frame_size;
    // 追加時
    if (frame_pos == 0) {
      *frame_data = gc_oarray0(2);
    } else {
      int add_size = ((ObjectHeader*)*frame_data)[-1].size/sizeof(void*);
      if(add_size==frame_pos) {
        Object* frame_data2 = gc_oarray0(add_size*2);
        memcpy((void*)frame_data2, (void*)*frame_data, sizeof(Object*)*add_size);
        *frame_data = frame_data2;
      }
    }
    frame_data = (Object**)(*frame_data);
    frame_data += frame_pos;
  }
  *frame_data = head;
  frame_list->frame_pos++;
  if (heap_num == heap_max) gc_collect();
  return head;
}

#define gc_new(type, size) (gc_new0(type, size))
#define gc_oarray(size) (gc_new(OBJ_BOXED_ARRAY, sizeof(Object*)*size))
#define gc_pair() (gc_new(OBJ_PAIR, sizeof(Object*)*2))
#define gc_array(size) (gc_new(OBJ_UNBOXED_ARRAY, size))
#define RECORD_BITMAP_NUM(n) (((n)+sizeof(long)*8-1) / (sizeof(long)*8) )
#define BIT(n) (1 << n)

Object* gc_record(long size, long bitmap) {
  Object* obj = gc_new(OBJ_RECORD, sizeof(Object*)*size+RECORD_BITMAP_NUM(size));
  obj->longs[size] = bitmap;
  return (Object*)obj;
}

Object* gc_int(int n) {
  int* data = (int*)gc_new(OBJ_UNBOXED_ARRAY, sizeof(int)*1);

  debug("int ptr %p\n", data);
  *data = n;
  return (Object*)data;
}

#define pool(a) (gc_add_pool(frame_list, a))

#define ENTER_FRAME(frame,SIZE) \
  Object* frame[SIZE+3]; \
  ((Frame*)frame)->frame_prev = frame_list; \
  ((Frame*)frame)->frame_size = SIZE+1; \
  ((Frame*)frame)->frame_pos = 0; \
  frame_list = (Frame*)frame; \

#define LEAVE_FRAME(frame) \
  frame_list = frame_list->frame_prev;
#define pool_ret(a) (gc_add_pool(frame_list->frame_prev, a))
Object* root_frame[256+3];

void gc_init() {

  ((Frame*)root_frame)->frame_prev = NULL;
  ((Frame*)root_frame)->frame_size = 256+1;
  ((Frame*)root_frame)->frame_pos = 0;

  frame_list = (Frame*)root_frame;
#ifdef __cplusplus
  heap_list.clear();
#else
  heap_list = NULL;
#endif
  heap_num = 0;
  heap_max = 8;
}

void gc_free() {
  frame_list = NULL;
  gc_collect();
}

void test() {
  void* frame[3+2];
  frame[0] = (void*)frame_list;
  frame[1] = (void*)2;
  frame_list = (Frame*)frame;

  Object* obj = pool(gc_new(OBJ_BOXED_ARRAY,sizeof(long)*2));
  assert(heap_num==1);
  gc_collect1();
  assert(heap_num==1);
  frame_list = frame_list->frame_prev;
  gc_collect1();
  assert(heap_num==0);
}

void test2() {
  ENTER_FRAME(frame,1);
  Object* obj = pool(gc_new(OBJ_BOXED_ARRAY,sizeof(long)*2));
  assert(heap_num==1);
  gc_collect1();
  assert(heap_num==1);
  LEAVE_FRAME(frame);
  gc_collect1();
  assert(heap_num==0);
}

void test3() {
  for(int j = 0; j < 10000; j+=111) {
    ENTER_FRAME(frame,3);
      Object* unboxed;// = pool(gc_array(sizeof(int)*2));
      // int配列
      for(int i = 0; i < j;i++){
        unboxed = pool(gc_array(sizeof(int)*2));
        unboxed->ints[0] = 50;
        unboxed->ints[1] = 60;
      }

      assert(heap_num>=j);
      gc_collect1();
      assert(heap_num>=j);
    LEAVE_FRAME();
    gc_collect1();
    assert(heap_num==0);
  }
}

static Object* test_int(int n) {
  ENTER_FRAME(frame,1);
  Object* A = pool_ret(pool(gc_int(n)));
  LEAVE_FRAME();
  gc_collect1();
  return A;
}

void test_record() {
  ENTER_FRAME(frame,1);

  // レコード
  Object* A = pool(gc_record(3,BIT(1)|BIT(2)));
  A->longs[0] = 10; // undata
  A->field[1] = gc_int(20);
  A->field[2] = gc_int(30);

  assert(heap_num==3);
  gc_collect1();
  assert(heap_num==3);
  LEAVE_FRAME();
  assert(heap_num==3);
  gc_collect1();
  assert(heap_num==0);
}

int fib(int n) {
  if(n <= 2) return 1;
  return fib(n-2)+fib(n-1);
}

// 全部ルート集合に加えられるので、何もしなくてもちゃんと動く。
void test_noframe() {
  // レコード
  Object* A = pool(gc_record(3,BIT(1)|BIT(2)));
  A->longs[0] = 10; // undata
  A->field[1] = gc_int(20);
  A->field[2] = test_int(30);

  printf("fib %d\n", fib(10));

  assert(heap_num==3);
  gc_collect1();
  assert(heap_num==3);
}

int main() {
  gc_init();
  test();
  gc_free();
  printf("--- test ok\n");
  gc_init();
  test2();
  gc_free();

  printf("--- test 2 ok\n");
  gc_init();
  test3();
  gc_free();

  printf("--- test3 ok\n");
  gc_init();
  test_record();
  gc_free();

  printf("--- test_record ok\n");
  gc_init();
  test_noframe();
  gc_free();

  printf("sizeof type %ld header %ld\n", sizeof(ObjectType), sizeof(ObjectHeader));
  return 0;
}
