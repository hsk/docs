/*

C だけで使える簡単な完全なGCをするためのサンプルプログラム

*/
//#define DEBUG

//#define NOGC
//#define NDEBUG

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

#include <set>
#include <list>
#include <vector>

using namespace std;



#ifdef DEBUG
#define debug printf
#else
#define debug noprintf
inline void noprintf(char* str, ...){}
#endif

enum ObjectType {
  OBJ_BOXED_ARRAY,
  OBJ_UNBOXED_ARRAY,
  OBJ_PAIR,
  OBJ_RECORD,
};

struct ObjectHeader {
  unsigned int size;
  unsigned char type;
  unsigned char marked;
};

union Object {
  struct {
    union Object *fst;
    union Object *snd;
  };
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
};

typedef vector<Object*> Frame;

list<Frame*> frame_list;

set<ObjectHeader*> heap_list;       // ローカル変数として、mp を生成
int heap_num;
int heap_max;

inline int heap_find(ObjectHeader* o) {
    return heap_list.find(o) != heap_list.end();
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
      gc_mark_object(object->fst);
      gc_mark_object(object->snd);
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
  for (list<Frame*>::iterator frame = frame_list.begin(); frame != frame_list.end();frame++) {
    for (vector<Object*>::iterator object = (*frame)->begin(); object != (*frame)->end();object++) {
      gc_mark_object(*object);
    }
  }
}

void gc_sweep() {

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
  heap_list.insert(head);
  head->marked = 0;
  head->size=size;
  heap_num++;

  return (Object*)&head[1];
}

#define gc_oarray0(size) (gc_new0(OBJ_BOXED_ARRAY, sizeof(Object*)*size))

inline Object* gc_add_pool(Frame* frame, Object* head) {
  frame->push_back(head);
  return head;
}

#define gc_new(type, size) (gc_new0(type, size))
#define gc_oarray(size) (gc_new(OBJ_BOXED_ARRAY, sizeof(Object*)*size))
#define gc_pair() (gc_new(OBJ_PAIR, sizeof(Object*)*2))
#define gc_array(size) (gc_new(OBJ_UNBOXED_ARRAY, size))
#define RECORD_BITMAP_NUM(n) (((n)+sizeof(long)*8-1) / (sizeof(long)*8) )
#define BIT(n) (1 << n)

inline Object* gc_record(long size, long bitmap) {
  Object* obj = gc_new(OBJ_RECORD, sizeof(Object*)*size+RECORD_BITMAP_NUM(size));
  obj->longs[size] = bitmap;
  return (Object*)obj;
}

inline Object* gc_int(int n) {
  int* data = (int*)gc_new(OBJ_UNBOXED_ARRAY, sizeof(int)*1);

  debug("int ptr %p\n", data);
  *data = n;
  return (Object*)data;
}

inline Object* pool(Object* head) {
  return gc_add_pool(frame_list.front(), head);
}

inline Object* pool_ret(Object* a) {
  for (list<Frame*>::iterator frame = frame_list.begin(); frame != frame_list.end();) {
    frame++;
    if(frame != frame_list.end()) return gc_add_pool(*frame, a);
  }
  return a;
}

void gc_init() {
  frame_list.clear();
  frame_list.push_front(new Frame());
  heap_list.clear();
  heap_num = 0;
  heap_max = 8;
}

void gc_free() {
  for (list<Frame*>::iterator frame = frame_list.begin(); frame != frame_list.end();frame++)
    delete (*frame);  
  frame_list.clear();
  gc_collect();
}

struct AutoPool{
  Frame frame;
  AutoPool(){
    frame_list.push_front(&frame);
  }
  ~AutoPool(){
    frame_list.pop_front();
  }
};

void test() {
  {
    AutoPool autopool;
    Object* obj = pool(gc_new(OBJ_BOXED_ARRAY,sizeof(long)*2));
    assert(heap_num==1);
    gc_collect1();
    assert(heap_num==1);
  }
  gc_collect1();
  assert(heap_num==0);
}

void test3() {
  for(int j = 0; j < 10000; j+=111) {
    {
      AutoPool autopool;
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
    }
    gc_collect1();
    assert(heap_num==0);
  }
}

static Object* test_int(int n) {
  Object* A;
  { AutoPool autopool;
    A = pool_ret(pool(gc_int(n)));
  }
  gc_collect1();
  return A;
}

void test_record() {
  { AutoPool autopool;

    // レコード
    Object* A = pool(gc_record(3,BIT(1)|BIT(2)));
    A->longs[0] = 10; // undata
    A->field[1] = gc_int(20);
    A->field[2] = gc_int(30);

    assert(heap_num==3);
    gc_collect1();
    assert(heap_num==3);
  }
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
  printf("test_noframe\n");
  // レコード
  Object* A = pool(gc_record(3,BIT(1)|BIT(2)));
  printf("test_noframe 1\n");
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
