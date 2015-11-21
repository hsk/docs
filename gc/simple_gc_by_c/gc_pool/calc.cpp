/*

C だけで使える簡単な完全なGCをするためのサンプルプログラム

*/
#define DEBUG

//#define NOGC
//#define NDEBUG

#ifdef NOGC
#define NDEBUG
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


struct GC{
  list<Frame*> frame_list;
  set<ObjectHeader*> heap_list;       // ローカル変数として、mp を生成
  int heap_num;
  int heap_max;
  Frame frame;
  GC() {
    frame_list.push_front(&frame);
    heap_list.clear();
    heap_num = 0;
    heap_max = 8;
  }
  ~GC() {
    frame_list.clear();
    collect();
  }

  int heap_find(ObjectHeader* o) {
      return heap_list.find(o) != heap_list.end();
  }

  void mark_object(Object* object) {
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
            mark_object(object->field[i]);
        break;
      case OBJ_PAIR:
        debug("PAIR\n");
        mark_object(object->fst);
        mark_object(object->snd);
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
            mark_object(object->field[i]);
          else
            debug("skip %d\n", i);
        }
        break;
    }
  }

  void mark() {
    for (list<Frame*>::iterator frame = frame_list.begin(); frame != frame_list.end();frame++) {
      for (vector<Object*>::iterator object = (*frame)->begin(); object != (*frame)->end();object++) {
        mark_object(*object);
      }
    }
  }

  void sweep() {

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

  void collect() {
    #ifdef NOGC
      return;
    #endif
    int prev_num = heap_num;

    mark();
    sweep();

    heap_max = prev_num * 2;

    debug("Collected %d objects, %d remaining.\n", prev_num - heap_num,
           heap_num);
  }

  Object* alloc(ObjectType type, int size) {

    ObjectHeader* head = (ObjectHeader*)malloc(sizeof(ObjectHeader)+size);
    //debug("alloc %p\n", head);
    head->type = type;
    heap_list.insert(head);
    head->marked = 0;
    head->size=size;
    heap_num++;

    return (Object*)&head[1];
  }

  Object* add_pool(Frame* frame, Object* head) {
    frame->push_back(head);
    return head;
  }

  Object* oarray(int size) { return alloc(OBJ_BOXED_ARRAY, sizeof(Object*)*size); }
  Object* pair() { return alloc(OBJ_PAIR, sizeof(Object*)*2); }
  Object* array(int size) { return alloc(OBJ_UNBOXED_ARRAY, size); }
  #define RECORD_BITMAP_NUM(n) (((n)+sizeof(long)*8-1) / (sizeof(long)*8) )
  #define BIT(n) (1 << n)

  Object* record(long size, long bitmap) {
    Object* obj = alloc(OBJ_RECORD, sizeof(Object*)*size+RECORD_BITMAP_NUM(size));
    obj->longs[size] = bitmap;
    return (Object*)obj;
  }

  Object* longv(long n) {
    long* data = (long*)alloc(OBJ_UNBOXED_ARRAY, sizeof(long)*1);
    *data = n;
    return (Object*)data;
  }

  Object* str(const char* n) {
    long len = strlen(n);
    char* data = (char*)alloc(OBJ_UNBOXED_ARRAY, len+1);
    memcpy(data, n, len+1);
    return (Object*)data;
  }
};
GC gc;

inline Object* pool(Object* head) {
  return gc.add_pool(gc.frame_list.front(), head);
}

inline Object* pool_ret(Object* a) {
  for (list<Frame*>::iterator frame = gc.frame_list.begin(); frame != gc.frame_list.end();) {
    frame++;
    if(frame != gc.frame_list.end()) return gc.add_pool(*frame, a);
  }
  return a;
}

struct AutoPool{
  Frame frame;
  AutoPool(){ gc.frame_list.push_front(&frame); }
  ~AutoPool(){ gc.frame_list.pop_front(); }
};

enum {EInt, EAdd, EMul, ESub, EDiv};


static Object* eint(long i) {
  Object* e = pool(gc.array(2*sizeof(long)));
  e->longs[0] = EInt;
  e->longs[1] = i;
  return e;
}

static Object* ebin(long tag, Object* e1, Object* e2) {
  Object* e = pool(gc.record(3,BIT(1) | BIT(2)));
  e->longs[0] = tag;
  e->field[1] = e1;
  e->field[2] = e2;
  return e;
}

static Object* eval(Object* e) {
	switch(e->longs[0]) {
	case EInt: return pool(gc.longv(e->longs[1]));
	case EAdd: return pool(gc.longv(eval(e->field[1])->longv + eval(e->field[2])->longv));
	case ESub: return pool(gc.longv(eval(e->field[1])->longv - eval(e->field[2])->longv));
	case EMul: return pool(gc.longv(eval(e->field[1])->longv * eval(e->field[2])->longv));
	case EDiv: return pool(gc.longv(eval(e->field[1])->longv / eval(e->field[2])->longv));
	default: return pool(gc.longv(1));
	}
}

struct Val {
  char* result;
  long b;
  long c;
};

static Val* model() {
  AutoPool autopool;
  Val* val = (Val*)pool_ret(pool(gc.oarray(3)));
  val->result = pool(gc.str("Calc"))->chars;
  val->b = eval(ebin(EMul, ebin(EAdd,eint(1),eint(2)), eint(5)))->longv;
  val->c = eval(ebin(EDiv, ebin(EMul,eint(10),eint(20)), eint(5)))->longv;
  return val;
}

static void view(Val* val) {
  printf("HTTP/1.0 200 OK\n");
  printf("text/html\n");
  printf("Cache-Control: max-age=0\n\n");

  printf("<html>\n");
  printf("<meta http-equiv=\"Content-Type\" content=\"text/html; charset=UTF-8\" />");
  printf("<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />\n");
  printf("<body>\n");
  printf("<h1>%s</h1>\n", val->result);
  printf("(1+2)*5=%ld<br/>\n", val->b);
  printf("(10*20)/5=%ld<br/>\n", val->c);

  printf("<hr/>\n");
  printf("<a href=\"javascript:history.back()\">back</a>\n");
  printf("</body>\n");
  printf("</html>\n");
}

int main() {
  Val* val = model();
  gc.collect();
  view(val);
  return 0;
}

