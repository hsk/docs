/*

multi vm gc

*/

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <memory.h>

//#define DEBUG
//#define DEBUG2
void noprintf(char* str, ...);
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

extern VM* vm;
extern Frame* frame_list;
extern Frame* frame_bottom;

int heap_find(VM* vm, ObjectHeader* o);
long heap_count(ObjectHeader* object);
void gc_mark_object(Object* object);
void gc_mark();
void vm_finalize(VM* _vm);

void gc_sweep(VM* _vm);
void gc_collect();
void gc_collect_end_vm(Object* data, VM* _vm);
void gc_collect_pipe(Object* data);
#define PUSH_VM(vmname) \
  VM* vmname = vm; \
  vm = vm_new(); \
  Frame* vmname##_tmp_bottom = frame_bottom; \

#define POP_VM(vmname,root) \
  debug2("********* POP_VM %p -> %p\n", vm, vmname); \
  gc_collect_end_vm(root,vmname); \
  frame_bottom = vmname##_tmp_bottom; \
  debug2("********* POP_VM DONE %ld -> %ld\n", vm->heap_num, vmname->heap_num); \
  vm = vmname; \

void* gc_alloc(ObjectType type, int size);
#define gc_alloc_pair() (gc_alloc(OBJ_PAIR, sizeof(Object*)*2))
#define gc_alloc_boxed_array(size) (gc_alloc(OBJ_BOXED_ARRAY, sizeof(Object*)*size))
#define gc_alloc_unboxed_array(size) (gc_alloc(OBJ_UNBOXED_ARRAY, size))
#define gc_alloc_record(n) (gc_alloc(OBJ_RECORD, sizeof(Object*)*n+RECORD_BITMAP_NUM(n)))
#define RECORD_BITMAP_NUM(n) (((n)+sizeof(long)*8-1) / (sizeof(long)*8) )
#define BIT(n) (1 << n)

void* gc_alloc_int(int n);
void* gc_alloc_long(long n);

Object* gc_copy(VM* vm, Object* object);
#define ENTER_FRAME(frame, SIZE) \
  Object* frame[SIZE+2]; \
  ((Frame*)frame)->frame_prev = frame_list; \
  ((Frame*)frame)->frame_size = SIZE; \
  frame_list = (Frame*)frame; \

#define ENTER_FRAME_ENUM(frame) ENTER_FRAME(frame, (frame##_END-2))

#define LEAVE_FRAME(frame) \
  frame_list = frame_list->frame_prev;

Object* vm_get_record(VM* _vm);
void vm_finalize(VM* _vm);
void vm_end(Object* o, VM* vm);
Object* vm_end_record(VM* vm);
VM* vm_new();
void gc_init();
void gc_free();

static Object* str(char* str) {
  long len = strlen(str);
  Object* o = gc_alloc_unboxed_array(len+1);
  strcpy(o->chars, str);
  return o;
}

static Object* str_cat(Object* str1, Object* str2) {
  long len = strlen(str1->chars) + strlen(str2->chars);
  Object* o = gc_alloc_unboxed_array(len+1);
  sprintf(o->chars, "%s%s", str1->chars, str2->chars);
  return o;
}
