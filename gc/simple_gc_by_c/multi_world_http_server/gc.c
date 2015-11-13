#include "gc.h"

VM* vm;
Frame* frame_list;
Frame* frame_bottom;

void noprintf(char* a,...){}

int heap_find(VM* vm, ObjectHeader* o) {
  ObjectHeader* object = vm->heap_list;
  while (object) {
    if(object == o) return 1;
    object = object->next;
  }
  return 0;
}

long heap_count(ObjectHeader* object) {
  long sum = 0;
  while (object) {
    sum++;
    object = object->next;
  }
  return sum;
}

void gc_mark_object(Object* object) {
  ObjectHeader* head = &((ObjectHeader*)object)[-1];
  debug("mark %p\n",head);
  long size;
  if (!heap_find(vm, head)) {
    debug2("******** unfind in heap %p\n", &head[1]);
    return;
  }
  debug2("find\n");
  if (head->marked) return;
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

void gc_sweep(VM* _vm) {
  ObjectHeader** object = &(vm->heap_list);
  debug2("object =%p\n", object);
  while (*object) {
    if (!(*object)->marked) {
      ObjectHeader* unreached = *object;
      *object = unreached->next;

      if(unreached->type == OBJ_VM) vm_finalize((VM*)&unreached[1]);
      
      free(unreached);

      vm->heap_num--;
    } else {
      (*object)->marked = 0;
      if(_vm) {
        debug2("gc sweep vm\n");
        ObjectHeader* moving = *object;
        *object = moving->next;
        debug2("id change\n");
        _vm->heap_num++;
        moving->next = _vm->heap_list;
        _vm->heap_list = moving;
        debug2("heap_num %ld %ld\n", _vm->heap_num, heap_count(_vm->heap_list));
        assert(_vm->heap_num == heap_count(_vm->heap_list));
      } else {
        object = &(*object)->next;
      }
    }
  }
}

void gc_collect() {
  long prev_num = vm->heap_num;

  debug2("gc mark\n");
  gc_mark();
  debug2("gc sweep\n");
  gc_sweep(NULL);

  vm->heap_max = prev_num * 2;

  debug("Collected %ld objects, %ld remaining.\n", prev_num - vm->heap_num,
         vm->heap_num);
}

void gc_collect_end_vm(Object* data, VM* _vm) {
  long prev_num = vm->heap_num;
  debug2("gc mark\n");
  gc_mark_object(data);
  debug2("gc sweep\n");
  gc_sweep(_vm);

  vm->heap_max = prev_num * 2;

  debug("Collected %ld objects, %ld moving.\n", prev_num - vm->heap_num,
         vm->heap_num);
}

void gc_collect_pipe(Object* data) {
  long prev_num = vm->heap_num;
  gc_mark_object(data);
  gc_sweep(NULL);

  vm->heap_max = prev_num * 2;

  debug("Collected %ld objects, %ld remaining.\n", prev_num - vm->heap_num,
         vm->heap_num);
}

void* gc_alloc(ObjectType type, int size) {
  debug2("gc alloc\n");
  debug2("vm=%p\n",vm);
  if (vm->heap_num == vm->heap_max) gc_collect();

  ObjectHeader* head = (ObjectHeader*)malloc(sizeof(ObjectHeader)+size);

  debug("gc_alloc %p\n", head);
  head->type = type;
  head->next = vm->heap_list;
  vm->heap_list = head;
  head->marked = 0;
  head->size=size;
  vm->heap_num++;

  return &head[1];
}

void* gc_alloc_int(int n) {
  int* data = gc_alloc(OBJ_UNBOXED_ARRAY, sizeof(int)*1);

  debug("int ptr %p\n", data);
  *data = n;
  return data;
}


Object* gc_copy(VM* vm, Object* object) {
  ObjectHeader* head = &((ObjectHeader*)object)[-1];
  debug("gc copy %p\n",head);
  long size;
  if (!heap_find(vm, head)) return object;
  long* bitmap;
  Object* new;
  switch(head->type) {
    case OBJ_BOXED_ARRAY:
      new = gc_alloc(head->type, head->size);
      size = ((int)head->size) / sizeof(long);
      debug("size=%ld\n",size);
      for(int i = 0; i < size; i++)
          new->field[i] = gc_copy(vm,object->field[i]);
      break;
    case OBJ_PAIR:
      new = gc_alloc(head->type, head->size);
      new->pair.fst = gc_copy(vm,object->pair.fst);
      new->pair.snd = gc_copy(vm,object->pair.snd);
      break;
    case OBJ_UNBOXED_ARRAY:
    case OBJ_VM:
      new = gc_alloc(head->type, head->size);
      memcpy(new, object, head->size);
      break;
    case OBJ_RECORD:
      size = ((int)head->size) / sizeof(long);
      new = gc_alloc(head->type, head->size);
      memcpy(new, object, head->size);
      bitmap = &object->longs[size];
      for(int i = 0; i < size; i++) {
        if(bitmap[i/sizeof(long)] & (1 << (i % sizeof(long))))
          new->field[i] = gc_copy(vm,object->field[i]);
      }
      break;
  }
  return new;
}

Object* vm_get_record(VM* _vm) {
  return gc_copy(_vm, _vm->record);
}

void vm_finalize(VM* _vm) {
  VM* tmp_vm = vm;

  vm = _vm;
  gc_collect();
  vm = tmp_vm;
}

void vm_end(Object* o, VM* vm) {
  gc_collect_end_vm(o, vm);
}

Object* vm_end_record(VM* vm) {
  gc_collect_end_vm(vm->record, vm);
  return vm->record;
}

VM* vm_new() {
  debug("vm_new\n");
  VM* vm = gc_alloc(OBJ_VM, sizeof(VM));
  debug("gc alloc ok\n");
  vm->record = NULL;
  vm->heap_list = NULL;
  vm->heap_num = 0;
  vm->heap_max = 256;
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
