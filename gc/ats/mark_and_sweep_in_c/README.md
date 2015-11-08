## 5. Cで実装されたMark & Sweepの例

まず、Cで簡単なMark&Sweepのソースを見ましょう。

```
#include <stdio.h>
#include <stdlib.h>

#define STACK_MAX 256

typedef enum {
  OBJ_INT,
  OBJ_PAIR
} ObjectType;
```

オブジェクトのタイプがintとペアの２つがあります。

```
typedef struct sObject {
  ObjectType type;
  unsigned char marked;
  struct sObject* next;
  union {
    /* OBJ_INT */
    int value;

    /* OBJ_PAIR */
    struct {
      struct sObject* head;
      struct sObject* tail;
    };
  };
} Object;
```

オブジェクトは、オブジェクトのタイプをもっていて、markedがマークビットです。

unionのデータに、intか、headとtailのポインタが入ります。
データはリストで繋げて管理します。

```
typedef struct {
  Object* stack[STACK_MAX];
  int stackSize;

  /* The first object in the linked list of all objects on the heap. */
  Object* firstObject;

  /* The total number of currently allocated objects. */
  int numObjects;

  /* The number of objects required to trigger a GC. */
  int maxObjects;
} VM;
```

仮想マシンの状態は、オブジェクトのデータが入るエリアがあり、
スタックのサイズを持ち、オブジェクトのリストと、オブジェクトの数と、オブジェクトの最大数を持ちます。


```
void assert(int condition, const char* message) {
  if (!condition) {
    printf("%s\n", message);
    exit(1);
  }
}
```

アザート関数はコンディションがfalseならメッセージを出力して終了します。

```
VM* newVM() {
  VM* vm = malloc(sizeof(VM));
  vm->stackSize = 0;
  vm->firstObject = NULL;
  vm->numObjects = 0;
  vm->maxObjects = 8;
  return vm;
}
```

newVM関数では、vmを作成して、初期化し返却します。

```
void push(VM* vm, Object* value) {
  assert(vm->stackSize < STACK_MAX, "Stack overflow!");
  vm->stack[vm->stackSize++] = value;
}

Object* pop(VM* vm) {
  assert(vm->stackSize > 0, "Stack underflow!");
  return vm->stack[--vm->stackSize];
}
```

pushとpopでvmにオブジェクトをプッシュしたりポップしたり出来ます。

ここからがGCのプログラムです。

```
void mark(Object* object) {
  /* If already marked, we're done. Check this first to avoid recursing
     on cycles in the object graph. */
  if (object->marked) return;

  object->marked = 1;

  if (object->type == OBJ_PAIR) {
    mark(object->head);
    mark(object->tail);
  }
}
```

mark関数はオブジェクトをマークします。markされていれば抜け、そうでなければマークビットを立て、ペアならさらに中を見にいきます。

```
void markAll(VM* vm)
{
  for (int i = 0; i < vm->stackSize; i++) {
    mark(vm->stack[i]);
  }
}
```

markAllはvm内のオブジェクトのすべてをアレします。(アレって何だ？w)

```
void sweep(VM* vm)
{
  Object** object = &vm->firstObject;
  while (*object) {
    if (!(*object)->marked) {
      /* This object wasn't reached, so remove it from the list and free it. */
      Object* unreached = *object;

      *object = unreached->next;
      free(unreached);

      vm->numObjects--;
    } else {
      /* This object was reached, so unmark it (for the next GC) and move on to
       the next. */
      (*object)->marked = 0;
      object = &(*object)->next;
    }
  }
}
```

スイープは、オブジェクトのリストを順番に見て行き、マークビットが立っていなければ、フリーして、マークビットが立っていれば、下ろします。

```
void gc(VM* vm) {
  int numObjects = vm->numObjects;

  markAll(vm);
  sweep(vm);

  vm->maxObjects = vm->numObjects * 2;

  printf("Collected %d objects, %d remaining.\n", numObjects - vm->numObjects,
         vm->numObjects);
}
```

gcは、最初にマークして、次にスイープします。
最初のオブジェクト数を保存しておき、終わった後のオブジェクト数からさし引く事で、コレクトされたオブジェクト数を表示します。

```
Object* newObject(VM* vm, ObjectType type) {
  if (vm->numObjects == vm->maxObjects) gc(vm);

  Object* object = malloc(sizeof(Object));
  object->type = type;
  object->next = vm->firstObject;
  vm->firstObject = object;
  object->marked = 0;

  vm->numObjects++;

  return object;
}
```

newObjectはオブジェクトタイプを指定してオブジェクトを作成します。

```
void pushInt(VM* vm, int intValue) {
  Object* object = newObject(vm, OBJ_INT);
  object->value = intValue;

  push(vm, object);
}
```

pushIntは整数値を取り、intのオブジェクトを作ってVMにプッシュする命令です。

```
Object* pushPair(VM* vm) {
  Object* object = newObject(vm, OBJ_PAIR);
  object->tail = pop(vm);
  object->head = pop(vm);

  push(vm, object);
  return object;
}
```

pushPairは、スタック上から２つの値を取得して、ペアを作り、プッシュする命令です。

```
void objectPrint(Object* object) {
  switch (object->type) {
    case OBJ_INT:
      printf("%d", object->value);
      break;

    case OBJ_PAIR:
      printf("(");
      objectPrint(object->head);
      printf(", ");
      objectPrint(object->tail);
      printf(")");
      break;
  }
}
```



```
void freeVM(VM *vm) {
  vm->stackSize = 0;
  gc(vm);
  free(vm);
}
```

freeVMはvmを開放します。

ここからはテストです。

test1は、vmを作って、1と2をスタックにプッシュします。
gcをしても、オブジェクトはスタック上にあるので消される事はありません。

```
void test1() {
  printf("Test 1: Objects on stack are preserved.\n");
  VM* vm = newVM();
  pushInt(vm, 1);
  pushInt(vm, 2);

  gc(vm);
  assert(vm->numObjects == 2, "Should have preserved objects.");
  freeVM(vm);
}
```

test2は1と2をpushしたあとpopして捨てます。したがって、2つのオブジェクトはgcで回収されます。

```
void test2() {
  printf("Test 2: Unreached objects are collected.\n");
  VM* vm = newVM();
  pushInt(vm, 1);
  pushInt(vm, 2);
  pop(vm);
  pop(vm);

  gc(vm);
  assert(vm->numObjects == 0, "Should have collected objects.");
  freeVM(vm);
}
```

test3は、1と2のペアを作り、3と4のペアを作って、そのペアを作ります。gcしてもオブジェクトはどれも消えません。

```
void test3() {
  printf("Test 3: Reach nested objects.\n");
  VM* vm = newVM();
  pushInt(vm, 1);
  pushInt(vm, 2);
  pushPair(vm);
  pushInt(vm, 3);
  pushInt(vm, 4);
  pushPair(vm);
  pushPair(vm);

  gc(vm);
  assert(vm->numObjects == 7, "Should have reached objects.");
  freeVM(vm);
}
```

test4では、aのtailにbをbのtailにaを入れます。2と4が消えて、aとbの参照は循環します。6つのオブジェクトを作って、2つのオブジェクトがGCで回収され4つのオブジェクトが残ります。

```
void test4() {
  printf("Test 4: Handle cycles.\n");
  VM* vm = newVM();
  pushInt(vm, 1);
  pushInt(vm, 2);
  Object* a = pushPair(vm);
  pushInt(vm, 3);
  pushInt(vm, 4);
  Object* b = pushPair(vm);

  /* Set up a cycle, and also make 2 and 4 unreachable and collectible. */
  a->tail = b;
  b->tail = a;

  gc(vm);
  assert(vm->numObjects == 4, "Should have collected objects.");
  freeVM(vm);
}
```

これは、ループで大量にオブジェクトを作るテストです。

```
void perfTest() {
  printf("Performance Test.\n");
  VM* vm = newVM();

  for (int i = 0; i < 1000; i++) {
    for (int j = 0; j < 20; j++) {
      pushInt(vm, i);
    }

    for (int k = 0; k < 20; k++) {
      pop(vm);
    }
  }
  freeVM(vm);
}
```

メイン関数で、様々なテストを呼び出します。

```
int main(int argc, const char * argv[]) {
  test1();
  test2();
  test3();
  test4();
  perfTest();

  return 0;
}
```
