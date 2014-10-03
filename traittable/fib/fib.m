#import <Foundation/Foundation.h>

@interface Fib : NSObject
-(Fib*)initWithInt:(int)n;
-(int)fib;
@end

@implementation Fib {
  int n;
}

-(Fib*)initWithInt:(int)m
{
  self = [super init];
  if (self) {
    n = m;
  }
  return self;
}

-(int)fib
{
  if (n < 2) return 1;
  return [[[Fib alloc]initWithInt:n-2]fib] +
         [[[Fib alloc]initWithInt:n-1]fib];
}
@end

#include <stdio.h>
#include <sys/time.h>
#include <memory.h>

long gett() {
  struct timeval tv;
  gettimeofday (&tv, NULL);
  return (tv.tv_sec) * 1000 + tv.tv_usec / 1000;
}

int fib(int n) {
  if(n < 2) return 1;
  return fib(n - 2) + fib(n - 1);
}

/*
struct Fib {
	int n;
	Fib(int n):n(n) {}
	int fib() {
		if (n < 2) return 1;
		return Fib(n - 1).fib() + Fib(n - 2).fib();
	}
};
*/

/*
struct Vec {
  int size;
  void** data;
};

Vec* newVec() {
  Vec* v = new Vec();
  v->size = 0;
  v->data = new void*[v->size];
  return v;
}
void setVec(Vec* v, int idx, void* d) {
  if (idx > v->size) {
    // resize
    void** data = new void*[idx+1];
    memcpy(v->data, data, sizeof(void*)*v->size);
    v->size = idx+1;
    delete[] v->data;
    v->data = data;
  }
  v->data[idx] = d;
}

struct Class {
  int id;
};

int Class_genId() {
  static int classId = -1;
  classId++;
  return classId;
}

// struct Int
int Int_classId = Class_genId();
struct Int {
  int id;
  int x;
  Int(int x):id(Int_classId),x(x){}
};

struct Fib2 {
  int (*fib)(Class*);
};
Vec* Fib2_v = newVec();

int Fib2_Int_fib(Class* self) {
  Int* p = (Int*)self;
  if(p-> x < 2) return 1;

  Int p1(p->x - 2);
  Int p2(p->x - 1);

  return ((Fib2*)Fib2_v->data[p->id])->fib((Class*)&p1) +
      ((Fib2*)Fib2_v->data[p->id])->fib((Class*)&p2);
}

Fib2* newFib2_Int() {
  Fib2 *impl = new Fib2();
  setVec(Fib2_v, Int_classId, (void*)impl);
  impl->fib = &Fib2_Int_fib;
  return impl;
}
Fib2* Fib2_Int_ = newFib2_Int();

int Int3_classId = Class_genId();
struct Int3 {
  int id;
  int x;
};

struct Fib3 {
  int (*fib)(Class*);
};
Vec* Fib3_v = newVec();

int Fib3_Int3_fib(Class* self) {
  Int3* p = (Int3*)self;
  if(p-> x < 2) return 1;

  Int3 p1 ={Int3_classId, p->x - 2};
  Int3 p2 ={Int3_classId, p->x - 1};

  return Fib3_Int3_fib((Class*)&p1) + Fib3_Int3_fib((Class*)&p2);
}

Fib3* newFib3_Int3() {
  Fib3 *impl = new Fib3();
  setVec(Fib3_v, Int3_classId, (void*)impl);
  impl->fib = &Fib3_Int3_fib;
  return impl;
}
Fib3* Fib3_Int3_ = newFib3_Int3();
*/

int main() {
  long start;

  start = gett();
  printf("%d\n", fib(40));
  printf("%ld\n", gett() - start);


  start = gett();
  @autoreleasepool {
    Fib *obj1 = [[Fib alloc]initWithInt:40];
    printf("%d\n", [obj1 fib]);
  }
  printf("%ld\n", gett() - start);

/*
  start = gett();
  printf("%d\n", Fib(40).fib());
  printf("%ld\n", gett() - start);

  start = gett();
  Int p(40);
  printf("%d\n", ((Fib2*)Fib2_v->data[p.id])->fib((Class*)&p));
  printf("%ld\n", gett() - start);

  start = gett();
  Int3 p3 = {Int3_classId, 40};
  printf("%d\n", ((Fib2*)Fib3_v->data[p3.id])->fib((Class*)&p3));
  printf("%ld\n", gett() - start);
*/
  return 0;
}
