#include <stdio.h>
#include <sys/time.h>
#include <memory.h>

long gett() {
  timeval tv;
  gettimeofday (&tv, NULL);
  return (tv.tv_sec) * 1000 + tv.tv_usec / 1000;
}

int fib(int n) {
  if(n < 2) return 1;
  return fib(n - 2) + fib(n - 1);
}

struct Fib {
	int n;
	Fib(int n):n(n) {}
	int fib() {
		if (n < 2) return 1;
		return Fib(n - 1).fib() + Fib(n - 2).fib();
	}
};
struct Fib2 {
  int n;
  Fib2(int n):n(n) {}
  int fib() {
    if (n < 2) return 1;
    Fib2 p(n-1);
    Fib2 p2(n-2);
    return p2.fib() + p.fib();
  }
};

namespace trait1 {


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

  void bench() {
    long start;
    start = gett();
    Int p(40);
    printf("%d\n", ((Fib2*)Fib2_v->data[p.id])->fib((Class*)&p));
    printf("%ld\n", gett() - start);

    start = gett();
    Int3 p3 = {Int3_classId, 40};
    printf("%d\n", ((Fib2*)Fib3_v->data[p3.id])->fib((Class*)&p3));
    printf("%ld\n", gett() - start);
  }
};

namespace trait2 {

  enum {PAGE_INIT_SIZE=0,PAGE_SIZE=0x100,PAGE_BIT=8,PAGE_MASK=0xff};

  struct Vec {
    int size;
    void*** data;
  };

  Vec* newVec() {
    Vec* v = new Vec();
    v->size = PAGE_INIT_SIZE;
    v->data = new void**[v->size];
    memset(v->data, 0, sizeof(void**)*v->size);
    return v;
  }
  void setVec(Vec* v, int idx, void* d) {
    int page = idx >> PAGE_BIT;
    idx = idx & PAGE_MASK;
    if (v->size <= page) {
      printf("realloc\n");
      void*** data = new void**[page+1];
      memcpy(v->data, data, sizeof(void**)*v->size);
      memset(&data[v->size], 0, sizeof(void**)*(page+1 - v->size));
      v->size = page+1;
      delete[] v->data;
      v->data = data;
    }
    if (v->data[page] == 0) {
      printf("page gen\n");
      v->data[page] = new void*[PAGE_SIZE];
    }
    v->data[page][idx] = d;
  }

  struct Class {
    int id;
  };

  int Class_genId() {
    static int classId = -1;
    classId+=70000;
    return classId;
  }

  // struct Int
  int Int_classId = Class_genId();
  struct Int {
    int id;
    int x;
    inline Int(int x):id(Int_classId),x(x){}
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

    return ((Fib2*)Fib2_v->data[p->id >> PAGE_BIT][p->id & PAGE_MASK])->fib((Class*)&p1) +
        ((Fib2*)Fib2_v->data[p->id >> PAGE_BIT][p->id & PAGE_MASK])->fib((Class*)&p2);
  }

  Fib2* newFib2_Int() {
    Fib2 *impl = new Fib2();
    setVec(Fib2_v, Int_classId, (void*)impl);
    impl->fib = &Fib2_Int_fib;
    return impl;
  }
  Fib2* Fib2_Int_ = newFib2_Int();

  struct Fib3 {
    int (*fib)(Class*);
  };
  Vec* Fib3_v = newVec();

  int Fib3_Int_fib(Class* self) {
    Int* p = (Int*)self;
    if(p-> x < 2) return 1;

    Int p1(p->x - 2);
    Int p2(p->x - 1);

    return Fib3_Int_fib((Class*)&p1) + Fib3_Int_fib((Class*)&p2);
  }

  Fib3* newFib3_Int() {
    Fib3 *impl = new Fib3();
    setVec(Fib3_v, Int_classId, (void*)impl);
    impl->fib = &Fib3_Int_fib;
    return impl;
  }
  Fib3* Fib3_Int_ = newFib3_Int();

  void bench() {
    long start;
    start = gett();
    Int p(40);
    printf("%d\n", ((Fib2*)Fib2_v->data[p.id >> PAGE_BIT][p.id & PAGE_MASK])->fib((Class*)&p));
    printf("%ld\n", gett() - start);

    start = gett();
    printf("%d\n", ((Fib3*)Fib3_v->data[p.id >> PAGE_BIT][p.id & PAGE_MASK])->fib((Class*)&p));
    printf("%ld\n", gett() - start);
  }
};

#include <map>
namespace traitmap {


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
  std::map<int, void*> Fib2_v;

  int Fib2_Int_fib(Class* self) {
    Int* p = (Int*)self;
    if(p-> x < 2) return 1;

    Int p1(p->x - 2);
    Int p2(p->x - 1);

    return ((Fib2*)Fib2_v[p->id])->fib((Class*)&p1) +
        ((Fib2*)Fib2_v[p->id])->fib((Class*)&p2);
  }

  Fib2* newFib2_Int() {
    Fib2 *impl = new Fib2();
    Fib2_v[Int_classId] = (void*)impl;
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
  std::map<int,void*> Fib3_v;

  int Fib3_Int3_fib(Class* self) {
    Int3* p = (Int3*)self;
    if(p-> x < 2) return 1;

    Int3 p1 ={Int3_classId, p->x - 2};
    Int3 p2 ={Int3_classId, p->x - 1};

    return Fib3_Int3_fib((Class*)&p1) + Fib3_Int3_fib((Class*)&p2);
  }

  Fib3* newFib3_Int3() {
    Fib3 *impl = new Fib3();
    Fib3_v[Int3_classId] = (void*)impl;
    impl->fib = &Fib3_Int3_fib;
    return impl;
  }
  Fib3* Fib3_Int3_ = newFib3_Int3();

  void bench() {
    long start;
    start = gett();
    Int p(40);
    printf("%d\n", ((Fib2*)Fib2_v[p.id])->fib((Class*)&p));
    printf("%ld\n", gett() - start);

    start = gett();
    Int3 p3 = {Int3_classId, 40};
    printf("%d\n", ((Fib2*)Fib3_v[p3.id])->fib((Class*)&p3));
    printf("%ld\n", gett() - start);


    printf("map size=10000\n");
    for(int i = 1; i < 10000; i++) {
      Fib2_v[i] = 0;
    }
    start = gett();
    printf("%d\n", ((Fib2*)Fib2_v[p.id])->fib((Class*)&p));
    printf("%ld\n", gett() - start);

  }
};

template<class K, class V>
struct lrumap {

  std::map<K,V> data;

  int tm;
  int capacity;
        
  typedef struct{K k;V *v;} Pair;

  Pair* vs;
  lrumap(int cap=10) {
    vs = new Pair[cap];
    memset(vs,0, sizeof(Pair)*cap);
    tm=-1;
    capacity=cap;
  }
  ~lrumap() { delete[] vs; }

  inline V& operator[](const K& k) {
    Pair& p = vs[0];
    if (p.k == k) {
      return *p.v;
    }
    for(int i = 1; i < capacity; i++) {
      Pair &p2 = vs[i];
      if (p2.k == k) {
        vs[0] = p2;
        vs[i] = p;
        return *p2.v;
      }
    }

    tm = (tm + 1) % capacity;
    vs[tm].k = k;
    vs[tm].v = &data[k];
    return *vs[tm].v;
  }
};

namespace traitlrumap {


  struct Class {
    int id;
  };

  int Class_genId() {
    static int classId = 0;
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
  lrumap<int, Fib2*> Fib2_v;

  int Fib2_Int_fib(Class* self) {
    Int* p = (Int*)self;
    if(p-> x < 2) return 1;

    Int p1(p->x - 2);
    Int p2(p->x - 1);

    return ((Fib2*)Fib2_v[p->id])->fib((Class*)&p1) +
        ((Fib2*)Fib2_v[p->id])->fib((Class*)&p2);
  }

  Fib2* newFib2_Int() {
    Fib2 *impl = new Fib2();
    Fib2_v[Int_classId] = impl;
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
  lrumap<int,Fib3*> Fib3_v;

  int Fib3_Int3_fib(Class* self) {
    Int3* p = (Int3*)self;
    if(p-> x < 2) return 1;

    Int3 p1 ={Int3_classId, p->x - 2};
    Int3 p2 ={Int3_classId, p->x - 1};

    return Fib3_Int3_fib((Class*)&p1) + Fib3_Int3_fib((Class*)&p2);
  }

  Fib3* newFib3_Int3() {
    Fib3 *impl = new Fib3();
    Fib3_v[Int3_classId] = impl;
    impl->fib = &Fib3_Int3_fib;
    return impl;
  }
  Fib3* Fib3_Int3_ = newFib3_Int3();

  void bench() {

    printf("bench start\n");
    long start;
    start = gett();
    Int p(40);
    printf("bench start 1 p.id=%d %p\n",p.id, Fib2_v[1]);
    printf("%d\n", (Fib2_v[p.id])->fib((Class*)&p));
    printf("%ld\n", gett() - start);

    start = gett();
    Int3 p3 = {Int3_classId, 40};
    printf("%d\n", (Fib3_v[p3.id])->fib((Class*)&p3));
    printf("%ld\n", gett() - start);


    printf("map size=10000\n");
    for(int i = 10; i < 10000; i++) {
      Fib2_v[i] = 0;
    }
    start = gett();
    printf("%d\n", ((Fib2*)Fib2_v[p.id])->fib((Class*)&p));
    printf("%ld\n", gett() - start);

  }
};



#include <unordered_map>

namespace traitunrole {


  struct Class {
    int id;
  };

  int Class_genId() {
    static int classId = 0;
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
  std::unordered_map<int, Fib2*> Fib2_v;

  int Fib2_Int_fib(Class* self) {
    Int* p = (Int*)self;
    if(p-> x < 2) return 1;

    Int p1(p->x - 2);
    Int p2(p->x - 1);

    return ((Fib2*)Fib2_v[p->id])->fib((Class*)&p1) +
        ((Fib2*)Fib2_v[p->id])->fib((Class*)&p2);
  }

  Fib2* newFib2_Int() {
    Fib2 *impl = new Fib2();
    Fib2_v[Int_classId] = impl;
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
  std::unordered_map<int,Fib3*> Fib3_v;

  int Fib3_Int3_fib(Class* self) {
    Int3* p = (Int3*)self;
    if(p-> x < 2) return 1;

    Int3 p1 ={Int3_classId, p->x - 2};
    Int3 p2 ={Int3_classId, p->x - 1};

    return Fib3_Int3_fib((Class*)&p1) + Fib3_Int3_fib((Class*)&p2);
  }

  Fib3* newFib3_Int3() {
    Fib3 *impl = new Fib3();
    Fib3_v[Int3_classId] = impl;
    impl->fib = &Fib3_Int3_fib;
    return impl;
  }
  Fib3* Fib3_Int3_ = newFib3_Int3();

  void bench() {

    printf("bench start\n");
    long start;
    start = gett();
    Int p(40);
    printf("bench start 1 p.id=%d %p\n",p.id, Fib2_v[1]);
    printf("%d\n", (Fib2_v[p.id])->fib((Class*)&p));
    printf("%ld\n", gett() - start);

    start = gett();
    Int3 p3 = {Int3_classId, 40};
    printf("%d\n", (Fib3_v[p3.id])->fib((Class*)&p3));
    printf("%ld\n", gett() - start);


    printf("map size=10000\n");
    for(int i = 10; i < 10000; i++) {
      Fib2_v[i] = 0;
    }
    start = gett();
    printf("%d\n", ((Fib2*)Fib2_v[p.id])->fib((Class*)&p));
    printf("%ld\n", gett() - start);

  }
};


namespace traitid {


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
    if (idx >= v->size) {
      printf("resize %p %ld\n", v->data[0], sizeof(void*)*v->size);
      // resize
      void** data = new void*[idx+1];
      memcpy(data, v->data, (int)(sizeof(void*)*v->size));
      v->size = idx+1;
      delete[] v->data;
      v->data = data;
    }
    v->data[idx] = d;
      printf("resize ok %p\n",v->data[0]);

  }

  struct Class {
    int id;
  };

  int Class_genId() {
    static int classId = 0;
    classId++;
    return classId;
  }

  // struct Int
  Vec* Int_v = newVec();
  struct Int {
    Vec* v;
    int x;
    Int(int x):v(Int_v),x(x){}
  };

  struct Impl_Int {
    int (*fib)(void*);
  };

  int Impl_Int_classId = 0;
  int Impl_Int_Int_fib(void* self) {
    Int* p = (Int*)self;
    if(p-> x < 2) return 1;

    Int p1(p->x - 2);
    Int p2(p->x - 1);

    return ((Impl_Int*)p1.v->data[Impl_Int_classId])->fib((void*)&p1) +
           ((Impl_Int*)p2.v->data[Impl_Int_classId])->fib((void*)&p2);
  }

  Impl_Int* newImpl_Int_Int() {
    Impl_Int *impl = new Impl_Int();
    setVec(Int_v, 0, (void*)impl);
    impl->fib = &Impl_Int_Int_fib;
    return impl;
  }
  Impl_Int* Impl_Int_Int_ = newImpl_Int_Int();


  struct Fib2 {
    int (*fib)(void*);
  };
  int Fib2_classId = Class_genId();

  int Fib2_Int_fib(void* self) {
    Int* p = (Int*)self;
    if(p-> x < 2) return 1;

    Int p1(p->x - 2);
    Int p2(p->x - 1);

    return ((Fib2*)p1.v->data[Fib2_classId])->fib((void*)&p1) +
        ((Fib2*)p2.v->data[Fib2_classId])->fib((void*)&p2);
  }

  Fib2* newFib2_Int() {
    Fib2 *impl = new Fib2();
    setVec(Int_v, Fib2_classId, (void*)impl);
    impl->fib = &Fib2_Int_fib;
    return impl;
  }
  Fib2* Fib2_Int_ = newFib2_Int();



  struct Fib3 {
    int (*fib)(void*);
  };
  int Fib3_classId = Class_genId();

  int Fib3_Int_fib(void* self) {
    Int* p = (Int*)self;
    if(p-> x < 2) return 1;

    Int p1(p->x - 2);
    Int p2(p->x - 1);

    return Fib3_Int_fib((void*)&p1) + Fib3_Int_fib((void*)&p2);
  }

  Fib3* newFib3_Int() {
    Fib3 *impl = new Fib3();
    setVec(Int_v, Fib3_classId, (void*)impl);
    impl->fib = &Fib3_Int_fib;
    return impl;
  }
  Fib3* Fib3_Int_ = newFib3_Int();

  void bench() {
    long start;

    start = gett();
    Int p(40);
    printf("%d\n", ((Impl_Int*)p.v->data[Impl_Int_classId])->fib((void*)&p));
    printf("%ld\n", gett() - start);

    start = gett();
    printf("%d\n", ((Fib2*)p.v->data[Fib2_classId])->fib((void*)&p));
    printf("%ld\n", gett() - start);

    start = gett();
    printf("%d\n", ((Fib3*)p.v->data[Fib3_classId])->fib((void*)&p));
    printf("%ld\n", gett() - start);
  }
};

int main() {
  /*
  traitid::bench();

  traitunrole::bench();
  traitlrumap::bench();
  traitmap::bench();
  */
  long start;


  start = gett();
  printf("%d\n", fib(40));
  printf("%ld\n", gett() - start);


  start = gett();
  printf("%d\n", Fib(40).fib());
  printf("%ld\n", gett() - start);
  start = gett();
  printf("%d\n", Fib2(40).fib());
  printf("%ld\n", gett() - start);

//  trait1::bench();
//  trait2::bench();

  return 0;
}
