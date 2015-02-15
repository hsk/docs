#include "fib2.h"
int Int_classId = Class_genId();
struct Int{

  int id;
  int x;
  Int (int x):id(Int_classId), x(x){

  }
};

struct Fib{

  int(*fib)(Class*);
};

Vec* Fib_v = newVec();

int Fib_Int_fib(Class* self_) {
  Int* self = ((Int*)self_);
  if (((self -> x) < 2))
    return 1;
  
  Int p1(((self -> x) - 2));
  Int p2(((self -> x) - 1));
  return ((((Fib*)(Fib_v -> data)[(p1 . id)]) -> fib)(((Class*)(& p1))) + (((Fib*)(Fib_v -> data)[(p2 . id)]) -> fib)(((Class*)(& p2))));
} 
Fib* newFib_Int() {
  Fib (* impl) = (new Fib());
  setVec(Fib_v, Int_classId, ((void*)impl));
  ((impl -> fib) = (& Fib_Int_fib));
  return impl;
} 
Fib* Fib_Int_ = newFib_Int();

int main() {
  long start = gett();
  Int p(40);
  printf("%d\n", (((Fib*)(Fib_v -> data)[(p . id)]) -> fib)(((Class*)(& p))));
  printf("%ld\n", (gett() - start));
  return 0;
} 

