#include "core.h"
#include <stdio.h>
struct Fib{

  int(*fib)(Class*);
};

Vec* Fib_v = newVec();

struct GetX{

  int(*getX)(Class*);
  void(*setX)(Class*, int);
};

Vec* GetX_v = newVec();

int Int_classId = Class_genId();
struct Int{

  int id;
  Int (int x):id(Int_classId), x(x){
{

    }
  }
  int x;
};


int Fib_Int_fib(Class* self_) {
  Int* self = ((Int*)self_);
{
    if (((self -> x) < 2))
      return 1;
    else {
      Int p1(((self -> x) - 2));
      Int p2(((self -> x) - 1));
      return ((((Fib*)(Fib_v -> data)[(p1 . id)]) -> fib)(((Class*)(& p1))) + (((Fib*)(Fib_v -> data)[(p2 . id)]) -> fib)(((Class*)(& p2))));
    }
  }
} 
Fib* newFib_Int() {
  Fib (* impl) = (new Fib());
  setVec(Fib_v, Int_classId, ((void*)impl));
  ((impl -> fib) = (& Fib_Int_fib));
  return impl;
} 
Fib* Fib_Int_ = newFib_Int();

int GetX_Int_getX(Class* self_) {
  Int* self = ((Int*)self_);
{
    return (self -> x);
  }
} 
void GetX_Int_setX(Class* self_, int x) {
  Int* self = ((Int*)self_);
{
    ((self -> x) = x);
  }
} 
GetX* newGetX_Int() {
  GetX (* impl) = (new GetX());
  setVec(GetX_v, Int_classId, ((void*)impl));
  ((impl -> getX) = (& GetX_Int_getX));
  ((impl -> setX) = (& GetX_Int_setX));
  return impl;
} 
GetX* GetX_Int_ = newGetX_Int();

int fib(int a) {
{
    if ((a < 2))
      return 1;
    
    return (fib((a - 2)) + fib((a - 1)));
  }
} 
int main() {
{
    long start = gett();
    int result = fib(40);
    printf("fib %d %d %ld\n", 40, result, (gett() - start));
    (start = gett());
    Int f(20);
    ((f . x) = 40);
    (result = (((Fib*)(Fib_v -> data)[(f . id)]) -> fib)(((Class*)(& f))));
    printf("fib %d %d %ld\n", (((GetX*)(GetX_v -> data)[(f . id)]) -> getX)(((Class*)(& f))), result, (gett() - start));
    return 0;
  }
} 

