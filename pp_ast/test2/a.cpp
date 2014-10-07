#include "core.h"
#include <stdio.h>
int fib(int a) {
{
    if ((a < 2))
      return 1;
    
    return (fib((a - 2)) + fib((a - 1)));
  }
} 
struct Fib{

  int(*fib)(Class*);
};

Vec* Fib_v = newVec();

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

int Add_classId = Class_genId();
struct Add{

  int id;
  Add (Class* x, Class* y):id(Add_classId), x(x), y(y){
{

    }
  }
  Class* x;
  Class* y;
};


struct Eval{

  int(*eval)(Class*);
};

Vec* Eval_v = newVec();

int Eval_Int_eval(Class* self_) {
  Int* self = ((Int*)self_);
  return (self -> x);
} 
Eval* newEval_Int() {
  Eval (* impl) = (new Eval());
  setVec(Eval_v, Int_classId, ((void*)impl));
  ((impl -> eval) = (& Eval_Int_eval));
  return impl;
} 
Eval* Eval_Int_ = newEval_Int();

int Eval_Add_eval(Class* self_) {
  Add* self = ((Add*)self_);
  return ((((Eval*)(Eval_v -> data)[((* (self -> x)) . id)]) -> eval)(((Class*)(& (* (self -> x))))) + (((Eval*)(Eval_v -> data)[((* (self -> y)) . id)]) -> eval)(((Class*)(& (* (self -> y))))));
} 
Eval* newEval_Add() {
  Eval (* impl) = (new Eval());
  setVec(Eval_v, Add_classId, ((void*)impl));
  ((impl -> eval) = (& Eval_Add_eval));
  return impl;
} 
Eval* Eval_Add_ = newEval_Add();

int main() {
{
    long start = gett();
    int result = fib(40);
    printf("fib %d %d %ld\n", 40, result, (gett() - start));
    (start = gett());
    Int f(20);
    ((f . x) = 40);
    (result = (((Fib*)(Fib_v -> data)[(f . id)]) -> fib)(((Class*)(& f))));
    printf("fib %d %d %ld\n", (f . x), result, (gett() - start));
    printf("eval %d\n", (((Eval*)(Eval_v -> data)[(f . id)]) -> eval)(((Class*)(& f))));
    Int f2(30);
    Add add(((Class*)(new Int(1))), ((Class*)(new Int(2))));
    printf("eval %d\n", (((Eval*)(Eval_v -> data)[(add . id)]) -> eval)(((Class*)(& add))));
    return 0;
  }
} 

