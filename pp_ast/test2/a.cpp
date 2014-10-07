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

int Exp_classId = Class_genId();
struct Exp{

  int id;
};


int EInt_classId = Class_genId();
struct EInt:Exp{

  int id;
  EInt (int x):id(EInt_classId), x(x){

  }
  int x;
};


int EAdd_classId = Class_genId();
struct EAdd:Exp{

  int id;
  EAdd (Exp* x, Exp* y):id(EAdd_classId), x(x), y(y){

  }
  Exp* x;
  Exp* y;
};


struct Eval{

  int(*eval)(Class*);
};

Vec* Eval_v = newVec();

int Eval_EInt_eval(Class* self_) {
  EInt* self = ((EInt*)self_);
  return (self -> x);
} 
Eval* newEval_EInt() {
  Eval (* impl) = (new Eval());
  setVec(Eval_v, EInt_classId, ((void*)impl));
  ((impl -> eval) = (& Eval_EInt_eval));
  return impl;
} 
Eval* Eval_EInt_ = newEval_EInt();

int Eval_EAdd_eval(Class* self_) {
  EAdd* self = ((EAdd*)self_);
  return ((((Eval*)(Eval_v -> data)[((* (self -> x)) . id)]) -> eval)(((Class*)(& (* (self -> x))))) + (((Eval*)(Eval_v -> data)[((* (self -> y)) . id)]) -> eval)(((Class*)(& (* (self -> y))))));
} 
Eval* newEval_EAdd() {
  Eval (* impl) = (new Eval());
  setVec(Eval_v, EAdd_classId, ((void*)impl));
  ((impl -> eval) = (& Eval_EAdd_eval));
  return impl;
} 
Eval* Eval_EAdd_ = newEval_EAdd();

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

int main() {
{
    long start = gett();
    int result = fib(40);
    printf("fib %d %d %ld\n", 40, result, (gett() - start));
    (start = gett());
    Int i(20);
    ((i . x) = 40);
    (result = (((Fib*)(Fib_v -> data)[(i . id)]) -> fib)(((Class*)(& i))));
    printf("fib %d %d %ld\n", (i . x), result, (gett() - start));
    printf("eval 40 = %d\n", (((Eval*)(Eval_v -> data)[(i . id)]) -> eval)(((Class*)(& i))));
    EAdd add((new EInt(1)), (new EInt(2)));
    printf("eval 1 + 2 = %d\n", (((Eval*)(Eval_v -> data)[(add . id)]) -> eval)(((Class*)(& add))));
    return 0;
  }
} 

