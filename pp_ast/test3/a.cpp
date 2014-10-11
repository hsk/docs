#include "core.h"
#include <stdio.h>
int fib(int a) {
  if ((a < 2))
    return 1;
  else
    return (fib((a - 2)) + fib((a - 1)));
} 
struct Fib{

  int(*fib)(Class*);
};

Vec* Fib_v = newVec();

int Int_classId = Class_genId();
struct Int{

  int id;
  Int (int x):id(Int_classId), x(x){

  }
  int x;
};


int Fib_Int_fib(Class* self_) {
  Int* self = ((Int*)self_);
  if (((self -> x) < 2))
    return 1;
  else {
    Int p1(((self -> x) - 2));
    Int p2(((self -> x) - 1));
    return ((((Fib*)(Fib_v -> data)[(p1 . id)]) -> fib)(((Class*)(& p1))) + (((Fib*)(Fib_v -> data)[(p2 . id)]) -> fib)(((Class*)(& p2))));
  }
} 
Fib* newFib_Int() {
  Fib (* impl) = (new Fib());
  setVec(Fib_v, Int_classId, ((void*)impl));
  ((impl -> fib) = (& Fib_Int_fib));
  return impl;
} 
Fib* Fib_Int_ = newFib_Int();

int E_classId = Class_genId();
struct E{

  int id;
  E ():id(E_classId){

  }
};


int EInt_classId = Class_genId();
struct EInt:E{

  EInt (int x):x(x){
    (id = EInt_classId);
{

    }
  }
  int x;
};


int EAdd_classId = Class_genId();
struct EAdd:E{

  EAdd (E* x, E* y):x(x), y(y){
    (id = EAdd_classId);
{

    }
  }
  E* x;
  E* y;
};


int EMul_classId = Class_genId();
struct EMul:E{

  EMul (E* x, E* y):x(x), y(y){
    (id = EMul_classId);
{

    }
  }
  E* x;
  E* y;
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

int Eval_EMul_eval(Class* self_) {
  EMul* self = ((EMul*)self_);
  return ((((Eval*)(Eval_v -> data)[((* (self -> x)) . id)]) -> eval)(((Class*)(& (* (self -> x))))) * (((Eval*)(Eval_v -> data)[((* (self -> y)) . id)]) -> eval)(((Class*)(& (* (self -> y))))));
} 
Eval* newEval_EMul() {
  Eval (* impl) = (new Eval());
  setVec(Eval_v, EMul_classId, ((void*)impl));
  ((impl -> eval) = (& Eval_EMul_eval));
  return impl;
} 
Eval* Eval_EMul_ = newEval_EMul();

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
  long start = gett();
  int result = fib(40);
  printf("fib %d %d %ld\n", 40, result, (gett() - start));
  (start = gett());
  Int i(20);
  ((i . x) = 40);
  (result = (((Fib*)(Fib_v -> data)[(i . id)]) -> fib)(((Class*)(& i))));
  printf("fib %d %d %ld\n", (i . x), result, (gett() - start));
  printf("eval 40 = %d\n", (((Eval*)(Eval_v -> data)[(i . id)]) -> eval)(((Class*)(& i))));
  EInt i2(41);
  printf("eval 41 = %d\n", (((Eval*)(Eval_v -> data)[(i2 . id)]) -> eval)(((Class*)(& i2))));
  EAdd add((new EInt(1)), (new EInt(22)));
  printf("eval 1 + 22 = %d\n", (((Eval*)(Eval_v -> data)[(add . id)]) -> eval)(((Class*)(& add))));
  EMul mul((new EAdd((new EInt(1)), (new EInt(2)))), (new EInt(111)));
  printf("eval (1+2) * 111= %d\n", (((Eval*)(Eval_v -> data)[(mul . id)]) -> eval)(((Class*)(& mul))));
  return 0;
} 

