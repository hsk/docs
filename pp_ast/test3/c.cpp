#include "core.h"
#include <stdio.h>
int S1_classId = Class_genId();
struct S1{

  int id;
  S1 ():id(S1_classId){

  }
};


int S1child1_classId = Class_genId();
struct S1child1:S1{

  S1child1 (int a):a(a){
    (id = S1child1_classId);
{

    }
  }
  int a;
};


int S1child2_classId = Class_genId();
struct S1child2:S1{

  S1child2 (int a):a(a){
    (id = S1child2_classId);
{

    }
  }
  int a;
};


struct Trait1{

  int(*trait1)(Class*);
};

Vec* Trait1_v = newVec();

int Trait1_S1child1_trait1(Class* self_) {
  S1child1* self = ((S1child1*)self_);
{
    return (self -> a);
  }
} 
Trait1* newTrait1_S1child1() {
  Trait1 (* impl) = (new Trait1());
  setVec(Trait1_v, S1child1_classId, ((void*)impl));
  ((impl -> trait1) = (& Trait1_S1child1_trait1));
  return impl;
} 
Trait1* Trait1_S1child1_ = newTrait1_S1child1();

int Trait1_S1child2_trait1(Class* self_) {
  S1child2* self = ((S1child2*)self_);
{
    return (self -> a);
  }
} 
Trait1* newTrait1_S1child2() {
  Trait1 (* impl) = (new Trait1());
  setVec(Trait1_v, S1child2_classId, ((void*)impl));
  ((impl -> trait1) = (& Trait1_S1child2_trait1));
  return impl;
} 
Trait1* Trait1_S1child2_ = newTrait1_S1child2();

int S2_classId = Class_genId();
struct S2{

  int id;
  S2 ():id(S2_classId){

  }
};


int S2child1_classId = Class_genId();
struct S2child1:S2{

  S2child1 (int a):a(a){
    (id = S2child1_classId);
{

    }
  }
  int a;
};


int S2child2_classId = Class_genId();
struct S2child2:S2{

  S2child2 (int a):a(a){
    (id = S2child2_classId);
{

    }
  }
  int a;
};


int S2child3_classId = Class_genId();
struct S2child3:S2{

  S2child3 (int a):a(a){
    (id = S2child3_classId);
{

    }
  }
  int a;
};


struct Trait2{

  int(*trait2)(Class*);
};

Vec* Trait2_v = newVec();

int Trait2_S2child1_trait2(Class* self_) {
  S2child1* self = ((S2child1*)self_);
{
    return (self -> a);
  }
} 
Trait2* newTrait2_S2child1() {
  Trait2 (* impl) = (new Trait2());
  setVec(Trait2_v, S2child1_classId, ((void*)impl));
  ((impl -> trait2) = (& Trait2_S2child1_trait2));
  return impl;
} 
Trait2* Trait2_S2child1_ = newTrait2_S2child1();

int Trait2_S2child2_trait2(Class* self_) {
  S2child2* self = ((S2child2*)self_);
{
    return (self -> a);
  }
} 
Trait2* newTrait2_S2child2() {
  Trait2 (* impl) = (new Trait2());
  setVec(Trait2_v, S2child2_classId, ((void*)impl));
  ((impl -> trait2) = (& Trait2_S2child2_trait2));
  return impl;
} 
Trait2* Trait2_S2child2_ = newTrait2_S2child2();

int Trait2_S2child3_trait2(Class* self_) {
  S2child3* self = ((S2child3*)self_);
{
    return (self -> a);
  }
} 
Trait2* newTrait2_S2child3() {
  Trait2 (* impl) = (new Trait2());
  setVec(Trait2_v, S2child3_classId, ((void*)impl));
  ((impl -> trait2) = (& Trait2_S2child3_trait2));
  return impl;
} 
Trait2* Trait2_S2child3_ = newTrait2_S2child3();

int S3_classId = Class_genId();
struct S3{

  int id;
  S3 ():id(S3_classId){

  }
};


int S3child1_classId = Class_genId();
struct S3child1:S3{

  S3child1 (int a):a(a){
    (id = S3child1_classId);
{

    }
  }
  int a;
};


int S3child2_classId = Class_genId();
struct S3child2:S3{

  S3child2 (int a):a(a){
    (id = S3child2_classId);
{

    }
  }
  int a;
};


int S3child3_classId = Class_genId();
struct S3child3:S3{

  S3child3 (int a):a(a){
    (id = S3child3_classId);
{

    }
  }
  int a;
};


struct Trait3{

  int(*trait3)(Class*);
};

Vec* Trait3_v = newVec();

int Trait3_S3child1_trait3(Class* self_) {
  S3child1* self = ((S3child1*)self_);
{
    return (self -> a);
  }
} 
Trait3* newTrait3_S3child1() {
  Trait3 (* impl) = (new Trait3());
  setVec(Trait3_v, S3child1_classId, ((void*)impl));
  ((impl -> trait3) = (& Trait3_S3child1_trait3));
  return impl;
} 
Trait3* Trait3_S3child1_ = newTrait3_S3child1();

int Trait3_S3child2_trait3(Class* self_) {
  S3child2* self = ((S3child2*)self_);
{
    return (self -> a);
  }
} 
Trait3* newTrait3_S3child2() {
  Trait3 (* impl) = (new Trait3());
  setVec(Trait3_v, S3child2_classId, ((void*)impl));
  ((impl -> trait3) = (& Trait3_S3child2_trait3));
  return impl;
} 
Trait3* Trait3_S3child2_ = newTrait3_S3child2();

int Trait1_S3child1_trait1(Class* self_) {
  S3child1* self = ((S3child1*)self_);
{
    return (self -> a);
  }
} 
Trait1* newTrait1_S3child1() {
  Trait1 (* impl) = (new Trait1());
  setVec(Trait1_v, S3child1_classId, ((void*)impl));
  ((impl -> trait1) = (& Trait1_S3child1_trait1));
  return impl;
} 
Trait1* Trait1_S3child1_ = newTrait1_S3child1();

int S4_classId = Class_genId();
struct S4{

  int id;
  S4 ():id(S4_classId){

  }
};


int S4child1_classId = Class_genId();
struct S4child1:S4{

  S4child1 (int a):a(a){
    (id = S4child1_classId);
{

    }
  }
  int a;
};


int S4child2_classId = Class_genId();
struct S4child2:S4{

  S4child2 (int a):a(a){
    (id = S4child2_classId);
{

    }
  }
  int a;
};


int S4child3_classId = Class_genId();
struct S4child3:S4{

  S4child3 (int a):a(a){
    (id = S4child3_classId);
{

    }
  }
  int a;
};


struct Trait4{

  int(*trait4)(Class*);
};

Vec* Trait4_v = newVec();

int Trait4_S4child1_trait4(Class* self_) {
  S4child1* self = ((S4child1*)self_);
{
    return (self -> a);
  }
} 
Trait4* newTrait4_S4child1() {
  Trait4 (* impl) = (new Trait4());
  setVec(Trait4_v, S4child1_classId, ((void*)impl));
  ((impl -> trait4) = (& Trait4_S4child1_trait4));
  return impl;
} 
Trait4* Trait4_S4child1_ = newTrait4_S4child1();

int Trait4_S4child2_trait4(Class* self_) {
  S4child2* self = ((S4child2*)self_);
{
    return (self -> a);
  }
} 
Trait4* newTrait4_S4child2() {
  Trait4 (* impl) = (new Trait4());
  setVec(Trait4_v, S4child2_classId, ((void*)impl));
  ((impl -> trait4) = (& Trait4_S4child2_trait4));
  return impl;
} 
Trait4* Trait4_S4child2_ = newTrait4_S4child2();

int Trait1_S4child1_trait1(Class* self_) {
  S4child1* self = ((S4child1*)self_);
{
    return (self -> a);
  }
} 
Trait1* newTrait1_S4child1() {
  Trait1 (* impl) = (new Trait1());
  setVec(Trait1_v, S4child1_classId, ((void*)impl));
  ((impl -> trait1) = (& Trait1_S4child1_trait1));
  return impl;
} 
Trait1* Trait1_S4child1_ = newTrait1_S4child1();

int S5_classId = Class_genId();
struct S5{

  int id;
  S5 ():id(S5_classId){

  }
};


int S5child1_classId = Class_genId();
struct S5child1:S5{

  S5child1 (int a):a(a){
    (id = S5child1_classId);
{

    }
  }
  int a;
};


int S5child2_classId = Class_genId();
struct S5child2:S5{

  S5child2 (int a):a(a){
    (id = S5child2_classId);
{

    }
  }
  int a;
};


int S5child3_classId = Class_genId();
struct S5child3:S5{

  S5child3 (int a):a(a){
    (id = S5child3_classId);
{

    }
  }
  int a;
};


int S5child4_classId = Class_genId();
struct S5child4:S5{

  S5child4 (int a):a(a){
    (id = S5child4_classId);
{

    }
  }
  int a;
};


struct Trait5{

  int(*trait5)(Class*);
};

Vec* Trait5_v = newVec();

int Trait5_S5child1_trait5(Class* self_) {
  S5child1* self = ((S5child1*)self_);
{
    return (self -> a);
  }
} 
Trait5* newTrait5_S5child1() {
  Trait5 (* impl) = (new Trait5());
  setVec(Trait5_v, S5child1_classId, ((void*)impl));
  ((impl -> trait5) = (& Trait5_S5child1_trait5));
  return impl;
} 
Trait5* Trait5_S5child1_ = newTrait5_S5child1();

int Trait5_S5child2_trait5(Class* self_) {
  S5child2* self = ((S5child2*)self_);
{
    return (self -> a);
  }
} 
Trait5* newTrait5_S5child2() {
  Trait5 (* impl) = (new Trait5());
  setVec(Trait5_v, S5child2_classId, ((void*)impl));
  ((impl -> trait5) = (& Trait5_S5child2_trait5));
  return impl;
} 
Trait5* Trait5_S5child2_ = newTrait5_S5child2();

int Trait2_S5child1_trait2(Class* self_) {
  S5child1* self = ((S5child1*)self_);
{
    return (self -> a);
  }
} 
Trait2* newTrait2_S5child1() {
  Trait2 (* impl) = (new Trait2());
  setVec(Trait2_v, S5child1_classId, ((void*)impl));
  ((impl -> trait2) = (& Trait2_S5child1_trait2));
  return impl;
} 
Trait2* Trait2_S5child1_ = newTrait2_S5child1();

int S6_classId = Class_genId();
struct S6{

  int id;
  S6 ():id(S6_classId){

  }
};


int S6child1_classId = Class_genId();
struct S6child1:S6{

  S6child1 (int a):a(a){
    (id = S6child1_classId);
{

    }
  }
  int a;
};


struct Trait6{

  int(*trait6)(Class*);
};

Vec* Trait6_v = newVec();

int Trait6_S6child1_trait6(Class* self_) {
  S6child1* self = ((S6child1*)self_);
{
    return (self -> a);
  }
} 
Trait6* newTrait6_S6child1() {
  Trait6 (* impl) = (new Trait6());
  setVec(Trait6_v, S6child1_classId, ((void*)impl));
  ((impl -> trait6) = (& Trait6_S6child1_trait6));
  return impl;
} 
Trait6* Trait6_S6child1_ = newTrait6_S6child1();

int Trait2_S6child1_trait2(Class* self_) {
  S6child1* self = ((S6child1*)self_);
{
    return (self -> a);
  }
} 
Trait2* newTrait2_S6child1() {
  Trait2 (* impl) = (new Trait2());
  setVec(Trait2_v, S6child1_classId, ((void*)impl));
  ((impl -> trait2) = (& Trait2_S6child1_trait2));
  return impl;
} 
Trait2* Trait2_S6child1_ = newTrait2_S6child1();

int S7_classId = Class_genId();
struct S7{

  int id;
  S7 ():id(S7_classId){

  }
};


int S7child1_classId = Class_genId();
struct S7child1:S7{

  S7child1 (int a):a(a){
    (id = S7child1_classId);
{

    }
  }
  int a;
};


int S7child2_classId = Class_genId();
struct S7child2:S7{

  S7child2 (int a):a(a){
    (id = S7child2_classId);
{

    }
  }
  int a;
};


int S7child3_classId = Class_genId();
struct S7child3:S7{

  S7child3 (int a):a(a){
    (id = S7child3_classId);
{

    }
  }
  int a;
};


int S7child4_classId = Class_genId();
struct S7child4:S7{

  S7child4 (int a):a(a){
    (id = S7child4_classId);
{

    }
  }
  int a;
};


struct Trait7{

  int(*trait7)(Class*);
};

Vec* Trait7_v = newVec();

int Trait7_S7child1_trait7(Class* self_) {
  S7child1* self = ((S7child1*)self_);
{
    return (self -> a);
  }
} 
Trait7* newTrait7_S7child1() {
  Trait7 (* impl) = (new Trait7());
  setVec(Trait7_v, S7child1_classId, ((void*)impl));
  ((impl -> trait7) = (& Trait7_S7child1_trait7));
  return impl;
} 
Trait7* Trait7_S7child1_ = newTrait7_S7child1();

int Trait7_S7child2_trait7(Class* self_) {
  S7child2* self = ((S7child2*)self_);
{
    return (self -> a);
  }
} 
Trait7* newTrait7_S7child2() {
  Trait7 (* impl) = (new Trait7());
  setVec(Trait7_v, S7child2_classId, ((void*)impl));
  ((impl -> trait7) = (& Trait7_S7child2_trait7));
  return impl;
} 
Trait7* Trait7_S7child2_ = newTrait7_S7child2();

int Trait7_S7child3_trait7(Class* self_) {
  S7child3* self = ((S7child3*)self_);
{
    return (self -> a);
  }
} 
Trait7* newTrait7_S7child3() {
  Trait7 (* impl) = (new Trait7());
  setVec(Trait7_v, S7child3_classId, ((void*)impl));
  ((impl -> trait7) = (& Trait7_S7child3_trait7));
  return impl;
} 
Trait7* Trait7_S7child3_ = newTrait7_S7child3();

int Trait2_S7child1_trait2(Class* self_) {
  S7child1* self = ((S7child1*)self_);
{
    return (self -> a);
  }
} 
Trait2* newTrait2_S7child1() {
  Trait2 (* impl) = (new Trait2());
  setVec(Trait2_v, S7child1_classId, ((void*)impl));
  ((impl -> trait2) = (& Trait2_S7child1_trait2));
  return impl;
} 
Trait2* Trait2_S7child1_ = newTrait2_S7child1();

int S8_classId = Class_genId();
struct S8{

  int id;
  S8 ():id(S8_classId){

  }
};


int S8child1_classId = Class_genId();
struct S8child1:S8{

  S8child1 (int a):a(a){
    (id = S8child1_classId);
{

    }
  }
  int a;
};


int S8child2_classId = Class_genId();
struct S8child2:S8{

  S8child2 (int a):a(a){
    (id = S8child2_classId);
{

    }
  }
  int a;
};


int S8child3_classId = Class_genId();
struct S8child3:S8{

  S8child3 (int a):a(a){
    (id = S8child3_classId);
{

    }
  }
  int a;
};


int S8child4_classId = Class_genId();
struct S8child4:S8{

  S8child4 (int a):a(a){
    (id = S8child4_classId);
{

    }
  }
  int a;
};


struct Trait8{

  int(*trait8)(Class*);
};

Vec* Trait8_v = newVec();

int Trait8_S8child1_trait8(Class* self_) {
  S8child1* self = ((S8child1*)self_);
{
    return (self -> a);
  }
} 
Trait8* newTrait8_S8child1() {
  Trait8 (* impl) = (new Trait8());
  setVec(Trait8_v, S8child1_classId, ((void*)impl));
  ((impl -> trait8) = (& Trait8_S8child1_trait8));
  return impl;
} 
Trait8* Trait8_S8child1_ = newTrait8_S8child1();

int Trait8_S8child2_trait8(Class* self_) {
  S8child2* self = ((S8child2*)self_);
{
    return (self -> a);
  }
} 
Trait8* newTrait8_S8child2() {
  Trait8 (* impl) = (new Trait8());
  setVec(Trait8_v, S8child2_classId, ((void*)impl));
  ((impl -> trait8) = (& Trait8_S8child2_trait8));
  return impl;
} 
Trait8* Trait8_S8child2_ = newTrait8_S8child2();

int Trait8_S8child3_trait8(Class* self_) {
  S8child3* self = ((S8child3*)self_);
{
    return (self -> a);
  }
} 
Trait8* newTrait8_S8child3() {
  Trait8 (* impl) = (new Trait8());
  setVec(Trait8_v, S8child3_classId, ((void*)impl));
  ((impl -> trait8) = (& Trait8_S8child3_trait8));
  return impl;
} 
Trait8* Trait8_S8child3_ = newTrait8_S8child3();

int Trait6_S8child1_trait6(Class* self_) {
  S8child1* self = ((S8child1*)self_);
{
    return (self -> a);
  }
} 
Trait6* newTrait6_S8child1() {
  Trait6 (* impl) = (new Trait6());
  setVec(Trait6_v, S8child1_classId, ((void*)impl));
  ((impl -> trait6) = (& Trait6_S8child1_trait6));
  return impl;
} 
Trait6* Trait6_S8child1_ = newTrait6_S8child1();

int S9_classId = Class_genId();
struct S9{

  int id;
  S9 ():id(S9_classId){

  }
};


int S9child1_classId = Class_genId();
struct S9child1:S9{

  S9child1 (int a):a(a){
    (id = S9child1_classId);
{

    }
  }
  int a;
};


struct Trait9{

  int(*trait9)(Class*);
};

Vec* Trait9_v = newVec();

int Trait9_S9child1_trait9(Class* self_) {
  S9child1* self = ((S9child1*)self_);
{
    return (self -> a);
  }
} 
Trait9* newTrait9_S9child1() {
  Trait9 (* impl) = (new Trait9());
  setVec(Trait9_v, S9child1_classId, ((void*)impl));
  ((impl -> trait9) = (& Trait9_S9child1_trait9));
  return impl;
} 
Trait9* Trait9_S9child1_ = newTrait9_S9child1();

int Trait5_S9child1_trait5(Class* self_) {
  S9child1* self = ((S9child1*)self_);
{
    return (self -> a);
  }
} 
Trait5* newTrait5_S9child1() {
  Trait5 (* impl) = (new Trait5());
  setVec(Trait5_v, S9child1_classId, ((void*)impl));
  ((impl -> trait5) = (& Trait5_S9child1_trait5));
  return impl;
} 
Trait5* Trait5_S9child1_ = newTrait5_S9child1();

int S10_classId = Class_genId();
struct S10{

  int id;
  S10 ():id(S10_classId){

  }
};


int S10child1_classId = Class_genId();
struct S10child1:S10{

  S10child1 (int a):a(a){
    (id = S10child1_classId);
{

    }
  }
  int a;
};


int S10child2_classId = Class_genId();
struct S10child2:S10{

  S10child2 (int a):a(a){
    (id = S10child2_classId);
{

    }
  }
  int a;
};


int S10child3_classId = Class_genId();
struct S10child3:S10{

  S10child3 (int a):a(a){
    (id = S10child3_classId);
{

    }
  }
  int a;
};


struct Trait10{

  int(*trait10)(Class*);
};

Vec* Trait10_v = newVec();

int Trait10_S10child1_trait10(Class* self_) {
  S10child1* self = ((S10child1*)self_);
{
    return (self -> a);
  }
} 
Trait10* newTrait10_S10child1() {
  Trait10 (* impl) = (new Trait10());
  setVec(Trait10_v, S10child1_classId, ((void*)impl));
  ((impl -> trait10) = (& Trait10_S10child1_trait10));
  return impl;
} 
Trait10* Trait10_S10child1_ = newTrait10_S10child1();

int Trait10_S10child2_trait10(Class* self_) {
  S10child2* self = ((S10child2*)self_);
{
    return (self -> a);
  }
} 
Trait10* newTrait10_S10child2() {
  Trait10 (* impl) = (new Trait10());
  setVec(Trait10_v, S10child2_classId, ((void*)impl));
  ((impl -> trait10) = (& Trait10_S10child2_trait10));
  return impl;
} 
Trait10* Trait10_S10child2_ = newTrait10_S10child2();

int Trait3_S10child1_trait3(Class* self_) {
  S10child1* self = ((S10child1*)self_);
{
    return (self -> a);
  }
} 
Trait3* newTrait3_S10child1() {
  Trait3 (* impl) = (new Trait3());
  setVec(Trait3_v, S10child1_classId, ((void*)impl));
  ((impl -> trait3) = (& Trait3_S10child1_trait3));
  return impl;
} 
Trait3* Trait3_S10child1_ = newTrait3_S10child1();

int S11_classId = Class_genId();
struct S11{

  int id;
  S11 ():id(S11_classId){

  }
};


int S11child1_classId = Class_genId();
struct S11child1:S11{

  S11child1 (int a):a(a){
    (id = S11child1_classId);
{

    }
  }
  int a;
};


int S11child2_classId = Class_genId();
struct S11child2:S11{

  S11child2 (int a):a(a){
    (id = S11child2_classId);
{

    }
  }
  int a;
};


int S11child3_classId = Class_genId();
struct S11child3:S11{

  S11child3 (int a):a(a){
    (id = S11child3_classId);
{

    }
  }
  int a;
};


int S11child4_classId = Class_genId();
struct S11child4:S11{

  S11child4 (int a):a(a){
    (id = S11child4_classId);
{

    }
  }
  int a;
};


struct Trait11{

  int(*trait11)(Class*);
};

Vec* Trait11_v = newVec();

int Trait11_S11child1_trait11(Class* self_) {
  S11child1* self = ((S11child1*)self_);
{
    return (self -> a);
  }
} 
Trait11* newTrait11_S11child1() {
  Trait11 (* impl) = (new Trait11());
  setVec(Trait11_v, S11child1_classId, ((void*)impl));
  ((impl -> trait11) = (& Trait11_S11child1_trait11));
  return impl;
} 
Trait11* Trait11_S11child1_ = newTrait11_S11child1();

int Trait11_S11child2_trait11(Class* self_) {
  S11child2* self = ((S11child2*)self_);
{
    return (self -> a);
  }
} 
Trait11* newTrait11_S11child2() {
  Trait11 (* impl) = (new Trait11());
  setVec(Trait11_v, S11child2_classId, ((void*)impl));
  ((impl -> trait11) = (& Trait11_S11child2_trait11));
  return impl;
} 
Trait11* Trait11_S11child2_ = newTrait11_S11child2();

int Trait11_S11child3_trait11(Class* self_) {
  S11child3* self = ((S11child3*)self_);
{
    return (self -> a);
  }
} 
Trait11* newTrait11_S11child3() {
  Trait11 (* impl) = (new Trait11());
  setVec(Trait11_v, S11child3_classId, ((void*)impl));
  ((impl -> trait11) = (& Trait11_S11child3_trait11));
  return impl;
} 
Trait11* Trait11_S11child3_ = newTrait11_S11child3();

int Trait6_S11child1_trait6(Class* self_) {
  S11child1* self = ((S11child1*)self_);
{
    return (self -> a);
  }
} 
Trait6* newTrait6_S11child1() {
  Trait6 (* impl) = (new Trait6());
  setVec(Trait6_v, S11child1_classId, ((void*)impl));
  ((impl -> trait6) = (& Trait6_S11child1_trait6));
  return impl;
} 
Trait6* Trait6_S11child1_ = newTrait6_S11child1();

int S12_classId = Class_genId();
struct S12{

  int id;
  S12 ():id(S12_classId){

  }
};


int S12child1_classId = Class_genId();
struct S12child1:S12{

  S12child1 (int a):a(a){
    (id = S12child1_classId);
{

    }
  }
  int a;
};


int S12child2_classId = Class_genId();
struct S12child2:S12{

  S12child2 (int a):a(a){
    (id = S12child2_classId);
{

    }
  }
  int a;
};


struct Trait12{

  int(*trait12)(Class*);
};

Vec* Trait12_v = newVec();

int Trait12_S12child1_trait12(Class* self_) {
  S12child1* self = ((S12child1*)self_);
{
    return (self -> a);
  }
} 
Trait12* newTrait12_S12child1() {
  Trait12 (* impl) = (new Trait12());
  setVec(Trait12_v, S12child1_classId, ((void*)impl));
  ((impl -> trait12) = (& Trait12_S12child1_trait12));
  return impl;
} 
Trait12* Trait12_S12child1_ = newTrait12_S12child1();

int Trait9_S12child1_trait9(Class* self_) {
  S12child1* self = ((S12child1*)self_);
{
    return (self -> a);
  }
} 
Trait9* newTrait9_S12child1() {
  Trait9 (* impl) = (new Trait9());
  setVec(Trait9_v, S12child1_classId, ((void*)impl));
  ((impl -> trait9) = (& Trait9_S12child1_trait9));
  return impl;
} 
Trait9* Trait9_S12child1_ = newTrait9_S12child1();

int S13_classId = Class_genId();
struct S13{

  int id;
  S13 ():id(S13_classId){

  }
};


int S13child1_classId = Class_genId();
struct S13child1:S13{

  S13child1 (int a):a(a){
    (id = S13child1_classId);
{

    }
  }
  int a;
};


struct Trait13{

  int(*trait13)(Class*);
};

Vec* Trait13_v = newVec();

int Trait13_S13child1_trait13(Class* self_) {
  S13child1* self = ((S13child1*)self_);
{
    return (self -> a);
  }
} 
Trait13* newTrait13_S13child1() {
  Trait13 (* impl) = (new Trait13());
  setVec(Trait13_v, S13child1_classId, ((void*)impl));
  ((impl -> trait13) = (& Trait13_S13child1_trait13));
  return impl;
} 
Trait13* Trait13_S13child1_ = newTrait13_S13child1();

int Trait2_S13child1_trait2(Class* self_) {
  S13child1* self = ((S13child1*)self_);
{
    return (self -> a);
  }
} 
Trait2* newTrait2_S13child1() {
  Trait2 (* impl) = (new Trait2());
  setVec(Trait2_v, S13child1_classId, ((void*)impl));
  ((impl -> trait2) = (& Trait2_S13child1_trait2));
  return impl;
} 
Trait2* Trait2_S13child1_ = newTrait2_S13child1();

int S14_classId = Class_genId();
struct S14{

  int id;
  S14 ():id(S14_classId){

  }
};


int S14child1_classId = Class_genId();
struct S14child1:S14{

  S14child1 (int a):a(a){
    (id = S14child1_classId);
{

    }
  }
  int a;
};


int S14child2_classId = Class_genId();
struct S14child2:S14{

  S14child2 (int a):a(a){
    (id = S14child2_classId);
{

    }
  }
  int a;
};


struct Trait14{

  int(*trait14)(Class*);
};

Vec* Trait14_v = newVec();

int Trait14_S14child1_trait14(Class* self_) {
  S14child1* self = ((S14child1*)self_);
{
    return (self -> a);
  }
} 
Trait14* newTrait14_S14child1() {
  Trait14 (* impl) = (new Trait14());
  setVec(Trait14_v, S14child1_classId, ((void*)impl));
  ((impl -> trait14) = (& Trait14_S14child1_trait14));
  return impl;
} 
Trait14* Trait14_S14child1_ = newTrait14_S14child1();

int Trait14_S14child2_trait14(Class* self_) {
  S14child2* self = ((S14child2*)self_);
{
    return (self -> a);
  }
} 
Trait14* newTrait14_S14child2() {
  Trait14 (* impl) = (new Trait14());
  setVec(Trait14_v, S14child2_classId, ((void*)impl));
  ((impl -> trait14) = (& Trait14_S14child2_trait14));
  return impl;
} 
Trait14* Trait14_S14child2_ = newTrait14_S14child2();

int S15_classId = Class_genId();
struct S15{

  int id;
  S15 ():id(S15_classId){

  }
};


int S15child1_classId = Class_genId();
struct S15child1:S15{

  S15child1 (int a):a(a){
    (id = S15child1_classId);
{

    }
  }
  int a;
};


int S15child2_classId = Class_genId();
struct S15child2:S15{

  S15child2 (int a):a(a){
    (id = S15child2_classId);
{

    }
  }
  int a;
};


struct Trait15{

  int(*trait15)(Class*);
};

Vec* Trait15_v = newVec();

int Trait15_S15child1_trait15(Class* self_) {
  S15child1* self = ((S15child1*)self_);
{
    return (self -> a);
  }
} 
Trait15* newTrait15_S15child1() {
  Trait15 (* impl) = (new Trait15());
  setVec(Trait15_v, S15child1_classId, ((void*)impl));
  ((impl -> trait15) = (& Trait15_S15child1_trait15));
  return impl;
} 
Trait15* Trait15_S15child1_ = newTrait15_S15child1();

int Trait15_S15child2_trait15(Class* self_) {
  S15child2* self = ((S15child2*)self_);
{
    return (self -> a);
  }
} 
Trait15* newTrait15_S15child2() {
  Trait15 (* impl) = (new Trait15());
  setVec(Trait15_v, S15child2_classId, ((void*)impl));
  ((impl -> trait15) = (& Trait15_S15child2_trait15));
  return impl;
} 
Trait15* Trait15_S15child2_ = newTrait15_S15child2();

int Trait3_S15child1_trait3(Class* self_) {
  S15child1* self = ((S15child1*)self_);
{
    return (self -> a);
  }
} 
Trait3* newTrait3_S15child1() {
  Trait3 (* impl) = (new Trait3());
  setVec(Trait3_v, S15child1_classId, ((void*)impl));
  ((impl -> trait3) = (& Trait3_S15child1_trait3));
  return impl;
} 
Trait3* Trait3_S15child1_ = newTrait3_S15child1();

int S16_classId = Class_genId();
struct S16{

  int id;
  S16 ():id(S16_classId){

  }
};


int S16child1_classId = Class_genId();
struct S16child1:S16{

  S16child1 (int a):a(a){
    (id = S16child1_classId);
{

    }
  }
  int a;
};


int S16child2_classId = Class_genId();
struct S16child2:S16{

  S16child2 (int a):a(a){
    (id = S16child2_classId);
{

    }
  }
  int a;
};


int S16child3_classId = Class_genId();
struct S16child3:S16{

  S16child3 (int a):a(a){
    (id = S16child3_classId);
{

    }
  }
  int a;
};


int S16child4_classId = Class_genId();
struct S16child4:S16{

  S16child4 (int a):a(a){
    (id = S16child4_classId);
{

    }
  }
  int a;
};


struct Trait16{

  int(*trait16)(Class*);
};

Vec* Trait16_v = newVec();

int Trait16_S16child1_trait16(Class* self_) {
  S16child1* self = ((S16child1*)self_);
{
    return (self -> a);
  }
} 
Trait16* newTrait16_S16child1() {
  Trait16 (* impl) = (new Trait16());
  setVec(Trait16_v, S16child1_classId, ((void*)impl));
  ((impl -> trait16) = (& Trait16_S16child1_trait16));
  return impl;
} 
Trait16* Trait16_S16child1_ = newTrait16_S16child1();

int Trait16_S16child2_trait16(Class* self_) {
  S16child2* self = ((S16child2*)self_);
{
    return (self -> a);
  }
} 
Trait16* newTrait16_S16child2() {
  Trait16 (* impl) = (new Trait16());
  setVec(Trait16_v, S16child2_classId, ((void*)impl));
  ((impl -> trait16) = (& Trait16_S16child2_trait16));
  return impl;
} 
Trait16* Trait16_S16child2_ = newTrait16_S16child2();

int Trait8_S16child1_trait8(Class* self_) {
  S16child1* self = ((S16child1*)self_);
{
    return (self -> a);
  }
} 
Trait8* newTrait8_S16child1() {
  Trait8 (* impl) = (new Trait8());
  setVec(Trait8_v, S16child1_classId, ((void*)impl));
  ((impl -> trait8) = (& Trait8_S16child1_trait8));
  return impl;
} 
Trait8* Trait8_S16child1_ = newTrait8_S16child1();

int S17_classId = Class_genId();
struct S17{

  int id;
  S17 ():id(S17_classId){

  }
};


int S17child1_classId = Class_genId();
struct S17child1:S17{

  S17child1 (int a):a(a){
    (id = S17child1_classId);
{

    }
  }
  int a;
};


int S17child2_classId = Class_genId();
struct S17child2:S17{

  S17child2 (int a):a(a){
    (id = S17child2_classId);
{

    }
  }
  int a;
};


struct Trait17{

  int(*trait17)(Class*);
};

Vec* Trait17_v = newVec();

int Trait17_S17child1_trait17(Class* self_) {
  S17child1* self = ((S17child1*)self_);
{
    return (self -> a);
  }
} 
Trait17* newTrait17_S17child1() {
  Trait17 (* impl) = (new Trait17());
  setVec(Trait17_v, S17child1_classId, ((void*)impl));
  ((impl -> trait17) = (& Trait17_S17child1_trait17));
  return impl;
} 
Trait17* Trait17_S17child1_ = newTrait17_S17child1();

int Trait6_S17child1_trait6(Class* self_) {
  S17child1* self = ((S17child1*)self_);
{
    return (self -> a);
  }
} 
Trait6* newTrait6_S17child1() {
  Trait6 (* impl) = (new Trait6());
  setVec(Trait6_v, S17child1_classId, ((void*)impl));
  ((impl -> trait6) = (& Trait6_S17child1_trait6));
  return impl;
} 
Trait6* Trait6_S17child1_ = newTrait6_S17child1();

int S18_classId = Class_genId();
struct S18{

  int id;
  S18 ():id(S18_classId){

  }
};


int S18child1_classId = Class_genId();
struct S18child1:S18{

  S18child1 (int a):a(a){
    (id = S18child1_classId);
{

    }
  }
  int a;
};


struct Trait18{

  int(*trait18)(Class*);
};

Vec* Trait18_v = newVec();

int Trait18_S18child1_trait18(Class* self_) {
  S18child1* self = ((S18child1*)self_);
{
    return (self -> a);
  }
} 
Trait18* newTrait18_S18child1() {
  Trait18 (* impl) = (new Trait18());
  setVec(Trait18_v, S18child1_classId, ((void*)impl));
  ((impl -> trait18) = (& Trait18_S18child1_trait18));
  return impl;
} 
Trait18* Trait18_S18child1_ = newTrait18_S18child1();

int Trait10_S18child1_trait10(Class* self_) {
  S18child1* self = ((S18child1*)self_);
{
    return (self -> a);
  }
} 
Trait10* newTrait10_S18child1() {
  Trait10 (* impl) = (new Trait10());
  setVec(Trait10_v, S18child1_classId, ((void*)impl));
  ((impl -> trait10) = (& Trait10_S18child1_trait10));
  return impl;
} 
Trait10* Trait10_S18child1_ = newTrait10_S18child1();

int S19_classId = Class_genId();
struct S19{

  int id;
  S19 ():id(S19_classId){

  }
};


int S19child1_classId = Class_genId();
struct S19child1:S19{

  S19child1 (int a):a(a){
    (id = S19child1_classId);
{

    }
  }
  int a;
};


int S19child2_classId = Class_genId();
struct S19child2:S19{

  S19child2 (int a):a(a){
    (id = S19child2_classId);
{

    }
  }
  int a;
};


struct Trait19{

  int(*trait19)(Class*);
};

Vec* Trait19_v = newVec();

int Trait19_S19child1_trait19(Class* self_) {
  S19child1* self = ((S19child1*)self_);
{
    return (self -> a);
  }
} 
Trait19* newTrait19_S19child1() {
  Trait19 (* impl) = (new Trait19());
  setVec(Trait19_v, S19child1_classId, ((void*)impl));
  ((impl -> trait19) = (& Trait19_S19child1_trait19));
  return impl;
} 
Trait19* Trait19_S19child1_ = newTrait19_S19child1();

int Trait19_S19child2_trait19(Class* self_) {
  S19child2* self = ((S19child2*)self_);
{
    return (self -> a);
  }
} 
Trait19* newTrait19_S19child2() {
  Trait19 (* impl) = (new Trait19());
  setVec(Trait19_v, S19child2_classId, ((void*)impl));
  ((impl -> trait19) = (& Trait19_S19child2_trait19));
  return impl;
} 
Trait19* Trait19_S19child2_ = newTrait19_S19child2();

int Trait8_S19child1_trait8(Class* self_) {
  S19child1* self = ((S19child1*)self_);
{
    return (self -> a);
  }
} 
Trait8* newTrait8_S19child1() {
  Trait8 (* impl) = (new Trait8());
  setVec(Trait8_v, S19child1_classId, ((void*)impl));
  ((impl -> trait8) = (& Trait8_S19child1_trait8));
  return impl;
} 
Trait8* Trait8_S19child1_ = newTrait8_S19child1();

int S20_classId = Class_genId();
struct S20{

  int id;
  S20 ():id(S20_classId){

  }
};


int S20child1_classId = Class_genId();
struct S20child1:S20{

  S20child1 (int a):a(a){
    (id = S20child1_classId);
{

    }
  }
  int a;
};


int S20child2_classId = Class_genId();
struct S20child2:S20{

  S20child2 (int a):a(a){
    (id = S20child2_classId);
{

    }
  }
  int a;
};


int S20child3_classId = Class_genId();
struct S20child3:S20{

  S20child3 (int a):a(a){
    (id = S20child3_classId);
{

    }
  }
  int a;
};


int S20child4_classId = Class_genId();
struct S20child4:S20{

  S20child4 (int a):a(a){
    (id = S20child4_classId);
{

    }
  }
  int a;
};


int S20child5_classId = Class_genId();
struct S20child5:S20{

  S20child5 (int a):a(a){
    (id = S20child5_classId);
{

    }
  }
  int a;
};


struct Trait20{

  int(*trait20)(Class*);
};

Vec* Trait20_v = newVec();

int Trait20_S20child1_trait20(Class* self_) {
  S20child1* self = ((S20child1*)self_);
{
    return (self -> a);
  }
} 
Trait20* newTrait20_S20child1() {
  Trait20 (* impl) = (new Trait20());
  setVec(Trait20_v, S20child1_classId, ((void*)impl));
  ((impl -> trait20) = (& Trait20_S20child1_trait20));
  return impl;
} 
Trait20* Trait20_S20child1_ = newTrait20_S20child1();

int Trait20_S20child2_trait20(Class* self_) {
  S20child2* self = ((S20child2*)self_);
{
    return (self -> a);
  }
} 
Trait20* newTrait20_S20child2() {
  Trait20 (* impl) = (new Trait20());
  setVec(Trait20_v, S20child2_classId, ((void*)impl));
  ((impl -> trait20) = (& Trait20_S20child2_trait20));
  return impl;
} 
Trait20* Trait20_S20child2_ = newTrait20_S20child2();

int Trait20_S20child3_trait20(Class* self_) {
  S20child3* self = ((S20child3*)self_);
{
    return (self -> a);
  }
} 
Trait20* newTrait20_S20child3() {
  Trait20 (* impl) = (new Trait20());
  setVec(Trait20_v, S20child3_classId, ((void*)impl));
  ((impl -> trait20) = (& Trait20_S20child3_trait20));
  return impl;
} 
Trait20* Trait20_S20child3_ = newTrait20_S20child3();

int Trait20_S20child4_trait20(Class* self_) {
  S20child4* self = ((S20child4*)self_);
{
    return (self -> a);
  }
} 
Trait20* newTrait20_S20child4() {
  Trait20 (* impl) = (new Trait20());
  setVec(Trait20_v, S20child4_classId, ((void*)impl));
  ((impl -> trait20) = (& Trait20_S20child4_trait20));
  return impl;
} 
Trait20* Trait20_S20child4_ = newTrait20_S20child4();

int Trait2_S20child1_trait2(Class* self_) {
  S20child1* self = ((S20child1*)self_);
{
    return (self -> a);
  }
} 
Trait2* newTrait2_S20child1() {
  Trait2 (* impl) = (new Trait2());
  setVec(Trait2_v, S20child1_classId, ((void*)impl));
  ((impl -> trait2) = (& Trait2_S20child1_trait2));
  return impl;
} 
Trait2* Trait2_S20child1_ = newTrait2_S20child1();

int S21_classId = Class_genId();
struct S21{

  int id;
  S21 ():id(S21_classId){

  }
};


int S21child1_classId = Class_genId();
struct S21child1:S21{

  S21child1 (int a):a(a){
    (id = S21child1_classId);
{

    }
  }
  int a;
};


int S21child2_classId = Class_genId();
struct S21child2:S21{

  S21child2 (int a):a(a){
    (id = S21child2_classId);
{

    }
  }
  int a;
};


int S21child3_classId = Class_genId();
struct S21child3:S21{

  S21child3 (int a):a(a){
    (id = S21child3_classId);
{

    }
  }
  int a;
};


struct Trait21{

  int(*trait21)(Class*);
};

Vec* Trait21_v = newVec();

int Trait21_S21child1_trait21(Class* self_) {
  S21child1* self = ((S21child1*)self_);
{
    return (self -> a);
  }
} 
Trait21* newTrait21_S21child1() {
  Trait21 (* impl) = (new Trait21());
  setVec(Trait21_v, S21child1_classId, ((void*)impl));
  ((impl -> trait21) = (& Trait21_S21child1_trait21));
  return impl;
} 
Trait21* Trait21_S21child1_ = newTrait21_S21child1();

int Trait21_S21child2_trait21(Class* self_) {
  S21child2* self = ((S21child2*)self_);
{
    return (self -> a);
  }
} 
Trait21* newTrait21_S21child2() {
  Trait21 (* impl) = (new Trait21());
  setVec(Trait21_v, S21child2_classId, ((void*)impl));
  ((impl -> trait21) = (& Trait21_S21child2_trait21));
  return impl;
} 
Trait21* Trait21_S21child2_ = newTrait21_S21child2();

int Trait21_S21child3_trait21(Class* self_) {
  S21child3* self = ((S21child3*)self_);
{
    return (self -> a);
  }
} 
Trait21* newTrait21_S21child3() {
  Trait21 (* impl) = (new Trait21());
  setVec(Trait21_v, S21child3_classId, ((void*)impl));
  ((impl -> trait21) = (& Trait21_S21child3_trait21));
  return impl;
} 
Trait21* Trait21_S21child3_ = newTrait21_S21child3();

int Trait2_S21child1_trait2(Class* self_) {
  S21child1* self = ((S21child1*)self_);
{
    return (self -> a);
  }
} 
Trait2* newTrait2_S21child1() {
  Trait2 (* impl) = (new Trait2());
  setVec(Trait2_v, S21child1_classId, ((void*)impl));
  ((impl -> trait2) = (& Trait2_S21child1_trait2));
  return impl;
} 
Trait2* Trait2_S21child1_ = newTrait2_S21child1();

int S22_classId = Class_genId();
struct S22{

  int id;
  S22 ():id(S22_classId){

  }
};


int S22child1_classId = Class_genId();
struct S22child1:S22{

  S22child1 (int a):a(a){
    (id = S22child1_classId);
{

    }
  }
  int a;
};


int S22child2_classId = Class_genId();
struct S22child2:S22{

  S22child2 (int a):a(a){
    (id = S22child2_classId);
{

    }
  }
  int a;
};


int S22child3_classId = Class_genId();
struct S22child3:S22{

  S22child3 (int a):a(a){
    (id = S22child3_classId);
{

    }
  }
  int a;
};


struct Trait22{

  int(*trait22)(Class*);
};

Vec* Trait22_v = newVec();

int Trait22_S22child1_trait22(Class* self_) {
  S22child1* self = ((S22child1*)self_);
{
    return (self -> a);
  }
} 
Trait22* newTrait22_S22child1() {
  Trait22 (* impl) = (new Trait22());
  setVec(Trait22_v, S22child1_classId, ((void*)impl));
  ((impl -> trait22) = (& Trait22_S22child1_trait22));
  return impl;
} 
Trait22* Trait22_S22child1_ = newTrait22_S22child1();

int Trait22_S22child2_trait22(Class* self_) {
  S22child2* self = ((S22child2*)self_);
{
    return (self -> a);
  }
} 
Trait22* newTrait22_S22child2() {
  Trait22 (* impl) = (new Trait22());
  setVec(Trait22_v, S22child2_classId, ((void*)impl));
  ((impl -> trait22) = (& Trait22_S22child2_trait22));
  return impl;
} 
Trait22* Trait22_S22child2_ = newTrait22_S22child2();

int Trait16_S22child1_trait16(Class* self_) {
  S22child1* self = ((S22child1*)self_);
{
    return (self -> a);
  }
} 
Trait16* newTrait16_S22child1() {
  Trait16 (* impl) = (new Trait16());
  setVec(Trait16_v, S22child1_classId, ((void*)impl));
  ((impl -> trait16) = (& Trait16_S22child1_trait16));
  return impl;
} 
Trait16* Trait16_S22child1_ = newTrait16_S22child1();

int S23_classId = Class_genId();
struct S23{

  int id;
  S23 ():id(S23_classId){

  }
};


int S23child1_classId = Class_genId();
struct S23child1:S23{

  S23child1 (int a):a(a){
    (id = S23child1_classId);
{

    }
  }
  int a;
};


int S23child2_classId = Class_genId();
struct S23child2:S23{

  S23child2 (int a):a(a){
    (id = S23child2_classId);
{

    }
  }
  int a;
};


struct Trait23{

  int(*trait23)(Class*);
};

Vec* Trait23_v = newVec();

int Trait23_S23child1_trait23(Class* self_) {
  S23child1* self = ((S23child1*)self_);
{
    return (self -> a);
  }
} 
Trait23* newTrait23_S23child1() {
  Trait23 (* impl) = (new Trait23());
  setVec(Trait23_v, S23child1_classId, ((void*)impl));
  ((impl -> trait23) = (& Trait23_S23child1_trait23));
  return impl;
} 
Trait23* Trait23_S23child1_ = newTrait23_S23child1();

int Trait21_S23child1_trait21(Class* self_) {
  S23child1* self = ((S23child1*)self_);
{
    return (self -> a);
  }
} 
Trait21* newTrait21_S23child1() {
  Trait21 (* impl) = (new Trait21());
  setVec(Trait21_v, S23child1_classId, ((void*)impl));
  ((impl -> trait21) = (& Trait21_S23child1_trait21));
  return impl;
} 
Trait21* Trait21_S23child1_ = newTrait21_S23child1();

int S24_classId = Class_genId();
struct S24{

  int id;
  S24 ():id(S24_classId){

  }
};


int S24child1_classId = Class_genId();
struct S24child1:S24{

  S24child1 (int a):a(a){
    (id = S24child1_classId);
{

    }
  }
  int a;
};


struct Trait24{

  int(*trait24)(Class*);
};

Vec* Trait24_v = newVec();

int Trait24_S24child1_trait24(Class* self_) {
  S24child1* self = ((S24child1*)self_);
{
    return (self -> a);
  }
} 
Trait24* newTrait24_S24child1() {
  Trait24 (* impl) = (new Trait24());
  setVec(Trait24_v, S24child1_classId, ((void*)impl));
  ((impl -> trait24) = (& Trait24_S24child1_trait24));
  return impl;
} 
Trait24* Trait24_S24child1_ = newTrait24_S24child1();

int Trait19_S24child1_trait19(Class* self_) {
  S24child1* self = ((S24child1*)self_);
{
    return (self -> a);
  }
} 
Trait19* newTrait19_S24child1() {
  Trait19 (* impl) = (new Trait19());
  setVec(Trait19_v, S24child1_classId, ((void*)impl));
  ((impl -> trait19) = (& Trait19_S24child1_trait19));
  return impl;
} 
Trait19* Trait19_S24child1_ = newTrait19_S24child1();

int S25_classId = Class_genId();
struct S25{

  int id;
  S25 ():id(S25_classId){

  }
};


int S25child1_classId = Class_genId();
struct S25child1:S25{

  S25child1 (int a):a(a){
    (id = S25child1_classId);
{

    }
  }
  int a;
};


int S25child2_classId = Class_genId();
struct S25child2:S25{

  S25child2 (int a):a(a){
    (id = S25child2_classId);
{

    }
  }
  int a;
};


int S25child3_classId = Class_genId();
struct S25child3:S25{

  S25child3 (int a):a(a){
    (id = S25child3_classId);
{

    }
  }
  int a;
};


int S25child4_classId = Class_genId();
struct S25child4:S25{

  S25child4 (int a):a(a){
    (id = S25child4_classId);
{

    }
  }
  int a;
};


struct Trait25{

  int(*trait25)(Class*);
};

Vec* Trait25_v = newVec();

int Trait25_S25child1_trait25(Class* self_) {
  S25child1* self = ((S25child1*)self_);
{
    return (self -> a);
  }
} 
Trait25* newTrait25_S25child1() {
  Trait25 (* impl) = (new Trait25());
  setVec(Trait25_v, S25child1_classId, ((void*)impl));
  ((impl -> trait25) = (& Trait25_S25child1_trait25));
  return impl;
} 
Trait25* Trait25_S25child1_ = newTrait25_S25child1();

int Trait25_S25child2_trait25(Class* self_) {
  S25child2* self = ((S25child2*)self_);
{
    return (self -> a);
  }
} 
Trait25* newTrait25_S25child2() {
  Trait25 (* impl) = (new Trait25());
  setVec(Trait25_v, S25child2_classId, ((void*)impl));
  ((impl -> trait25) = (& Trait25_S25child2_trait25));
  return impl;
} 
Trait25* Trait25_S25child2_ = newTrait25_S25child2();

int Trait25_S25child3_trait25(Class* self_) {
  S25child3* self = ((S25child3*)self_);
{
    return (self -> a);
  }
} 
Trait25* newTrait25_S25child3() {
  Trait25 (* impl) = (new Trait25());
  setVec(Trait25_v, S25child3_classId, ((void*)impl));
  ((impl -> trait25) = (& Trait25_S25child3_trait25));
  return impl;
} 
Trait25* Trait25_S25child3_ = newTrait25_S25child3();

int Trait9_S25child1_trait9(Class* self_) {
  S25child1* self = ((S25child1*)self_);
{
    return (self -> a);
  }
} 
Trait9* newTrait9_S25child1() {
  Trait9 (* impl) = (new Trait9());
  setVec(Trait9_v, S25child1_classId, ((void*)impl));
  ((impl -> trait9) = (& Trait9_S25child1_trait9));
  return impl;
} 
Trait9* Trait9_S25child1_ = newTrait9_S25child1();

int S26_classId = Class_genId();
struct S26{

  int id;
  S26 ():id(S26_classId){

  }
};


int S26child1_classId = Class_genId();
struct S26child1:S26{

  S26child1 (int a):a(a){
    (id = S26child1_classId);
{

    }
  }
  int a;
};


int S26child2_classId = Class_genId();
struct S26child2:S26{

  S26child2 (int a):a(a){
    (id = S26child2_classId);
{

    }
  }
  int a;
};


struct Trait26{

  int(*trait26)(Class*);
};

Vec* Trait26_v = newVec();

int Trait26_S26child1_trait26(Class* self_) {
  S26child1* self = ((S26child1*)self_);
{
    return (self -> a);
  }
} 
Trait26* newTrait26_S26child1() {
  Trait26 (* impl) = (new Trait26());
  setVec(Trait26_v, S26child1_classId, ((void*)impl));
  ((impl -> trait26) = (& Trait26_S26child1_trait26));
  return impl;
} 
Trait26* Trait26_S26child1_ = newTrait26_S26child1();

int Trait6_S26child1_trait6(Class* self_) {
  S26child1* self = ((S26child1*)self_);
{
    return (self -> a);
  }
} 
Trait6* newTrait6_S26child1() {
  Trait6 (* impl) = (new Trait6());
  setVec(Trait6_v, S26child1_classId, ((void*)impl));
  ((impl -> trait6) = (& Trait6_S26child1_trait6));
  return impl;
} 
Trait6* Trait6_S26child1_ = newTrait6_S26child1();

int S27_classId = Class_genId();
struct S27{

  int id;
  S27 ():id(S27_classId){

  }
};


int S27child1_classId = Class_genId();
struct S27child1:S27{

  S27child1 (int a):a(a){
    (id = S27child1_classId);
{

    }
  }
  int a;
};


struct Trait27{

  int(*trait27)(Class*);
};

Vec* Trait27_v = newVec();

int Trait27_S27child1_trait27(Class* self_) {
  S27child1* self = ((S27child1*)self_);
{
    return (self -> a);
  }
} 
Trait27* newTrait27_S27child1() {
  Trait27 (* impl) = (new Trait27());
  setVec(Trait27_v, S27child1_classId, ((void*)impl));
  ((impl -> trait27) = (& Trait27_S27child1_trait27));
  return impl;
} 
Trait27* Trait27_S27child1_ = newTrait27_S27child1();

int Trait19_S27child1_trait19(Class* self_) {
  S27child1* self = ((S27child1*)self_);
{
    return (self -> a);
  }
} 
Trait19* newTrait19_S27child1() {
  Trait19 (* impl) = (new Trait19());
  setVec(Trait19_v, S27child1_classId, ((void*)impl));
  ((impl -> trait19) = (& Trait19_S27child1_trait19));
  return impl;
} 
Trait19* Trait19_S27child1_ = newTrait19_S27child1();

int S28_classId = Class_genId();
struct S28{

  int id;
  S28 ():id(S28_classId){

  }
};


int S28child1_classId = Class_genId();
struct S28child1:S28{

  S28child1 (int a):a(a){
    (id = S28child1_classId);
{

    }
  }
  int a;
};


struct Trait28{

  int(*trait28)(Class*);
};

Vec* Trait28_v = newVec();

int Trait28_S28child1_trait28(Class* self_) {
  S28child1* self = ((S28child1*)self_);
{
    return (self -> a);
  }
} 
Trait28* newTrait28_S28child1() {
  Trait28 (* impl) = (new Trait28());
  setVec(Trait28_v, S28child1_classId, ((void*)impl));
  ((impl -> trait28) = (& Trait28_S28child1_trait28));
  return impl;
} 
Trait28* Trait28_S28child1_ = newTrait28_S28child1();

int Trait27_S28child1_trait27(Class* self_) {
  S28child1* self = ((S28child1*)self_);
{
    return (self -> a);
  }
} 
Trait27* newTrait27_S28child1() {
  Trait27 (* impl) = (new Trait27());
  setVec(Trait27_v, S28child1_classId, ((void*)impl));
  ((impl -> trait27) = (& Trait27_S28child1_trait27));
  return impl;
} 
Trait27* Trait27_S28child1_ = newTrait27_S28child1();

int S29_classId = Class_genId();
struct S29{

  int id;
  S29 ():id(S29_classId){

  }
};


int S29child1_classId = Class_genId();
struct S29child1:S29{

  S29child1 (int a):a(a){
    (id = S29child1_classId);
{

    }
  }
  int a;
};


struct Trait29{

  int(*trait29)(Class*);
};

Vec* Trait29_v = newVec();

int Trait29_S29child1_trait29(Class* self_) {
  S29child1* self = ((S29child1*)self_);
{
    return (self -> a);
  }
} 
Trait29* newTrait29_S29child1() {
  Trait29 (* impl) = (new Trait29());
  setVec(Trait29_v, S29child1_classId, ((void*)impl));
  ((impl -> trait29) = (& Trait29_S29child1_trait29));
  return impl;
} 
Trait29* Trait29_S29child1_ = newTrait29_S29child1();

int Trait26_S29child1_trait26(Class* self_) {
  S29child1* self = ((S29child1*)self_);
{
    return (self -> a);
  }
} 
Trait26* newTrait26_S29child1() {
  Trait26 (* impl) = (new Trait26());
  setVec(Trait26_v, S29child1_classId, ((void*)impl));
  ((impl -> trait26) = (& Trait26_S29child1_trait26));
  return impl;
} 
Trait26* Trait26_S29child1_ = newTrait26_S29child1();

int S30_classId = Class_genId();
struct S30{

  int id;
  S30 ():id(S30_classId){

  }
};


int S30child1_classId = Class_genId();
struct S30child1:S30{

  S30child1 (int a):a(a){
    (id = S30child1_classId);
{

    }
  }
  int a;
};


int S30child2_classId = Class_genId();
struct S30child2:S30{

  S30child2 (int a):a(a){
    (id = S30child2_classId);
{

    }
  }
  int a;
};


int S30child3_classId = Class_genId();
struct S30child3:S30{

  S30child3 (int a):a(a){
    (id = S30child3_classId);
{

    }
  }
  int a;
};


int S30child4_classId = Class_genId();
struct S30child4:S30{

  S30child4 (int a):a(a){
    (id = S30child4_classId);
{

    }
  }
  int a;
};


int S30child5_classId = Class_genId();
struct S30child5:S30{

  S30child5 (int a):a(a){
    (id = S30child5_classId);
{

    }
  }
  int a;
};


struct Trait30{

  int(*trait30)(Class*);
};

Vec* Trait30_v = newVec();

int Trait30_S30child1_trait30(Class* self_) {
  S30child1* self = ((S30child1*)self_);
{
    return (self -> a);
  }
} 
Trait30* newTrait30_S30child1() {
  Trait30 (* impl) = (new Trait30());
  setVec(Trait30_v, S30child1_classId, ((void*)impl));
  ((impl -> trait30) = (& Trait30_S30child1_trait30));
  return impl;
} 
Trait30* Trait30_S30child1_ = newTrait30_S30child1();

int Trait30_S30child2_trait30(Class* self_) {
  S30child2* self = ((S30child2*)self_);
{
    return (self -> a);
  }
} 
Trait30* newTrait30_S30child2() {
  Trait30 (* impl) = (new Trait30());
  setVec(Trait30_v, S30child2_classId, ((void*)impl));
  ((impl -> trait30) = (& Trait30_S30child2_trait30));
  return impl;
} 
Trait30* Trait30_S30child2_ = newTrait30_S30child2();

int Trait17_S30child1_trait17(Class* self_) {
  S30child1* self = ((S30child1*)self_);
{
    return (self -> a);
  }
} 
Trait17* newTrait17_S30child1() {
  Trait17 (* impl) = (new Trait17());
  setVec(Trait17_v, S30child1_classId, ((void*)impl));
  ((impl -> trait17) = (& Trait17_S30child1_trait17));
  return impl;
} 
Trait17* Trait17_S30child1_ = newTrait17_S30child1();

int S31_classId = Class_genId();
struct S31{

  int id;
  S31 ():id(S31_classId){

  }
};


int S31child1_classId = Class_genId();
struct S31child1:S31{

  S31child1 (int a):a(a){
    (id = S31child1_classId);
{

    }
  }
  int a;
};


int S31child2_classId = Class_genId();
struct S31child2:S31{

  S31child2 (int a):a(a){
    (id = S31child2_classId);
{

    }
  }
  int a;
};


int S31child3_classId = Class_genId();
struct S31child3:S31{

  S31child3 (int a):a(a){
    (id = S31child3_classId);
{

    }
  }
  int a;
};


int S31child4_classId = Class_genId();
struct S31child4:S31{

  S31child4 (int a):a(a){
    (id = S31child4_classId);
{

    }
  }
  int a;
};


struct Trait31{

  int(*trait31)(Class*);
};

Vec* Trait31_v = newVec();

int Trait31_S31child1_trait31(Class* self_) {
  S31child1* self = ((S31child1*)self_);
{
    return (self -> a);
  }
} 
Trait31* newTrait31_S31child1() {
  Trait31 (* impl) = (new Trait31());
  setVec(Trait31_v, S31child1_classId, ((void*)impl));
  ((impl -> trait31) = (& Trait31_S31child1_trait31));
  return impl;
} 
Trait31* Trait31_S31child1_ = newTrait31_S31child1();

int Trait9_S31child1_trait9(Class* self_) {
  S31child1* self = ((S31child1*)self_);
{
    return (self -> a);
  }
} 
Trait9* newTrait9_S31child1() {
  Trait9 (* impl) = (new Trait9());
  setVec(Trait9_v, S31child1_classId, ((void*)impl));
  ((impl -> trait9) = (& Trait9_S31child1_trait9));
  return impl;
} 
Trait9* Trait9_S31child1_ = newTrait9_S31child1();

int S32_classId = Class_genId();
struct S32{

  int id;
  S32 ():id(S32_classId){

  }
};


int S32child1_classId = Class_genId();
struct S32child1:S32{

  S32child1 (int a):a(a){
    (id = S32child1_classId);
{

    }
  }
  int a;
};


int S32child2_classId = Class_genId();
struct S32child2:S32{

  S32child2 (int a):a(a){
    (id = S32child2_classId);
{

    }
  }
  int a;
};


int S32child3_classId = Class_genId();
struct S32child3:S32{

  S32child3 (int a):a(a){
    (id = S32child3_classId);
{

    }
  }
  int a;
};


struct Trait32{

  int(*trait32)(Class*);
};

Vec* Trait32_v = newVec();

int Trait32_S32child1_trait32(Class* self_) {
  S32child1* self = ((S32child1*)self_);
{
    return (self -> a);
  }
} 
Trait32* newTrait32_S32child1() {
  Trait32 (* impl) = (new Trait32());
  setVec(Trait32_v, S32child1_classId, ((void*)impl));
  ((impl -> trait32) = (& Trait32_S32child1_trait32));
  return impl;
} 
Trait32* Trait32_S32child1_ = newTrait32_S32child1();

int Trait32_S32child2_trait32(Class* self_) {
  S32child2* self = ((S32child2*)self_);
{
    return (self -> a);
  }
} 
Trait32* newTrait32_S32child2() {
  Trait32 (* impl) = (new Trait32());
  setVec(Trait32_v, S32child2_classId, ((void*)impl));
  ((impl -> trait32) = (& Trait32_S32child2_trait32));
  return impl;
} 
Trait32* Trait32_S32child2_ = newTrait32_S32child2();

int Trait31_S32child1_trait31(Class* self_) {
  S32child1* self = ((S32child1*)self_);
{
    return (self -> a);
  }
} 
Trait31* newTrait31_S32child1() {
  Trait31 (* impl) = (new Trait31());
  setVec(Trait31_v, S32child1_classId, ((void*)impl));
  ((impl -> trait31) = (& Trait31_S32child1_trait31));
  return impl;
} 
Trait31* Trait31_S32child1_ = newTrait31_S32child1();

int S33_classId = Class_genId();
struct S33{

  int id;
  S33 ():id(S33_classId){

  }
};


int S33child1_classId = Class_genId();
struct S33child1:S33{

  S33child1 (int a):a(a){
    (id = S33child1_classId);
{

    }
  }
  int a;
};


int S33child2_classId = Class_genId();
struct S33child2:S33{

  S33child2 (int a):a(a){
    (id = S33child2_classId);
{

    }
  }
  int a;
};


struct Trait33{

  int(*trait33)(Class*);
};

Vec* Trait33_v = newVec();

int Trait33_S33child1_trait33(Class* self_) {
  S33child1* self = ((S33child1*)self_);
{
    return (self -> a);
  }
} 
Trait33* newTrait33_S33child1() {
  Trait33 (* impl) = (new Trait33());
  setVec(Trait33_v, S33child1_classId, ((void*)impl));
  ((impl -> trait33) = (& Trait33_S33child1_trait33));
  return impl;
} 
Trait33* Trait33_S33child1_ = newTrait33_S33child1();

int Trait9_S33child1_trait9(Class* self_) {
  S33child1* self = ((S33child1*)self_);
{
    return (self -> a);
  }
} 
Trait9* newTrait9_S33child1() {
  Trait9 (* impl) = (new Trait9());
  setVec(Trait9_v, S33child1_classId, ((void*)impl));
  ((impl -> trait9) = (& Trait9_S33child1_trait9));
  return impl;
} 
Trait9* Trait9_S33child1_ = newTrait9_S33child1();

int S34_classId = Class_genId();
struct S34{

  int id;
  S34 ():id(S34_classId){

  }
};


int S34child1_classId = Class_genId();
struct S34child1:S34{

  S34child1 (int a):a(a){
    (id = S34child1_classId);
{

    }
  }
  int a;
};


struct Trait34{

  int(*trait34)(Class*);
};

Vec* Trait34_v = newVec();

int Trait34_S34child1_trait34(Class* self_) {
  S34child1* self = ((S34child1*)self_);
{
    return (self -> a);
  }
} 
Trait34* newTrait34_S34child1() {
  Trait34 (* impl) = (new Trait34());
  setVec(Trait34_v, S34child1_classId, ((void*)impl));
  ((impl -> trait34) = (& Trait34_S34child1_trait34));
  return impl;
} 
Trait34* Trait34_S34child1_ = newTrait34_S34child1();

int Trait17_S34child1_trait17(Class* self_) {
  S34child1* self = ((S34child1*)self_);
{
    return (self -> a);
  }
} 
Trait17* newTrait17_S34child1() {
  Trait17 (* impl) = (new Trait17());
  setVec(Trait17_v, S34child1_classId, ((void*)impl));
  ((impl -> trait17) = (& Trait17_S34child1_trait17));
  return impl;
} 
Trait17* Trait17_S34child1_ = newTrait17_S34child1();

int S35_classId = Class_genId();
struct S35{

  int id;
  S35 ():id(S35_classId){

  }
};


int S35child1_classId = Class_genId();
struct S35child1:S35{

  S35child1 (int a):a(a){
    (id = S35child1_classId);
{

    }
  }
  int a;
};


int S35child2_classId = Class_genId();
struct S35child2:S35{

  S35child2 (int a):a(a){
    (id = S35child2_classId);
{

    }
  }
  int a;
};


int S35child3_classId = Class_genId();
struct S35child3:S35{

  S35child3 (int a):a(a){
    (id = S35child3_classId);
{

    }
  }
  int a;
};


int S35child4_classId = Class_genId();
struct S35child4:S35{

  S35child4 (int a):a(a){
    (id = S35child4_classId);
{

    }
  }
  int a;
};


struct Trait35{

  int(*trait35)(Class*);
};

Vec* Trait35_v = newVec();

int Trait35_S35child1_trait35(Class* self_) {
  S35child1* self = ((S35child1*)self_);
{
    return (self -> a);
  }
} 
Trait35* newTrait35_S35child1() {
  Trait35 (* impl) = (new Trait35());
  setVec(Trait35_v, S35child1_classId, ((void*)impl));
  ((impl -> trait35) = (& Trait35_S35child1_trait35));
  return impl;
} 
Trait35* Trait35_S35child1_ = newTrait35_S35child1();

int Trait35_S35child2_trait35(Class* self_) {
  S35child2* self = ((S35child2*)self_);
{
    return (self -> a);
  }
} 
Trait35* newTrait35_S35child2() {
  Trait35 (* impl) = (new Trait35());
  setVec(Trait35_v, S35child2_classId, ((void*)impl));
  ((impl -> trait35) = (& Trait35_S35child2_trait35));
  return impl;
} 
Trait35* Trait35_S35child2_ = newTrait35_S35child2();

int Trait17_S35child1_trait17(Class* self_) {
  S35child1* self = ((S35child1*)self_);
{
    return (self -> a);
  }
} 
Trait17* newTrait17_S35child1() {
  Trait17 (* impl) = (new Trait17());
  setVec(Trait17_v, S35child1_classId, ((void*)impl));
  ((impl -> trait17) = (& Trait17_S35child1_trait17));
  return impl;
} 
Trait17* Trait17_S35child1_ = newTrait17_S35child1();

int S36_classId = Class_genId();
struct S36{

  int id;
  S36 ():id(S36_classId){

  }
};


int S36child1_classId = Class_genId();
struct S36child1:S36{

  S36child1 (int a):a(a){
    (id = S36child1_classId);
{

    }
  }
  int a;
};


int S36child2_classId = Class_genId();
struct S36child2:S36{

  S36child2 (int a):a(a){
    (id = S36child2_classId);
{

    }
  }
  int a;
};


int S36child3_classId = Class_genId();
struct S36child3:S36{

  S36child3 (int a):a(a){
    (id = S36child3_classId);
{

    }
  }
  int a;
};


int S36child4_classId = Class_genId();
struct S36child4:S36{

  S36child4 (int a):a(a){
    (id = S36child4_classId);
{

    }
  }
  int a;
};


struct Trait36{

  int(*trait36)(Class*);
};

Vec* Trait36_v = newVec();

int Trait36_S36child1_trait36(Class* self_) {
  S36child1* self = ((S36child1*)self_);
{
    return (self -> a);
  }
} 
Trait36* newTrait36_S36child1() {
  Trait36 (* impl) = (new Trait36());
  setVec(Trait36_v, S36child1_classId, ((void*)impl));
  ((impl -> trait36) = (& Trait36_S36child1_trait36));
  return impl;
} 
Trait36* Trait36_S36child1_ = newTrait36_S36child1();

int Trait36_S36child2_trait36(Class* self_) {
  S36child2* self = ((S36child2*)self_);
{
    return (self -> a);
  }
} 
Trait36* newTrait36_S36child2() {
  Trait36 (* impl) = (new Trait36());
  setVec(Trait36_v, S36child2_classId, ((void*)impl));
  ((impl -> trait36) = (& Trait36_S36child2_trait36));
  return impl;
} 
Trait36* Trait36_S36child2_ = newTrait36_S36child2();

int Trait17_S36child1_trait17(Class* self_) {
  S36child1* self = ((S36child1*)self_);
{
    return (self -> a);
  }
} 
Trait17* newTrait17_S36child1() {
  Trait17 (* impl) = (new Trait17());
  setVec(Trait17_v, S36child1_classId, ((void*)impl));
  ((impl -> trait17) = (& Trait17_S36child1_trait17));
  return impl;
} 
Trait17* Trait17_S36child1_ = newTrait17_S36child1();

int S37_classId = Class_genId();
struct S37{

  int id;
  S37 ():id(S37_classId){

  }
};


int S37child1_classId = Class_genId();
struct S37child1:S37{

  S37child1 (int a):a(a){
    (id = S37child1_classId);
{

    }
  }
  int a;
};


struct Trait37{

  int(*trait37)(Class*);
};

Vec* Trait37_v = newVec();

int Trait37_S37child1_trait37(Class* self_) {
  S37child1* self = ((S37child1*)self_);
{
    return (self -> a);
  }
} 
Trait37* newTrait37_S37child1() {
  Trait37 (* impl) = (new Trait37());
  setVec(Trait37_v, S37child1_classId, ((void*)impl));
  ((impl -> trait37) = (& Trait37_S37child1_trait37));
  return impl;
} 
Trait37* Trait37_S37child1_ = newTrait37_S37child1();

int Trait18_S37child1_trait18(Class* self_) {
  S37child1* self = ((S37child1*)self_);
{
    return (self -> a);
  }
} 
Trait18* newTrait18_S37child1() {
  Trait18 (* impl) = (new Trait18());
  setVec(Trait18_v, S37child1_classId, ((void*)impl));
  ((impl -> trait18) = (& Trait18_S37child1_trait18));
  return impl;
} 
Trait18* Trait18_S37child1_ = newTrait18_S37child1();

int S38_classId = Class_genId();
struct S38{

  int id;
  S38 ():id(S38_classId){

  }
};


int S38child1_classId = Class_genId();
struct S38child1:S38{

  S38child1 (int a):a(a){
    (id = S38child1_classId);
{

    }
  }
  int a;
};


int S38child2_classId = Class_genId();
struct S38child2:S38{

  S38child2 (int a):a(a){
    (id = S38child2_classId);
{

    }
  }
  int a;
};


int S38child3_classId = Class_genId();
struct S38child3:S38{

  S38child3 (int a):a(a){
    (id = S38child3_classId);
{

    }
  }
  int a;
};


int S38child4_classId = Class_genId();
struct S38child4:S38{

  S38child4 (int a):a(a){
    (id = S38child4_classId);
{

    }
  }
  int a;
};


int S38child5_classId = Class_genId();
struct S38child5:S38{

  S38child5 (int a):a(a){
    (id = S38child5_classId);
{

    }
  }
  int a;
};


struct Trait38{

  int(*trait38)(Class*);
};

Vec* Trait38_v = newVec();

int Trait38_S38child1_trait38(Class* self_) {
  S38child1* self = ((S38child1*)self_);
{
    return (self -> a);
  }
} 
Trait38* newTrait38_S38child1() {
  Trait38 (* impl) = (new Trait38());
  setVec(Trait38_v, S38child1_classId, ((void*)impl));
  ((impl -> trait38) = (& Trait38_S38child1_trait38));
  return impl;
} 
Trait38* Trait38_S38child1_ = newTrait38_S38child1();

int Trait38_S38child2_trait38(Class* self_) {
  S38child2* self = ((S38child2*)self_);
{
    return (self -> a);
  }
} 
Trait38* newTrait38_S38child2() {
  Trait38 (* impl) = (new Trait38());
  setVec(Trait38_v, S38child2_classId, ((void*)impl));
  ((impl -> trait38) = (& Trait38_S38child2_trait38));
  return impl;
} 
Trait38* Trait38_S38child2_ = newTrait38_S38child2();

int Trait1_S38child1_trait1(Class* self_) {
  S38child1* self = ((S38child1*)self_);
{
    return (self -> a);
  }
} 
Trait1* newTrait1_S38child1() {
  Trait1 (* impl) = (new Trait1());
  setVec(Trait1_v, S38child1_classId, ((void*)impl));
  ((impl -> trait1) = (& Trait1_S38child1_trait1));
  return impl;
} 
Trait1* Trait1_S38child1_ = newTrait1_S38child1();

int S39_classId = Class_genId();
struct S39{

  int id;
  S39 ():id(S39_classId){

  }
};


int S39child1_classId = Class_genId();
struct S39child1:S39{

  S39child1 (int a):a(a){
    (id = S39child1_classId);
{

    }
  }
  int a;
};


int S39child2_classId = Class_genId();
struct S39child2:S39{

  S39child2 (int a):a(a){
    (id = S39child2_classId);
{

    }
  }
  int a;
};


struct Trait39{

  int(*trait39)(Class*);
};

Vec* Trait39_v = newVec();

int Trait39_S39child1_trait39(Class* self_) {
  S39child1* self = ((S39child1*)self_);
{
    return (self -> a);
  }
} 
Trait39* newTrait39_S39child1() {
  Trait39 (* impl) = (new Trait39());
  setVec(Trait39_v, S39child1_classId, ((void*)impl));
  ((impl -> trait39) = (& Trait39_S39child1_trait39));
  return impl;
} 
Trait39* Trait39_S39child1_ = newTrait39_S39child1();

int Trait23_S39child1_trait23(Class* self_) {
  S39child1* self = ((S39child1*)self_);
{
    return (self -> a);
  }
} 
Trait23* newTrait23_S39child1() {
  Trait23 (* impl) = (new Trait23());
  setVec(Trait23_v, S39child1_classId, ((void*)impl));
  ((impl -> trait23) = (& Trait23_S39child1_trait23));
  return impl;
} 
Trait23* Trait23_S39child1_ = newTrait23_S39child1();

int S40_classId = Class_genId();
struct S40{

  int id;
  S40 ():id(S40_classId){

  }
};


int S40child1_classId = Class_genId();
struct S40child1:S40{

  S40child1 (int a):a(a){
    (id = S40child1_classId);
{

    }
  }
  int a;
};


int S40child2_classId = Class_genId();
struct S40child2:S40{

  S40child2 (int a):a(a){
    (id = S40child2_classId);
{

    }
  }
  int a;
};


int S40child3_classId = Class_genId();
struct S40child3:S40{

  S40child3 (int a):a(a){
    (id = S40child3_classId);
{

    }
  }
  int a;
};


int S40child4_classId = Class_genId();
struct S40child4:S40{

  S40child4 (int a):a(a){
    (id = S40child4_classId);
{

    }
  }
  int a;
};


struct Trait40{

  int(*trait40)(Class*);
};

Vec* Trait40_v = newVec();

int Trait40_S40child1_trait40(Class* self_) {
  S40child1* self = ((S40child1*)self_);
{
    return (self -> a);
  }
} 
Trait40* newTrait40_S40child1() {
  Trait40 (* impl) = (new Trait40());
  setVec(Trait40_v, S40child1_classId, ((void*)impl));
  ((impl -> trait40) = (& Trait40_S40child1_trait40));
  return impl;
} 
Trait40* Trait40_S40child1_ = newTrait40_S40child1();

int Trait40_S40child2_trait40(Class* self_) {
  S40child2* self = ((S40child2*)self_);
{
    return (self -> a);
  }
} 
Trait40* newTrait40_S40child2() {
  Trait40 (* impl) = (new Trait40());
  setVec(Trait40_v, S40child2_classId, ((void*)impl));
  ((impl -> trait40) = (& Trait40_S40child2_trait40));
  return impl;
} 
Trait40* Trait40_S40child2_ = newTrait40_S40child2();

int Trait9_S40child1_trait9(Class* self_) {
  S40child1* self = ((S40child1*)self_);
{
    return (self -> a);
  }
} 
Trait9* newTrait9_S40child1() {
  Trait9 (* impl) = (new Trait9());
  setVec(Trait9_v, S40child1_classId, ((void*)impl));
  ((impl -> trait9) = (& Trait9_S40child1_trait9));
  return impl;
} 
Trait9* Trait9_S40child1_ = newTrait9_S40child1();

int S41_classId = Class_genId();
struct S41{

  int id;
  S41 ():id(S41_classId){

  }
};


int S41child1_classId = Class_genId();
struct S41child1:S41{

  S41child1 (int a):a(a){
    (id = S41child1_classId);
{

    }
  }
  int a;
};


int S41child2_classId = Class_genId();
struct S41child2:S41{

  S41child2 (int a):a(a){
    (id = S41child2_classId);
{

    }
  }
  int a;
};


struct Trait41{

  int(*trait41)(Class*);
};

Vec* Trait41_v = newVec();

int Trait41_S41child1_trait41(Class* self_) {
  S41child1* self = ((S41child1*)self_);
{
    return (self -> a);
  }
} 
Trait41* newTrait41_S41child1() {
  Trait41 (* impl) = (new Trait41());
  setVec(Trait41_v, S41child1_classId, ((void*)impl));
  ((impl -> trait41) = (& Trait41_S41child1_trait41));
  return impl;
} 
Trait41* Trait41_S41child1_ = newTrait41_S41child1();

int Trait34_S41child1_trait34(Class* self_) {
  S41child1* self = ((S41child1*)self_);
{
    return (self -> a);
  }
} 
Trait34* newTrait34_S41child1() {
  Trait34 (* impl) = (new Trait34());
  setVec(Trait34_v, S41child1_classId, ((void*)impl));
  ((impl -> trait34) = (& Trait34_S41child1_trait34));
  return impl;
} 
Trait34* Trait34_S41child1_ = newTrait34_S41child1();

int S42_classId = Class_genId();
struct S42{

  int id;
  S42 ():id(S42_classId){

  }
};


int S42child1_classId = Class_genId();
struct S42child1:S42{

  S42child1 (int a):a(a){
    (id = S42child1_classId);
{

    }
  }
  int a;
};


struct Trait42{

  int(*trait42)(Class*);
};

Vec* Trait42_v = newVec();

int Trait42_S42child1_trait42(Class* self_) {
  S42child1* self = ((S42child1*)self_);
{
    return (self -> a);
  }
} 
Trait42* newTrait42_S42child1() {
  Trait42 (* impl) = (new Trait42());
  setVec(Trait42_v, S42child1_classId, ((void*)impl));
  ((impl -> trait42) = (& Trait42_S42child1_trait42));
  return impl;
} 
Trait42* Trait42_S42child1_ = newTrait42_S42child1();

int Trait7_S42child1_trait7(Class* self_) {
  S42child1* self = ((S42child1*)self_);
{
    return (self -> a);
  }
} 
Trait7* newTrait7_S42child1() {
  Trait7 (* impl) = (new Trait7());
  setVec(Trait7_v, S42child1_classId, ((void*)impl));
  ((impl -> trait7) = (& Trait7_S42child1_trait7));
  return impl;
} 
Trait7* Trait7_S42child1_ = newTrait7_S42child1();

int S43_classId = Class_genId();
struct S43{

  int id;
  S43 ():id(S43_classId){

  }
};


int S43child1_classId = Class_genId();
struct S43child1:S43{

  S43child1 (int a):a(a){
    (id = S43child1_classId);
{

    }
  }
  int a;
};


int S43child2_classId = Class_genId();
struct S43child2:S43{

  S43child2 (int a):a(a){
    (id = S43child2_classId);
{

    }
  }
  int a;
};


int S43child3_classId = Class_genId();
struct S43child3:S43{

  S43child3 (int a):a(a){
    (id = S43child3_classId);
{

    }
  }
  int a;
};


struct Trait43{

  int(*trait43)(Class*);
};

Vec* Trait43_v = newVec();

int Trait43_S43child1_trait43(Class* self_) {
  S43child1* self = ((S43child1*)self_);
{
    return (self -> a);
  }
} 
Trait43* newTrait43_S43child1() {
  Trait43 (* impl) = (new Trait43());
  setVec(Trait43_v, S43child1_classId, ((void*)impl));
  ((impl -> trait43) = (& Trait43_S43child1_trait43));
  return impl;
} 
Trait43* Trait43_S43child1_ = newTrait43_S43child1();

int Trait43_S43child2_trait43(Class* self_) {
  S43child2* self = ((S43child2*)self_);
{
    return (self -> a);
  }
} 
Trait43* newTrait43_S43child2() {
  Trait43 (* impl) = (new Trait43());
  setVec(Trait43_v, S43child2_classId, ((void*)impl));
  ((impl -> trait43) = (& Trait43_S43child2_trait43));
  return impl;
} 
Trait43* Trait43_S43child2_ = newTrait43_S43child2();

int Trait3_S43child1_trait3(Class* self_) {
  S43child1* self = ((S43child1*)self_);
{
    return (self -> a);
  }
} 
Trait3* newTrait3_S43child1() {
  Trait3 (* impl) = (new Trait3());
  setVec(Trait3_v, S43child1_classId, ((void*)impl));
  ((impl -> trait3) = (& Trait3_S43child1_trait3));
  return impl;
} 
Trait3* Trait3_S43child1_ = newTrait3_S43child1();

int S44_classId = Class_genId();
struct S44{

  int id;
  S44 ():id(S44_classId){

  }
};


int S44child1_classId = Class_genId();
struct S44child1:S44{

  S44child1 (int a):a(a){
    (id = S44child1_classId);
{

    }
  }
  int a;
};


int S44child2_classId = Class_genId();
struct S44child2:S44{

  S44child2 (int a):a(a){
    (id = S44child2_classId);
{

    }
  }
  int a;
};


int S44child3_classId = Class_genId();
struct S44child3:S44{

  S44child3 (int a):a(a){
    (id = S44child3_classId);
{

    }
  }
  int a;
};


int S44child4_classId = Class_genId();
struct S44child4:S44{

  S44child4 (int a):a(a){
    (id = S44child4_classId);
{

    }
  }
  int a;
};


struct Trait44{

  int(*trait44)(Class*);
};

Vec* Trait44_v = newVec();

int Trait44_S44child1_trait44(Class* self_) {
  S44child1* self = ((S44child1*)self_);
{
    return (self -> a);
  }
} 
Trait44* newTrait44_S44child1() {
  Trait44 (* impl) = (new Trait44());
  setVec(Trait44_v, S44child1_classId, ((void*)impl));
  ((impl -> trait44) = (& Trait44_S44child1_trait44));
  return impl;
} 
Trait44* Trait44_S44child1_ = newTrait44_S44child1();

int Trait44_S44child2_trait44(Class* self_) {
  S44child2* self = ((S44child2*)self_);
{
    return (self -> a);
  }
} 
Trait44* newTrait44_S44child2() {
  Trait44 (* impl) = (new Trait44());
  setVec(Trait44_v, S44child2_classId, ((void*)impl));
  ((impl -> trait44) = (& Trait44_S44child2_trait44));
  return impl;
} 
Trait44* Trait44_S44child2_ = newTrait44_S44child2();

int Trait44_S44child3_trait44(Class* self_) {
  S44child3* self = ((S44child3*)self_);
{
    return (self -> a);
  }
} 
Trait44* newTrait44_S44child3() {
  Trait44 (* impl) = (new Trait44());
  setVec(Trait44_v, S44child3_classId, ((void*)impl));
  ((impl -> trait44) = (& Trait44_S44child3_trait44));
  return impl;
} 
Trait44* Trait44_S44child3_ = newTrait44_S44child3();

int Trait36_S44child1_trait36(Class* self_) {
  S44child1* self = ((S44child1*)self_);
{
    return (self -> a);
  }
} 
Trait36* newTrait36_S44child1() {
  Trait36 (* impl) = (new Trait36());
  setVec(Trait36_v, S44child1_classId, ((void*)impl));
  ((impl -> trait36) = (& Trait36_S44child1_trait36));
  return impl;
} 
Trait36* Trait36_S44child1_ = newTrait36_S44child1();

int S45_classId = Class_genId();
struct S45{

  int id;
  S45 ():id(S45_classId){

  }
};


int S45child1_classId = Class_genId();
struct S45child1:S45{

  S45child1 (int a):a(a){
    (id = S45child1_classId);
{

    }
  }
  int a;
};


int S45child2_classId = Class_genId();
struct S45child2:S45{

  S45child2 (int a):a(a){
    (id = S45child2_classId);
{

    }
  }
  int a;
};


int S45child3_classId = Class_genId();
struct S45child3:S45{

  S45child3 (int a):a(a){
    (id = S45child3_classId);
{

    }
  }
  int a;
};


struct Trait45{

  int(*trait45)(Class*);
};

Vec* Trait45_v = newVec();

int Trait45_S45child1_trait45(Class* self_) {
  S45child1* self = ((S45child1*)self_);
{
    return (self -> a);
  }
} 
Trait45* newTrait45_S45child1() {
  Trait45 (* impl) = (new Trait45());
  setVec(Trait45_v, S45child1_classId, ((void*)impl));
  ((impl -> trait45) = (& Trait45_S45child1_trait45));
  return impl;
} 
Trait45* Trait45_S45child1_ = newTrait45_S45child1();

int Trait45_S45child2_trait45(Class* self_) {
  S45child2* self = ((S45child2*)self_);
{
    return (self -> a);
  }
} 
Trait45* newTrait45_S45child2() {
  Trait45 (* impl) = (new Trait45());
  setVec(Trait45_v, S45child2_classId, ((void*)impl));
  ((impl -> trait45) = (& Trait45_S45child2_trait45));
  return impl;
} 
Trait45* Trait45_S45child2_ = newTrait45_S45child2();

int Trait45_S45child3_trait45(Class* self_) {
  S45child3* self = ((S45child3*)self_);
{
    return (self -> a);
  }
} 
Trait45* newTrait45_S45child3() {
  Trait45 (* impl) = (new Trait45());
  setVec(Trait45_v, S45child3_classId, ((void*)impl));
  ((impl -> trait45) = (& Trait45_S45child3_trait45));
  return impl;
} 
Trait45* Trait45_S45child3_ = newTrait45_S45child3();

int Trait10_S45child1_trait10(Class* self_) {
  S45child1* self = ((S45child1*)self_);
{
    return (self -> a);
  }
} 
Trait10* newTrait10_S45child1() {
  Trait10 (* impl) = (new Trait10());
  setVec(Trait10_v, S45child1_classId, ((void*)impl));
  ((impl -> trait10) = (& Trait10_S45child1_trait10));
  return impl;
} 
Trait10* Trait10_S45child1_ = newTrait10_S45child1();

int S46_classId = Class_genId();
struct S46{

  int id;
  S46 ():id(S46_classId){

  }
};


int S46child1_classId = Class_genId();
struct S46child1:S46{

  S46child1 (int a):a(a){
    (id = S46child1_classId);
{

    }
  }
  int a;
};


int S46child2_classId = Class_genId();
struct S46child2:S46{

  S46child2 (int a):a(a){
    (id = S46child2_classId);
{

    }
  }
  int a;
};


int S46child3_classId = Class_genId();
struct S46child3:S46{

  S46child3 (int a):a(a){
    (id = S46child3_classId);
{

    }
  }
  int a;
};


struct Trait46{

  int(*trait46)(Class*);
};

Vec* Trait46_v = newVec();

int Trait46_S46child1_trait46(Class* self_) {
  S46child1* self = ((S46child1*)self_);
{
    return (self -> a);
  }
} 
Trait46* newTrait46_S46child1() {
  Trait46 (* impl) = (new Trait46());
  setVec(Trait46_v, S46child1_classId, ((void*)impl));
  ((impl -> trait46) = (& Trait46_S46child1_trait46));
  return impl;
} 
Trait46* Trait46_S46child1_ = newTrait46_S46child1();

int Trait46_S46child2_trait46(Class* self_) {
  S46child2* self = ((S46child2*)self_);
{
    return (self -> a);
  }
} 
Trait46* newTrait46_S46child2() {
  Trait46 (* impl) = (new Trait46());
  setVec(Trait46_v, S46child2_classId, ((void*)impl));
  ((impl -> trait46) = (& Trait46_S46child2_trait46));
  return impl;
} 
Trait46* Trait46_S46child2_ = newTrait46_S46child2();

int Trait46_S46child3_trait46(Class* self_) {
  S46child3* self = ((S46child3*)self_);
{
    return (self -> a);
  }
} 
Trait46* newTrait46_S46child3() {
  Trait46 (* impl) = (new Trait46());
  setVec(Trait46_v, S46child3_classId, ((void*)impl));
  ((impl -> trait46) = (& Trait46_S46child3_trait46));
  return impl;
} 
Trait46* Trait46_S46child3_ = newTrait46_S46child3();

int Trait7_S46child1_trait7(Class* self_) {
  S46child1* self = ((S46child1*)self_);
{
    return (self -> a);
  }
} 
Trait7* newTrait7_S46child1() {
  Trait7 (* impl) = (new Trait7());
  setVec(Trait7_v, S46child1_classId, ((void*)impl));
  ((impl -> trait7) = (& Trait7_S46child1_trait7));
  return impl;
} 
Trait7* Trait7_S46child1_ = newTrait7_S46child1();

int S47_classId = Class_genId();
struct S47{

  int id;
  S47 ():id(S47_classId){

  }
};


int S47child1_classId = Class_genId();
struct S47child1:S47{

  S47child1 (int a):a(a){
    (id = S47child1_classId);
{

    }
  }
  int a;
};


int S47child2_classId = Class_genId();
struct S47child2:S47{

  S47child2 (int a):a(a){
    (id = S47child2_classId);
{

    }
  }
  int a;
};


struct Trait47{

  int(*trait47)(Class*);
};

Vec* Trait47_v = newVec();

int Trait47_S47child1_trait47(Class* self_) {
  S47child1* self = ((S47child1*)self_);
{
    return (self -> a);
  }
} 
Trait47* newTrait47_S47child1() {
  Trait47 (* impl) = (new Trait47());
  setVec(Trait47_v, S47child1_classId, ((void*)impl));
  ((impl -> trait47) = (& Trait47_S47child1_trait47));
  return impl;
} 
Trait47* Trait47_S47child1_ = newTrait47_S47child1();

int Trait47_S47child2_trait47(Class* self_) {
  S47child2* self = ((S47child2*)self_);
{
    return (self -> a);
  }
} 
Trait47* newTrait47_S47child2() {
  Trait47 (* impl) = (new Trait47());
  setVec(Trait47_v, S47child2_classId, ((void*)impl));
  ((impl -> trait47) = (& Trait47_S47child2_trait47));
  return impl;
} 
Trait47* Trait47_S47child2_ = newTrait47_S47child2();

int Trait9_S47child1_trait9(Class* self_) {
  S47child1* self = ((S47child1*)self_);
{
    return (self -> a);
  }
} 
Trait9* newTrait9_S47child1() {
  Trait9 (* impl) = (new Trait9());
  setVec(Trait9_v, S47child1_classId, ((void*)impl));
  ((impl -> trait9) = (& Trait9_S47child1_trait9));
  return impl;
} 
Trait9* Trait9_S47child1_ = newTrait9_S47child1();

int S48_classId = Class_genId();
struct S48{

  int id;
  S48 ():id(S48_classId){

  }
};


int S48child1_classId = Class_genId();
struct S48child1:S48{

  S48child1 (int a):a(a){
    (id = S48child1_classId);
{

    }
  }
  int a;
};


int S48child2_classId = Class_genId();
struct S48child2:S48{

  S48child2 (int a):a(a){
    (id = S48child2_classId);
{

    }
  }
  int a;
};


int S48child3_classId = Class_genId();
struct S48child3:S48{

  S48child3 (int a):a(a){
    (id = S48child3_classId);
{

    }
  }
  int a;
};


int S48child4_classId = Class_genId();
struct S48child4:S48{

  S48child4 (int a):a(a){
    (id = S48child4_classId);
{

    }
  }
  int a;
};


int S48child5_classId = Class_genId();
struct S48child5:S48{

  S48child5 (int a):a(a){
    (id = S48child5_classId);
{

    }
  }
  int a;
};


struct Trait48{

  int(*trait48)(Class*);
};

Vec* Trait48_v = newVec();

int Trait48_S48child1_trait48(Class* self_) {
  S48child1* self = ((S48child1*)self_);
{
    return (self -> a);
  }
} 
Trait48* newTrait48_S48child1() {
  Trait48 (* impl) = (new Trait48());
  setVec(Trait48_v, S48child1_classId, ((void*)impl));
  ((impl -> trait48) = (& Trait48_S48child1_trait48));
  return impl;
} 
Trait48* Trait48_S48child1_ = newTrait48_S48child1();

int Trait48_S48child2_trait48(Class* self_) {
  S48child2* self = ((S48child2*)self_);
{
    return (self -> a);
  }
} 
Trait48* newTrait48_S48child2() {
  Trait48 (* impl) = (new Trait48());
  setVec(Trait48_v, S48child2_classId, ((void*)impl));
  ((impl -> trait48) = (& Trait48_S48child2_trait48));
  return impl;
} 
Trait48* Trait48_S48child2_ = newTrait48_S48child2();

int Trait15_S48child1_trait15(Class* self_) {
  S48child1* self = ((S48child1*)self_);
{
    return (self -> a);
  }
} 
Trait15* newTrait15_S48child1() {
  Trait15 (* impl) = (new Trait15());
  setVec(Trait15_v, S48child1_classId, ((void*)impl));
  ((impl -> trait15) = (& Trait15_S48child1_trait15));
  return impl;
} 
Trait15* Trait15_S48child1_ = newTrait15_S48child1();

int S49_classId = Class_genId();
struct S49{

  int id;
  S49 ():id(S49_classId){

  }
};


int S49child1_classId = Class_genId();
struct S49child1:S49{

  S49child1 (int a):a(a){
    (id = S49child1_classId);
{

    }
  }
  int a;
};


int S49child2_classId = Class_genId();
struct S49child2:S49{

  S49child2 (int a):a(a){
    (id = S49child2_classId);
{

    }
  }
  int a;
};


struct Trait49{

  int(*trait49)(Class*);
};

Vec* Trait49_v = newVec();

int Trait49_S49child1_trait49(Class* self_) {
  S49child1* self = ((S49child1*)self_);
{
    return (self -> a);
  }
} 
Trait49* newTrait49_S49child1() {
  Trait49 (* impl) = (new Trait49());
  setVec(Trait49_v, S49child1_classId, ((void*)impl));
  ((impl -> trait49) = (& Trait49_S49child1_trait49));
  return impl;
} 
Trait49* Trait49_S49child1_ = newTrait49_S49child1();

int Trait49_S49child2_trait49(Class* self_) {
  S49child2* self = ((S49child2*)self_);
{
    return (self -> a);
  }
} 
Trait49* newTrait49_S49child2() {
  Trait49 (* impl) = (new Trait49());
  setVec(Trait49_v, S49child2_classId, ((void*)impl));
  ((impl -> trait49) = (& Trait49_S49child2_trait49));
  return impl;
} 
Trait49* Trait49_S49child2_ = newTrait49_S49child2();

int Trait48_S49child1_trait48(Class* self_) {
  S49child1* self = ((S49child1*)self_);
{
    return (self -> a);
  }
} 
Trait48* newTrait48_S49child1() {
  Trait48 (* impl) = (new Trait48());
  setVec(Trait48_v, S49child1_classId, ((void*)impl));
  ((impl -> trait48) = (& Trait48_S49child1_trait48));
  return impl;
} 
Trait48* Trait48_S49child1_ = newTrait48_S49child1();

int S50_classId = Class_genId();
struct S50{

  int id;
  S50 ():id(S50_classId){

  }
};


int S50child1_classId = Class_genId();
struct S50child1:S50{

  S50child1 (int a):a(a){
    (id = S50child1_classId);
{

    }
  }
  int a;
};


struct Trait50{

  int(*trait50)(Class*);
};

Vec* Trait50_v = newVec();

int Trait50_S50child1_trait50(Class* self_) {
  S50child1* self = ((S50child1*)self_);
{
    return (self -> a);
  }
} 
Trait50* newTrait50_S50child1() {
  Trait50 (* impl) = (new Trait50());
  setVec(Trait50_v, S50child1_classId, ((void*)impl));
  ((impl -> trait50) = (& Trait50_S50child1_trait50));
  return impl;
} 
Trait50* Trait50_S50child1_ = newTrait50_S50child1();

int Trait43_S50child1_trait43(Class* self_) {
  S50child1* self = ((S50child1*)self_);
{
    return (self -> a);
  }
} 
Trait43* newTrait43_S50child1() {
  Trait43 (* impl) = (new Trait43());
  setVec(Trait43_v, S50child1_classId, ((void*)impl));
  ((impl -> trait43) = (& Trait43_S50child1_trait43));
  return impl;
} 
Trait43* Trait43_S50child1_ = newTrait43_S50child1();

int S51_classId = Class_genId();
struct S51{

  int id;
  S51 ():id(S51_classId){

  }
};


int S51child1_classId = Class_genId();
struct S51child1:S51{

  S51child1 (int a):a(a){
    (id = S51child1_classId);
{

    }
  }
  int a;
};


int S51child2_classId = Class_genId();
struct S51child2:S51{

  S51child2 (int a):a(a){
    (id = S51child2_classId);
{

    }
  }
  int a;
};


struct Trait51{

  int(*trait51)(Class*);
};

Vec* Trait51_v = newVec();

int Trait51_S51child1_trait51(Class* self_) {
  S51child1* self = ((S51child1*)self_);
{
    return (self -> a);
  }
} 
Trait51* newTrait51_S51child1() {
  Trait51 (* impl) = (new Trait51());
  setVec(Trait51_v, S51child1_classId, ((void*)impl));
  ((impl -> trait51) = (& Trait51_S51child1_trait51));
  return impl;
} 
Trait51* Trait51_S51child1_ = newTrait51_S51child1();

int Trait25_S51child1_trait25(Class* self_) {
  S51child1* self = ((S51child1*)self_);
{
    return (self -> a);
  }
} 
Trait25* newTrait25_S51child1() {
  Trait25 (* impl) = (new Trait25());
  setVec(Trait25_v, S51child1_classId, ((void*)impl));
  ((impl -> trait25) = (& Trait25_S51child1_trait25));
  return impl;
} 
Trait25* Trait25_S51child1_ = newTrait25_S51child1();

int S52_classId = Class_genId();
struct S52{

  int id;
  S52 ():id(S52_classId){

  }
};


int S52child1_classId = Class_genId();
struct S52child1:S52{

  S52child1 (int a):a(a){
    (id = S52child1_classId);
{

    }
  }
  int a;
};


struct Trait52{

  int(*trait52)(Class*);
};

Vec* Trait52_v = newVec();

int Trait52_S52child1_trait52(Class* self_) {
  S52child1* self = ((S52child1*)self_);
{
    return (self -> a);
  }
} 
Trait52* newTrait52_S52child1() {
  Trait52 (* impl) = (new Trait52());
  setVec(Trait52_v, S52child1_classId, ((void*)impl));
  ((impl -> trait52) = (& Trait52_S52child1_trait52));
  return impl;
} 
Trait52* Trait52_S52child1_ = newTrait52_S52child1();

int Trait18_S52child1_trait18(Class* self_) {
  S52child1* self = ((S52child1*)self_);
{
    return (self -> a);
  }
} 
Trait18* newTrait18_S52child1() {
  Trait18 (* impl) = (new Trait18());
  setVec(Trait18_v, S52child1_classId, ((void*)impl));
  ((impl -> trait18) = (& Trait18_S52child1_trait18));
  return impl;
} 
Trait18* Trait18_S52child1_ = newTrait18_S52child1();

int S53_classId = Class_genId();
struct S53{

  int id;
  S53 ():id(S53_classId){

  }
};


int S53child1_classId = Class_genId();
struct S53child1:S53{

  S53child1 (int a):a(a){
    (id = S53child1_classId);
{

    }
  }
  int a;
};


int S53child2_classId = Class_genId();
struct S53child2:S53{

  S53child2 (int a):a(a){
    (id = S53child2_classId);
{

    }
  }
  int a;
};


struct Trait53{

  int(*trait53)(Class*);
};

Vec* Trait53_v = newVec();

int Trait53_S53child1_trait53(Class* self_) {
  S53child1* self = ((S53child1*)self_);
{
    return (self -> a);
  }
} 
Trait53* newTrait53_S53child1() {
  Trait53 (* impl) = (new Trait53());
  setVec(Trait53_v, S53child1_classId, ((void*)impl));
  ((impl -> trait53) = (& Trait53_S53child1_trait53));
  return impl;
} 
Trait53* Trait53_S53child1_ = newTrait53_S53child1();

int Trait53_S53child2_trait53(Class* self_) {
  S53child2* self = ((S53child2*)self_);
{
    return (self -> a);
  }
} 
Trait53* newTrait53_S53child2() {
  Trait53 (* impl) = (new Trait53());
  setVec(Trait53_v, S53child2_classId, ((void*)impl));
  ((impl -> trait53) = (& Trait53_S53child2_trait53));
  return impl;
} 
Trait53* Trait53_S53child2_ = newTrait53_S53child2();

int Trait47_S53child1_trait47(Class* self_) {
  S53child1* self = ((S53child1*)self_);
{
    return (self -> a);
  }
} 
Trait47* newTrait47_S53child1() {
  Trait47 (* impl) = (new Trait47());
  setVec(Trait47_v, S53child1_classId, ((void*)impl));
  ((impl -> trait47) = (& Trait47_S53child1_trait47));
  return impl;
} 
Trait47* Trait47_S53child1_ = newTrait47_S53child1();

int S54_classId = Class_genId();
struct S54{

  int id;
  S54 ():id(S54_classId){

  }
};


int S54child1_classId = Class_genId();
struct S54child1:S54{

  S54child1 (int a):a(a){
    (id = S54child1_classId);
{

    }
  }
  int a;
};


int S54child2_classId = Class_genId();
struct S54child2:S54{

  S54child2 (int a):a(a){
    (id = S54child2_classId);
{

    }
  }
  int a;
};


struct Trait54{

  int(*trait54)(Class*);
};

Vec* Trait54_v = newVec();

int Trait54_S54child1_trait54(Class* self_) {
  S54child1* self = ((S54child1*)self_);
{
    return (self -> a);
  }
} 
Trait54* newTrait54_S54child1() {
  Trait54 (* impl) = (new Trait54());
  setVec(Trait54_v, S54child1_classId, ((void*)impl));
  ((impl -> trait54) = (& Trait54_S54child1_trait54));
  return impl;
} 
Trait54* Trait54_S54child1_ = newTrait54_S54child1();

int Trait54_S54child2_trait54(Class* self_) {
  S54child2* self = ((S54child2*)self_);
{
    return (self -> a);
  }
} 
Trait54* newTrait54_S54child2() {
  Trait54 (* impl) = (new Trait54());
  setVec(Trait54_v, S54child2_classId, ((void*)impl));
  ((impl -> trait54) = (& Trait54_S54child2_trait54));
  return impl;
} 
Trait54* Trait54_S54child2_ = newTrait54_S54child2();

int Trait4_S54child1_trait4(Class* self_) {
  S54child1* self = ((S54child1*)self_);
{
    return (self -> a);
  }
} 
Trait4* newTrait4_S54child1() {
  Trait4 (* impl) = (new Trait4());
  setVec(Trait4_v, S54child1_classId, ((void*)impl));
  ((impl -> trait4) = (& Trait4_S54child1_trait4));
  return impl;
} 
Trait4* Trait4_S54child1_ = newTrait4_S54child1();

int S55_classId = Class_genId();
struct S55{

  int id;
  S55 ():id(S55_classId){

  }
};


int S55child1_classId = Class_genId();
struct S55child1:S55{

  S55child1 (int a):a(a){
    (id = S55child1_classId);
{

    }
  }
  int a;
};


int S55child2_classId = Class_genId();
struct S55child2:S55{

  S55child2 (int a):a(a){
    (id = S55child2_classId);
{

    }
  }
  int a;
};


struct Trait55{

  int(*trait55)(Class*);
};

Vec* Trait55_v = newVec();

int Trait55_S55child1_trait55(Class* self_) {
  S55child1* self = ((S55child1*)self_);
{
    return (self -> a);
  }
} 
Trait55* newTrait55_S55child1() {
  Trait55 (* impl) = (new Trait55());
  setVec(Trait55_v, S55child1_classId, ((void*)impl));
  ((impl -> trait55) = (& Trait55_S55child1_trait55));
  return impl;
} 
Trait55* Trait55_S55child1_ = newTrait55_S55child1();

int Trait5_S55child1_trait5(Class* self_) {
  S55child1* self = ((S55child1*)self_);
{
    return (self -> a);
  }
} 
Trait5* newTrait5_S55child1() {
  Trait5 (* impl) = (new Trait5());
  setVec(Trait5_v, S55child1_classId, ((void*)impl));
  ((impl -> trait5) = (& Trait5_S55child1_trait5));
  return impl;
} 
Trait5* Trait5_S55child1_ = newTrait5_S55child1();

int S56_classId = Class_genId();
struct S56{

  int id;
  S56 ():id(S56_classId){

  }
};


int S56child1_classId = Class_genId();
struct S56child1:S56{

  S56child1 (int a):a(a){
    (id = S56child1_classId);
{

    }
  }
  int a;
};


struct Trait56{

  int(*trait56)(Class*);
};

Vec* Trait56_v = newVec();

int Trait56_S56child1_trait56(Class* self_) {
  S56child1* self = ((S56child1*)self_);
{
    return (self -> a);
  }
} 
Trait56* newTrait56_S56child1() {
  Trait56 (* impl) = (new Trait56());
  setVec(Trait56_v, S56child1_classId, ((void*)impl));
  ((impl -> trait56) = (& Trait56_S56child1_trait56));
  return impl;
} 
Trait56* Trait56_S56child1_ = newTrait56_S56child1();

int Trait17_S56child1_trait17(Class* self_) {
  S56child1* self = ((S56child1*)self_);
{
    return (self -> a);
  }
} 
Trait17* newTrait17_S56child1() {
  Trait17 (* impl) = (new Trait17());
  setVec(Trait17_v, S56child1_classId, ((void*)impl));
  ((impl -> trait17) = (& Trait17_S56child1_trait17));
  return impl;
} 
Trait17* Trait17_S56child1_ = newTrait17_S56child1();

int S57_classId = Class_genId();
struct S57{

  int id;
  S57 ():id(S57_classId){

  }
};


int S57child1_classId = Class_genId();
struct S57child1:S57{

  S57child1 (int a):a(a){
    (id = S57child1_classId);
{

    }
  }
  int a;
};


int S57child2_classId = Class_genId();
struct S57child2:S57{

  S57child2 (int a):a(a){
    (id = S57child2_classId);
{

    }
  }
  int a;
};


struct Trait57{

  int(*trait57)(Class*);
};

Vec* Trait57_v = newVec();

int Trait57_S57child1_trait57(Class* self_) {
  S57child1* self = ((S57child1*)self_);
{
    return (self -> a);
  }
} 
Trait57* newTrait57_S57child1() {
  Trait57 (* impl) = (new Trait57());
  setVec(Trait57_v, S57child1_classId, ((void*)impl));
  ((impl -> trait57) = (& Trait57_S57child1_trait57));
  return impl;
} 
Trait57* Trait57_S57child1_ = newTrait57_S57child1();

int Trait57_S57child2_trait57(Class* self_) {
  S57child2* self = ((S57child2*)self_);
{
    return (self -> a);
  }
} 
Trait57* newTrait57_S57child2() {
  Trait57 (* impl) = (new Trait57());
  setVec(Trait57_v, S57child2_classId, ((void*)impl));
  ((impl -> trait57) = (& Trait57_S57child2_trait57));
  return impl;
} 
Trait57* Trait57_S57child2_ = newTrait57_S57child2();

int Trait42_S57child1_trait42(Class* self_) {
  S57child1* self = ((S57child1*)self_);
{
    return (self -> a);
  }
} 
Trait42* newTrait42_S57child1() {
  Trait42 (* impl) = (new Trait42());
  setVec(Trait42_v, S57child1_classId, ((void*)impl));
  ((impl -> trait42) = (& Trait42_S57child1_trait42));
  return impl;
} 
Trait42* Trait42_S57child1_ = newTrait42_S57child1();

int S58_classId = Class_genId();
struct S58{

  int id;
  S58 ():id(S58_classId){

  }
};


int S58child1_classId = Class_genId();
struct S58child1:S58{

  S58child1 (int a):a(a){
    (id = S58child1_classId);
{

    }
  }
  int a;
};


int S58child2_classId = Class_genId();
struct S58child2:S58{

  S58child2 (int a):a(a){
    (id = S58child2_classId);
{

    }
  }
  int a;
};


int S58child3_classId = Class_genId();
struct S58child3:S58{

  S58child3 (int a):a(a){
    (id = S58child3_classId);
{

    }
  }
  int a;
};


struct Trait58{

  int(*trait58)(Class*);
};

Vec* Trait58_v = newVec();

int Trait58_S58child1_trait58(Class* self_) {
  S58child1* self = ((S58child1*)self_);
{
    return (self -> a);
  }
} 
Trait58* newTrait58_S58child1() {
  Trait58 (* impl) = (new Trait58());
  setVec(Trait58_v, S58child1_classId, ((void*)impl));
  ((impl -> trait58) = (& Trait58_S58child1_trait58));
  return impl;
} 
Trait58* Trait58_S58child1_ = newTrait58_S58child1();

int Trait58_S58child2_trait58(Class* self_) {
  S58child2* self = ((S58child2*)self_);
{
    return (self -> a);
  }
} 
Trait58* newTrait58_S58child2() {
  Trait58 (* impl) = (new Trait58());
  setVec(Trait58_v, S58child2_classId, ((void*)impl));
  ((impl -> trait58) = (& Trait58_S58child2_trait58));
  return impl;
} 
Trait58* Trait58_S58child2_ = newTrait58_S58child2();

int Trait2_S58child1_trait2(Class* self_) {
  S58child1* self = ((S58child1*)self_);
{
    return (self -> a);
  }
} 
Trait2* newTrait2_S58child1() {
  Trait2 (* impl) = (new Trait2());
  setVec(Trait2_v, S58child1_classId, ((void*)impl));
  ((impl -> trait2) = (& Trait2_S58child1_trait2));
  return impl;
} 
Trait2* Trait2_S58child1_ = newTrait2_S58child1();

int S59_classId = Class_genId();
struct S59{

  int id;
  S59 ():id(S59_classId){

  }
};


int S59child1_classId = Class_genId();
struct S59child1:S59{

  S59child1 (int a):a(a){
    (id = S59child1_classId);
{

    }
  }
  int a;
};


int S59child2_classId = Class_genId();
struct S59child2:S59{

  S59child2 (int a):a(a){
    (id = S59child2_classId);
{

    }
  }
  int a;
};


int S59child3_classId = Class_genId();
struct S59child3:S59{

  S59child3 (int a):a(a){
    (id = S59child3_classId);
{

    }
  }
  int a;
};


struct Trait59{

  int(*trait59)(Class*);
};

Vec* Trait59_v = newVec();

int Trait59_S59child1_trait59(Class* self_) {
  S59child1* self = ((S59child1*)self_);
{
    return (self -> a);
  }
} 
Trait59* newTrait59_S59child1() {
  Trait59 (* impl) = (new Trait59());
  setVec(Trait59_v, S59child1_classId, ((void*)impl));
  ((impl -> trait59) = (& Trait59_S59child1_trait59));
  return impl;
} 
Trait59* Trait59_S59child1_ = newTrait59_S59child1();

int Trait59_S59child2_trait59(Class* self_) {
  S59child2* self = ((S59child2*)self_);
{
    return (self -> a);
  }
} 
Trait59* newTrait59_S59child2() {
  Trait59 (* impl) = (new Trait59());
  setVec(Trait59_v, S59child2_classId, ((void*)impl));
  ((impl -> trait59) = (& Trait59_S59child2_trait59));
  return impl;
} 
Trait59* Trait59_S59child2_ = newTrait59_S59child2();

int Trait59_S59child3_trait59(Class* self_) {
  S59child3* self = ((S59child3*)self_);
{
    return (self -> a);
  }
} 
Trait59* newTrait59_S59child3() {
  Trait59 (* impl) = (new Trait59());
  setVec(Trait59_v, S59child3_classId, ((void*)impl));
  ((impl -> trait59) = (& Trait59_S59child3_trait59));
  return impl;
} 
Trait59* Trait59_S59child3_ = newTrait59_S59child3();

int Trait34_S59child1_trait34(Class* self_) {
  S59child1* self = ((S59child1*)self_);
{
    return (self -> a);
  }
} 
Trait34* newTrait34_S59child1() {
  Trait34 (* impl) = (new Trait34());
  setVec(Trait34_v, S59child1_classId, ((void*)impl));
  ((impl -> trait34) = (& Trait34_S59child1_trait34));
  return impl;
} 
Trait34* Trait34_S59child1_ = newTrait34_S59child1();

int S60_classId = Class_genId();
struct S60{

  int id;
  S60 ():id(S60_classId){

  }
};


int S60child1_classId = Class_genId();
struct S60child1:S60{

  S60child1 (int a):a(a){
    (id = S60child1_classId);
{

    }
  }
  int a;
};


int S60child2_classId = Class_genId();
struct S60child2:S60{

  S60child2 (int a):a(a){
    (id = S60child2_classId);
{

    }
  }
  int a;
};


struct Trait60{

  int(*trait60)(Class*);
};

Vec* Trait60_v = newVec();

int Trait60_S60child1_trait60(Class* self_) {
  S60child1* self = ((S60child1*)self_);
{
    return (self -> a);
  }
} 
Trait60* newTrait60_S60child1() {
  Trait60 (* impl) = (new Trait60());
  setVec(Trait60_v, S60child1_classId, ((void*)impl));
  ((impl -> trait60) = (& Trait60_S60child1_trait60));
  return impl;
} 
Trait60* Trait60_S60child1_ = newTrait60_S60child1();

int Trait51_S60child1_trait51(Class* self_) {
  S60child1* self = ((S60child1*)self_);
{
    return (self -> a);
  }
} 
Trait51* newTrait51_S60child1() {
  Trait51 (* impl) = (new Trait51());
  setVec(Trait51_v, S60child1_classId, ((void*)impl));
  ((impl -> trait51) = (& Trait51_S60child1_trait51));
  return impl;
} 
Trait51* Trait51_S60child1_ = newTrait51_S60child1();

int S61_classId = Class_genId();
struct S61{

  int id;
  S61 ():id(S61_classId){

  }
};


int S61child1_classId = Class_genId();
struct S61child1:S61{

  S61child1 (int a):a(a){
    (id = S61child1_classId);
{

    }
  }
  int a;
};


int S61child2_classId = Class_genId();
struct S61child2:S61{

  S61child2 (int a):a(a){
    (id = S61child2_classId);
{

    }
  }
  int a;
};


int S61child3_classId = Class_genId();
struct S61child3:S61{

  S61child3 (int a):a(a){
    (id = S61child3_classId);
{

    }
  }
  int a;
};


int S61child4_classId = Class_genId();
struct S61child4:S61{

  S61child4 (int a):a(a){
    (id = S61child4_classId);
{

    }
  }
  int a;
};


struct Trait61{

  int(*trait61)(Class*);
};

Vec* Trait61_v = newVec();

int Trait61_S61child1_trait61(Class* self_) {
  S61child1* self = ((S61child1*)self_);
{
    return (self -> a);
  }
} 
Trait61* newTrait61_S61child1() {
  Trait61 (* impl) = (new Trait61());
  setVec(Trait61_v, S61child1_classId, ((void*)impl));
  ((impl -> trait61) = (& Trait61_S61child1_trait61));
  return impl;
} 
Trait61* Trait61_S61child1_ = newTrait61_S61child1();

int Trait61_S61child2_trait61(Class* self_) {
  S61child2* self = ((S61child2*)self_);
{
    return (self -> a);
  }
} 
Trait61* newTrait61_S61child2() {
  Trait61 (* impl) = (new Trait61());
  setVec(Trait61_v, S61child2_classId, ((void*)impl));
  ((impl -> trait61) = (& Trait61_S61child2_trait61));
  return impl;
} 
Trait61* Trait61_S61child2_ = newTrait61_S61child2();

int Trait61_S61child3_trait61(Class* self_) {
  S61child3* self = ((S61child3*)self_);
{
    return (self -> a);
  }
} 
Trait61* newTrait61_S61child3() {
  Trait61 (* impl) = (new Trait61());
  setVec(Trait61_v, S61child3_classId, ((void*)impl));
  ((impl -> trait61) = (& Trait61_S61child3_trait61));
  return impl;
} 
Trait61* Trait61_S61child3_ = newTrait61_S61child3();

int Trait10_S61child1_trait10(Class* self_) {
  S61child1* self = ((S61child1*)self_);
{
    return (self -> a);
  }
} 
Trait10* newTrait10_S61child1() {
  Trait10 (* impl) = (new Trait10());
  setVec(Trait10_v, S61child1_classId, ((void*)impl));
  ((impl -> trait10) = (& Trait10_S61child1_trait10));
  return impl;
} 
Trait10* Trait10_S61child1_ = newTrait10_S61child1();

int S62_classId = Class_genId();
struct S62{

  int id;
  S62 ():id(S62_classId){

  }
};


int S62child1_classId = Class_genId();
struct S62child1:S62{

  S62child1 (int a):a(a){
    (id = S62child1_classId);
{

    }
  }
  int a;
};


struct Trait62{

  int(*trait62)(Class*);
};

Vec* Trait62_v = newVec();

int Trait62_S62child1_trait62(Class* self_) {
  S62child1* self = ((S62child1*)self_);
{
    return (self -> a);
  }
} 
Trait62* newTrait62_S62child1() {
  Trait62 (* impl) = (new Trait62());
  setVec(Trait62_v, S62child1_classId, ((void*)impl));
  ((impl -> trait62) = (& Trait62_S62child1_trait62));
  return impl;
} 
Trait62* Trait62_S62child1_ = newTrait62_S62child1();

int Trait38_S62child1_trait38(Class* self_) {
  S62child1* self = ((S62child1*)self_);
{
    return (self -> a);
  }
} 
Trait38* newTrait38_S62child1() {
  Trait38 (* impl) = (new Trait38());
  setVec(Trait38_v, S62child1_classId, ((void*)impl));
  ((impl -> trait38) = (& Trait38_S62child1_trait38));
  return impl;
} 
Trait38* Trait38_S62child1_ = newTrait38_S62child1();

int S63_classId = Class_genId();
struct S63{

  int id;
  S63 ():id(S63_classId){

  }
};


int S63child1_classId = Class_genId();
struct S63child1:S63{

  S63child1 (int a):a(a){
    (id = S63child1_classId);
{

    }
  }
  int a;
};


int S63child2_classId = Class_genId();
struct S63child2:S63{

  S63child2 (int a):a(a){
    (id = S63child2_classId);
{

    }
  }
  int a;
};


struct Trait63{

  int(*trait63)(Class*);
};

Vec* Trait63_v = newVec();

int Trait63_S63child1_trait63(Class* self_) {
  S63child1* self = ((S63child1*)self_);
{
    return (self -> a);
  }
} 
Trait63* newTrait63_S63child1() {
  Trait63 (* impl) = (new Trait63());
  setVec(Trait63_v, S63child1_classId, ((void*)impl));
  ((impl -> trait63) = (& Trait63_S63child1_trait63));
  return impl;
} 
Trait63* Trait63_S63child1_ = newTrait63_S63child1();

int Trait47_S63child1_trait47(Class* self_) {
  S63child1* self = ((S63child1*)self_);
{
    return (self -> a);
  }
} 
Trait47* newTrait47_S63child1() {
  Trait47 (* impl) = (new Trait47());
  setVec(Trait47_v, S63child1_classId, ((void*)impl));
  ((impl -> trait47) = (& Trait47_S63child1_trait47));
  return impl;
} 
Trait47* Trait47_S63child1_ = newTrait47_S63child1();

int S64_classId = Class_genId();
struct S64{

  int id;
  S64 ():id(S64_classId){

  }
};


int S64child1_classId = Class_genId();
struct S64child1:S64{

  S64child1 (int a):a(a){
    (id = S64child1_classId);
{

    }
  }
  int a;
};


struct Trait64{

  int(*trait64)(Class*);
};

Vec* Trait64_v = newVec();

int Trait64_S64child1_trait64(Class* self_) {
  S64child1* self = ((S64child1*)self_);
{
    return (self -> a);
  }
} 
Trait64* newTrait64_S64child1() {
  Trait64 (* impl) = (new Trait64());
  setVec(Trait64_v, S64child1_classId, ((void*)impl));
  ((impl -> trait64) = (& Trait64_S64child1_trait64));
  return impl;
} 
Trait64* Trait64_S64child1_ = newTrait64_S64child1();

int Trait1_S64child1_trait1(Class* self_) {
  S64child1* self = ((S64child1*)self_);
{
    return (self -> a);
  }
} 
Trait1* newTrait1_S64child1() {
  Trait1 (* impl) = (new Trait1());
  setVec(Trait1_v, S64child1_classId, ((void*)impl));
  ((impl -> trait1) = (& Trait1_S64child1_trait1));
  return impl;
} 
Trait1* Trait1_S64child1_ = newTrait1_S64child1();

int S65_classId = Class_genId();
struct S65{

  int id;
  S65 ():id(S65_classId){

  }
};


int S65child1_classId = Class_genId();
struct S65child1:S65{

  S65child1 (int a):a(a){
    (id = S65child1_classId);
{

    }
  }
  int a;
};


int S65child2_classId = Class_genId();
struct S65child2:S65{

  S65child2 (int a):a(a){
    (id = S65child2_classId);
{

    }
  }
  int a;
};


struct Trait65{

  int(*trait65)(Class*);
};

Vec* Trait65_v = newVec();

int Trait65_S65child1_trait65(Class* self_) {
  S65child1* self = ((S65child1*)self_);
{
    return (self -> a);
  }
} 
Trait65* newTrait65_S65child1() {
  Trait65 (* impl) = (new Trait65());
  setVec(Trait65_v, S65child1_classId, ((void*)impl));
  ((impl -> trait65) = (& Trait65_S65child1_trait65));
  return impl;
} 
Trait65* Trait65_S65child1_ = newTrait65_S65child1();

int Trait29_S65child1_trait29(Class* self_) {
  S65child1* self = ((S65child1*)self_);
{
    return (self -> a);
  }
} 
Trait29* newTrait29_S65child1() {
  Trait29 (* impl) = (new Trait29());
  setVec(Trait29_v, S65child1_classId, ((void*)impl));
  ((impl -> trait29) = (& Trait29_S65child1_trait29));
  return impl;
} 
Trait29* Trait29_S65child1_ = newTrait29_S65child1();

int S66_classId = Class_genId();
struct S66{

  int id;
  S66 ():id(S66_classId){

  }
};


int S66child1_classId = Class_genId();
struct S66child1:S66{

  S66child1 (int a):a(a){
    (id = S66child1_classId);
{

    }
  }
  int a;
};


int S66child2_classId = Class_genId();
struct S66child2:S66{

  S66child2 (int a):a(a){
    (id = S66child2_classId);
{

    }
  }
  int a;
};


struct Trait66{

  int(*trait66)(Class*);
};

Vec* Trait66_v = newVec();

int Trait66_S66child1_trait66(Class* self_) {
  S66child1* self = ((S66child1*)self_);
{
    return (self -> a);
  }
} 
Trait66* newTrait66_S66child1() {
  Trait66 (* impl) = (new Trait66());
  setVec(Trait66_v, S66child1_classId, ((void*)impl));
  ((impl -> trait66) = (& Trait66_S66child1_trait66));
  return impl;
} 
Trait66* Trait66_S66child1_ = newTrait66_S66child1();

int Trait30_S66child1_trait30(Class* self_) {
  S66child1* self = ((S66child1*)self_);
{
    return (self -> a);
  }
} 
Trait30* newTrait30_S66child1() {
  Trait30 (* impl) = (new Trait30());
  setVec(Trait30_v, S66child1_classId, ((void*)impl));
  ((impl -> trait30) = (& Trait30_S66child1_trait30));
  return impl;
} 
Trait30* Trait30_S66child1_ = newTrait30_S66child1();

int S67_classId = Class_genId();
struct S67{

  int id;
  S67 ():id(S67_classId){

  }
};


int S67child1_classId = Class_genId();
struct S67child1:S67{

  S67child1 (int a):a(a){
    (id = S67child1_classId);
{

    }
  }
  int a;
};


int S67child2_classId = Class_genId();
struct S67child2:S67{

  S67child2 (int a):a(a){
    (id = S67child2_classId);
{

    }
  }
  int a;
};


int S67child3_classId = Class_genId();
struct S67child3:S67{

  S67child3 (int a):a(a){
    (id = S67child3_classId);
{

    }
  }
  int a;
};


struct Trait67{

  int(*trait67)(Class*);
};

Vec* Trait67_v = newVec();

int Trait67_S67child1_trait67(Class* self_) {
  S67child1* self = ((S67child1*)self_);
{
    return (self -> a);
  }
} 
Trait67* newTrait67_S67child1() {
  Trait67 (* impl) = (new Trait67());
  setVec(Trait67_v, S67child1_classId, ((void*)impl));
  ((impl -> trait67) = (& Trait67_S67child1_trait67));
  return impl;
} 
Trait67* Trait67_S67child1_ = newTrait67_S67child1();

int Trait67_S67child2_trait67(Class* self_) {
  S67child2* self = ((S67child2*)self_);
{
    return (self -> a);
  }
} 
Trait67* newTrait67_S67child2() {
  Trait67 (* impl) = (new Trait67());
  setVec(Trait67_v, S67child2_classId, ((void*)impl));
  ((impl -> trait67) = (& Trait67_S67child2_trait67));
  return impl;
} 
Trait67* Trait67_S67child2_ = newTrait67_S67child2();

int Trait48_S67child1_trait48(Class* self_) {
  S67child1* self = ((S67child1*)self_);
{
    return (self -> a);
  }
} 
Trait48* newTrait48_S67child1() {
  Trait48 (* impl) = (new Trait48());
  setVec(Trait48_v, S67child1_classId, ((void*)impl));
  ((impl -> trait48) = (& Trait48_S67child1_trait48));
  return impl;
} 
Trait48* Trait48_S67child1_ = newTrait48_S67child1();

int S68_classId = Class_genId();
struct S68{

  int id;
  S68 ():id(S68_classId){

  }
};


int S68child1_classId = Class_genId();
struct S68child1:S68{

  S68child1 (int a):a(a){
    (id = S68child1_classId);
{

    }
  }
  int a;
};


int S68child2_classId = Class_genId();
struct S68child2:S68{

  S68child2 (int a):a(a){
    (id = S68child2_classId);
{

    }
  }
  int a;
};


int S68child3_classId = Class_genId();
struct S68child3:S68{

  S68child3 (int a):a(a){
    (id = S68child3_classId);
{

    }
  }
  int a;
};


int S68child4_classId = Class_genId();
struct S68child4:S68{

  S68child4 (int a):a(a){
    (id = S68child4_classId);
{

    }
  }
  int a;
};


int S68child5_classId = Class_genId();
struct S68child5:S68{

  S68child5 (int a):a(a){
    (id = S68child5_classId);
{

    }
  }
  int a;
};


struct Trait68{

  int(*trait68)(Class*);
};

Vec* Trait68_v = newVec();

int Trait68_S68child1_trait68(Class* self_) {
  S68child1* self = ((S68child1*)self_);
{
    return (self -> a);
  }
} 
Trait68* newTrait68_S68child1() {
  Trait68 (* impl) = (new Trait68());
  setVec(Trait68_v, S68child1_classId, ((void*)impl));
  ((impl -> trait68) = (& Trait68_S68child1_trait68));
  return impl;
} 
Trait68* Trait68_S68child1_ = newTrait68_S68child1();

int Trait68_S68child2_trait68(Class* self_) {
  S68child2* self = ((S68child2*)self_);
{
    return (self -> a);
  }
} 
Trait68* newTrait68_S68child2() {
  Trait68 (* impl) = (new Trait68());
  setVec(Trait68_v, S68child2_classId, ((void*)impl));
  ((impl -> trait68) = (& Trait68_S68child2_trait68));
  return impl;
} 
Trait68* Trait68_S68child2_ = newTrait68_S68child2();

int Trait48_S68child1_trait48(Class* self_) {
  S68child1* self = ((S68child1*)self_);
{
    return (self -> a);
  }
} 
Trait48* newTrait48_S68child1() {
  Trait48 (* impl) = (new Trait48());
  setVec(Trait48_v, S68child1_classId, ((void*)impl));
  ((impl -> trait48) = (& Trait48_S68child1_trait48));
  return impl;
} 
Trait48* Trait48_S68child1_ = newTrait48_S68child1();

int S69_classId = Class_genId();
struct S69{

  int id;
  S69 ():id(S69_classId){

  }
};


int S69child1_classId = Class_genId();
struct S69child1:S69{

  S69child1 (int a):a(a){
    (id = S69child1_classId);
{

    }
  }
  int a;
};


int S69child2_classId = Class_genId();
struct S69child2:S69{

  S69child2 (int a):a(a){
    (id = S69child2_classId);
{

    }
  }
  int a;
};


struct Trait69{

  int(*trait69)(Class*);
};

Vec* Trait69_v = newVec();

int Trait69_S69child1_trait69(Class* self_) {
  S69child1* self = ((S69child1*)self_);
{
    return (self -> a);
  }
} 
Trait69* newTrait69_S69child1() {
  Trait69 (* impl) = (new Trait69());
  setVec(Trait69_v, S69child1_classId, ((void*)impl));
  ((impl -> trait69) = (& Trait69_S69child1_trait69));
  return impl;
} 
Trait69* Trait69_S69child1_ = newTrait69_S69child1();

int Trait13_S69child1_trait13(Class* self_) {
  S69child1* self = ((S69child1*)self_);
{
    return (self -> a);
  }
} 
Trait13* newTrait13_S69child1() {
  Trait13 (* impl) = (new Trait13());
  setVec(Trait13_v, S69child1_classId, ((void*)impl));
  ((impl -> trait13) = (& Trait13_S69child1_trait13));
  return impl;
} 
Trait13* Trait13_S69child1_ = newTrait13_S69child1();

int S70_classId = Class_genId();
struct S70{

  int id;
  S70 ():id(S70_classId){

  }
};


int S70child1_classId = Class_genId();
struct S70child1:S70{

  S70child1 (int a):a(a){
    (id = S70child1_classId);
{

    }
  }
  int a;
};


int S70child2_classId = Class_genId();
struct S70child2:S70{

  S70child2 (int a):a(a){
    (id = S70child2_classId);
{

    }
  }
  int a;
};


int S70child3_classId = Class_genId();
struct S70child3:S70{

  S70child3 (int a):a(a){
    (id = S70child3_classId);
{

    }
  }
  int a;
};


struct Trait70{

  int(*trait70)(Class*);
};

Vec* Trait70_v = newVec();

int Trait70_S70child1_trait70(Class* self_) {
  S70child1* self = ((S70child1*)self_);
{
    return (self -> a);
  }
} 
Trait70* newTrait70_S70child1() {
  Trait70 (* impl) = (new Trait70());
  setVec(Trait70_v, S70child1_classId, ((void*)impl));
  ((impl -> trait70) = (& Trait70_S70child1_trait70));
  return impl;
} 
Trait70* Trait70_S70child1_ = newTrait70_S70child1();

int Trait70_S70child2_trait70(Class* self_) {
  S70child2* self = ((S70child2*)self_);
{
    return (self -> a);
  }
} 
Trait70* newTrait70_S70child2() {
  Trait70 (* impl) = (new Trait70());
  setVec(Trait70_v, S70child2_classId, ((void*)impl));
  ((impl -> trait70) = (& Trait70_S70child2_trait70));
  return impl;
} 
Trait70* Trait70_S70child2_ = newTrait70_S70child2();

int Trait2_S70child1_trait2(Class* self_) {
  S70child1* self = ((S70child1*)self_);
{
    return (self -> a);
  }
} 
Trait2* newTrait2_S70child1() {
  Trait2 (* impl) = (new Trait2());
  setVec(Trait2_v, S70child1_classId, ((void*)impl));
  ((impl -> trait2) = (& Trait2_S70child1_trait2));
  return impl;
} 
Trait2* Trait2_S70child1_ = newTrait2_S70child1();

int S71_classId = Class_genId();
struct S71{

  int id;
  S71 ():id(S71_classId){

  }
};


int S71child1_classId = Class_genId();
struct S71child1:S71{

  S71child1 (int a):a(a){
    (id = S71child1_classId);
{

    }
  }
  int a;
};


int S71child2_classId = Class_genId();
struct S71child2:S71{

  S71child2 (int a):a(a){
    (id = S71child2_classId);
{

    }
  }
  int a;
};


struct Trait71{

  int(*trait71)(Class*);
};

Vec* Trait71_v = newVec();

int Trait71_S71child1_trait71(Class* self_) {
  S71child1* self = ((S71child1*)self_);
{
    return (self -> a);
  }
} 
Trait71* newTrait71_S71child1() {
  Trait71 (* impl) = (new Trait71());
  setVec(Trait71_v, S71child1_classId, ((void*)impl));
  ((impl -> trait71) = (& Trait71_S71child1_trait71));
  return impl;
} 
Trait71* Trait71_S71child1_ = newTrait71_S71child1();

int Trait71_S71child2_trait71(Class* self_) {
  S71child2* self = ((S71child2*)self_);
{
    return (self -> a);
  }
} 
Trait71* newTrait71_S71child2() {
  Trait71 (* impl) = (new Trait71());
  setVec(Trait71_v, S71child2_classId, ((void*)impl));
  ((impl -> trait71) = (& Trait71_S71child2_trait71));
  return impl;
} 
Trait71* Trait71_S71child2_ = newTrait71_S71child2();

int Trait18_S71child1_trait18(Class* self_) {
  S71child1* self = ((S71child1*)self_);
{
    return (self -> a);
  }
} 
Trait18* newTrait18_S71child1() {
  Trait18 (* impl) = (new Trait18());
  setVec(Trait18_v, S71child1_classId, ((void*)impl));
  ((impl -> trait18) = (& Trait18_S71child1_trait18));
  return impl;
} 
Trait18* Trait18_S71child1_ = newTrait18_S71child1();

int S72_classId = Class_genId();
struct S72{

  int id;
  S72 ():id(S72_classId){

  }
};


int S72child1_classId = Class_genId();
struct S72child1:S72{

  S72child1 (int a):a(a){
    (id = S72child1_classId);
{

    }
  }
  int a;
};


int S72child2_classId = Class_genId();
struct S72child2:S72{

  S72child2 (int a):a(a){
    (id = S72child2_classId);
{

    }
  }
  int a;
};


struct Trait72{

  int(*trait72)(Class*);
};

Vec* Trait72_v = newVec();

int Trait72_S72child1_trait72(Class* self_) {
  S72child1* self = ((S72child1*)self_);
{
    return (self -> a);
  }
} 
Trait72* newTrait72_S72child1() {
  Trait72 (* impl) = (new Trait72());
  setVec(Trait72_v, S72child1_classId, ((void*)impl));
  ((impl -> trait72) = (& Trait72_S72child1_trait72));
  return impl;
} 
Trait72* Trait72_S72child1_ = newTrait72_S72child1();

int Trait72_S72child2_trait72(Class* self_) {
  S72child2* self = ((S72child2*)self_);
{
    return (self -> a);
  }
} 
Trait72* newTrait72_S72child2() {
  Trait72 (* impl) = (new Trait72());
  setVec(Trait72_v, S72child2_classId, ((void*)impl));
  ((impl -> trait72) = (& Trait72_S72child2_trait72));
  return impl;
} 
Trait72* Trait72_S72child2_ = newTrait72_S72child2();

int Trait27_S72child1_trait27(Class* self_) {
  S72child1* self = ((S72child1*)self_);
{
    return (self -> a);
  }
} 
Trait27* newTrait27_S72child1() {
  Trait27 (* impl) = (new Trait27());
  setVec(Trait27_v, S72child1_classId, ((void*)impl));
  ((impl -> trait27) = (& Trait27_S72child1_trait27));
  return impl;
} 
Trait27* Trait27_S72child1_ = newTrait27_S72child1();

int S73_classId = Class_genId();
struct S73{

  int id;
  S73 ():id(S73_classId){

  }
};


int S73child1_classId = Class_genId();
struct S73child1:S73{

  S73child1 (int a):a(a){
    (id = S73child1_classId);
{

    }
  }
  int a;
};


int S73child2_classId = Class_genId();
struct S73child2:S73{

  S73child2 (int a):a(a){
    (id = S73child2_classId);
{

    }
  }
  int a;
};


struct Trait73{

  int(*trait73)(Class*);
};

Vec* Trait73_v = newVec();

int Trait73_S73child1_trait73(Class* self_) {
  S73child1* self = ((S73child1*)self_);
{
    return (self -> a);
  }
} 
Trait73* newTrait73_S73child1() {
  Trait73 (* impl) = (new Trait73());
  setVec(Trait73_v, S73child1_classId, ((void*)impl));
  ((impl -> trait73) = (& Trait73_S73child1_trait73));
  return impl;
} 
Trait73* Trait73_S73child1_ = newTrait73_S73child1();

int Trait7_S73child1_trait7(Class* self_) {
  S73child1* self = ((S73child1*)self_);
{
    return (self -> a);
  }
} 
Trait7* newTrait7_S73child1() {
  Trait7 (* impl) = (new Trait7());
  setVec(Trait7_v, S73child1_classId, ((void*)impl));
  ((impl -> trait7) = (& Trait7_S73child1_trait7));
  return impl;
} 
Trait7* Trait7_S73child1_ = newTrait7_S73child1();

int S74_classId = Class_genId();
struct S74{

  int id;
  S74 ():id(S74_classId){

  }
};


int S74child1_classId = Class_genId();
struct S74child1:S74{

  S74child1 (int a):a(a){
    (id = S74child1_classId);
{

    }
  }
  int a;
};


int S74child2_classId = Class_genId();
struct S74child2:S74{

  S74child2 (int a):a(a){
    (id = S74child2_classId);
{

    }
  }
  int a;
};


struct Trait74{

  int(*trait74)(Class*);
};

Vec* Trait74_v = newVec();

int Trait74_S74child1_trait74(Class* self_) {
  S74child1* self = ((S74child1*)self_);
{
    return (self -> a);
  }
} 
Trait74* newTrait74_S74child1() {
  Trait74 (* impl) = (new Trait74());
  setVec(Trait74_v, S74child1_classId, ((void*)impl));
  ((impl -> trait74) = (& Trait74_S74child1_trait74));
  return impl;
} 
Trait74* Trait74_S74child1_ = newTrait74_S74child1();

int Trait20_S74child1_trait20(Class* self_) {
  S74child1* self = ((S74child1*)self_);
{
    return (self -> a);
  }
} 
Trait20* newTrait20_S74child1() {
  Trait20 (* impl) = (new Trait20());
  setVec(Trait20_v, S74child1_classId, ((void*)impl));
  ((impl -> trait20) = (& Trait20_S74child1_trait20));
  return impl;
} 
Trait20* Trait20_S74child1_ = newTrait20_S74child1();

int S75_classId = Class_genId();
struct S75{

  int id;
  S75 ():id(S75_classId){

  }
};


int S75child1_classId = Class_genId();
struct S75child1:S75{

  S75child1 (int a):a(a){
    (id = S75child1_classId);
{

    }
  }
  int a;
};


int S75child2_classId = Class_genId();
struct S75child2:S75{

  S75child2 (int a):a(a){
    (id = S75child2_classId);
{

    }
  }
  int a;
};


int S75child3_classId = Class_genId();
struct S75child3:S75{

  S75child3 (int a):a(a){
    (id = S75child3_classId);
{

    }
  }
  int a;
};


int S75child4_classId = Class_genId();
struct S75child4:S75{

  S75child4 (int a):a(a){
    (id = S75child4_classId);
{

    }
  }
  int a;
};


struct Trait75{

  int(*trait75)(Class*);
};

Vec* Trait75_v = newVec();

int Trait75_S75child1_trait75(Class* self_) {
  S75child1* self = ((S75child1*)self_);
{
    return (self -> a);
  }
} 
Trait75* newTrait75_S75child1() {
  Trait75 (* impl) = (new Trait75());
  setVec(Trait75_v, S75child1_classId, ((void*)impl));
  ((impl -> trait75) = (& Trait75_S75child1_trait75));
  return impl;
} 
Trait75* Trait75_S75child1_ = newTrait75_S75child1();

int Trait75_S75child2_trait75(Class* self_) {
  S75child2* self = ((S75child2*)self_);
{
    return (self -> a);
  }
} 
Trait75* newTrait75_S75child2() {
  Trait75 (* impl) = (new Trait75());
  setVec(Trait75_v, S75child2_classId, ((void*)impl));
  ((impl -> trait75) = (& Trait75_S75child2_trait75));
  return impl;
} 
Trait75* Trait75_S75child2_ = newTrait75_S75child2();

int Trait75_S75child3_trait75(Class* self_) {
  S75child3* self = ((S75child3*)self_);
{
    return (self -> a);
  }
} 
Trait75* newTrait75_S75child3() {
  Trait75 (* impl) = (new Trait75());
  setVec(Trait75_v, S75child3_classId, ((void*)impl));
  ((impl -> trait75) = (& Trait75_S75child3_trait75));
  return impl;
} 
Trait75* Trait75_S75child3_ = newTrait75_S75child3();

int Trait42_S75child1_trait42(Class* self_) {
  S75child1* self = ((S75child1*)self_);
{
    return (self -> a);
  }
} 
Trait42* newTrait42_S75child1() {
  Trait42 (* impl) = (new Trait42());
  setVec(Trait42_v, S75child1_classId, ((void*)impl));
  ((impl -> trait42) = (& Trait42_S75child1_trait42));
  return impl;
} 
Trait42* Trait42_S75child1_ = newTrait42_S75child1();

int S76_classId = Class_genId();
struct S76{

  int id;
  S76 ():id(S76_classId){

  }
};


int S76child1_classId = Class_genId();
struct S76child1:S76{

  S76child1 (int a):a(a){
    (id = S76child1_classId);
{

    }
  }
  int a;
};


int S76child2_classId = Class_genId();
struct S76child2:S76{

  S76child2 (int a):a(a){
    (id = S76child2_classId);
{

    }
  }
  int a;
};


int S76child3_classId = Class_genId();
struct S76child3:S76{

  S76child3 (int a):a(a){
    (id = S76child3_classId);
{

    }
  }
  int a;
};


struct Trait76{

  int(*trait76)(Class*);
};

Vec* Trait76_v = newVec();

int Trait76_S76child1_trait76(Class* self_) {
  S76child1* self = ((S76child1*)self_);
{
    return (self -> a);
  }
} 
Trait76* newTrait76_S76child1() {
  Trait76 (* impl) = (new Trait76());
  setVec(Trait76_v, S76child1_classId, ((void*)impl));
  ((impl -> trait76) = (& Trait76_S76child1_trait76));
  return impl;
} 
Trait76* Trait76_S76child1_ = newTrait76_S76child1();

int Trait76_S76child2_trait76(Class* self_) {
  S76child2* self = ((S76child2*)self_);
{
    return (self -> a);
  }
} 
Trait76* newTrait76_S76child2() {
  Trait76 (* impl) = (new Trait76());
  setVec(Trait76_v, S76child2_classId, ((void*)impl));
  ((impl -> trait76) = (& Trait76_S76child2_trait76));
  return impl;
} 
Trait76* Trait76_S76child2_ = newTrait76_S76child2();

int Trait55_S76child1_trait55(Class* self_) {
  S76child1* self = ((S76child1*)self_);
{
    return (self -> a);
  }
} 
Trait55* newTrait55_S76child1() {
  Trait55 (* impl) = (new Trait55());
  setVec(Trait55_v, S76child1_classId, ((void*)impl));
  ((impl -> trait55) = (& Trait55_S76child1_trait55));
  return impl;
} 
Trait55* Trait55_S76child1_ = newTrait55_S76child1();

int S77_classId = Class_genId();
struct S77{

  int id;
  S77 ():id(S77_classId){

  }
};


int S77child1_classId = Class_genId();
struct S77child1:S77{

  S77child1 (int a):a(a){
    (id = S77child1_classId);
{

    }
  }
  int a;
};


int S77child2_classId = Class_genId();
struct S77child2:S77{

  S77child2 (int a):a(a){
    (id = S77child2_classId);
{

    }
  }
  int a;
};


int S77child3_classId = Class_genId();
struct S77child3:S77{

  S77child3 (int a):a(a){
    (id = S77child3_classId);
{

    }
  }
  int a;
};


int S77child4_classId = Class_genId();
struct S77child4:S77{

  S77child4 (int a):a(a){
    (id = S77child4_classId);
{

    }
  }
  int a;
};


int S77child5_classId = Class_genId();
struct S77child5:S77{

  S77child5 (int a):a(a){
    (id = S77child5_classId);
{

    }
  }
  int a;
};


struct Trait77{

  int(*trait77)(Class*);
};

Vec* Trait77_v = newVec();

int Trait77_S77child1_trait77(Class* self_) {
  S77child1* self = ((S77child1*)self_);
{
    return (self -> a);
  }
} 
Trait77* newTrait77_S77child1() {
  Trait77 (* impl) = (new Trait77());
  setVec(Trait77_v, S77child1_classId, ((void*)impl));
  ((impl -> trait77) = (& Trait77_S77child1_trait77));
  return impl;
} 
Trait77* Trait77_S77child1_ = newTrait77_S77child1();

int Trait77_S77child2_trait77(Class* self_) {
  S77child2* self = ((S77child2*)self_);
{
    return (self -> a);
  }
} 
Trait77* newTrait77_S77child2() {
  Trait77 (* impl) = (new Trait77());
  setVec(Trait77_v, S77child2_classId, ((void*)impl));
  ((impl -> trait77) = (& Trait77_S77child2_trait77));
  return impl;
} 
Trait77* Trait77_S77child2_ = newTrait77_S77child2();

int Trait77_S77child3_trait77(Class* self_) {
  S77child3* self = ((S77child3*)self_);
{
    return (self -> a);
  }
} 
Trait77* newTrait77_S77child3() {
  Trait77 (* impl) = (new Trait77());
  setVec(Trait77_v, S77child3_classId, ((void*)impl));
  ((impl -> trait77) = (& Trait77_S77child3_trait77));
  return impl;
} 
Trait77* Trait77_S77child3_ = newTrait77_S77child3();

int Trait8_S77child1_trait8(Class* self_) {
  S77child1* self = ((S77child1*)self_);
{
    return (self -> a);
  }
} 
Trait8* newTrait8_S77child1() {
  Trait8 (* impl) = (new Trait8());
  setVec(Trait8_v, S77child1_classId, ((void*)impl));
  ((impl -> trait8) = (& Trait8_S77child1_trait8));
  return impl;
} 
Trait8* Trait8_S77child1_ = newTrait8_S77child1();

int S78_classId = Class_genId();
struct S78{

  int id;
  S78 ():id(S78_classId){

  }
};


int S78child1_classId = Class_genId();
struct S78child1:S78{

  S78child1 (int a):a(a){
    (id = S78child1_classId);
{

    }
  }
  int a;
};


int S78child2_classId = Class_genId();
struct S78child2:S78{

  S78child2 (int a):a(a){
    (id = S78child2_classId);
{

    }
  }
  int a;
};


struct Trait78{

  int(*trait78)(Class*);
};

Vec* Trait78_v = newVec();

int Trait78_S78child1_trait78(Class* self_) {
  S78child1* self = ((S78child1*)self_);
{
    return (self -> a);
  }
} 
Trait78* newTrait78_S78child1() {
  Trait78 (* impl) = (new Trait78());
  setVec(Trait78_v, S78child1_classId, ((void*)impl));
  ((impl -> trait78) = (& Trait78_S78child1_trait78));
  return impl;
} 
Trait78* Trait78_S78child1_ = newTrait78_S78child1();

int Trait2_S78child1_trait2(Class* self_) {
  S78child1* self = ((S78child1*)self_);
{
    return (self -> a);
  }
} 
Trait2* newTrait2_S78child1() {
  Trait2 (* impl) = (new Trait2());
  setVec(Trait2_v, S78child1_classId, ((void*)impl));
  ((impl -> trait2) = (& Trait2_S78child1_trait2));
  return impl;
} 
Trait2* Trait2_S78child1_ = newTrait2_S78child1();

int S79_classId = Class_genId();
struct S79{

  int id;
  S79 ():id(S79_classId){

  }
};


int S79child1_classId = Class_genId();
struct S79child1:S79{

  S79child1 (int a):a(a){
    (id = S79child1_classId);
{

    }
  }
  int a;
};


int S79child2_classId = Class_genId();
struct S79child2:S79{

  S79child2 (int a):a(a){
    (id = S79child2_classId);
{

    }
  }
  int a;
};


int S79child3_classId = Class_genId();
struct S79child3:S79{

  S79child3 (int a):a(a){
    (id = S79child3_classId);
{

    }
  }
  int a;
};


int S79child4_classId = Class_genId();
struct S79child4:S79{

  S79child4 (int a):a(a){
    (id = S79child4_classId);
{

    }
  }
  int a;
};


struct Trait79{

  int(*trait79)(Class*);
};

Vec* Trait79_v = newVec();

int Trait79_S79child1_trait79(Class* self_) {
  S79child1* self = ((S79child1*)self_);
{
    return (self -> a);
  }
} 
Trait79* newTrait79_S79child1() {
  Trait79 (* impl) = (new Trait79());
  setVec(Trait79_v, S79child1_classId, ((void*)impl));
  ((impl -> trait79) = (& Trait79_S79child1_trait79));
  return impl;
} 
Trait79* Trait79_S79child1_ = newTrait79_S79child1();

int Trait79_S79child2_trait79(Class* self_) {
  S79child2* self = ((S79child2*)self_);
{
    return (self -> a);
  }
} 
Trait79* newTrait79_S79child2() {
  Trait79 (* impl) = (new Trait79());
  setVec(Trait79_v, S79child2_classId, ((void*)impl));
  ((impl -> trait79) = (& Trait79_S79child2_trait79));
  return impl;
} 
Trait79* Trait79_S79child2_ = newTrait79_S79child2();

int Trait61_S79child1_trait61(Class* self_) {
  S79child1* self = ((S79child1*)self_);
{
    return (self -> a);
  }
} 
Trait61* newTrait61_S79child1() {
  Trait61 (* impl) = (new Trait61());
  setVec(Trait61_v, S79child1_classId, ((void*)impl));
  ((impl -> trait61) = (& Trait61_S79child1_trait61));
  return impl;
} 
Trait61* Trait61_S79child1_ = newTrait61_S79child1();

int S80_classId = Class_genId();
struct S80{

  int id;
  S80 ():id(S80_classId){

  }
};


int S80child1_classId = Class_genId();
struct S80child1:S80{

  S80child1 (int a):a(a){
    (id = S80child1_classId);
{

    }
  }
  int a;
};


struct Trait80{

  int(*trait80)(Class*);
};

Vec* Trait80_v = newVec();

int Trait80_S80child1_trait80(Class* self_) {
  S80child1* self = ((S80child1*)self_);
{
    return (self -> a);
  }
} 
Trait80* newTrait80_S80child1() {
  Trait80 (* impl) = (new Trait80());
  setVec(Trait80_v, S80child1_classId, ((void*)impl));
  ((impl -> trait80) = (& Trait80_S80child1_trait80));
  return impl;
} 
Trait80* Trait80_S80child1_ = newTrait80_S80child1();

int Trait70_S80child1_trait70(Class* self_) {
  S80child1* self = ((S80child1*)self_);
{
    return (self -> a);
  }
} 
Trait70* newTrait70_S80child1() {
  Trait70 (* impl) = (new Trait70());
  setVec(Trait70_v, S80child1_classId, ((void*)impl));
  ((impl -> trait70) = (& Trait70_S80child1_trait70));
  return impl;
} 
Trait70* Trait70_S80child1_ = newTrait70_S80child1();

int S81_classId = Class_genId();
struct S81{

  int id;
  S81 ():id(S81_classId){

  }
};


int S81child1_classId = Class_genId();
struct S81child1:S81{

  S81child1 (int a):a(a){
    (id = S81child1_classId);
{

    }
  }
  int a;
};


struct Trait81{

  int(*trait81)(Class*);
};

Vec* Trait81_v = newVec();

int Trait81_S81child1_trait81(Class* self_) {
  S81child1* self = ((S81child1*)self_);
{
    return (self -> a);
  }
} 
Trait81* newTrait81_S81child1() {
  Trait81 (* impl) = (new Trait81());
  setVec(Trait81_v, S81child1_classId, ((void*)impl));
  ((impl -> trait81) = (& Trait81_S81child1_trait81));
  return impl;
} 
Trait81* Trait81_S81child1_ = newTrait81_S81child1();

int S82_classId = Class_genId();
struct S82{

  int id;
  S82 ():id(S82_classId){

  }
};


int S82child1_classId = Class_genId();
struct S82child1:S82{

  S82child1 (int a):a(a){
    (id = S82child1_classId);
{

    }
  }
  int a;
};


struct Trait82{

  int(*trait82)(Class*);
};

Vec* Trait82_v = newVec();

int Trait82_S82child1_trait82(Class* self_) {
  S82child1* self = ((S82child1*)self_);
{
    return (self -> a);
  }
} 
Trait82* newTrait82_S82child1() {
  Trait82 (* impl) = (new Trait82());
  setVec(Trait82_v, S82child1_classId, ((void*)impl));
  ((impl -> trait82) = (& Trait82_S82child1_trait82));
  return impl;
} 
Trait82* Trait82_S82child1_ = newTrait82_S82child1();

int Trait34_S82child1_trait34(Class* self_) {
  S82child1* self = ((S82child1*)self_);
{
    return (self -> a);
  }
} 
Trait34* newTrait34_S82child1() {
  Trait34 (* impl) = (new Trait34());
  setVec(Trait34_v, S82child1_classId, ((void*)impl));
  ((impl -> trait34) = (& Trait34_S82child1_trait34));
  return impl;
} 
Trait34* Trait34_S82child1_ = newTrait34_S82child1();

int S83_classId = Class_genId();
struct S83{

  int id;
  S83 ():id(S83_classId){

  }
};


int S83child1_classId = Class_genId();
struct S83child1:S83{

  S83child1 (int a):a(a){
    (id = S83child1_classId);
{

    }
  }
  int a;
};


int S83child2_classId = Class_genId();
struct S83child2:S83{

  S83child2 (int a):a(a){
    (id = S83child2_classId);
{

    }
  }
  int a;
};


int S83child3_classId = Class_genId();
struct S83child3:S83{

  S83child3 (int a):a(a){
    (id = S83child3_classId);
{

    }
  }
  int a;
};


int S83child4_classId = Class_genId();
struct S83child4:S83{

  S83child4 (int a):a(a){
    (id = S83child4_classId);
{

    }
  }
  int a;
};


struct Trait83{

  int(*trait83)(Class*);
};

Vec* Trait83_v = newVec();

int Trait83_S83child1_trait83(Class* self_) {
  S83child1* self = ((S83child1*)self_);
{
    return (self -> a);
  }
} 
Trait83* newTrait83_S83child1() {
  Trait83 (* impl) = (new Trait83());
  setVec(Trait83_v, S83child1_classId, ((void*)impl));
  ((impl -> trait83) = (& Trait83_S83child1_trait83));
  return impl;
} 
Trait83* Trait83_S83child1_ = newTrait83_S83child1();

int Trait83_S83child2_trait83(Class* self_) {
  S83child2* self = ((S83child2*)self_);
{
    return (self -> a);
  }
} 
Trait83* newTrait83_S83child2() {
  Trait83 (* impl) = (new Trait83());
  setVec(Trait83_v, S83child2_classId, ((void*)impl));
  ((impl -> trait83) = (& Trait83_S83child2_trait83));
  return impl;
} 
Trait83* Trait83_S83child2_ = newTrait83_S83child2();

int Trait83_S83child3_trait83(Class* self_) {
  S83child3* self = ((S83child3*)self_);
{
    return (self -> a);
  }
} 
Trait83* newTrait83_S83child3() {
  Trait83 (* impl) = (new Trait83());
  setVec(Trait83_v, S83child3_classId, ((void*)impl));
  ((impl -> trait83) = (& Trait83_S83child3_trait83));
  return impl;
} 
Trait83* Trait83_S83child3_ = newTrait83_S83child3();

int Trait44_S83child1_trait44(Class* self_) {
  S83child1* self = ((S83child1*)self_);
{
    return (self -> a);
  }
} 
Trait44* newTrait44_S83child1() {
  Trait44 (* impl) = (new Trait44());
  setVec(Trait44_v, S83child1_classId, ((void*)impl));
  ((impl -> trait44) = (& Trait44_S83child1_trait44));
  return impl;
} 
Trait44* Trait44_S83child1_ = newTrait44_S83child1();

int S84_classId = Class_genId();
struct S84{

  int id;
  S84 ():id(S84_classId){

  }
};


int S84child1_classId = Class_genId();
struct S84child1:S84{

  S84child1 (int a):a(a){
    (id = S84child1_classId);
{

    }
  }
  int a;
};


int S84child2_classId = Class_genId();
struct S84child2:S84{

  S84child2 (int a):a(a){
    (id = S84child2_classId);
{

    }
  }
  int a;
};


int S84child3_classId = Class_genId();
struct S84child3:S84{

  S84child3 (int a):a(a){
    (id = S84child3_classId);
{

    }
  }
  int a;
};


struct Trait84{

  int(*trait84)(Class*);
};

Vec* Trait84_v = newVec();

int Trait84_S84child1_trait84(Class* self_) {
  S84child1* self = ((S84child1*)self_);
{
    return (self -> a);
  }
} 
Trait84* newTrait84_S84child1() {
  Trait84 (* impl) = (new Trait84());
  setVec(Trait84_v, S84child1_classId, ((void*)impl));
  ((impl -> trait84) = (& Trait84_S84child1_trait84));
  return impl;
} 
Trait84* Trait84_S84child1_ = newTrait84_S84child1();

int Trait84_S84child2_trait84(Class* self_) {
  S84child2* self = ((S84child2*)self_);
{
    return (self -> a);
  }
} 
Trait84* newTrait84_S84child2() {
  Trait84 (* impl) = (new Trait84());
  setVec(Trait84_v, S84child2_classId, ((void*)impl));
  ((impl -> trait84) = (& Trait84_S84child2_trait84));
  return impl;
} 
Trait84* Trait84_S84child2_ = newTrait84_S84child2();

int Trait11_S84child1_trait11(Class* self_) {
  S84child1* self = ((S84child1*)self_);
{
    return (self -> a);
  }
} 
Trait11* newTrait11_S84child1() {
  Trait11 (* impl) = (new Trait11());
  setVec(Trait11_v, S84child1_classId, ((void*)impl));
  ((impl -> trait11) = (& Trait11_S84child1_trait11));
  return impl;
} 
Trait11* Trait11_S84child1_ = newTrait11_S84child1();

int S85_classId = Class_genId();
struct S85{

  int id;
  S85 ():id(S85_classId){

  }
};


int S85child1_classId = Class_genId();
struct S85child1:S85{

  S85child1 (int a):a(a){
    (id = S85child1_classId);
{

    }
  }
  int a;
};


int S85child2_classId = Class_genId();
struct S85child2:S85{

  S85child2 (int a):a(a){
    (id = S85child2_classId);
{

    }
  }
  int a;
};


int S85child3_classId = Class_genId();
struct S85child3:S85{

  S85child3 (int a):a(a){
    (id = S85child3_classId);
{

    }
  }
  int a;
};


int S85child4_classId = Class_genId();
struct S85child4:S85{

  S85child4 (int a):a(a){
    (id = S85child4_classId);
{

    }
  }
  int a;
};


struct Trait85{

  int(*trait85)(Class*);
};

Vec* Trait85_v = newVec();

int Trait85_S85child1_trait85(Class* self_) {
  S85child1* self = ((S85child1*)self_);
{
    return (self -> a);
  }
} 
Trait85* newTrait85_S85child1() {
  Trait85 (* impl) = (new Trait85());
  setVec(Trait85_v, S85child1_classId, ((void*)impl));
  ((impl -> trait85) = (& Trait85_S85child1_trait85));
  return impl;
} 
Trait85* Trait85_S85child1_ = newTrait85_S85child1();

int Trait85_S85child2_trait85(Class* self_) {
  S85child2* self = ((S85child2*)self_);
{
    return (self -> a);
  }
} 
Trait85* newTrait85_S85child2() {
  Trait85 (* impl) = (new Trait85());
  setVec(Trait85_v, S85child2_classId, ((void*)impl));
  ((impl -> trait85) = (& Trait85_S85child2_trait85));
  return impl;
} 
Trait85* Trait85_S85child2_ = newTrait85_S85child2();

int Trait85_S85child3_trait85(Class* self_) {
  S85child3* self = ((S85child3*)self_);
{
    return (self -> a);
  }
} 
Trait85* newTrait85_S85child3() {
  Trait85 (* impl) = (new Trait85());
  setVec(Trait85_v, S85child3_classId, ((void*)impl));
  ((impl -> trait85) = (& Trait85_S85child3_trait85));
  return impl;
} 
Trait85* Trait85_S85child3_ = newTrait85_S85child3();

int Trait85_S85child4_trait85(Class* self_) {
  S85child4* self = ((S85child4*)self_);
{
    return (self -> a);
  }
} 
Trait85* newTrait85_S85child4() {
  Trait85 (* impl) = (new Trait85());
  setVec(Trait85_v, S85child4_classId, ((void*)impl));
  ((impl -> trait85) = (& Trait85_S85child4_trait85));
  return impl;
} 
Trait85* Trait85_S85child4_ = newTrait85_S85child4();

int Trait41_S85child1_trait41(Class* self_) {
  S85child1* self = ((S85child1*)self_);
{
    return (self -> a);
  }
} 
Trait41* newTrait41_S85child1() {
  Trait41 (* impl) = (new Trait41());
  setVec(Trait41_v, S85child1_classId, ((void*)impl));
  ((impl -> trait41) = (& Trait41_S85child1_trait41));
  return impl;
} 
Trait41* Trait41_S85child1_ = newTrait41_S85child1();

int S86_classId = Class_genId();
struct S86{

  int id;
  S86 ():id(S86_classId){

  }
};


int S86child1_classId = Class_genId();
struct S86child1:S86{

  S86child1 (int a):a(a){
    (id = S86child1_classId);
{

    }
  }
  int a;
};


int S86child2_classId = Class_genId();
struct S86child2:S86{

  S86child2 (int a):a(a){
    (id = S86child2_classId);
{

    }
  }
  int a;
};


struct Trait86{

  int(*trait86)(Class*);
};

Vec* Trait86_v = newVec();

int Trait86_S86child1_trait86(Class* self_) {
  S86child1* self = ((S86child1*)self_);
{
    return (self -> a);
  }
} 
Trait86* newTrait86_S86child1() {
  Trait86 (* impl) = (new Trait86());
  setVec(Trait86_v, S86child1_classId, ((void*)impl));
  ((impl -> trait86) = (& Trait86_S86child1_trait86));
  return impl;
} 
Trait86* Trait86_S86child1_ = newTrait86_S86child1();

int Trait16_S86child1_trait16(Class* self_) {
  S86child1* self = ((S86child1*)self_);
{
    return (self -> a);
  }
} 
Trait16* newTrait16_S86child1() {
  Trait16 (* impl) = (new Trait16());
  setVec(Trait16_v, S86child1_classId, ((void*)impl));
  ((impl -> trait16) = (& Trait16_S86child1_trait16));
  return impl;
} 
Trait16* Trait16_S86child1_ = newTrait16_S86child1();

int S87_classId = Class_genId();
struct S87{

  int id;
  S87 ():id(S87_classId){

  }
};


int S87child1_classId = Class_genId();
struct S87child1:S87{

  S87child1 (int a):a(a){
    (id = S87child1_classId);
{

    }
  }
  int a;
};


int S87child2_classId = Class_genId();
struct S87child2:S87{

  S87child2 (int a):a(a){
    (id = S87child2_classId);
{

    }
  }
  int a;
};


struct Trait87{

  int(*trait87)(Class*);
};

Vec* Trait87_v = newVec();

int Trait87_S87child1_trait87(Class* self_) {
  S87child1* self = ((S87child1*)self_);
{
    return (self -> a);
  }
} 
Trait87* newTrait87_S87child1() {
  Trait87 (* impl) = (new Trait87());
  setVec(Trait87_v, S87child1_classId, ((void*)impl));
  ((impl -> trait87) = (& Trait87_S87child1_trait87));
  return impl;
} 
Trait87* Trait87_S87child1_ = newTrait87_S87child1();

int Trait52_S87child1_trait52(Class* self_) {
  S87child1* self = ((S87child1*)self_);
{
    return (self -> a);
  }
} 
Trait52* newTrait52_S87child1() {
  Trait52 (* impl) = (new Trait52());
  setVec(Trait52_v, S87child1_classId, ((void*)impl));
  ((impl -> trait52) = (& Trait52_S87child1_trait52));
  return impl;
} 
Trait52* Trait52_S87child1_ = newTrait52_S87child1();

int S88_classId = Class_genId();
struct S88{

  int id;
  S88 ():id(S88_classId){

  }
};


int S88child1_classId = Class_genId();
struct S88child1:S88{

  S88child1 (int a):a(a){
    (id = S88child1_classId);
{

    }
  }
  int a;
};


int S88child2_classId = Class_genId();
struct S88child2:S88{

  S88child2 (int a):a(a){
    (id = S88child2_classId);
{

    }
  }
  int a;
};


struct Trait88{

  int(*trait88)(Class*);
};

Vec* Trait88_v = newVec();

int Trait88_S88child1_trait88(Class* self_) {
  S88child1* self = ((S88child1*)self_);
{
    return (self -> a);
  }
} 
Trait88* newTrait88_S88child1() {
  Trait88 (* impl) = (new Trait88());
  setVec(Trait88_v, S88child1_classId, ((void*)impl));
  ((impl -> trait88) = (& Trait88_S88child1_trait88));
  return impl;
} 
Trait88* Trait88_S88child1_ = newTrait88_S88child1();

int Trait66_S88child1_trait66(Class* self_) {
  S88child1* self = ((S88child1*)self_);
{
    return (self -> a);
  }
} 
Trait66* newTrait66_S88child1() {
  Trait66 (* impl) = (new Trait66());
  setVec(Trait66_v, S88child1_classId, ((void*)impl));
  ((impl -> trait66) = (& Trait66_S88child1_trait66));
  return impl;
} 
Trait66* Trait66_S88child1_ = newTrait66_S88child1();

int S89_classId = Class_genId();
struct S89{

  int id;
  S89 ():id(S89_classId){

  }
};


int S89child1_classId = Class_genId();
struct S89child1:S89{

  S89child1 (int a):a(a){
    (id = S89child1_classId);
{

    }
  }
  int a;
};


int S89child2_classId = Class_genId();
struct S89child2:S89{

  S89child2 (int a):a(a){
    (id = S89child2_classId);
{

    }
  }
  int a;
};


int S89child3_classId = Class_genId();
struct S89child3:S89{

  S89child3 (int a):a(a){
    (id = S89child3_classId);
{

    }
  }
  int a;
};


struct Trait89{

  int(*trait89)(Class*);
};

Vec* Trait89_v = newVec();

int Trait89_S89child1_trait89(Class* self_) {
  S89child1* self = ((S89child1*)self_);
{
    return (self -> a);
  }
} 
Trait89* newTrait89_S89child1() {
  Trait89 (* impl) = (new Trait89());
  setVec(Trait89_v, S89child1_classId, ((void*)impl));
  ((impl -> trait89) = (& Trait89_S89child1_trait89));
  return impl;
} 
Trait89* Trait89_S89child1_ = newTrait89_S89child1();

int Trait13_S89child1_trait13(Class* self_) {
  S89child1* self = ((S89child1*)self_);
{
    return (self -> a);
  }
} 
Trait13* newTrait13_S89child1() {
  Trait13 (* impl) = (new Trait13());
  setVec(Trait13_v, S89child1_classId, ((void*)impl));
  ((impl -> trait13) = (& Trait13_S89child1_trait13));
  return impl;
} 
Trait13* Trait13_S89child1_ = newTrait13_S89child1();

int S90_classId = Class_genId();
struct S90{

  int id;
  S90 ():id(S90_classId){

  }
};


int S90child1_classId = Class_genId();
struct S90child1:S90{

  S90child1 (int a):a(a){
    (id = S90child1_classId);
{

    }
  }
  int a;
};


int S90child2_classId = Class_genId();
struct S90child2:S90{

  S90child2 (int a):a(a){
    (id = S90child2_classId);
{

    }
  }
  int a;
};


int S90child3_classId = Class_genId();
struct S90child3:S90{

  S90child3 (int a):a(a){
    (id = S90child3_classId);
{

    }
  }
  int a;
};


int S90child4_classId = Class_genId();
struct S90child4:S90{

  S90child4 (int a):a(a){
    (id = S90child4_classId);
{

    }
  }
  int a;
};


struct Trait90{

  int(*trait90)(Class*);
};

Vec* Trait90_v = newVec();

int Trait90_S90child1_trait90(Class* self_) {
  S90child1* self = ((S90child1*)self_);
{
    return (self -> a);
  }
} 
Trait90* newTrait90_S90child1() {
  Trait90 (* impl) = (new Trait90());
  setVec(Trait90_v, S90child1_classId, ((void*)impl));
  ((impl -> trait90) = (& Trait90_S90child1_trait90));
  return impl;
} 
Trait90* Trait90_S90child1_ = newTrait90_S90child1();

int Trait90_S90child2_trait90(Class* self_) {
  S90child2* self = ((S90child2*)self_);
{
    return (self -> a);
  }
} 
Trait90* newTrait90_S90child2() {
  Trait90 (* impl) = (new Trait90());
  setVec(Trait90_v, S90child2_classId, ((void*)impl));
  ((impl -> trait90) = (& Trait90_S90child2_trait90));
  return impl;
} 
Trait90* Trait90_S90child2_ = newTrait90_S90child2();

int Trait90_S90child3_trait90(Class* self_) {
  S90child3* self = ((S90child3*)self_);
{
    return (self -> a);
  }
} 
Trait90* newTrait90_S90child3() {
  Trait90 (* impl) = (new Trait90());
  setVec(Trait90_v, S90child3_classId, ((void*)impl));
  ((impl -> trait90) = (& Trait90_S90child3_trait90));
  return impl;
} 
Trait90* Trait90_S90child3_ = newTrait90_S90child3();

int Trait16_S90child1_trait16(Class* self_) {
  S90child1* self = ((S90child1*)self_);
{
    return (self -> a);
  }
} 
Trait16* newTrait16_S90child1() {
  Trait16 (* impl) = (new Trait16());
  setVec(Trait16_v, S90child1_classId, ((void*)impl));
  ((impl -> trait16) = (& Trait16_S90child1_trait16));
  return impl;
} 
Trait16* Trait16_S90child1_ = newTrait16_S90child1();

int S91_classId = Class_genId();
struct S91{

  int id;
  S91 ():id(S91_classId){

  }
};


int S91child1_classId = Class_genId();
struct S91child1:S91{

  S91child1 (int a):a(a){
    (id = S91child1_classId);
{

    }
  }
  int a;
};


int S91child2_classId = Class_genId();
struct S91child2:S91{

  S91child2 (int a):a(a){
    (id = S91child2_classId);
{

    }
  }
  int a;
};


int S91child3_classId = Class_genId();
struct S91child3:S91{

  S91child3 (int a):a(a){
    (id = S91child3_classId);
{

    }
  }
  int a;
};


int S91child4_classId = Class_genId();
struct S91child4:S91{

  S91child4 (int a):a(a){
    (id = S91child4_classId);
{

    }
  }
  int a;
};


int S91child5_classId = Class_genId();
struct S91child5:S91{

  S91child5 (int a):a(a){
    (id = S91child5_classId);
{

    }
  }
  int a;
};


struct Trait91{

  int(*trait91)(Class*);
};

Vec* Trait91_v = newVec();

int Trait91_S91child1_trait91(Class* self_) {
  S91child1* self = ((S91child1*)self_);
{
    return (self -> a);
  }
} 
Trait91* newTrait91_S91child1() {
  Trait91 (* impl) = (new Trait91());
  setVec(Trait91_v, S91child1_classId, ((void*)impl));
  ((impl -> trait91) = (& Trait91_S91child1_trait91));
  return impl;
} 
Trait91* Trait91_S91child1_ = newTrait91_S91child1();

int Trait91_S91child2_trait91(Class* self_) {
  S91child2* self = ((S91child2*)self_);
{
    return (self -> a);
  }
} 
Trait91* newTrait91_S91child2() {
  Trait91 (* impl) = (new Trait91());
  setVec(Trait91_v, S91child2_classId, ((void*)impl));
  ((impl -> trait91) = (& Trait91_S91child2_trait91));
  return impl;
} 
Trait91* Trait91_S91child2_ = newTrait91_S91child2();

int Trait25_S91child1_trait25(Class* self_) {
  S91child1* self = ((S91child1*)self_);
{
    return (self -> a);
  }
} 
Trait25* newTrait25_S91child1() {
  Trait25 (* impl) = (new Trait25());
  setVec(Trait25_v, S91child1_classId, ((void*)impl));
  ((impl -> trait25) = (& Trait25_S91child1_trait25));
  return impl;
} 
Trait25* Trait25_S91child1_ = newTrait25_S91child1();

int S92_classId = Class_genId();
struct S92{

  int id;
  S92 ():id(S92_classId){

  }
};


int S92child1_classId = Class_genId();
struct S92child1:S92{

  S92child1 (int a):a(a){
    (id = S92child1_classId);
{

    }
  }
  int a;
};


int S92child2_classId = Class_genId();
struct S92child2:S92{

  S92child2 (int a):a(a){
    (id = S92child2_classId);
{

    }
  }
  int a;
};


int S92child3_classId = Class_genId();
struct S92child3:S92{

  S92child3 (int a):a(a){
    (id = S92child3_classId);
{

    }
  }
  int a;
};


struct Trait92{

  int(*trait92)(Class*);
};

Vec* Trait92_v = newVec();

int Trait92_S92child1_trait92(Class* self_) {
  S92child1* self = ((S92child1*)self_);
{
    return (self -> a);
  }
} 
Trait92* newTrait92_S92child1() {
  Trait92 (* impl) = (new Trait92());
  setVec(Trait92_v, S92child1_classId, ((void*)impl));
  ((impl -> trait92) = (& Trait92_S92child1_trait92));
  return impl;
} 
Trait92* Trait92_S92child1_ = newTrait92_S92child1();

int Trait92_S92child2_trait92(Class* self_) {
  S92child2* self = ((S92child2*)self_);
{
    return (self -> a);
  }
} 
Trait92* newTrait92_S92child2() {
  Trait92 (* impl) = (new Trait92());
  setVec(Trait92_v, S92child2_classId, ((void*)impl));
  ((impl -> trait92) = (& Trait92_S92child2_trait92));
  return impl;
} 
Trait92* Trait92_S92child2_ = newTrait92_S92child2();

int Trait92_S92child3_trait92(Class* self_) {
  S92child3* self = ((S92child3*)self_);
{
    return (self -> a);
  }
} 
Trait92* newTrait92_S92child3() {
  Trait92 (* impl) = (new Trait92());
  setVec(Trait92_v, S92child3_classId, ((void*)impl));
  ((impl -> trait92) = (& Trait92_S92child3_trait92));
  return impl;
} 
Trait92* Trait92_S92child3_ = newTrait92_S92child3();

int Trait10_S92child1_trait10(Class* self_) {
  S92child1* self = ((S92child1*)self_);
{
    return (self -> a);
  }
} 
Trait10* newTrait10_S92child1() {
  Trait10 (* impl) = (new Trait10());
  setVec(Trait10_v, S92child1_classId, ((void*)impl));
  ((impl -> trait10) = (& Trait10_S92child1_trait10));
  return impl;
} 
Trait10* Trait10_S92child1_ = newTrait10_S92child1();

int S93_classId = Class_genId();
struct S93{

  int id;
  S93 ():id(S93_classId){

  }
};


int S93child1_classId = Class_genId();
struct S93child1:S93{

  S93child1 (int a):a(a){
    (id = S93child1_classId);
{

    }
  }
  int a;
};


struct Trait93{

  int(*trait93)(Class*);
};

Vec* Trait93_v = newVec();

int Trait93_S93child1_trait93(Class* self_) {
  S93child1* self = ((S93child1*)self_);
{
    return (self -> a);
  }
} 
Trait93* newTrait93_S93child1() {
  Trait93 (* impl) = (new Trait93());
  setVec(Trait93_v, S93child1_classId, ((void*)impl));
  ((impl -> trait93) = (& Trait93_S93child1_trait93));
  return impl;
} 
Trait93* Trait93_S93child1_ = newTrait93_S93child1();

int Trait2_S93child1_trait2(Class* self_) {
  S93child1* self = ((S93child1*)self_);
{
    return (self -> a);
  }
} 
Trait2* newTrait2_S93child1() {
  Trait2 (* impl) = (new Trait2());
  setVec(Trait2_v, S93child1_classId, ((void*)impl));
  ((impl -> trait2) = (& Trait2_S93child1_trait2));
  return impl;
} 
Trait2* Trait2_S93child1_ = newTrait2_S93child1();

int S94_classId = Class_genId();
struct S94{

  int id;
  S94 ():id(S94_classId){

  }
};


int S94child1_classId = Class_genId();
struct S94child1:S94{

  S94child1 (int a):a(a){
    (id = S94child1_classId);
{

    }
  }
  int a;
};


int S94child2_classId = Class_genId();
struct S94child2:S94{

  S94child2 (int a):a(a){
    (id = S94child2_classId);
{

    }
  }
  int a;
};


struct Trait94{

  int(*trait94)(Class*);
};

Vec* Trait94_v = newVec();

int Trait94_S94child1_trait94(Class* self_) {
  S94child1* self = ((S94child1*)self_);
{
    return (self -> a);
  }
} 
Trait94* newTrait94_S94child1() {
  Trait94 (* impl) = (new Trait94());
  setVec(Trait94_v, S94child1_classId, ((void*)impl));
  ((impl -> trait94) = (& Trait94_S94child1_trait94));
  return impl;
} 
Trait94* Trait94_S94child1_ = newTrait94_S94child1();

int Trait3_S94child1_trait3(Class* self_) {
  S94child1* self = ((S94child1*)self_);
{
    return (self -> a);
  }
} 
Trait3* newTrait3_S94child1() {
  Trait3 (* impl) = (new Trait3());
  setVec(Trait3_v, S94child1_classId, ((void*)impl));
  ((impl -> trait3) = (& Trait3_S94child1_trait3));
  return impl;
} 
Trait3* Trait3_S94child1_ = newTrait3_S94child1();

int S95_classId = Class_genId();
struct S95{

  int id;
  S95 ():id(S95_classId){

  }
};


int S95child1_classId = Class_genId();
struct S95child1:S95{

  S95child1 (int a):a(a){
    (id = S95child1_classId);
{

    }
  }
  int a;
};


int S95child2_classId = Class_genId();
struct S95child2:S95{

  S95child2 (int a):a(a){
    (id = S95child2_classId);
{

    }
  }
  int a;
};


int S95child3_classId = Class_genId();
struct S95child3:S95{

  S95child3 (int a):a(a){
    (id = S95child3_classId);
{

    }
  }
  int a;
};


struct Trait95{

  int(*trait95)(Class*);
};

Vec* Trait95_v = newVec();

int Trait95_S95child1_trait95(Class* self_) {
  S95child1* self = ((S95child1*)self_);
{
    return (self -> a);
  }
} 
Trait95* newTrait95_S95child1() {
  Trait95 (* impl) = (new Trait95());
  setVec(Trait95_v, S95child1_classId, ((void*)impl));
  ((impl -> trait95) = (& Trait95_S95child1_trait95));
  return impl;
} 
Trait95* Trait95_S95child1_ = newTrait95_S95child1();

int Trait95_S95child2_trait95(Class* self_) {
  S95child2* self = ((S95child2*)self_);
{
    return (self -> a);
  }
} 
Trait95* newTrait95_S95child2() {
  Trait95 (* impl) = (new Trait95());
  setVec(Trait95_v, S95child2_classId, ((void*)impl));
  ((impl -> trait95) = (& Trait95_S95child2_trait95));
  return impl;
} 
Trait95* Trait95_S95child2_ = newTrait95_S95child2();

int Trait18_S95child1_trait18(Class* self_) {
  S95child1* self = ((S95child1*)self_);
{
    return (self -> a);
  }
} 
Trait18* newTrait18_S95child1() {
  Trait18 (* impl) = (new Trait18());
  setVec(Trait18_v, S95child1_classId, ((void*)impl));
  ((impl -> trait18) = (& Trait18_S95child1_trait18));
  return impl;
} 
Trait18* Trait18_S95child1_ = newTrait18_S95child1();

int S96_classId = Class_genId();
struct S96{

  int id;
  S96 ():id(S96_classId){

  }
};


int S96child1_classId = Class_genId();
struct S96child1:S96{

  S96child1 (int a):a(a){
    (id = S96child1_classId);
{

    }
  }
  int a;
};


int S96child2_classId = Class_genId();
struct S96child2:S96{

  S96child2 (int a):a(a){
    (id = S96child2_classId);
{

    }
  }
  int a;
};


int S96child3_classId = Class_genId();
struct S96child3:S96{

  S96child3 (int a):a(a){
    (id = S96child3_classId);
{

    }
  }
  int a;
};


struct Trait96{

  int(*trait96)(Class*);
};

Vec* Trait96_v = newVec();

int Trait96_S96child1_trait96(Class* self_) {
  S96child1* self = ((S96child1*)self_);
{
    return (self -> a);
  }
} 
Trait96* newTrait96_S96child1() {
  Trait96 (* impl) = (new Trait96());
  setVec(Trait96_v, S96child1_classId, ((void*)impl));
  ((impl -> trait96) = (& Trait96_S96child1_trait96));
  return impl;
} 
Trait96* Trait96_S96child1_ = newTrait96_S96child1();

int Trait96_S96child2_trait96(Class* self_) {
  S96child2* self = ((S96child2*)self_);
{
    return (self -> a);
  }
} 
Trait96* newTrait96_S96child2() {
  Trait96 (* impl) = (new Trait96());
  setVec(Trait96_v, S96child2_classId, ((void*)impl));
  ((impl -> trait96) = (& Trait96_S96child2_trait96));
  return impl;
} 
Trait96* Trait96_S96child2_ = newTrait96_S96child2();

int Trait25_S96child1_trait25(Class* self_) {
  S96child1* self = ((S96child1*)self_);
{
    return (self -> a);
  }
} 
Trait25* newTrait25_S96child1() {
  Trait25 (* impl) = (new Trait25());
  setVec(Trait25_v, S96child1_classId, ((void*)impl));
  ((impl -> trait25) = (& Trait25_S96child1_trait25));
  return impl;
} 
Trait25* Trait25_S96child1_ = newTrait25_S96child1();

int S97_classId = Class_genId();
struct S97{

  int id;
  S97 ():id(S97_classId){

  }
};


int S97child1_classId = Class_genId();
struct S97child1:S97{

  S97child1 (int a):a(a){
    (id = S97child1_classId);
{

    }
  }
  int a;
};


int S97child2_classId = Class_genId();
struct S97child2:S97{

  S97child2 (int a):a(a){
    (id = S97child2_classId);
{

    }
  }
  int a;
};


struct Trait97{

  int(*trait97)(Class*);
};

Vec* Trait97_v = newVec();

int Trait97_S97child1_trait97(Class* self_) {
  S97child1* self = ((S97child1*)self_);
{
    return (self -> a);
  }
} 
Trait97* newTrait97_S97child1() {
  Trait97 (* impl) = (new Trait97());
  setVec(Trait97_v, S97child1_classId, ((void*)impl));
  ((impl -> trait97) = (& Trait97_S97child1_trait97));
  return impl;
} 
Trait97* Trait97_S97child1_ = newTrait97_S97child1();

int Trait97_S97child2_trait97(Class* self_) {
  S97child2* self = ((S97child2*)self_);
{
    return (self -> a);
  }
} 
Trait97* newTrait97_S97child2() {
  Trait97 (* impl) = (new Trait97());
  setVec(Trait97_v, S97child2_classId, ((void*)impl));
  ((impl -> trait97) = (& Trait97_S97child2_trait97));
  return impl;
} 
Trait97* Trait97_S97child2_ = newTrait97_S97child2();

int Trait63_S97child1_trait63(Class* self_) {
  S97child1* self = ((S97child1*)self_);
{
    return (self -> a);
  }
} 
Trait63* newTrait63_S97child1() {
  Trait63 (* impl) = (new Trait63());
  setVec(Trait63_v, S97child1_classId, ((void*)impl));
  ((impl -> trait63) = (& Trait63_S97child1_trait63));
  return impl;
} 
Trait63* Trait63_S97child1_ = newTrait63_S97child1();

int S98_classId = Class_genId();
struct S98{

  int id;
  S98 ():id(S98_classId){

  }
};


int S98child1_classId = Class_genId();
struct S98child1:S98{

  S98child1 (int a):a(a){
    (id = S98child1_classId);
{

    }
  }
  int a;
};


int S98child2_classId = Class_genId();
struct S98child2:S98{

  S98child2 (int a):a(a){
    (id = S98child2_classId);
{

    }
  }
  int a;
};


struct Trait98{

  int(*trait98)(Class*);
};

Vec* Trait98_v = newVec();

int Trait98_S98child1_trait98(Class* self_) {
  S98child1* self = ((S98child1*)self_);
{
    return (self -> a);
  }
} 
Trait98* newTrait98_S98child1() {
  Trait98 (* impl) = (new Trait98());
  setVec(Trait98_v, S98child1_classId, ((void*)impl));
  ((impl -> trait98) = (& Trait98_S98child1_trait98));
  return impl;
} 
Trait98* Trait98_S98child1_ = newTrait98_S98child1();

int Trait98_S98child2_trait98(Class* self_) {
  S98child2* self = ((S98child2*)self_);
{
    return (self -> a);
  }
} 
Trait98* newTrait98_S98child2() {
  Trait98 (* impl) = (new Trait98());
  setVec(Trait98_v, S98child2_classId, ((void*)impl));
  ((impl -> trait98) = (& Trait98_S98child2_trait98));
  return impl;
} 
Trait98* Trait98_S98child2_ = newTrait98_S98child2();

int Trait61_S98child1_trait61(Class* self_) {
  S98child1* self = ((S98child1*)self_);
{
    return (self -> a);
  }
} 
Trait61* newTrait61_S98child1() {
  Trait61 (* impl) = (new Trait61());
  setVec(Trait61_v, S98child1_classId, ((void*)impl));
  ((impl -> trait61) = (& Trait61_S98child1_trait61));
  return impl;
} 
Trait61* Trait61_S98child1_ = newTrait61_S98child1();

int S99_classId = Class_genId();
struct S99{

  int id;
  S99 ():id(S99_classId){

  }
};


int S99child1_classId = Class_genId();
struct S99child1:S99{

  S99child1 (int a):a(a){
    (id = S99child1_classId);
{

    }
  }
  int a;
};


int S99child2_classId = Class_genId();
struct S99child2:S99{

  S99child2 (int a):a(a){
    (id = S99child2_classId);
{

    }
  }
  int a;
};


struct Trait99{

  int(*trait99)(Class*);
};

Vec* Trait99_v = newVec();

int Trait99_S99child1_trait99(Class* self_) {
  S99child1* self = ((S99child1*)self_);
{
    return (self -> a);
  }
} 
Trait99* newTrait99_S99child1() {
  Trait99 (* impl) = (new Trait99());
  setVec(Trait99_v, S99child1_classId, ((void*)impl));
  ((impl -> trait99) = (& Trait99_S99child1_trait99));
  return impl;
} 
Trait99* Trait99_S99child1_ = newTrait99_S99child1();

int Trait22_S99child1_trait22(Class* self_) {
  S99child1* self = ((S99child1*)self_);
{
    return (self -> a);
  }
} 
Trait22* newTrait22_S99child1() {
  Trait22 (* impl) = (new Trait22());
  setVec(Trait22_v, S99child1_classId, ((void*)impl));
  ((impl -> trait22) = (& Trait22_S99child1_trait22));
  return impl;
} 
Trait22* Trait22_S99child1_ = newTrait22_S99child1();

int S100_classId = Class_genId();
struct S100{

  int id;
  S100 ():id(S100_classId){

  }
};


int S100child1_classId = Class_genId();
struct S100child1:S100{

  S100child1 (int a):a(a){
    (id = S100child1_classId);
{

    }
  }
  int a;
};


int S100child2_classId = Class_genId();
struct S100child2:S100{

  S100child2 (int a):a(a){
    (id = S100child2_classId);
{

    }
  }
  int a;
};


int S100child3_classId = Class_genId();
struct S100child3:S100{

  S100child3 (int a):a(a){
    (id = S100child3_classId);
{

    }
  }
  int a;
};


int S100child4_classId = Class_genId();
struct S100child4:S100{

  S100child4 (int a):a(a){
    (id = S100child4_classId);
{

    }
  }
  int a;
};


struct Trait100{

  int(*trait100)(Class*);
};

Vec* Trait100_v = newVec();

int Trait100_S100child1_trait100(Class* self_) {
  S100child1* self = ((S100child1*)self_);
{
    return (self -> a);
  }
} 
Trait100* newTrait100_S100child1() {
  Trait100 (* impl) = (new Trait100());
  setVec(Trait100_v, S100child1_classId, ((void*)impl));
  ((impl -> trait100) = (& Trait100_S100child1_trait100));
  return impl;
} 
Trait100* Trait100_S100child1_ = newTrait100_S100child1();

int Trait100_S100child2_trait100(Class* self_) {
  S100child2* self = ((S100child2*)self_);
{
    return (self -> a);
  }
} 
Trait100* newTrait100_S100child2() {
  Trait100 (* impl) = (new Trait100());
  setVec(Trait100_v, S100child2_classId, ((void*)impl));
  ((impl -> trait100) = (& Trait100_S100child2_trait100));
  return impl;
} 
Trait100* Trait100_S100child2_ = newTrait100_S100child2();

int Trait76_S100child1_trait76(Class* self_) {
  S100child1* self = ((S100child1*)self_);
{
    return (self -> a);
  }
} 
Trait76* newTrait76_S100child1() {
  Trait76 (* impl) = (new Trait76());
  setVec(Trait76_v, S100child1_classId, ((void*)impl));
  ((impl -> trait76) = (& Trait76_S100child1_trait76));
  return impl;
} 
Trait76* Trait76_S100child1_ = newTrait76_S100child1();

int S101_classId = Class_genId();
struct S101{

  int id;
  S101 ():id(S101_classId){

  }
};


int S101child1_classId = Class_genId();
struct S101child1:S101{

  S101child1 (int a):a(a){
    (id = S101child1_classId);
{

    }
  }
  int a;
};


int S101child2_classId = Class_genId();
struct S101child2:S101{

  S101child2 (int a):a(a){
    (id = S101child2_classId);
{

    }
  }
  int a;
};


int S101child3_classId = Class_genId();
struct S101child3:S101{

  S101child3 (int a):a(a){
    (id = S101child3_classId);
{

    }
  }
  int a;
};


int S101child4_classId = Class_genId();
struct S101child4:S101{

  S101child4 (int a):a(a){
    (id = S101child4_classId);
{

    }
  }
  int a;
};


struct Trait101{

  int(*trait101)(Class*);
};

Vec* Trait101_v = newVec();

int Trait101_S101child1_trait101(Class* self_) {
  S101child1* self = ((S101child1*)self_);
{
    return (self -> a);
  }
} 
Trait101* newTrait101_S101child1() {
  Trait101 (* impl) = (new Trait101());
  setVec(Trait101_v, S101child1_classId, ((void*)impl));
  ((impl -> trait101) = (& Trait101_S101child1_trait101));
  return impl;
} 
Trait101* Trait101_S101child1_ = newTrait101_S101child1();

int Trait101_S101child2_trait101(Class* self_) {
  S101child2* self = ((S101child2*)self_);
{
    return (self -> a);
  }
} 
Trait101* newTrait101_S101child2() {
  Trait101 (* impl) = (new Trait101());
  setVec(Trait101_v, S101child2_classId, ((void*)impl));
  ((impl -> trait101) = (& Trait101_S101child2_trait101));
  return impl;
} 
Trait101* Trait101_S101child2_ = newTrait101_S101child2();

int Trait91_S101child1_trait91(Class* self_) {
  S101child1* self = ((S101child1*)self_);
{
    return (self -> a);
  }
} 
Trait91* newTrait91_S101child1() {
  Trait91 (* impl) = (new Trait91());
  setVec(Trait91_v, S101child1_classId, ((void*)impl));
  ((impl -> trait91) = (& Trait91_S101child1_trait91));
  return impl;
} 
Trait91* Trait91_S101child1_ = newTrait91_S101child1();

int S102_classId = Class_genId();
struct S102{

  int id;
  S102 ():id(S102_classId){

  }
};


int S102child1_classId = Class_genId();
struct S102child1:S102{

  S102child1 (int a):a(a){
    (id = S102child1_classId);
{

    }
  }
  int a;
};


int S102child2_classId = Class_genId();
struct S102child2:S102{

  S102child2 (int a):a(a){
    (id = S102child2_classId);
{

    }
  }
  int a;
};


struct Trait102{

  int(*trait102)(Class*);
};

Vec* Trait102_v = newVec();

int Trait102_S102child1_trait102(Class* self_) {
  S102child1* self = ((S102child1*)self_);
{
    return (self -> a);
  }
} 
Trait102* newTrait102_S102child1() {
  Trait102 (* impl) = (new Trait102());
  setVec(Trait102_v, S102child1_classId, ((void*)impl));
  ((impl -> trait102) = (& Trait102_S102child1_trait102));
  return impl;
} 
Trait102* Trait102_S102child1_ = newTrait102_S102child1();

int Trait46_S102child1_trait46(Class* self_) {
  S102child1* self = ((S102child1*)self_);
{
    return (self -> a);
  }
} 
Trait46* newTrait46_S102child1() {
  Trait46 (* impl) = (new Trait46());
  setVec(Trait46_v, S102child1_classId, ((void*)impl));
  ((impl -> trait46) = (& Trait46_S102child1_trait46));
  return impl;
} 
Trait46* Trait46_S102child1_ = newTrait46_S102child1();

int S103_classId = Class_genId();
struct S103{

  int id;
  S103 ():id(S103_classId){

  }
};


int S103child1_classId = Class_genId();
struct S103child1:S103{

  S103child1 (int a):a(a){
    (id = S103child1_classId);
{

    }
  }
  int a;
};


struct Trait103{

  int(*trait103)(Class*);
};

Vec* Trait103_v = newVec();

int Trait103_S103child1_trait103(Class* self_) {
  S103child1* self = ((S103child1*)self_);
{
    return (self -> a);
  }
} 
Trait103* newTrait103_S103child1() {
  Trait103 (* impl) = (new Trait103());
  setVec(Trait103_v, S103child1_classId, ((void*)impl));
  ((impl -> trait103) = (& Trait103_S103child1_trait103));
  return impl;
} 
Trait103* Trait103_S103child1_ = newTrait103_S103child1();

int Trait73_S103child1_trait73(Class* self_) {
  S103child1* self = ((S103child1*)self_);
{
    return (self -> a);
  }
} 
Trait73* newTrait73_S103child1() {
  Trait73 (* impl) = (new Trait73());
  setVec(Trait73_v, S103child1_classId, ((void*)impl));
  ((impl -> trait73) = (& Trait73_S103child1_trait73));
  return impl;
} 
Trait73* Trait73_S103child1_ = newTrait73_S103child1();

int S104_classId = Class_genId();
struct S104{

  int id;
  S104 ():id(S104_classId){

  }
};


int S104child1_classId = Class_genId();
struct S104child1:S104{

  S104child1 (int a):a(a){
    (id = S104child1_classId);
{

    }
  }
  int a;
};


int S104child2_classId = Class_genId();
struct S104child2:S104{

  S104child2 (int a):a(a){
    (id = S104child2_classId);
{

    }
  }
  int a;
};


struct Trait104{

  int(*trait104)(Class*);
};

Vec* Trait104_v = newVec();

int Trait104_S104child1_trait104(Class* self_) {
  S104child1* self = ((S104child1*)self_);
{
    return (self -> a);
  }
} 
Trait104* newTrait104_S104child1() {
  Trait104 (* impl) = (new Trait104());
  setVec(Trait104_v, S104child1_classId, ((void*)impl));
  ((impl -> trait104) = (& Trait104_S104child1_trait104));
  return impl;
} 
Trait104* Trait104_S104child1_ = newTrait104_S104child1();

int Trait104_S104child2_trait104(Class* self_) {
  S104child2* self = ((S104child2*)self_);
{
    return (self -> a);
  }
} 
Trait104* newTrait104_S104child2() {
  Trait104 (* impl) = (new Trait104());
  setVec(Trait104_v, S104child2_classId, ((void*)impl));
  ((impl -> trait104) = (& Trait104_S104child2_trait104));
  return impl;
} 
Trait104* Trait104_S104child2_ = newTrait104_S104child2();

int Trait44_S104child1_trait44(Class* self_) {
  S104child1* self = ((S104child1*)self_);
{
    return (self -> a);
  }
} 
Trait44* newTrait44_S104child1() {
  Trait44 (* impl) = (new Trait44());
  setVec(Trait44_v, S104child1_classId, ((void*)impl));
  ((impl -> trait44) = (& Trait44_S104child1_trait44));
  return impl;
} 
Trait44* Trait44_S104child1_ = newTrait44_S104child1();

int S105_classId = Class_genId();
struct S105{

  int id;
  S105 ():id(S105_classId){

  }
};


int S105child1_classId = Class_genId();
struct S105child1:S105{

  S105child1 (int a):a(a){
    (id = S105child1_classId);
{

    }
  }
  int a;
};


int S105child2_classId = Class_genId();
struct S105child2:S105{

  S105child2 (int a):a(a){
    (id = S105child2_classId);
{

    }
  }
  int a;
};


int S105child3_classId = Class_genId();
struct S105child3:S105{

  S105child3 (int a):a(a){
    (id = S105child3_classId);
{

    }
  }
  int a;
};


struct Trait105{

  int(*trait105)(Class*);
};

Vec* Trait105_v = newVec();

int Trait105_S105child1_trait105(Class* self_) {
  S105child1* self = ((S105child1*)self_);
{
    return (self -> a);
  }
} 
Trait105* newTrait105_S105child1() {
  Trait105 (* impl) = (new Trait105());
  setVec(Trait105_v, S105child1_classId, ((void*)impl));
  ((impl -> trait105) = (& Trait105_S105child1_trait105));
  return impl;
} 
Trait105* Trait105_S105child1_ = newTrait105_S105child1();

int Trait105_S105child2_trait105(Class* self_) {
  S105child2* self = ((S105child2*)self_);
{
    return (self -> a);
  }
} 
Trait105* newTrait105_S105child2() {
  Trait105 (* impl) = (new Trait105());
  setVec(Trait105_v, S105child2_classId, ((void*)impl));
  ((impl -> trait105) = (& Trait105_S105child2_trait105));
  return impl;
} 
Trait105* Trait105_S105child2_ = newTrait105_S105child2();

int Trait105_S105child3_trait105(Class* self_) {
  S105child3* self = ((S105child3*)self_);
{
    return (self -> a);
  }
} 
Trait105* newTrait105_S105child3() {
  Trait105 (* impl) = (new Trait105());
  setVec(Trait105_v, S105child3_classId, ((void*)impl));
  ((impl -> trait105) = (& Trait105_S105child3_trait105));
  return impl;
} 
Trait105* Trait105_S105child3_ = newTrait105_S105child3();

int Trait70_S105child1_trait70(Class* self_) {
  S105child1* self = ((S105child1*)self_);
{
    return (self -> a);
  }
} 
Trait70* newTrait70_S105child1() {
  Trait70 (* impl) = (new Trait70());
  setVec(Trait70_v, S105child1_classId, ((void*)impl));
  ((impl -> trait70) = (& Trait70_S105child1_trait70));
  return impl;
} 
Trait70* Trait70_S105child1_ = newTrait70_S105child1();

int S106_classId = Class_genId();
struct S106{

  int id;
  S106 ():id(S106_classId){

  }
};


int S106child1_classId = Class_genId();
struct S106child1:S106{

  S106child1 (int a):a(a){
    (id = S106child1_classId);
{

    }
  }
  int a;
};


int S106child2_classId = Class_genId();
struct S106child2:S106{

  S106child2 (int a):a(a){
    (id = S106child2_classId);
{

    }
  }
  int a;
};


int S106child3_classId = Class_genId();
struct S106child3:S106{

  S106child3 (int a):a(a){
    (id = S106child3_classId);
{

    }
  }
  int a;
};


int S106child4_classId = Class_genId();
struct S106child4:S106{

  S106child4 (int a):a(a){
    (id = S106child4_classId);
{

    }
  }
  int a;
};


struct Trait106{

  int(*trait106)(Class*);
};

Vec* Trait106_v = newVec();

int Trait106_S106child1_trait106(Class* self_) {
  S106child1* self = ((S106child1*)self_);
{
    return (self -> a);
  }
} 
Trait106* newTrait106_S106child1() {
  Trait106 (* impl) = (new Trait106());
  setVec(Trait106_v, S106child1_classId, ((void*)impl));
  ((impl -> trait106) = (& Trait106_S106child1_trait106));
  return impl;
} 
Trait106* Trait106_S106child1_ = newTrait106_S106child1();

int Trait106_S106child2_trait106(Class* self_) {
  S106child2* self = ((S106child2*)self_);
{
    return (self -> a);
  }
} 
Trait106* newTrait106_S106child2() {
  Trait106 (* impl) = (new Trait106());
  setVec(Trait106_v, S106child2_classId, ((void*)impl));
  ((impl -> trait106) = (& Trait106_S106child2_trait106));
  return impl;
} 
Trait106* Trait106_S106child2_ = newTrait106_S106child2();

int Trait42_S106child1_trait42(Class* self_) {
  S106child1* self = ((S106child1*)self_);
{
    return (self -> a);
  }
} 
Trait42* newTrait42_S106child1() {
  Trait42 (* impl) = (new Trait42());
  setVec(Trait42_v, S106child1_classId, ((void*)impl));
  ((impl -> trait42) = (& Trait42_S106child1_trait42));
  return impl;
} 
Trait42* Trait42_S106child1_ = newTrait42_S106child1();

int S107_classId = Class_genId();
struct S107{

  int id;
  S107 ():id(S107_classId){

  }
};


int S107child1_classId = Class_genId();
struct S107child1:S107{

  S107child1 (int a):a(a){
    (id = S107child1_classId);
{

    }
  }
  int a;
};


struct Trait107{

  int(*trait107)(Class*);
};

Vec* Trait107_v = newVec();

int Trait107_S107child1_trait107(Class* self_) {
  S107child1* self = ((S107child1*)self_);
{
    return (self -> a);
  }
} 
Trait107* newTrait107_S107child1() {
  Trait107 (* impl) = (new Trait107());
  setVec(Trait107_v, S107child1_classId, ((void*)impl));
  ((impl -> trait107) = (& Trait107_S107child1_trait107));
  return impl;
} 
Trait107* Trait107_S107child1_ = newTrait107_S107child1();

int Trait26_S107child1_trait26(Class* self_) {
  S107child1* self = ((S107child1*)self_);
{
    return (self -> a);
  }
} 
Trait26* newTrait26_S107child1() {
  Trait26 (* impl) = (new Trait26());
  setVec(Trait26_v, S107child1_classId, ((void*)impl));
  ((impl -> trait26) = (& Trait26_S107child1_trait26));
  return impl;
} 
Trait26* Trait26_S107child1_ = newTrait26_S107child1();

int S108_classId = Class_genId();
struct S108{

  int id;
  S108 ():id(S108_classId){

  }
};


int S108child1_classId = Class_genId();
struct S108child1:S108{

  S108child1 (int a):a(a){
    (id = S108child1_classId);
{

    }
  }
  int a;
};


int S108child2_classId = Class_genId();
struct S108child2:S108{

  S108child2 (int a):a(a){
    (id = S108child2_classId);
{

    }
  }
  int a;
};


int S108child3_classId = Class_genId();
struct S108child3:S108{

  S108child3 (int a):a(a){
    (id = S108child3_classId);
{

    }
  }
  int a;
};


struct Trait108{

  int(*trait108)(Class*);
};

Vec* Trait108_v = newVec();

int Trait108_S108child1_trait108(Class* self_) {
  S108child1* self = ((S108child1*)self_);
{
    return (self -> a);
  }
} 
Trait108* newTrait108_S108child1() {
  Trait108 (* impl) = (new Trait108());
  setVec(Trait108_v, S108child1_classId, ((void*)impl));
  ((impl -> trait108) = (& Trait108_S108child1_trait108));
  return impl;
} 
Trait108* Trait108_S108child1_ = newTrait108_S108child1();

int Trait11_S108child1_trait11(Class* self_) {
  S108child1* self = ((S108child1*)self_);
{
    return (self -> a);
  }
} 
Trait11* newTrait11_S108child1() {
  Trait11 (* impl) = (new Trait11());
  setVec(Trait11_v, S108child1_classId, ((void*)impl));
  ((impl -> trait11) = (& Trait11_S108child1_trait11));
  return impl;
} 
Trait11* Trait11_S108child1_ = newTrait11_S108child1();

int S109_classId = Class_genId();
struct S109{

  int id;
  S109 ():id(S109_classId){

  }
};


int S109child1_classId = Class_genId();
struct S109child1:S109{

  S109child1 (int a):a(a){
    (id = S109child1_classId);
{

    }
  }
  int a;
};


int S109child2_classId = Class_genId();
struct S109child2:S109{

  S109child2 (int a):a(a){
    (id = S109child2_classId);
{

    }
  }
  int a;
};


int S109child3_classId = Class_genId();
struct S109child3:S109{

  S109child3 (int a):a(a){
    (id = S109child3_classId);
{

    }
  }
  int a;
};


int S109child4_classId = Class_genId();
struct S109child4:S109{

  S109child4 (int a):a(a){
    (id = S109child4_classId);
{

    }
  }
  int a;
};


struct Trait109{

  int(*trait109)(Class*);
};

Vec* Trait109_v = newVec();

int Trait109_S109child1_trait109(Class* self_) {
  S109child1* self = ((S109child1*)self_);
{
    return (self -> a);
  }
} 
Trait109* newTrait109_S109child1() {
  Trait109 (* impl) = (new Trait109());
  setVec(Trait109_v, S109child1_classId, ((void*)impl));
  ((impl -> trait109) = (& Trait109_S109child1_trait109));
  return impl;
} 
Trait109* Trait109_S109child1_ = newTrait109_S109child1();

int Trait109_S109child2_trait109(Class* self_) {
  S109child2* self = ((S109child2*)self_);
{
    return (self -> a);
  }
} 
Trait109* newTrait109_S109child2() {
  Trait109 (* impl) = (new Trait109());
  setVec(Trait109_v, S109child2_classId, ((void*)impl));
  ((impl -> trait109) = (& Trait109_S109child2_trait109));
  return impl;
} 
Trait109* Trait109_S109child2_ = newTrait109_S109child2();

int Trait109_S109child3_trait109(Class* self_) {
  S109child3* self = ((S109child3*)self_);
{
    return (self -> a);
  }
} 
Trait109* newTrait109_S109child3() {
  Trait109 (* impl) = (new Trait109());
  setVec(Trait109_v, S109child3_classId, ((void*)impl));
  ((impl -> trait109) = (& Trait109_S109child3_trait109));
  return impl;
} 
Trait109* Trait109_S109child3_ = newTrait109_S109child3();

int Trait61_S109child1_trait61(Class* self_) {
  S109child1* self = ((S109child1*)self_);
{
    return (self -> a);
  }
} 
Trait61* newTrait61_S109child1() {
  Trait61 (* impl) = (new Trait61());
  setVec(Trait61_v, S109child1_classId, ((void*)impl));
  ((impl -> trait61) = (& Trait61_S109child1_trait61));
  return impl;
} 
Trait61* Trait61_S109child1_ = newTrait61_S109child1();

int S110_classId = Class_genId();
struct S110{

  int id;
  S110 ():id(S110_classId){

  }
};


int S110child1_classId = Class_genId();
struct S110child1:S110{

  S110child1 (int a):a(a){
    (id = S110child1_classId);
{

    }
  }
  int a;
};


int S110child2_classId = Class_genId();
struct S110child2:S110{

  S110child2 (int a):a(a){
    (id = S110child2_classId);
{

    }
  }
  int a;
};


struct Trait110{

  int(*trait110)(Class*);
};

Vec* Trait110_v = newVec();

int Trait110_S110child1_trait110(Class* self_) {
  S110child1* self = ((S110child1*)self_);
{
    return (self -> a);
  }
} 
Trait110* newTrait110_S110child1() {
  Trait110 (* impl) = (new Trait110());
  setVec(Trait110_v, S110child1_classId, ((void*)impl));
  ((impl -> trait110) = (& Trait110_S110child1_trait110));
  return impl;
} 
Trait110* Trait110_S110child1_ = newTrait110_S110child1();

int Trait110_S110child2_trait110(Class* self_) {
  S110child2* self = ((S110child2*)self_);
{
    return (self -> a);
  }
} 
Trait110* newTrait110_S110child2() {
  Trait110 (* impl) = (new Trait110());
  setVec(Trait110_v, S110child2_classId, ((void*)impl));
  ((impl -> trait110) = (& Trait110_S110child2_trait110));
  return impl;
} 
Trait110* Trait110_S110child2_ = newTrait110_S110child2();

int Trait75_S110child1_trait75(Class* self_) {
  S110child1* self = ((S110child1*)self_);
{
    return (self -> a);
  }
} 
Trait75* newTrait75_S110child1() {
  Trait75 (* impl) = (new Trait75());
  setVec(Trait75_v, S110child1_classId, ((void*)impl));
  ((impl -> trait75) = (& Trait75_S110child1_trait75));
  return impl;
} 
Trait75* Trait75_S110child1_ = newTrait75_S110child1();

int S111_classId = Class_genId();
struct S111{

  int id;
  S111 ():id(S111_classId){

  }
};


int S111child1_classId = Class_genId();
struct S111child1:S111{

  S111child1 (int a):a(a){
    (id = S111child1_classId);
{

    }
  }
  int a;
};


int S111child2_classId = Class_genId();
struct S111child2:S111{

  S111child2 (int a):a(a){
    (id = S111child2_classId);
{

    }
  }
  int a;
};


struct Trait111{

  int(*trait111)(Class*);
};

Vec* Trait111_v = newVec();

int Trait111_S111child1_trait111(Class* self_) {
  S111child1* self = ((S111child1*)self_);
{
    return (self -> a);
  }
} 
Trait111* newTrait111_S111child1() {
  Trait111 (* impl) = (new Trait111());
  setVec(Trait111_v, S111child1_classId, ((void*)impl));
  ((impl -> trait111) = (& Trait111_S111child1_trait111));
  return impl;
} 
Trait111* Trait111_S111child1_ = newTrait111_S111child1();

int Trait82_S111child1_trait82(Class* self_) {
  S111child1* self = ((S111child1*)self_);
{
    return (self -> a);
  }
} 
Trait82* newTrait82_S111child1() {
  Trait82 (* impl) = (new Trait82());
  setVec(Trait82_v, S111child1_classId, ((void*)impl));
  ((impl -> trait82) = (& Trait82_S111child1_trait82));
  return impl;
} 
Trait82* Trait82_S111child1_ = newTrait82_S111child1();

int S112_classId = Class_genId();
struct S112{

  int id;
  S112 ():id(S112_classId){

  }
};


int S112child1_classId = Class_genId();
struct S112child1:S112{

  S112child1 (int a):a(a){
    (id = S112child1_classId);
{

    }
  }
  int a;
};


int S112child2_classId = Class_genId();
struct S112child2:S112{

  S112child2 (int a):a(a){
    (id = S112child2_classId);
{

    }
  }
  int a;
};


int S112child3_classId = Class_genId();
struct S112child3:S112{

  S112child3 (int a):a(a){
    (id = S112child3_classId);
{

    }
  }
  int a;
};


int S112child4_classId = Class_genId();
struct S112child4:S112{

  S112child4 (int a):a(a){
    (id = S112child4_classId);
{

    }
  }
  int a;
};


struct Trait112{

  int(*trait112)(Class*);
};

Vec* Trait112_v = newVec();

int Trait112_S112child1_trait112(Class* self_) {
  S112child1* self = ((S112child1*)self_);
{
    return (self -> a);
  }
} 
Trait112* newTrait112_S112child1() {
  Trait112 (* impl) = (new Trait112());
  setVec(Trait112_v, S112child1_classId, ((void*)impl));
  ((impl -> trait112) = (& Trait112_S112child1_trait112));
  return impl;
} 
Trait112* Trait112_S112child1_ = newTrait112_S112child1();

int Trait86_S112child1_trait86(Class* self_) {
  S112child1* self = ((S112child1*)self_);
{
    return (self -> a);
  }
} 
Trait86* newTrait86_S112child1() {
  Trait86 (* impl) = (new Trait86());
  setVec(Trait86_v, S112child1_classId, ((void*)impl));
  ((impl -> trait86) = (& Trait86_S112child1_trait86));
  return impl;
} 
Trait86* Trait86_S112child1_ = newTrait86_S112child1();

int S113_classId = Class_genId();
struct S113{

  int id;
  S113 ():id(S113_classId){

  }
};


int S113child1_classId = Class_genId();
struct S113child1:S113{

  S113child1 (int a):a(a){
    (id = S113child1_classId);
{

    }
  }
  int a;
};


int S113child2_classId = Class_genId();
struct S113child2:S113{

  S113child2 (int a):a(a){
    (id = S113child2_classId);
{

    }
  }
  int a;
};


int S113child3_classId = Class_genId();
struct S113child3:S113{

  S113child3 (int a):a(a){
    (id = S113child3_classId);
{

    }
  }
  int a;
};


int S113child4_classId = Class_genId();
struct S113child4:S113{

  S113child4 (int a):a(a){
    (id = S113child4_classId);
{

    }
  }
  int a;
};


int S113child5_classId = Class_genId();
struct S113child5:S113{

  S113child5 (int a):a(a){
    (id = S113child5_classId);
{

    }
  }
  int a;
};


struct Trait113{

  int(*trait113)(Class*);
};

Vec* Trait113_v = newVec();

int Trait113_S113child1_trait113(Class* self_) {
  S113child1* self = ((S113child1*)self_);
{
    return (self -> a);
  }
} 
Trait113* newTrait113_S113child1() {
  Trait113 (* impl) = (new Trait113());
  setVec(Trait113_v, S113child1_classId, ((void*)impl));
  ((impl -> trait113) = (& Trait113_S113child1_trait113));
  return impl;
} 
Trait113* Trait113_S113child1_ = newTrait113_S113child1();

int Trait113_S113child2_trait113(Class* self_) {
  S113child2* self = ((S113child2*)self_);
{
    return (self -> a);
  }
} 
Trait113* newTrait113_S113child2() {
  Trait113 (* impl) = (new Trait113());
  setVec(Trait113_v, S113child2_classId, ((void*)impl));
  ((impl -> trait113) = (& Trait113_S113child2_trait113));
  return impl;
} 
Trait113* Trait113_S113child2_ = newTrait113_S113child2();

int Trait39_S113child1_trait39(Class* self_) {
  S113child1* self = ((S113child1*)self_);
{
    return (self -> a);
  }
} 
Trait39* newTrait39_S113child1() {
  Trait39 (* impl) = (new Trait39());
  setVec(Trait39_v, S113child1_classId, ((void*)impl));
  ((impl -> trait39) = (& Trait39_S113child1_trait39));
  return impl;
} 
Trait39* Trait39_S113child1_ = newTrait39_S113child1();

int S114_classId = Class_genId();
struct S114{

  int id;
  S114 ():id(S114_classId){

  }
};


int S114child1_classId = Class_genId();
struct S114child1:S114{

  S114child1 (int a):a(a){
    (id = S114child1_classId);
{

    }
  }
  int a;
};


int S114child2_classId = Class_genId();
struct S114child2:S114{

  S114child2 (int a):a(a){
    (id = S114child2_classId);
{

    }
  }
  int a;
};


int S114child3_classId = Class_genId();
struct S114child3:S114{

  S114child3 (int a):a(a){
    (id = S114child3_classId);
{

    }
  }
  int a;
};


struct Trait114{

  int(*trait114)(Class*);
};

Vec* Trait114_v = newVec();

int Trait114_S114child1_trait114(Class* self_) {
  S114child1* self = ((S114child1*)self_);
{
    return (self -> a);
  }
} 
Trait114* newTrait114_S114child1() {
  Trait114 (* impl) = (new Trait114());
  setVec(Trait114_v, S114child1_classId, ((void*)impl));
  ((impl -> trait114) = (& Trait114_S114child1_trait114));
  return impl;
} 
Trait114* Trait114_S114child1_ = newTrait114_S114child1();

int Trait110_S114child1_trait110(Class* self_) {
  S114child1* self = ((S114child1*)self_);
{
    return (self -> a);
  }
} 
Trait110* newTrait110_S114child1() {
  Trait110 (* impl) = (new Trait110());
  setVec(Trait110_v, S114child1_classId, ((void*)impl));
  ((impl -> trait110) = (& Trait110_S114child1_trait110));
  return impl;
} 
Trait110* Trait110_S114child1_ = newTrait110_S114child1();

int S115_classId = Class_genId();
struct S115{

  int id;
  S115 ():id(S115_classId){

  }
};


int S115child1_classId = Class_genId();
struct S115child1:S115{

  S115child1 (int a):a(a){
    (id = S115child1_classId);
{

    }
  }
  int a;
};


int S115child2_classId = Class_genId();
struct S115child2:S115{

  S115child2 (int a):a(a){
    (id = S115child2_classId);
{

    }
  }
  int a;
};


struct Trait115{

  int(*trait115)(Class*);
};

Vec* Trait115_v = newVec();

int Trait115_S115child1_trait115(Class* self_) {
  S115child1* self = ((S115child1*)self_);
{
    return (self -> a);
  }
} 
Trait115* newTrait115_S115child1() {
  Trait115 (* impl) = (new Trait115());
  setVec(Trait115_v, S115child1_classId, ((void*)impl));
  ((impl -> trait115) = (& Trait115_S115child1_trait115));
  return impl;
} 
Trait115* Trait115_S115child1_ = newTrait115_S115child1();

int Trait88_S115child1_trait88(Class* self_) {
  S115child1* self = ((S115child1*)self_);
{
    return (self -> a);
  }
} 
Trait88* newTrait88_S115child1() {
  Trait88 (* impl) = (new Trait88());
  setVec(Trait88_v, S115child1_classId, ((void*)impl));
  ((impl -> trait88) = (& Trait88_S115child1_trait88));
  return impl;
} 
Trait88* Trait88_S115child1_ = newTrait88_S115child1();

int S116_classId = Class_genId();
struct S116{

  int id;
  S116 ():id(S116_classId){

  }
};


int S116child1_classId = Class_genId();
struct S116child1:S116{

  S116child1 (int a):a(a){
    (id = S116child1_classId);
{

    }
  }
  int a;
};


int S116child2_classId = Class_genId();
struct S116child2:S116{

  S116child2 (int a):a(a){
    (id = S116child2_classId);
{

    }
  }
  int a;
};


int S116child3_classId = Class_genId();
struct S116child3:S116{

  S116child3 (int a):a(a){
    (id = S116child3_classId);
{

    }
  }
  int a;
};


int S116child4_classId = Class_genId();
struct S116child4:S116{

  S116child4 (int a):a(a){
    (id = S116child4_classId);
{

    }
  }
  int a;
};


struct Trait116{

  int(*trait116)(Class*);
};

Vec* Trait116_v = newVec();

int Trait116_S116child1_trait116(Class* self_) {
  S116child1* self = ((S116child1*)self_);
{
    return (self -> a);
  }
} 
Trait116* newTrait116_S116child1() {
  Trait116 (* impl) = (new Trait116());
  setVec(Trait116_v, S116child1_classId, ((void*)impl));
  ((impl -> trait116) = (& Trait116_S116child1_trait116));
  return impl;
} 
Trait116* Trait116_S116child1_ = newTrait116_S116child1();

int Trait116_S116child2_trait116(Class* self_) {
  S116child2* self = ((S116child2*)self_);
{
    return (self -> a);
  }
} 
Trait116* newTrait116_S116child2() {
  Trait116 (* impl) = (new Trait116());
  setVec(Trait116_v, S116child2_classId, ((void*)impl));
  ((impl -> trait116) = (& Trait116_S116child2_trait116));
  return impl;
} 
Trait116* Trait116_S116child2_ = newTrait116_S116child2();

int Trait116_S116child3_trait116(Class* self_) {
  S116child3* self = ((S116child3*)self_);
{
    return (self -> a);
  }
} 
Trait116* newTrait116_S116child3() {
  Trait116 (* impl) = (new Trait116());
  setVec(Trait116_v, S116child3_classId, ((void*)impl));
  ((impl -> trait116) = (& Trait116_S116child3_trait116));
  return impl;
} 
Trait116* Trait116_S116child3_ = newTrait116_S116child3();

int Trait74_S116child1_trait74(Class* self_) {
  S116child1* self = ((S116child1*)self_);
{
    return (self -> a);
  }
} 
Trait74* newTrait74_S116child1() {
  Trait74 (* impl) = (new Trait74());
  setVec(Trait74_v, S116child1_classId, ((void*)impl));
  ((impl -> trait74) = (& Trait74_S116child1_trait74));
  return impl;
} 
Trait74* Trait74_S116child1_ = newTrait74_S116child1();

int S117_classId = Class_genId();
struct S117{

  int id;
  S117 ():id(S117_classId){

  }
};


int S117child1_classId = Class_genId();
struct S117child1:S117{

  S117child1 (int a):a(a){
    (id = S117child1_classId);
{

    }
  }
  int a;
};


int S117child2_classId = Class_genId();
struct S117child2:S117{

  S117child2 (int a):a(a){
    (id = S117child2_classId);
{

    }
  }
  int a;
};


int S117child3_classId = Class_genId();
struct S117child3:S117{

  S117child3 (int a):a(a){
    (id = S117child3_classId);
{

    }
  }
  int a;
};


struct Trait117{

  int(*trait117)(Class*);
};

Vec* Trait117_v = newVec();

int Trait117_S117child1_trait117(Class* self_) {
  S117child1* self = ((S117child1*)self_);
{
    return (self -> a);
  }
} 
Trait117* newTrait117_S117child1() {
  Trait117 (* impl) = (new Trait117());
  setVec(Trait117_v, S117child1_classId, ((void*)impl));
  ((impl -> trait117) = (& Trait117_S117child1_trait117));
  return impl;
} 
Trait117* Trait117_S117child1_ = newTrait117_S117child1();

int Trait85_S117child1_trait85(Class* self_) {
  S117child1* self = ((S117child1*)self_);
{
    return (self -> a);
  }
} 
Trait85* newTrait85_S117child1() {
  Trait85 (* impl) = (new Trait85());
  setVec(Trait85_v, S117child1_classId, ((void*)impl));
  ((impl -> trait85) = (& Trait85_S117child1_trait85));
  return impl;
} 
Trait85* Trait85_S117child1_ = newTrait85_S117child1();

int S118_classId = Class_genId();
struct S118{

  int id;
  S118 ():id(S118_classId){

  }
};


int S118child1_classId = Class_genId();
struct S118child1:S118{

  S118child1 (int a):a(a){
    (id = S118child1_classId);
{

    }
  }
  int a;
};


int S118child2_classId = Class_genId();
struct S118child2:S118{

  S118child2 (int a):a(a){
    (id = S118child2_classId);
{

    }
  }
  int a;
};


int S118child3_classId = Class_genId();
struct S118child3:S118{

  S118child3 (int a):a(a){
    (id = S118child3_classId);
{

    }
  }
  int a;
};


int S118child4_classId = Class_genId();
struct S118child4:S118{

  S118child4 (int a):a(a){
    (id = S118child4_classId);
{

    }
  }
  int a;
};


struct Trait118{

  int(*trait118)(Class*);
};

Vec* Trait118_v = newVec();

int Trait118_S118child1_trait118(Class* self_) {
  S118child1* self = ((S118child1*)self_);
{
    return (self -> a);
  }
} 
Trait118* newTrait118_S118child1() {
  Trait118 (* impl) = (new Trait118());
  setVec(Trait118_v, S118child1_classId, ((void*)impl));
  ((impl -> trait118) = (& Trait118_S118child1_trait118));
  return impl;
} 
Trait118* Trait118_S118child1_ = newTrait118_S118child1();

int Trait118_S118child2_trait118(Class* self_) {
  S118child2* self = ((S118child2*)self_);
{
    return (self -> a);
  }
} 
Trait118* newTrait118_S118child2() {
  Trait118 (* impl) = (new Trait118());
  setVec(Trait118_v, S118child2_classId, ((void*)impl));
  ((impl -> trait118) = (& Trait118_S118child2_trait118));
  return impl;
} 
Trait118* Trait118_S118child2_ = newTrait118_S118child2();

int Trait85_S118child1_trait85(Class* self_) {
  S118child1* self = ((S118child1*)self_);
{
    return (self -> a);
  }
} 
Trait85* newTrait85_S118child1() {
  Trait85 (* impl) = (new Trait85());
  setVec(Trait85_v, S118child1_classId, ((void*)impl));
  ((impl -> trait85) = (& Trait85_S118child1_trait85));
  return impl;
} 
Trait85* Trait85_S118child1_ = newTrait85_S118child1();

int S119_classId = Class_genId();
struct S119{

  int id;
  S119 ():id(S119_classId){

  }
};


int S119child1_classId = Class_genId();
struct S119child1:S119{

  S119child1 (int a):a(a){
    (id = S119child1_classId);
{

    }
  }
  int a;
};


struct Trait119{

  int(*trait119)(Class*);
};

Vec* Trait119_v = newVec();

int Trait119_S119child1_trait119(Class* self_) {
  S119child1* self = ((S119child1*)self_);
{
    return (self -> a);
  }
} 
Trait119* newTrait119_S119child1() {
  Trait119 (* impl) = (new Trait119());
  setVec(Trait119_v, S119child1_classId, ((void*)impl));
  ((impl -> trait119) = (& Trait119_S119child1_trait119));
  return impl;
} 
Trait119* Trait119_S119child1_ = newTrait119_S119child1();

int Trait95_S119child1_trait95(Class* self_) {
  S119child1* self = ((S119child1*)self_);
{
    return (self -> a);
  }
} 
Trait95* newTrait95_S119child1() {
  Trait95 (* impl) = (new Trait95());
  setVec(Trait95_v, S119child1_classId, ((void*)impl));
  ((impl -> trait95) = (& Trait95_S119child1_trait95));
  return impl;
} 
Trait95* Trait95_S119child1_ = newTrait95_S119child1();

int S120_classId = Class_genId();
struct S120{

  int id;
  S120 ():id(S120_classId){

  }
};


int S120child1_classId = Class_genId();
struct S120child1:S120{

  S120child1 (int a):a(a){
    (id = S120child1_classId);
{

    }
  }
  int a;
};


int S120child2_classId = Class_genId();
struct S120child2:S120{

  S120child2 (int a):a(a){
    (id = S120child2_classId);
{

    }
  }
  int a;
};


struct Trait120{

  int(*trait120)(Class*);
};

Vec* Trait120_v = newVec();

int Trait120_S120child1_trait120(Class* self_) {
  S120child1* self = ((S120child1*)self_);
{
    return (self -> a);
  }
} 
Trait120* newTrait120_S120child1() {
  Trait120 (* impl) = (new Trait120());
  setVec(Trait120_v, S120child1_classId, ((void*)impl));
  ((impl -> trait120) = (& Trait120_S120child1_trait120));
  return impl;
} 
Trait120* Trait120_S120child1_ = newTrait120_S120child1();

int Trait50_S120child1_trait50(Class* self_) {
  S120child1* self = ((S120child1*)self_);
{
    return (self -> a);
  }
} 
Trait50* newTrait50_S120child1() {
  Trait50 (* impl) = (new Trait50());
  setVec(Trait50_v, S120child1_classId, ((void*)impl));
  ((impl -> trait50) = (& Trait50_S120child1_trait50));
  return impl;
} 
Trait50* Trait50_S120child1_ = newTrait50_S120child1();

int S121_classId = Class_genId();
struct S121{

  int id;
  S121 ():id(S121_classId){

  }
};


int S121child1_classId = Class_genId();
struct S121child1:S121{

  S121child1 (int a):a(a){
    (id = S121child1_classId);
{

    }
  }
  int a;
};


int S121child2_classId = Class_genId();
struct S121child2:S121{

  S121child2 (int a):a(a){
    (id = S121child2_classId);
{

    }
  }
  int a;
};


int S121child3_classId = Class_genId();
struct S121child3:S121{

  S121child3 (int a):a(a){
    (id = S121child3_classId);
{

    }
  }
  int a;
};


int S121child4_classId = Class_genId();
struct S121child4:S121{

  S121child4 (int a):a(a){
    (id = S121child4_classId);
{

    }
  }
  int a;
};


struct Trait121{

  int(*trait121)(Class*);
};

Vec* Trait121_v = newVec();

int Trait121_S121child1_trait121(Class* self_) {
  S121child1* self = ((S121child1*)self_);
{
    return (self -> a);
  }
} 
Trait121* newTrait121_S121child1() {
  Trait121 (* impl) = (new Trait121());
  setVec(Trait121_v, S121child1_classId, ((void*)impl));
  ((impl -> trait121) = (& Trait121_S121child1_trait121));
  return impl;
} 
Trait121* Trait121_S121child1_ = newTrait121_S121child1();

int Trait121_S121child2_trait121(Class* self_) {
  S121child2* self = ((S121child2*)self_);
{
    return (self -> a);
  }
} 
Trait121* newTrait121_S121child2() {
  Trait121 (* impl) = (new Trait121());
  setVec(Trait121_v, S121child2_classId, ((void*)impl));
  ((impl -> trait121) = (& Trait121_S121child2_trait121));
  return impl;
} 
Trait121* Trait121_S121child2_ = newTrait121_S121child2();

int Trait34_S121child1_trait34(Class* self_) {
  S121child1* self = ((S121child1*)self_);
{
    return (self -> a);
  }
} 
Trait34* newTrait34_S121child1() {
  Trait34 (* impl) = (new Trait34());
  setVec(Trait34_v, S121child1_classId, ((void*)impl));
  ((impl -> trait34) = (& Trait34_S121child1_trait34));
  return impl;
} 
Trait34* Trait34_S121child1_ = newTrait34_S121child1();

int S122_classId = Class_genId();
struct S122{

  int id;
  S122 ():id(S122_classId){

  }
};


int S122child1_classId = Class_genId();
struct S122child1:S122{

  S122child1 (int a):a(a){
    (id = S122child1_classId);
{

    }
  }
  int a;
};


int S122child2_classId = Class_genId();
struct S122child2:S122{

  S122child2 (int a):a(a){
    (id = S122child2_classId);
{

    }
  }
  int a;
};


struct Trait122{

  int(*trait122)(Class*);
};

Vec* Trait122_v = newVec();

int Trait122_S122child1_trait122(Class* self_) {
  S122child1* self = ((S122child1*)self_);
{
    return (self -> a);
  }
} 
Trait122* newTrait122_S122child1() {
  Trait122 (* impl) = (new Trait122());
  setVec(Trait122_v, S122child1_classId, ((void*)impl));
  ((impl -> trait122) = (& Trait122_S122child1_trait122));
  return impl;
} 
Trait122* Trait122_S122child1_ = newTrait122_S122child1();

int Trait122_S122child2_trait122(Class* self_) {
  S122child2* self = ((S122child2*)self_);
{
    return (self -> a);
  }
} 
Trait122* newTrait122_S122child2() {
  Trait122 (* impl) = (new Trait122());
  setVec(Trait122_v, S122child2_classId, ((void*)impl));
  ((impl -> trait122) = (& Trait122_S122child2_trait122));
  return impl;
} 
Trait122* Trait122_S122child2_ = newTrait122_S122child2();

int Trait46_S122child1_trait46(Class* self_) {
  S122child1* self = ((S122child1*)self_);
{
    return (self -> a);
  }
} 
Trait46* newTrait46_S122child1() {
  Trait46 (* impl) = (new Trait46());
  setVec(Trait46_v, S122child1_classId, ((void*)impl));
  ((impl -> trait46) = (& Trait46_S122child1_trait46));
  return impl;
} 
Trait46* Trait46_S122child1_ = newTrait46_S122child1();

int S123_classId = Class_genId();
struct S123{

  int id;
  S123 ():id(S123_classId){

  }
};


int S123child1_classId = Class_genId();
struct S123child1:S123{

  S123child1 (int a):a(a){
    (id = S123child1_classId);
{

    }
  }
  int a;
};


int S123child2_classId = Class_genId();
struct S123child2:S123{

  S123child2 (int a):a(a){
    (id = S123child2_classId);
{

    }
  }
  int a;
};


struct Trait123{

  int(*trait123)(Class*);
};

Vec* Trait123_v = newVec();

int Trait123_S123child1_trait123(Class* self_) {
  S123child1* self = ((S123child1*)self_);
{
    return (self -> a);
  }
} 
Trait123* newTrait123_S123child1() {
  Trait123 (* impl) = (new Trait123());
  setVec(Trait123_v, S123child1_classId, ((void*)impl));
  ((impl -> trait123) = (& Trait123_S123child1_trait123));
  return impl;
} 
Trait123* Trait123_S123child1_ = newTrait123_S123child1();

int Trait52_S123child1_trait52(Class* self_) {
  S123child1* self = ((S123child1*)self_);
{
    return (self -> a);
  }
} 
Trait52* newTrait52_S123child1() {
  Trait52 (* impl) = (new Trait52());
  setVec(Trait52_v, S123child1_classId, ((void*)impl));
  ((impl -> trait52) = (& Trait52_S123child1_trait52));
  return impl;
} 
Trait52* Trait52_S123child1_ = newTrait52_S123child1();

int S124_classId = Class_genId();
struct S124{

  int id;
  S124 ():id(S124_classId){

  }
};


int S124child1_classId = Class_genId();
struct S124child1:S124{

  S124child1 (int a):a(a){
    (id = S124child1_classId);
{

    }
  }
  int a;
};


struct Trait124{

  int(*trait124)(Class*);
};

Vec* Trait124_v = newVec();

int Trait124_S124child1_trait124(Class* self_) {
  S124child1* self = ((S124child1*)self_);
{
    return (self -> a);
  }
} 
Trait124* newTrait124_S124child1() {
  Trait124 (* impl) = (new Trait124());
  setVec(Trait124_v, S124child1_classId, ((void*)impl));
  ((impl -> trait124) = (& Trait124_S124child1_trait124));
  return impl;
} 
Trait124* Trait124_S124child1_ = newTrait124_S124child1();

int Trait48_S124child1_trait48(Class* self_) {
  S124child1* self = ((S124child1*)self_);
{
    return (self -> a);
  }
} 
Trait48* newTrait48_S124child1() {
  Trait48 (* impl) = (new Trait48());
  setVec(Trait48_v, S124child1_classId, ((void*)impl));
  ((impl -> trait48) = (& Trait48_S124child1_trait48));
  return impl;
} 
Trait48* Trait48_S124child1_ = newTrait48_S124child1();

int S125_classId = Class_genId();
struct S125{

  int id;
  S125 ():id(S125_classId){

  }
};


int S125child1_classId = Class_genId();
struct S125child1:S125{

  S125child1 (int a):a(a){
    (id = S125child1_classId);
{

    }
  }
  int a;
};


int S125child2_classId = Class_genId();
struct S125child2:S125{

  S125child2 (int a):a(a){
    (id = S125child2_classId);
{

    }
  }
  int a;
};


int S125child3_classId = Class_genId();
struct S125child3:S125{

  S125child3 (int a):a(a){
    (id = S125child3_classId);
{

    }
  }
  int a;
};


int S125child4_classId = Class_genId();
struct S125child4:S125{

  S125child4 (int a):a(a){
    (id = S125child4_classId);
{

    }
  }
  int a;
};


struct Trait125{

  int(*trait125)(Class*);
};

Vec* Trait125_v = newVec();

int Trait125_S125child1_trait125(Class* self_) {
  S125child1* self = ((S125child1*)self_);
{
    return (self -> a);
  }
} 
Trait125* newTrait125_S125child1() {
  Trait125 (* impl) = (new Trait125());
  setVec(Trait125_v, S125child1_classId, ((void*)impl));
  ((impl -> trait125) = (& Trait125_S125child1_trait125));
  return impl;
} 
Trait125* Trait125_S125child1_ = newTrait125_S125child1();

int Trait125_S125child2_trait125(Class* self_) {
  S125child2* self = ((S125child2*)self_);
{
    return (self -> a);
  }
} 
Trait125* newTrait125_S125child2() {
  Trait125 (* impl) = (new Trait125());
  setVec(Trait125_v, S125child2_classId, ((void*)impl));
  ((impl -> trait125) = (& Trait125_S125child2_trait125));
  return impl;
} 
Trait125* Trait125_S125child2_ = newTrait125_S125child2();

int Trait125_S125child3_trait125(Class* self_) {
  S125child3* self = ((S125child3*)self_);
{
    return (self -> a);
  }
} 
Trait125* newTrait125_S125child3() {
  Trait125 (* impl) = (new Trait125());
  setVec(Trait125_v, S125child3_classId, ((void*)impl));
  ((impl -> trait125) = (& Trait125_S125child3_trait125));
  return impl;
} 
Trait125* Trait125_S125child3_ = newTrait125_S125child3();

int Trait125_S125child4_trait125(Class* self_) {
  S125child4* self = ((S125child4*)self_);
{
    return (self -> a);
  }
} 
Trait125* newTrait125_S125child4() {
  Trait125 (* impl) = (new Trait125());
  setVec(Trait125_v, S125child4_classId, ((void*)impl));
  ((impl -> trait125) = (& Trait125_S125child4_trait125));
  return impl;
} 
Trait125* Trait125_S125child4_ = newTrait125_S125child4();

int Trait33_S125child1_trait33(Class* self_) {
  S125child1* self = ((S125child1*)self_);
{
    return (self -> a);
  }
} 
Trait33* newTrait33_S125child1() {
  Trait33 (* impl) = (new Trait33());
  setVec(Trait33_v, S125child1_classId, ((void*)impl));
  ((impl -> trait33) = (& Trait33_S125child1_trait33));
  return impl;
} 
Trait33* Trait33_S125child1_ = newTrait33_S125child1();

int S126_classId = Class_genId();
struct S126{

  int id;
  S126 ():id(S126_classId){

  }
};


int S126child1_classId = Class_genId();
struct S126child1:S126{

  S126child1 (int a):a(a){
    (id = S126child1_classId);
{

    }
  }
  int a;
};


int S126child2_classId = Class_genId();
struct S126child2:S126{

  S126child2 (int a):a(a){
    (id = S126child2_classId);
{

    }
  }
  int a;
};


int S126child3_classId = Class_genId();
struct S126child3:S126{

  S126child3 (int a):a(a){
    (id = S126child3_classId);
{

    }
  }
  int a;
};


int S126child4_classId = Class_genId();
struct S126child4:S126{

  S126child4 (int a):a(a){
    (id = S126child4_classId);
{

    }
  }
  int a;
};


struct Trait126{

  int(*trait126)(Class*);
};

Vec* Trait126_v = newVec();

int Trait126_S126child1_trait126(Class* self_) {
  S126child1* self = ((S126child1*)self_);
{
    return (self -> a);
  }
} 
Trait126* newTrait126_S126child1() {
  Trait126 (* impl) = (new Trait126());
  setVec(Trait126_v, S126child1_classId, ((void*)impl));
  ((impl -> trait126) = (& Trait126_S126child1_trait126));
  return impl;
} 
Trait126* Trait126_S126child1_ = newTrait126_S126child1();

int Trait126_S126child2_trait126(Class* self_) {
  S126child2* self = ((S126child2*)self_);
{
    return (self -> a);
  }
} 
Trait126* newTrait126_S126child2() {
  Trait126 (* impl) = (new Trait126());
  setVec(Trait126_v, S126child2_classId, ((void*)impl));
  ((impl -> trait126) = (& Trait126_S126child2_trait126));
  return impl;
} 
Trait126* Trait126_S126child2_ = newTrait126_S126child2();

int Trait126_S126child3_trait126(Class* self_) {
  S126child3* self = ((S126child3*)self_);
{
    return (self -> a);
  }
} 
Trait126* newTrait126_S126child3() {
  Trait126 (* impl) = (new Trait126());
  setVec(Trait126_v, S126child3_classId, ((void*)impl));
  ((impl -> trait126) = (& Trait126_S126child3_trait126));
  return impl;
} 
Trait126* Trait126_S126child3_ = newTrait126_S126child3();

int Trait11_S126child1_trait11(Class* self_) {
  S126child1* self = ((S126child1*)self_);
{
    return (self -> a);
  }
} 
Trait11* newTrait11_S126child1() {
  Trait11 (* impl) = (new Trait11());
  setVec(Trait11_v, S126child1_classId, ((void*)impl));
  ((impl -> trait11) = (& Trait11_S126child1_trait11));
  return impl;
} 
Trait11* Trait11_S126child1_ = newTrait11_S126child1();

int S127_classId = Class_genId();
struct S127{

  int id;
  S127 ():id(S127_classId){

  }
};


int S127child1_classId = Class_genId();
struct S127child1:S127{

  S127child1 (int a):a(a){
    (id = S127child1_classId);
{

    }
  }
  int a;
};


int S127child2_classId = Class_genId();
struct S127child2:S127{

  S127child2 (int a):a(a){
    (id = S127child2_classId);
{

    }
  }
  int a;
};


int S127child3_classId = Class_genId();
struct S127child3:S127{

  S127child3 (int a):a(a){
    (id = S127child3_classId);
{

    }
  }
  int a;
};


struct Trait127{

  int(*trait127)(Class*);
};

Vec* Trait127_v = newVec();

int Trait127_S127child1_trait127(Class* self_) {
  S127child1* self = ((S127child1*)self_);
{
    return (self -> a);
  }
} 
Trait127* newTrait127_S127child1() {
  Trait127 (* impl) = (new Trait127());
  setVec(Trait127_v, S127child1_classId, ((void*)impl));
  ((impl -> trait127) = (& Trait127_S127child1_trait127));
  return impl;
} 
Trait127* Trait127_S127child1_ = newTrait127_S127child1();

int Trait127_S127child2_trait127(Class* self_) {
  S127child2* self = ((S127child2*)self_);
{
    return (self -> a);
  }
} 
Trait127* newTrait127_S127child2() {
  Trait127 (* impl) = (new Trait127());
  setVec(Trait127_v, S127child2_classId, ((void*)impl));
  ((impl -> trait127) = (& Trait127_S127child2_trait127));
  return impl;
} 
Trait127* Trait127_S127child2_ = newTrait127_S127child2();

int Trait71_S127child1_trait71(Class* self_) {
  S127child1* self = ((S127child1*)self_);
{
    return (self -> a);
  }
} 
Trait71* newTrait71_S127child1() {
  Trait71 (* impl) = (new Trait71());
  setVec(Trait71_v, S127child1_classId, ((void*)impl));
  ((impl -> trait71) = (& Trait71_S127child1_trait71));
  return impl;
} 
Trait71* Trait71_S127child1_ = newTrait71_S127child1();

int S128_classId = Class_genId();
struct S128{

  int id;
  S128 ():id(S128_classId){

  }
};


int S128child1_classId = Class_genId();
struct S128child1:S128{

  S128child1 (int a):a(a){
    (id = S128child1_classId);
{

    }
  }
  int a;
};


int S128child2_classId = Class_genId();
struct S128child2:S128{

  S128child2 (int a):a(a){
    (id = S128child2_classId);
{

    }
  }
  int a;
};


struct Trait128{

  int(*trait128)(Class*);
};

Vec* Trait128_v = newVec();

int Trait128_S128child1_trait128(Class* self_) {
  S128child1* self = ((S128child1*)self_);
{
    return (self -> a);
  }
} 
Trait128* newTrait128_S128child1() {
  Trait128 (* impl) = (new Trait128());
  setVec(Trait128_v, S128child1_classId, ((void*)impl));
  ((impl -> trait128) = (& Trait128_S128child1_trait128));
  return impl;
} 
Trait128* Trait128_S128child1_ = newTrait128_S128child1();

int Trait124_S128child1_trait124(Class* self_) {
  S128child1* self = ((S128child1*)self_);
{
    return (self -> a);
  }
} 
Trait124* newTrait124_S128child1() {
  Trait124 (* impl) = (new Trait124());
  setVec(Trait124_v, S128child1_classId, ((void*)impl));
  ((impl -> trait124) = (& Trait124_S128child1_trait124));
  return impl;
} 
Trait124* Trait124_S128child1_ = newTrait124_S128child1();

int S129_classId = Class_genId();
struct S129{

  int id;
  S129 ():id(S129_classId){

  }
};


int S129child1_classId = Class_genId();
struct S129child1:S129{

  S129child1 (int a):a(a){
    (id = S129child1_classId);
{

    }
  }
  int a;
};


int S129child2_classId = Class_genId();
struct S129child2:S129{

  S129child2 (int a):a(a){
    (id = S129child2_classId);
{

    }
  }
  int a;
};


struct Trait129{

  int(*trait129)(Class*);
};

Vec* Trait129_v = newVec();

int Trait129_S129child1_trait129(Class* self_) {
  S129child1* self = ((S129child1*)self_);
{
    return (self -> a);
  }
} 
Trait129* newTrait129_S129child1() {
  Trait129 (* impl) = (new Trait129());
  setVec(Trait129_v, S129child1_classId, ((void*)impl));
  ((impl -> trait129) = (& Trait129_S129child1_trait129));
  return impl;
} 
Trait129* Trait129_S129child1_ = newTrait129_S129child1();

int Trait43_S129child1_trait43(Class* self_) {
  S129child1* self = ((S129child1*)self_);
{
    return (self -> a);
  }
} 
Trait43* newTrait43_S129child1() {
  Trait43 (* impl) = (new Trait43());
  setVec(Trait43_v, S129child1_classId, ((void*)impl));
  ((impl -> trait43) = (& Trait43_S129child1_trait43));
  return impl;
} 
Trait43* Trait43_S129child1_ = newTrait43_S129child1();

int S130_classId = Class_genId();
struct S130{

  int id;
  S130 ():id(S130_classId){

  }
};


int S130child1_classId = Class_genId();
struct S130child1:S130{

  S130child1 (int a):a(a){
    (id = S130child1_classId);
{

    }
  }
  int a;
};


struct Trait130{

  int(*trait130)(Class*);
};

Vec* Trait130_v = newVec();

int Trait130_S130child1_trait130(Class* self_) {
  S130child1* self = ((S130child1*)self_);
{
    return (self -> a);
  }
} 
Trait130* newTrait130_S130child1() {
  Trait130 (* impl) = (new Trait130());
  setVec(Trait130_v, S130child1_classId, ((void*)impl));
  ((impl -> trait130) = (& Trait130_S130child1_trait130));
  return impl;
} 
Trait130* Trait130_S130child1_ = newTrait130_S130child1();

int Trait83_S130child1_trait83(Class* self_) {
  S130child1* self = ((S130child1*)self_);
{
    return (self -> a);
  }
} 
Trait83* newTrait83_S130child1() {
  Trait83 (* impl) = (new Trait83());
  setVec(Trait83_v, S130child1_classId, ((void*)impl));
  ((impl -> trait83) = (& Trait83_S130child1_trait83));
  return impl;
} 
Trait83* Trait83_S130child1_ = newTrait83_S130child1();

int S131_classId = Class_genId();
struct S131{

  int id;
  S131 ():id(S131_classId){

  }
};


int S131child1_classId = Class_genId();
struct S131child1:S131{

  S131child1 (int a):a(a){
    (id = S131child1_classId);
{

    }
  }
  int a;
};


int S131child2_classId = Class_genId();
struct S131child2:S131{

  S131child2 (int a):a(a){
    (id = S131child2_classId);
{

    }
  }
  int a;
};


struct Trait131{

  int(*trait131)(Class*);
};

Vec* Trait131_v = newVec();

int Trait131_S131child1_trait131(Class* self_) {
  S131child1* self = ((S131child1*)self_);
{
    return (self -> a);
  }
} 
Trait131* newTrait131_S131child1() {
  Trait131 (* impl) = (new Trait131());
  setVec(Trait131_v, S131child1_classId, ((void*)impl));
  ((impl -> trait131) = (& Trait131_S131child1_trait131));
  return impl;
} 
Trait131* Trait131_S131child1_ = newTrait131_S131child1();

int Trait131_S131child2_trait131(Class* self_) {
  S131child2* self = ((S131child2*)self_);
{
    return (self -> a);
  }
} 
Trait131* newTrait131_S131child2() {
  Trait131 (* impl) = (new Trait131());
  setVec(Trait131_v, S131child2_classId, ((void*)impl));
  ((impl -> trait131) = (& Trait131_S131child2_trait131));
  return impl;
} 
Trait131* Trait131_S131child2_ = newTrait131_S131child2();

int Trait56_S131child1_trait56(Class* self_) {
  S131child1* self = ((S131child1*)self_);
{
    return (self -> a);
  }
} 
Trait56* newTrait56_S131child1() {
  Trait56 (* impl) = (new Trait56());
  setVec(Trait56_v, S131child1_classId, ((void*)impl));
  ((impl -> trait56) = (& Trait56_S131child1_trait56));
  return impl;
} 
Trait56* Trait56_S131child1_ = newTrait56_S131child1();

int S132_classId = Class_genId();
struct S132{

  int id;
  S132 ():id(S132_classId){

  }
};


int S132child1_classId = Class_genId();
struct S132child1:S132{

  S132child1 (int a):a(a){
    (id = S132child1_classId);
{

    }
  }
  int a;
};


int S132child2_classId = Class_genId();
struct S132child2:S132{

  S132child2 (int a):a(a){
    (id = S132child2_classId);
{

    }
  }
  int a;
};


int S132child3_classId = Class_genId();
struct S132child3:S132{

  S132child3 (int a):a(a){
    (id = S132child3_classId);
{

    }
  }
  int a;
};


int S132child4_classId = Class_genId();
struct S132child4:S132{

  S132child4 (int a):a(a){
    (id = S132child4_classId);
{

    }
  }
  int a;
};


struct Trait132{

  int(*trait132)(Class*);
};

Vec* Trait132_v = newVec();

int Trait132_S132child1_trait132(Class* self_) {
  S132child1* self = ((S132child1*)self_);
{
    return (self -> a);
  }
} 
Trait132* newTrait132_S132child1() {
  Trait132 (* impl) = (new Trait132());
  setVec(Trait132_v, S132child1_classId, ((void*)impl));
  ((impl -> trait132) = (& Trait132_S132child1_trait132));
  return impl;
} 
Trait132* Trait132_S132child1_ = newTrait132_S132child1();

int Trait132_S132child2_trait132(Class* self_) {
  S132child2* self = ((S132child2*)self_);
{
    return (self -> a);
  }
} 
Trait132* newTrait132_S132child2() {
  Trait132 (* impl) = (new Trait132());
  setVec(Trait132_v, S132child2_classId, ((void*)impl));
  ((impl -> trait132) = (& Trait132_S132child2_trait132));
  return impl;
} 
Trait132* Trait132_S132child2_ = newTrait132_S132child2();

int Trait132_S132child3_trait132(Class* self_) {
  S132child3* self = ((S132child3*)self_);
{
    return (self -> a);
  }
} 
Trait132* newTrait132_S132child3() {
  Trait132 (* impl) = (new Trait132());
  setVec(Trait132_v, S132child3_classId, ((void*)impl));
  ((impl -> trait132) = (& Trait132_S132child3_trait132));
  return impl;
} 
Trait132* Trait132_S132child3_ = newTrait132_S132child3();

int Trait48_S132child1_trait48(Class* self_) {
  S132child1* self = ((S132child1*)self_);
{
    return (self -> a);
  }
} 
Trait48* newTrait48_S132child1() {
  Trait48 (* impl) = (new Trait48());
  setVec(Trait48_v, S132child1_classId, ((void*)impl));
  ((impl -> trait48) = (& Trait48_S132child1_trait48));
  return impl;
} 
Trait48* Trait48_S132child1_ = newTrait48_S132child1();

int S133_classId = Class_genId();
struct S133{

  int id;
  S133 ():id(S133_classId){

  }
};


int S133child1_classId = Class_genId();
struct S133child1:S133{

  S133child1 (int a):a(a){
    (id = S133child1_classId);
{

    }
  }
  int a;
};


int S133child2_classId = Class_genId();
struct S133child2:S133{

  S133child2 (int a):a(a){
    (id = S133child2_classId);
{

    }
  }
  int a;
};


struct Trait133{

  int(*trait133)(Class*);
};

Vec* Trait133_v = newVec();

int Trait133_S133child1_trait133(Class* self_) {
  S133child1* self = ((S133child1*)self_);
{
    return (self -> a);
  }
} 
Trait133* newTrait133_S133child1() {
  Trait133 (* impl) = (new Trait133());
  setVec(Trait133_v, S133child1_classId, ((void*)impl));
  ((impl -> trait133) = (& Trait133_S133child1_trait133));
  return impl;
} 
Trait133* Trait133_S133child1_ = newTrait133_S133child1();

int Trait133_S133child2_trait133(Class* self_) {
  S133child2* self = ((S133child2*)self_);
{
    return (self -> a);
  }
} 
Trait133* newTrait133_S133child2() {
  Trait133 (* impl) = (new Trait133());
  setVec(Trait133_v, S133child2_classId, ((void*)impl));
  ((impl -> trait133) = (& Trait133_S133child2_trait133));
  return impl;
} 
Trait133* Trait133_S133child2_ = newTrait133_S133child2();

int Trait41_S133child1_trait41(Class* self_) {
  S133child1* self = ((S133child1*)self_);
{
    return (self -> a);
  }
} 
Trait41* newTrait41_S133child1() {
  Trait41 (* impl) = (new Trait41());
  setVec(Trait41_v, S133child1_classId, ((void*)impl));
  ((impl -> trait41) = (& Trait41_S133child1_trait41));
  return impl;
} 
Trait41* Trait41_S133child1_ = newTrait41_S133child1();

int S134_classId = Class_genId();
struct S134{

  int id;
  S134 ():id(S134_classId){

  }
};


int S134child1_classId = Class_genId();
struct S134child1:S134{

  S134child1 (int a):a(a){
    (id = S134child1_classId);
{

    }
  }
  int a;
};


struct Trait134{

  int(*trait134)(Class*);
};

Vec* Trait134_v = newVec();

int Trait134_S134child1_trait134(Class* self_) {
  S134child1* self = ((S134child1*)self_);
{
    return (self -> a);
  }
} 
Trait134* newTrait134_S134child1() {
  Trait134 (* impl) = (new Trait134());
  setVec(Trait134_v, S134child1_classId, ((void*)impl));
  ((impl -> trait134) = (& Trait134_S134child1_trait134));
  return impl;
} 
Trait134* Trait134_S134child1_ = newTrait134_S134child1();

int Trait81_S134child1_trait81(Class* self_) {
  S134child1* self = ((S134child1*)self_);
{
    return (self -> a);
  }
} 
Trait81* newTrait81_S134child1() {
  Trait81 (* impl) = (new Trait81());
  setVec(Trait81_v, S134child1_classId, ((void*)impl));
  ((impl -> trait81) = (& Trait81_S134child1_trait81));
  return impl;
} 
Trait81* Trait81_S134child1_ = newTrait81_S134child1();

int S135_classId = Class_genId();
struct S135{

  int id;
  S135 ():id(S135_classId){

  }
};


int S135child1_classId = Class_genId();
struct S135child1:S135{

  S135child1 (int a):a(a){
    (id = S135child1_classId);
{

    }
  }
  int a;
};


int S135child2_classId = Class_genId();
struct S135child2:S135{

  S135child2 (int a):a(a){
    (id = S135child2_classId);
{

    }
  }
  int a;
};


int S135child3_classId = Class_genId();
struct S135child3:S135{

  S135child3 (int a):a(a){
    (id = S135child3_classId);
{

    }
  }
  int a;
};


struct Trait135{

  int(*trait135)(Class*);
};

Vec* Trait135_v = newVec();

int Trait135_S135child1_trait135(Class* self_) {
  S135child1* self = ((S135child1*)self_);
{
    return (self -> a);
  }
} 
Trait135* newTrait135_S135child1() {
  Trait135 (* impl) = (new Trait135());
  setVec(Trait135_v, S135child1_classId, ((void*)impl));
  ((impl -> trait135) = (& Trait135_S135child1_trait135));
  return impl;
} 
Trait135* Trait135_S135child1_ = newTrait135_S135child1();

int Trait135_S135child2_trait135(Class* self_) {
  S135child2* self = ((S135child2*)self_);
{
    return (self -> a);
  }
} 
Trait135* newTrait135_S135child2() {
  Trait135 (* impl) = (new Trait135());
  setVec(Trait135_v, S135child2_classId, ((void*)impl));
  ((impl -> trait135) = (& Trait135_S135child2_trait135));
  return impl;
} 
Trait135* Trait135_S135child2_ = newTrait135_S135child2();

int Trait133_S135child1_trait133(Class* self_) {
  S135child1* self = ((S135child1*)self_);
{
    return (self -> a);
  }
} 
Trait133* newTrait133_S135child1() {
  Trait133 (* impl) = (new Trait133());
  setVec(Trait133_v, S135child1_classId, ((void*)impl));
  ((impl -> trait133) = (& Trait133_S135child1_trait133));
  return impl;
} 
Trait133* Trait133_S135child1_ = newTrait133_S135child1();

int S136_classId = Class_genId();
struct S136{

  int id;
  S136 ():id(S136_classId){

  }
};


int S136child1_classId = Class_genId();
struct S136child1:S136{

  S136child1 (int a):a(a){
    (id = S136child1_classId);
{

    }
  }
  int a;
};


struct Trait136{

  int(*trait136)(Class*);
};

Vec* Trait136_v = newVec();

int Trait136_S136child1_trait136(Class* self_) {
  S136child1* self = ((S136child1*)self_);
{
    return (self -> a);
  }
} 
Trait136* newTrait136_S136child1() {
  Trait136 (* impl) = (new Trait136());
  setVec(Trait136_v, S136child1_classId, ((void*)impl));
  ((impl -> trait136) = (& Trait136_S136child1_trait136));
  return impl;
} 
Trait136* Trait136_S136child1_ = newTrait136_S136child1();

int Trait108_S136child1_trait108(Class* self_) {
  S136child1* self = ((S136child1*)self_);
{
    return (self -> a);
  }
} 
Trait108* newTrait108_S136child1() {
  Trait108 (* impl) = (new Trait108());
  setVec(Trait108_v, S136child1_classId, ((void*)impl));
  ((impl -> trait108) = (& Trait108_S136child1_trait108));
  return impl;
} 
Trait108* Trait108_S136child1_ = newTrait108_S136child1();

int S137_classId = Class_genId();
struct S137{

  int id;
  S137 ():id(S137_classId){

  }
};


int S137child1_classId = Class_genId();
struct S137child1:S137{

  S137child1 (int a):a(a){
    (id = S137child1_classId);
{

    }
  }
  int a;
};


struct Trait137{

  int(*trait137)(Class*);
};

Vec* Trait137_v = newVec();

int Trait137_S137child1_trait137(Class* self_) {
  S137child1* self = ((S137child1*)self_);
{
    return (self -> a);
  }
} 
Trait137* newTrait137_S137child1() {
  Trait137 (* impl) = (new Trait137());
  setVec(Trait137_v, S137child1_classId, ((void*)impl));
  ((impl -> trait137) = (& Trait137_S137child1_trait137));
  return impl;
} 
Trait137* Trait137_S137child1_ = newTrait137_S137child1();

int Trait49_S137child1_trait49(Class* self_) {
  S137child1* self = ((S137child1*)self_);
{
    return (self -> a);
  }
} 
Trait49* newTrait49_S137child1() {
  Trait49 (* impl) = (new Trait49());
  setVec(Trait49_v, S137child1_classId, ((void*)impl));
  ((impl -> trait49) = (& Trait49_S137child1_trait49));
  return impl;
} 
Trait49* Trait49_S137child1_ = newTrait49_S137child1();

int S138_classId = Class_genId();
struct S138{

  int id;
  S138 ():id(S138_classId){

  }
};


int S138child1_classId = Class_genId();
struct S138child1:S138{

  S138child1 (int a):a(a){
    (id = S138child1_classId);
{

    }
  }
  int a;
};


int S138child2_classId = Class_genId();
struct S138child2:S138{

  S138child2 (int a):a(a){
    (id = S138child2_classId);
{

    }
  }
  int a;
};


int S138child3_classId = Class_genId();
struct S138child3:S138{

  S138child3 (int a):a(a){
    (id = S138child3_classId);
{

    }
  }
  int a;
};


int S138child4_classId = Class_genId();
struct S138child4:S138{

  S138child4 (int a):a(a){
    (id = S138child4_classId);
{

    }
  }
  int a;
};


int S138child5_classId = Class_genId();
struct S138child5:S138{

  S138child5 (int a):a(a){
    (id = S138child5_classId);
{

    }
  }
  int a;
};


struct Trait138{

  int(*trait138)(Class*);
};

Vec* Trait138_v = newVec();

int Trait138_S138child1_trait138(Class* self_) {
  S138child1* self = ((S138child1*)self_);
{
    return (self -> a);
  }
} 
Trait138* newTrait138_S138child1() {
  Trait138 (* impl) = (new Trait138());
  setVec(Trait138_v, S138child1_classId, ((void*)impl));
  ((impl -> trait138) = (& Trait138_S138child1_trait138));
  return impl;
} 
Trait138* Trait138_S138child1_ = newTrait138_S138child1();

int Trait16_S138child1_trait16(Class* self_) {
  S138child1* self = ((S138child1*)self_);
{
    return (self -> a);
  }
} 
Trait16* newTrait16_S138child1() {
  Trait16 (* impl) = (new Trait16());
  setVec(Trait16_v, S138child1_classId, ((void*)impl));
  ((impl -> trait16) = (& Trait16_S138child1_trait16));
  return impl;
} 
Trait16* Trait16_S138child1_ = newTrait16_S138child1();

int S139_classId = Class_genId();
struct S139{

  int id;
  S139 ():id(S139_classId){

  }
};


int S139child1_classId = Class_genId();
struct S139child1:S139{

  S139child1 (int a):a(a){
    (id = S139child1_classId);
{

    }
  }
  int a;
};


int S139child2_classId = Class_genId();
struct S139child2:S139{

  S139child2 (int a):a(a){
    (id = S139child2_classId);
{

    }
  }
  int a;
};


struct Trait139{

  int(*trait139)(Class*);
};

Vec* Trait139_v = newVec();

int Trait139_S139child1_trait139(Class* self_) {
  S139child1* self = ((S139child1*)self_);
{
    return (self -> a);
  }
} 
Trait139* newTrait139_S139child1() {
  Trait139 (* impl) = (new Trait139());
  setVec(Trait139_v, S139child1_classId, ((void*)impl));
  ((impl -> trait139) = (& Trait139_S139child1_trait139));
  return impl;
} 
Trait139* Trait139_S139child1_ = newTrait139_S139child1();

int Trait139_S139child2_trait139(Class* self_) {
  S139child2* self = ((S139child2*)self_);
{
    return (self -> a);
  }
} 
Trait139* newTrait139_S139child2() {
  Trait139 (* impl) = (new Trait139());
  setVec(Trait139_v, S139child2_classId, ((void*)impl));
  ((impl -> trait139) = (& Trait139_S139child2_trait139));
  return impl;
} 
Trait139* Trait139_S139child2_ = newTrait139_S139child2();

int Trait125_S139child1_trait125(Class* self_) {
  S139child1* self = ((S139child1*)self_);
{
    return (self -> a);
  }
} 
Trait125* newTrait125_S139child1() {
  Trait125 (* impl) = (new Trait125());
  setVec(Trait125_v, S139child1_classId, ((void*)impl));
  ((impl -> trait125) = (& Trait125_S139child1_trait125));
  return impl;
} 
Trait125* Trait125_S139child1_ = newTrait125_S139child1();

int S140_classId = Class_genId();
struct S140{

  int id;
  S140 ():id(S140_classId){

  }
};


int S140child1_classId = Class_genId();
struct S140child1:S140{

  S140child1 (int a):a(a){
    (id = S140child1_classId);
{

    }
  }
  int a;
};


struct Trait140{

  int(*trait140)(Class*);
};

Vec* Trait140_v = newVec();

int Trait140_S140child1_trait140(Class* self_) {
  S140child1* self = ((S140child1*)self_);
{
    return (self -> a);
  }
} 
Trait140* newTrait140_S140child1() {
  Trait140 (* impl) = (new Trait140());
  setVec(Trait140_v, S140child1_classId, ((void*)impl));
  ((impl -> trait140) = (& Trait140_S140child1_trait140));
  return impl;
} 
Trait140* Trait140_S140child1_ = newTrait140_S140child1();

int Trait4_S140child1_trait4(Class* self_) {
  S140child1* self = ((S140child1*)self_);
{
    return (self -> a);
  }
} 
Trait4* newTrait4_S140child1() {
  Trait4 (* impl) = (new Trait4());
  setVec(Trait4_v, S140child1_classId, ((void*)impl));
  ((impl -> trait4) = (& Trait4_S140child1_trait4));
  return impl;
} 
Trait4* Trait4_S140child1_ = newTrait4_S140child1();

int S141_classId = Class_genId();
struct S141{

  int id;
  S141 ():id(S141_classId){

  }
};


int S141child1_classId = Class_genId();
struct S141child1:S141{

  S141child1 (int a):a(a){
    (id = S141child1_classId);
{

    }
  }
  int a;
};


int S141child2_classId = Class_genId();
struct S141child2:S141{

  S141child2 (int a):a(a){
    (id = S141child2_classId);
{

    }
  }
  int a;
};


int S141child3_classId = Class_genId();
struct S141child3:S141{

  S141child3 (int a):a(a){
    (id = S141child3_classId);
{

    }
  }
  int a;
};


struct Trait141{

  int(*trait141)(Class*);
};

Vec* Trait141_v = newVec();

int Trait141_S141child1_trait141(Class* self_) {
  S141child1* self = ((S141child1*)self_);
{
    return (self -> a);
  }
} 
Trait141* newTrait141_S141child1() {
  Trait141 (* impl) = (new Trait141());
  setVec(Trait141_v, S141child1_classId, ((void*)impl));
  ((impl -> trait141) = (& Trait141_S141child1_trait141));
  return impl;
} 
Trait141* Trait141_S141child1_ = newTrait141_S141child1();

int Trait21_S141child1_trait21(Class* self_) {
  S141child1* self = ((S141child1*)self_);
{
    return (self -> a);
  }
} 
Trait21* newTrait21_S141child1() {
  Trait21 (* impl) = (new Trait21());
  setVec(Trait21_v, S141child1_classId, ((void*)impl));
  ((impl -> trait21) = (& Trait21_S141child1_trait21));
  return impl;
} 
Trait21* Trait21_S141child1_ = newTrait21_S141child1();

int S142_classId = Class_genId();
struct S142{

  int id;
  S142 ():id(S142_classId){

  }
};


int S142child1_classId = Class_genId();
struct S142child1:S142{

  S142child1 (int a):a(a){
    (id = S142child1_classId);
{

    }
  }
  int a;
};


int S142child2_classId = Class_genId();
struct S142child2:S142{

  S142child2 (int a):a(a){
    (id = S142child2_classId);
{

    }
  }
  int a;
};


int S142child3_classId = Class_genId();
struct S142child3:S142{

  S142child3 (int a):a(a){
    (id = S142child3_classId);
{

    }
  }
  int a;
};


int S142child4_classId = Class_genId();
struct S142child4:S142{

  S142child4 (int a):a(a){
    (id = S142child4_classId);
{

    }
  }
  int a;
};


struct Trait142{

  int(*trait142)(Class*);
};

Vec* Trait142_v = newVec();

int Trait142_S142child1_trait142(Class* self_) {
  S142child1* self = ((S142child1*)self_);
{
    return (self -> a);
  }
} 
Trait142* newTrait142_S142child1() {
  Trait142 (* impl) = (new Trait142());
  setVec(Trait142_v, S142child1_classId, ((void*)impl));
  ((impl -> trait142) = (& Trait142_S142child1_trait142));
  return impl;
} 
Trait142* Trait142_S142child1_ = newTrait142_S142child1();

int Trait142_S142child2_trait142(Class* self_) {
  S142child2* self = ((S142child2*)self_);
{
    return (self -> a);
  }
} 
Trait142* newTrait142_S142child2() {
  Trait142 (* impl) = (new Trait142());
  setVec(Trait142_v, S142child2_classId, ((void*)impl));
  ((impl -> trait142) = (& Trait142_S142child2_trait142));
  return impl;
} 
Trait142* Trait142_S142child2_ = newTrait142_S142child2();

int Trait51_S142child1_trait51(Class* self_) {
  S142child1* self = ((S142child1*)self_);
{
    return (self -> a);
  }
} 
Trait51* newTrait51_S142child1() {
  Trait51 (* impl) = (new Trait51());
  setVec(Trait51_v, S142child1_classId, ((void*)impl));
  ((impl -> trait51) = (& Trait51_S142child1_trait51));
  return impl;
} 
Trait51* Trait51_S142child1_ = newTrait51_S142child1();

int S143_classId = Class_genId();
struct S143{

  int id;
  S143 ():id(S143_classId){

  }
};


int S143child1_classId = Class_genId();
struct S143child1:S143{

  S143child1 (int a):a(a){
    (id = S143child1_classId);
{

    }
  }
  int a;
};


struct Trait143{

  int(*trait143)(Class*);
};

Vec* Trait143_v = newVec();

int Trait143_S143child1_trait143(Class* self_) {
  S143child1* self = ((S143child1*)self_);
{
    return (self -> a);
  }
} 
Trait143* newTrait143_S143child1() {
  Trait143 (* impl) = (new Trait143());
  setVec(Trait143_v, S143child1_classId, ((void*)impl));
  ((impl -> trait143) = (& Trait143_S143child1_trait143));
  return impl;
} 
Trait143* Trait143_S143child1_ = newTrait143_S143child1();

int Trait95_S143child1_trait95(Class* self_) {
  S143child1* self = ((S143child1*)self_);
{
    return (self -> a);
  }
} 
Trait95* newTrait95_S143child1() {
  Trait95 (* impl) = (new Trait95());
  setVec(Trait95_v, S143child1_classId, ((void*)impl));
  ((impl -> trait95) = (& Trait95_S143child1_trait95));
  return impl;
} 
Trait95* Trait95_S143child1_ = newTrait95_S143child1();

int S144_classId = Class_genId();
struct S144{

  int id;
  S144 ():id(S144_classId){

  }
};


int S144child1_classId = Class_genId();
struct S144child1:S144{

  S144child1 (int a):a(a){
    (id = S144child1_classId);
{

    }
  }
  int a;
};


int S144child2_classId = Class_genId();
struct S144child2:S144{

  S144child2 (int a):a(a){
    (id = S144child2_classId);
{

    }
  }
  int a;
};


struct Trait144{

  int(*trait144)(Class*);
};

Vec* Trait144_v = newVec();

int Trait144_S144child1_trait144(Class* self_) {
  S144child1* self = ((S144child1*)self_);
{
    return (self -> a);
  }
} 
Trait144* newTrait144_S144child1() {
  Trait144 (* impl) = (new Trait144());
  setVec(Trait144_v, S144child1_classId, ((void*)impl));
  ((impl -> trait144) = (& Trait144_S144child1_trait144));
  return impl;
} 
Trait144* Trait144_S144child1_ = newTrait144_S144child1();

int Trait144_S144child2_trait144(Class* self_) {
  S144child2* self = ((S144child2*)self_);
{
    return (self -> a);
  }
} 
Trait144* newTrait144_S144child2() {
  Trait144 (* impl) = (new Trait144());
  setVec(Trait144_v, S144child2_classId, ((void*)impl));
  ((impl -> trait144) = (& Trait144_S144child2_trait144));
  return impl;
} 
Trait144* Trait144_S144child2_ = newTrait144_S144child2();

int Trait3_S144child1_trait3(Class* self_) {
  S144child1* self = ((S144child1*)self_);
{
    return (self -> a);
  }
} 
Trait3* newTrait3_S144child1() {
  Trait3 (* impl) = (new Trait3());
  setVec(Trait3_v, S144child1_classId, ((void*)impl));
  ((impl -> trait3) = (& Trait3_S144child1_trait3));
  return impl;
} 
Trait3* Trait3_S144child1_ = newTrait3_S144child1();

int S145_classId = Class_genId();
struct S145{

  int id;
  S145 ():id(S145_classId){

  }
};


int S145child1_classId = Class_genId();
struct S145child1:S145{

  S145child1 (int a):a(a){
    (id = S145child1_classId);
{

    }
  }
  int a;
};


int S145child2_classId = Class_genId();
struct S145child2:S145{

  S145child2 (int a):a(a){
    (id = S145child2_classId);
{

    }
  }
  int a;
};


struct Trait145{

  int(*trait145)(Class*);
};

Vec* Trait145_v = newVec();

int Trait145_S145child1_trait145(Class* self_) {
  S145child1* self = ((S145child1*)self_);
{
    return (self -> a);
  }
} 
Trait145* newTrait145_S145child1() {
  Trait145 (* impl) = (new Trait145());
  setVec(Trait145_v, S145child1_classId, ((void*)impl));
  ((impl -> trait145) = (& Trait145_S145child1_trait145));
  return impl;
} 
Trait145* Trait145_S145child1_ = newTrait145_S145child1();

int Trait145_S145child2_trait145(Class* self_) {
  S145child2* self = ((S145child2*)self_);
{
    return (self -> a);
  }
} 
Trait145* newTrait145_S145child2() {
  Trait145 (* impl) = (new Trait145());
  setVec(Trait145_v, S145child2_classId, ((void*)impl));
  ((impl -> trait145) = (& Trait145_S145child2_trait145));
  return impl;
} 
Trait145* Trait145_S145child2_ = newTrait145_S145child2();

int Trait94_S145child1_trait94(Class* self_) {
  S145child1* self = ((S145child1*)self_);
{
    return (self -> a);
  }
} 
Trait94* newTrait94_S145child1() {
  Trait94 (* impl) = (new Trait94());
  setVec(Trait94_v, S145child1_classId, ((void*)impl));
  ((impl -> trait94) = (& Trait94_S145child1_trait94));
  return impl;
} 
Trait94* Trait94_S145child1_ = newTrait94_S145child1();

int S146_classId = Class_genId();
struct S146{

  int id;
  S146 ():id(S146_classId){

  }
};


int S146child1_classId = Class_genId();
struct S146child1:S146{

  S146child1 (int a):a(a){
    (id = S146child1_classId);
{

    }
  }
  int a;
};


int S146child2_classId = Class_genId();
struct S146child2:S146{

  S146child2 (int a):a(a){
    (id = S146child2_classId);
{

    }
  }
  int a;
};


int S146child3_classId = Class_genId();
struct S146child3:S146{

  S146child3 (int a):a(a){
    (id = S146child3_classId);
{

    }
  }
  int a;
};


struct Trait146{

  int(*trait146)(Class*);
};

Vec* Trait146_v = newVec();

int Trait146_S146child1_trait146(Class* self_) {
  S146child1* self = ((S146child1*)self_);
{
    return (self -> a);
  }
} 
Trait146* newTrait146_S146child1() {
  Trait146 (* impl) = (new Trait146());
  setVec(Trait146_v, S146child1_classId, ((void*)impl));
  ((impl -> trait146) = (& Trait146_S146child1_trait146));
  return impl;
} 
Trait146* Trait146_S146child1_ = newTrait146_S146child1();

int Trait132_S146child1_trait132(Class* self_) {
  S146child1* self = ((S146child1*)self_);
{
    return (self -> a);
  }
} 
Trait132* newTrait132_S146child1() {
  Trait132 (* impl) = (new Trait132());
  setVec(Trait132_v, S146child1_classId, ((void*)impl));
  ((impl -> trait132) = (& Trait132_S146child1_trait132));
  return impl;
} 
Trait132* Trait132_S146child1_ = newTrait132_S146child1();

int S147_classId = Class_genId();
struct S147{

  int id;
  S147 ():id(S147_classId){

  }
};


int S147child1_classId = Class_genId();
struct S147child1:S147{

  S147child1 (int a):a(a){
    (id = S147child1_classId);
{

    }
  }
  int a;
};


int S147child2_classId = Class_genId();
struct S147child2:S147{

  S147child2 (int a):a(a){
    (id = S147child2_classId);
{

    }
  }
  int a;
};


int S147child3_classId = Class_genId();
struct S147child3:S147{

  S147child3 (int a):a(a){
    (id = S147child3_classId);
{

    }
  }
  int a;
};


int S147child4_classId = Class_genId();
struct S147child4:S147{

  S147child4 (int a):a(a){
    (id = S147child4_classId);
{

    }
  }
  int a;
};


struct Trait147{

  int(*trait147)(Class*);
};

Vec* Trait147_v = newVec();

int Trait147_S147child1_trait147(Class* self_) {
  S147child1* self = ((S147child1*)self_);
{
    return (self -> a);
  }
} 
Trait147* newTrait147_S147child1() {
  Trait147 (* impl) = (new Trait147());
  setVec(Trait147_v, S147child1_classId, ((void*)impl));
  ((impl -> trait147) = (& Trait147_S147child1_trait147));
  return impl;
} 
Trait147* Trait147_S147child1_ = newTrait147_S147child1();

int Trait82_S147child1_trait82(Class* self_) {
  S147child1* self = ((S147child1*)self_);
{
    return (self -> a);
  }
} 
Trait82* newTrait82_S147child1() {
  Trait82 (* impl) = (new Trait82());
  setVec(Trait82_v, S147child1_classId, ((void*)impl));
  ((impl -> trait82) = (& Trait82_S147child1_trait82));
  return impl;
} 
Trait82* Trait82_S147child1_ = newTrait82_S147child1();

int S148_classId = Class_genId();
struct S148{

  int id;
  S148 ():id(S148_classId){

  }
};


int S148child1_classId = Class_genId();
struct S148child1:S148{

  S148child1 (int a):a(a){
    (id = S148child1_classId);
{

    }
  }
  int a;
};


int S148child2_classId = Class_genId();
struct S148child2:S148{

  S148child2 (int a):a(a){
    (id = S148child2_classId);
{

    }
  }
  int a;
};


struct Trait148{

  int(*trait148)(Class*);
};

Vec* Trait148_v = newVec();

int Trait148_S148child1_trait148(Class* self_) {
  S148child1* self = ((S148child1*)self_);
{
    return (self -> a);
  }
} 
Trait148* newTrait148_S148child1() {
  Trait148 (* impl) = (new Trait148());
  setVec(Trait148_v, S148child1_classId, ((void*)impl));
  ((impl -> trait148) = (& Trait148_S148child1_trait148));
  return impl;
} 
Trait148* Trait148_S148child1_ = newTrait148_S148child1();

int Trait49_S148child1_trait49(Class* self_) {
  S148child1* self = ((S148child1*)self_);
{
    return (self -> a);
  }
} 
Trait49* newTrait49_S148child1() {
  Trait49 (* impl) = (new Trait49());
  setVec(Trait49_v, S148child1_classId, ((void*)impl));
  ((impl -> trait49) = (& Trait49_S148child1_trait49));
  return impl;
} 
Trait49* Trait49_S148child1_ = newTrait49_S148child1();

int S149_classId = Class_genId();
struct S149{

  int id;
  S149 ():id(S149_classId){

  }
};


int S149child1_classId = Class_genId();
struct S149child1:S149{

  S149child1 (int a):a(a){
    (id = S149child1_classId);
{

    }
  }
  int a;
};


struct Trait149{

  int(*trait149)(Class*);
};

Vec* Trait149_v = newVec();

int Trait149_S149child1_trait149(Class* self_) {
  S149child1* self = ((S149child1*)self_);
{
    return (self -> a);
  }
} 
Trait149* newTrait149_S149child1() {
  Trait149 (* impl) = (new Trait149());
  setVec(Trait149_v, S149child1_classId, ((void*)impl));
  ((impl -> trait149) = (& Trait149_S149child1_trait149));
  return impl;
} 
Trait149* Trait149_S149child1_ = newTrait149_S149child1();

int Trait53_S149child1_trait53(Class* self_) {
  S149child1* self = ((S149child1*)self_);
{
    return (self -> a);
  }
} 
Trait53* newTrait53_S149child1() {
  Trait53 (* impl) = (new Trait53());
  setVec(Trait53_v, S149child1_classId, ((void*)impl));
  ((impl -> trait53) = (& Trait53_S149child1_trait53));
  return impl;
} 
Trait53* Trait53_S149child1_ = newTrait53_S149child1();

int S150_classId = Class_genId();
struct S150{

  int id;
  S150 ():id(S150_classId){

  }
};


int S150child1_classId = Class_genId();
struct S150child1:S150{

  S150child1 (int a):a(a){
    (id = S150child1_classId);
{

    }
  }
  int a;
};


int S150child2_classId = Class_genId();
struct S150child2:S150{

  S150child2 (int a):a(a){
    (id = S150child2_classId);
{

    }
  }
  int a;
};


struct Trait150{

  int(*trait150)(Class*);
};

Vec* Trait150_v = newVec();

int Trait150_S150child1_trait150(Class* self_) {
  S150child1* self = ((S150child1*)self_);
{
    return (self -> a);
  }
} 
Trait150* newTrait150_S150child1() {
  Trait150 (* impl) = (new Trait150());
  setVec(Trait150_v, S150child1_classId, ((void*)impl));
  ((impl -> trait150) = (& Trait150_S150child1_trait150));
  return impl;
} 
Trait150* Trait150_S150child1_ = newTrait150_S150child1();

int Trait150_S150child2_trait150(Class* self_) {
  S150child2* self = ((S150child2*)self_);
{
    return (self -> a);
  }
} 
Trait150* newTrait150_S150child2() {
  Trait150 (* impl) = (new Trait150());
  setVec(Trait150_v, S150child2_classId, ((void*)impl));
  ((impl -> trait150) = (& Trait150_S150child2_trait150));
  return impl;
} 
Trait150* Trait150_S150child2_ = newTrait150_S150child2();

int Trait143_S150child1_trait143(Class* self_) {
  S150child1* self = ((S150child1*)self_);
{
    return (self -> a);
  }
} 
Trait143* newTrait143_S150child1() {
  Trait143 (* impl) = (new Trait143());
  setVec(Trait143_v, S150child1_classId, ((void*)impl));
  ((impl -> trait143) = (& Trait143_S150child1_trait143));
  return impl;
} 
Trait143* Trait143_S150child1_ = newTrait143_S150child1();

int S151_classId = Class_genId();
struct S151{

  int id;
  S151 ():id(S151_classId){

  }
};


int S151child1_classId = Class_genId();
struct S151child1:S151{

  S151child1 (int a):a(a){
    (id = S151child1_classId);
{

    }
  }
  int a;
};


struct Trait151{

  int(*trait151)(Class*);
};

Vec* Trait151_v = newVec();

int Trait151_S151child1_trait151(Class* self_) {
  S151child1* self = ((S151child1*)self_);
{
    return (self -> a);
  }
} 
Trait151* newTrait151_S151child1() {
  Trait151 (* impl) = (new Trait151());
  setVec(Trait151_v, S151child1_classId, ((void*)impl));
  ((impl -> trait151) = (& Trait151_S151child1_trait151));
  return impl;
} 
Trait151* Trait151_S151child1_ = newTrait151_S151child1();

int Trait7_S151child1_trait7(Class* self_) {
  S151child1* self = ((S151child1*)self_);
{
    return (self -> a);
  }
} 
Trait7* newTrait7_S151child1() {
  Trait7 (* impl) = (new Trait7());
  setVec(Trait7_v, S151child1_classId, ((void*)impl));
  ((impl -> trait7) = (& Trait7_S151child1_trait7));
  return impl;
} 
Trait7* Trait7_S151child1_ = newTrait7_S151child1();

int S152_classId = Class_genId();
struct S152{

  int id;
  S152 ():id(S152_classId){

  }
};


int S152child1_classId = Class_genId();
struct S152child1:S152{

  S152child1 (int a):a(a){
    (id = S152child1_classId);
{

    }
  }
  int a;
};


int S152child2_classId = Class_genId();
struct S152child2:S152{

  S152child2 (int a):a(a){
    (id = S152child2_classId);
{

    }
  }
  int a;
};


int S152child3_classId = Class_genId();
struct S152child3:S152{

  S152child3 (int a):a(a){
    (id = S152child3_classId);
{

    }
  }
  int a;
};


struct Trait152{

  int(*trait152)(Class*);
};

Vec* Trait152_v = newVec();

int Trait152_S152child1_trait152(Class* self_) {
  S152child1* self = ((S152child1*)self_);
{
    return (self -> a);
  }
} 
Trait152* newTrait152_S152child1() {
  Trait152 (* impl) = (new Trait152());
  setVec(Trait152_v, S152child1_classId, ((void*)impl));
  ((impl -> trait152) = (& Trait152_S152child1_trait152));
  return impl;
} 
Trait152* Trait152_S152child1_ = newTrait152_S152child1();

int Trait152_S152child2_trait152(Class* self_) {
  S152child2* self = ((S152child2*)self_);
{
    return (self -> a);
  }
} 
Trait152* newTrait152_S152child2() {
  Trait152 (* impl) = (new Trait152());
  setVec(Trait152_v, S152child2_classId, ((void*)impl));
  ((impl -> trait152) = (& Trait152_S152child2_trait152));
  return impl;
} 
Trait152* Trait152_S152child2_ = newTrait152_S152child2();

int Trait13_S152child1_trait13(Class* self_) {
  S152child1* self = ((S152child1*)self_);
{
    return (self -> a);
  }
} 
Trait13* newTrait13_S152child1() {
  Trait13 (* impl) = (new Trait13());
  setVec(Trait13_v, S152child1_classId, ((void*)impl));
  ((impl -> trait13) = (& Trait13_S152child1_trait13));
  return impl;
} 
Trait13* Trait13_S152child1_ = newTrait13_S152child1();

int S153_classId = Class_genId();
struct S153{

  int id;
  S153 ():id(S153_classId){

  }
};


int S153child1_classId = Class_genId();
struct S153child1:S153{

  S153child1 (int a):a(a){
    (id = S153child1_classId);
{

    }
  }
  int a;
};


int S153child2_classId = Class_genId();
struct S153child2:S153{

  S153child2 (int a):a(a){
    (id = S153child2_classId);
{

    }
  }
  int a;
};


int S153child3_classId = Class_genId();
struct S153child3:S153{

  S153child3 (int a):a(a){
    (id = S153child3_classId);
{

    }
  }
  int a;
};


struct Trait153{

  int(*trait153)(Class*);
};

Vec* Trait153_v = newVec();

int Trait153_S153child1_trait153(Class* self_) {
  S153child1* self = ((S153child1*)self_);
{
    return (self -> a);
  }
} 
Trait153* newTrait153_S153child1() {
  Trait153 (* impl) = (new Trait153());
  setVec(Trait153_v, S153child1_classId, ((void*)impl));
  ((impl -> trait153) = (& Trait153_S153child1_trait153));
  return impl;
} 
Trait153* Trait153_S153child1_ = newTrait153_S153child1();

int Trait153_S153child2_trait153(Class* self_) {
  S153child2* self = ((S153child2*)self_);
{
    return (self -> a);
  }
} 
Trait153* newTrait153_S153child2() {
  Trait153 (* impl) = (new Trait153());
  setVec(Trait153_v, S153child2_classId, ((void*)impl));
  ((impl -> trait153) = (& Trait153_S153child2_trait153));
  return impl;
} 
Trait153* Trait153_S153child2_ = newTrait153_S153child2();

int Trait153_S153child3_trait153(Class* self_) {
  S153child3* self = ((S153child3*)self_);
{
    return (self -> a);
  }
} 
Trait153* newTrait153_S153child3() {
  Trait153 (* impl) = (new Trait153());
  setVec(Trait153_v, S153child3_classId, ((void*)impl));
  ((impl -> trait153) = (& Trait153_S153child3_trait153));
  return impl;
} 
Trait153* Trait153_S153child3_ = newTrait153_S153child3();

int Trait17_S153child1_trait17(Class* self_) {
  S153child1* self = ((S153child1*)self_);
{
    return (self -> a);
  }
} 
Trait17* newTrait17_S153child1() {
  Trait17 (* impl) = (new Trait17());
  setVec(Trait17_v, S153child1_classId, ((void*)impl));
  ((impl -> trait17) = (& Trait17_S153child1_trait17));
  return impl;
} 
Trait17* Trait17_S153child1_ = newTrait17_S153child1();

int S154_classId = Class_genId();
struct S154{

  int id;
  S154 ():id(S154_classId){

  }
};


int S154child1_classId = Class_genId();
struct S154child1:S154{

  S154child1 (int a):a(a){
    (id = S154child1_classId);
{

    }
  }
  int a;
};


int S154child2_classId = Class_genId();
struct S154child2:S154{

  S154child2 (int a):a(a){
    (id = S154child2_classId);
{

    }
  }
  int a;
};


int S154child3_classId = Class_genId();
struct S154child3:S154{

  S154child3 (int a):a(a){
    (id = S154child3_classId);
{

    }
  }
  int a;
};


struct Trait154{

  int(*trait154)(Class*);
};

Vec* Trait154_v = newVec();

int Trait154_S154child1_trait154(Class* self_) {
  S154child1* self = ((S154child1*)self_);
{
    return (self -> a);
  }
} 
Trait154* newTrait154_S154child1() {
  Trait154 (* impl) = (new Trait154());
  setVec(Trait154_v, S154child1_classId, ((void*)impl));
  ((impl -> trait154) = (& Trait154_S154child1_trait154));
  return impl;
} 
Trait154* Trait154_S154child1_ = newTrait154_S154child1();

int Trait154_S154child2_trait154(Class* self_) {
  S154child2* self = ((S154child2*)self_);
{
    return (self -> a);
  }
} 
Trait154* newTrait154_S154child2() {
  Trait154 (* impl) = (new Trait154());
  setVec(Trait154_v, S154child2_classId, ((void*)impl));
  ((impl -> trait154) = (& Trait154_S154child2_trait154));
  return impl;
} 
Trait154* Trait154_S154child2_ = newTrait154_S154child2();

int Trait154_S154child3_trait154(Class* self_) {
  S154child3* self = ((S154child3*)self_);
{
    return (self -> a);
  }
} 
Trait154* newTrait154_S154child3() {
  Trait154 (* impl) = (new Trait154());
  setVec(Trait154_v, S154child3_classId, ((void*)impl));
  ((impl -> trait154) = (& Trait154_S154child3_trait154));
  return impl;
} 
Trait154* Trait154_S154child3_ = newTrait154_S154child3();

int Trait76_S154child1_trait76(Class* self_) {
  S154child1* self = ((S154child1*)self_);
{
    return (self -> a);
  }
} 
Trait76* newTrait76_S154child1() {
  Trait76 (* impl) = (new Trait76());
  setVec(Trait76_v, S154child1_classId, ((void*)impl));
  ((impl -> trait76) = (& Trait76_S154child1_trait76));
  return impl;
} 
Trait76* Trait76_S154child1_ = newTrait76_S154child1();

int S155_classId = Class_genId();
struct S155{

  int id;
  S155 ():id(S155_classId){

  }
};


int S155child1_classId = Class_genId();
struct S155child1:S155{

  S155child1 (int a):a(a){
    (id = S155child1_classId);
{

    }
  }
  int a;
};


int S155child2_classId = Class_genId();
struct S155child2:S155{

  S155child2 (int a):a(a){
    (id = S155child2_classId);
{

    }
  }
  int a;
};


struct Trait155{

  int(*trait155)(Class*);
};

Vec* Trait155_v = newVec();

int Trait155_S155child1_trait155(Class* self_) {
  S155child1* self = ((S155child1*)self_);
{
    return (self -> a);
  }
} 
Trait155* newTrait155_S155child1() {
  Trait155 (* impl) = (new Trait155());
  setVec(Trait155_v, S155child1_classId, ((void*)impl));
  ((impl -> trait155) = (& Trait155_S155child1_trait155));
  return impl;
} 
Trait155* Trait155_S155child1_ = newTrait155_S155child1();

int Trait155_S155child2_trait155(Class* self_) {
  S155child2* self = ((S155child2*)self_);
{
    return (self -> a);
  }
} 
Trait155* newTrait155_S155child2() {
  Trait155 (* impl) = (new Trait155());
  setVec(Trait155_v, S155child2_classId, ((void*)impl));
  ((impl -> trait155) = (& Trait155_S155child2_trait155));
  return impl;
} 
Trait155* Trait155_S155child2_ = newTrait155_S155child2();

int Trait52_S155child1_trait52(Class* self_) {
  S155child1* self = ((S155child1*)self_);
{
    return (self -> a);
  }
} 
Trait52* newTrait52_S155child1() {
  Trait52 (* impl) = (new Trait52());
  setVec(Trait52_v, S155child1_classId, ((void*)impl));
  ((impl -> trait52) = (& Trait52_S155child1_trait52));
  return impl;
} 
Trait52* Trait52_S155child1_ = newTrait52_S155child1();

int S156_classId = Class_genId();
struct S156{

  int id;
  S156 ():id(S156_classId){

  }
};


int S156child1_classId = Class_genId();
struct S156child1:S156{

  S156child1 (int a):a(a){
    (id = S156child1_classId);
{

    }
  }
  int a;
};


int S156child2_classId = Class_genId();
struct S156child2:S156{

  S156child2 (int a):a(a){
    (id = S156child2_classId);
{

    }
  }
  int a;
};


int S156child3_classId = Class_genId();
struct S156child3:S156{

  S156child3 (int a):a(a){
    (id = S156child3_classId);
{

    }
  }
  int a;
};


struct Trait156{

  int(*trait156)(Class*);
};

Vec* Trait156_v = newVec();

int Trait156_S156child1_trait156(Class* self_) {
  S156child1* self = ((S156child1*)self_);
{
    return (self -> a);
  }
} 
Trait156* newTrait156_S156child1() {
  Trait156 (* impl) = (new Trait156());
  setVec(Trait156_v, S156child1_classId, ((void*)impl));
  ((impl -> trait156) = (& Trait156_S156child1_trait156));
  return impl;
} 
Trait156* Trait156_S156child1_ = newTrait156_S156child1();

int Trait156_S156child2_trait156(Class* self_) {
  S156child2* self = ((S156child2*)self_);
{
    return (self -> a);
  }
} 
Trait156* newTrait156_S156child2() {
  Trait156 (* impl) = (new Trait156());
  setVec(Trait156_v, S156child2_classId, ((void*)impl));
  ((impl -> trait156) = (& Trait156_S156child2_trait156));
  return impl;
} 
Trait156* Trait156_S156child2_ = newTrait156_S156child2();

int Trait14_S156child1_trait14(Class* self_) {
  S156child1* self = ((S156child1*)self_);
{
    return (self -> a);
  }
} 
Trait14* newTrait14_S156child1() {
  Trait14 (* impl) = (new Trait14());
  setVec(Trait14_v, S156child1_classId, ((void*)impl));
  ((impl -> trait14) = (& Trait14_S156child1_trait14));
  return impl;
} 
Trait14* Trait14_S156child1_ = newTrait14_S156child1();

int S157_classId = Class_genId();
struct S157{

  int id;
  S157 ():id(S157_classId){

  }
};


int S157child1_classId = Class_genId();
struct S157child1:S157{

  S157child1 (int a):a(a){
    (id = S157child1_classId);
{

    }
  }
  int a;
};


int S157child2_classId = Class_genId();
struct S157child2:S157{

  S157child2 (int a):a(a){
    (id = S157child2_classId);
{

    }
  }
  int a;
};


int S157child3_classId = Class_genId();
struct S157child3:S157{

  S157child3 (int a):a(a){
    (id = S157child3_classId);
{

    }
  }
  int a;
};


struct Trait157{

  int(*trait157)(Class*);
};

Vec* Trait157_v = newVec();

int Trait157_S157child1_trait157(Class* self_) {
  S157child1* self = ((S157child1*)self_);
{
    return (self -> a);
  }
} 
Trait157* newTrait157_S157child1() {
  Trait157 (* impl) = (new Trait157());
  setVec(Trait157_v, S157child1_classId, ((void*)impl));
  ((impl -> trait157) = (& Trait157_S157child1_trait157));
  return impl;
} 
Trait157* Trait157_S157child1_ = newTrait157_S157child1();

int Trait157_S157child2_trait157(Class* self_) {
  S157child2* self = ((S157child2*)self_);
{
    return (self -> a);
  }
} 
Trait157* newTrait157_S157child2() {
  Trait157 (* impl) = (new Trait157());
  setVec(Trait157_v, S157child2_classId, ((void*)impl));
  ((impl -> trait157) = (& Trait157_S157child2_trait157));
  return impl;
} 
Trait157* Trait157_S157child2_ = newTrait157_S157child2();

int Trait95_S157child1_trait95(Class* self_) {
  S157child1* self = ((S157child1*)self_);
{
    return (self -> a);
  }
} 
Trait95* newTrait95_S157child1() {
  Trait95 (* impl) = (new Trait95());
  setVec(Trait95_v, S157child1_classId, ((void*)impl));
  ((impl -> trait95) = (& Trait95_S157child1_trait95));
  return impl;
} 
Trait95* Trait95_S157child1_ = newTrait95_S157child1();

int S158_classId = Class_genId();
struct S158{

  int id;
  S158 ():id(S158_classId){

  }
};


int S158child1_classId = Class_genId();
struct S158child1:S158{

  S158child1 (int a):a(a){
    (id = S158child1_classId);
{

    }
  }
  int a;
};


int S158child2_classId = Class_genId();
struct S158child2:S158{

  S158child2 (int a):a(a){
    (id = S158child2_classId);
{

    }
  }
  int a;
};


int S158child3_classId = Class_genId();
struct S158child3:S158{

  S158child3 (int a):a(a){
    (id = S158child3_classId);
{

    }
  }
  int a;
};


struct Trait158{

  int(*trait158)(Class*);
};

Vec* Trait158_v = newVec();

int Trait158_S158child1_trait158(Class* self_) {
  S158child1* self = ((S158child1*)self_);
{
    return (self -> a);
  }
} 
Trait158* newTrait158_S158child1() {
  Trait158 (* impl) = (new Trait158());
  setVec(Trait158_v, S158child1_classId, ((void*)impl));
  ((impl -> trait158) = (& Trait158_S158child1_trait158));
  return impl;
} 
Trait158* Trait158_S158child1_ = newTrait158_S158child1();

int Trait158_S158child2_trait158(Class* self_) {
  S158child2* self = ((S158child2*)self_);
{
    return (self -> a);
  }
} 
Trait158* newTrait158_S158child2() {
  Trait158 (* impl) = (new Trait158());
  setVec(Trait158_v, S158child2_classId, ((void*)impl));
  ((impl -> trait158) = (& Trait158_S158child2_trait158));
  return impl;
} 
Trait158* Trait158_S158child2_ = newTrait158_S158child2();

int Trait158_S158child3_trait158(Class* self_) {
  S158child3* self = ((S158child3*)self_);
{
    return (self -> a);
  }
} 
Trait158* newTrait158_S158child3() {
  Trait158 (* impl) = (new Trait158());
  setVec(Trait158_v, S158child3_classId, ((void*)impl));
  ((impl -> trait158) = (& Trait158_S158child3_trait158));
  return impl;
} 
Trait158* Trait158_S158child3_ = newTrait158_S158child3();

int Trait110_S158child1_trait110(Class* self_) {
  S158child1* self = ((S158child1*)self_);
{
    return (self -> a);
  }
} 
Trait110* newTrait110_S158child1() {
  Trait110 (* impl) = (new Trait110());
  setVec(Trait110_v, S158child1_classId, ((void*)impl));
  ((impl -> trait110) = (& Trait110_S158child1_trait110));
  return impl;
} 
Trait110* Trait110_S158child1_ = newTrait110_S158child1();

int S159_classId = Class_genId();
struct S159{

  int id;
  S159 ():id(S159_classId){

  }
};


int S159child1_classId = Class_genId();
struct S159child1:S159{

  S159child1 (int a):a(a){
    (id = S159child1_classId);
{

    }
  }
  int a;
};


int S159child2_classId = Class_genId();
struct S159child2:S159{

  S159child2 (int a):a(a){
    (id = S159child2_classId);
{

    }
  }
  int a;
};


int S159child3_classId = Class_genId();
struct S159child3:S159{

  S159child3 (int a):a(a){
    (id = S159child3_classId);
{

    }
  }
  int a;
};


int S159child4_classId = Class_genId();
struct S159child4:S159{

  S159child4 (int a):a(a){
    (id = S159child4_classId);
{

    }
  }
  int a;
};


struct Trait159{

  int(*trait159)(Class*);
};

Vec* Trait159_v = newVec();

int Trait159_S159child1_trait159(Class* self_) {
  S159child1* self = ((S159child1*)self_);
{
    return (self -> a);
  }
} 
Trait159* newTrait159_S159child1() {
  Trait159 (* impl) = (new Trait159());
  setVec(Trait159_v, S159child1_classId, ((void*)impl));
  ((impl -> trait159) = (& Trait159_S159child1_trait159));
  return impl;
} 
Trait159* Trait159_S159child1_ = newTrait159_S159child1();

int Trait150_S159child1_trait150(Class* self_) {
  S159child1* self = ((S159child1*)self_);
{
    return (self -> a);
  }
} 
Trait150* newTrait150_S159child1() {
  Trait150 (* impl) = (new Trait150());
  setVec(Trait150_v, S159child1_classId, ((void*)impl));
  ((impl -> trait150) = (& Trait150_S159child1_trait150));
  return impl;
} 
Trait150* Trait150_S159child1_ = newTrait150_S159child1();

int S160_classId = Class_genId();
struct S160{

  int id;
  S160 ():id(S160_classId){

  }
};


int S160child1_classId = Class_genId();
struct S160child1:S160{

  S160child1 (int a):a(a){
    (id = S160child1_classId);
{

    }
  }
  int a;
};


int S160child2_classId = Class_genId();
struct S160child2:S160{

  S160child2 (int a):a(a){
    (id = S160child2_classId);
{

    }
  }
  int a;
};


int S160child3_classId = Class_genId();
struct S160child3:S160{

  S160child3 (int a):a(a){
    (id = S160child3_classId);
{

    }
  }
  int a;
};


int S160child4_classId = Class_genId();
struct S160child4:S160{

  S160child4 (int a):a(a){
    (id = S160child4_classId);
{

    }
  }
  int a;
};


struct Trait160{

  int(*trait160)(Class*);
};

Vec* Trait160_v = newVec();

int Trait160_S160child1_trait160(Class* self_) {
  S160child1* self = ((S160child1*)self_);
{
    return (self -> a);
  }
} 
Trait160* newTrait160_S160child1() {
  Trait160 (* impl) = (new Trait160());
  setVec(Trait160_v, S160child1_classId, ((void*)impl));
  ((impl -> trait160) = (& Trait160_S160child1_trait160));
  return impl;
} 
Trait160* Trait160_S160child1_ = newTrait160_S160child1();

int Trait8_S160child1_trait8(Class* self_) {
  S160child1* self = ((S160child1*)self_);
{
    return (self -> a);
  }
} 
Trait8* newTrait8_S160child1() {
  Trait8 (* impl) = (new Trait8());
  setVec(Trait8_v, S160child1_classId, ((void*)impl));
  ((impl -> trait8) = (& Trait8_S160child1_trait8));
  return impl;
} 
Trait8* Trait8_S160child1_ = newTrait8_S160child1();

int S161_classId = Class_genId();
struct S161{

  int id;
  S161 ():id(S161_classId){

  }
};


int S161child1_classId = Class_genId();
struct S161child1:S161{

  S161child1 (int a):a(a){
    (id = S161child1_classId);
{

    }
  }
  int a;
};


int S161child2_classId = Class_genId();
struct S161child2:S161{

  S161child2 (int a):a(a){
    (id = S161child2_classId);
{

    }
  }
  int a;
};


int S161child3_classId = Class_genId();
struct S161child3:S161{

  S161child3 (int a):a(a){
    (id = S161child3_classId);
{

    }
  }
  int a;
};


int S161child4_classId = Class_genId();
struct S161child4:S161{

  S161child4 (int a):a(a){
    (id = S161child4_classId);
{

    }
  }
  int a;
};


struct Trait161{

  int(*trait161)(Class*);
};

Vec* Trait161_v = newVec();

int Trait161_S161child1_trait161(Class* self_) {
  S161child1* self = ((S161child1*)self_);
{
    return (self -> a);
  }
} 
Trait161* newTrait161_S161child1() {
  Trait161 (* impl) = (new Trait161());
  setVec(Trait161_v, S161child1_classId, ((void*)impl));
  ((impl -> trait161) = (& Trait161_S161child1_trait161));
  return impl;
} 
Trait161* Trait161_S161child1_ = newTrait161_S161child1();

int Trait161_S161child2_trait161(Class* self_) {
  S161child2* self = ((S161child2*)self_);
{
    return (self -> a);
  }
} 
Trait161* newTrait161_S161child2() {
  Trait161 (* impl) = (new Trait161());
  setVec(Trait161_v, S161child2_classId, ((void*)impl));
  ((impl -> trait161) = (& Trait161_S161child2_trait161));
  return impl;
} 
Trait161* Trait161_S161child2_ = newTrait161_S161child2();

int Trait135_S161child1_trait135(Class* self_) {
  S161child1* self = ((S161child1*)self_);
{
    return (self -> a);
  }
} 
Trait135* newTrait135_S161child1() {
  Trait135 (* impl) = (new Trait135());
  setVec(Trait135_v, S161child1_classId, ((void*)impl));
  ((impl -> trait135) = (& Trait135_S161child1_trait135));
  return impl;
} 
Trait135* Trait135_S161child1_ = newTrait135_S161child1();

int S162_classId = Class_genId();
struct S162{

  int id;
  S162 ():id(S162_classId){

  }
};


int S162child1_classId = Class_genId();
struct S162child1:S162{

  S162child1 (int a):a(a){
    (id = S162child1_classId);
{

    }
  }
  int a;
};


int S162child2_classId = Class_genId();
struct S162child2:S162{

  S162child2 (int a):a(a){
    (id = S162child2_classId);
{

    }
  }
  int a;
};


struct Trait162{

  int(*trait162)(Class*);
};

Vec* Trait162_v = newVec();

int Trait162_S162child1_trait162(Class* self_) {
  S162child1* self = ((S162child1*)self_);
{
    return (self -> a);
  }
} 
Trait162* newTrait162_S162child1() {
  Trait162 (* impl) = (new Trait162());
  setVec(Trait162_v, S162child1_classId, ((void*)impl));
  ((impl -> trait162) = (& Trait162_S162child1_trait162));
  return impl;
} 
Trait162* Trait162_S162child1_ = newTrait162_S162child1();

int Trait6_S162child1_trait6(Class* self_) {
  S162child1* self = ((S162child1*)self_);
{
    return (self -> a);
  }
} 
Trait6* newTrait6_S162child1() {
  Trait6 (* impl) = (new Trait6());
  setVec(Trait6_v, S162child1_classId, ((void*)impl));
  ((impl -> trait6) = (& Trait6_S162child1_trait6));
  return impl;
} 
Trait6* Trait6_S162child1_ = newTrait6_S162child1();

int S163_classId = Class_genId();
struct S163{

  int id;
  S163 ():id(S163_classId){

  }
};


int S163child1_classId = Class_genId();
struct S163child1:S163{

  S163child1 (int a):a(a){
    (id = S163child1_classId);
{

    }
  }
  int a;
};


int S163child2_classId = Class_genId();
struct S163child2:S163{

  S163child2 (int a):a(a){
    (id = S163child2_classId);
{

    }
  }
  int a;
};


int S163child3_classId = Class_genId();
struct S163child3:S163{

  S163child3 (int a):a(a){
    (id = S163child3_classId);
{

    }
  }
  int a;
};


int S163child4_classId = Class_genId();
struct S163child4:S163{

  S163child4 (int a):a(a){
    (id = S163child4_classId);
{

    }
  }
  int a;
};


int S163child5_classId = Class_genId();
struct S163child5:S163{

  S163child5 (int a):a(a){
    (id = S163child5_classId);
{

    }
  }
  int a;
};


struct Trait163{

  int(*trait163)(Class*);
};

Vec* Trait163_v = newVec();

int Trait163_S163child1_trait163(Class* self_) {
  S163child1* self = ((S163child1*)self_);
{
    return (self -> a);
  }
} 
Trait163* newTrait163_S163child1() {
  Trait163 (* impl) = (new Trait163());
  setVec(Trait163_v, S163child1_classId, ((void*)impl));
  ((impl -> trait163) = (& Trait163_S163child1_trait163));
  return impl;
} 
Trait163* Trait163_S163child1_ = newTrait163_S163child1();

int Trait163_S163child2_trait163(Class* self_) {
  S163child2* self = ((S163child2*)self_);
{
    return (self -> a);
  }
} 
Trait163* newTrait163_S163child2() {
  Trait163 (* impl) = (new Trait163());
  setVec(Trait163_v, S163child2_classId, ((void*)impl));
  ((impl -> trait163) = (& Trait163_S163child2_trait163));
  return impl;
} 
Trait163* Trait163_S163child2_ = newTrait163_S163child2();

int Trait28_S163child1_trait28(Class* self_) {
  S163child1* self = ((S163child1*)self_);
{
    return (self -> a);
  }
} 
Trait28* newTrait28_S163child1() {
  Trait28 (* impl) = (new Trait28());
  setVec(Trait28_v, S163child1_classId, ((void*)impl));
  ((impl -> trait28) = (& Trait28_S163child1_trait28));
  return impl;
} 
Trait28* Trait28_S163child1_ = newTrait28_S163child1();

int S164_classId = Class_genId();
struct S164{

  int id;
  S164 ():id(S164_classId){

  }
};


int S164child1_classId = Class_genId();
struct S164child1:S164{

  S164child1 (int a):a(a){
    (id = S164child1_classId);
{

    }
  }
  int a;
};


struct Trait164{

  int(*trait164)(Class*);
};

Vec* Trait164_v = newVec();

int Trait164_S164child1_trait164(Class* self_) {
  S164child1* self = ((S164child1*)self_);
{
    return (self -> a);
  }
} 
Trait164* newTrait164_S164child1() {
  Trait164 (* impl) = (new Trait164());
  setVec(Trait164_v, S164child1_classId, ((void*)impl));
  ((impl -> trait164) = (& Trait164_S164child1_trait164));
  return impl;
} 
Trait164* Trait164_S164child1_ = newTrait164_S164child1();

int Trait23_S164child1_trait23(Class* self_) {
  S164child1* self = ((S164child1*)self_);
{
    return (self -> a);
  }
} 
Trait23* newTrait23_S164child1() {
  Trait23 (* impl) = (new Trait23());
  setVec(Trait23_v, S164child1_classId, ((void*)impl));
  ((impl -> trait23) = (& Trait23_S164child1_trait23));
  return impl;
} 
Trait23* Trait23_S164child1_ = newTrait23_S164child1();

int S165_classId = Class_genId();
struct S165{

  int id;
  S165 ():id(S165_classId){

  }
};


int S165child1_classId = Class_genId();
struct S165child1:S165{

  S165child1 (int a):a(a){
    (id = S165child1_classId);
{

    }
  }
  int a;
};


int S165child2_classId = Class_genId();
struct S165child2:S165{

  S165child2 (int a):a(a){
    (id = S165child2_classId);
{

    }
  }
  int a;
};


struct Trait165{

  int(*trait165)(Class*);
};

Vec* Trait165_v = newVec();

int Trait165_S165child1_trait165(Class* self_) {
  S165child1* self = ((S165child1*)self_);
{
    return (self -> a);
  }
} 
Trait165* newTrait165_S165child1() {
  Trait165 (* impl) = (new Trait165());
  setVec(Trait165_v, S165child1_classId, ((void*)impl));
  ((impl -> trait165) = (& Trait165_S165child1_trait165));
  return impl;
} 
Trait165* Trait165_S165child1_ = newTrait165_S165child1();

int Trait165_S165child2_trait165(Class* self_) {
  S165child2* self = ((S165child2*)self_);
{
    return (self -> a);
  }
} 
Trait165* newTrait165_S165child2() {
  Trait165 (* impl) = (new Trait165());
  setVec(Trait165_v, S165child2_classId, ((void*)impl));
  ((impl -> trait165) = (& Trait165_S165child2_trait165));
  return impl;
} 
Trait165* Trait165_S165child2_ = newTrait165_S165child2();

int Trait33_S165child1_trait33(Class* self_) {
  S165child1* self = ((S165child1*)self_);
{
    return (self -> a);
  }
} 
Trait33* newTrait33_S165child1() {
  Trait33 (* impl) = (new Trait33());
  setVec(Trait33_v, S165child1_classId, ((void*)impl));
  ((impl -> trait33) = (& Trait33_S165child1_trait33));
  return impl;
} 
Trait33* Trait33_S165child1_ = newTrait33_S165child1();

int S166_classId = Class_genId();
struct S166{

  int id;
  S166 ():id(S166_classId){

  }
};


int S166child1_classId = Class_genId();
struct S166child1:S166{

  S166child1 (int a):a(a){
    (id = S166child1_classId);
{

    }
  }
  int a;
};


struct Trait166{

  int(*trait166)(Class*);
};

Vec* Trait166_v = newVec();

int Trait166_S166child1_trait166(Class* self_) {
  S166child1* self = ((S166child1*)self_);
{
    return (self -> a);
  }
} 
Trait166* newTrait166_S166child1() {
  Trait166 (* impl) = (new Trait166());
  setVec(Trait166_v, S166child1_classId, ((void*)impl));
  ((impl -> trait166) = (& Trait166_S166child1_trait166));
  return impl;
} 
Trait166* Trait166_S166child1_ = newTrait166_S166child1();

int Trait102_S166child1_trait102(Class* self_) {
  S166child1* self = ((S166child1*)self_);
{
    return (self -> a);
  }
} 
Trait102* newTrait102_S166child1() {
  Trait102 (* impl) = (new Trait102());
  setVec(Trait102_v, S166child1_classId, ((void*)impl));
  ((impl -> trait102) = (& Trait102_S166child1_trait102));
  return impl;
} 
Trait102* Trait102_S166child1_ = newTrait102_S166child1();

int S167_classId = Class_genId();
struct S167{

  int id;
  S167 ():id(S167_classId){

  }
};


int S167child1_classId = Class_genId();
struct S167child1:S167{

  S167child1 (int a):a(a){
    (id = S167child1_classId);
{

    }
  }
  int a;
};


int S167child2_classId = Class_genId();
struct S167child2:S167{

  S167child2 (int a):a(a){
    (id = S167child2_classId);
{

    }
  }
  int a;
};


int S167child3_classId = Class_genId();
struct S167child3:S167{

  S167child3 (int a):a(a){
    (id = S167child3_classId);
{

    }
  }
  int a;
};


struct Trait167{

  int(*trait167)(Class*);
};

Vec* Trait167_v = newVec();

int Trait167_S167child1_trait167(Class* self_) {
  S167child1* self = ((S167child1*)self_);
{
    return (self -> a);
  }
} 
Trait167* newTrait167_S167child1() {
  Trait167 (* impl) = (new Trait167());
  setVec(Trait167_v, S167child1_classId, ((void*)impl));
  ((impl -> trait167) = (& Trait167_S167child1_trait167));
  return impl;
} 
Trait167* Trait167_S167child1_ = newTrait167_S167child1();

int Trait5_S167child1_trait5(Class* self_) {
  S167child1* self = ((S167child1*)self_);
{
    return (self -> a);
  }
} 
Trait5* newTrait5_S167child1() {
  Trait5 (* impl) = (new Trait5());
  setVec(Trait5_v, S167child1_classId, ((void*)impl));
  ((impl -> trait5) = (& Trait5_S167child1_trait5));
  return impl;
} 
Trait5* Trait5_S167child1_ = newTrait5_S167child1();

int S168_classId = Class_genId();
struct S168{

  int id;
  S168 ():id(S168_classId){

  }
};


int S168child1_classId = Class_genId();
struct S168child1:S168{

  S168child1 (int a):a(a){
    (id = S168child1_classId);
{

    }
  }
  int a;
};


struct Trait168{

  int(*trait168)(Class*);
};

Vec* Trait168_v = newVec();

int Trait168_S168child1_trait168(Class* self_) {
  S168child1* self = ((S168child1*)self_);
{
    return (self -> a);
  }
} 
Trait168* newTrait168_S168child1() {
  Trait168 (* impl) = (new Trait168());
  setVec(Trait168_v, S168child1_classId, ((void*)impl));
  ((impl -> trait168) = (& Trait168_S168child1_trait168));
  return impl;
} 
Trait168* Trait168_S168child1_ = newTrait168_S168child1();

int Trait166_S168child1_trait166(Class* self_) {
  S168child1* self = ((S168child1*)self_);
{
    return (self -> a);
  }
} 
Trait166* newTrait166_S168child1() {
  Trait166 (* impl) = (new Trait166());
  setVec(Trait166_v, S168child1_classId, ((void*)impl));
  ((impl -> trait166) = (& Trait166_S168child1_trait166));
  return impl;
} 
Trait166* Trait166_S168child1_ = newTrait166_S168child1();

int S169_classId = Class_genId();
struct S169{

  int id;
  S169 ():id(S169_classId){

  }
};


int S169child1_classId = Class_genId();
struct S169child1:S169{

  S169child1 (int a):a(a){
    (id = S169child1_classId);
{

    }
  }
  int a;
};


int S169child2_classId = Class_genId();
struct S169child2:S169{

  S169child2 (int a):a(a){
    (id = S169child2_classId);
{

    }
  }
  int a;
};


int S169child3_classId = Class_genId();
struct S169child3:S169{

  S169child3 (int a):a(a){
    (id = S169child3_classId);
{

    }
  }
  int a;
};


int S169child4_classId = Class_genId();
struct S169child4:S169{

  S169child4 (int a):a(a){
    (id = S169child4_classId);
{

    }
  }
  int a;
};


struct Trait169{

  int(*trait169)(Class*);
};

Vec* Trait169_v = newVec();

int Trait169_S169child1_trait169(Class* self_) {
  S169child1* self = ((S169child1*)self_);
{
    return (self -> a);
  }
} 
Trait169* newTrait169_S169child1() {
  Trait169 (* impl) = (new Trait169());
  setVec(Trait169_v, S169child1_classId, ((void*)impl));
  ((impl -> trait169) = (& Trait169_S169child1_trait169));
  return impl;
} 
Trait169* Trait169_S169child1_ = newTrait169_S169child1();

int Trait28_S169child1_trait28(Class* self_) {
  S169child1* self = ((S169child1*)self_);
{
    return (self -> a);
  }
} 
Trait28* newTrait28_S169child1() {
  Trait28 (* impl) = (new Trait28());
  setVec(Trait28_v, S169child1_classId, ((void*)impl));
  ((impl -> trait28) = (& Trait28_S169child1_trait28));
  return impl;
} 
Trait28* Trait28_S169child1_ = newTrait28_S169child1();

int S170_classId = Class_genId();
struct S170{

  int id;
  S170 ():id(S170_classId){

  }
};


int S170child1_classId = Class_genId();
struct S170child1:S170{

  S170child1 (int a):a(a){
    (id = S170child1_classId);
{

    }
  }
  int a;
};


int S170child2_classId = Class_genId();
struct S170child2:S170{

  S170child2 (int a):a(a){
    (id = S170child2_classId);
{

    }
  }
  int a;
};


struct Trait170{

  int(*trait170)(Class*);
};

Vec* Trait170_v = newVec();

int Trait170_S170child1_trait170(Class* self_) {
  S170child1* self = ((S170child1*)self_);
{
    return (self -> a);
  }
} 
Trait170* newTrait170_S170child1() {
  Trait170 (* impl) = (new Trait170());
  setVec(Trait170_v, S170child1_classId, ((void*)impl));
  ((impl -> trait170) = (& Trait170_S170child1_trait170));
  return impl;
} 
Trait170* Trait170_S170child1_ = newTrait170_S170child1();

int Trait78_S170child1_trait78(Class* self_) {
  S170child1* self = ((S170child1*)self_);
{
    return (self -> a);
  }
} 
Trait78* newTrait78_S170child1() {
  Trait78 (* impl) = (new Trait78());
  setVec(Trait78_v, S170child1_classId, ((void*)impl));
  ((impl -> trait78) = (& Trait78_S170child1_trait78));
  return impl;
} 
Trait78* Trait78_S170child1_ = newTrait78_S170child1();

int S171_classId = Class_genId();
struct S171{

  int id;
  S171 ():id(S171_classId){

  }
};


int S171child1_classId = Class_genId();
struct S171child1:S171{

  S171child1 (int a):a(a){
    (id = S171child1_classId);
{

    }
  }
  int a;
};


int S171child2_classId = Class_genId();
struct S171child2:S171{

  S171child2 (int a):a(a){
    (id = S171child2_classId);
{

    }
  }
  int a;
};


int S171child3_classId = Class_genId();
struct S171child3:S171{

  S171child3 (int a):a(a){
    (id = S171child3_classId);
{

    }
  }
  int a;
};


int S171child4_classId = Class_genId();
struct S171child4:S171{

  S171child4 (int a):a(a){
    (id = S171child4_classId);
{

    }
  }
  int a;
};


struct Trait171{

  int(*trait171)(Class*);
};

Vec* Trait171_v = newVec();

int Trait171_S171child1_trait171(Class* self_) {
  S171child1* self = ((S171child1*)self_);
{
    return (self -> a);
  }
} 
Trait171* newTrait171_S171child1() {
  Trait171 (* impl) = (new Trait171());
  setVec(Trait171_v, S171child1_classId, ((void*)impl));
  ((impl -> trait171) = (& Trait171_S171child1_trait171));
  return impl;
} 
Trait171* Trait171_S171child1_ = newTrait171_S171child1();

int Trait171_S171child2_trait171(Class* self_) {
  S171child2* self = ((S171child2*)self_);
{
    return (self -> a);
  }
} 
Trait171* newTrait171_S171child2() {
  Trait171 (* impl) = (new Trait171());
  setVec(Trait171_v, S171child2_classId, ((void*)impl));
  ((impl -> trait171) = (& Trait171_S171child2_trait171));
  return impl;
} 
Trait171* Trait171_S171child2_ = newTrait171_S171child2();

int Trait171_S171child3_trait171(Class* self_) {
  S171child3* self = ((S171child3*)self_);
{
    return (self -> a);
  }
} 
Trait171* newTrait171_S171child3() {
  Trait171 (* impl) = (new Trait171());
  setVec(Trait171_v, S171child3_classId, ((void*)impl));
  ((impl -> trait171) = (& Trait171_S171child3_trait171));
  return impl;
} 
Trait171* Trait171_S171child3_ = newTrait171_S171child3();

int Trait128_S171child1_trait128(Class* self_) {
  S171child1* self = ((S171child1*)self_);
{
    return (self -> a);
  }
} 
Trait128* newTrait128_S171child1() {
  Trait128 (* impl) = (new Trait128());
  setVec(Trait128_v, S171child1_classId, ((void*)impl));
  ((impl -> trait128) = (& Trait128_S171child1_trait128));
  return impl;
} 
Trait128* Trait128_S171child1_ = newTrait128_S171child1();

int S172_classId = Class_genId();
struct S172{

  int id;
  S172 ():id(S172_classId){

  }
};


int S172child1_classId = Class_genId();
struct S172child1:S172{

  S172child1 (int a):a(a){
    (id = S172child1_classId);
{

    }
  }
  int a;
};


int S172child2_classId = Class_genId();
struct S172child2:S172{

  S172child2 (int a):a(a){
    (id = S172child2_classId);
{

    }
  }
  int a;
};


int S172child3_classId = Class_genId();
struct S172child3:S172{

  S172child3 (int a):a(a){
    (id = S172child3_classId);
{

    }
  }
  int a;
};


struct Trait172{

  int(*trait172)(Class*);
};

Vec* Trait172_v = newVec();

int Trait172_S172child1_trait172(Class* self_) {
  S172child1* self = ((S172child1*)self_);
{
    return (self -> a);
  }
} 
Trait172* newTrait172_S172child1() {
  Trait172 (* impl) = (new Trait172());
  setVec(Trait172_v, S172child1_classId, ((void*)impl));
  ((impl -> trait172) = (& Trait172_S172child1_trait172));
  return impl;
} 
Trait172* Trait172_S172child1_ = newTrait172_S172child1();

int Trait172_S172child2_trait172(Class* self_) {
  S172child2* self = ((S172child2*)self_);
{
    return (self -> a);
  }
} 
Trait172* newTrait172_S172child2() {
  Trait172 (* impl) = (new Trait172());
  setVec(Trait172_v, S172child2_classId, ((void*)impl));
  ((impl -> trait172) = (& Trait172_S172child2_trait172));
  return impl;
} 
Trait172* Trait172_S172child2_ = newTrait172_S172child2();

int Trait130_S172child1_trait130(Class* self_) {
  S172child1* self = ((S172child1*)self_);
{
    return (self -> a);
  }
} 
Trait130* newTrait130_S172child1() {
  Trait130 (* impl) = (new Trait130());
  setVec(Trait130_v, S172child1_classId, ((void*)impl));
  ((impl -> trait130) = (& Trait130_S172child1_trait130));
  return impl;
} 
Trait130* Trait130_S172child1_ = newTrait130_S172child1();

int S173_classId = Class_genId();
struct S173{

  int id;
  S173 ():id(S173_classId){

  }
};


int S173child1_classId = Class_genId();
struct S173child1:S173{

  S173child1 (int a):a(a){
    (id = S173child1_classId);
{

    }
  }
  int a;
};


int S173child2_classId = Class_genId();
struct S173child2:S173{

  S173child2 (int a):a(a){
    (id = S173child2_classId);
{

    }
  }
  int a;
};


int S173child3_classId = Class_genId();
struct S173child3:S173{

  S173child3 (int a):a(a){
    (id = S173child3_classId);
{

    }
  }
  int a;
};


struct Trait173{

  int(*trait173)(Class*);
};

Vec* Trait173_v = newVec();

int Trait173_S173child1_trait173(Class* self_) {
  S173child1* self = ((S173child1*)self_);
{
    return (self -> a);
  }
} 
Trait173* newTrait173_S173child1() {
  Trait173 (* impl) = (new Trait173());
  setVec(Trait173_v, S173child1_classId, ((void*)impl));
  ((impl -> trait173) = (& Trait173_S173child1_trait173));
  return impl;
} 
Trait173* Trait173_S173child1_ = newTrait173_S173child1();

int Trait173_S173child2_trait173(Class* self_) {
  S173child2* self = ((S173child2*)self_);
{
    return (self -> a);
  }
} 
Trait173* newTrait173_S173child2() {
  Trait173 (* impl) = (new Trait173());
  setVec(Trait173_v, S173child2_classId, ((void*)impl));
  ((impl -> trait173) = (& Trait173_S173child2_trait173));
  return impl;
} 
Trait173* Trait173_S173child2_ = newTrait173_S173child2();

int Trait79_S173child1_trait79(Class* self_) {
  S173child1* self = ((S173child1*)self_);
{
    return (self -> a);
  }
} 
Trait79* newTrait79_S173child1() {
  Trait79 (* impl) = (new Trait79());
  setVec(Trait79_v, S173child1_classId, ((void*)impl));
  ((impl -> trait79) = (& Trait79_S173child1_trait79));
  return impl;
} 
Trait79* Trait79_S173child1_ = newTrait79_S173child1();

int S174_classId = Class_genId();
struct S174{

  int id;
  S174 ():id(S174_classId){

  }
};


int S174child1_classId = Class_genId();
struct S174child1:S174{

  S174child1 (int a):a(a){
    (id = S174child1_classId);
{

    }
  }
  int a;
};


int S174child2_classId = Class_genId();
struct S174child2:S174{

  S174child2 (int a):a(a){
    (id = S174child2_classId);
{

    }
  }
  int a;
};


int S174child3_classId = Class_genId();
struct S174child3:S174{

  S174child3 (int a):a(a){
    (id = S174child3_classId);
{

    }
  }
  int a;
};


struct Trait174{

  int(*trait174)(Class*);
};

Vec* Trait174_v = newVec();

int Trait174_S174child1_trait174(Class* self_) {
  S174child1* self = ((S174child1*)self_);
{
    return (self -> a);
  }
} 
Trait174* newTrait174_S174child1() {
  Trait174 (* impl) = (new Trait174());
  setVec(Trait174_v, S174child1_classId, ((void*)impl));
  ((impl -> trait174) = (& Trait174_S174child1_trait174));
  return impl;
} 
Trait174* Trait174_S174child1_ = newTrait174_S174child1();

int Trait174_S174child2_trait174(Class* self_) {
  S174child2* self = ((S174child2*)self_);
{
    return (self -> a);
  }
} 
Trait174* newTrait174_S174child2() {
  Trait174 (* impl) = (new Trait174());
  setVec(Trait174_v, S174child2_classId, ((void*)impl));
  ((impl -> trait174) = (& Trait174_S174child2_trait174));
  return impl;
} 
Trait174* Trait174_S174child2_ = newTrait174_S174child2();

int Trait174_S174child3_trait174(Class* self_) {
  S174child3* self = ((S174child3*)self_);
{
    return (self -> a);
  }
} 
Trait174* newTrait174_S174child3() {
  Trait174 (* impl) = (new Trait174());
  setVec(Trait174_v, S174child3_classId, ((void*)impl));
  ((impl -> trait174) = (& Trait174_S174child3_trait174));
  return impl;
} 
Trait174* Trait174_S174child3_ = newTrait174_S174child3();

int Trait126_S174child1_trait126(Class* self_) {
  S174child1* self = ((S174child1*)self_);
{
    return (self -> a);
  }
} 
Trait126* newTrait126_S174child1() {
  Trait126 (* impl) = (new Trait126());
  setVec(Trait126_v, S174child1_classId, ((void*)impl));
  ((impl -> trait126) = (& Trait126_S174child1_trait126));
  return impl;
} 
Trait126* Trait126_S174child1_ = newTrait126_S174child1();

int S175_classId = Class_genId();
struct S175{

  int id;
  S175 ():id(S175_classId){

  }
};


int S175child1_classId = Class_genId();
struct S175child1:S175{

  S175child1 (int a):a(a){
    (id = S175child1_classId);
{

    }
  }
  int a;
};


int S175child2_classId = Class_genId();
struct S175child2:S175{

  S175child2 (int a):a(a){
    (id = S175child2_classId);
{

    }
  }
  int a;
};


struct Trait175{

  int(*trait175)(Class*);
};

Vec* Trait175_v = newVec();

int Trait175_S175child1_trait175(Class* self_) {
  S175child1* self = ((S175child1*)self_);
{
    return (self -> a);
  }
} 
Trait175* newTrait175_S175child1() {
  Trait175 (* impl) = (new Trait175());
  setVec(Trait175_v, S175child1_classId, ((void*)impl));
  ((impl -> trait175) = (& Trait175_S175child1_trait175));
  return impl;
} 
Trait175* Trait175_S175child1_ = newTrait175_S175child1();

int Trait175_S175child2_trait175(Class* self_) {
  S175child2* self = ((S175child2*)self_);
{
    return (self -> a);
  }
} 
Trait175* newTrait175_S175child2() {
  Trait175 (* impl) = (new Trait175());
  setVec(Trait175_v, S175child2_classId, ((void*)impl));
  ((impl -> trait175) = (& Trait175_S175child2_trait175));
  return impl;
} 
Trait175* Trait175_S175child2_ = newTrait175_S175child2();

int Trait161_S175child1_trait161(Class* self_) {
  S175child1* self = ((S175child1*)self_);
{
    return (self -> a);
  }
} 
Trait161* newTrait161_S175child1() {
  Trait161 (* impl) = (new Trait161());
  setVec(Trait161_v, S175child1_classId, ((void*)impl));
  ((impl -> trait161) = (& Trait161_S175child1_trait161));
  return impl;
} 
Trait161* Trait161_S175child1_ = newTrait161_S175child1();

int S176_classId = Class_genId();
struct S176{

  int id;
  S176 ():id(S176_classId){

  }
};


int S176child1_classId = Class_genId();
struct S176child1:S176{

  S176child1 (int a):a(a){
    (id = S176child1_classId);
{

    }
  }
  int a;
};


int S176child2_classId = Class_genId();
struct S176child2:S176{

  S176child2 (int a):a(a){
    (id = S176child2_classId);
{

    }
  }
  int a;
};


struct Trait176{

  int(*trait176)(Class*);
};

Vec* Trait176_v = newVec();

int Trait176_S176child1_trait176(Class* self_) {
  S176child1* self = ((S176child1*)self_);
{
    return (self -> a);
  }
} 
Trait176* newTrait176_S176child1() {
  Trait176 (* impl) = (new Trait176());
  setVec(Trait176_v, S176child1_classId, ((void*)impl));
  ((impl -> trait176) = (& Trait176_S176child1_trait176));
  return impl;
} 
Trait176* Trait176_S176child1_ = newTrait176_S176child1();

int Trait38_S176child1_trait38(Class* self_) {
  S176child1* self = ((S176child1*)self_);
{
    return (self -> a);
  }
} 
Trait38* newTrait38_S176child1() {
  Trait38 (* impl) = (new Trait38());
  setVec(Trait38_v, S176child1_classId, ((void*)impl));
  ((impl -> trait38) = (& Trait38_S176child1_trait38));
  return impl;
} 
Trait38* Trait38_S176child1_ = newTrait38_S176child1();

int S177_classId = Class_genId();
struct S177{

  int id;
  S177 ():id(S177_classId){

  }
};


int S177child1_classId = Class_genId();
struct S177child1:S177{

  S177child1 (int a):a(a){
    (id = S177child1_classId);
{

    }
  }
  int a;
};


int S177child2_classId = Class_genId();
struct S177child2:S177{

  S177child2 (int a):a(a){
    (id = S177child2_classId);
{

    }
  }
  int a;
};


int S177child3_classId = Class_genId();
struct S177child3:S177{

  S177child3 (int a):a(a){
    (id = S177child3_classId);
{

    }
  }
  int a;
};


struct Trait177{

  int(*trait177)(Class*);
};

Vec* Trait177_v = newVec();

int Trait177_S177child1_trait177(Class* self_) {
  S177child1* self = ((S177child1*)self_);
{
    return (self -> a);
  }
} 
Trait177* newTrait177_S177child1() {
  Trait177 (* impl) = (new Trait177());
  setVec(Trait177_v, S177child1_classId, ((void*)impl));
  ((impl -> trait177) = (& Trait177_S177child1_trait177));
  return impl;
} 
Trait177* Trait177_S177child1_ = newTrait177_S177child1();

int Trait177_S177child2_trait177(Class* self_) {
  S177child2* self = ((S177child2*)self_);
{
    return (self -> a);
  }
} 
Trait177* newTrait177_S177child2() {
  Trait177 (* impl) = (new Trait177());
  setVec(Trait177_v, S177child2_classId, ((void*)impl));
  ((impl -> trait177) = (& Trait177_S177child2_trait177));
  return impl;
} 
Trait177* Trait177_S177child2_ = newTrait177_S177child2();

int Trait177_S177child3_trait177(Class* self_) {
  S177child3* self = ((S177child3*)self_);
{
    return (self -> a);
  }
} 
Trait177* newTrait177_S177child3() {
  Trait177 (* impl) = (new Trait177());
  setVec(Trait177_v, S177child3_classId, ((void*)impl));
  ((impl -> trait177) = (& Trait177_S177child3_trait177));
  return impl;
} 
Trait177* Trait177_S177child3_ = newTrait177_S177child3();

int Trait32_S177child1_trait32(Class* self_) {
  S177child1* self = ((S177child1*)self_);
{
    return (self -> a);
  }
} 
Trait32* newTrait32_S177child1() {
  Trait32 (* impl) = (new Trait32());
  setVec(Trait32_v, S177child1_classId, ((void*)impl));
  ((impl -> trait32) = (& Trait32_S177child1_trait32));
  return impl;
} 
Trait32* Trait32_S177child1_ = newTrait32_S177child1();

int S178_classId = Class_genId();
struct S178{

  int id;
  S178 ():id(S178_classId){

  }
};


int S178child1_classId = Class_genId();
struct S178child1:S178{

  S178child1 (int a):a(a){
    (id = S178child1_classId);
{

    }
  }
  int a;
};


int S178child2_classId = Class_genId();
struct S178child2:S178{

  S178child2 (int a):a(a){
    (id = S178child2_classId);
{

    }
  }
  int a;
};


int S178child3_classId = Class_genId();
struct S178child3:S178{

  S178child3 (int a):a(a){
    (id = S178child3_classId);
{

    }
  }
  int a;
};


struct Trait178{

  int(*trait178)(Class*);
};

Vec* Trait178_v = newVec();

int Trait178_S178child1_trait178(Class* self_) {
  S178child1* self = ((S178child1*)self_);
{
    return (self -> a);
  }
} 
Trait178* newTrait178_S178child1() {
  Trait178 (* impl) = (new Trait178());
  setVec(Trait178_v, S178child1_classId, ((void*)impl));
  ((impl -> trait178) = (& Trait178_S178child1_trait178));
  return impl;
} 
Trait178* Trait178_S178child1_ = newTrait178_S178child1();

int Trait178_S178child2_trait178(Class* self_) {
  S178child2* self = ((S178child2*)self_);
{
    return (self -> a);
  }
} 
Trait178* newTrait178_S178child2() {
  Trait178 (* impl) = (new Trait178());
  setVec(Trait178_v, S178child2_classId, ((void*)impl));
  ((impl -> trait178) = (& Trait178_S178child2_trait178));
  return impl;
} 
Trait178* Trait178_S178child2_ = newTrait178_S178child2();

int Trait152_S178child1_trait152(Class* self_) {
  S178child1* self = ((S178child1*)self_);
{
    return (self -> a);
  }
} 
Trait152* newTrait152_S178child1() {
  Trait152 (* impl) = (new Trait152());
  setVec(Trait152_v, S178child1_classId, ((void*)impl));
  ((impl -> trait152) = (& Trait152_S178child1_trait152));
  return impl;
} 
Trait152* Trait152_S178child1_ = newTrait152_S178child1();

int S179_classId = Class_genId();
struct S179{

  int id;
  S179 ():id(S179_classId){

  }
};


int S179child1_classId = Class_genId();
struct S179child1:S179{

  S179child1 (int a):a(a){
    (id = S179child1_classId);
{

    }
  }
  int a;
};


int S179child2_classId = Class_genId();
struct S179child2:S179{

  S179child2 (int a):a(a){
    (id = S179child2_classId);
{

    }
  }
  int a;
};


struct Trait179{

  int(*trait179)(Class*);
};

Vec* Trait179_v = newVec();

int Trait179_S179child1_trait179(Class* self_) {
  S179child1* self = ((S179child1*)self_);
{
    return (self -> a);
  }
} 
Trait179* newTrait179_S179child1() {
  Trait179 (* impl) = (new Trait179());
  setVec(Trait179_v, S179child1_classId, ((void*)impl));
  ((impl -> trait179) = (& Trait179_S179child1_trait179));
  return impl;
} 
Trait179* Trait179_S179child1_ = newTrait179_S179child1();

int Trait119_S179child1_trait119(Class* self_) {
  S179child1* self = ((S179child1*)self_);
{
    return (self -> a);
  }
} 
Trait119* newTrait119_S179child1() {
  Trait119 (* impl) = (new Trait119());
  setVec(Trait119_v, S179child1_classId, ((void*)impl));
  ((impl -> trait119) = (& Trait119_S179child1_trait119));
  return impl;
} 
Trait119* Trait119_S179child1_ = newTrait119_S179child1();

int S180_classId = Class_genId();
struct S180{

  int id;
  S180 ():id(S180_classId){

  }
};


int S180child1_classId = Class_genId();
struct S180child1:S180{

  S180child1 (int a):a(a){
    (id = S180child1_classId);
{

    }
  }
  int a;
};


struct Trait180{

  int(*trait180)(Class*);
};

Vec* Trait180_v = newVec();

int Trait180_S180child1_trait180(Class* self_) {
  S180child1* self = ((S180child1*)self_);
{
    return (self -> a);
  }
} 
Trait180* newTrait180_S180child1() {
  Trait180 (* impl) = (new Trait180());
  setVec(Trait180_v, S180child1_classId, ((void*)impl));
  ((impl -> trait180) = (& Trait180_S180child1_trait180));
  return impl;
} 
Trait180* Trait180_S180child1_ = newTrait180_S180child1();

int Trait39_S180child1_trait39(Class* self_) {
  S180child1* self = ((S180child1*)self_);
{
    return (self -> a);
  }
} 
Trait39* newTrait39_S180child1() {
  Trait39 (* impl) = (new Trait39());
  setVec(Trait39_v, S180child1_classId, ((void*)impl));
  ((impl -> trait39) = (& Trait39_S180child1_trait39));
  return impl;
} 
Trait39* Trait39_S180child1_ = newTrait39_S180child1();

int S181_classId = Class_genId();
struct S181{

  int id;
  S181 ():id(S181_classId){

  }
};


int S181child1_classId = Class_genId();
struct S181child1:S181{

  S181child1 (int a):a(a){
    (id = S181child1_classId);
{

    }
  }
  int a;
};


int S181child2_classId = Class_genId();
struct S181child2:S181{

  S181child2 (int a):a(a){
    (id = S181child2_classId);
{

    }
  }
  int a;
};


int S181child3_classId = Class_genId();
struct S181child3:S181{

  S181child3 (int a):a(a){
    (id = S181child3_classId);
{

    }
  }
  int a;
};


int S181child4_classId = Class_genId();
struct S181child4:S181{

  S181child4 (int a):a(a){
    (id = S181child4_classId);
{

    }
  }
  int a;
};


struct Trait181{

  int(*trait181)(Class*);
};

Vec* Trait181_v = newVec();

int Trait181_S181child1_trait181(Class* self_) {
  S181child1* self = ((S181child1*)self_);
{
    return (self -> a);
  }
} 
Trait181* newTrait181_S181child1() {
  Trait181 (* impl) = (new Trait181());
  setVec(Trait181_v, S181child1_classId, ((void*)impl));
  ((impl -> trait181) = (& Trait181_S181child1_trait181));
  return impl;
} 
Trait181* Trait181_S181child1_ = newTrait181_S181child1();

int Trait181_S181child2_trait181(Class* self_) {
  S181child2* self = ((S181child2*)self_);
{
    return (self -> a);
  }
} 
Trait181* newTrait181_S181child2() {
  Trait181 (* impl) = (new Trait181());
  setVec(Trait181_v, S181child2_classId, ((void*)impl));
  ((impl -> trait181) = (& Trait181_S181child2_trait181));
  return impl;
} 
Trait181* Trait181_S181child2_ = newTrait181_S181child2();

int Trait181_S181child3_trait181(Class* self_) {
  S181child3* self = ((S181child3*)self_);
{
    return (self -> a);
  }
} 
Trait181* newTrait181_S181child3() {
  Trait181 (* impl) = (new Trait181());
  setVec(Trait181_v, S181child3_classId, ((void*)impl));
  ((impl -> trait181) = (& Trait181_S181child3_trait181));
  return impl;
} 
Trait181* Trait181_S181child3_ = newTrait181_S181child3();

int Trait128_S181child1_trait128(Class* self_) {
  S181child1* self = ((S181child1*)self_);
{
    return (self -> a);
  }
} 
Trait128* newTrait128_S181child1() {
  Trait128 (* impl) = (new Trait128());
  setVec(Trait128_v, S181child1_classId, ((void*)impl));
  ((impl -> trait128) = (& Trait128_S181child1_trait128));
  return impl;
} 
Trait128* Trait128_S181child1_ = newTrait128_S181child1();

int S182_classId = Class_genId();
struct S182{

  int id;
  S182 ():id(S182_classId){

  }
};


int S182child1_classId = Class_genId();
struct S182child1:S182{

  S182child1 (int a):a(a){
    (id = S182child1_classId);
{

    }
  }
  int a;
};


int S182child2_classId = Class_genId();
struct S182child2:S182{

  S182child2 (int a):a(a){
    (id = S182child2_classId);
{

    }
  }
  int a;
};


int S182child3_classId = Class_genId();
struct S182child3:S182{

  S182child3 (int a):a(a){
    (id = S182child3_classId);
{

    }
  }
  int a;
};


struct Trait182{

  int(*trait182)(Class*);
};

Vec* Trait182_v = newVec();

int Trait182_S182child1_trait182(Class* self_) {
  S182child1* self = ((S182child1*)self_);
{
    return (self -> a);
  }
} 
Trait182* newTrait182_S182child1() {
  Trait182 (* impl) = (new Trait182());
  setVec(Trait182_v, S182child1_classId, ((void*)impl));
  ((impl -> trait182) = (& Trait182_S182child1_trait182));
  return impl;
} 
Trait182* Trait182_S182child1_ = newTrait182_S182child1();

int Trait162_S182child1_trait162(Class* self_) {
  S182child1* self = ((S182child1*)self_);
{
    return (self -> a);
  }
} 
Trait162* newTrait162_S182child1() {
  Trait162 (* impl) = (new Trait162());
  setVec(Trait162_v, S182child1_classId, ((void*)impl));
  ((impl -> trait162) = (& Trait162_S182child1_trait162));
  return impl;
} 
Trait162* Trait162_S182child1_ = newTrait162_S182child1();

int S183_classId = Class_genId();
struct S183{

  int id;
  S183 ():id(S183_classId){

  }
};


int S183child1_classId = Class_genId();
struct S183child1:S183{

  S183child1 (int a):a(a){
    (id = S183child1_classId);
{

    }
  }
  int a;
};


int S183child2_classId = Class_genId();
struct S183child2:S183{

  S183child2 (int a):a(a){
    (id = S183child2_classId);
{

    }
  }
  int a;
};


int S183child3_classId = Class_genId();
struct S183child3:S183{

  S183child3 (int a):a(a){
    (id = S183child3_classId);
{

    }
  }
  int a;
};


int S183child4_classId = Class_genId();
struct S183child4:S183{

  S183child4 (int a):a(a){
    (id = S183child4_classId);
{

    }
  }
  int a;
};


struct Trait183{

  int(*trait183)(Class*);
};

Vec* Trait183_v = newVec();

int Trait183_S183child1_trait183(Class* self_) {
  S183child1* self = ((S183child1*)self_);
{
    return (self -> a);
  }
} 
Trait183* newTrait183_S183child1() {
  Trait183 (* impl) = (new Trait183());
  setVec(Trait183_v, S183child1_classId, ((void*)impl));
  ((impl -> trait183) = (& Trait183_S183child1_trait183));
  return impl;
} 
Trait183* Trait183_S183child1_ = newTrait183_S183child1();

int Trait183_S183child2_trait183(Class* self_) {
  S183child2* self = ((S183child2*)self_);
{
    return (self -> a);
  }
} 
Trait183* newTrait183_S183child2() {
  Trait183 (* impl) = (new Trait183());
  setVec(Trait183_v, S183child2_classId, ((void*)impl));
  ((impl -> trait183) = (& Trait183_S183child2_trait183));
  return impl;
} 
Trait183* Trait183_S183child2_ = newTrait183_S183child2();

int Trait96_S183child1_trait96(Class* self_) {
  S183child1* self = ((S183child1*)self_);
{
    return (self -> a);
  }
} 
Trait96* newTrait96_S183child1() {
  Trait96 (* impl) = (new Trait96());
  setVec(Trait96_v, S183child1_classId, ((void*)impl));
  ((impl -> trait96) = (& Trait96_S183child1_trait96));
  return impl;
} 
Trait96* Trait96_S183child1_ = newTrait96_S183child1();

int S184_classId = Class_genId();
struct S184{

  int id;
  S184 ():id(S184_classId){

  }
};


int S184child1_classId = Class_genId();
struct S184child1:S184{

  S184child1 (int a):a(a){
    (id = S184child1_classId);
{

    }
  }
  int a;
};


int S184child2_classId = Class_genId();
struct S184child2:S184{

  S184child2 (int a):a(a){
    (id = S184child2_classId);
{

    }
  }
  int a;
};


struct Trait184{

  int(*trait184)(Class*);
};

Vec* Trait184_v = newVec();

int Trait184_S184child1_trait184(Class* self_) {
  S184child1* self = ((S184child1*)self_);
{
    return (self -> a);
  }
} 
Trait184* newTrait184_S184child1() {
  Trait184 (* impl) = (new Trait184());
  setVec(Trait184_v, S184child1_classId, ((void*)impl));
  ((impl -> trait184) = (& Trait184_S184child1_trait184));
  return impl;
} 
Trait184* Trait184_S184child1_ = newTrait184_S184child1();

int Trait184_S184child2_trait184(Class* self_) {
  S184child2* self = ((S184child2*)self_);
{
    return (self -> a);
  }
} 
Trait184* newTrait184_S184child2() {
  Trait184 (* impl) = (new Trait184());
  setVec(Trait184_v, S184child2_classId, ((void*)impl));
  ((impl -> trait184) = (& Trait184_S184child2_trait184));
  return impl;
} 
Trait184* Trait184_S184child2_ = newTrait184_S184child2();

int Trait173_S184child1_trait173(Class* self_) {
  S184child1* self = ((S184child1*)self_);
{
    return (self -> a);
  }
} 
Trait173* newTrait173_S184child1() {
  Trait173 (* impl) = (new Trait173());
  setVec(Trait173_v, S184child1_classId, ((void*)impl));
  ((impl -> trait173) = (& Trait173_S184child1_trait173));
  return impl;
} 
Trait173* Trait173_S184child1_ = newTrait173_S184child1();

int S185_classId = Class_genId();
struct S185{

  int id;
  S185 ():id(S185_classId){

  }
};


int S185child1_classId = Class_genId();
struct S185child1:S185{

  S185child1 (int a):a(a){
    (id = S185child1_classId);
{

    }
  }
  int a;
};


int S185child2_classId = Class_genId();
struct S185child2:S185{

  S185child2 (int a):a(a){
    (id = S185child2_classId);
{

    }
  }
  int a;
};


int S185child3_classId = Class_genId();
struct S185child3:S185{

  S185child3 (int a):a(a){
    (id = S185child3_classId);
{

    }
  }
  int a;
};


struct Trait185{

  int(*trait185)(Class*);
};

Vec* Trait185_v = newVec();

int Trait185_S185child1_trait185(Class* self_) {
  S185child1* self = ((S185child1*)self_);
{
    return (self -> a);
  }
} 
Trait185* newTrait185_S185child1() {
  Trait185 (* impl) = (new Trait185());
  setVec(Trait185_v, S185child1_classId, ((void*)impl));
  ((impl -> trait185) = (& Trait185_S185child1_trait185));
  return impl;
} 
Trait185* Trait185_S185child1_ = newTrait185_S185child1();

int Trait185_S185child2_trait185(Class* self_) {
  S185child2* self = ((S185child2*)self_);
{
    return (self -> a);
  }
} 
Trait185* newTrait185_S185child2() {
  Trait185 (* impl) = (new Trait185());
  setVec(Trait185_v, S185child2_classId, ((void*)impl));
  ((impl -> trait185) = (& Trait185_S185child2_trait185));
  return impl;
} 
Trait185* Trait185_S185child2_ = newTrait185_S185child2();

int Trait86_S185child1_trait86(Class* self_) {
  S185child1* self = ((S185child1*)self_);
{
    return (self -> a);
  }
} 
Trait86* newTrait86_S185child1() {
  Trait86 (* impl) = (new Trait86());
  setVec(Trait86_v, S185child1_classId, ((void*)impl));
  ((impl -> trait86) = (& Trait86_S185child1_trait86));
  return impl;
} 
Trait86* Trait86_S185child1_ = newTrait86_S185child1();

int S186_classId = Class_genId();
struct S186{

  int id;
  S186 ():id(S186_classId){

  }
};


int S186child1_classId = Class_genId();
struct S186child1:S186{

  S186child1 (int a):a(a){
    (id = S186child1_classId);
{

    }
  }
  int a;
};


int S186child2_classId = Class_genId();
struct S186child2:S186{

  S186child2 (int a):a(a){
    (id = S186child2_classId);
{

    }
  }
  int a;
};


struct Trait186{

  int(*trait186)(Class*);
};

Vec* Trait186_v = newVec();

int Trait186_S186child1_trait186(Class* self_) {
  S186child1* self = ((S186child1*)self_);
{
    return (self -> a);
  }
} 
Trait186* newTrait186_S186child1() {
  Trait186 (* impl) = (new Trait186());
  setVec(Trait186_v, S186child1_classId, ((void*)impl));
  ((impl -> trait186) = (& Trait186_S186child1_trait186));
  return impl;
} 
Trait186* Trait186_S186child1_ = newTrait186_S186child1();

int Trait101_S186child1_trait101(Class* self_) {
  S186child1* self = ((S186child1*)self_);
{
    return (self -> a);
  }
} 
Trait101* newTrait101_S186child1() {
  Trait101 (* impl) = (new Trait101());
  setVec(Trait101_v, S186child1_classId, ((void*)impl));
  ((impl -> trait101) = (& Trait101_S186child1_trait101));
  return impl;
} 
Trait101* Trait101_S186child1_ = newTrait101_S186child1();

int S187_classId = Class_genId();
struct S187{

  int id;
  S187 ():id(S187_classId){

  }
};


int S187child1_classId = Class_genId();
struct S187child1:S187{

  S187child1 (int a):a(a){
    (id = S187child1_classId);
{

    }
  }
  int a;
};


int S187child2_classId = Class_genId();
struct S187child2:S187{

  S187child2 (int a):a(a){
    (id = S187child2_classId);
{

    }
  }
  int a;
};


struct Trait187{

  int(*trait187)(Class*);
};

Vec* Trait187_v = newVec();

int Trait187_S187child1_trait187(Class* self_) {
  S187child1* self = ((S187child1*)self_);
{
    return (self -> a);
  }
} 
Trait187* newTrait187_S187child1() {
  Trait187 (* impl) = (new Trait187());
  setVec(Trait187_v, S187child1_classId, ((void*)impl));
  ((impl -> trait187) = (& Trait187_S187child1_trait187));
  return impl;
} 
Trait187* Trait187_S187child1_ = newTrait187_S187child1();

int Trait187_S187child2_trait187(Class* self_) {
  S187child2* self = ((S187child2*)self_);
{
    return (self -> a);
  }
} 
Trait187* newTrait187_S187child2() {
  Trait187 (* impl) = (new Trait187());
  setVec(Trait187_v, S187child2_classId, ((void*)impl));
  ((impl -> trait187) = (& Trait187_S187child2_trait187));
  return impl;
} 
Trait187* Trait187_S187child2_ = newTrait187_S187child2();

int Trait133_S187child1_trait133(Class* self_) {
  S187child1* self = ((S187child1*)self_);
{
    return (self -> a);
  }
} 
Trait133* newTrait133_S187child1() {
  Trait133 (* impl) = (new Trait133());
  setVec(Trait133_v, S187child1_classId, ((void*)impl));
  ((impl -> trait133) = (& Trait133_S187child1_trait133));
  return impl;
} 
Trait133* Trait133_S187child1_ = newTrait133_S187child1();

int S188_classId = Class_genId();
struct S188{

  int id;
  S188 ():id(S188_classId){

  }
};


int S188child1_classId = Class_genId();
struct S188child1:S188{

  S188child1 (int a):a(a){
    (id = S188child1_classId);
{

    }
  }
  int a;
};


int S188child2_classId = Class_genId();
struct S188child2:S188{

  S188child2 (int a):a(a){
    (id = S188child2_classId);
{

    }
  }
  int a;
};


struct Trait188{

  int(*trait188)(Class*);
};

Vec* Trait188_v = newVec();

int Trait188_S188child1_trait188(Class* self_) {
  S188child1* self = ((S188child1*)self_);
{
    return (self -> a);
  }
} 
Trait188* newTrait188_S188child1() {
  Trait188 (* impl) = (new Trait188());
  setVec(Trait188_v, S188child1_classId, ((void*)impl));
  ((impl -> trait188) = (& Trait188_S188child1_trait188));
  return impl;
} 
Trait188* Trait188_S188child1_ = newTrait188_S188child1();

int Trait50_S188child1_trait50(Class* self_) {
  S188child1* self = ((S188child1*)self_);
{
    return (self -> a);
  }
} 
Trait50* newTrait50_S188child1() {
  Trait50 (* impl) = (new Trait50());
  setVec(Trait50_v, S188child1_classId, ((void*)impl));
  ((impl -> trait50) = (& Trait50_S188child1_trait50));
  return impl;
} 
Trait50* Trait50_S188child1_ = newTrait50_S188child1();

int S189_classId = Class_genId();
struct S189{

  int id;
  S189 ():id(S189_classId){

  }
};


int S189child1_classId = Class_genId();
struct S189child1:S189{

  S189child1 (int a):a(a){
    (id = S189child1_classId);
{

    }
  }
  int a;
};


struct Trait189{

  int(*trait189)(Class*);
};

Vec* Trait189_v = newVec();

int Trait189_S189child1_trait189(Class* self_) {
  S189child1* self = ((S189child1*)self_);
{
    return (self -> a);
  }
} 
Trait189* newTrait189_S189child1() {
  Trait189 (* impl) = (new Trait189());
  setVec(Trait189_v, S189child1_classId, ((void*)impl));
  ((impl -> trait189) = (& Trait189_S189child1_trait189));
  return impl;
} 
Trait189* Trait189_S189child1_ = newTrait189_S189child1();

int Trait35_S189child1_trait35(Class* self_) {
  S189child1* self = ((S189child1*)self_);
{
    return (self -> a);
  }
} 
Trait35* newTrait35_S189child1() {
  Trait35 (* impl) = (new Trait35());
  setVec(Trait35_v, S189child1_classId, ((void*)impl));
  ((impl -> trait35) = (& Trait35_S189child1_trait35));
  return impl;
} 
Trait35* Trait35_S189child1_ = newTrait35_S189child1();

int S190_classId = Class_genId();
struct S190{

  int id;
  S190 ():id(S190_classId){

  }
};


int S190child1_classId = Class_genId();
struct S190child1:S190{

  S190child1 (int a):a(a){
    (id = S190child1_classId);
{

    }
  }
  int a;
};


int S190child2_classId = Class_genId();
struct S190child2:S190{

  S190child2 (int a):a(a){
    (id = S190child2_classId);
{

    }
  }
  int a;
};


int S190child3_classId = Class_genId();
struct S190child3:S190{

  S190child3 (int a):a(a){
    (id = S190child3_classId);
{

    }
  }
  int a;
};


struct Trait190{

  int(*trait190)(Class*);
};

Vec* Trait190_v = newVec();

int Trait190_S190child1_trait190(Class* self_) {
  S190child1* self = ((S190child1*)self_);
{
    return (self -> a);
  }
} 
Trait190* newTrait190_S190child1() {
  Trait190 (* impl) = (new Trait190());
  setVec(Trait190_v, S190child1_classId, ((void*)impl));
  ((impl -> trait190) = (& Trait190_S190child1_trait190));
  return impl;
} 
Trait190* Trait190_S190child1_ = newTrait190_S190child1();

int Trait190_S190child2_trait190(Class* self_) {
  S190child2* self = ((S190child2*)self_);
{
    return (self -> a);
  }
} 
Trait190* newTrait190_S190child2() {
  Trait190 (* impl) = (new Trait190());
  setVec(Trait190_v, S190child2_classId, ((void*)impl));
  ((impl -> trait190) = (& Trait190_S190child2_trait190));
  return impl;
} 
Trait190* Trait190_S190child2_ = newTrait190_S190child2();

int Trait190_S190child3_trait190(Class* self_) {
  S190child3* self = ((S190child3*)self_);
{
    return (self -> a);
  }
} 
Trait190* newTrait190_S190child3() {
  Trait190 (* impl) = (new Trait190());
  setVec(Trait190_v, S190child3_classId, ((void*)impl));
  ((impl -> trait190) = (& Trait190_S190child3_trait190));
  return impl;
} 
Trait190* Trait190_S190child3_ = newTrait190_S190child3();

int Trait55_S190child1_trait55(Class* self_) {
  S190child1* self = ((S190child1*)self_);
{
    return (self -> a);
  }
} 
Trait55* newTrait55_S190child1() {
  Trait55 (* impl) = (new Trait55());
  setVec(Trait55_v, S190child1_classId, ((void*)impl));
  ((impl -> trait55) = (& Trait55_S190child1_trait55));
  return impl;
} 
Trait55* Trait55_S190child1_ = newTrait55_S190child1();

int S191_classId = Class_genId();
struct S191{

  int id;
  S191 ():id(S191_classId){

  }
};


int S191child1_classId = Class_genId();
struct S191child1:S191{

  S191child1 (int a):a(a){
    (id = S191child1_classId);
{

    }
  }
  int a;
};


int S191child2_classId = Class_genId();
struct S191child2:S191{

  S191child2 (int a):a(a){
    (id = S191child2_classId);
{

    }
  }
  int a;
};


int S191child3_classId = Class_genId();
struct S191child3:S191{

  S191child3 (int a):a(a){
    (id = S191child3_classId);
{

    }
  }
  int a;
};


struct Trait191{

  int(*trait191)(Class*);
};

Vec* Trait191_v = newVec();

int Trait191_S191child1_trait191(Class* self_) {
  S191child1* self = ((S191child1*)self_);
{
    return (self -> a);
  }
} 
Trait191* newTrait191_S191child1() {
  Trait191 (* impl) = (new Trait191());
  setVec(Trait191_v, S191child1_classId, ((void*)impl));
  ((impl -> trait191) = (& Trait191_S191child1_trait191));
  return impl;
} 
Trait191* Trait191_S191child1_ = newTrait191_S191child1();

int Trait191_S191child2_trait191(Class* self_) {
  S191child2* self = ((S191child2*)self_);
{
    return (self -> a);
  }
} 
Trait191* newTrait191_S191child2() {
  Trait191 (* impl) = (new Trait191());
  setVec(Trait191_v, S191child2_classId, ((void*)impl));
  ((impl -> trait191) = (& Trait191_S191child2_trait191));
  return impl;
} 
Trait191* Trait191_S191child2_ = newTrait191_S191child2();

int Trait191_S191child3_trait191(Class* self_) {
  S191child3* self = ((S191child3*)self_);
{
    return (self -> a);
  }
} 
Trait191* newTrait191_S191child3() {
  Trait191 (* impl) = (new Trait191());
  setVec(Trait191_v, S191child3_classId, ((void*)impl));
  ((impl -> trait191) = (& Trait191_S191child3_trait191));
  return impl;
} 
Trait191* Trait191_S191child3_ = newTrait191_S191child3();

int Trait103_S191child1_trait103(Class* self_) {
  S191child1* self = ((S191child1*)self_);
{
    return (self -> a);
  }
} 
Trait103* newTrait103_S191child1() {
  Trait103 (* impl) = (new Trait103());
  setVec(Trait103_v, S191child1_classId, ((void*)impl));
  ((impl -> trait103) = (& Trait103_S191child1_trait103));
  return impl;
} 
Trait103* Trait103_S191child1_ = newTrait103_S191child1();

int S192_classId = Class_genId();
struct S192{

  int id;
  S192 ():id(S192_classId){

  }
};


int S192child1_classId = Class_genId();
struct S192child1:S192{

  S192child1 (int a):a(a){
    (id = S192child1_classId);
{

    }
  }
  int a;
};


int S192child2_classId = Class_genId();
struct S192child2:S192{

  S192child2 (int a):a(a){
    (id = S192child2_classId);
{

    }
  }
  int a;
};


struct Trait192{

  int(*trait192)(Class*);
};

Vec* Trait192_v = newVec();

int Trait192_S192child1_trait192(Class* self_) {
  S192child1* self = ((S192child1*)self_);
{
    return (self -> a);
  }
} 
Trait192* newTrait192_S192child1() {
  Trait192 (* impl) = (new Trait192());
  setVec(Trait192_v, S192child1_classId, ((void*)impl));
  ((impl -> trait192) = (& Trait192_S192child1_trait192));
  return impl;
} 
Trait192* Trait192_S192child1_ = newTrait192_S192child1();

int Trait192_S192child2_trait192(Class* self_) {
  S192child2* self = ((S192child2*)self_);
{
    return (self -> a);
  }
} 
Trait192* newTrait192_S192child2() {
  Trait192 (* impl) = (new Trait192());
  setVec(Trait192_v, S192child2_classId, ((void*)impl));
  ((impl -> trait192) = (& Trait192_S192child2_trait192));
  return impl;
} 
Trait192* Trait192_S192child2_ = newTrait192_S192child2();

int Trait55_S192child1_trait55(Class* self_) {
  S192child1* self = ((S192child1*)self_);
{
    return (self -> a);
  }
} 
Trait55* newTrait55_S192child1() {
  Trait55 (* impl) = (new Trait55());
  setVec(Trait55_v, S192child1_classId, ((void*)impl));
  ((impl -> trait55) = (& Trait55_S192child1_trait55));
  return impl;
} 
Trait55* Trait55_S192child1_ = newTrait55_S192child1();

int S193_classId = Class_genId();
struct S193{

  int id;
  S193 ():id(S193_classId){

  }
};


int S193child1_classId = Class_genId();
struct S193child1:S193{

  S193child1 (int a):a(a){
    (id = S193child1_classId);
{

    }
  }
  int a;
};


int S193child2_classId = Class_genId();
struct S193child2:S193{

  S193child2 (int a):a(a){
    (id = S193child2_classId);
{

    }
  }
  int a;
};


struct Trait193{

  int(*trait193)(Class*);
};

Vec* Trait193_v = newVec();

int Trait193_S193child1_trait193(Class* self_) {
  S193child1* self = ((S193child1*)self_);
{
    return (self -> a);
  }
} 
Trait193* newTrait193_S193child1() {
  Trait193 (* impl) = (new Trait193());
  setVec(Trait193_v, S193child1_classId, ((void*)impl));
  ((impl -> trait193) = (& Trait193_S193child1_trait193));
  return impl;
} 
Trait193* Trait193_S193child1_ = newTrait193_S193child1();

int Trait193_S193child2_trait193(Class* self_) {
  S193child2* self = ((S193child2*)self_);
{
    return (self -> a);
  }
} 
Trait193* newTrait193_S193child2() {
  Trait193 (* impl) = (new Trait193());
  setVec(Trait193_v, S193child2_classId, ((void*)impl));
  ((impl -> trait193) = (& Trait193_S193child2_trait193));
  return impl;
} 
Trait193* Trait193_S193child2_ = newTrait193_S193child2();

int Trait190_S193child1_trait190(Class* self_) {
  S193child1* self = ((S193child1*)self_);
{
    return (self -> a);
  }
} 
Trait190* newTrait190_S193child1() {
  Trait190 (* impl) = (new Trait190());
  setVec(Trait190_v, S193child1_classId, ((void*)impl));
  ((impl -> trait190) = (& Trait190_S193child1_trait190));
  return impl;
} 
Trait190* Trait190_S193child1_ = newTrait190_S193child1();

int S194_classId = Class_genId();
struct S194{

  int id;
  S194 ():id(S194_classId){

  }
};


int S194child1_classId = Class_genId();
struct S194child1:S194{

  S194child1 (int a):a(a){
    (id = S194child1_classId);
{

    }
  }
  int a;
};


int S194child2_classId = Class_genId();
struct S194child2:S194{

  S194child2 (int a):a(a){
    (id = S194child2_classId);
{

    }
  }
  int a;
};


struct Trait194{

  int(*trait194)(Class*);
};

Vec* Trait194_v = newVec();

int Trait194_S194child1_trait194(Class* self_) {
  S194child1* self = ((S194child1*)self_);
{
    return (self -> a);
  }
} 
Trait194* newTrait194_S194child1() {
  Trait194 (* impl) = (new Trait194());
  setVec(Trait194_v, S194child1_classId, ((void*)impl));
  ((impl -> trait194) = (& Trait194_S194child1_trait194));
  return impl;
} 
Trait194* Trait194_S194child1_ = newTrait194_S194child1();

int Trait194_S194child2_trait194(Class* self_) {
  S194child2* self = ((S194child2*)self_);
{
    return (self -> a);
  }
} 
Trait194* newTrait194_S194child2() {
  Trait194 (* impl) = (new Trait194());
  setVec(Trait194_v, S194child2_classId, ((void*)impl));
  ((impl -> trait194) = (& Trait194_S194child2_trait194));
  return impl;
} 
Trait194* Trait194_S194child2_ = newTrait194_S194child2();

int Trait17_S194child1_trait17(Class* self_) {
  S194child1* self = ((S194child1*)self_);
{
    return (self -> a);
  }
} 
Trait17* newTrait17_S194child1() {
  Trait17 (* impl) = (new Trait17());
  setVec(Trait17_v, S194child1_classId, ((void*)impl));
  ((impl -> trait17) = (& Trait17_S194child1_trait17));
  return impl;
} 
Trait17* Trait17_S194child1_ = newTrait17_S194child1();

int S195_classId = Class_genId();
struct S195{

  int id;
  S195 ():id(S195_classId){

  }
};


int S195child1_classId = Class_genId();
struct S195child1:S195{

  S195child1 (int a):a(a){
    (id = S195child1_classId);
{

    }
  }
  int a;
};


int S195child2_classId = Class_genId();
struct S195child2:S195{

  S195child2 (int a):a(a){
    (id = S195child2_classId);
{

    }
  }
  int a;
};


int S195child3_classId = Class_genId();
struct S195child3:S195{

  S195child3 (int a):a(a){
    (id = S195child3_classId);
{

    }
  }
  int a;
};


struct Trait195{

  int(*trait195)(Class*);
};

Vec* Trait195_v = newVec();

int Trait195_S195child1_trait195(Class* self_) {
  S195child1* self = ((S195child1*)self_);
{
    return (self -> a);
  }
} 
Trait195* newTrait195_S195child1() {
  Trait195 (* impl) = (new Trait195());
  setVec(Trait195_v, S195child1_classId, ((void*)impl));
  ((impl -> trait195) = (& Trait195_S195child1_trait195));
  return impl;
} 
Trait195* Trait195_S195child1_ = newTrait195_S195child1();

int Trait195_S195child2_trait195(Class* self_) {
  S195child2* self = ((S195child2*)self_);
{
    return (self -> a);
  }
} 
Trait195* newTrait195_S195child2() {
  Trait195 (* impl) = (new Trait195());
  setVec(Trait195_v, S195child2_classId, ((void*)impl));
  ((impl -> trait195) = (& Trait195_S195child2_trait195));
  return impl;
} 
Trait195* Trait195_S195child2_ = newTrait195_S195child2();

int Trait195_S195child3_trait195(Class* self_) {
  S195child3* self = ((S195child3*)self_);
{
    return (self -> a);
  }
} 
Trait195* newTrait195_S195child3() {
  Trait195 (* impl) = (new Trait195());
  setVec(Trait195_v, S195child3_classId, ((void*)impl));
  ((impl -> trait195) = (& Trait195_S195child3_trait195));
  return impl;
} 
Trait195* Trait195_S195child3_ = newTrait195_S195child3();

int Trait143_S195child1_trait143(Class* self_) {
  S195child1* self = ((S195child1*)self_);
{
    return (self -> a);
  }
} 
Trait143* newTrait143_S195child1() {
  Trait143 (* impl) = (new Trait143());
  setVec(Trait143_v, S195child1_classId, ((void*)impl));
  ((impl -> trait143) = (& Trait143_S195child1_trait143));
  return impl;
} 
Trait143* Trait143_S195child1_ = newTrait143_S195child1();

int S196_classId = Class_genId();
struct S196{

  int id;
  S196 ():id(S196_classId){

  }
};


int S196child1_classId = Class_genId();
struct S196child1:S196{

  S196child1 (int a):a(a){
    (id = S196child1_classId);
{

    }
  }
  int a;
};


struct Trait196{

  int(*trait196)(Class*);
};

Vec* Trait196_v = newVec();

int Trait196_S196child1_trait196(Class* self_) {
  S196child1* self = ((S196child1*)self_);
{
    return (self -> a);
  }
} 
Trait196* newTrait196_S196child1() {
  Trait196 (* impl) = (new Trait196());
  setVec(Trait196_v, S196child1_classId, ((void*)impl));
  ((impl -> trait196) = (& Trait196_S196child1_trait196));
  return impl;
} 
Trait196* Trait196_S196child1_ = newTrait196_S196child1();

int Trait11_S196child1_trait11(Class* self_) {
  S196child1* self = ((S196child1*)self_);
{
    return (self -> a);
  }
} 
Trait11* newTrait11_S196child1() {
  Trait11 (* impl) = (new Trait11());
  setVec(Trait11_v, S196child1_classId, ((void*)impl));
  ((impl -> trait11) = (& Trait11_S196child1_trait11));
  return impl;
} 
Trait11* Trait11_S196child1_ = newTrait11_S196child1();

int S197_classId = Class_genId();
struct S197{

  int id;
  S197 ():id(S197_classId){

  }
};


int S197child1_classId = Class_genId();
struct S197child1:S197{

  S197child1 (int a):a(a){
    (id = S197child1_classId);
{

    }
  }
  int a;
};


struct Trait197{

  int(*trait197)(Class*);
};

Vec* Trait197_v = newVec();

int Trait197_S197child1_trait197(Class* self_) {
  S197child1* self = ((S197child1*)self_);
{
    return (self -> a);
  }
} 
Trait197* newTrait197_S197child1() {
  Trait197 (* impl) = (new Trait197());
  setVec(Trait197_v, S197child1_classId, ((void*)impl));
  ((impl -> trait197) = (& Trait197_S197child1_trait197));
  return impl;
} 
Trait197* Trait197_S197child1_ = newTrait197_S197child1();

int Trait133_S197child1_trait133(Class* self_) {
  S197child1* self = ((S197child1*)self_);
{
    return (self -> a);
  }
} 
Trait133* newTrait133_S197child1() {
  Trait133 (* impl) = (new Trait133());
  setVec(Trait133_v, S197child1_classId, ((void*)impl));
  ((impl -> trait133) = (& Trait133_S197child1_trait133));
  return impl;
} 
Trait133* Trait133_S197child1_ = newTrait133_S197child1();

int S198_classId = Class_genId();
struct S198{

  int id;
  S198 ():id(S198_classId){

  }
};


int S198child1_classId = Class_genId();
struct S198child1:S198{

  S198child1 (int a):a(a){
    (id = S198child1_classId);
{

    }
  }
  int a;
};


int S198child2_classId = Class_genId();
struct S198child2:S198{

  S198child2 (int a):a(a){
    (id = S198child2_classId);
{

    }
  }
  int a;
};


struct Trait198{

  int(*trait198)(Class*);
};

Vec* Trait198_v = newVec();

int Trait198_S198child1_trait198(Class* self_) {
  S198child1* self = ((S198child1*)self_);
{
    return (self -> a);
  }
} 
Trait198* newTrait198_S198child1() {
  Trait198 (* impl) = (new Trait198());
  setVec(Trait198_v, S198child1_classId, ((void*)impl));
  ((impl -> trait198) = (& Trait198_S198child1_trait198));
  return impl;
} 
Trait198* Trait198_S198child1_ = newTrait198_S198child1();

int Trait198_S198child2_trait198(Class* self_) {
  S198child2* self = ((S198child2*)self_);
{
    return (self -> a);
  }
} 
Trait198* newTrait198_S198child2() {
  Trait198 (* impl) = (new Trait198());
  setVec(Trait198_v, S198child2_classId, ((void*)impl));
  ((impl -> trait198) = (& Trait198_S198child2_trait198));
  return impl;
} 
Trait198* Trait198_S198child2_ = newTrait198_S198child2();

int Trait72_S198child1_trait72(Class* self_) {
  S198child1* self = ((S198child1*)self_);
{
    return (self -> a);
  }
} 
Trait72* newTrait72_S198child1() {
  Trait72 (* impl) = (new Trait72());
  setVec(Trait72_v, S198child1_classId, ((void*)impl));
  ((impl -> trait72) = (& Trait72_S198child1_trait72));
  return impl;
} 
Trait72* Trait72_S198child1_ = newTrait72_S198child1();

int S199_classId = Class_genId();
struct S199{

  int id;
  S199 ():id(S199_classId){

  }
};


int S199child1_classId = Class_genId();
struct S199child1:S199{

  S199child1 (int a):a(a){
    (id = S199child1_classId);
{

    }
  }
  int a;
};


int S199child2_classId = Class_genId();
struct S199child2:S199{

  S199child2 (int a):a(a){
    (id = S199child2_classId);
{

    }
  }
  int a;
};


int S199child3_classId = Class_genId();
struct S199child3:S199{

  S199child3 (int a):a(a){
    (id = S199child3_classId);
{

    }
  }
  int a;
};


int S199child4_classId = Class_genId();
struct S199child4:S199{

  S199child4 (int a):a(a){
    (id = S199child4_classId);
{

    }
  }
  int a;
};


struct Trait199{

  int(*trait199)(Class*);
};

Vec* Trait199_v = newVec();

int Trait199_S199child1_trait199(Class* self_) {
  S199child1* self = ((S199child1*)self_);
{
    return (self -> a);
  }
} 
Trait199* newTrait199_S199child1() {
  Trait199 (* impl) = (new Trait199());
  setVec(Trait199_v, S199child1_classId, ((void*)impl));
  ((impl -> trait199) = (& Trait199_S199child1_trait199));
  return impl;
} 
Trait199* Trait199_S199child1_ = newTrait199_S199child1();

int Trait199_S199child2_trait199(Class* self_) {
  S199child2* self = ((S199child2*)self_);
{
    return (self -> a);
  }
} 
Trait199* newTrait199_S199child2() {
  Trait199 (* impl) = (new Trait199());
  setVec(Trait199_v, S199child2_classId, ((void*)impl));
  ((impl -> trait199) = (& Trait199_S199child2_trait199));
  return impl;
} 
Trait199* Trait199_S199child2_ = newTrait199_S199child2();

int Trait12_S199child1_trait12(Class* self_) {
  S199child1* self = ((S199child1*)self_);
{
    return (self -> a);
  }
} 
Trait12* newTrait12_S199child1() {
  Trait12 (* impl) = (new Trait12());
  setVec(Trait12_v, S199child1_classId, ((void*)impl));
  ((impl -> trait12) = (& Trait12_S199child1_trait12));
  return impl;
} 
Trait12* Trait12_S199child1_ = newTrait12_S199child1();

int S200_classId = Class_genId();
struct S200{

  int id;
  S200 ():id(S200_classId){

  }
};


int S200child1_classId = Class_genId();
struct S200child1:S200{

  S200child1 (int a):a(a){
    (id = S200child1_classId);
{

    }
  }
  int a;
};


int S200child2_classId = Class_genId();
struct S200child2:S200{

  S200child2 (int a):a(a){
    (id = S200child2_classId);
{

    }
  }
  int a;
};


int S200child3_classId = Class_genId();
struct S200child3:S200{

  S200child3 (int a):a(a){
    (id = S200child3_classId);
{

    }
  }
  int a;
};


struct Trait200{

  int(*trait200)(Class*);
};

Vec* Trait200_v = newVec();

int Trait200_S200child1_trait200(Class* self_) {
  S200child1* self = ((S200child1*)self_);
{
    return (self -> a);
  }
} 
Trait200* newTrait200_S200child1() {
  Trait200 (* impl) = (new Trait200());
  setVec(Trait200_v, S200child1_classId, ((void*)impl));
  ((impl -> trait200) = (& Trait200_S200child1_trait200));
  return impl;
} 
Trait200* Trait200_S200child1_ = newTrait200_S200child1();

int Trait200_S200child2_trait200(Class* self_) {
  S200child2* self = ((S200child2*)self_);
{
    return (self -> a);
  }
} 
Trait200* newTrait200_S200child2() {
  Trait200 (* impl) = (new Trait200());
  setVec(Trait200_v, S200child2_classId, ((void*)impl));
  ((impl -> trait200) = (& Trait200_S200child2_trait200));
  return impl;
} 
Trait200* Trait200_S200child2_ = newTrait200_S200child2();

int Trait59_S200child1_trait59(Class* self_) {
  S200child1* self = ((S200child1*)self_);
{
    return (self -> a);
  }
} 
Trait59* newTrait59_S200child1() {
  Trait59 (* impl) = (new Trait59());
  setVec(Trait59_v, S200child1_classId, ((void*)impl));
  ((impl -> trait59) = (& Trait59_S200child1_trait59));
  return impl;
} 
Trait59* Trait59_S200child1_ = newTrait59_S200child1();

int S201_classId = Class_genId();
struct S201{

  int id;
  S201 ():id(S201_classId){

  }
};


int S201child1_classId = Class_genId();
struct S201child1:S201{

  S201child1 (int a):a(a){
    (id = S201child1_classId);
{

    }
  }
  int a;
};


int S201child2_classId = Class_genId();
struct S201child2:S201{

  S201child2 (int a):a(a){
    (id = S201child2_classId);
{

    }
  }
  int a;
};


struct Trait201{

  int(*trait201)(Class*);
};

Vec* Trait201_v = newVec();

int Trait201_S201child1_trait201(Class* self_) {
  S201child1* self = ((S201child1*)self_);
{
    return (self -> a);
  }
} 
Trait201* newTrait201_S201child1() {
  Trait201 (* impl) = (new Trait201());
  setVec(Trait201_v, S201child1_classId, ((void*)impl));
  ((impl -> trait201) = (& Trait201_S201child1_trait201));
  return impl;
} 
Trait201* Trait201_S201child1_ = newTrait201_S201child1();

int Trait201_S201child2_trait201(Class* self_) {
  S201child2* self = ((S201child2*)self_);
{
    return (self -> a);
  }
} 
Trait201* newTrait201_S201child2() {
  Trait201 (* impl) = (new Trait201());
  setVec(Trait201_v, S201child2_classId, ((void*)impl));
  ((impl -> trait201) = (& Trait201_S201child2_trait201));
  return impl;
} 
Trait201* Trait201_S201child2_ = newTrait201_S201child2();

int Trait139_S201child1_trait139(Class* self_) {
  S201child1* self = ((S201child1*)self_);
{
    return (self -> a);
  }
} 
Trait139* newTrait139_S201child1() {
  Trait139 (* impl) = (new Trait139());
  setVec(Trait139_v, S201child1_classId, ((void*)impl));
  ((impl -> trait139) = (& Trait139_S201child1_trait139));
  return impl;
} 
Trait139* Trait139_S201child1_ = newTrait139_S201child1();

int S202_classId = Class_genId();
struct S202{

  int id;
  S202 ():id(S202_classId){

  }
};


int S202child1_classId = Class_genId();
struct S202child1:S202{

  S202child1 (int a):a(a){
    (id = S202child1_classId);
{

    }
  }
  int a;
};


int S202child2_classId = Class_genId();
struct S202child2:S202{

  S202child2 (int a):a(a){
    (id = S202child2_classId);
{

    }
  }
  int a;
};


int S202child3_classId = Class_genId();
struct S202child3:S202{

  S202child3 (int a):a(a){
    (id = S202child3_classId);
{

    }
  }
  int a;
};


int S202child4_classId = Class_genId();
struct S202child4:S202{

  S202child4 (int a):a(a){
    (id = S202child4_classId);
{

    }
  }
  int a;
};


struct Trait202{

  int(*trait202)(Class*);
};

Vec* Trait202_v = newVec();

int Trait202_S202child1_trait202(Class* self_) {
  S202child1* self = ((S202child1*)self_);
{
    return (self -> a);
  }
} 
Trait202* newTrait202_S202child1() {
  Trait202 (* impl) = (new Trait202());
  setVec(Trait202_v, S202child1_classId, ((void*)impl));
  ((impl -> trait202) = (& Trait202_S202child1_trait202));
  return impl;
} 
Trait202* Trait202_S202child1_ = newTrait202_S202child1();

int Trait59_S202child1_trait59(Class* self_) {
  S202child1* self = ((S202child1*)self_);
{
    return (self -> a);
  }
} 
Trait59* newTrait59_S202child1() {
  Trait59 (* impl) = (new Trait59());
  setVec(Trait59_v, S202child1_classId, ((void*)impl));
  ((impl -> trait59) = (& Trait59_S202child1_trait59));
  return impl;
} 
Trait59* Trait59_S202child1_ = newTrait59_S202child1();

int S203_classId = Class_genId();
struct S203{

  int id;
  S203 ():id(S203_classId){

  }
};


int S203child1_classId = Class_genId();
struct S203child1:S203{

  S203child1 (int a):a(a){
    (id = S203child1_classId);
{

    }
  }
  int a;
};


struct Trait203{

  int(*trait203)(Class*);
};

Vec* Trait203_v = newVec();

int Trait203_S203child1_trait203(Class* self_) {
  S203child1* self = ((S203child1*)self_);
{
    return (self -> a);
  }
} 
Trait203* newTrait203_S203child1() {
  Trait203 (* impl) = (new Trait203());
  setVec(Trait203_v, S203child1_classId, ((void*)impl));
  ((impl -> trait203) = (& Trait203_S203child1_trait203));
  return impl;
} 
Trait203* Trait203_S203child1_ = newTrait203_S203child1();

int Trait163_S203child1_trait163(Class* self_) {
  S203child1* self = ((S203child1*)self_);
{
    return (self -> a);
  }
} 
Trait163* newTrait163_S203child1() {
  Trait163 (* impl) = (new Trait163());
  setVec(Trait163_v, S203child1_classId, ((void*)impl));
  ((impl -> trait163) = (& Trait163_S203child1_trait163));
  return impl;
} 
Trait163* Trait163_S203child1_ = newTrait163_S203child1();

int S204_classId = Class_genId();
struct S204{

  int id;
  S204 ():id(S204_classId){

  }
};


int S204child1_classId = Class_genId();
struct S204child1:S204{

  S204child1 (int a):a(a){
    (id = S204child1_classId);
{

    }
  }
  int a;
};


struct Trait204{

  int(*trait204)(Class*);
};

Vec* Trait204_v = newVec();

int Trait204_S204child1_trait204(Class* self_) {
  S204child1* self = ((S204child1*)self_);
{
    return (self -> a);
  }
} 
Trait204* newTrait204_S204child1() {
  Trait204 (* impl) = (new Trait204());
  setVec(Trait204_v, S204child1_classId, ((void*)impl));
  ((impl -> trait204) = (& Trait204_S204child1_trait204));
  return impl;
} 
Trait204* Trait204_S204child1_ = newTrait204_S204child1();

int Trait86_S204child1_trait86(Class* self_) {
  S204child1* self = ((S204child1*)self_);
{
    return (self -> a);
  }
} 
Trait86* newTrait86_S204child1() {
  Trait86 (* impl) = (new Trait86());
  setVec(Trait86_v, S204child1_classId, ((void*)impl));
  ((impl -> trait86) = (& Trait86_S204child1_trait86));
  return impl;
} 
Trait86* Trait86_S204child1_ = newTrait86_S204child1();

int S205_classId = Class_genId();
struct S205{

  int id;
  S205 ():id(S205_classId){

  }
};


int S205child1_classId = Class_genId();
struct S205child1:S205{

  S205child1 (int a):a(a){
    (id = S205child1_classId);
{

    }
  }
  int a;
};


int S205child2_classId = Class_genId();
struct S205child2:S205{

  S205child2 (int a):a(a){
    (id = S205child2_classId);
{

    }
  }
  int a;
};


int S205child3_classId = Class_genId();
struct S205child3:S205{

  S205child3 (int a):a(a){
    (id = S205child3_classId);
{

    }
  }
  int a;
};


struct Trait205{

  int(*trait205)(Class*);
};

Vec* Trait205_v = newVec();

int Trait205_S205child1_trait205(Class* self_) {
  S205child1* self = ((S205child1*)self_);
{
    return (self -> a);
  }
} 
Trait205* newTrait205_S205child1() {
  Trait205 (* impl) = (new Trait205());
  setVec(Trait205_v, S205child1_classId, ((void*)impl));
  ((impl -> trait205) = (& Trait205_S205child1_trait205));
  return impl;
} 
Trait205* Trait205_S205child1_ = newTrait205_S205child1();

int Trait205_S205child2_trait205(Class* self_) {
  S205child2* self = ((S205child2*)self_);
{
    return (self -> a);
  }
} 
Trait205* newTrait205_S205child2() {
  Trait205 (* impl) = (new Trait205());
  setVec(Trait205_v, S205child2_classId, ((void*)impl));
  ((impl -> trait205) = (& Trait205_S205child2_trait205));
  return impl;
} 
Trait205* Trait205_S205child2_ = newTrait205_S205child2();

int Trait63_S205child1_trait63(Class* self_) {
  S205child1* self = ((S205child1*)self_);
{
    return (self -> a);
  }
} 
Trait63* newTrait63_S205child1() {
  Trait63 (* impl) = (new Trait63());
  setVec(Trait63_v, S205child1_classId, ((void*)impl));
  ((impl -> trait63) = (& Trait63_S205child1_trait63));
  return impl;
} 
Trait63* Trait63_S205child1_ = newTrait63_S205child1();

int S206_classId = Class_genId();
struct S206{

  int id;
  S206 ():id(S206_classId){

  }
};


int S206child1_classId = Class_genId();
struct S206child1:S206{

  S206child1 (int a):a(a){
    (id = S206child1_classId);
{

    }
  }
  int a;
};


struct Trait206{

  int(*trait206)(Class*);
};

Vec* Trait206_v = newVec();

int Trait206_S206child1_trait206(Class* self_) {
  S206child1* self = ((S206child1*)self_);
{
    return (self -> a);
  }
} 
Trait206* newTrait206_S206child1() {
  Trait206 (* impl) = (new Trait206());
  setVec(Trait206_v, S206child1_classId, ((void*)impl));
  ((impl -> trait206) = (& Trait206_S206child1_trait206));
  return impl;
} 
Trait206* Trait206_S206child1_ = newTrait206_S206child1();

int Trait175_S206child1_trait175(Class* self_) {
  S206child1* self = ((S206child1*)self_);
{
    return (self -> a);
  }
} 
Trait175* newTrait175_S206child1() {
  Trait175 (* impl) = (new Trait175());
  setVec(Trait175_v, S206child1_classId, ((void*)impl));
  ((impl -> trait175) = (& Trait175_S206child1_trait175));
  return impl;
} 
Trait175* Trait175_S206child1_ = newTrait175_S206child1();

int S207_classId = Class_genId();
struct S207{

  int id;
  S207 ():id(S207_classId){

  }
};


int S207child1_classId = Class_genId();
struct S207child1:S207{

  S207child1 (int a):a(a){
    (id = S207child1_classId);
{

    }
  }
  int a;
};


int S207child2_classId = Class_genId();
struct S207child2:S207{

  S207child2 (int a):a(a){
    (id = S207child2_classId);
{

    }
  }
  int a;
};


int S207child3_classId = Class_genId();
struct S207child3:S207{

  S207child3 (int a):a(a){
    (id = S207child3_classId);
{

    }
  }
  int a;
};


struct Trait207{

  int(*trait207)(Class*);
};

Vec* Trait207_v = newVec();

int Trait207_S207child1_trait207(Class* self_) {
  S207child1* self = ((S207child1*)self_);
{
    return (self -> a);
  }
} 
Trait207* newTrait207_S207child1() {
  Trait207 (* impl) = (new Trait207());
  setVec(Trait207_v, S207child1_classId, ((void*)impl));
  ((impl -> trait207) = (& Trait207_S207child1_trait207));
  return impl;
} 
Trait207* Trait207_S207child1_ = newTrait207_S207child1();

int Trait207_S207child2_trait207(Class* self_) {
  S207child2* self = ((S207child2*)self_);
{
    return (self -> a);
  }
} 
Trait207* newTrait207_S207child2() {
  Trait207 (* impl) = (new Trait207());
  setVec(Trait207_v, S207child2_classId, ((void*)impl));
  ((impl -> trait207) = (& Trait207_S207child2_trait207));
  return impl;
} 
Trait207* Trait207_S207child2_ = newTrait207_S207child2();

int Trait45_S207child1_trait45(Class* self_) {
  S207child1* self = ((S207child1*)self_);
{
    return (self -> a);
  }
} 
Trait45* newTrait45_S207child1() {
  Trait45 (* impl) = (new Trait45());
  setVec(Trait45_v, S207child1_classId, ((void*)impl));
  ((impl -> trait45) = (& Trait45_S207child1_trait45));
  return impl;
} 
Trait45* Trait45_S207child1_ = newTrait45_S207child1();

int S208_classId = Class_genId();
struct S208{

  int id;
  S208 ():id(S208_classId){

  }
};


int S208child1_classId = Class_genId();
struct S208child1:S208{

  S208child1 (int a):a(a){
    (id = S208child1_classId);
{

    }
  }
  int a;
};


int S208child2_classId = Class_genId();
struct S208child2:S208{

  S208child2 (int a):a(a){
    (id = S208child2_classId);
{

    }
  }
  int a;
};


struct Trait208{

  int(*trait208)(Class*);
};

Vec* Trait208_v = newVec();

int Trait208_S208child1_trait208(Class* self_) {
  S208child1* self = ((S208child1*)self_);
{
    return (self -> a);
  }
} 
Trait208* newTrait208_S208child1() {
  Trait208 (* impl) = (new Trait208());
  setVec(Trait208_v, S208child1_classId, ((void*)impl));
  ((impl -> trait208) = (& Trait208_S208child1_trait208));
  return impl;
} 
Trait208* Trait208_S208child1_ = newTrait208_S208child1();

int Trait76_S208child1_trait76(Class* self_) {
  S208child1* self = ((S208child1*)self_);
{
    return (self -> a);
  }
} 
Trait76* newTrait76_S208child1() {
  Trait76 (* impl) = (new Trait76());
  setVec(Trait76_v, S208child1_classId, ((void*)impl));
  ((impl -> trait76) = (& Trait76_S208child1_trait76));
  return impl;
} 
Trait76* Trait76_S208child1_ = newTrait76_S208child1();

int S209_classId = Class_genId();
struct S209{

  int id;
  S209 ():id(S209_classId){

  }
};


int S209child1_classId = Class_genId();
struct S209child1:S209{

  S209child1 (int a):a(a){
    (id = S209child1_classId);
{

    }
  }
  int a;
};


int S209child2_classId = Class_genId();
struct S209child2:S209{

  S209child2 (int a):a(a){
    (id = S209child2_classId);
{

    }
  }
  int a;
};


struct Trait209{

  int(*trait209)(Class*);
};

Vec* Trait209_v = newVec();

int Trait209_S209child1_trait209(Class* self_) {
  S209child1* self = ((S209child1*)self_);
{
    return (self -> a);
  }
} 
Trait209* newTrait209_S209child1() {
  Trait209 (* impl) = (new Trait209());
  setVec(Trait209_v, S209child1_classId, ((void*)impl));
  ((impl -> trait209) = (& Trait209_S209child1_trait209));
  return impl;
} 
Trait209* Trait209_S209child1_ = newTrait209_S209child1();

int Trait75_S209child1_trait75(Class* self_) {
  S209child1* self = ((S209child1*)self_);
{
    return (self -> a);
  }
} 
Trait75* newTrait75_S209child1() {
  Trait75 (* impl) = (new Trait75());
  setVec(Trait75_v, S209child1_classId, ((void*)impl));
  ((impl -> trait75) = (& Trait75_S209child1_trait75));
  return impl;
} 
Trait75* Trait75_S209child1_ = newTrait75_S209child1();

int S210_classId = Class_genId();
struct S210{

  int id;
  S210 ():id(S210_classId){

  }
};


int S210child1_classId = Class_genId();
struct S210child1:S210{

  S210child1 (int a):a(a){
    (id = S210child1_classId);
{

    }
  }
  int a;
};


int S210child2_classId = Class_genId();
struct S210child2:S210{

  S210child2 (int a):a(a){
    (id = S210child2_classId);
{

    }
  }
  int a;
};


struct Trait210{

  int(*trait210)(Class*);
};

Vec* Trait210_v = newVec();

int Trait210_S210child1_trait210(Class* self_) {
  S210child1* self = ((S210child1*)self_);
{
    return (self -> a);
  }
} 
Trait210* newTrait210_S210child1() {
  Trait210 (* impl) = (new Trait210());
  setVec(Trait210_v, S210child1_classId, ((void*)impl));
  ((impl -> trait210) = (& Trait210_S210child1_trait210));
  return impl;
} 
Trait210* Trait210_S210child1_ = newTrait210_S210child1();

int Trait83_S210child1_trait83(Class* self_) {
  S210child1* self = ((S210child1*)self_);
{
    return (self -> a);
  }
} 
Trait83* newTrait83_S210child1() {
  Trait83 (* impl) = (new Trait83());
  setVec(Trait83_v, S210child1_classId, ((void*)impl));
  ((impl -> trait83) = (& Trait83_S210child1_trait83));
  return impl;
} 
Trait83* Trait83_S210child1_ = newTrait83_S210child1();

int S211_classId = Class_genId();
struct S211{

  int id;
  S211 ():id(S211_classId){

  }
};


int S211child1_classId = Class_genId();
struct S211child1:S211{

  S211child1 (int a):a(a){
    (id = S211child1_classId);
{

    }
  }
  int a;
};


int S211child2_classId = Class_genId();
struct S211child2:S211{

  S211child2 (int a):a(a){
    (id = S211child2_classId);
{

    }
  }
  int a;
};


int S211child3_classId = Class_genId();
struct S211child3:S211{

  S211child3 (int a):a(a){
    (id = S211child3_classId);
{

    }
  }
  int a;
};


int S211child4_classId = Class_genId();
struct S211child4:S211{

  S211child4 (int a):a(a){
    (id = S211child4_classId);
{

    }
  }
  int a;
};


struct Trait211{

  int(*trait211)(Class*);
};

Vec* Trait211_v = newVec();

int Trait211_S211child1_trait211(Class* self_) {
  S211child1* self = ((S211child1*)self_);
{
    return (self -> a);
  }
} 
Trait211* newTrait211_S211child1() {
  Trait211 (* impl) = (new Trait211());
  setVec(Trait211_v, S211child1_classId, ((void*)impl));
  ((impl -> trait211) = (& Trait211_S211child1_trait211));
  return impl;
} 
Trait211* Trait211_S211child1_ = newTrait211_S211child1();

int Trait211_S211child2_trait211(Class* self_) {
  S211child2* self = ((S211child2*)self_);
{
    return (self -> a);
  }
} 
Trait211* newTrait211_S211child2() {
  Trait211 (* impl) = (new Trait211());
  setVec(Trait211_v, S211child2_classId, ((void*)impl));
  ((impl -> trait211) = (& Trait211_S211child2_trait211));
  return impl;
} 
Trait211* Trait211_S211child2_ = newTrait211_S211child2();

int Trait187_S211child1_trait187(Class* self_) {
  S211child1* self = ((S211child1*)self_);
{
    return (self -> a);
  }
} 
Trait187* newTrait187_S211child1() {
  Trait187 (* impl) = (new Trait187());
  setVec(Trait187_v, S211child1_classId, ((void*)impl));
  ((impl -> trait187) = (& Trait187_S211child1_trait187));
  return impl;
} 
Trait187* Trait187_S211child1_ = newTrait187_S211child1();

int S212_classId = Class_genId();
struct S212{

  int id;
  S212 ():id(S212_classId){

  }
};


int S212child1_classId = Class_genId();
struct S212child1:S212{

  S212child1 (int a):a(a){
    (id = S212child1_classId);
{

    }
  }
  int a;
};


int S212child2_classId = Class_genId();
struct S212child2:S212{

  S212child2 (int a):a(a){
    (id = S212child2_classId);
{

    }
  }
  int a;
};


int S212child3_classId = Class_genId();
struct S212child3:S212{

  S212child3 (int a):a(a){
    (id = S212child3_classId);
{

    }
  }
  int a;
};


struct Trait212{

  int(*trait212)(Class*);
};

Vec* Trait212_v = newVec();

int Trait212_S212child1_trait212(Class* self_) {
  S212child1* self = ((S212child1*)self_);
{
    return (self -> a);
  }
} 
Trait212* newTrait212_S212child1() {
  Trait212 (* impl) = (new Trait212());
  setVec(Trait212_v, S212child1_classId, ((void*)impl));
  ((impl -> trait212) = (& Trait212_S212child1_trait212));
  return impl;
} 
Trait212* Trait212_S212child1_ = newTrait212_S212child1();

int Trait119_S212child1_trait119(Class* self_) {
  S212child1* self = ((S212child1*)self_);
{
    return (self -> a);
  }
} 
Trait119* newTrait119_S212child1() {
  Trait119 (* impl) = (new Trait119());
  setVec(Trait119_v, S212child1_classId, ((void*)impl));
  ((impl -> trait119) = (& Trait119_S212child1_trait119));
  return impl;
} 
Trait119* Trait119_S212child1_ = newTrait119_S212child1();

int S213_classId = Class_genId();
struct S213{

  int id;
  S213 ():id(S213_classId){

  }
};


int S213child1_classId = Class_genId();
struct S213child1:S213{

  S213child1 (int a):a(a){
    (id = S213child1_classId);
{

    }
  }
  int a;
};


int S213child2_classId = Class_genId();
struct S213child2:S213{

  S213child2 (int a):a(a){
    (id = S213child2_classId);
{

    }
  }
  int a;
};


int S213child3_classId = Class_genId();
struct S213child3:S213{

  S213child3 (int a):a(a){
    (id = S213child3_classId);
{

    }
  }
  int a;
};


struct Trait213{

  int(*trait213)(Class*);
};

Vec* Trait213_v = newVec();

int Trait213_S213child1_trait213(Class* self_) {
  S213child1* self = ((S213child1*)self_);
{
    return (self -> a);
  }
} 
Trait213* newTrait213_S213child1() {
  Trait213 (* impl) = (new Trait213());
  setVec(Trait213_v, S213child1_classId, ((void*)impl));
  ((impl -> trait213) = (& Trait213_S213child1_trait213));
  return impl;
} 
Trait213* Trait213_S213child1_ = newTrait213_S213child1();

int Trait167_S213child1_trait167(Class* self_) {
  S213child1* self = ((S213child1*)self_);
{
    return (self -> a);
  }
} 
Trait167* newTrait167_S213child1() {
  Trait167 (* impl) = (new Trait167());
  setVec(Trait167_v, S213child1_classId, ((void*)impl));
  ((impl -> trait167) = (& Trait167_S213child1_trait167));
  return impl;
} 
Trait167* Trait167_S213child1_ = newTrait167_S213child1();

int S214_classId = Class_genId();
struct S214{

  int id;
  S214 ():id(S214_classId){

  }
};


int S214child1_classId = Class_genId();
struct S214child1:S214{

  S214child1 (int a):a(a){
    (id = S214child1_classId);
{

    }
  }
  int a;
};


int S214child2_classId = Class_genId();
struct S214child2:S214{

  S214child2 (int a):a(a){
    (id = S214child2_classId);
{

    }
  }
  int a;
};


int S214child3_classId = Class_genId();
struct S214child3:S214{

  S214child3 (int a):a(a){
    (id = S214child3_classId);
{

    }
  }
  int a;
};


int S214child4_classId = Class_genId();
struct S214child4:S214{

  S214child4 (int a):a(a){
    (id = S214child4_classId);
{

    }
  }
  int a;
};


struct Trait214{

  int(*trait214)(Class*);
};

Vec* Trait214_v = newVec();

int Trait214_S214child1_trait214(Class* self_) {
  S214child1* self = ((S214child1*)self_);
{
    return (self -> a);
  }
} 
Trait214* newTrait214_S214child1() {
  Trait214 (* impl) = (new Trait214());
  setVec(Trait214_v, S214child1_classId, ((void*)impl));
  ((impl -> trait214) = (& Trait214_S214child1_trait214));
  return impl;
} 
Trait214* Trait214_S214child1_ = newTrait214_S214child1();

int Trait214_S214child2_trait214(Class* self_) {
  S214child2* self = ((S214child2*)self_);
{
    return (self -> a);
  }
} 
Trait214* newTrait214_S214child2() {
  Trait214 (* impl) = (new Trait214());
  setVec(Trait214_v, S214child2_classId, ((void*)impl));
  ((impl -> trait214) = (& Trait214_S214child2_trait214));
  return impl;
} 
Trait214* Trait214_S214child2_ = newTrait214_S214child2();

int Trait214_S214child3_trait214(Class* self_) {
  S214child3* self = ((S214child3*)self_);
{
    return (self -> a);
  }
} 
Trait214* newTrait214_S214child3() {
  Trait214 (* impl) = (new Trait214());
  setVec(Trait214_v, S214child3_classId, ((void*)impl));
  ((impl -> trait214) = (& Trait214_S214child3_trait214));
  return impl;
} 
Trait214* Trait214_S214child3_ = newTrait214_S214child3();

int Trait200_S214child1_trait200(Class* self_) {
  S214child1* self = ((S214child1*)self_);
{
    return (self -> a);
  }
} 
Trait200* newTrait200_S214child1() {
  Trait200 (* impl) = (new Trait200());
  setVec(Trait200_v, S214child1_classId, ((void*)impl));
  ((impl -> trait200) = (& Trait200_S214child1_trait200));
  return impl;
} 
Trait200* Trait200_S214child1_ = newTrait200_S214child1();

int S215_classId = Class_genId();
struct S215{

  int id;
  S215 ():id(S215_classId){

  }
};


int S215child1_classId = Class_genId();
struct S215child1:S215{

  S215child1 (int a):a(a){
    (id = S215child1_classId);
{

    }
  }
  int a;
};


int S215child2_classId = Class_genId();
struct S215child2:S215{

  S215child2 (int a):a(a){
    (id = S215child2_classId);
{

    }
  }
  int a;
};


struct Trait215{

  int(*trait215)(Class*);
};

Vec* Trait215_v = newVec();

int Trait215_S215child1_trait215(Class* self_) {
  S215child1* self = ((S215child1*)self_);
{
    return (self -> a);
  }
} 
Trait215* newTrait215_S215child1() {
  Trait215 (* impl) = (new Trait215());
  setVec(Trait215_v, S215child1_classId, ((void*)impl));
  ((impl -> trait215) = (& Trait215_S215child1_trait215));
  return impl;
} 
Trait215* Trait215_S215child1_ = newTrait215_S215child1();

int Trait215_S215child2_trait215(Class* self_) {
  S215child2* self = ((S215child2*)self_);
{
    return (self -> a);
  }
} 
Trait215* newTrait215_S215child2() {
  Trait215 (* impl) = (new Trait215());
  setVec(Trait215_v, S215child2_classId, ((void*)impl));
  ((impl -> trait215) = (& Trait215_S215child2_trait215));
  return impl;
} 
Trait215* Trait215_S215child2_ = newTrait215_S215child2();

int Trait169_S215child1_trait169(Class* self_) {
  S215child1* self = ((S215child1*)self_);
{
    return (self -> a);
  }
} 
Trait169* newTrait169_S215child1() {
  Trait169 (* impl) = (new Trait169());
  setVec(Trait169_v, S215child1_classId, ((void*)impl));
  ((impl -> trait169) = (& Trait169_S215child1_trait169));
  return impl;
} 
Trait169* Trait169_S215child1_ = newTrait169_S215child1();

int S216_classId = Class_genId();
struct S216{

  int id;
  S216 ():id(S216_classId){

  }
};


int S216child1_classId = Class_genId();
struct S216child1:S216{

  S216child1 (int a):a(a){
    (id = S216child1_classId);
{

    }
  }
  int a;
};


int S216child2_classId = Class_genId();
struct S216child2:S216{

  S216child2 (int a):a(a){
    (id = S216child2_classId);
{

    }
  }
  int a;
};


struct Trait216{

  int(*trait216)(Class*);
};

Vec* Trait216_v = newVec();

int Trait216_S216child1_trait216(Class* self_) {
  S216child1* self = ((S216child1*)self_);
{
    return (self -> a);
  }
} 
Trait216* newTrait216_S216child1() {
  Trait216 (* impl) = (new Trait216());
  setVec(Trait216_v, S216child1_classId, ((void*)impl));
  ((impl -> trait216) = (& Trait216_S216child1_trait216));
  return impl;
} 
Trait216* Trait216_S216child1_ = newTrait216_S216child1();

int Trait61_S216child1_trait61(Class* self_) {
  S216child1* self = ((S216child1*)self_);
{
    return (self -> a);
  }
} 
Trait61* newTrait61_S216child1() {
  Trait61 (* impl) = (new Trait61());
  setVec(Trait61_v, S216child1_classId, ((void*)impl));
  ((impl -> trait61) = (& Trait61_S216child1_trait61));
  return impl;
} 
Trait61* Trait61_S216child1_ = newTrait61_S216child1();

int S217_classId = Class_genId();
struct S217{

  int id;
  S217 ():id(S217_classId){

  }
};


int S217child1_classId = Class_genId();
struct S217child1:S217{

  S217child1 (int a):a(a){
    (id = S217child1_classId);
{

    }
  }
  int a;
};


struct Trait217{

  int(*trait217)(Class*);
};

Vec* Trait217_v = newVec();

int Trait217_S217child1_trait217(Class* self_) {
  S217child1* self = ((S217child1*)self_);
{
    return (self -> a);
  }
} 
Trait217* newTrait217_S217child1() {
  Trait217 (* impl) = (new Trait217());
  setVec(Trait217_v, S217child1_classId, ((void*)impl));
  ((impl -> trait217) = (& Trait217_S217child1_trait217));
  return impl;
} 
Trait217* Trait217_S217child1_ = newTrait217_S217child1();

int Trait151_S217child1_trait151(Class* self_) {
  S217child1* self = ((S217child1*)self_);
{
    return (self -> a);
  }
} 
Trait151* newTrait151_S217child1() {
  Trait151 (* impl) = (new Trait151());
  setVec(Trait151_v, S217child1_classId, ((void*)impl));
  ((impl -> trait151) = (& Trait151_S217child1_trait151));
  return impl;
} 
Trait151* Trait151_S217child1_ = newTrait151_S217child1();

int S218_classId = Class_genId();
struct S218{

  int id;
  S218 ():id(S218_classId){

  }
};


int S218child1_classId = Class_genId();
struct S218child1:S218{

  S218child1 (int a):a(a){
    (id = S218child1_classId);
{

    }
  }
  int a;
};


int S218child2_classId = Class_genId();
struct S218child2:S218{

  S218child2 (int a):a(a){
    (id = S218child2_classId);
{

    }
  }
  int a;
};


int S218child3_classId = Class_genId();
struct S218child3:S218{

  S218child3 (int a):a(a){
    (id = S218child3_classId);
{

    }
  }
  int a;
};


struct Trait218{

  int(*trait218)(Class*);
};

Vec* Trait218_v = newVec();

int Trait218_S218child1_trait218(Class* self_) {
  S218child1* self = ((S218child1*)self_);
{
    return (self -> a);
  }
} 
Trait218* newTrait218_S218child1() {
  Trait218 (* impl) = (new Trait218());
  setVec(Trait218_v, S218child1_classId, ((void*)impl));
  ((impl -> trait218) = (& Trait218_S218child1_trait218));
  return impl;
} 
Trait218* Trait218_S218child1_ = newTrait218_S218child1();

int Trait218_S218child2_trait218(Class* self_) {
  S218child2* self = ((S218child2*)self_);
{
    return (self -> a);
  }
} 
Trait218* newTrait218_S218child2() {
  Trait218 (* impl) = (new Trait218());
  setVec(Trait218_v, S218child2_classId, ((void*)impl));
  ((impl -> trait218) = (& Trait218_S218child2_trait218));
  return impl;
} 
Trait218* Trait218_S218child2_ = newTrait218_S218child2();

int Trait172_S218child1_trait172(Class* self_) {
  S218child1* self = ((S218child1*)self_);
{
    return (self -> a);
  }
} 
Trait172* newTrait172_S218child1() {
  Trait172 (* impl) = (new Trait172());
  setVec(Trait172_v, S218child1_classId, ((void*)impl));
  ((impl -> trait172) = (& Trait172_S218child1_trait172));
  return impl;
} 
Trait172* Trait172_S218child1_ = newTrait172_S218child1();

int S219_classId = Class_genId();
struct S219{

  int id;
  S219 ():id(S219_classId){

  }
};


int S219child1_classId = Class_genId();
struct S219child1:S219{

  S219child1 (int a):a(a){
    (id = S219child1_classId);
{

    }
  }
  int a;
};


int S219child2_classId = Class_genId();
struct S219child2:S219{

  S219child2 (int a):a(a){
    (id = S219child2_classId);
{

    }
  }
  int a;
};


int S219child3_classId = Class_genId();
struct S219child3:S219{

  S219child3 (int a):a(a){
    (id = S219child3_classId);
{

    }
  }
  int a;
};


struct Trait219{

  int(*trait219)(Class*);
};

Vec* Trait219_v = newVec();

int Trait219_S219child1_trait219(Class* self_) {
  S219child1* self = ((S219child1*)self_);
{
    return (self -> a);
  }
} 
Trait219* newTrait219_S219child1() {
  Trait219 (* impl) = (new Trait219());
  setVec(Trait219_v, S219child1_classId, ((void*)impl));
  ((impl -> trait219) = (& Trait219_S219child1_trait219));
  return impl;
} 
Trait219* Trait219_S219child1_ = newTrait219_S219child1();

int Trait139_S219child1_trait139(Class* self_) {
  S219child1* self = ((S219child1*)self_);
{
    return (self -> a);
  }
} 
Trait139* newTrait139_S219child1() {
  Trait139 (* impl) = (new Trait139());
  setVec(Trait139_v, S219child1_classId, ((void*)impl));
  ((impl -> trait139) = (& Trait139_S219child1_trait139));
  return impl;
} 
Trait139* Trait139_S219child1_ = newTrait139_S219child1();

int S220_classId = Class_genId();
struct S220{

  int id;
  S220 ():id(S220_classId){

  }
};


int S220child1_classId = Class_genId();
struct S220child1:S220{

  S220child1 (int a):a(a){
    (id = S220child1_classId);
{

    }
  }
  int a;
};


struct Trait220{

  int(*trait220)(Class*);
};

Vec* Trait220_v = newVec();

int Trait220_S220child1_trait220(Class* self_) {
  S220child1* self = ((S220child1*)self_);
{
    return (self -> a);
  }
} 
Trait220* newTrait220_S220child1() {
  Trait220 (* impl) = (new Trait220());
  setVec(Trait220_v, S220child1_classId, ((void*)impl));
  ((impl -> trait220) = (& Trait220_S220child1_trait220));
  return impl;
} 
Trait220* Trait220_S220child1_ = newTrait220_S220child1();

int Trait197_S220child1_trait197(Class* self_) {
  S220child1* self = ((S220child1*)self_);
{
    return (self -> a);
  }
} 
Trait197* newTrait197_S220child1() {
  Trait197 (* impl) = (new Trait197());
  setVec(Trait197_v, S220child1_classId, ((void*)impl));
  ((impl -> trait197) = (& Trait197_S220child1_trait197));
  return impl;
} 
Trait197* Trait197_S220child1_ = newTrait197_S220child1();

int S221_classId = Class_genId();
struct S221{

  int id;
  S221 ():id(S221_classId){

  }
};


int S221child1_classId = Class_genId();
struct S221child1:S221{

  S221child1 (int a):a(a){
    (id = S221child1_classId);
{

    }
  }
  int a;
};


int S221child2_classId = Class_genId();
struct S221child2:S221{

  S221child2 (int a):a(a){
    (id = S221child2_classId);
{

    }
  }
  int a;
};


struct Trait221{

  int(*trait221)(Class*);
};

Vec* Trait221_v = newVec();

int Trait221_S221child1_trait221(Class* self_) {
  S221child1* self = ((S221child1*)self_);
{
    return (self -> a);
  }
} 
Trait221* newTrait221_S221child1() {
  Trait221 (* impl) = (new Trait221());
  setVec(Trait221_v, S221child1_classId, ((void*)impl));
  ((impl -> trait221) = (& Trait221_S221child1_trait221));
  return impl;
} 
Trait221* Trait221_S221child1_ = newTrait221_S221child1();

int Trait93_S221child1_trait93(Class* self_) {
  S221child1* self = ((S221child1*)self_);
{
    return (self -> a);
  }
} 
Trait93* newTrait93_S221child1() {
  Trait93 (* impl) = (new Trait93());
  setVec(Trait93_v, S221child1_classId, ((void*)impl));
  ((impl -> trait93) = (& Trait93_S221child1_trait93));
  return impl;
} 
Trait93* Trait93_S221child1_ = newTrait93_S221child1();

int S222_classId = Class_genId();
struct S222{

  int id;
  S222 ():id(S222_classId){

  }
};


int S222child1_classId = Class_genId();
struct S222child1:S222{

  S222child1 (int a):a(a){
    (id = S222child1_classId);
{

    }
  }
  int a;
};


int S222child2_classId = Class_genId();
struct S222child2:S222{

  S222child2 (int a):a(a){
    (id = S222child2_classId);
{

    }
  }
  int a;
};


struct Trait222{

  int(*trait222)(Class*);
};

Vec* Trait222_v = newVec();

int Trait222_S222child1_trait222(Class* self_) {
  S222child1* self = ((S222child1*)self_);
{
    return (self -> a);
  }
} 
Trait222* newTrait222_S222child1() {
  Trait222 (* impl) = (new Trait222());
  setVec(Trait222_v, S222child1_classId, ((void*)impl));
  ((impl -> trait222) = (& Trait222_S222child1_trait222));
  return impl;
} 
Trait222* Trait222_S222child1_ = newTrait222_S222child1();

int Trait109_S222child1_trait109(Class* self_) {
  S222child1* self = ((S222child1*)self_);
{
    return (self -> a);
  }
} 
Trait109* newTrait109_S222child1() {
  Trait109 (* impl) = (new Trait109());
  setVec(Trait109_v, S222child1_classId, ((void*)impl));
  ((impl -> trait109) = (& Trait109_S222child1_trait109));
  return impl;
} 
Trait109* Trait109_S222child1_ = newTrait109_S222child1();

int S223_classId = Class_genId();
struct S223{

  int id;
  S223 ():id(S223_classId){

  }
};


int S223child1_classId = Class_genId();
struct S223child1:S223{

  S223child1 (int a):a(a){
    (id = S223child1_classId);
{

    }
  }
  int a;
};


int S223child2_classId = Class_genId();
struct S223child2:S223{

  S223child2 (int a):a(a){
    (id = S223child2_classId);
{

    }
  }
  int a;
};


int S223child3_classId = Class_genId();
struct S223child3:S223{

  S223child3 (int a):a(a){
    (id = S223child3_classId);
{

    }
  }
  int a;
};


struct Trait223{

  int(*trait223)(Class*);
};

Vec* Trait223_v = newVec();

int Trait223_S223child1_trait223(Class* self_) {
  S223child1* self = ((S223child1*)self_);
{
    return (self -> a);
  }
} 
Trait223* newTrait223_S223child1() {
  Trait223 (* impl) = (new Trait223());
  setVec(Trait223_v, S223child1_classId, ((void*)impl));
  ((impl -> trait223) = (& Trait223_S223child1_trait223));
  return impl;
} 
Trait223* Trait223_S223child1_ = newTrait223_S223child1();

int Trait223_S223child2_trait223(Class* self_) {
  S223child2* self = ((S223child2*)self_);
{
    return (self -> a);
  }
} 
Trait223* newTrait223_S223child2() {
  Trait223 (* impl) = (new Trait223());
  setVec(Trait223_v, S223child2_classId, ((void*)impl));
  ((impl -> trait223) = (& Trait223_S223child2_trait223));
  return impl;
} 
Trait223* Trait223_S223child2_ = newTrait223_S223child2();

int Trait110_S223child1_trait110(Class* self_) {
  S223child1* self = ((S223child1*)self_);
{
    return (self -> a);
  }
} 
Trait110* newTrait110_S223child1() {
  Trait110 (* impl) = (new Trait110());
  setVec(Trait110_v, S223child1_classId, ((void*)impl));
  ((impl -> trait110) = (& Trait110_S223child1_trait110));
  return impl;
} 
Trait110* Trait110_S223child1_ = newTrait110_S223child1();

int S224_classId = Class_genId();
struct S224{

  int id;
  S224 ():id(S224_classId){

  }
};


int S224child1_classId = Class_genId();
struct S224child1:S224{

  S224child1 (int a):a(a){
    (id = S224child1_classId);
{

    }
  }
  int a;
};


int S224child2_classId = Class_genId();
struct S224child2:S224{

  S224child2 (int a):a(a){
    (id = S224child2_classId);
{

    }
  }
  int a;
};


int S224child3_classId = Class_genId();
struct S224child3:S224{

  S224child3 (int a):a(a){
    (id = S224child3_classId);
{

    }
  }
  int a;
};


struct Trait224{

  int(*trait224)(Class*);
};

Vec* Trait224_v = newVec();

int Trait224_S224child1_trait224(Class* self_) {
  S224child1* self = ((S224child1*)self_);
{
    return (self -> a);
  }
} 
Trait224* newTrait224_S224child1() {
  Trait224 (* impl) = (new Trait224());
  setVec(Trait224_v, S224child1_classId, ((void*)impl));
  ((impl -> trait224) = (& Trait224_S224child1_trait224));
  return impl;
} 
Trait224* Trait224_S224child1_ = newTrait224_S224child1();

int Trait224_S224child2_trait224(Class* self_) {
  S224child2* self = ((S224child2*)self_);
{
    return (self -> a);
  }
} 
Trait224* newTrait224_S224child2() {
  Trait224 (* impl) = (new Trait224());
  setVec(Trait224_v, S224child2_classId, ((void*)impl));
  ((impl -> trait224) = (& Trait224_S224child2_trait224));
  return impl;
} 
Trait224* Trait224_S224child2_ = newTrait224_S224child2();

int Trait21_S224child1_trait21(Class* self_) {
  S224child1* self = ((S224child1*)self_);
{
    return (self -> a);
  }
} 
Trait21* newTrait21_S224child1() {
  Trait21 (* impl) = (new Trait21());
  setVec(Trait21_v, S224child1_classId, ((void*)impl));
  ((impl -> trait21) = (& Trait21_S224child1_trait21));
  return impl;
} 
Trait21* Trait21_S224child1_ = newTrait21_S224child1();

int S225_classId = Class_genId();
struct S225{

  int id;
  S225 ():id(S225_classId){

  }
};


int S225child1_classId = Class_genId();
struct S225child1:S225{

  S225child1 (int a):a(a){
    (id = S225child1_classId);
{

    }
  }
  int a;
};


int S225child2_classId = Class_genId();
struct S225child2:S225{

  S225child2 (int a):a(a){
    (id = S225child2_classId);
{

    }
  }
  int a;
};


int S225child3_classId = Class_genId();
struct S225child3:S225{

  S225child3 (int a):a(a){
    (id = S225child3_classId);
{

    }
  }
  int a;
};


int S225child4_classId = Class_genId();
struct S225child4:S225{

  S225child4 (int a):a(a){
    (id = S225child4_classId);
{

    }
  }
  int a;
};


struct Trait225{

  int(*trait225)(Class*);
};

Vec* Trait225_v = newVec();

int Trait225_S225child1_trait225(Class* self_) {
  S225child1* self = ((S225child1*)self_);
{
    return (self -> a);
  }
} 
Trait225* newTrait225_S225child1() {
  Trait225 (* impl) = (new Trait225());
  setVec(Trait225_v, S225child1_classId, ((void*)impl));
  ((impl -> trait225) = (& Trait225_S225child1_trait225));
  return impl;
} 
Trait225* Trait225_S225child1_ = newTrait225_S225child1();

int Trait225_S225child2_trait225(Class* self_) {
  S225child2* self = ((S225child2*)self_);
{
    return (self -> a);
  }
} 
Trait225* newTrait225_S225child2() {
  Trait225 (* impl) = (new Trait225());
  setVec(Trait225_v, S225child2_classId, ((void*)impl));
  ((impl -> trait225) = (& Trait225_S225child2_trait225));
  return impl;
} 
Trait225* Trait225_S225child2_ = newTrait225_S225child2();

int Trait225_S225child3_trait225(Class* self_) {
  S225child3* self = ((S225child3*)self_);
{
    return (self -> a);
  }
} 
Trait225* newTrait225_S225child3() {
  Trait225 (* impl) = (new Trait225());
  setVec(Trait225_v, S225child3_classId, ((void*)impl));
  ((impl -> trait225) = (& Trait225_S225child3_trait225));
  return impl;
} 
Trait225* Trait225_S225child3_ = newTrait225_S225child3();

int Trait154_S225child1_trait154(Class* self_) {
  S225child1* self = ((S225child1*)self_);
{
    return (self -> a);
  }
} 
Trait154* newTrait154_S225child1() {
  Trait154 (* impl) = (new Trait154());
  setVec(Trait154_v, S225child1_classId, ((void*)impl));
  ((impl -> trait154) = (& Trait154_S225child1_trait154));
  return impl;
} 
Trait154* Trait154_S225child1_ = newTrait154_S225child1();

int S226_classId = Class_genId();
struct S226{

  int id;
  S226 ():id(S226_classId){

  }
};


int S226child1_classId = Class_genId();
struct S226child1:S226{

  S226child1 (int a):a(a){
    (id = S226child1_classId);
{

    }
  }
  int a;
};


int S226child2_classId = Class_genId();
struct S226child2:S226{

  S226child2 (int a):a(a){
    (id = S226child2_classId);
{

    }
  }
  int a;
};


int S226child3_classId = Class_genId();
struct S226child3:S226{

  S226child3 (int a):a(a){
    (id = S226child3_classId);
{

    }
  }
  int a;
};


struct Trait226{

  int(*trait226)(Class*);
};

Vec* Trait226_v = newVec();

int Trait226_S226child1_trait226(Class* self_) {
  S226child1* self = ((S226child1*)self_);
{
    return (self -> a);
  }
} 
Trait226* newTrait226_S226child1() {
  Trait226 (* impl) = (new Trait226());
  setVec(Trait226_v, S226child1_classId, ((void*)impl));
  ((impl -> trait226) = (& Trait226_S226child1_trait226));
  return impl;
} 
Trait226* Trait226_S226child1_ = newTrait226_S226child1();

int Trait226_S226child2_trait226(Class* self_) {
  S226child2* self = ((S226child2*)self_);
{
    return (self -> a);
  }
} 
Trait226* newTrait226_S226child2() {
  Trait226 (* impl) = (new Trait226());
  setVec(Trait226_v, S226child2_classId, ((void*)impl));
  ((impl -> trait226) = (& Trait226_S226child2_trait226));
  return impl;
} 
Trait226* Trait226_S226child2_ = newTrait226_S226child2();

int Trait226_S226child3_trait226(Class* self_) {
  S226child3* self = ((S226child3*)self_);
{
    return (self -> a);
  }
} 
Trait226* newTrait226_S226child3() {
  Trait226 (* impl) = (new Trait226());
  setVec(Trait226_v, S226child3_classId, ((void*)impl));
  ((impl -> trait226) = (& Trait226_S226child3_trait226));
  return impl;
} 
Trait226* Trait226_S226child3_ = newTrait226_S226child3();

int Trait36_S226child1_trait36(Class* self_) {
  S226child1* self = ((S226child1*)self_);
{
    return (self -> a);
  }
} 
Trait36* newTrait36_S226child1() {
  Trait36 (* impl) = (new Trait36());
  setVec(Trait36_v, S226child1_classId, ((void*)impl));
  ((impl -> trait36) = (& Trait36_S226child1_trait36));
  return impl;
} 
Trait36* Trait36_S226child1_ = newTrait36_S226child1();

int S227_classId = Class_genId();
struct S227{

  int id;
  S227 ():id(S227_classId){

  }
};


int S227child1_classId = Class_genId();
struct S227child1:S227{

  S227child1 (int a):a(a){
    (id = S227child1_classId);
{

    }
  }
  int a;
};


int S227child2_classId = Class_genId();
struct S227child2:S227{

  S227child2 (int a):a(a){
    (id = S227child2_classId);
{

    }
  }
  int a;
};


struct Trait227{

  int(*trait227)(Class*);
};

Vec* Trait227_v = newVec();

int Trait227_S227child1_trait227(Class* self_) {
  S227child1* self = ((S227child1*)self_);
{
    return (self -> a);
  }
} 
Trait227* newTrait227_S227child1() {
  Trait227 (* impl) = (new Trait227());
  setVec(Trait227_v, S227child1_classId, ((void*)impl));
  ((impl -> trait227) = (& Trait227_S227child1_trait227));
  return impl;
} 
Trait227* Trait227_S227child1_ = newTrait227_S227child1();

int Trait6_S227child1_trait6(Class* self_) {
  S227child1* self = ((S227child1*)self_);
{
    return (self -> a);
  }
} 
Trait6* newTrait6_S227child1() {
  Trait6 (* impl) = (new Trait6());
  setVec(Trait6_v, S227child1_classId, ((void*)impl));
  ((impl -> trait6) = (& Trait6_S227child1_trait6));
  return impl;
} 
Trait6* Trait6_S227child1_ = newTrait6_S227child1();

int S228_classId = Class_genId();
struct S228{

  int id;
  S228 ():id(S228_classId){

  }
};


int S228child1_classId = Class_genId();
struct S228child1:S228{

  S228child1 (int a):a(a){
    (id = S228child1_classId);
{

    }
  }
  int a;
};


int S228child2_classId = Class_genId();
struct S228child2:S228{

  S228child2 (int a):a(a){
    (id = S228child2_classId);
{

    }
  }
  int a;
};


struct Trait228{

  int(*trait228)(Class*);
};

Vec* Trait228_v = newVec();

int Trait228_S228child1_trait228(Class* self_) {
  S228child1* self = ((S228child1*)self_);
{
    return (self -> a);
  }
} 
Trait228* newTrait228_S228child1() {
  Trait228 (* impl) = (new Trait228());
  setVec(Trait228_v, S228child1_classId, ((void*)impl));
  ((impl -> trait228) = (& Trait228_S228child1_trait228));
  return impl;
} 
Trait228* Trait228_S228child1_ = newTrait228_S228child1();

int Trait24_S228child1_trait24(Class* self_) {
  S228child1* self = ((S228child1*)self_);
{
    return (self -> a);
  }
} 
Trait24* newTrait24_S228child1() {
  Trait24 (* impl) = (new Trait24());
  setVec(Trait24_v, S228child1_classId, ((void*)impl));
  ((impl -> trait24) = (& Trait24_S228child1_trait24));
  return impl;
} 
Trait24* Trait24_S228child1_ = newTrait24_S228child1();

int S229_classId = Class_genId();
struct S229{

  int id;
  S229 ():id(S229_classId){

  }
};


int S229child1_classId = Class_genId();
struct S229child1:S229{

  S229child1 (int a):a(a){
    (id = S229child1_classId);
{

    }
  }
  int a;
};


int S229child2_classId = Class_genId();
struct S229child2:S229{

  S229child2 (int a):a(a){
    (id = S229child2_classId);
{

    }
  }
  int a;
};


struct Trait229{

  int(*trait229)(Class*);
};

Vec* Trait229_v = newVec();

int Trait229_S229child1_trait229(Class* self_) {
  S229child1* self = ((S229child1*)self_);
{
    return (self -> a);
  }
} 
Trait229* newTrait229_S229child1() {
  Trait229 (* impl) = (new Trait229());
  setVec(Trait229_v, S229child1_classId, ((void*)impl));
  ((impl -> trait229) = (& Trait229_S229child1_trait229));
  return impl;
} 
Trait229* Trait229_S229child1_ = newTrait229_S229child1();

int Trait229_S229child2_trait229(Class* self_) {
  S229child2* self = ((S229child2*)self_);
{
    return (self -> a);
  }
} 
Trait229* newTrait229_S229child2() {
  Trait229 (* impl) = (new Trait229());
  setVec(Trait229_v, S229child2_classId, ((void*)impl));
  ((impl -> trait229) = (& Trait229_S229child2_trait229));
  return impl;
} 
Trait229* Trait229_S229child2_ = newTrait229_S229child2();

int Trait16_S229child1_trait16(Class* self_) {
  S229child1* self = ((S229child1*)self_);
{
    return (self -> a);
  }
} 
Trait16* newTrait16_S229child1() {
  Trait16 (* impl) = (new Trait16());
  setVec(Trait16_v, S229child1_classId, ((void*)impl));
  ((impl -> trait16) = (& Trait16_S229child1_trait16));
  return impl;
} 
Trait16* Trait16_S229child1_ = newTrait16_S229child1();

int S230_classId = Class_genId();
struct S230{

  int id;
  S230 ():id(S230_classId){

  }
};


int S230child1_classId = Class_genId();
struct S230child1:S230{

  S230child1 (int a):a(a){
    (id = S230child1_classId);
{

    }
  }
  int a;
};


int S230child2_classId = Class_genId();
struct S230child2:S230{

  S230child2 (int a):a(a){
    (id = S230child2_classId);
{

    }
  }
  int a;
};


int S230child3_classId = Class_genId();
struct S230child3:S230{

  S230child3 (int a):a(a){
    (id = S230child3_classId);
{

    }
  }
  int a;
};


struct Trait230{

  int(*trait230)(Class*);
};

Vec* Trait230_v = newVec();

int Trait230_S230child1_trait230(Class* self_) {
  S230child1* self = ((S230child1*)self_);
{
    return (self -> a);
  }
} 
Trait230* newTrait230_S230child1() {
  Trait230 (* impl) = (new Trait230());
  setVec(Trait230_v, S230child1_classId, ((void*)impl));
  ((impl -> trait230) = (& Trait230_S230child1_trait230));
  return impl;
} 
Trait230* Trait230_S230child1_ = newTrait230_S230child1();

int Trait230_S230child2_trait230(Class* self_) {
  S230child2* self = ((S230child2*)self_);
{
    return (self -> a);
  }
} 
Trait230* newTrait230_S230child2() {
  Trait230 (* impl) = (new Trait230());
  setVec(Trait230_v, S230child2_classId, ((void*)impl));
  ((impl -> trait230) = (& Trait230_S230child2_trait230));
  return impl;
} 
Trait230* Trait230_S230child2_ = newTrait230_S230child2();

int Trait125_S230child1_trait125(Class* self_) {
  S230child1* self = ((S230child1*)self_);
{
    return (self -> a);
  }
} 
Trait125* newTrait125_S230child1() {
  Trait125 (* impl) = (new Trait125());
  setVec(Trait125_v, S230child1_classId, ((void*)impl));
  ((impl -> trait125) = (& Trait125_S230child1_trait125));
  return impl;
} 
Trait125* Trait125_S230child1_ = newTrait125_S230child1();

int S231_classId = Class_genId();
struct S231{

  int id;
  S231 ():id(S231_classId){

  }
};


int S231child1_classId = Class_genId();
struct S231child1:S231{

  S231child1 (int a):a(a){
    (id = S231child1_classId);
{

    }
  }
  int a;
};


int S231child2_classId = Class_genId();
struct S231child2:S231{

  S231child2 (int a):a(a){
    (id = S231child2_classId);
{

    }
  }
  int a;
};


struct Trait231{

  int(*trait231)(Class*);
};

Vec* Trait231_v = newVec();

int Trait231_S231child1_trait231(Class* self_) {
  S231child1* self = ((S231child1*)self_);
{
    return (self -> a);
  }
} 
Trait231* newTrait231_S231child1() {
  Trait231 (* impl) = (new Trait231());
  setVec(Trait231_v, S231child1_classId, ((void*)impl));
  ((impl -> trait231) = (& Trait231_S231child1_trait231));
  return impl;
} 
Trait231* Trait231_S231child1_ = newTrait231_S231child1();

int Trait143_S231child1_trait143(Class* self_) {
  S231child1* self = ((S231child1*)self_);
{
    return (self -> a);
  }
} 
Trait143* newTrait143_S231child1() {
  Trait143 (* impl) = (new Trait143());
  setVec(Trait143_v, S231child1_classId, ((void*)impl));
  ((impl -> trait143) = (& Trait143_S231child1_trait143));
  return impl;
} 
Trait143* Trait143_S231child1_ = newTrait143_S231child1();

int S232_classId = Class_genId();
struct S232{

  int id;
  S232 ():id(S232_classId){

  }
};


int S232child1_classId = Class_genId();
struct S232child1:S232{

  S232child1 (int a):a(a){
    (id = S232child1_classId);
{

    }
  }
  int a;
};


struct Trait232{

  int(*trait232)(Class*);
};

Vec* Trait232_v = newVec();

int Trait232_S232child1_trait232(Class* self_) {
  S232child1* self = ((S232child1*)self_);
{
    return (self -> a);
  }
} 
Trait232* newTrait232_S232child1() {
  Trait232 (* impl) = (new Trait232());
  setVec(Trait232_v, S232child1_classId, ((void*)impl));
  ((impl -> trait232) = (& Trait232_S232child1_trait232));
  return impl;
} 
Trait232* Trait232_S232child1_ = newTrait232_S232child1();

int Trait183_S232child1_trait183(Class* self_) {
  S232child1* self = ((S232child1*)self_);
{
    return (self -> a);
  }
} 
Trait183* newTrait183_S232child1() {
  Trait183 (* impl) = (new Trait183());
  setVec(Trait183_v, S232child1_classId, ((void*)impl));
  ((impl -> trait183) = (& Trait183_S232child1_trait183));
  return impl;
} 
Trait183* Trait183_S232child1_ = newTrait183_S232child1();

int S233_classId = Class_genId();
struct S233{

  int id;
  S233 ():id(S233_classId){

  }
};


int S233child1_classId = Class_genId();
struct S233child1:S233{

  S233child1 (int a):a(a){
    (id = S233child1_classId);
{

    }
  }
  int a;
};


int S233child2_classId = Class_genId();
struct S233child2:S233{

  S233child2 (int a):a(a){
    (id = S233child2_classId);
{

    }
  }
  int a;
};


int S233child3_classId = Class_genId();
struct S233child3:S233{

  S233child3 (int a):a(a){
    (id = S233child3_classId);
{

    }
  }
  int a;
};


int S233child4_classId = Class_genId();
struct S233child4:S233{

  S233child4 (int a):a(a){
    (id = S233child4_classId);
{

    }
  }
  int a;
};


int S233child5_classId = Class_genId();
struct S233child5:S233{

  S233child5 (int a):a(a){
    (id = S233child5_classId);
{

    }
  }
  int a;
};


struct Trait233{

  int(*trait233)(Class*);
};

Vec* Trait233_v = newVec();

int Trait233_S233child1_trait233(Class* self_) {
  S233child1* self = ((S233child1*)self_);
{
    return (self -> a);
  }
} 
Trait233* newTrait233_S233child1() {
  Trait233 (* impl) = (new Trait233());
  setVec(Trait233_v, S233child1_classId, ((void*)impl));
  ((impl -> trait233) = (& Trait233_S233child1_trait233));
  return impl;
} 
Trait233* Trait233_S233child1_ = newTrait233_S233child1();

int Trait145_S233child1_trait145(Class* self_) {
  S233child1* self = ((S233child1*)self_);
{
    return (self -> a);
  }
} 
Trait145* newTrait145_S233child1() {
  Trait145 (* impl) = (new Trait145());
  setVec(Trait145_v, S233child1_classId, ((void*)impl));
  ((impl -> trait145) = (& Trait145_S233child1_trait145));
  return impl;
} 
Trait145* Trait145_S233child1_ = newTrait145_S233child1();

int S234_classId = Class_genId();
struct S234{

  int id;
  S234 ():id(S234_classId){

  }
};


int S234child1_classId = Class_genId();
struct S234child1:S234{

  S234child1 (int a):a(a){
    (id = S234child1_classId);
{

    }
  }
  int a;
};


int S234child2_classId = Class_genId();
struct S234child2:S234{

  S234child2 (int a):a(a){
    (id = S234child2_classId);
{

    }
  }
  int a;
};


int S234child3_classId = Class_genId();
struct S234child3:S234{

  S234child3 (int a):a(a){
    (id = S234child3_classId);
{

    }
  }
  int a;
};


struct Trait234{

  int(*trait234)(Class*);
};

Vec* Trait234_v = newVec();

int Trait234_S234child1_trait234(Class* self_) {
  S234child1* self = ((S234child1*)self_);
{
    return (self -> a);
  }
} 
Trait234* newTrait234_S234child1() {
  Trait234 (* impl) = (new Trait234());
  setVec(Trait234_v, S234child1_classId, ((void*)impl));
  ((impl -> trait234) = (& Trait234_S234child1_trait234));
  return impl;
} 
Trait234* Trait234_S234child1_ = newTrait234_S234child1();

int Trait234_S234child2_trait234(Class* self_) {
  S234child2* self = ((S234child2*)self_);
{
    return (self -> a);
  }
} 
Trait234* newTrait234_S234child2() {
  Trait234 (* impl) = (new Trait234());
  setVec(Trait234_v, S234child2_classId, ((void*)impl));
  ((impl -> trait234) = (& Trait234_S234child2_trait234));
  return impl;
} 
Trait234* Trait234_S234child2_ = newTrait234_S234child2();

int Trait234_S234child3_trait234(Class* self_) {
  S234child3* self = ((S234child3*)self_);
{
    return (self -> a);
  }
} 
Trait234* newTrait234_S234child3() {
  Trait234 (* impl) = (new Trait234());
  setVec(Trait234_v, S234child3_classId, ((void*)impl));
  ((impl -> trait234) = (& Trait234_S234child3_trait234));
  return impl;
} 
Trait234* Trait234_S234child3_ = newTrait234_S234child3();

int Trait6_S234child1_trait6(Class* self_) {
  S234child1* self = ((S234child1*)self_);
{
    return (self -> a);
  }
} 
Trait6* newTrait6_S234child1() {
  Trait6 (* impl) = (new Trait6());
  setVec(Trait6_v, S234child1_classId, ((void*)impl));
  ((impl -> trait6) = (& Trait6_S234child1_trait6));
  return impl;
} 
Trait6* Trait6_S234child1_ = newTrait6_S234child1();

int S235_classId = Class_genId();
struct S235{

  int id;
  S235 ():id(S235_classId){

  }
};


int S235child1_classId = Class_genId();
struct S235child1:S235{

  S235child1 (int a):a(a){
    (id = S235child1_classId);
{

    }
  }
  int a;
};


int S235child2_classId = Class_genId();
struct S235child2:S235{

  S235child2 (int a):a(a){
    (id = S235child2_classId);
{

    }
  }
  int a;
};


int S235child3_classId = Class_genId();
struct S235child3:S235{

  S235child3 (int a):a(a){
    (id = S235child3_classId);
{

    }
  }
  int a;
};


int S235child4_classId = Class_genId();
struct S235child4:S235{

  S235child4 (int a):a(a){
    (id = S235child4_classId);
{

    }
  }
  int a;
};


struct Trait235{

  int(*trait235)(Class*);
};

Vec* Trait235_v = newVec();

int Trait235_S235child1_trait235(Class* self_) {
  S235child1* self = ((S235child1*)self_);
{
    return (self -> a);
  }
} 
Trait235* newTrait235_S235child1() {
  Trait235 (* impl) = (new Trait235());
  setVec(Trait235_v, S235child1_classId, ((void*)impl));
  ((impl -> trait235) = (& Trait235_S235child1_trait235));
  return impl;
} 
Trait235* Trait235_S235child1_ = newTrait235_S235child1();

int Trait235_S235child2_trait235(Class* self_) {
  S235child2* self = ((S235child2*)self_);
{
    return (self -> a);
  }
} 
Trait235* newTrait235_S235child2() {
  Trait235 (* impl) = (new Trait235());
  setVec(Trait235_v, S235child2_classId, ((void*)impl));
  ((impl -> trait235) = (& Trait235_S235child2_trait235));
  return impl;
} 
Trait235* Trait235_S235child2_ = newTrait235_S235child2();

int Trait235_S235child3_trait235(Class* self_) {
  S235child3* self = ((S235child3*)self_);
{
    return (self -> a);
  }
} 
Trait235* newTrait235_S235child3() {
  Trait235 (* impl) = (new Trait235());
  setVec(Trait235_v, S235child3_classId, ((void*)impl));
  ((impl -> trait235) = (& Trait235_S235child3_trait235));
  return impl;
} 
Trait235* Trait235_S235child3_ = newTrait235_S235child3();

int Trait235_S235child4_trait235(Class* self_) {
  S235child4* self = ((S235child4*)self_);
{
    return (self -> a);
  }
} 
Trait235* newTrait235_S235child4() {
  Trait235 (* impl) = (new Trait235());
  setVec(Trait235_v, S235child4_classId, ((void*)impl));
  ((impl -> trait235) = (& Trait235_S235child4_trait235));
  return impl;
} 
Trait235* Trait235_S235child4_ = newTrait235_S235child4();

int Trait208_S235child1_trait208(Class* self_) {
  S235child1* self = ((S235child1*)self_);
{
    return (self -> a);
  }
} 
Trait208* newTrait208_S235child1() {
  Trait208 (* impl) = (new Trait208());
  setVec(Trait208_v, S235child1_classId, ((void*)impl));
  ((impl -> trait208) = (& Trait208_S235child1_trait208));
  return impl;
} 
Trait208* Trait208_S235child1_ = newTrait208_S235child1();

int S236_classId = Class_genId();
struct S236{

  int id;
  S236 ():id(S236_classId){

  }
};


int S236child1_classId = Class_genId();
struct S236child1:S236{

  S236child1 (int a):a(a){
    (id = S236child1_classId);
{

    }
  }
  int a;
};


int S236child2_classId = Class_genId();
struct S236child2:S236{

  S236child2 (int a):a(a){
    (id = S236child2_classId);
{

    }
  }
  int a;
};


int S236child3_classId = Class_genId();
struct S236child3:S236{

  S236child3 (int a):a(a){
    (id = S236child3_classId);
{

    }
  }
  int a;
};


int S236child4_classId = Class_genId();
struct S236child4:S236{

  S236child4 (int a):a(a){
    (id = S236child4_classId);
{

    }
  }
  int a;
};


struct Trait236{

  int(*trait236)(Class*);
};

Vec* Trait236_v = newVec();

int Trait236_S236child1_trait236(Class* self_) {
  S236child1* self = ((S236child1*)self_);
{
    return (self -> a);
  }
} 
Trait236* newTrait236_S236child1() {
  Trait236 (* impl) = (new Trait236());
  setVec(Trait236_v, S236child1_classId, ((void*)impl));
  ((impl -> trait236) = (& Trait236_S236child1_trait236));
  return impl;
} 
Trait236* Trait236_S236child1_ = newTrait236_S236child1();

int Trait236_S236child2_trait236(Class* self_) {
  S236child2* self = ((S236child2*)self_);
{
    return (self -> a);
  }
} 
Trait236* newTrait236_S236child2() {
  Trait236 (* impl) = (new Trait236());
  setVec(Trait236_v, S236child2_classId, ((void*)impl));
  ((impl -> trait236) = (& Trait236_S236child2_trait236));
  return impl;
} 
Trait236* Trait236_S236child2_ = newTrait236_S236child2();

int Trait236_S236child3_trait236(Class* self_) {
  S236child3* self = ((S236child3*)self_);
{
    return (self -> a);
  }
} 
Trait236* newTrait236_S236child3() {
  Trait236 (* impl) = (new Trait236());
  setVec(Trait236_v, S236child3_classId, ((void*)impl));
  ((impl -> trait236) = (& Trait236_S236child3_trait236));
  return impl;
} 
Trait236* Trait236_S236child3_ = newTrait236_S236child3();

int Trait185_S236child1_trait185(Class* self_) {
  S236child1* self = ((S236child1*)self_);
{
    return (self -> a);
  }
} 
Trait185* newTrait185_S236child1() {
  Trait185 (* impl) = (new Trait185());
  setVec(Trait185_v, S236child1_classId, ((void*)impl));
  ((impl -> trait185) = (& Trait185_S236child1_trait185));
  return impl;
} 
Trait185* Trait185_S236child1_ = newTrait185_S236child1();

int S237_classId = Class_genId();
struct S237{

  int id;
  S237 ():id(S237_classId){

  }
};


int S237child1_classId = Class_genId();
struct S237child1:S237{

  S237child1 (int a):a(a){
    (id = S237child1_classId);
{

    }
  }
  int a;
};


int S237child2_classId = Class_genId();
struct S237child2:S237{

  S237child2 (int a):a(a){
    (id = S237child2_classId);
{

    }
  }
  int a;
};


struct Trait237{

  int(*trait237)(Class*);
};

Vec* Trait237_v = newVec();

int Trait237_S237child1_trait237(Class* self_) {
  S237child1* self = ((S237child1*)self_);
{
    return (self -> a);
  }
} 
Trait237* newTrait237_S237child1() {
  Trait237 (* impl) = (new Trait237());
  setVec(Trait237_v, S237child1_classId, ((void*)impl));
  ((impl -> trait237) = (& Trait237_S237child1_trait237));
  return impl;
} 
Trait237* Trait237_S237child1_ = newTrait237_S237child1();

int Trait237_S237child2_trait237(Class* self_) {
  S237child2* self = ((S237child2*)self_);
{
    return (self -> a);
  }
} 
Trait237* newTrait237_S237child2() {
  Trait237 (* impl) = (new Trait237());
  setVec(Trait237_v, S237child2_classId, ((void*)impl));
  ((impl -> trait237) = (& Trait237_S237child2_trait237));
  return impl;
} 
Trait237* Trait237_S237child2_ = newTrait237_S237child2();

int Trait91_S237child1_trait91(Class* self_) {
  S237child1* self = ((S237child1*)self_);
{
    return (self -> a);
  }
} 
Trait91* newTrait91_S237child1() {
  Trait91 (* impl) = (new Trait91());
  setVec(Trait91_v, S237child1_classId, ((void*)impl));
  ((impl -> trait91) = (& Trait91_S237child1_trait91));
  return impl;
} 
Trait91* Trait91_S237child1_ = newTrait91_S237child1();

int S238_classId = Class_genId();
struct S238{

  int id;
  S238 ():id(S238_classId){

  }
};


int S238child1_classId = Class_genId();
struct S238child1:S238{

  S238child1 (int a):a(a){
    (id = S238child1_classId);
{

    }
  }
  int a;
};


int S238child2_classId = Class_genId();
struct S238child2:S238{

  S238child2 (int a):a(a){
    (id = S238child2_classId);
{

    }
  }
  int a;
};


int S238child3_classId = Class_genId();
struct S238child3:S238{

  S238child3 (int a):a(a){
    (id = S238child3_classId);
{

    }
  }
  int a;
};


struct Trait238{

  int(*trait238)(Class*);
};

Vec* Trait238_v = newVec();

int Trait238_S238child1_trait238(Class* self_) {
  S238child1* self = ((S238child1*)self_);
{
    return (self -> a);
  }
} 
Trait238* newTrait238_S238child1() {
  Trait238 (* impl) = (new Trait238());
  setVec(Trait238_v, S238child1_classId, ((void*)impl));
  ((impl -> trait238) = (& Trait238_S238child1_trait238));
  return impl;
} 
Trait238* Trait238_S238child1_ = newTrait238_S238child1();

int Trait238_S238child2_trait238(Class* self_) {
  S238child2* self = ((S238child2*)self_);
{
    return (self -> a);
  }
} 
Trait238* newTrait238_S238child2() {
  Trait238 (* impl) = (new Trait238());
  setVec(Trait238_v, S238child2_classId, ((void*)impl));
  ((impl -> trait238) = (& Trait238_S238child2_trait238));
  return impl;
} 
Trait238* Trait238_S238child2_ = newTrait238_S238child2();

int Trait163_S238child1_trait163(Class* self_) {
  S238child1* self = ((S238child1*)self_);
{
    return (self -> a);
  }
} 
Trait163* newTrait163_S238child1() {
  Trait163 (* impl) = (new Trait163());
  setVec(Trait163_v, S238child1_classId, ((void*)impl));
  ((impl -> trait163) = (& Trait163_S238child1_trait163));
  return impl;
} 
Trait163* Trait163_S238child1_ = newTrait163_S238child1();

int S239_classId = Class_genId();
struct S239{

  int id;
  S239 ():id(S239_classId){

  }
};


int S239child1_classId = Class_genId();
struct S239child1:S239{

  S239child1 (int a):a(a){
    (id = S239child1_classId);
{

    }
  }
  int a;
};


struct Trait239{

  int(*trait239)(Class*);
};

Vec* Trait239_v = newVec();

int Trait239_S239child1_trait239(Class* self_) {
  S239child1* self = ((S239child1*)self_);
{
    return (self -> a);
  }
} 
Trait239* newTrait239_S239child1() {
  Trait239 (* impl) = (new Trait239());
  setVec(Trait239_v, S239child1_classId, ((void*)impl));
  ((impl -> trait239) = (& Trait239_S239child1_trait239));
  return impl;
} 
Trait239* Trait239_S239child1_ = newTrait239_S239child1();

int Trait25_S239child1_trait25(Class* self_) {
  S239child1* self = ((S239child1*)self_);
{
    return (self -> a);
  }
} 
Trait25* newTrait25_S239child1() {
  Trait25 (* impl) = (new Trait25());
  setVec(Trait25_v, S239child1_classId, ((void*)impl));
  ((impl -> trait25) = (& Trait25_S239child1_trait25));
  return impl;
} 
Trait25* Trait25_S239child1_ = newTrait25_S239child1();

int S240_classId = Class_genId();
struct S240{

  int id;
  S240 ():id(S240_classId){

  }
};


int S240child1_classId = Class_genId();
struct S240child1:S240{

  S240child1 (int a):a(a){
    (id = S240child1_classId);
{

    }
  }
  int a;
};


int S240child2_classId = Class_genId();
struct S240child2:S240{

  S240child2 (int a):a(a){
    (id = S240child2_classId);
{

    }
  }
  int a;
};


int S240child3_classId = Class_genId();
struct S240child3:S240{

  S240child3 (int a):a(a){
    (id = S240child3_classId);
{

    }
  }
  int a;
};


struct Trait240{

  int(*trait240)(Class*);
};

Vec* Trait240_v = newVec();

int Trait240_S240child1_trait240(Class* self_) {
  S240child1* self = ((S240child1*)self_);
{
    return (self -> a);
  }
} 
Trait240* newTrait240_S240child1() {
  Trait240 (* impl) = (new Trait240());
  setVec(Trait240_v, S240child1_classId, ((void*)impl));
  ((impl -> trait240) = (& Trait240_S240child1_trait240));
  return impl;
} 
Trait240* Trait240_S240child1_ = newTrait240_S240child1();

int Trait240_S240child2_trait240(Class* self_) {
  S240child2* self = ((S240child2*)self_);
{
    return (self -> a);
  }
} 
Trait240* newTrait240_S240child2() {
  Trait240 (* impl) = (new Trait240());
  setVec(Trait240_v, S240child2_classId, ((void*)impl));
  ((impl -> trait240) = (& Trait240_S240child2_trait240));
  return impl;
} 
Trait240* Trait240_S240child2_ = newTrait240_S240child2();

int Trait240_S240child3_trait240(Class* self_) {
  S240child3* self = ((S240child3*)self_);
{
    return (self -> a);
  }
} 
Trait240* newTrait240_S240child3() {
  Trait240 (* impl) = (new Trait240());
  setVec(Trait240_v, S240child3_classId, ((void*)impl));
  ((impl -> trait240) = (& Trait240_S240child3_trait240));
  return impl;
} 
Trait240* Trait240_S240child3_ = newTrait240_S240child3();

int Trait70_S240child1_trait70(Class* self_) {
  S240child1* self = ((S240child1*)self_);
{
    return (self -> a);
  }
} 
Trait70* newTrait70_S240child1() {
  Trait70 (* impl) = (new Trait70());
  setVec(Trait70_v, S240child1_classId, ((void*)impl));
  ((impl -> trait70) = (& Trait70_S240child1_trait70));
  return impl;
} 
Trait70* Trait70_S240child1_ = newTrait70_S240child1();

int S241_classId = Class_genId();
struct S241{

  int id;
  S241 ():id(S241_classId){

  }
};


int S241child1_classId = Class_genId();
struct S241child1:S241{

  S241child1 (int a):a(a){
    (id = S241child1_classId);
{

    }
  }
  int a;
};


int S241child2_classId = Class_genId();
struct S241child2:S241{

  S241child2 (int a):a(a){
    (id = S241child2_classId);
{

    }
  }
  int a;
};


int S241child3_classId = Class_genId();
struct S241child3:S241{

  S241child3 (int a):a(a){
    (id = S241child3_classId);
{

    }
  }
  int a;
};


struct Trait241{

  int(*trait241)(Class*);
};

Vec* Trait241_v = newVec();

int Trait241_S241child1_trait241(Class* self_) {
  S241child1* self = ((S241child1*)self_);
{
    return (self -> a);
  }
} 
Trait241* newTrait241_S241child1() {
  Trait241 (* impl) = (new Trait241());
  setVec(Trait241_v, S241child1_classId, ((void*)impl));
  ((impl -> trait241) = (& Trait241_S241child1_trait241));
  return impl;
} 
Trait241* Trait241_S241child1_ = newTrait241_S241child1();

int Trait241_S241child2_trait241(Class* self_) {
  S241child2* self = ((S241child2*)self_);
{
    return (self -> a);
  }
} 
Trait241* newTrait241_S241child2() {
  Trait241 (* impl) = (new Trait241());
  setVec(Trait241_v, S241child2_classId, ((void*)impl));
  ((impl -> trait241) = (& Trait241_S241child2_trait241));
  return impl;
} 
Trait241* Trait241_S241child2_ = newTrait241_S241child2();

int Trait20_S241child1_trait20(Class* self_) {
  S241child1* self = ((S241child1*)self_);
{
    return (self -> a);
  }
} 
Trait20* newTrait20_S241child1() {
  Trait20 (* impl) = (new Trait20());
  setVec(Trait20_v, S241child1_classId, ((void*)impl));
  ((impl -> trait20) = (& Trait20_S241child1_trait20));
  return impl;
} 
Trait20* Trait20_S241child1_ = newTrait20_S241child1();

int S242_classId = Class_genId();
struct S242{

  int id;
  S242 ():id(S242_classId){

  }
};


int S242child1_classId = Class_genId();
struct S242child1:S242{

  S242child1 (int a):a(a){
    (id = S242child1_classId);
{

    }
  }
  int a;
};


struct Trait242{

  int(*trait242)(Class*);
};

Vec* Trait242_v = newVec();

int Trait242_S242child1_trait242(Class* self_) {
  S242child1* self = ((S242child1*)self_);
{
    return (self -> a);
  }
} 
Trait242* newTrait242_S242child1() {
  Trait242 (* impl) = (new Trait242());
  setVec(Trait242_v, S242child1_classId, ((void*)impl));
  ((impl -> trait242) = (& Trait242_S242child1_trait242));
  return impl;
} 
Trait242* Trait242_S242child1_ = newTrait242_S242child1();

int Trait48_S242child1_trait48(Class* self_) {
  S242child1* self = ((S242child1*)self_);
{
    return (self -> a);
  }
} 
Trait48* newTrait48_S242child1() {
  Trait48 (* impl) = (new Trait48());
  setVec(Trait48_v, S242child1_classId, ((void*)impl));
  ((impl -> trait48) = (& Trait48_S242child1_trait48));
  return impl;
} 
Trait48* Trait48_S242child1_ = newTrait48_S242child1();

int S243_classId = Class_genId();
struct S243{

  int id;
  S243 ():id(S243_classId){

  }
};


int S243child1_classId = Class_genId();
struct S243child1:S243{

  S243child1 (int a):a(a){
    (id = S243child1_classId);
{

    }
  }
  int a;
};


struct Trait243{

  int(*trait243)(Class*);
};

Vec* Trait243_v = newVec();

int Trait243_S243child1_trait243(Class* self_) {
  S243child1* self = ((S243child1*)self_);
{
    return (self -> a);
  }
} 
Trait243* newTrait243_S243child1() {
  Trait243 (* impl) = (new Trait243());
  setVec(Trait243_v, S243child1_classId, ((void*)impl));
  ((impl -> trait243) = (& Trait243_S243child1_trait243));
  return impl;
} 
Trait243* Trait243_S243child1_ = newTrait243_S243child1();

int Trait36_S243child1_trait36(Class* self_) {
  S243child1* self = ((S243child1*)self_);
{
    return (self -> a);
  }
} 
Trait36* newTrait36_S243child1() {
  Trait36 (* impl) = (new Trait36());
  setVec(Trait36_v, S243child1_classId, ((void*)impl));
  ((impl -> trait36) = (& Trait36_S243child1_trait36));
  return impl;
} 
Trait36* Trait36_S243child1_ = newTrait36_S243child1();

int S244_classId = Class_genId();
struct S244{

  int id;
  S244 ():id(S244_classId){

  }
};


int S244child1_classId = Class_genId();
struct S244child1:S244{

  S244child1 (int a):a(a){
    (id = S244child1_classId);
{

    }
  }
  int a;
};


int S244child2_classId = Class_genId();
struct S244child2:S244{

  S244child2 (int a):a(a){
    (id = S244child2_classId);
{

    }
  }
  int a;
};


struct Trait244{

  int(*trait244)(Class*);
};

Vec* Trait244_v = newVec();

int Trait244_S244child1_trait244(Class* self_) {
  S244child1* self = ((S244child1*)self_);
{
    return (self -> a);
  }
} 
Trait244* newTrait244_S244child1() {
  Trait244 (* impl) = (new Trait244());
  setVec(Trait244_v, S244child1_classId, ((void*)impl));
  ((impl -> trait244) = (& Trait244_S244child1_trait244));
  return impl;
} 
Trait244* Trait244_S244child1_ = newTrait244_S244child1();

int Trait244_S244child2_trait244(Class* self_) {
  S244child2* self = ((S244child2*)self_);
{
    return (self -> a);
  }
} 
Trait244* newTrait244_S244child2() {
  Trait244 (* impl) = (new Trait244());
  setVec(Trait244_v, S244child2_classId, ((void*)impl));
  ((impl -> trait244) = (& Trait244_S244child2_trait244));
  return impl;
} 
Trait244* Trait244_S244child2_ = newTrait244_S244child2();

int Trait105_S244child1_trait105(Class* self_) {
  S244child1* self = ((S244child1*)self_);
{
    return (self -> a);
  }
} 
Trait105* newTrait105_S244child1() {
  Trait105 (* impl) = (new Trait105());
  setVec(Trait105_v, S244child1_classId, ((void*)impl));
  ((impl -> trait105) = (& Trait105_S244child1_trait105));
  return impl;
} 
Trait105* Trait105_S244child1_ = newTrait105_S244child1();

int S245_classId = Class_genId();
struct S245{

  int id;
  S245 ():id(S245_classId){

  }
};


int S245child1_classId = Class_genId();
struct S245child1:S245{

  S245child1 (int a):a(a){
    (id = S245child1_classId);
{

    }
  }
  int a;
};


int S245child2_classId = Class_genId();
struct S245child2:S245{

  S245child2 (int a):a(a){
    (id = S245child2_classId);
{

    }
  }
  int a;
};


struct Trait245{

  int(*trait245)(Class*);
};

Vec* Trait245_v = newVec();

int Trait245_S245child1_trait245(Class* self_) {
  S245child1* self = ((S245child1*)self_);
{
    return (self -> a);
  }
} 
Trait245* newTrait245_S245child1() {
  Trait245 (* impl) = (new Trait245());
  setVec(Trait245_v, S245child1_classId, ((void*)impl));
  ((impl -> trait245) = (& Trait245_S245child1_trait245));
  return impl;
} 
Trait245* Trait245_S245child1_ = newTrait245_S245child1();

int Trait89_S245child1_trait89(Class* self_) {
  S245child1* self = ((S245child1*)self_);
{
    return (self -> a);
  }
} 
Trait89* newTrait89_S245child1() {
  Trait89 (* impl) = (new Trait89());
  setVec(Trait89_v, S245child1_classId, ((void*)impl));
  ((impl -> trait89) = (& Trait89_S245child1_trait89));
  return impl;
} 
Trait89* Trait89_S245child1_ = newTrait89_S245child1();

int S246_classId = Class_genId();
struct S246{

  int id;
  S246 ():id(S246_classId){

  }
};


int S246child1_classId = Class_genId();
struct S246child1:S246{

  S246child1 (int a):a(a){
    (id = S246child1_classId);
{

    }
  }
  int a;
};


int S246child2_classId = Class_genId();
struct S246child2:S246{

  S246child2 (int a):a(a){
    (id = S246child2_classId);
{

    }
  }
  int a;
};


int S246child3_classId = Class_genId();
struct S246child3:S246{

  S246child3 (int a):a(a){
    (id = S246child3_classId);
{

    }
  }
  int a;
};


struct Trait246{

  int(*trait246)(Class*);
};

Vec* Trait246_v = newVec();

int Trait246_S246child1_trait246(Class* self_) {
  S246child1* self = ((S246child1*)self_);
{
    return (self -> a);
  }
} 
Trait246* newTrait246_S246child1() {
  Trait246 (* impl) = (new Trait246());
  setVec(Trait246_v, S246child1_classId, ((void*)impl));
  ((impl -> trait246) = (& Trait246_S246child1_trait246));
  return impl;
} 
Trait246* Trait246_S246child1_ = newTrait246_S246child1();

int Trait246_S246child2_trait246(Class* self_) {
  S246child2* self = ((S246child2*)self_);
{
    return (self -> a);
  }
} 
Trait246* newTrait246_S246child2() {
  Trait246 (* impl) = (new Trait246());
  setVec(Trait246_v, S246child2_classId, ((void*)impl));
  ((impl -> trait246) = (& Trait246_S246child2_trait246));
  return impl;
} 
Trait246* Trait246_S246child2_ = newTrait246_S246child2();

int Trait246_S246child3_trait246(Class* self_) {
  S246child3* self = ((S246child3*)self_);
{
    return (self -> a);
  }
} 
Trait246* newTrait246_S246child3() {
  Trait246 (* impl) = (new Trait246());
  setVec(Trait246_v, S246child3_classId, ((void*)impl));
  ((impl -> trait246) = (& Trait246_S246child3_trait246));
  return impl;
} 
Trait246* Trait246_S246child3_ = newTrait246_S246child3();

int Trait73_S246child1_trait73(Class* self_) {
  S246child1* self = ((S246child1*)self_);
{
    return (self -> a);
  }
} 
Trait73* newTrait73_S246child1() {
  Trait73 (* impl) = (new Trait73());
  setVec(Trait73_v, S246child1_classId, ((void*)impl));
  ((impl -> trait73) = (& Trait73_S246child1_trait73));
  return impl;
} 
Trait73* Trait73_S246child1_ = newTrait73_S246child1();

int S247_classId = Class_genId();
struct S247{

  int id;
  S247 ():id(S247_classId){

  }
};


int S247child1_classId = Class_genId();
struct S247child1:S247{

  S247child1 (int a):a(a){
    (id = S247child1_classId);
{

    }
  }
  int a;
};


int S247child2_classId = Class_genId();
struct S247child2:S247{

  S247child2 (int a):a(a){
    (id = S247child2_classId);
{

    }
  }
  int a;
};


int S247child3_classId = Class_genId();
struct S247child3:S247{

  S247child3 (int a):a(a){
    (id = S247child3_classId);
{

    }
  }
  int a;
};


struct Trait247{

  int(*trait247)(Class*);
};

Vec* Trait247_v = newVec();

int Trait247_S247child1_trait247(Class* self_) {
  S247child1* self = ((S247child1*)self_);
{
    return (self -> a);
  }
} 
Trait247* newTrait247_S247child1() {
  Trait247 (* impl) = (new Trait247());
  setVec(Trait247_v, S247child1_classId, ((void*)impl));
  ((impl -> trait247) = (& Trait247_S247child1_trait247));
  return impl;
} 
Trait247* Trait247_S247child1_ = newTrait247_S247child1();

int Trait247_S247child2_trait247(Class* self_) {
  S247child2* self = ((S247child2*)self_);
{
    return (self -> a);
  }
} 
Trait247* newTrait247_S247child2() {
  Trait247 (* impl) = (new Trait247());
  setVec(Trait247_v, S247child2_classId, ((void*)impl));
  ((impl -> trait247) = (& Trait247_S247child2_trait247));
  return impl;
} 
Trait247* Trait247_S247child2_ = newTrait247_S247child2();

int Trait188_S247child1_trait188(Class* self_) {
  S247child1* self = ((S247child1*)self_);
{
    return (self -> a);
  }
} 
Trait188* newTrait188_S247child1() {
  Trait188 (* impl) = (new Trait188());
  setVec(Trait188_v, S247child1_classId, ((void*)impl));
  ((impl -> trait188) = (& Trait188_S247child1_trait188));
  return impl;
} 
Trait188* Trait188_S247child1_ = newTrait188_S247child1();

int S248_classId = Class_genId();
struct S248{

  int id;
  S248 ():id(S248_classId){

  }
};


int S248child1_classId = Class_genId();
struct S248child1:S248{

  S248child1 (int a):a(a){
    (id = S248child1_classId);
{

    }
  }
  int a;
};


int S248child2_classId = Class_genId();
struct S248child2:S248{

  S248child2 (int a):a(a){
    (id = S248child2_classId);
{

    }
  }
  int a;
};


struct Trait248{

  int(*trait248)(Class*);
};

Vec* Trait248_v = newVec();

int Trait248_S248child1_trait248(Class* self_) {
  S248child1* self = ((S248child1*)self_);
{
    return (self -> a);
  }
} 
Trait248* newTrait248_S248child1() {
  Trait248 (* impl) = (new Trait248());
  setVec(Trait248_v, S248child1_classId, ((void*)impl));
  ((impl -> trait248) = (& Trait248_S248child1_trait248));
  return impl;
} 
Trait248* Trait248_S248child1_ = newTrait248_S248child1();

int Trait163_S248child1_trait163(Class* self_) {
  S248child1* self = ((S248child1*)self_);
{
    return (self -> a);
  }
} 
Trait163* newTrait163_S248child1() {
  Trait163 (* impl) = (new Trait163());
  setVec(Trait163_v, S248child1_classId, ((void*)impl));
  ((impl -> trait163) = (& Trait163_S248child1_trait163));
  return impl;
} 
Trait163* Trait163_S248child1_ = newTrait163_S248child1();

int S249_classId = Class_genId();
struct S249{

  int id;
  S249 ():id(S249_classId){

  }
};


int S249child1_classId = Class_genId();
struct S249child1:S249{

  S249child1 (int a):a(a){
    (id = S249child1_classId);
{

    }
  }
  int a;
};


int S249child2_classId = Class_genId();
struct S249child2:S249{

  S249child2 (int a):a(a){
    (id = S249child2_classId);
{

    }
  }
  int a;
};


int S249child3_classId = Class_genId();
struct S249child3:S249{

  S249child3 (int a):a(a){
    (id = S249child3_classId);
{

    }
  }
  int a;
};


struct Trait249{

  int(*trait249)(Class*);
};

Vec* Trait249_v = newVec();

int Trait249_S249child1_trait249(Class* self_) {
  S249child1* self = ((S249child1*)self_);
{
    return (self -> a);
  }
} 
Trait249* newTrait249_S249child1() {
  Trait249 (* impl) = (new Trait249());
  setVec(Trait249_v, S249child1_classId, ((void*)impl));
  ((impl -> trait249) = (& Trait249_S249child1_trait249));
  return impl;
} 
Trait249* Trait249_S249child1_ = newTrait249_S249child1();

int Trait249_S249child2_trait249(Class* self_) {
  S249child2* self = ((S249child2*)self_);
{
    return (self -> a);
  }
} 
Trait249* newTrait249_S249child2() {
  Trait249 (* impl) = (new Trait249());
  setVec(Trait249_v, S249child2_classId, ((void*)impl));
  ((impl -> trait249) = (& Trait249_S249child2_trait249));
  return impl;
} 
Trait249* Trait249_S249child2_ = newTrait249_S249child2();

int Trait65_S249child1_trait65(Class* self_) {
  S249child1* self = ((S249child1*)self_);
{
    return (self -> a);
  }
} 
Trait65* newTrait65_S249child1() {
  Trait65 (* impl) = (new Trait65());
  setVec(Trait65_v, S249child1_classId, ((void*)impl));
  ((impl -> trait65) = (& Trait65_S249child1_trait65));
  return impl;
} 
Trait65* Trait65_S249child1_ = newTrait65_S249child1();

int S250_classId = Class_genId();
struct S250{

  int id;
  S250 ():id(S250_classId){

  }
};


int S250child1_classId = Class_genId();
struct S250child1:S250{

  S250child1 (int a):a(a){
    (id = S250child1_classId);
{

    }
  }
  int a;
};


int S250child2_classId = Class_genId();
struct S250child2:S250{

  S250child2 (int a):a(a){
    (id = S250child2_classId);
{

    }
  }
  int a;
};


struct Trait250{

  int(*trait250)(Class*);
};

Vec* Trait250_v = newVec();

int Trait250_S250child1_trait250(Class* self_) {
  S250child1* self = ((S250child1*)self_);
{
    return (self -> a);
  }
} 
Trait250* newTrait250_S250child1() {
  Trait250 (* impl) = (new Trait250());
  setVec(Trait250_v, S250child1_classId, ((void*)impl));
  ((impl -> trait250) = (& Trait250_S250child1_trait250));
  return impl;
} 
Trait250* Trait250_S250child1_ = newTrait250_S250child1();

int Trait146_S250child1_trait146(Class* self_) {
  S250child1* self = ((S250child1*)self_);
{
    return (self -> a);
  }
} 
Trait146* newTrait146_S250child1() {
  Trait146 (* impl) = (new Trait146());
  setVec(Trait146_v, S250child1_classId, ((void*)impl));
  ((impl -> trait146) = (& Trait146_S250child1_trait146));
  return impl;
} 
Trait146* Trait146_S250child1_ = newTrait146_S250child1();

int S251_classId = Class_genId();
struct S251{

  int id;
  S251 ():id(S251_classId){

  }
};


int S251child1_classId = Class_genId();
struct S251child1:S251{

  S251child1 (int a):a(a){
    (id = S251child1_classId);
{

    }
  }
  int a;
};


int S251child2_classId = Class_genId();
struct S251child2:S251{

  S251child2 (int a):a(a){
    (id = S251child2_classId);
{

    }
  }
  int a;
};


int S251child3_classId = Class_genId();
struct S251child3:S251{

  S251child3 (int a):a(a){
    (id = S251child3_classId);
{

    }
  }
  int a;
};


struct Trait251{

  int(*trait251)(Class*);
};

Vec* Trait251_v = newVec();

int Trait251_S251child1_trait251(Class* self_) {
  S251child1* self = ((S251child1*)self_);
{
    return (self -> a);
  }
} 
Trait251* newTrait251_S251child1() {
  Trait251 (* impl) = (new Trait251());
  setVec(Trait251_v, S251child1_classId, ((void*)impl));
  ((impl -> trait251) = (& Trait251_S251child1_trait251));
  return impl;
} 
Trait251* Trait251_S251child1_ = newTrait251_S251child1();

int Trait251_S251child2_trait251(Class* self_) {
  S251child2* self = ((S251child2*)self_);
{
    return (self -> a);
  }
} 
Trait251* newTrait251_S251child2() {
  Trait251 (* impl) = (new Trait251());
  setVec(Trait251_v, S251child2_classId, ((void*)impl));
  ((impl -> trait251) = (& Trait251_S251child2_trait251));
  return impl;
} 
Trait251* Trait251_S251child2_ = newTrait251_S251child2();

int Trait130_S251child1_trait130(Class* self_) {
  S251child1* self = ((S251child1*)self_);
{
    return (self -> a);
  }
} 
Trait130* newTrait130_S251child1() {
  Trait130 (* impl) = (new Trait130());
  setVec(Trait130_v, S251child1_classId, ((void*)impl));
  ((impl -> trait130) = (& Trait130_S251child1_trait130));
  return impl;
} 
Trait130* Trait130_S251child1_ = newTrait130_S251child1();

int S252_classId = Class_genId();
struct S252{

  int id;
  S252 ():id(S252_classId){

  }
};


int S252child1_classId = Class_genId();
struct S252child1:S252{

  S252child1 (int a):a(a){
    (id = S252child1_classId);
{

    }
  }
  int a;
};


struct Trait252{

  int(*trait252)(Class*);
};

Vec* Trait252_v = newVec();

int Trait252_S252child1_trait252(Class* self_) {
  S252child1* self = ((S252child1*)self_);
{
    return (self -> a);
  }
} 
Trait252* newTrait252_S252child1() {
  Trait252 (* impl) = (new Trait252());
  setVec(Trait252_v, S252child1_classId, ((void*)impl));
  ((impl -> trait252) = (& Trait252_S252child1_trait252));
  return impl;
} 
Trait252* Trait252_S252child1_ = newTrait252_S252child1();

int Trait220_S252child1_trait220(Class* self_) {
  S252child1* self = ((S252child1*)self_);
{
    return (self -> a);
  }
} 
Trait220* newTrait220_S252child1() {
  Trait220 (* impl) = (new Trait220());
  setVec(Trait220_v, S252child1_classId, ((void*)impl));
  ((impl -> trait220) = (& Trait220_S252child1_trait220));
  return impl;
} 
Trait220* Trait220_S252child1_ = newTrait220_S252child1();

int S253_classId = Class_genId();
struct S253{

  int id;
  S253 ():id(S253_classId){

  }
};


int S253child1_classId = Class_genId();
struct S253child1:S253{

  S253child1 (int a):a(a){
    (id = S253child1_classId);
{

    }
  }
  int a;
};


struct Trait253{

  int(*trait253)(Class*);
};

Vec* Trait253_v = newVec();

int Trait253_S253child1_trait253(Class* self_) {
  S253child1* self = ((S253child1*)self_);
{
    return (self -> a);
  }
} 
Trait253* newTrait253_S253child1() {
  Trait253 (* impl) = (new Trait253());
  setVec(Trait253_v, S253child1_classId, ((void*)impl));
  ((impl -> trait253) = (& Trait253_S253child1_trait253));
  return impl;
} 
Trait253* Trait253_S253child1_ = newTrait253_S253child1();

int Trait214_S253child1_trait214(Class* self_) {
  S253child1* self = ((S253child1*)self_);
{
    return (self -> a);
  }
} 
Trait214* newTrait214_S253child1() {
  Trait214 (* impl) = (new Trait214());
  setVec(Trait214_v, S253child1_classId, ((void*)impl));
  ((impl -> trait214) = (& Trait214_S253child1_trait214));
  return impl;
} 
Trait214* Trait214_S253child1_ = newTrait214_S253child1();

int S254_classId = Class_genId();
struct S254{

  int id;
  S254 ():id(S254_classId){

  }
};


int S254child1_classId = Class_genId();
struct S254child1:S254{

  S254child1 (int a):a(a){
    (id = S254child1_classId);
{

    }
  }
  int a;
};


int S254child2_classId = Class_genId();
struct S254child2:S254{

  S254child2 (int a):a(a){
    (id = S254child2_classId);
{

    }
  }
  int a;
};


int S254child3_classId = Class_genId();
struct S254child3:S254{

  S254child3 (int a):a(a){
    (id = S254child3_classId);
{

    }
  }
  int a;
};


int S254child4_classId = Class_genId();
struct S254child4:S254{

  S254child4 (int a):a(a){
    (id = S254child4_classId);
{

    }
  }
  int a;
};


struct Trait254{

  int(*trait254)(Class*);
};

Vec* Trait254_v = newVec();

int Trait254_S254child1_trait254(Class* self_) {
  S254child1* self = ((S254child1*)self_);
{
    return (self -> a);
  }
} 
Trait254* newTrait254_S254child1() {
  Trait254 (* impl) = (new Trait254());
  setVec(Trait254_v, S254child1_classId, ((void*)impl));
  ((impl -> trait254) = (& Trait254_S254child1_trait254));
  return impl;
} 
Trait254* Trait254_S254child1_ = newTrait254_S254child1();

int Trait254_S254child2_trait254(Class* self_) {
  S254child2* self = ((S254child2*)self_);
{
    return (self -> a);
  }
} 
Trait254* newTrait254_S254child2() {
  Trait254 (* impl) = (new Trait254());
  setVec(Trait254_v, S254child2_classId, ((void*)impl));
  ((impl -> trait254) = (& Trait254_S254child2_trait254));
  return impl;
} 
Trait254* Trait254_S254child2_ = newTrait254_S254child2();

int Trait1_S254child1_trait1(Class* self_) {
  S254child1* self = ((S254child1*)self_);
{
    return (self -> a);
  }
} 
Trait1* newTrait1_S254child1() {
  Trait1 (* impl) = (new Trait1());
  setVec(Trait1_v, S254child1_classId, ((void*)impl));
  ((impl -> trait1) = (& Trait1_S254child1_trait1));
  return impl;
} 
Trait1* Trait1_S254child1_ = newTrait1_S254child1();

int S255_classId = Class_genId();
struct S255{

  int id;
  S255 ():id(S255_classId){

  }
};


int S255child1_classId = Class_genId();
struct S255child1:S255{

  S255child1 (int a):a(a){
    (id = S255child1_classId);
{

    }
  }
  int a;
};


int S255child2_classId = Class_genId();
struct S255child2:S255{

  S255child2 (int a):a(a){
    (id = S255child2_classId);
{

    }
  }
  int a;
};


struct Trait255{

  int(*trait255)(Class*);
};

Vec* Trait255_v = newVec();

int Trait255_S255child1_trait255(Class* self_) {
  S255child1* self = ((S255child1*)self_);
{
    return (self -> a);
  }
} 
Trait255* newTrait255_S255child1() {
  Trait255 (* impl) = (new Trait255());
  setVec(Trait255_v, S255child1_classId, ((void*)impl));
  ((impl -> trait255) = (& Trait255_S255child1_trait255));
  return impl;
} 
Trait255* Trait255_S255child1_ = newTrait255_S255child1();

int Trait255_S255child2_trait255(Class* self_) {
  S255child2* self = ((S255child2*)self_);
{
    return (self -> a);
  }
} 
Trait255* newTrait255_S255child2() {
  Trait255 (* impl) = (new Trait255());
  setVec(Trait255_v, S255child2_classId, ((void*)impl));
  ((impl -> trait255) = (& Trait255_S255child2_trait255));
  return impl;
} 
Trait255* Trait255_S255child2_ = newTrait255_S255child2();

int Trait227_S255child1_trait227(Class* self_) {
  S255child1* self = ((S255child1*)self_);
{
    return (self -> a);
  }
} 
Trait227* newTrait227_S255child1() {
  Trait227 (* impl) = (new Trait227());
  setVec(Trait227_v, S255child1_classId, ((void*)impl));
  ((impl -> trait227) = (& Trait227_S255child1_trait227));
  return impl;
} 
Trait227* Trait227_S255child1_ = newTrait227_S255child1();

int S256_classId = Class_genId();
struct S256{

  int id;
  S256 ():id(S256_classId){

  }
};


int S256child1_classId = Class_genId();
struct S256child1:S256{

  S256child1 (int a):a(a){
    (id = S256child1_classId);
{

    }
  }
  int a;
};


struct Trait256{

  int(*trait256)(Class*);
};

Vec* Trait256_v = newVec();

int Trait256_S256child1_trait256(Class* self_) {
  S256child1* self = ((S256child1*)self_);
{
    return (self -> a);
  }
} 
Trait256* newTrait256_S256child1() {
  Trait256 (* impl) = (new Trait256());
  setVec(Trait256_v, S256child1_classId, ((void*)impl));
  ((impl -> trait256) = (& Trait256_S256child1_trait256));
  return impl;
} 
Trait256* Trait256_S256child1_ = newTrait256_S256child1();

int Trait144_S256child1_trait144(Class* self_) {
  S256child1* self = ((S256child1*)self_);
{
    return (self -> a);
  }
} 
Trait144* newTrait144_S256child1() {
  Trait144 (* impl) = (new Trait144());
  setVec(Trait144_v, S256child1_classId, ((void*)impl));
  ((impl -> trait144) = (& Trait144_S256child1_trait144));
  return impl;
} 
Trait144* Trait144_S256child1_ = newTrait144_S256child1();

int S257_classId = Class_genId();
struct S257{

  int id;
  S257 ():id(S257_classId){

  }
};


int S257child1_classId = Class_genId();
struct S257child1:S257{

  S257child1 (int a):a(a){
    (id = S257child1_classId);
{

    }
  }
  int a;
};


int S257child2_classId = Class_genId();
struct S257child2:S257{

  S257child2 (int a):a(a){
    (id = S257child2_classId);
{

    }
  }
  int a;
};


int S257child3_classId = Class_genId();
struct S257child3:S257{

  S257child3 (int a):a(a){
    (id = S257child3_classId);
{

    }
  }
  int a;
};


int S257child4_classId = Class_genId();
struct S257child4:S257{

  S257child4 (int a):a(a){
    (id = S257child4_classId);
{

    }
  }
  int a;
};


struct Trait257{

  int(*trait257)(Class*);
};

Vec* Trait257_v = newVec();

int Trait257_S257child1_trait257(Class* self_) {
  S257child1* self = ((S257child1*)self_);
{
    return (self -> a);
  }
} 
Trait257* newTrait257_S257child1() {
  Trait257 (* impl) = (new Trait257());
  setVec(Trait257_v, S257child1_classId, ((void*)impl));
  ((impl -> trait257) = (& Trait257_S257child1_trait257));
  return impl;
} 
Trait257* Trait257_S257child1_ = newTrait257_S257child1();

int Trait257_S257child2_trait257(Class* self_) {
  S257child2* self = ((S257child2*)self_);
{
    return (self -> a);
  }
} 
Trait257* newTrait257_S257child2() {
  Trait257 (* impl) = (new Trait257());
  setVec(Trait257_v, S257child2_classId, ((void*)impl));
  ((impl -> trait257) = (& Trait257_S257child2_trait257));
  return impl;
} 
Trait257* Trait257_S257child2_ = newTrait257_S257child2();

int Trait257_S257child3_trait257(Class* self_) {
  S257child3* self = ((S257child3*)self_);
{
    return (self -> a);
  }
} 
Trait257* newTrait257_S257child3() {
  Trait257 (* impl) = (new Trait257());
  setVec(Trait257_v, S257child3_classId, ((void*)impl));
  ((impl -> trait257) = (& Trait257_S257child3_trait257));
  return impl;
} 
Trait257* Trait257_S257child3_ = newTrait257_S257child3();

int Trait160_S257child1_trait160(Class* self_) {
  S257child1* self = ((S257child1*)self_);
{
    return (self -> a);
  }
} 
Trait160* newTrait160_S257child1() {
  Trait160 (* impl) = (new Trait160());
  setVec(Trait160_v, S257child1_classId, ((void*)impl));
  ((impl -> trait160) = (& Trait160_S257child1_trait160));
  return impl;
} 
Trait160* Trait160_S257child1_ = newTrait160_S257child1();

int S258_classId = Class_genId();
struct S258{

  int id;
  S258 ():id(S258_classId){

  }
};


int S258child1_classId = Class_genId();
struct S258child1:S258{

  S258child1 (int a):a(a){
    (id = S258child1_classId);
{

    }
  }
  int a;
};


int S258child2_classId = Class_genId();
struct S258child2:S258{

  S258child2 (int a):a(a){
    (id = S258child2_classId);
{

    }
  }
  int a;
};


int S258child3_classId = Class_genId();
struct S258child3:S258{

  S258child3 (int a):a(a){
    (id = S258child3_classId);
{

    }
  }
  int a;
};


int S258child4_classId = Class_genId();
struct S258child4:S258{

  S258child4 (int a):a(a){
    (id = S258child4_classId);
{

    }
  }
  int a;
};


struct Trait258{

  int(*trait258)(Class*);
};

Vec* Trait258_v = newVec();

int Trait258_S258child1_trait258(Class* self_) {
  S258child1* self = ((S258child1*)self_);
{
    return (self -> a);
  }
} 
Trait258* newTrait258_S258child1() {
  Trait258 (* impl) = (new Trait258());
  setVec(Trait258_v, S258child1_classId, ((void*)impl));
  ((impl -> trait258) = (& Trait258_S258child1_trait258));
  return impl;
} 
Trait258* Trait258_S258child1_ = newTrait258_S258child1();

int Trait107_S258child1_trait107(Class* self_) {
  S258child1* self = ((S258child1*)self_);
{
    return (self -> a);
  }
} 
Trait107* newTrait107_S258child1() {
  Trait107 (* impl) = (new Trait107());
  setVec(Trait107_v, S258child1_classId, ((void*)impl));
  ((impl -> trait107) = (& Trait107_S258child1_trait107));
  return impl;
} 
Trait107* Trait107_S258child1_ = newTrait107_S258child1();

int S259_classId = Class_genId();
struct S259{

  int id;
  S259 ():id(S259_classId){

  }
};


int S259child1_classId = Class_genId();
struct S259child1:S259{

  S259child1 (int a):a(a){
    (id = S259child1_classId);
{

    }
  }
  int a;
};


int S259child2_classId = Class_genId();
struct S259child2:S259{

  S259child2 (int a):a(a){
    (id = S259child2_classId);
{

    }
  }
  int a;
};


struct Trait259{

  int(*trait259)(Class*);
};

Vec* Trait259_v = newVec();

int Trait259_S259child1_trait259(Class* self_) {
  S259child1* self = ((S259child1*)self_);
{
    return (self -> a);
  }
} 
Trait259* newTrait259_S259child1() {
  Trait259 (* impl) = (new Trait259());
  setVec(Trait259_v, S259child1_classId, ((void*)impl));
  ((impl -> trait259) = (& Trait259_S259child1_trait259));
  return impl;
} 
Trait259* Trait259_S259child1_ = newTrait259_S259child1();

int Trait259_S259child2_trait259(Class* self_) {
  S259child2* self = ((S259child2*)self_);
{
    return (self -> a);
  }
} 
Trait259* newTrait259_S259child2() {
  Trait259 (* impl) = (new Trait259());
  setVec(Trait259_v, S259child2_classId, ((void*)impl));
  ((impl -> trait259) = (& Trait259_S259child2_trait259));
  return impl;
} 
Trait259* Trait259_S259child2_ = newTrait259_S259child2();

int Trait97_S259child1_trait97(Class* self_) {
  S259child1* self = ((S259child1*)self_);
{
    return (self -> a);
  }
} 
Trait97* newTrait97_S259child1() {
  Trait97 (* impl) = (new Trait97());
  setVec(Trait97_v, S259child1_classId, ((void*)impl));
  ((impl -> trait97) = (& Trait97_S259child1_trait97));
  return impl;
} 
Trait97* Trait97_S259child1_ = newTrait97_S259child1();

int S260_classId = Class_genId();
struct S260{

  int id;
  S260 ():id(S260_classId){

  }
};


int S260child1_classId = Class_genId();
struct S260child1:S260{

  S260child1 (int a):a(a){
    (id = S260child1_classId);
{

    }
  }
  int a;
};


int S260child2_classId = Class_genId();
struct S260child2:S260{

  S260child2 (int a):a(a){
    (id = S260child2_classId);
{

    }
  }
  int a;
};


int S260child3_classId = Class_genId();
struct S260child3:S260{

  S260child3 (int a):a(a){
    (id = S260child3_classId);
{

    }
  }
  int a;
};


int S260child4_classId = Class_genId();
struct S260child4:S260{

  S260child4 (int a):a(a){
    (id = S260child4_classId);
{

    }
  }
  int a;
};


struct Trait260{

  int(*trait260)(Class*);
};

Vec* Trait260_v = newVec();

int Trait260_S260child1_trait260(Class* self_) {
  S260child1* self = ((S260child1*)self_);
{
    return (self -> a);
  }
} 
Trait260* newTrait260_S260child1() {
  Trait260 (* impl) = (new Trait260());
  setVec(Trait260_v, S260child1_classId, ((void*)impl));
  ((impl -> trait260) = (& Trait260_S260child1_trait260));
  return impl;
} 
Trait260* Trait260_S260child1_ = newTrait260_S260child1();

int Trait260_S260child2_trait260(Class* self_) {
  S260child2* self = ((S260child2*)self_);
{
    return (self -> a);
  }
} 
Trait260* newTrait260_S260child2() {
  Trait260 (* impl) = (new Trait260());
  setVec(Trait260_v, S260child2_classId, ((void*)impl));
  ((impl -> trait260) = (& Trait260_S260child2_trait260));
  return impl;
} 
Trait260* Trait260_S260child2_ = newTrait260_S260child2();

int Trait211_S260child1_trait211(Class* self_) {
  S260child1* self = ((S260child1*)self_);
{
    return (self -> a);
  }
} 
Trait211* newTrait211_S260child1() {
  Trait211 (* impl) = (new Trait211());
  setVec(Trait211_v, S260child1_classId, ((void*)impl));
  ((impl -> trait211) = (& Trait211_S260child1_trait211));
  return impl;
} 
Trait211* Trait211_S260child1_ = newTrait211_S260child1();

int S261_classId = Class_genId();
struct S261{

  int id;
  S261 ():id(S261_classId){

  }
};


int S261child1_classId = Class_genId();
struct S261child1:S261{

  S261child1 (int a):a(a){
    (id = S261child1_classId);
{

    }
  }
  int a;
};


struct Trait261{

  int(*trait261)(Class*);
};

Vec* Trait261_v = newVec();

int Trait261_S261child1_trait261(Class* self_) {
  S261child1* self = ((S261child1*)self_);
{
    return (self -> a);
  }
} 
Trait261* newTrait261_S261child1() {
  Trait261 (* impl) = (new Trait261());
  setVec(Trait261_v, S261child1_classId, ((void*)impl));
  ((impl -> trait261) = (& Trait261_S261child1_trait261));
  return impl;
} 
Trait261* Trait261_S261child1_ = newTrait261_S261child1();

int Trait239_S261child1_trait239(Class* self_) {
  S261child1* self = ((S261child1*)self_);
{
    return (self -> a);
  }
} 
Trait239* newTrait239_S261child1() {
  Trait239 (* impl) = (new Trait239());
  setVec(Trait239_v, S261child1_classId, ((void*)impl));
  ((impl -> trait239) = (& Trait239_S261child1_trait239));
  return impl;
} 
Trait239* Trait239_S261child1_ = newTrait239_S261child1();

int S262_classId = Class_genId();
struct S262{

  int id;
  S262 ():id(S262_classId){

  }
};


int S262child1_classId = Class_genId();
struct S262child1:S262{

  S262child1 (int a):a(a){
    (id = S262child1_classId);
{

    }
  }
  int a;
};


int S262child2_classId = Class_genId();
struct S262child2:S262{

  S262child2 (int a):a(a){
    (id = S262child2_classId);
{

    }
  }
  int a;
};


int S262child3_classId = Class_genId();
struct S262child3:S262{

  S262child3 (int a):a(a){
    (id = S262child3_classId);
{

    }
  }
  int a;
};


struct Trait262{

  int(*trait262)(Class*);
};

Vec* Trait262_v = newVec();

int Trait262_S262child1_trait262(Class* self_) {
  S262child1* self = ((S262child1*)self_);
{
    return (self -> a);
  }
} 
Trait262* newTrait262_S262child1() {
  Trait262 (* impl) = (new Trait262());
  setVec(Trait262_v, S262child1_classId, ((void*)impl));
  ((impl -> trait262) = (& Trait262_S262child1_trait262));
  return impl;
} 
Trait262* Trait262_S262child1_ = newTrait262_S262child1();

int Trait262_S262child2_trait262(Class* self_) {
  S262child2* self = ((S262child2*)self_);
{
    return (self -> a);
  }
} 
Trait262* newTrait262_S262child2() {
  Trait262 (* impl) = (new Trait262());
  setVec(Trait262_v, S262child2_classId, ((void*)impl));
  ((impl -> trait262) = (& Trait262_S262child2_trait262));
  return impl;
} 
Trait262* Trait262_S262child2_ = newTrait262_S262child2();

int Trait262_S262child3_trait262(Class* self_) {
  S262child3* self = ((S262child3*)self_);
{
    return (self -> a);
  }
} 
Trait262* newTrait262_S262child3() {
  Trait262 (* impl) = (new Trait262());
  setVec(Trait262_v, S262child3_classId, ((void*)impl));
  ((impl -> trait262) = (& Trait262_S262child3_trait262));
  return impl;
} 
Trait262* Trait262_S262child3_ = newTrait262_S262child3();

int Trait20_S262child1_trait20(Class* self_) {
  S262child1* self = ((S262child1*)self_);
{
    return (self -> a);
  }
} 
Trait20* newTrait20_S262child1() {
  Trait20 (* impl) = (new Trait20());
  setVec(Trait20_v, S262child1_classId, ((void*)impl));
  ((impl -> trait20) = (& Trait20_S262child1_trait20));
  return impl;
} 
Trait20* Trait20_S262child1_ = newTrait20_S262child1();

int S263_classId = Class_genId();
struct S263{

  int id;
  S263 ():id(S263_classId){

  }
};


int S263child1_classId = Class_genId();
struct S263child1:S263{

  S263child1 (int a):a(a){
    (id = S263child1_classId);
{

    }
  }
  int a;
};


int S263child2_classId = Class_genId();
struct S263child2:S263{

  S263child2 (int a):a(a){
    (id = S263child2_classId);
{

    }
  }
  int a;
};


int S263child3_classId = Class_genId();
struct S263child3:S263{

  S263child3 (int a):a(a){
    (id = S263child3_classId);
{

    }
  }
  int a;
};


struct Trait263{

  int(*trait263)(Class*);
};

Vec* Trait263_v = newVec();

int Trait263_S263child1_trait263(Class* self_) {
  S263child1* self = ((S263child1*)self_);
{
    return (self -> a);
  }
} 
Trait263* newTrait263_S263child1() {
  Trait263 (* impl) = (new Trait263());
  setVec(Trait263_v, S263child1_classId, ((void*)impl));
  ((impl -> trait263) = (& Trait263_S263child1_trait263));
  return impl;
} 
Trait263* Trait263_S263child1_ = newTrait263_S263child1();

int Trait263_S263child2_trait263(Class* self_) {
  S263child2* self = ((S263child2*)self_);
{
    return (self -> a);
  }
} 
Trait263* newTrait263_S263child2() {
  Trait263 (* impl) = (new Trait263());
  setVec(Trait263_v, S263child2_classId, ((void*)impl));
  ((impl -> trait263) = (& Trait263_S263child2_trait263));
  return impl;
} 
Trait263* Trait263_S263child2_ = newTrait263_S263child2();

int Trait175_S263child1_trait175(Class* self_) {
  S263child1* self = ((S263child1*)self_);
{
    return (self -> a);
  }
} 
Trait175* newTrait175_S263child1() {
  Trait175 (* impl) = (new Trait175());
  setVec(Trait175_v, S263child1_classId, ((void*)impl));
  ((impl -> trait175) = (& Trait175_S263child1_trait175));
  return impl;
} 
Trait175* Trait175_S263child1_ = newTrait175_S263child1();

int S264_classId = Class_genId();
struct S264{

  int id;
  S264 ():id(S264_classId){

  }
};


int S264child1_classId = Class_genId();
struct S264child1:S264{

  S264child1 (int a):a(a){
    (id = S264child1_classId);
{

    }
  }
  int a;
};


int S264child2_classId = Class_genId();
struct S264child2:S264{

  S264child2 (int a):a(a){
    (id = S264child2_classId);
{

    }
  }
  int a;
};


struct Trait264{

  int(*trait264)(Class*);
};

Vec* Trait264_v = newVec();

int Trait264_S264child1_trait264(Class* self_) {
  S264child1* self = ((S264child1*)self_);
{
    return (self -> a);
  }
} 
Trait264* newTrait264_S264child1() {
  Trait264 (* impl) = (new Trait264());
  setVec(Trait264_v, S264child1_classId, ((void*)impl));
  ((impl -> trait264) = (& Trait264_S264child1_trait264));
  return impl;
} 
Trait264* Trait264_S264child1_ = newTrait264_S264child1();

int Trait14_S264child1_trait14(Class* self_) {
  S264child1* self = ((S264child1*)self_);
{
    return (self -> a);
  }
} 
Trait14* newTrait14_S264child1() {
  Trait14 (* impl) = (new Trait14());
  setVec(Trait14_v, S264child1_classId, ((void*)impl));
  ((impl -> trait14) = (& Trait14_S264child1_trait14));
  return impl;
} 
Trait14* Trait14_S264child1_ = newTrait14_S264child1();

int S265_classId = Class_genId();
struct S265{

  int id;
  S265 ():id(S265_classId){

  }
};


int S265child1_classId = Class_genId();
struct S265child1:S265{

  S265child1 (int a):a(a){
    (id = S265child1_classId);
{

    }
  }
  int a;
};


int S265child2_classId = Class_genId();
struct S265child2:S265{

  S265child2 (int a):a(a){
    (id = S265child2_classId);
{

    }
  }
  int a;
};


int S265child3_classId = Class_genId();
struct S265child3:S265{

  S265child3 (int a):a(a){
    (id = S265child3_classId);
{

    }
  }
  int a;
};


struct Trait265{

  int(*trait265)(Class*);
};

Vec* Trait265_v = newVec();

int Trait265_S265child1_trait265(Class* self_) {
  S265child1* self = ((S265child1*)self_);
{
    return (self -> a);
  }
} 
Trait265* newTrait265_S265child1() {
  Trait265 (* impl) = (new Trait265());
  setVec(Trait265_v, S265child1_classId, ((void*)impl));
  ((impl -> trait265) = (& Trait265_S265child1_trait265));
  return impl;
} 
Trait265* Trait265_S265child1_ = newTrait265_S265child1();

int Trait265_S265child2_trait265(Class* self_) {
  S265child2* self = ((S265child2*)self_);
{
    return (self -> a);
  }
} 
Trait265* newTrait265_S265child2() {
  Trait265 (* impl) = (new Trait265());
  setVec(Trait265_v, S265child2_classId, ((void*)impl));
  ((impl -> trait265) = (& Trait265_S265child2_trait265));
  return impl;
} 
Trait265* Trait265_S265child2_ = newTrait265_S265child2();

int Trait142_S265child1_trait142(Class* self_) {
  S265child1* self = ((S265child1*)self_);
{
    return (self -> a);
  }
} 
Trait142* newTrait142_S265child1() {
  Trait142 (* impl) = (new Trait142());
  setVec(Trait142_v, S265child1_classId, ((void*)impl));
  ((impl -> trait142) = (& Trait142_S265child1_trait142));
  return impl;
} 
Trait142* Trait142_S265child1_ = newTrait142_S265child1();

int S266_classId = Class_genId();
struct S266{

  int id;
  S266 ():id(S266_classId){

  }
};


int S266child1_classId = Class_genId();
struct S266child1:S266{

  S266child1 (int a):a(a){
    (id = S266child1_classId);
{

    }
  }
  int a;
};


int S266child2_classId = Class_genId();
struct S266child2:S266{

  S266child2 (int a):a(a){
    (id = S266child2_classId);
{

    }
  }
  int a;
};


int S266child3_classId = Class_genId();
struct S266child3:S266{

  S266child3 (int a):a(a){
    (id = S266child3_classId);
{

    }
  }
  int a;
};


struct Trait266{

  int(*trait266)(Class*);
};

Vec* Trait266_v = newVec();

int Trait266_S266child1_trait266(Class* self_) {
  S266child1* self = ((S266child1*)self_);
{
    return (self -> a);
  }
} 
Trait266* newTrait266_S266child1() {
  Trait266 (* impl) = (new Trait266());
  setVec(Trait266_v, S266child1_classId, ((void*)impl));
  ((impl -> trait266) = (& Trait266_S266child1_trait266));
  return impl;
} 
Trait266* Trait266_S266child1_ = newTrait266_S266child1();

int Trait26_S266child1_trait26(Class* self_) {
  S266child1* self = ((S266child1*)self_);
{
    return (self -> a);
  }
} 
Trait26* newTrait26_S266child1() {
  Trait26 (* impl) = (new Trait26());
  setVec(Trait26_v, S266child1_classId, ((void*)impl));
  ((impl -> trait26) = (& Trait26_S266child1_trait26));
  return impl;
} 
Trait26* Trait26_S266child1_ = newTrait26_S266child1();

int S267_classId = Class_genId();
struct S267{

  int id;
  S267 ():id(S267_classId){

  }
};


int S267child1_classId = Class_genId();
struct S267child1:S267{

  S267child1 (int a):a(a){
    (id = S267child1_classId);
{

    }
  }
  int a;
};


int S267child2_classId = Class_genId();
struct S267child2:S267{

  S267child2 (int a):a(a){
    (id = S267child2_classId);
{

    }
  }
  int a;
};


struct Trait267{

  int(*trait267)(Class*);
};

Vec* Trait267_v = newVec();

int Trait267_S267child1_trait267(Class* self_) {
  S267child1* self = ((S267child1*)self_);
{
    return (self -> a);
  }
} 
Trait267* newTrait267_S267child1() {
  Trait267 (* impl) = (new Trait267());
  setVec(Trait267_v, S267child1_classId, ((void*)impl));
  ((impl -> trait267) = (& Trait267_S267child1_trait267));
  return impl;
} 
Trait267* Trait267_S267child1_ = newTrait267_S267child1();

int Trait230_S267child1_trait230(Class* self_) {
  S267child1* self = ((S267child1*)self_);
{
    return (self -> a);
  }
} 
Trait230* newTrait230_S267child1() {
  Trait230 (* impl) = (new Trait230());
  setVec(Trait230_v, S267child1_classId, ((void*)impl));
  ((impl -> trait230) = (& Trait230_S267child1_trait230));
  return impl;
} 
Trait230* Trait230_S267child1_ = newTrait230_S267child1();

int S268_classId = Class_genId();
struct S268{

  int id;
  S268 ():id(S268_classId){

  }
};


int S268child1_classId = Class_genId();
struct S268child1:S268{

  S268child1 (int a):a(a){
    (id = S268child1_classId);
{

    }
  }
  int a;
};


int S268child2_classId = Class_genId();
struct S268child2:S268{

  S268child2 (int a):a(a){
    (id = S268child2_classId);
{

    }
  }
  int a;
};


struct Trait268{

  int(*trait268)(Class*);
};

Vec* Trait268_v = newVec();

int Trait268_S268child1_trait268(Class* self_) {
  S268child1* self = ((S268child1*)self_);
{
    return (self -> a);
  }
} 
Trait268* newTrait268_S268child1() {
  Trait268 (* impl) = (new Trait268());
  setVec(Trait268_v, S268child1_classId, ((void*)impl));
  ((impl -> trait268) = (& Trait268_S268child1_trait268));
  return impl;
} 
Trait268* Trait268_S268child1_ = newTrait268_S268child1();

int Trait200_S268child1_trait200(Class* self_) {
  S268child1* self = ((S268child1*)self_);
{
    return (self -> a);
  }
} 
Trait200* newTrait200_S268child1() {
  Trait200 (* impl) = (new Trait200());
  setVec(Trait200_v, S268child1_classId, ((void*)impl));
  ((impl -> trait200) = (& Trait200_S268child1_trait200));
  return impl;
} 
Trait200* Trait200_S268child1_ = newTrait200_S268child1();

int S269_classId = Class_genId();
struct S269{

  int id;
  S269 ():id(S269_classId){

  }
};


int S269child1_classId = Class_genId();
struct S269child1:S269{

  S269child1 (int a):a(a){
    (id = S269child1_classId);
{

    }
  }
  int a;
};


int S269child2_classId = Class_genId();
struct S269child2:S269{

  S269child2 (int a):a(a){
    (id = S269child2_classId);
{

    }
  }
  int a;
};


int S269child3_classId = Class_genId();
struct S269child3:S269{

  S269child3 (int a):a(a){
    (id = S269child3_classId);
{

    }
  }
  int a;
};


struct Trait269{

  int(*trait269)(Class*);
};

Vec* Trait269_v = newVec();

int Trait269_S269child1_trait269(Class* self_) {
  S269child1* self = ((S269child1*)self_);
{
    return (self -> a);
  }
} 
Trait269* newTrait269_S269child1() {
  Trait269 (* impl) = (new Trait269());
  setVec(Trait269_v, S269child1_classId, ((void*)impl));
  ((impl -> trait269) = (& Trait269_S269child1_trait269));
  return impl;
} 
Trait269* Trait269_S269child1_ = newTrait269_S269child1();

int Trait269_S269child2_trait269(Class* self_) {
  S269child2* self = ((S269child2*)self_);
{
    return (self -> a);
  }
} 
Trait269* newTrait269_S269child2() {
  Trait269 (* impl) = (new Trait269());
  setVec(Trait269_v, S269child2_classId, ((void*)impl));
  ((impl -> trait269) = (& Trait269_S269child2_trait269));
  return impl;
} 
Trait269* Trait269_S269child2_ = newTrait269_S269child2();

int Trait1_S269child1_trait1(Class* self_) {
  S269child1* self = ((S269child1*)self_);
{
    return (self -> a);
  }
} 
Trait1* newTrait1_S269child1() {
  Trait1 (* impl) = (new Trait1());
  setVec(Trait1_v, S269child1_classId, ((void*)impl));
  ((impl -> trait1) = (& Trait1_S269child1_trait1));
  return impl;
} 
Trait1* Trait1_S269child1_ = newTrait1_S269child1();

int S270_classId = Class_genId();
struct S270{

  int id;
  S270 ():id(S270_classId){

  }
};


int S270child1_classId = Class_genId();
struct S270child1:S270{

  S270child1 (int a):a(a){
    (id = S270child1_classId);
{

    }
  }
  int a;
};


int S270child2_classId = Class_genId();
struct S270child2:S270{

  S270child2 (int a):a(a){
    (id = S270child2_classId);
{

    }
  }
  int a;
};


struct Trait270{

  int(*trait270)(Class*);
};

Vec* Trait270_v = newVec();

int Trait270_S270child1_trait270(Class* self_) {
  S270child1* self = ((S270child1*)self_);
{
    return (self -> a);
  }
} 
Trait270* newTrait270_S270child1() {
  Trait270 (* impl) = (new Trait270());
  setVec(Trait270_v, S270child1_classId, ((void*)impl));
  ((impl -> trait270) = (& Trait270_S270child1_trait270));
  return impl;
} 
Trait270* Trait270_S270child1_ = newTrait270_S270child1();

int Trait270_S270child2_trait270(Class* self_) {
  S270child2* self = ((S270child2*)self_);
{
    return (self -> a);
  }
} 
Trait270* newTrait270_S270child2() {
  Trait270 (* impl) = (new Trait270());
  setVec(Trait270_v, S270child2_classId, ((void*)impl));
  ((impl -> trait270) = (& Trait270_S270child2_trait270));
  return impl;
} 
Trait270* Trait270_S270child2_ = newTrait270_S270child2();

int Trait14_S270child1_trait14(Class* self_) {
  S270child1* self = ((S270child1*)self_);
{
    return (self -> a);
  }
} 
Trait14* newTrait14_S270child1() {
  Trait14 (* impl) = (new Trait14());
  setVec(Trait14_v, S270child1_classId, ((void*)impl));
  ((impl -> trait14) = (& Trait14_S270child1_trait14));
  return impl;
} 
Trait14* Trait14_S270child1_ = newTrait14_S270child1();

int S271_classId = Class_genId();
struct S271{

  int id;
  S271 ():id(S271_classId){

  }
};


int S271child1_classId = Class_genId();
struct S271child1:S271{

  S271child1 (int a):a(a){
    (id = S271child1_classId);
{

    }
  }
  int a;
};


int S271child2_classId = Class_genId();
struct S271child2:S271{

  S271child2 (int a):a(a){
    (id = S271child2_classId);
{

    }
  }
  int a;
};


struct Trait271{

  int(*trait271)(Class*);
};

Vec* Trait271_v = newVec();

int Trait271_S271child1_trait271(Class* self_) {
  S271child1* self = ((S271child1*)self_);
{
    return (self -> a);
  }
} 
Trait271* newTrait271_S271child1() {
  Trait271 (* impl) = (new Trait271());
  setVec(Trait271_v, S271child1_classId, ((void*)impl));
  ((impl -> trait271) = (& Trait271_S271child1_trait271));
  return impl;
} 
Trait271* Trait271_S271child1_ = newTrait271_S271child1();

int Trait180_S271child1_trait180(Class* self_) {
  S271child1* self = ((S271child1*)self_);
{
    return (self -> a);
  }
} 
Trait180* newTrait180_S271child1() {
  Trait180 (* impl) = (new Trait180());
  setVec(Trait180_v, S271child1_classId, ((void*)impl));
  ((impl -> trait180) = (& Trait180_S271child1_trait180));
  return impl;
} 
Trait180* Trait180_S271child1_ = newTrait180_S271child1();

int S272_classId = Class_genId();
struct S272{

  int id;
  S272 ():id(S272_classId){

  }
};


int S272child1_classId = Class_genId();
struct S272child1:S272{

  S272child1 (int a):a(a){
    (id = S272child1_classId);
{

    }
  }
  int a;
};


struct Trait272{

  int(*trait272)(Class*);
};

Vec* Trait272_v = newVec();

int Trait272_S272child1_trait272(Class* self_) {
  S272child1* self = ((S272child1*)self_);
{
    return (self -> a);
  }
} 
Trait272* newTrait272_S272child1() {
  Trait272 (* impl) = (new Trait272());
  setVec(Trait272_v, S272child1_classId, ((void*)impl));
  ((impl -> trait272) = (& Trait272_S272child1_trait272));
  return impl;
} 
Trait272* Trait272_S272child1_ = newTrait272_S272child1();

int Trait187_S272child1_trait187(Class* self_) {
  S272child1* self = ((S272child1*)self_);
{
    return (self -> a);
  }
} 
Trait187* newTrait187_S272child1() {
  Trait187 (* impl) = (new Trait187());
  setVec(Trait187_v, S272child1_classId, ((void*)impl));
  ((impl -> trait187) = (& Trait187_S272child1_trait187));
  return impl;
} 
Trait187* Trait187_S272child1_ = newTrait187_S272child1();

int S273_classId = Class_genId();
struct S273{

  int id;
  S273 ():id(S273_classId){

  }
};


int S273child1_classId = Class_genId();
struct S273child1:S273{

  S273child1 (int a):a(a){
    (id = S273child1_classId);
{

    }
  }
  int a;
};


int S273child2_classId = Class_genId();
struct S273child2:S273{

  S273child2 (int a):a(a){
    (id = S273child2_classId);
{

    }
  }
  int a;
};


int S273child3_classId = Class_genId();
struct S273child3:S273{

  S273child3 (int a):a(a){
    (id = S273child3_classId);
{

    }
  }
  int a;
};


struct Trait273{

  int(*trait273)(Class*);
};

Vec* Trait273_v = newVec();

int Trait273_S273child1_trait273(Class* self_) {
  S273child1* self = ((S273child1*)self_);
{
    return (self -> a);
  }
} 
Trait273* newTrait273_S273child1() {
  Trait273 (* impl) = (new Trait273());
  setVec(Trait273_v, S273child1_classId, ((void*)impl));
  ((impl -> trait273) = (& Trait273_S273child1_trait273));
  return impl;
} 
Trait273* Trait273_S273child1_ = newTrait273_S273child1();

int Trait225_S273child1_trait225(Class* self_) {
  S273child1* self = ((S273child1*)self_);
{
    return (self -> a);
  }
} 
Trait225* newTrait225_S273child1() {
  Trait225 (* impl) = (new Trait225());
  setVec(Trait225_v, S273child1_classId, ((void*)impl));
  ((impl -> trait225) = (& Trait225_S273child1_trait225));
  return impl;
} 
Trait225* Trait225_S273child1_ = newTrait225_S273child1();

int S274_classId = Class_genId();
struct S274{

  int id;
  S274 ():id(S274_classId){

  }
};


int S274child1_classId = Class_genId();
struct S274child1:S274{

  S274child1 (int a):a(a){
    (id = S274child1_classId);
{

    }
  }
  int a;
};


int S274child2_classId = Class_genId();
struct S274child2:S274{

  S274child2 (int a):a(a){
    (id = S274child2_classId);
{

    }
  }
  int a;
};


struct Trait274{

  int(*trait274)(Class*);
};

Vec* Trait274_v = newVec();

int Trait274_S274child1_trait274(Class* self_) {
  S274child1* self = ((S274child1*)self_);
{
    return (self -> a);
  }
} 
Trait274* newTrait274_S274child1() {
  Trait274 (* impl) = (new Trait274());
  setVec(Trait274_v, S274child1_classId, ((void*)impl));
  ((impl -> trait274) = (& Trait274_S274child1_trait274));
  return impl;
} 
Trait274* Trait274_S274child1_ = newTrait274_S274child1();

int Trait100_S274child1_trait100(Class* self_) {
  S274child1* self = ((S274child1*)self_);
{
    return (self -> a);
  }
} 
Trait100* newTrait100_S274child1() {
  Trait100 (* impl) = (new Trait100());
  setVec(Trait100_v, S274child1_classId, ((void*)impl));
  ((impl -> trait100) = (& Trait100_S274child1_trait100));
  return impl;
} 
Trait100* Trait100_S274child1_ = newTrait100_S274child1();

int S275_classId = Class_genId();
struct S275{

  int id;
  S275 ():id(S275_classId){

  }
};


int S275child1_classId = Class_genId();
struct S275child1:S275{

  S275child1 (int a):a(a){
    (id = S275child1_classId);
{

    }
  }
  int a;
};


int S275child2_classId = Class_genId();
struct S275child2:S275{

  S275child2 (int a):a(a){
    (id = S275child2_classId);
{

    }
  }
  int a;
};


int S275child3_classId = Class_genId();
struct S275child3:S275{

  S275child3 (int a):a(a){
    (id = S275child3_classId);
{

    }
  }
  int a;
};


struct Trait275{

  int(*trait275)(Class*);
};

Vec* Trait275_v = newVec();

int Trait275_S275child1_trait275(Class* self_) {
  S275child1* self = ((S275child1*)self_);
{
    return (self -> a);
  }
} 
Trait275* newTrait275_S275child1() {
  Trait275 (* impl) = (new Trait275());
  setVec(Trait275_v, S275child1_classId, ((void*)impl));
  ((impl -> trait275) = (& Trait275_S275child1_trait275));
  return impl;
} 
Trait275* Trait275_S275child1_ = newTrait275_S275child1();

int Trait132_S275child1_trait132(Class* self_) {
  S275child1* self = ((S275child1*)self_);
{
    return (self -> a);
  }
} 
Trait132* newTrait132_S275child1() {
  Trait132 (* impl) = (new Trait132());
  setVec(Trait132_v, S275child1_classId, ((void*)impl));
  ((impl -> trait132) = (& Trait132_S275child1_trait132));
  return impl;
} 
Trait132* Trait132_S275child1_ = newTrait132_S275child1();

int S276_classId = Class_genId();
struct S276{

  int id;
  S276 ():id(S276_classId){

  }
};


int S276child1_classId = Class_genId();
struct S276child1:S276{

  S276child1 (int a):a(a){
    (id = S276child1_classId);
{

    }
  }
  int a;
};


int S276child2_classId = Class_genId();
struct S276child2:S276{

  S276child2 (int a):a(a){
    (id = S276child2_classId);
{

    }
  }
  int a;
};


int S276child3_classId = Class_genId();
struct S276child3:S276{

  S276child3 (int a):a(a){
    (id = S276child3_classId);
{

    }
  }
  int a;
};


struct Trait276{

  int(*trait276)(Class*);
};

Vec* Trait276_v = newVec();

int Trait276_S276child1_trait276(Class* self_) {
  S276child1* self = ((S276child1*)self_);
{
    return (self -> a);
  }
} 
Trait276* newTrait276_S276child1() {
  Trait276 (* impl) = (new Trait276());
  setVec(Trait276_v, S276child1_classId, ((void*)impl));
  ((impl -> trait276) = (& Trait276_S276child1_trait276));
  return impl;
} 
Trait276* Trait276_S276child1_ = newTrait276_S276child1();

int Trait276_S276child2_trait276(Class* self_) {
  S276child2* self = ((S276child2*)self_);
{
    return (self -> a);
  }
} 
Trait276* newTrait276_S276child2() {
  Trait276 (* impl) = (new Trait276());
  setVec(Trait276_v, S276child2_classId, ((void*)impl));
  ((impl -> trait276) = (& Trait276_S276child2_trait276));
  return impl;
} 
Trait276* Trait276_S276child2_ = newTrait276_S276child2();

int Trait161_S276child1_trait161(Class* self_) {
  S276child1* self = ((S276child1*)self_);
{
    return (self -> a);
  }
} 
Trait161* newTrait161_S276child1() {
  Trait161 (* impl) = (new Trait161());
  setVec(Trait161_v, S276child1_classId, ((void*)impl));
  ((impl -> trait161) = (& Trait161_S276child1_trait161));
  return impl;
} 
Trait161* Trait161_S276child1_ = newTrait161_S276child1();

int S277_classId = Class_genId();
struct S277{

  int id;
  S277 ():id(S277_classId){

  }
};


int S277child1_classId = Class_genId();
struct S277child1:S277{

  S277child1 (int a):a(a){
    (id = S277child1_classId);
{

    }
  }
  int a;
};


int S277child2_classId = Class_genId();
struct S277child2:S277{

  S277child2 (int a):a(a){
    (id = S277child2_classId);
{

    }
  }
  int a;
};


struct Trait277{

  int(*trait277)(Class*);
};

Vec* Trait277_v = newVec();

int Trait277_S277child1_trait277(Class* self_) {
  S277child1* self = ((S277child1*)self_);
{
    return (self -> a);
  }
} 
Trait277* newTrait277_S277child1() {
  Trait277 (* impl) = (new Trait277());
  setVec(Trait277_v, S277child1_classId, ((void*)impl));
  ((impl -> trait277) = (& Trait277_S277child1_trait277));
  return impl;
} 
Trait277* Trait277_S277child1_ = newTrait277_S277child1();

int Trait277_S277child2_trait277(Class* self_) {
  S277child2* self = ((S277child2*)self_);
{
    return (self -> a);
  }
} 
Trait277* newTrait277_S277child2() {
  Trait277 (* impl) = (new Trait277());
  setVec(Trait277_v, S277child2_classId, ((void*)impl));
  ((impl -> trait277) = (& Trait277_S277child2_trait277));
  return impl;
} 
Trait277* Trait277_S277child2_ = newTrait277_S277child2();

int Trait267_S277child1_trait267(Class* self_) {
  S277child1* self = ((S277child1*)self_);
{
    return (self -> a);
  }
} 
Trait267* newTrait267_S277child1() {
  Trait267 (* impl) = (new Trait267());
  setVec(Trait267_v, S277child1_classId, ((void*)impl));
  ((impl -> trait267) = (& Trait267_S277child1_trait267));
  return impl;
} 
Trait267* Trait267_S277child1_ = newTrait267_S277child1();

int S278_classId = Class_genId();
struct S278{

  int id;
  S278 ():id(S278_classId){

  }
};


int S278child1_classId = Class_genId();
struct S278child1:S278{

  S278child1 (int a):a(a){
    (id = S278child1_classId);
{

    }
  }
  int a;
};


struct Trait278{

  int(*trait278)(Class*);
};

Vec* Trait278_v = newVec();

int Trait278_S278child1_trait278(Class* self_) {
  S278child1* self = ((S278child1*)self_);
{
    return (self -> a);
  }
} 
Trait278* newTrait278_S278child1() {
  Trait278 (* impl) = (new Trait278());
  setVec(Trait278_v, S278child1_classId, ((void*)impl));
  ((impl -> trait278) = (& Trait278_S278child1_trait278));
  return impl;
} 
Trait278* Trait278_S278child1_ = newTrait278_S278child1();

int Trait141_S278child1_trait141(Class* self_) {
  S278child1* self = ((S278child1*)self_);
{
    return (self -> a);
  }
} 
Trait141* newTrait141_S278child1() {
  Trait141 (* impl) = (new Trait141());
  setVec(Trait141_v, S278child1_classId, ((void*)impl));
  ((impl -> trait141) = (& Trait141_S278child1_trait141));
  return impl;
} 
Trait141* Trait141_S278child1_ = newTrait141_S278child1();

int S279_classId = Class_genId();
struct S279{

  int id;
  S279 ():id(S279_classId){

  }
};


int S279child1_classId = Class_genId();
struct S279child1:S279{

  S279child1 (int a):a(a){
    (id = S279child1_classId);
{

    }
  }
  int a;
};


int S279child2_classId = Class_genId();
struct S279child2:S279{

  S279child2 (int a):a(a){
    (id = S279child2_classId);
{

    }
  }
  int a;
};


int S279child3_classId = Class_genId();
struct S279child3:S279{

  S279child3 (int a):a(a){
    (id = S279child3_classId);
{

    }
  }
  int a;
};


struct Trait279{

  int(*trait279)(Class*);
};

Vec* Trait279_v = newVec();

int Trait279_S279child1_trait279(Class* self_) {
  S279child1* self = ((S279child1*)self_);
{
    return (self -> a);
  }
} 
Trait279* newTrait279_S279child1() {
  Trait279 (* impl) = (new Trait279());
  setVec(Trait279_v, S279child1_classId, ((void*)impl));
  ((impl -> trait279) = (& Trait279_S279child1_trait279));
  return impl;
} 
Trait279* Trait279_S279child1_ = newTrait279_S279child1();

int Trait279_S279child2_trait279(Class* self_) {
  S279child2* self = ((S279child2*)self_);
{
    return (self -> a);
  }
} 
Trait279* newTrait279_S279child2() {
  Trait279 (* impl) = (new Trait279());
  setVec(Trait279_v, S279child2_classId, ((void*)impl));
  ((impl -> trait279) = (& Trait279_S279child2_trait279));
  return impl;
} 
Trait279* Trait279_S279child2_ = newTrait279_S279child2();

int Trait273_S279child1_trait273(Class* self_) {
  S279child1* self = ((S279child1*)self_);
{
    return (self -> a);
  }
} 
Trait273* newTrait273_S279child1() {
  Trait273 (* impl) = (new Trait273());
  setVec(Trait273_v, S279child1_classId, ((void*)impl));
  ((impl -> trait273) = (& Trait273_S279child1_trait273));
  return impl;
} 
Trait273* Trait273_S279child1_ = newTrait273_S279child1();

int S280_classId = Class_genId();
struct S280{

  int id;
  S280 ():id(S280_classId){

  }
};


int S280child1_classId = Class_genId();
struct S280child1:S280{

  S280child1 (int a):a(a){
    (id = S280child1_classId);
{

    }
  }
  int a;
};


int S280child2_classId = Class_genId();
struct S280child2:S280{

  S280child2 (int a):a(a){
    (id = S280child2_classId);
{

    }
  }
  int a;
};


int S280child3_classId = Class_genId();
struct S280child3:S280{

  S280child3 (int a):a(a){
    (id = S280child3_classId);
{

    }
  }
  int a;
};


int S280child4_classId = Class_genId();
struct S280child4:S280{

  S280child4 (int a):a(a){
    (id = S280child4_classId);
{

    }
  }
  int a;
};


struct Trait280{

  int(*trait280)(Class*);
};

Vec* Trait280_v = newVec();

int Trait280_S280child1_trait280(Class* self_) {
  S280child1* self = ((S280child1*)self_);
{
    return (self -> a);
  }
} 
Trait280* newTrait280_S280child1() {
  Trait280 (* impl) = (new Trait280());
  setVec(Trait280_v, S280child1_classId, ((void*)impl));
  ((impl -> trait280) = (& Trait280_S280child1_trait280));
  return impl;
} 
Trait280* Trait280_S280child1_ = newTrait280_S280child1();

int Trait280_S280child2_trait280(Class* self_) {
  S280child2* self = ((S280child2*)self_);
{
    return (self -> a);
  }
} 
Trait280* newTrait280_S280child2() {
  Trait280 (* impl) = (new Trait280());
  setVec(Trait280_v, S280child2_classId, ((void*)impl));
  ((impl -> trait280) = (& Trait280_S280child2_trait280));
  return impl;
} 
Trait280* Trait280_S280child2_ = newTrait280_S280child2();

int Trait199_S280child1_trait199(Class* self_) {
  S280child1* self = ((S280child1*)self_);
{
    return (self -> a);
  }
} 
Trait199* newTrait199_S280child1() {
  Trait199 (* impl) = (new Trait199());
  setVec(Trait199_v, S280child1_classId, ((void*)impl));
  ((impl -> trait199) = (& Trait199_S280child1_trait199));
  return impl;
} 
Trait199* Trait199_S280child1_ = newTrait199_S280child1();

int S281_classId = Class_genId();
struct S281{

  int id;
  S281 ():id(S281_classId){

  }
};


int S281child1_classId = Class_genId();
struct S281child1:S281{

  S281child1 (int a):a(a){
    (id = S281child1_classId);
{

    }
  }
  int a;
};


int S281child2_classId = Class_genId();
struct S281child2:S281{

  S281child2 (int a):a(a){
    (id = S281child2_classId);
{

    }
  }
  int a;
};


int S281child3_classId = Class_genId();
struct S281child3:S281{

  S281child3 (int a):a(a){
    (id = S281child3_classId);
{

    }
  }
  int a;
};


struct Trait281{

  int(*trait281)(Class*);
};

Vec* Trait281_v = newVec();

int Trait281_S281child1_trait281(Class* self_) {
  S281child1* self = ((S281child1*)self_);
{
    return (self -> a);
  }
} 
Trait281* newTrait281_S281child1() {
  Trait281 (* impl) = (new Trait281());
  setVec(Trait281_v, S281child1_classId, ((void*)impl));
  ((impl -> trait281) = (& Trait281_S281child1_trait281));
  return impl;
} 
Trait281* Trait281_S281child1_ = newTrait281_S281child1();

int Trait7_S281child1_trait7(Class* self_) {
  S281child1* self = ((S281child1*)self_);
{
    return (self -> a);
  }
} 
Trait7* newTrait7_S281child1() {
  Trait7 (* impl) = (new Trait7());
  setVec(Trait7_v, S281child1_classId, ((void*)impl));
  ((impl -> trait7) = (& Trait7_S281child1_trait7));
  return impl;
} 
Trait7* Trait7_S281child1_ = newTrait7_S281child1();

int S282_classId = Class_genId();
struct S282{

  int id;
  S282 ():id(S282_classId){

  }
};


int S282child1_classId = Class_genId();
struct S282child1:S282{

  S282child1 (int a):a(a){
    (id = S282child1_classId);
{

    }
  }
  int a;
};


int S282child2_classId = Class_genId();
struct S282child2:S282{

  S282child2 (int a):a(a){
    (id = S282child2_classId);
{

    }
  }
  int a;
};


int S282child3_classId = Class_genId();
struct S282child3:S282{

  S282child3 (int a):a(a){
    (id = S282child3_classId);
{

    }
  }
  int a;
};


struct Trait282{

  int(*trait282)(Class*);
};

Vec* Trait282_v = newVec();

int Trait282_S282child1_trait282(Class* self_) {
  S282child1* self = ((S282child1*)self_);
{
    return (self -> a);
  }
} 
Trait282* newTrait282_S282child1() {
  Trait282 (* impl) = (new Trait282());
  setVec(Trait282_v, S282child1_classId, ((void*)impl));
  ((impl -> trait282) = (& Trait282_S282child1_trait282));
  return impl;
} 
Trait282* Trait282_S282child1_ = newTrait282_S282child1();

int Trait282_S282child2_trait282(Class* self_) {
  S282child2* self = ((S282child2*)self_);
{
    return (self -> a);
  }
} 
Trait282* newTrait282_S282child2() {
  Trait282 (* impl) = (new Trait282());
  setVec(Trait282_v, S282child2_classId, ((void*)impl));
  ((impl -> trait282) = (& Trait282_S282child2_trait282));
  return impl;
} 
Trait282* Trait282_S282child2_ = newTrait282_S282child2();

int Trait282_S282child3_trait282(Class* self_) {
  S282child3* self = ((S282child3*)self_);
{
    return (self -> a);
  }
} 
Trait282* newTrait282_S282child3() {
  Trait282 (* impl) = (new Trait282());
  setVec(Trait282_v, S282child3_classId, ((void*)impl));
  ((impl -> trait282) = (& Trait282_S282child3_trait282));
  return impl;
} 
Trait282* Trait282_S282child3_ = newTrait282_S282child3();

int Trait193_S282child1_trait193(Class* self_) {
  S282child1* self = ((S282child1*)self_);
{
    return (self -> a);
  }
} 
Trait193* newTrait193_S282child1() {
  Trait193 (* impl) = (new Trait193());
  setVec(Trait193_v, S282child1_classId, ((void*)impl));
  ((impl -> trait193) = (& Trait193_S282child1_trait193));
  return impl;
} 
Trait193* Trait193_S282child1_ = newTrait193_S282child1();

int S283_classId = Class_genId();
struct S283{

  int id;
  S283 ():id(S283_classId){

  }
};


int S283child1_classId = Class_genId();
struct S283child1:S283{

  S283child1 (int a):a(a){
    (id = S283child1_classId);
{

    }
  }
  int a;
};


int S283child2_classId = Class_genId();
struct S283child2:S283{

  S283child2 (int a):a(a){
    (id = S283child2_classId);
{

    }
  }
  int a;
};


struct Trait283{

  int(*trait283)(Class*);
};

Vec* Trait283_v = newVec();

int Trait283_S283child1_trait283(Class* self_) {
  S283child1* self = ((S283child1*)self_);
{
    return (self -> a);
  }
} 
Trait283* newTrait283_S283child1() {
  Trait283 (* impl) = (new Trait283());
  setVec(Trait283_v, S283child1_classId, ((void*)impl));
  ((impl -> trait283) = (& Trait283_S283child1_trait283));
  return impl;
} 
Trait283* Trait283_S283child1_ = newTrait283_S283child1();

int Trait283_S283child2_trait283(Class* self_) {
  S283child2* self = ((S283child2*)self_);
{
    return (self -> a);
  }
} 
Trait283* newTrait283_S283child2() {
  Trait283 (* impl) = (new Trait283());
  setVec(Trait283_v, S283child2_classId, ((void*)impl));
  ((impl -> trait283) = (& Trait283_S283child2_trait283));
  return impl;
} 
Trait283* Trait283_S283child2_ = newTrait283_S283child2();

int Trait184_S283child1_trait184(Class* self_) {
  S283child1* self = ((S283child1*)self_);
{
    return (self -> a);
  }
} 
Trait184* newTrait184_S283child1() {
  Trait184 (* impl) = (new Trait184());
  setVec(Trait184_v, S283child1_classId, ((void*)impl));
  ((impl -> trait184) = (& Trait184_S283child1_trait184));
  return impl;
} 
Trait184* Trait184_S283child1_ = newTrait184_S283child1();

int S284_classId = Class_genId();
struct S284{

  int id;
  S284 ():id(S284_classId){

  }
};


int S284child1_classId = Class_genId();
struct S284child1:S284{

  S284child1 (int a):a(a){
    (id = S284child1_classId);
{

    }
  }
  int a;
};


int S284child2_classId = Class_genId();
struct S284child2:S284{

  S284child2 (int a):a(a){
    (id = S284child2_classId);
{

    }
  }
  int a;
};


struct Trait284{

  int(*trait284)(Class*);
};

Vec* Trait284_v = newVec();

int Trait284_S284child1_trait284(Class* self_) {
  S284child1* self = ((S284child1*)self_);
{
    return (self -> a);
  }
} 
Trait284* newTrait284_S284child1() {
  Trait284 (* impl) = (new Trait284());
  setVec(Trait284_v, S284child1_classId, ((void*)impl));
  ((impl -> trait284) = (& Trait284_S284child1_trait284));
  return impl;
} 
Trait284* Trait284_S284child1_ = newTrait284_S284child1();

int Trait284_S284child2_trait284(Class* self_) {
  S284child2* self = ((S284child2*)self_);
{
    return (self -> a);
  }
} 
Trait284* newTrait284_S284child2() {
  Trait284 (* impl) = (new Trait284());
  setVec(Trait284_v, S284child2_classId, ((void*)impl));
  ((impl -> trait284) = (& Trait284_S284child2_trait284));
  return impl;
} 
Trait284* Trait284_S284child2_ = newTrait284_S284child2();

int Trait42_S284child1_trait42(Class* self_) {
  S284child1* self = ((S284child1*)self_);
{
    return (self -> a);
  }
} 
Trait42* newTrait42_S284child1() {
  Trait42 (* impl) = (new Trait42());
  setVec(Trait42_v, S284child1_classId, ((void*)impl));
  ((impl -> trait42) = (& Trait42_S284child1_trait42));
  return impl;
} 
Trait42* Trait42_S284child1_ = newTrait42_S284child1();

int S285_classId = Class_genId();
struct S285{

  int id;
  S285 ():id(S285_classId){

  }
};


int S285child1_classId = Class_genId();
struct S285child1:S285{

  S285child1 (int a):a(a){
    (id = S285child1_classId);
{

    }
  }
  int a;
};


int S285child2_classId = Class_genId();
struct S285child2:S285{

  S285child2 (int a):a(a){
    (id = S285child2_classId);
{

    }
  }
  int a;
};


int S285child3_classId = Class_genId();
struct S285child3:S285{

  S285child3 (int a):a(a){
    (id = S285child3_classId);
{

    }
  }
  int a;
};


struct Trait285{

  int(*trait285)(Class*);
};

Vec* Trait285_v = newVec();

int Trait285_S285child1_trait285(Class* self_) {
  S285child1* self = ((S285child1*)self_);
{
    return (self -> a);
  }
} 
Trait285* newTrait285_S285child1() {
  Trait285 (* impl) = (new Trait285());
  setVec(Trait285_v, S285child1_classId, ((void*)impl));
  ((impl -> trait285) = (& Trait285_S285child1_trait285));
  return impl;
} 
Trait285* Trait285_S285child1_ = newTrait285_S285child1();

int Trait285_S285child2_trait285(Class* self_) {
  S285child2* self = ((S285child2*)self_);
{
    return (self -> a);
  }
} 
Trait285* newTrait285_S285child2() {
  Trait285 (* impl) = (new Trait285());
  setVec(Trait285_v, S285child2_classId, ((void*)impl));
  ((impl -> trait285) = (& Trait285_S285child2_trait285));
  return impl;
} 
Trait285* Trait285_S285child2_ = newTrait285_S285child2();

int Trait285_S285child3_trait285(Class* self_) {
  S285child3* self = ((S285child3*)self_);
{
    return (self -> a);
  }
} 
Trait285* newTrait285_S285child3() {
  Trait285 (* impl) = (new Trait285());
  setVec(Trait285_v, S285child3_classId, ((void*)impl));
  ((impl -> trait285) = (& Trait285_S285child3_trait285));
  return impl;
} 
Trait285* Trait285_S285child3_ = newTrait285_S285child3();

int Trait244_S285child1_trait244(Class* self_) {
  S285child1* self = ((S285child1*)self_);
{
    return (self -> a);
  }
} 
Trait244* newTrait244_S285child1() {
  Trait244 (* impl) = (new Trait244());
  setVec(Trait244_v, S285child1_classId, ((void*)impl));
  ((impl -> trait244) = (& Trait244_S285child1_trait244));
  return impl;
} 
Trait244* Trait244_S285child1_ = newTrait244_S285child1();

int S286_classId = Class_genId();
struct S286{

  int id;
  S286 ():id(S286_classId){

  }
};


int S286child1_classId = Class_genId();
struct S286child1:S286{

  S286child1 (int a):a(a){
    (id = S286child1_classId);
{

    }
  }
  int a;
};


int S286child2_classId = Class_genId();
struct S286child2:S286{

  S286child2 (int a):a(a){
    (id = S286child2_classId);
{

    }
  }
  int a;
};


struct Trait286{

  int(*trait286)(Class*);
};

Vec* Trait286_v = newVec();

int Trait286_S286child1_trait286(Class* self_) {
  S286child1* self = ((S286child1*)self_);
{
    return (self -> a);
  }
} 
Trait286* newTrait286_S286child1() {
  Trait286 (* impl) = (new Trait286());
  setVec(Trait286_v, S286child1_classId, ((void*)impl));
  ((impl -> trait286) = (& Trait286_S286child1_trait286));
  return impl;
} 
Trait286* Trait286_S286child1_ = newTrait286_S286child1();

int Trait230_S286child1_trait230(Class* self_) {
  S286child1* self = ((S286child1*)self_);
{
    return (self -> a);
  }
} 
Trait230* newTrait230_S286child1() {
  Trait230 (* impl) = (new Trait230());
  setVec(Trait230_v, S286child1_classId, ((void*)impl));
  ((impl -> trait230) = (& Trait230_S286child1_trait230));
  return impl;
} 
Trait230* Trait230_S286child1_ = newTrait230_S286child1();

int S287_classId = Class_genId();
struct S287{

  int id;
  S287 ():id(S287_classId){

  }
};


int S287child1_classId = Class_genId();
struct S287child1:S287{

  S287child1 (int a):a(a){
    (id = S287child1_classId);
{

    }
  }
  int a;
};


int S287child2_classId = Class_genId();
struct S287child2:S287{

  S287child2 (int a):a(a){
    (id = S287child2_classId);
{

    }
  }
  int a;
};


int S287child3_classId = Class_genId();
struct S287child3:S287{

  S287child3 (int a):a(a){
    (id = S287child3_classId);
{

    }
  }
  int a;
};


int S287child4_classId = Class_genId();
struct S287child4:S287{

  S287child4 (int a):a(a){
    (id = S287child4_classId);
{

    }
  }
  int a;
};


struct Trait287{

  int(*trait287)(Class*);
};

Vec* Trait287_v = newVec();

int Trait287_S287child1_trait287(Class* self_) {
  S287child1* self = ((S287child1*)self_);
{
    return (self -> a);
  }
} 
Trait287* newTrait287_S287child1() {
  Trait287 (* impl) = (new Trait287());
  setVec(Trait287_v, S287child1_classId, ((void*)impl));
  ((impl -> trait287) = (& Trait287_S287child1_trait287));
  return impl;
} 
Trait287* Trait287_S287child1_ = newTrait287_S287child1();

int Trait287_S287child2_trait287(Class* self_) {
  S287child2* self = ((S287child2*)self_);
{
    return (self -> a);
  }
} 
Trait287* newTrait287_S287child2() {
  Trait287 (* impl) = (new Trait287());
  setVec(Trait287_v, S287child2_classId, ((void*)impl));
  ((impl -> trait287) = (& Trait287_S287child2_trait287));
  return impl;
} 
Trait287* Trait287_S287child2_ = newTrait287_S287child2();

int Trait79_S287child1_trait79(Class* self_) {
  S287child1* self = ((S287child1*)self_);
{
    return (self -> a);
  }
} 
Trait79* newTrait79_S287child1() {
  Trait79 (* impl) = (new Trait79());
  setVec(Trait79_v, S287child1_classId, ((void*)impl));
  ((impl -> trait79) = (& Trait79_S287child1_trait79));
  return impl;
} 
Trait79* Trait79_S287child1_ = newTrait79_S287child1();

int S288_classId = Class_genId();
struct S288{

  int id;
  S288 ():id(S288_classId){

  }
};


int S288child1_classId = Class_genId();
struct S288child1:S288{

  S288child1 (int a):a(a){
    (id = S288child1_classId);
{

    }
  }
  int a;
};


int S288child2_classId = Class_genId();
struct S288child2:S288{

  S288child2 (int a):a(a){
    (id = S288child2_classId);
{

    }
  }
  int a;
};


int S288child3_classId = Class_genId();
struct S288child3:S288{

  S288child3 (int a):a(a){
    (id = S288child3_classId);
{

    }
  }
  int a;
};


struct Trait288{

  int(*trait288)(Class*);
};

Vec* Trait288_v = newVec();

int Trait288_S288child1_trait288(Class* self_) {
  S288child1* self = ((S288child1*)self_);
{
    return (self -> a);
  }
} 
Trait288* newTrait288_S288child1() {
  Trait288 (* impl) = (new Trait288());
  setVec(Trait288_v, S288child1_classId, ((void*)impl));
  ((impl -> trait288) = (& Trait288_S288child1_trait288));
  return impl;
} 
Trait288* Trait288_S288child1_ = newTrait288_S288child1();

int Trait288_S288child2_trait288(Class* self_) {
  S288child2* self = ((S288child2*)self_);
{
    return (self -> a);
  }
} 
Trait288* newTrait288_S288child2() {
  Trait288 (* impl) = (new Trait288());
  setVec(Trait288_v, S288child2_classId, ((void*)impl));
  ((impl -> trait288) = (& Trait288_S288child2_trait288));
  return impl;
} 
Trait288* Trait288_S288child2_ = newTrait288_S288child2();

int Trait136_S288child1_trait136(Class* self_) {
  S288child1* self = ((S288child1*)self_);
{
    return (self -> a);
  }
} 
Trait136* newTrait136_S288child1() {
  Trait136 (* impl) = (new Trait136());
  setVec(Trait136_v, S288child1_classId, ((void*)impl));
  ((impl -> trait136) = (& Trait136_S288child1_trait136));
  return impl;
} 
Trait136* Trait136_S288child1_ = newTrait136_S288child1();

int S289_classId = Class_genId();
struct S289{

  int id;
  S289 ():id(S289_classId){

  }
};


int S289child1_classId = Class_genId();
struct S289child1:S289{

  S289child1 (int a):a(a){
    (id = S289child1_classId);
{

    }
  }
  int a;
};


struct Trait289{

  int(*trait289)(Class*);
};

Vec* Trait289_v = newVec();

int Trait289_S289child1_trait289(Class* self_) {
  S289child1* self = ((S289child1*)self_);
{
    return (self -> a);
  }
} 
Trait289* newTrait289_S289child1() {
  Trait289 (* impl) = (new Trait289());
  setVec(Trait289_v, S289child1_classId, ((void*)impl));
  ((impl -> trait289) = (& Trait289_S289child1_trait289));
  return impl;
} 
Trait289* Trait289_S289child1_ = newTrait289_S289child1();

int Trait286_S289child1_trait286(Class* self_) {
  S289child1* self = ((S289child1*)self_);
{
    return (self -> a);
  }
} 
Trait286* newTrait286_S289child1() {
  Trait286 (* impl) = (new Trait286());
  setVec(Trait286_v, S289child1_classId, ((void*)impl));
  ((impl -> trait286) = (& Trait286_S289child1_trait286));
  return impl;
} 
Trait286* Trait286_S289child1_ = newTrait286_S289child1();

int S290_classId = Class_genId();
struct S290{

  int id;
  S290 ():id(S290_classId){

  }
};


int S290child1_classId = Class_genId();
struct S290child1:S290{

  S290child1 (int a):a(a){
    (id = S290child1_classId);
{

    }
  }
  int a;
};


int S290child2_classId = Class_genId();
struct S290child2:S290{

  S290child2 (int a):a(a){
    (id = S290child2_classId);
{

    }
  }
  int a;
};


int S290child3_classId = Class_genId();
struct S290child3:S290{

  S290child3 (int a):a(a){
    (id = S290child3_classId);
{

    }
  }
  int a;
};


int S290child4_classId = Class_genId();
struct S290child4:S290{

  S290child4 (int a):a(a){
    (id = S290child4_classId);
{

    }
  }
  int a;
};


struct Trait290{

  int(*trait290)(Class*);
};

Vec* Trait290_v = newVec();

int Trait290_S290child1_trait290(Class* self_) {
  S290child1* self = ((S290child1*)self_);
{
    return (self -> a);
  }
} 
Trait290* newTrait290_S290child1() {
  Trait290 (* impl) = (new Trait290());
  setVec(Trait290_v, S290child1_classId, ((void*)impl));
  ((impl -> trait290) = (& Trait290_S290child1_trait290));
  return impl;
} 
Trait290* Trait290_S290child1_ = newTrait290_S290child1();

int Trait290_S290child2_trait290(Class* self_) {
  S290child2* self = ((S290child2*)self_);
{
    return (self -> a);
  }
} 
Trait290* newTrait290_S290child2() {
  Trait290 (* impl) = (new Trait290());
  setVec(Trait290_v, S290child2_classId, ((void*)impl));
  ((impl -> trait290) = (& Trait290_S290child2_trait290));
  return impl;
} 
Trait290* Trait290_S290child2_ = newTrait290_S290child2();

int Trait290_S290child3_trait290(Class* self_) {
  S290child3* self = ((S290child3*)self_);
{
    return (self -> a);
  }
} 
Trait290* newTrait290_S290child3() {
  Trait290 (* impl) = (new Trait290());
  setVec(Trait290_v, S290child3_classId, ((void*)impl));
  ((impl -> trait290) = (& Trait290_S290child3_trait290));
  return impl;
} 
Trait290* Trait290_S290child3_ = newTrait290_S290child3();

int Trait41_S290child1_trait41(Class* self_) {
  S290child1* self = ((S290child1*)self_);
{
    return (self -> a);
  }
} 
Trait41* newTrait41_S290child1() {
  Trait41 (* impl) = (new Trait41());
  setVec(Trait41_v, S290child1_classId, ((void*)impl));
  ((impl -> trait41) = (& Trait41_S290child1_trait41));
  return impl;
} 
Trait41* Trait41_S290child1_ = newTrait41_S290child1();

int S291_classId = Class_genId();
struct S291{

  int id;
  S291 ():id(S291_classId){

  }
};


int S291child1_classId = Class_genId();
struct S291child1:S291{

  S291child1 (int a):a(a){
    (id = S291child1_classId);
{

    }
  }
  int a;
};


int S291child2_classId = Class_genId();
struct S291child2:S291{

  S291child2 (int a):a(a){
    (id = S291child2_classId);
{

    }
  }
  int a;
};


struct Trait291{

  int(*trait291)(Class*);
};

Vec* Trait291_v = newVec();

int Trait291_S291child1_trait291(Class* self_) {
  S291child1* self = ((S291child1*)self_);
{
    return (self -> a);
  }
} 
Trait291* newTrait291_S291child1() {
  Trait291 (* impl) = (new Trait291());
  setVec(Trait291_v, S291child1_classId, ((void*)impl));
  ((impl -> trait291) = (& Trait291_S291child1_trait291));
  return impl;
} 
Trait291* Trait291_S291child1_ = newTrait291_S291child1();

int Trait291_S291child2_trait291(Class* self_) {
  S291child2* self = ((S291child2*)self_);
{
    return (self -> a);
  }
} 
Trait291* newTrait291_S291child2() {
  Trait291 (* impl) = (new Trait291());
  setVec(Trait291_v, S291child2_classId, ((void*)impl));
  ((impl -> trait291) = (& Trait291_S291child2_trait291));
  return impl;
} 
Trait291* Trait291_S291child2_ = newTrait291_S291child2();

int Trait237_S291child1_trait237(Class* self_) {
  S291child1* self = ((S291child1*)self_);
{
    return (self -> a);
  }
} 
Trait237* newTrait237_S291child1() {
  Trait237 (* impl) = (new Trait237());
  setVec(Trait237_v, S291child1_classId, ((void*)impl));
  ((impl -> trait237) = (& Trait237_S291child1_trait237));
  return impl;
} 
Trait237* Trait237_S291child1_ = newTrait237_S291child1();

int S292_classId = Class_genId();
struct S292{

  int id;
  S292 ():id(S292_classId){

  }
};


int S292child1_classId = Class_genId();
struct S292child1:S292{

  S292child1 (int a):a(a){
    (id = S292child1_classId);
{

    }
  }
  int a;
};


struct Trait292{

  int(*trait292)(Class*);
};

Vec* Trait292_v = newVec();

int Trait292_S292child1_trait292(Class* self_) {
  S292child1* self = ((S292child1*)self_);
{
    return (self -> a);
  }
} 
Trait292* newTrait292_S292child1() {
  Trait292 (* impl) = (new Trait292());
  setVec(Trait292_v, S292child1_classId, ((void*)impl));
  ((impl -> trait292) = (& Trait292_S292child1_trait292));
  return impl;
} 
Trait292* Trait292_S292child1_ = newTrait292_S292child1();

int Trait91_S292child1_trait91(Class* self_) {
  S292child1* self = ((S292child1*)self_);
{
    return (self -> a);
  }
} 
Trait91* newTrait91_S292child1() {
  Trait91 (* impl) = (new Trait91());
  setVec(Trait91_v, S292child1_classId, ((void*)impl));
  ((impl -> trait91) = (& Trait91_S292child1_trait91));
  return impl;
} 
Trait91* Trait91_S292child1_ = newTrait91_S292child1();

int S293_classId = Class_genId();
struct S293{

  int id;
  S293 ():id(S293_classId){

  }
};


int S293child1_classId = Class_genId();
struct S293child1:S293{

  S293child1 (int a):a(a){
    (id = S293child1_classId);
{

    }
  }
  int a;
};


int S293child2_classId = Class_genId();
struct S293child2:S293{

  S293child2 (int a):a(a){
    (id = S293child2_classId);
{

    }
  }
  int a;
};


int S293child3_classId = Class_genId();
struct S293child3:S293{

  S293child3 (int a):a(a){
    (id = S293child3_classId);
{

    }
  }
  int a;
};


struct Trait293{

  int(*trait293)(Class*);
};

Vec* Trait293_v = newVec();

int Trait293_S293child1_trait293(Class* self_) {
  S293child1* self = ((S293child1*)self_);
{
    return (self -> a);
  }
} 
Trait293* newTrait293_S293child1() {
  Trait293 (* impl) = (new Trait293());
  setVec(Trait293_v, S293child1_classId, ((void*)impl));
  ((impl -> trait293) = (& Trait293_S293child1_trait293));
  return impl;
} 
Trait293* Trait293_S293child1_ = newTrait293_S293child1();

int Trait293_S293child2_trait293(Class* self_) {
  S293child2* self = ((S293child2*)self_);
{
    return (self -> a);
  }
} 
Trait293* newTrait293_S293child2() {
  Trait293 (* impl) = (new Trait293());
  setVec(Trait293_v, S293child2_classId, ((void*)impl));
  ((impl -> trait293) = (& Trait293_S293child2_trait293));
  return impl;
} 
Trait293* Trait293_S293child2_ = newTrait293_S293child2();

int Trait62_S293child1_trait62(Class* self_) {
  S293child1* self = ((S293child1*)self_);
{
    return (self -> a);
  }
} 
Trait62* newTrait62_S293child1() {
  Trait62 (* impl) = (new Trait62());
  setVec(Trait62_v, S293child1_classId, ((void*)impl));
  ((impl -> trait62) = (& Trait62_S293child1_trait62));
  return impl;
} 
Trait62* Trait62_S293child1_ = newTrait62_S293child1();

int S294_classId = Class_genId();
struct S294{

  int id;
  S294 ():id(S294_classId){

  }
};


int S294child1_classId = Class_genId();
struct S294child1:S294{

  S294child1 (int a):a(a){
    (id = S294child1_classId);
{

    }
  }
  int a;
};


int S294child2_classId = Class_genId();
struct S294child2:S294{

  S294child2 (int a):a(a){
    (id = S294child2_classId);
{

    }
  }
  int a;
};


int S294child3_classId = Class_genId();
struct S294child3:S294{

  S294child3 (int a):a(a){
    (id = S294child3_classId);
{

    }
  }
  int a;
};


int S294child4_classId = Class_genId();
struct S294child4:S294{

  S294child4 (int a):a(a){
    (id = S294child4_classId);
{

    }
  }
  int a;
};


struct Trait294{

  int(*trait294)(Class*);
};

Vec* Trait294_v = newVec();

int Trait294_S294child1_trait294(Class* self_) {
  S294child1* self = ((S294child1*)self_);
{
    return (self -> a);
  }
} 
Trait294* newTrait294_S294child1() {
  Trait294 (* impl) = (new Trait294());
  setVec(Trait294_v, S294child1_classId, ((void*)impl));
  ((impl -> trait294) = (& Trait294_S294child1_trait294));
  return impl;
} 
Trait294* Trait294_S294child1_ = newTrait294_S294child1();

int Trait294_S294child2_trait294(Class* self_) {
  S294child2* self = ((S294child2*)self_);
{
    return (self -> a);
  }
} 
Trait294* newTrait294_S294child2() {
  Trait294 (* impl) = (new Trait294());
  setVec(Trait294_v, S294child2_classId, ((void*)impl));
  ((impl -> trait294) = (& Trait294_S294child2_trait294));
  return impl;
} 
Trait294* Trait294_S294child2_ = newTrait294_S294child2();

int Trait294_S294child3_trait294(Class* self_) {
  S294child3* self = ((S294child3*)self_);
{
    return (self -> a);
  }
} 
Trait294* newTrait294_S294child3() {
  Trait294 (* impl) = (new Trait294());
  setVec(Trait294_v, S294child3_classId, ((void*)impl));
  ((impl -> trait294) = (& Trait294_S294child3_trait294));
  return impl;
} 
Trait294* Trait294_S294child3_ = newTrait294_S294child3();

int Trait294_S294child4_trait294(Class* self_) {
  S294child4* self = ((S294child4*)self_);
{
    return (self -> a);
  }
} 
Trait294* newTrait294_S294child4() {
  Trait294 (* impl) = (new Trait294());
  setVec(Trait294_v, S294child4_classId, ((void*)impl));
  ((impl -> trait294) = (& Trait294_S294child4_trait294));
  return impl;
} 
Trait294* Trait294_S294child4_ = newTrait294_S294child4();

int Trait218_S294child1_trait218(Class* self_) {
  S294child1* self = ((S294child1*)self_);
{
    return (self -> a);
  }
} 
Trait218* newTrait218_S294child1() {
  Trait218 (* impl) = (new Trait218());
  setVec(Trait218_v, S294child1_classId, ((void*)impl));
  ((impl -> trait218) = (& Trait218_S294child1_trait218));
  return impl;
} 
Trait218* Trait218_S294child1_ = newTrait218_S294child1();

int S295_classId = Class_genId();
struct S295{

  int id;
  S295 ():id(S295_classId){

  }
};


int S295child1_classId = Class_genId();
struct S295child1:S295{

  S295child1 (int a):a(a){
    (id = S295child1_classId);
{

    }
  }
  int a;
};


int S295child2_classId = Class_genId();
struct S295child2:S295{

  S295child2 (int a):a(a){
    (id = S295child2_classId);
{

    }
  }
  int a;
};


int S295child3_classId = Class_genId();
struct S295child3:S295{

  S295child3 (int a):a(a){
    (id = S295child3_classId);
{

    }
  }
  int a;
};


int S295child4_classId = Class_genId();
struct S295child4:S295{

  S295child4 (int a):a(a){
    (id = S295child4_classId);
{

    }
  }
  int a;
};


struct Trait295{

  int(*trait295)(Class*);
};

Vec* Trait295_v = newVec();

int Trait295_S295child1_trait295(Class* self_) {
  S295child1* self = ((S295child1*)self_);
{
    return (self -> a);
  }
} 
Trait295* newTrait295_S295child1() {
  Trait295 (* impl) = (new Trait295());
  setVec(Trait295_v, S295child1_classId, ((void*)impl));
  ((impl -> trait295) = (& Trait295_S295child1_trait295));
  return impl;
} 
Trait295* Trait295_S295child1_ = newTrait295_S295child1();

int Trait295_S295child2_trait295(Class* self_) {
  S295child2* self = ((S295child2*)self_);
{
    return (self -> a);
  }
} 
Trait295* newTrait295_S295child2() {
  Trait295 (* impl) = (new Trait295());
  setVec(Trait295_v, S295child2_classId, ((void*)impl));
  ((impl -> trait295) = (& Trait295_S295child2_trait295));
  return impl;
} 
Trait295* Trait295_S295child2_ = newTrait295_S295child2();

int Trait295_S295child3_trait295(Class* self_) {
  S295child3* self = ((S295child3*)self_);
{
    return (self -> a);
  }
} 
Trait295* newTrait295_S295child3() {
  Trait295 (* impl) = (new Trait295());
  setVec(Trait295_v, S295child3_classId, ((void*)impl));
  ((impl -> trait295) = (& Trait295_S295child3_trait295));
  return impl;
} 
Trait295* Trait295_S295child3_ = newTrait295_S295child3();

int Trait295_S295child4_trait295(Class* self_) {
  S295child4* self = ((S295child4*)self_);
{
    return (self -> a);
  }
} 
Trait295* newTrait295_S295child4() {
  Trait295 (* impl) = (new Trait295());
  setVec(Trait295_v, S295child4_classId, ((void*)impl));
  ((impl -> trait295) = (& Trait295_S295child4_trait295));
  return impl;
} 
Trait295* Trait295_S295child4_ = newTrait295_S295child4();

int Trait76_S295child1_trait76(Class* self_) {
  S295child1* self = ((S295child1*)self_);
{
    return (self -> a);
  }
} 
Trait76* newTrait76_S295child1() {
  Trait76 (* impl) = (new Trait76());
  setVec(Trait76_v, S295child1_classId, ((void*)impl));
  ((impl -> trait76) = (& Trait76_S295child1_trait76));
  return impl;
} 
Trait76* Trait76_S295child1_ = newTrait76_S295child1();

int S296_classId = Class_genId();
struct S296{

  int id;
  S296 ():id(S296_classId){

  }
};


int S296child1_classId = Class_genId();
struct S296child1:S296{

  S296child1 (int a):a(a){
    (id = S296child1_classId);
{

    }
  }
  int a;
};


int S296child2_classId = Class_genId();
struct S296child2:S296{

  S296child2 (int a):a(a){
    (id = S296child2_classId);
{

    }
  }
  int a;
};


int S296child3_classId = Class_genId();
struct S296child3:S296{

  S296child3 (int a):a(a){
    (id = S296child3_classId);
{

    }
  }
  int a;
};


int S296child4_classId = Class_genId();
struct S296child4:S296{

  S296child4 (int a):a(a){
    (id = S296child4_classId);
{

    }
  }
  int a;
};


struct Trait296{

  int(*trait296)(Class*);
};

Vec* Trait296_v = newVec();

int Trait296_S296child1_trait296(Class* self_) {
  S296child1* self = ((S296child1*)self_);
{
    return (self -> a);
  }
} 
Trait296* newTrait296_S296child1() {
  Trait296 (* impl) = (new Trait296());
  setVec(Trait296_v, S296child1_classId, ((void*)impl));
  ((impl -> trait296) = (& Trait296_S296child1_trait296));
  return impl;
} 
Trait296* Trait296_S296child1_ = newTrait296_S296child1();

int Trait296_S296child2_trait296(Class* self_) {
  S296child2* self = ((S296child2*)self_);
{
    return (self -> a);
  }
} 
Trait296* newTrait296_S296child2() {
  Trait296 (* impl) = (new Trait296());
  setVec(Trait296_v, S296child2_classId, ((void*)impl));
  ((impl -> trait296) = (& Trait296_S296child2_trait296));
  return impl;
} 
Trait296* Trait296_S296child2_ = newTrait296_S296child2();

int Trait77_S296child1_trait77(Class* self_) {
  S296child1* self = ((S296child1*)self_);
{
    return (self -> a);
  }
} 
Trait77* newTrait77_S296child1() {
  Trait77 (* impl) = (new Trait77());
  setVec(Trait77_v, S296child1_classId, ((void*)impl));
  ((impl -> trait77) = (& Trait77_S296child1_trait77));
  return impl;
} 
Trait77* Trait77_S296child1_ = newTrait77_S296child1();

int S297_classId = Class_genId();
struct S297{

  int id;
  S297 ():id(S297_classId){

  }
};


int S297child1_classId = Class_genId();
struct S297child1:S297{

  S297child1 (int a):a(a){
    (id = S297child1_classId);
{

    }
  }
  int a;
};


int S297child2_classId = Class_genId();
struct S297child2:S297{

  S297child2 (int a):a(a){
    (id = S297child2_classId);
{

    }
  }
  int a;
};


int S297child3_classId = Class_genId();
struct S297child3:S297{

  S297child3 (int a):a(a){
    (id = S297child3_classId);
{

    }
  }
  int a;
};


struct Trait297{

  int(*trait297)(Class*);
};

Vec* Trait297_v = newVec();

int Trait297_S297child1_trait297(Class* self_) {
  S297child1* self = ((S297child1*)self_);
{
    return (self -> a);
  }
} 
Trait297* newTrait297_S297child1() {
  Trait297 (* impl) = (new Trait297());
  setVec(Trait297_v, S297child1_classId, ((void*)impl));
  ((impl -> trait297) = (& Trait297_S297child1_trait297));
  return impl;
} 
Trait297* Trait297_S297child1_ = newTrait297_S297child1();

int Trait297_S297child2_trait297(Class* self_) {
  S297child2* self = ((S297child2*)self_);
{
    return (self -> a);
  }
} 
Trait297* newTrait297_S297child2() {
  Trait297 (* impl) = (new Trait297());
  setVec(Trait297_v, S297child2_classId, ((void*)impl));
  ((impl -> trait297) = (& Trait297_S297child2_trait297));
  return impl;
} 
Trait297* Trait297_S297child2_ = newTrait297_S297child2();

int Trait297_S297child3_trait297(Class* self_) {
  S297child3* self = ((S297child3*)self_);
{
    return (self -> a);
  }
} 
Trait297* newTrait297_S297child3() {
  Trait297 (* impl) = (new Trait297());
  setVec(Trait297_v, S297child3_classId, ((void*)impl));
  ((impl -> trait297) = (& Trait297_S297child3_trait297));
  return impl;
} 
Trait297* Trait297_S297child3_ = newTrait297_S297child3();

int Trait194_S297child1_trait194(Class* self_) {
  S297child1* self = ((S297child1*)self_);
{
    return (self -> a);
  }
} 
Trait194* newTrait194_S297child1() {
  Trait194 (* impl) = (new Trait194());
  setVec(Trait194_v, S297child1_classId, ((void*)impl));
  ((impl -> trait194) = (& Trait194_S297child1_trait194));
  return impl;
} 
Trait194* Trait194_S297child1_ = newTrait194_S297child1();

int S298_classId = Class_genId();
struct S298{

  int id;
  S298 ():id(S298_classId){

  }
};


int S298child1_classId = Class_genId();
struct S298child1:S298{

  S298child1 (int a):a(a){
    (id = S298child1_classId);
{

    }
  }
  int a;
};


int S298child2_classId = Class_genId();
struct S298child2:S298{

  S298child2 (int a):a(a){
    (id = S298child2_classId);
{

    }
  }
  int a;
};


int S298child3_classId = Class_genId();
struct S298child3:S298{

  S298child3 (int a):a(a){
    (id = S298child3_classId);
{

    }
  }
  int a;
};


int S298child4_classId = Class_genId();
struct S298child4:S298{

  S298child4 (int a):a(a){
    (id = S298child4_classId);
{

    }
  }
  int a;
};


struct Trait298{

  int(*trait298)(Class*);
};

Vec* Trait298_v = newVec();

int Trait298_S298child1_trait298(Class* self_) {
  S298child1* self = ((S298child1*)self_);
{
    return (self -> a);
  }
} 
Trait298* newTrait298_S298child1() {
  Trait298 (* impl) = (new Trait298());
  setVec(Trait298_v, S298child1_classId, ((void*)impl));
  ((impl -> trait298) = (& Trait298_S298child1_trait298));
  return impl;
} 
Trait298* Trait298_S298child1_ = newTrait298_S298child1();

int Trait141_S298child1_trait141(Class* self_) {
  S298child1* self = ((S298child1*)self_);
{
    return (self -> a);
  }
} 
Trait141* newTrait141_S298child1() {
  Trait141 (* impl) = (new Trait141());
  setVec(Trait141_v, S298child1_classId, ((void*)impl));
  ((impl -> trait141) = (& Trait141_S298child1_trait141));
  return impl;
} 
Trait141* Trait141_S298child1_ = newTrait141_S298child1();

int S299_classId = Class_genId();
struct S299{

  int id;
  S299 ():id(S299_classId){

  }
};


int S299child1_classId = Class_genId();
struct S299child1:S299{

  S299child1 (int a):a(a){
    (id = S299child1_classId);
{

    }
  }
  int a;
};


int S299child2_classId = Class_genId();
struct S299child2:S299{

  S299child2 (int a):a(a){
    (id = S299child2_classId);
{

    }
  }
  int a;
};


struct Trait299{

  int(*trait299)(Class*);
};

Vec* Trait299_v = newVec();

int Trait299_S299child1_trait299(Class* self_) {
  S299child1* self = ((S299child1*)self_);
{
    return (self -> a);
  }
} 
Trait299* newTrait299_S299child1() {
  Trait299 (* impl) = (new Trait299());
  setVec(Trait299_v, S299child1_classId, ((void*)impl));
  ((impl -> trait299) = (& Trait299_S299child1_trait299));
  return impl;
} 
Trait299* Trait299_S299child1_ = newTrait299_S299child1();

int Trait299_S299child2_trait299(Class* self_) {
  S299child2* self = ((S299child2*)self_);
{
    return (self -> a);
  }
} 
Trait299* newTrait299_S299child2() {
  Trait299 (* impl) = (new Trait299());
  setVec(Trait299_v, S299child2_classId, ((void*)impl));
  ((impl -> trait299) = (& Trait299_S299child2_trait299));
  return impl;
} 
Trait299* Trait299_S299child2_ = newTrait299_S299child2();

int Trait202_S299child1_trait202(Class* self_) {
  S299child1* self = ((S299child1*)self_);
{
    return (self -> a);
  }
} 
Trait202* newTrait202_S299child1() {
  Trait202 (* impl) = (new Trait202());
  setVec(Trait202_v, S299child1_classId, ((void*)impl));
  ((impl -> trait202) = (& Trait202_S299child1_trait202));
  return impl;
} 
Trait202* Trait202_S299child1_ = newTrait202_S299child1();

int S300_classId = Class_genId();
struct S300{

  int id;
  S300 ():id(S300_classId){

  }
};


int S300child1_classId = Class_genId();
struct S300child1:S300{

  S300child1 (int a):a(a){
    (id = S300child1_classId);
{

    }
  }
  int a;
};


int S300child2_classId = Class_genId();
struct S300child2:S300{

  S300child2 (int a):a(a){
    (id = S300child2_classId);
{

    }
  }
  int a;
};


int S300child3_classId = Class_genId();
struct S300child3:S300{

  S300child3 (int a):a(a){
    (id = S300child3_classId);
{

    }
  }
  int a;
};


int S300child4_classId = Class_genId();
struct S300child4:S300{

  S300child4 (int a):a(a){
    (id = S300child4_classId);
{

    }
  }
  int a;
};


int S300child5_classId = Class_genId();
struct S300child5:S300{

  S300child5 (int a):a(a){
    (id = S300child5_classId);
{

    }
  }
  int a;
};


struct Trait300{

  int(*trait300)(Class*);
};

Vec* Trait300_v = newVec();

int Trait300_S300child1_trait300(Class* self_) {
  S300child1* self = ((S300child1*)self_);
{
    return (self -> a);
  }
} 
Trait300* newTrait300_S300child1() {
  Trait300 (* impl) = (new Trait300());
  setVec(Trait300_v, S300child1_classId, ((void*)impl));
  ((impl -> trait300) = (& Trait300_S300child1_trait300));
  return impl;
} 
Trait300* Trait300_S300child1_ = newTrait300_S300child1();

int Trait300_S300child2_trait300(Class* self_) {
  S300child2* self = ((S300child2*)self_);
{
    return (self -> a);
  }
} 
Trait300* newTrait300_S300child2() {
  Trait300 (* impl) = (new Trait300());
  setVec(Trait300_v, S300child2_classId, ((void*)impl));
  ((impl -> trait300) = (& Trait300_S300child2_trait300));
  return impl;
} 
Trait300* Trait300_S300child2_ = newTrait300_S300child2();

int Trait300_S300child3_trait300(Class* self_) {
  S300child3* self = ((S300child3*)self_);
{
    return (self -> a);
  }
} 
Trait300* newTrait300_S300child3() {
  Trait300 (* impl) = (new Trait300());
  setVec(Trait300_v, S300child3_classId, ((void*)impl));
  ((impl -> trait300) = (& Trait300_S300child3_trait300));
  return impl;
} 
Trait300* Trait300_S300child3_ = newTrait300_S300child3();

int Trait300_S300child4_trait300(Class* self_) {
  S300child4* self = ((S300child4*)self_);
{
    return (self -> a);
  }
} 
Trait300* newTrait300_S300child4() {
  Trait300 (* impl) = (new Trait300());
  setVec(Trait300_v, S300child4_classId, ((void*)impl));
  ((impl -> trait300) = (& Trait300_S300child4_trait300));
  return impl;
} 
Trait300* Trait300_S300child4_ = newTrait300_S300child4();

int Trait89_S300child1_trait89(Class* self_) {
  S300child1* self = ((S300child1*)self_);
{
    return (self -> a);
  }
} 
Trait89* newTrait89_S300child1() {
  Trait89 (* impl) = (new Trait89());
  setVec(Trait89_v, S300child1_classId, ((void*)impl));
  ((impl -> trait89) = (& Trait89_S300child1_trait89));
  return impl;
} 
Trait89* Trait89_S300child1_ = newTrait89_S300child1();

int S301_classId = Class_genId();
struct S301{

  int id;
  S301 ():id(S301_classId){

  }
};


int S301child1_classId = Class_genId();
struct S301child1:S301{

  S301child1 (int a):a(a){
    (id = S301child1_classId);
{

    }
  }
  int a;
};


int S301child2_classId = Class_genId();
struct S301child2:S301{

  S301child2 (int a):a(a){
    (id = S301child2_classId);
{

    }
  }
  int a;
};


int S301child3_classId = Class_genId();
struct S301child3:S301{

  S301child3 (int a):a(a){
    (id = S301child3_classId);
{

    }
  }
  int a;
};


struct Trait301{

  int(*trait301)(Class*);
};

Vec* Trait301_v = newVec();

int Trait301_S301child1_trait301(Class* self_) {
  S301child1* self = ((S301child1*)self_);
{
    return (self -> a);
  }
} 
Trait301* newTrait301_S301child1() {
  Trait301 (* impl) = (new Trait301());
  setVec(Trait301_v, S301child1_classId, ((void*)impl));
  ((impl -> trait301) = (& Trait301_S301child1_trait301));
  return impl;
} 
Trait301* Trait301_S301child1_ = newTrait301_S301child1();

int Trait301_S301child2_trait301(Class* self_) {
  S301child2* self = ((S301child2*)self_);
{
    return (self -> a);
  }
} 
Trait301* newTrait301_S301child2() {
  Trait301 (* impl) = (new Trait301());
  setVec(Trait301_v, S301child2_classId, ((void*)impl));
  ((impl -> trait301) = (& Trait301_S301child2_trait301));
  return impl;
} 
Trait301* Trait301_S301child2_ = newTrait301_S301child2();

int Trait11_S301child1_trait11(Class* self_) {
  S301child1* self = ((S301child1*)self_);
{
    return (self -> a);
  }
} 
Trait11* newTrait11_S301child1() {
  Trait11 (* impl) = (new Trait11());
  setVec(Trait11_v, S301child1_classId, ((void*)impl));
  ((impl -> trait11) = (& Trait11_S301child1_trait11));
  return impl;
} 
Trait11* Trait11_S301child1_ = newTrait11_S301child1();

int S302_classId = Class_genId();
struct S302{

  int id;
  S302 ():id(S302_classId){

  }
};


int S302child1_classId = Class_genId();
struct S302child1:S302{

  S302child1 (int a):a(a){
    (id = S302child1_classId);
{

    }
  }
  int a;
};


int S302child2_classId = Class_genId();
struct S302child2:S302{

  S302child2 (int a):a(a){
    (id = S302child2_classId);
{

    }
  }
  int a;
};


struct Trait302{

  int(*trait302)(Class*);
};

Vec* Trait302_v = newVec();

int Trait302_S302child1_trait302(Class* self_) {
  S302child1* self = ((S302child1*)self_);
{
    return (self -> a);
  }
} 
Trait302* newTrait302_S302child1() {
  Trait302 (* impl) = (new Trait302());
  setVec(Trait302_v, S302child1_classId, ((void*)impl));
  ((impl -> trait302) = (& Trait302_S302child1_trait302));
  return impl;
} 
Trait302* Trait302_S302child1_ = newTrait302_S302child1();

int Trait192_S302child1_trait192(Class* self_) {
  S302child1* self = ((S302child1*)self_);
{
    return (self -> a);
  }
} 
Trait192* newTrait192_S302child1() {
  Trait192 (* impl) = (new Trait192());
  setVec(Trait192_v, S302child1_classId, ((void*)impl));
  ((impl -> trait192) = (& Trait192_S302child1_trait192));
  return impl;
} 
Trait192* Trait192_S302child1_ = newTrait192_S302child1();

int S303_classId = Class_genId();
struct S303{

  int id;
  S303 ():id(S303_classId){

  }
};


int S303child1_classId = Class_genId();
struct S303child1:S303{

  S303child1 (int a):a(a){
    (id = S303child1_classId);
{

    }
  }
  int a;
};


struct Trait303{

  int(*trait303)(Class*);
};

Vec* Trait303_v = newVec();

int Trait303_S303child1_trait303(Class* self_) {
  S303child1* self = ((S303child1*)self_);
{
    return (self -> a);
  }
} 
Trait303* newTrait303_S303child1() {
  Trait303 (* impl) = (new Trait303());
  setVec(Trait303_v, S303child1_classId, ((void*)impl));
  ((impl -> trait303) = (& Trait303_S303child1_trait303));
  return impl;
} 
Trait303* Trait303_S303child1_ = newTrait303_S303child1();

int Trait205_S303child1_trait205(Class* self_) {
  S303child1* self = ((S303child1*)self_);
{
    return (self -> a);
  }
} 
Trait205* newTrait205_S303child1() {
  Trait205 (* impl) = (new Trait205());
  setVec(Trait205_v, S303child1_classId, ((void*)impl));
  ((impl -> trait205) = (& Trait205_S303child1_trait205));
  return impl;
} 
Trait205* Trait205_S303child1_ = newTrait205_S303child1();

int S304_classId = Class_genId();
struct S304{

  int id;
  S304 ():id(S304_classId){

  }
};


int S304child1_classId = Class_genId();
struct S304child1:S304{

  S304child1 (int a):a(a){
    (id = S304child1_classId);
{

    }
  }
  int a;
};


int S304child2_classId = Class_genId();
struct S304child2:S304{

  S304child2 (int a):a(a){
    (id = S304child2_classId);
{

    }
  }
  int a;
};


struct Trait304{

  int(*trait304)(Class*);
};

Vec* Trait304_v = newVec();

int Trait304_S304child1_trait304(Class* self_) {
  S304child1* self = ((S304child1*)self_);
{
    return (self -> a);
  }
} 
Trait304* newTrait304_S304child1() {
  Trait304 (* impl) = (new Trait304());
  setVec(Trait304_v, S304child1_classId, ((void*)impl));
  ((impl -> trait304) = (& Trait304_S304child1_trait304));
  return impl;
} 
Trait304* Trait304_S304child1_ = newTrait304_S304child1();

int Trait304_S304child2_trait304(Class* self_) {
  S304child2* self = ((S304child2*)self_);
{
    return (self -> a);
  }
} 
Trait304* newTrait304_S304child2() {
  Trait304 (* impl) = (new Trait304());
  setVec(Trait304_v, S304child2_classId, ((void*)impl));
  ((impl -> trait304) = (& Trait304_S304child2_trait304));
  return impl;
} 
Trait304* Trait304_S304child2_ = newTrait304_S304child2();

int Trait221_S304child1_trait221(Class* self_) {
  S304child1* self = ((S304child1*)self_);
{
    return (self -> a);
  }
} 
Trait221* newTrait221_S304child1() {
  Trait221 (* impl) = (new Trait221());
  setVec(Trait221_v, S304child1_classId, ((void*)impl));
  ((impl -> trait221) = (& Trait221_S304child1_trait221));
  return impl;
} 
Trait221* Trait221_S304child1_ = newTrait221_S304child1();

int S305_classId = Class_genId();
struct S305{

  int id;
  S305 ():id(S305_classId){

  }
};


int S305child1_classId = Class_genId();
struct S305child1:S305{

  S305child1 (int a):a(a){
    (id = S305child1_classId);
{

    }
  }
  int a;
};


int S305child2_classId = Class_genId();
struct S305child2:S305{

  S305child2 (int a):a(a){
    (id = S305child2_classId);
{

    }
  }
  int a;
};


int S305child3_classId = Class_genId();
struct S305child3:S305{

  S305child3 (int a):a(a){
    (id = S305child3_classId);
{

    }
  }
  int a;
};


struct Trait305{

  int(*trait305)(Class*);
};

Vec* Trait305_v = newVec();

int Trait305_S305child1_trait305(Class* self_) {
  S305child1* self = ((S305child1*)self_);
{
    return (self -> a);
  }
} 
Trait305* newTrait305_S305child1() {
  Trait305 (* impl) = (new Trait305());
  setVec(Trait305_v, S305child1_classId, ((void*)impl));
  ((impl -> trait305) = (& Trait305_S305child1_trait305));
  return impl;
} 
Trait305* Trait305_S305child1_ = newTrait305_S305child1();

int Trait229_S305child1_trait229(Class* self_) {
  S305child1* self = ((S305child1*)self_);
{
    return (self -> a);
  }
} 
Trait229* newTrait229_S305child1() {
  Trait229 (* impl) = (new Trait229());
  setVec(Trait229_v, S305child1_classId, ((void*)impl));
  ((impl -> trait229) = (& Trait229_S305child1_trait229));
  return impl;
} 
Trait229* Trait229_S305child1_ = newTrait229_S305child1();

int S306_classId = Class_genId();
struct S306{

  int id;
  S306 ():id(S306_classId){

  }
};


int S306child1_classId = Class_genId();
struct S306child1:S306{

  S306child1 (int a):a(a){
    (id = S306child1_classId);
{

    }
  }
  int a;
};


int S306child2_classId = Class_genId();
struct S306child2:S306{

  S306child2 (int a):a(a){
    (id = S306child2_classId);
{

    }
  }
  int a;
};


struct Trait306{

  int(*trait306)(Class*);
};

Vec* Trait306_v = newVec();

int Trait306_S306child1_trait306(Class* self_) {
  S306child1* self = ((S306child1*)self_);
{
    return (self -> a);
  }
} 
Trait306* newTrait306_S306child1() {
  Trait306 (* impl) = (new Trait306());
  setVec(Trait306_v, S306child1_classId, ((void*)impl));
  ((impl -> trait306) = (& Trait306_S306child1_trait306));
  return impl;
} 
Trait306* Trait306_S306child1_ = newTrait306_S306child1();

int Trait306_S306child2_trait306(Class* self_) {
  S306child2* self = ((S306child2*)self_);
{
    return (self -> a);
  }
} 
Trait306* newTrait306_S306child2() {
  Trait306 (* impl) = (new Trait306());
  setVec(Trait306_v, S306child2_classId, ((void*)impl));
  ((impl -> trait306) = (& Trait306_S306child2_trait306));
  return impl;
} 
Trait306* Trait306_S306child2_ = newTrait306_S306child2();

int Trait86_S306child1_trait86(Class* self_) {
  S306child1* self = ((S306child1*)self_);
{
    return (self -> a);
  }
} 
Trait86* newTrait86_S306child1() {
  Trait86 (* impl) = (new Trait86());
  setVec(Trait86_v, S306child1_classId, ((void*)impl));
  ((impl -> trait86) = (& Trait86_S306child1_trait86));
  return impl;
} 
Trait86* Trait86_S306child1_ = newTrait86_S306child1();

int S307_classId = Class_genId();
struct S307{

  int id;
  S307 ():id(S307_classId){

  }
};


int S307child1_classId = Class_genId();
struct S307child1:S307{

  S307child1 (int a):a(a){
    (id = S307child1_classId);
{

    }
  }
  int a;
};


int S307child2_classId = Class_genId();
struct S307child2:S307{

  S307child2 (int a):a(a){
    (id = S307child2_classId);
{

    }
  }
  int a;
};


int S307child3_classId = Class_genId();
struct S307child3:S307{

  S307child3 (int a):a(a){
    (id = S307child3_classId);
{

    }
  }
  int a;
};


int S307child4_classId = Class_genId();
struct S307child4:S307{

  S307child4 (int a):a(a){
    (id = S307child4_classId);
{

    }
  }
  int a;
};


int S307child5_classId = Class_genId();
struct S307child5:S307{

  S307child5 (int a):a(a){
    (id = S307child5_classId);
{

    }
  }
  int a;
};


struct Trait307{

  int(*trait307)(Class*);
};

Vec* Trait307_v = newVec();

int Trait307_S307child1_trait307(Class* self_) {
  S307child1* self = ((S307child1*)self_);
{
    return (self -> a);
  }
} 
Trait307* newTrait307_S307child1() {
  Trait307 (* impl) = (new Trait307());
  setVec(Trait307_v, S307child1_classId, ((void*)impl));
  ((impl -> trait307) = (& Trait307_S307child1_trait307));
  return impl;
} 
Trait307* Trait307_S307child1_ = newTrait307_S307child1();

int Trait307_S307child2_trait307(Class* self_) {
  S307child2* self = ((S307child2*)self_);
{
    return (self -> a);
  }
} 
Trait307* newTrait307_S307child2() {
  Trait307 (* impl) = (new Trait307());
  setVec(Trait307_v, S307child2_classId, ((void*)impl));
  ((impl -> trait307) = (& Trait307_S307child2_trait307));
  return impl;
} 
Trait307* Trait307_S307child2_ = newTrait307_S307child2();

int Trait145_S307child1_trait145(Class* self_) {
  S307child1* self = ((S307child1*)self_);
{
    return (self -> a);
  }
} 
Trait145* newTrait145_S307child1() {
  Trait145 (* impl) = (new Trait145());
  setVec(Trait145_v, S307child1_classId, ((void*)impl));
  ((impl -> trait145) = (& Trait145_S307child1_trait145));
  return impl;
} 
Trait145* Trait145_S307child1_ = newTrait145_S307child1();

int S308_classId = Class_genId();
struct S308{

  int id;
  S308 ():id(S308_classId){

  }
};


int S308child1_classId = Class_genId();
struct S308child1:S308{

  S308child1 (int a):a(a){
    (id = S308child1_classId);
{

    }
  }
  int a;
};


int S308child2_classId = Class_genId();
struct S308child2:S308{

  S308child2 (int a):a(a){
    (id = S308child2_classId);
{

    }
  }
  int a;
};


int S308child3_classId = Class_genId();
struct S308child3:S308{

  S308child3 (int a):a(a){
    (id = S308child3_classId);
{

    }
  }
  int a;
};


struct Trait308{

  int(*trait308)(Class*);
};

Vec* Trait308_v = newVec();

int Trait308_S308child1_trait308(Class* self_) {
  S308child1* self = ((S308child1*)self_);
{
    return (self -> a);
  }
} 
Trait308* newTrait308_S308child1() {
  Trait308 (* impl) = (new Trait308());
  setVec(Trait308_v, S308child1_classId, ((void*)impl));
  ((impl -> trait308) = (& Trait308_S308child1_trait308));
  return impl;
} 
Trait308* Trait308_S308child1_ = newTrait308_S308child1();

int Trait308_S308child2_trait308(Class* self_) {
  S308child2* self = ((S308child2*)self_);
{
    return (self -> a);
  }
} 
Trait308* newTrait308_S308child2() {
  Trait308 (* impl) = (new Trait308());
  setVec(Trait308_v, S308child2_classId, ((void*)impl));
  ((impl -> trait308) = (& Trait308_S308child2_trait308));
  return impl;
} 
Trait308* Trait308_S308child2_ = newTrait308_S308child2();

int Trait308_S308child3_trait308(Class* self_) {
  S308child3* self = ((S308child3*)self_);
{
    return (self -> a);
  }
} 
Trait308* newTrait308_S308child3() {
  Trait308 (* impl) = (new Trait308());
  setVec(Trait308_v, S308child3_classId, ((void*)impl));
  ((impl -> trait308) = (& Trait308_S308child3_trait308));
  return impl;
} 
Trait308* Trait308_S308child3_ = newTrait308_S308child3();

int Trait232_S308child1_trait232(Class* self_) {
  S308child1* self = ((S308child1*)self_);
{
    return (self -> a);
  }
} 
Trait232* newTrait232_S308child1() {
  Trait232 (* impl) = (new Trait232());
  setVec(Trait232_v, S308child1_classId, ((void*)impl));
  ((impl -> trait232) = (& Trait232_S308child1_trait232));
  return impl;
} 
Trait232* Trait232_S308child1_ = newTrait232_S308child1();

int S309_classId = Class_genId();
struct S309{

  int id;
  S309 ():id(S309_classId){

  }
};


int S309child1_classId = Class_genId();
struct S309child1:S309{

  S309child1 (int a):a(a){
    (id = S309child1_classId);
{

    }
  }
  int a;
};


int S309child2_classId = Class_genId();
struct S309child2:S309{

  S309child2 (int a):a(a){
    (id = S309child2_classId);
{

    }
  }
  int a;
};


int S309child3_classId = Class_genId();
struct S309child3:S309{

  S309child3 (int a):a(a){
    (id = S309child3_classId);
{

    }
  }
  int a;
};


struct Trait309{

  int(*trait309)(Class*);
};

Vec* Trait309_v = newVec();

int Trait309_S309child1_trait309(Class* self_) {
  S309child1* self = ((S309child1*)self_);
{
    return (self -> a);
  }
} 
Trait309* newTrait309_S309child1() {
  Trait309 (* impl) = (new Trait309());
  setVec(Trait309_v, S309child1_classId, ((void*)impl));
  ((impl -> trait309) = (& Trait309_S309child1_trait309));
  return impl;
} 
Trait309* Trait309_S309child1_ = newTrait309_S309child1();

int Trait309_S309child2_trait309(Class* self_) {
  S309child2* self = ((S309child2*)self_);
{
    return (self -> a);
  }
} 
Trait309* newTrait309_S309child2() {
  Trait309 (* impl) = (new Trait309());
  setVec(Trait309_v, S309child2_classId, ((void*)impl));
  ((impl -> trait309) = (& Trait309_S309child2_trait309));
  return impl;
} 
Trait309* Trait309_S309child2_ = newTrait309_S309child2();

int Trait309_S309child3_trait309(Class* self_) {
  S309child3* self = ((S309child3*)self_);
{
    return (self -> a);
  }
} 
Trait309* newTrait309_S309child3() {
  Trait309 (* impl) = (new Trait309());
  setVec(Trait309_v, S309child3_classId, ((void*)impl));
  ((impl -> trait309) = (& Trait309_S309child3_trait309));
  return impl;
} 
Trait309* Trait309_S309child3_ = newTrait309_S309child3();

int Trait159_S309child1_trait159(Class* self_) {
  S309child1* self = ((S309child1*)self_);
{
    return (self -> a);
  }
} 
Trait159* newTrait159_S309child1() {
  Trait159 (* impl) = (new Trait159());
  setVec(Trait159_v, S309child1_classId, ((void*)impl));
  ((impl -> trait159) = (& Trait159_S309child1_trait159));
  return impl;
} 
Trait159* Trait159_S309child1_ = newTrait159_S309child1();

int S310_classId = Class_genId();
struct S310{

  int id;
  S310 ():id(S310_classId){

  }
};


int S310child1_classId = Class_genId();
struct S310child1:S310{

  S310child1 (int a):a(a){
    (id = S310child1_classId);
{

    }
  }
  int a;
};


int S310child2_classId = Class_genId();
struct S310child2:S310{

  S310child2 (int a):a(a){
    (id = S310child2_classId);
{

    }
  }
  int a;
};


int S310child3_classId = Class_genId();
struct S310child3:S310{

  S310child3 (int a):a(a){
    (id = S310child3_classId);
{

    }
  }
  int a;
};


struct Trait310{

  int(*trait310)(Class*);
};

Vec* Trait310_v = newVec();

int Trait310_S310child1_trait310(Class* self_) {
  S310child1* self = ((S310child1*)self_);
{
    return (self -> a);
  }
} 
Trait310* newTrait310_S310child1() {
  Trait310 (* impl) = (new Trait310());
  setVec(Trait310_v, S310child1_classId, ((void*)impl));
  ((impl -> trait310) = (& Trait310_S310child1_trait310));
  return impl;
} 
Trait310* Trait310_S310child1_ = newTrait310_S310child1();

int Trait310_S310child2_trait310(Class* self_) {
  S310child2* self = ((S310child2*)self_);
{
    return (self -> a);
  }
} 
Trait310* newTrait310_S310child2() {
  Trait310 (* impl) = (new Trait310());
  setVec(Trait310_v, S310child2_classId, ((void*)impl));
  ((impl -> trait310) = (& Trait310_S310child2_trait310));
  return impl;
} 
Trait310* Trait310_S310child2_ = newTrait310_S310child2();

int Trait310_S310child3_trait310(Class* self_) {
  S310child3* self = ((S310child3*)self_);
{
    return (self -> a);
  }
} 
Trait310* newTrait310_S310child3() {
  Trait310 (* impl) = (new Trait310());
  setVec(Trait310_v, S310child3_classId, ((void*)impl));
  ((impl -> trait310) = (& Trait310_S310child3_trait310));
  return impl;
} 
Trait310* Trait310_S310child3_ = newTrait310_S310child3();

int Trait217_S310child1_trait217(Class* self_) {
  S310child1* self = ((S310child1*)self_);
{
    return (self -> a);
  }
} 
Trait217* newTrait217_S310child1() {
  Trait217 (* impl) = (new Trait217());
  setVec(Trait217_v, S310child1_classId, ((void*)impl));
  ((impl -> trait217) = (& Trait217_S310child1_trait217));
  return impl;
} 
Trait217* Trait217_S310child1_ = newTrait217_S310child1();

int S311_classId = Class_genId();
struct S311{

  int id;
  S311 ():id(S311_classId){

  }
};


int S311child1_classId = Class_genId();
struct S311child1:S311{

  S311child1 (int a):a(a){
    (id = S311child1_classId);
{

    }
  }
  int a;
};


int S311child2_classId = Class_genId();
struct S311child2:S311{

  S311child2 (int a):a(a){
    (id = S311child2_classId);
{

    }
  }
  int a;
};


struct Trait311{

  int(*trait311)(Class*);
};

Vec* Trait311_v = newVec();

int Trait311_S311child1_trait311(Class* self_) {
  S311child1* self = ((S311child1*)self_);
{
    return (self -> a);
  }
} 
Trait311* newTrait311_S311child1() {
  Trait311 (* impl) = (new Trait311());
  setVec(Trait311_v, S311child1_classId, ((void*)impl));
  ((impl -> trait311) = (& Trait311_S311child1_trait311));
  return impl;
} 
Trait311* Trait311_S311child1_ = newTrait311_S311child1();

int Trait311_S311child2_trait311(Class* self_) {
  S311child2* self = ((S311child2*)self_);
{
    return (self -> a);
  }
} 
Trait311* newTrait311_S311child2() {
  Trait311 (* impl) = (new Trait311());
  setVec(Trait311_v, S311child2_classId, ((void*)impl));
  ((impl -> trait311) = (& Trait311_S311child2_trait311));
  return impl;
} 
Trait311* Trait311_S311child2_ = newTrait311_S311child2();

int Trait281_S311child1_trait281(Class* self_) {
  S311child1* self = ((S311child1*)self_);
{
    return (self -> a);
  }
} 
Trait281* newTrait281_S311child1() {
  Trait281 (* impl) = (new Trait281());
  setVec(Trait281_v, S311child1_classId, ((void*)impl));
  ((impl -> trait281) = (& Trait281_S311child1_trait281));
  return impl;
} 
Trait281* Trait281_S311child1_ = newTrait281_S311child1();

int S312_classId = Class_genId();
struct S312{

  int id;
  S312 ():id(S312_classId){

  }
};


int S312child1_classId = Class_genId();
struct S312child1:S312{

  S312child1 (int a):a(a){
    (id = S312child1_classId);
{

    }
  }
  int a;
};


int S312child2_classId = Class_genId();
struct S312child2:S312{

  S312child2 (int a):a(a){
    (id = S312child2_classId);
{

    }
  }
  int a;
};


int S312child3_classId = Class_genId();
struct S312child3:S312{

  S312child3 (int a):a(a){
    (id = S312child3_classId);
{

    }
  }
  int a;
};


struct Trait312{

  int(*trait312)(Class*);
};

Vec* Trait312_v = newVec();

int Trait312_S312child1_trait312(Class* self_) {
  S312child1* self = ((S312child1*)self_);
{
    return (self -> a);
  }
} 
Trait312* newTrait312_S312child1() {
  Trait312 (* impl) = (new Trait312());
  setVec(Trait312_v, S312child1_classId, ((void*)impl));
  ((impl -> trait312) = (& Trait312_S312child1_trait312));
  return impl;
} 
Trait312* Trait312_S312child1_ = newTrait312_S312child1();

int Trait312_S312child2_trait312(Class* self_) {
  S312child2* self = ((S312child2*)self_);
{
    return (self -> a);
  }
} 
Trait312* newTrait312_S312child2() {
  Trait312 (* impl) = (new Trait312());
  setVec(Trait312_v, S312child2_classId, ((void*)impl));
  ((impl -> trait312) = (& Trait312_S312child2_trait312));
  return impl;
} 
Trait312* Trait312_S312child2_ = newTrait312_S312child2();

int Trait246_S312child1_trait246(Class* self_) {
  S312child1* self = ((S312child1*)self_);
{
    return (self -> a);
  }
} 
Trait246* newTrait246_S312child1() {
  Trait246 (* impl) = (new Trait246());
  setVec(Trait246_v, S312child1_classId, ((void*)impl));
  ((impl -> trait246) = (& Trait246_S312child1_trait246));
  return impl;
} 
Trait246* Trait246_S312child1_ = newTrait246_S312child1();

int S313_classId = Class_genId();
struct S313{

  int id;
  S313 ():id(S313_classId){

  }
};


int S313child1_classId = Class_genId();
struct S313child1:S313{

  S313child1 (int a):a(a){
    (id = S313child1_classId);
{

    }
  }
  int a;
};


int S313child2_classId = Class_genId();
struct S313child2:S313{

  S313child2 (int a):a(a){
    (id = S313child2_classId);
{

    }
  }
  int a;
};


struct Trait313{

  int(*trait313)(Class*);
};

Vec* Trait313_v = newVec();

int Trait313_S313child1_trait313(Class* self_) {
  S313child1* self = ((S313child1*)self_);
{
    return (self -> a);
  }
} 
Trait313* newTrait313_S313child1() {
  Trait313 (* impl) = (new Trait313());
  setVec(Trait313_v, S313child1_classId, ((void*)impl));
  ((impl -> trait313) = (& Trait313_S313child1_trait313));
  return impl;
} 
Trait313* Trait313_S313child1_ = newTrait313_S313child1();

int Trait313_S313child2_trait313(Class* self_) {
  S313child2* self = ((S313child2*)self_);
{
    return (self -> a);
  }
} 
Trait313* newTrait313_S313child2() {
  Trait313 (* impl) = (new Trait313());
  setVec(Trait313_v, S313child2_classId, ((void*)impl));
  ((impl -> trait313) = (& Trait313_S313child2_trait313));
  return impl;
} 
Trait313* Trait313_S313child2_ = newTrait313_S313child2();

int Trait128_S313child1_trait128(Class* self_) {
  S313child1* self = ((S313child1*)self_);
{
    return (self -> a);
  }
} 
Trait128* newTrait128_S313child1() {
  Trait128 (* impl) = (new Trait128());
  setVec(Trait128_v, S313child1_classId, ((void*)impl));
  ((impl -> trait128) = (& Trait128_S313child1_trait128));
  return impl;
} 
Trait128* Trait128_S313child1_ = newTrait128_S313child1();

int S314_classId = Class_genId();
struct S314{

  int id;
  S314 ():id(S314_classId){

  }
};


int S314child1_classId = Class_genId();
struct S314child1:S314{

  S314child1 (int a):a(a){
    (id = S314child1_classId);
{

    }
  }
  int a;
};


struct Trait314{

  int(*trait314)(Class*);
};

Vec* Trait314_v = newVec();

int Trait314_S314child1_trait314(Class* self_) {
  S314child1* self = ((S314child1*)self_);
{
    return (self -> a);
  }
} 
Trait314* newTrait314_S314child1() {
  Trait314 (* impl) = (new Trait314());
  setVec(Trait314_v, S314child1_classId, ((void*)impl));
  ((impl -> trait314) = (& Trait314_S314child1_trait314));
  return impl;
} 
Trait314* Trait314_S314child1_ = newTrait314_S314child1();

int Trait62_S314child1_trait62(Class* self_) {
  S314child1* self = ((S314child1*)self_);
{
    return (self -> a);
  }
} 
Trait62* newTrait62_S314child1() {
  Trait62 (* impl) = (new Trait62());
  setVec(Trait62_v, S314child1_classId, ((void*)impl));
  ((impl -> trait62) = (& Trait62_S314child1_trait62));
  return impl;
} 
Trait62* Trait62_S314child1_ = newTrait62_S314child1();

int S315_classId = Class_genId();
struct S315{

  int id;
  S315 ():id(S315_classId){

  }
};


int S315child1_classId = Class_genId();
struct S315child1:S315{

  S315child1 (int a):a(a){
    (id = S315child1_classId);
{

    }
  }
  int a;
};


struct Trait315{

  int(*trait315)(Class*);
};

Vec* Trait315_v = newVec();

int Trait315_S315child1_trait315(Class* self_) {
  S315child1* self = ((S315child1*)self_);
{
    return (self -> a);
  }
} 
Trait315* newTrait315_S315child1() {
  Trait315 (* impl) = (new Trait315());
  setVec(Trait315_v, S315child1_classId, ((void*)impl));
  ((impl -> trait315) = (& Trait315_S315child1_trait315));
  return impl;
} 
Trait315* Trait315_S315child1_ = newTrait315_S315child1();

int Trait225_S315child1_trait225(Class* self_) {
  S315child1* self = ((S315child1*)self_);
{
    return (self -> a);
  }
} 
Trait225* newTrait225_S315child1() {
  Trait225 (* impl) = (new Trait225());
  setVec(Trait225_v, S315child1_classId, ((void*)impl));
  ((impl -> trait225) = (& Trait225_S315child1_trait225));
  return impl;
} 
Trait225* Trait225_S315child1_ = newTrait225_S315child1();

int S316_classId = Class_genId();
struct S316{

  int id;
  S316 ():id(S316_classId){

  }
};


int S316child1_classId = Class_genId();
struct S316child1:S316{

  S316child1 (int a):a(a){
    (id = S316child1_classId);
{

    }
  }
  int a;
};


int S316child2_classId = Class_genId();
struct S316child2:S316{

  S316child2 (int a):a(a){
    (id = S316child2_classId);
{

    }
  }
  int a;
};


int S316child3_classId = Class_genId();
struct S316child3:S316{

  S316child3 (int a):a(a){
    (id = S316child3_classId);
{

    }
  }
  int a;
};


struct Trait316{

  int(*trait316)(Class*);
};

Vec* Trait316_v = newVec();

int Trait316_S316child1_trait316(Class* self_) {
  S316child1* self = ((S316child1*)self_);
{
    return (self -> a);
  }
} 
Trait316* newTrait316_S316child1() {
  Trait316 (* impl) = (new Trait316());
  setVec(Trait316_v, S316child1_classId, ((void*)impl));
  ((impl -> trait316) = (& Trait316_S316child1_trait316));
  return impl;
} 
Trait316* Trait316_S316child1_ = newTrait316_S316child1();

int Trait316_S316child2_trait316(Class* self_) {
  S316child2* self = ((S316child2*)self_);
{
    return (self -> a);
  }
} 
Trait316* newTrait316_S316child2() {
  Trait316 (* impl) = (new Trait316());
  setVec(Trait316_v, S316child2_classId, ((void*)impl));
  ((impl -> trait316) = (& Trait316_S316child2_trait316));
  return impl;
} 
Trait316* Trait316_S316child2_ = newTrait316_S316child2();

int Trait102_S316child1_trait102(Class* self_) {
  S316child1* self = ((S316child1*)self_);
{
    return (self -> a);
  }
} 
Trait102* newTrait102_S316child1() {
  Trait102 (* impl) = (new Trait102());
  setVec(Trait102_v, S316child1_classId, ((void*)impl));
  ((impl -> trait102) = (& Trait102_S316child1_trait102));
  return impl;
} 
Trait102* Trait102_S316child1_ = newTrait102_S316child1();

int S317_classId = Class_genId();
struct S317{

  int id;
  S317 ():id(S317_classId){

  }
};


int S317child1_classId = Class_genId();
struct S317child1:S317{

  S317child1 (int a):a(a){
    (id = S317child1_classId);
{

    }
  }
  int a;
};


int S317child2_classId = Class_genId();
struct S317child2:S317{

  S317child2 (int a):a(a){
    (id = S317child2_classId);
{

    }
  }
  int a;
};


struct Trait317{

  int(*trait317)(Class*);
};

Vec* Trait317_v = newVec();

int Trait317_S317child1_trait317(Class* self_) {
  S317child1* self = ((S317child1*)self_);
{
    return (self -> a);
  }
} 
Trait317* newTrait317_S317child1() {
  Trait317 (* impl) = (new Trait317());
  setVec(Trait317_v, S317child1_classId, ((void*)impl));
  ((impl -> trait317) = (& Trait317_S317child1_trait317));
  return impl;
} 
Trait317* Trait317_S317child1_ = newTrait317_S317child1();

int Trait21_S317child1_trait21(Class* self_) {
  S317child1* self = ((S317child1*)self_);
{
    return (self -> a);
  }
} 
Trait21* newTrait21_S317child1() {
  Trait21 (* impl) = (new Trait21());
  setVec(Trait21_v, S317child1_classId, ((void*)impl));
  ((impl -> trait21) = (& Trait21_S317child1_trait21));
  return impl;
} 
Trait21* Trait21_S317child1_ = newTrait21_S317child1();

int S318_classId = Class_genId();
struct S318{

  int id;
  S318 ():id(S318_classId){

  }
};


int S318child1_classId = Class_genId();
struct S318child1:S318{

  S318child1 (int a):a(a){
    (id = S318child1_classId);
{

    }
  }
  int a;
};


int S318child2_classId = Class_genId();
struct S318child2:S318{

  S318child2 (int a):a(a){
    (id = S318child2_classId);
{

    }
  }
  int a;
};


int S318child3_classId = Class_genId();
struct S318child3:S318{

  S318child3 (int a):a(a){
    (id = S318child3_classId);
{

    }
  }
  int a;
};


struct Trait318{

  int(*trait318)(Class*);
};

Vec* Trait318_v = newVec();

int Trait318_S318child1_trait318(Class* self_) {
  S318child1* self = ((S318child1*)self_);
{
    return (self -> a);
  }
} 
Trait318* newTrait318_S318child1() {
  Trait318 (* impl) = (new Trait318());
  setVec(Trait318_v, S318child1_classId, ((void*)impl));
  ((impl -> trait318) = (& Trait318_S318child1_trait318));
  return impl;
} 
Trait318* Trait318_S318child1_ = newTrait318_S318child1();

int Trait318_S318child2_trait318(Class* self_) {
  S318child2* self = ((S318child2*)self_);
{
    return (self -> a);
  }
} 
Trait318* newTrait318_S318child2() {
  Trait318 (* impl) = (new Trait318());
  setVec(Trait318_v, S318child2_classId, ((void*)impl));
  ((impl -> trait318) = (& Trait318_S318child2_trait318));
  return impl;
} 
Trait318* Trait318_S318child2_ = newTrait318_S318child2();

int Trait318_S318child3_trait318(Class* self_) {
  S318child3* self = ((S318child3*)self_);
{
    return (self -> a);
  }
} 
Trait318* newTrait318_S318child3() {
  Trait318 (* impl) = (new Trait318());
  setVec(Trait318_v, S318child3_classId, ((void*)impl));
  ((impl -> trait318) = (& Trait318_S318child3_trait318));
  return impl;
} 
Trait318* Trait318_S318child3_ = newTrait318_S318child3();

int Trait172_S318child1_trait172(Class* self_) {
  S318child1* self = ((S318child1*)self_);
{
    return (self -> a);
  }
} 
Trait172* newTrait172_S318child1() {
  Trait172 (* impl) = (new Trait172());
  setVec(Trait172_v, S318child1_classId, ((void*)impl));
  ((impl -> trait172) = (& Trait172_S318child1_trait172));
  return impl;
} 
Trait172* Trait172_S318child1_ = newTrait172_S318child1();

int S319_classId = Class_genId();
struct S319{

  int id;
  S319 ():id(S319_classId){

  }
};


int S319child1_classId = Class_genId();
struct S319child1:S319{

  S319child1 (int a):a(a){
    (id = S319child1_classId);
{

    }
  }
  int a;
};


int S319child2_classId = Class_genId();
struct S319child2:S319{

  S319child2 (int a):a(a){
    (id = S319child2_classId);
{

    }
  }
  int a;
};


int S319child3_classId = Class_genId();
struct S319child3:S319{

  S319child3 (int a):a(a){
    (id = S319child3_classId);
{

    }
  }
  int a;
};


struct Trait319{

  int(*trait319)(Class*);
};

Vec* Trait319_v = newVec();

int Trait319_S319child1_trait319(Class* self_) {
  S319child1* self = ((S319child1*)self_);
{
    return (self -> a);
  }
} 
Trait319* newTrait319_S319child1() {
  Trait319 (* impl) = (new Trait319());
  setVec(Trait319_v, S319child1_classId, ((void*)impl));
  ((impl -> trait319) = (& Trait319_S319child1_trait319));
  return impl;
} 
Trait319* Trait319_S319child1_ = newTrait319_S319child1();

int Trait219_S319child1_trait219(Class* self_) {
  S319child1* self = ((S319child1*)self_);
{
    return (self -> a);
  }
} 
Trait219* newTrait219_S319child1() {
  Trait219 (* impl) = (new Trait219());
  setVec(Trait219_v, S319child1_classId, ((void*)impl));
  ((impl -> trait219) = (& Trait219_S319child1_trait219));
  return impl;
} 
Trait219* Trait219_S319child1_ = newTrait219_S319child1();

int S320_classId = Class_genId();
struct S320{

  int id;
  S320 ():id(S320_classId){

  }
};


int S320child1_classId = Class_genId();
struct S320child1:S320{

  S320child1 (int a):a(a){
    (id = S320child1_classId);
{

    }
  }
  int a;
};


int S320child2_classId = Class_genId();
struct S320child2:S320{

  S320child2 (int a):a(a){
    (id = S320child2_classId);
{

    }
  }
  int a;
};


struct Trait320{

  int(*trait320)(Class*);
};

Vec* Trait320_v = newVec();

int Trait320_S320child1_trait320(Class* self_) {
  S320child1* self = ((S320child1*)self_);
{
    return (self -> a);
  }
} 
Trait320* newTrait320_S320child1() {
  Trait320 (* impl) = (new Trait320());
  setVec(Trait320_v, S320child1_classId, ((void*)impl));
  ((impl -> trait320) = (& Trait320_S320child1_trait320));
  return impl;
} 
Trait320* Trait320_S320child1_ = newTrait320_S320child1();

int Trait48_S320child1_trait48(Class* self_) {
  S320child1* self = ((S320child1*)self_);
{
    return (self -> a);
  }
} 
Trait48* newTrait48_S320child1() {
  Trait48 (* impl) = (new Trait48());
  setVec(Trait48_v, S320child1_classId, ((void*)impl));
  ((impl -> trait48) = (& Trait48_S320child1_trait48));
  return impl;
} 
Trait48* Trait48_S320child1_ = newTrait48_S320child1();

int S321_classId = Class_genId();
struct S321{

  int id;
  S321 ():id(S321_classId){

  }
};


int S321child1_classId = Class_genId();
struct S321child1:S321{

  S321child1 (int a):a(a){
    (id = S321child1_classId);
{

    }
  }
  int a;
};


int S321child2_classId = Class_genId();
struct S321child2:S321{

  S321child2 (int a):a(a){
    (id = S321child2_classId);
{

    }
  }
  int a;
};


int S321child3_classId = Class_genId();
struct S321child3:S321{

  S321child3 (int a):a(a){
    (id = S321child3_classId);
{

    }
  }
  int a;
};


struct Trait321{

  int(*trait321)(Class*);
};

Vec* Trait321_v = newVec();

int Trait321_S321child1_trait321(Class* self_) {
  S321child1* self = ((S321child1*)self_);
{
    return (self -> a);
  }
} 
Trait321* newTrait321_S321child1() {
  Trait321 (* impl) = (new Trait321());
  setVec(Trait321_v, S321child1_classId, ((void*)impl));
  ((impl -> trait321) = (& Trait321_S321child1_trait321));
  return impl;
} 
Trait321* Trait321_S321child1_ = newTrait321_S321child1();

int Trait321_S321child2_trait321(Class* self_) {
  S321child2* self = ((S321child2*)self_);
{
    return (self -> a);
  }
} 
Trait321* newTrait321_S321child2() {
  Trait321 (* impl) = (new Trait321());
  setVec(Trait321_v, S321child2_classId, ((void*)impl));
  ((impl -> trait321) = (& Trait321_S321child2_trait321));
  return impl;
} 
Trait321* Trait321_S321child2_ = newTrait321_S321child2();

int Trait156_S321child1_trait156(Class* self_) {
  S321child1* self = ((S321child1*)self_);
{
    return (self -> a);
  }
} 
Trait156* newTrait156_S321child1() {
  Trait156 (* impl) = (new Trait156());
  setVec(Trait156_v, S321child1_classId, ((void*)impl));
  ((impl -> trait156) = (& Trait156_S321child1_trait156));
  return impl;
} 
Trait156* Trait156_S321child1_ = newTrait156_S321child1();

int S322_classId = Class_genId();
struct S322{

  int id;
  S322 ():id(S322_classId){

  }
};


int S322child1_classId = Class_genId();
struct S322child1:S322{

  S322child1 (int a):a(a){
    (id = S322child1_classId);
{

    }
  }
  int a;
};


int S322child2_classId = Class_genId();
struct S322child2:S322{

  S322child2 (int a):a(a){
    (id = S322child2_classId);
{

    }
  }
  int a;
};


int S322child3_classId = Class_genId();
struct S322child3:S322{

  S322child3 (int a):a(a){
    (id = S322child3_classId);
{

    }
  }
  int a;
};


struct Trait322{

  int(*trait322)(Class*);
};

Vec* Trait322_v = newVec();

int Trait322_S322child1_trait322(Class* self_) {
  S322child1* self = ((S322child1*)self_);
{
    return (self -> a);
  }
} 
Trait322* newTrait322_S322child1() {
  Trait322 (* impl) = (new Trait322());
  setVec(Trait322_v, S322child1_classId, ((void*)impl));
  ((impl -> trait322) = (& Trait322_S322child1_trait322));
  return impl;
} 
Trait322* Trait322_S322child1_ = newTrait322_S322child1();

int Trait248_S322child1_trait248(Class* self_) {
  S322child1* self = ((S322child1*)self_);
{
    return (self -> a);
  }
} 
Trait248* newTrait248_S322child1() {
  Trait248 (* impl) = (new Trait248());
  setVec(Trait248_v, S322child1_classId, ((void*)impl));
  ((impl -> trait248) = (& Trait248_S322child1_trait248));
  return impl;
} 
Trait248* Trait248_S322child1_ = newTrait248_S322child1();

int S323_classId = Class_genId();
struct S323{

  int id;
  S323 ():id(S323_classId){

  }
};


int S323child1_classId = Class_genId();
struct S323child1:S323{

  S323child1 (int a):a(a){
    (id = S323child1_classId);
{

    }
  }
  int a;
};


int S323child2_classId = Class_genId();
struct S323child2:S323{

  S323child2 (int a):a(a){
    (id = S323child2_classId);
{

    }
  }
  int a;
};


struct Trait323{

  int(*trait323)(Class*);
};

Vec* Trait323_v = newVec();

int Trait323_S323child1_trait323(Class* self_) {
  S323child1* self = ((S323child1*)self_);
{
    return (self -> a);
  }
} 
Trait323* newTrait323_S323child1() {
  Trait323 (* impl) = (new Trait323());
  setVec(Trait323_v, S323child1_classId, ((void*)impl));
  ((impl -> trait323) = (& Trait323_S323child1_trait323));
  return impl;
} 
Trait323* Trait323_S323child1_ = newTrait323_S323child1();

int Trait323_S323child2_trait323(Class* self_) {
  S323child2* self = ((S323child2*)self_);
{
    return (self -> a);
  }
} 
Trait323* newTrait323_S323child2() {
  Trait323 (* impl) = (new Trait323());
  setVec(Trait323_v, S323child2_classId, ((void*)impl));
  ((impl -> trait323) = (& Trait323_S323child2_trait323));
  return impl;
} 
Trait323* Trait323_S323child2_ = newTrait323_S323child2();

int Trait149_S323child1_trait149(Class* self_) {
  S323child1* self = ((S323child1*)self_);
{
    return (self -> a);
  }
} 
Trait149* newTrait149_S323child1() {
  Trait149 (* impl) = (new Trait149());
  setVec(Trait149_v, S323child1_classId, ((void*)impl));
  ((impl -> trait149) = (& Trait149_S323child1_trait149));
  return impl;
} 
Trait149* Trait149_S323child1_ = newTrait149_S323child1();

int S324_classId = Class_genId();
struct S324{

  int id;
  S324 ():id(S324_classId){

  }
};


int S324child1_classId = Class_genId();
struct S324child1:S324{

  S324child1 (int a):a(a){
    (id = S324child1_classId);
{

    }
  }
  int a;
};


struct Trait324{

  int(*trait324)(Class*);
};

Vec* Trait324_v = newVec();

int Trait324_S324child1_trait324(Class* self_) {
  S324child1* self = ((S324child1*)self_);
{
    return (self -> a);
  }
} 
Trait324* newTrait324_S324child1() {
  Trait324 (* impl) = (new Trait324());
  setVec(Trait324_v, S324child1_classId, ((void*)impl));
  ((impl -> trait324) = (& Trait324_S324child1_trait324));
  return impl;
} 
Trait324* Trait324_S324child1_ = newTrait324_S324child1();

int Trait242_S324child1_trait242(Class* self_) {
  S324child1* self = ((S324child1*)self_);
{
    return (self -> a);
  }
} 
Trait242* newTrait242_S324child1() {
  Trait242 (* impl) = (new Trait242());
  setVec(Trait242_v, S324child1_classId, ((void*)impl));
  ((impl -> trait242) = (& Trait242_S324child1_trait242));
  return impl;
} 
Trait242* Trait242_S324child1_ = newTrait242_S324child1();

int S325_classId = Class_genId();
struct S325{

  int id;
  S325 ():id(S325_classId){

  }
};


int S325child1_classId = Class_genId();
struct S325child1:S325{

  S325child1 (int a):a(a){
    (id = S325child1_classId);
{

    }
  }
  int a;
};


int S325child2_classId = Class_genId();
struct S325child2:S325{

  S325child2 (int a):a(a){
    (id = S325child2_classId);
{

    }
  }
  int a;
};


int S325child3_classId = Class_genId();
struct S325child3:S325{

  S325child3 (int a):a(a){
    (id = S325child3_classId);
{

    }
  }
  int a;
};


int S325child4_classId = Class_genId();
struct S325child4:S325{

  S325child4 (int a):a(a){
    (id = S325child4_classId);
{

    }
  }
  int a;
};


struct Trait325{

  int(*trait325)(Class*);
};

Vec* Trait325_v = newVec();

int Trait325_S325child1_trait325(Class* self_) {
  S325child1* self = ((S325child1*)self_);
{
    return (self -> a);
  }
} 
Trait325* newTrait325_S325child1() {
  Trait325 (* impl) = (new Trait325());
  setVec(Trait325_v, S325child1_classId, ((void*)impl));
  ((impl -> trait325) = (& Trait325_S325child1_trait325));
  return impl;
} 
Trait325* Trait325_S325child1_ = newTrait325_S325child1();

int Trait174_S325child1_trait174(Class* self_) {
  S325child1* self = ((S325child1*)self_);
{
    return (self -> a);
  }
} 
Trait174* newTrait174_S325child1() {
  Trait174 (* impl) = (new Trait174());
  setVec(Trait174_v, S325child1_classId, ((void*)impl));
  ((impl -> trait174) = (& Trait174_S325child1_trait174));
  return impl;
} 
Trait174* Trait174_S325child1_ = newTrait174_S325child1();

int S326_classId = Class_genId();
struct S326{

  int id;
  S326 ():id(S326_classId){

  }
};


int S326child1_classId = Class_genId();
struct S326child1:S326{

  S326child1 (int a):a(a){
    (id = S326child1_classId);
{

    }
  }
  int a;
};


int S326child2_classId = Class_genId();
struct S326child2:S326{

  S326child2 (int a):a(a){
    (id = S326child2_classId);
{

    }
  }
  int a;
};


int S326child3_classId = Class_genId();
struct S326child3:S326{

  S326child3 (int a):a(a){
    (id = S326child3_classId);
{

    }
  }
  int a;
};


int S326child4_classId = Class_genId();
struct S326child4:S326{

  S326child4 (int a):a(a){
    (id = S326child4_classId);
{

    }
  }
  int a;
};


struct Trait326{

  int(*trait326)(Class*);
};

Vec* Trait326_v = newVec();

int Trait326_S326child1_trait326(Class* self_) {
  S326child1* self = ((S326child1*)self_);
{
    return (self -> a);
  }
} 
Trait326* newTrait326_S326child1() {
  Trait326 (* impl) = (new Trait326());
  setVec(Trait326_v, S326child1_classId, ((void*)impl));
  ((impl -> trait326) = (& Trait326_S326child1_trait326));
  return impl;
} 
Trait326* Trait326_S326child1_ = newTrait326_S326child1();

int Trait315_S326child1_trait315(Class* self_) {
  S326child1* self = ((S326child1*)self_);
{
    return (self -> a);
  }
} 
Trait315* newTrait315_S326child1() {
  Trait315 (* impl) = (new Trait315());
  setVec(Trait315_v, S326child1_classId, ((void*)impl));
  ((impl -> trait315) = (& Trait315_S326child1_trait315));
  return impl;
} 
Trait315* Trait315_S326child1_ = newTrait315_S326child1();

int S327_classId = Class_genId();
struct S327{

  int id;
  S327 ():id(S327_classId){

  }
};


int S327child1_classId = Class_genId();
struct S327child1:S327{

  S327child1 (int a):a(a){
    (id = S327child1_classId);
{

    }
  }
  int a;
};


int S327child2_classId = Class_genId();
struct S327child2:S327{

  S327child2 (int a):a(a){
    (id = S327child2_classId);
{

    }
  }
  int a;
};


int S327child3_classId = Class_genId();
struct S327child3:S327{

  S327child3 (int a):a(a){
    (id = S327child3_classId);
{

    }
  }
  int a;
};


int S327child4_classId = Class_genId();
struct S327child4:S327{

  S327child4 (int a):a(a){
    (id = S327child4_classId);
{

    }
  }
  int a;
};


struct Trait327{

  int(*trait327)(Class*);
};

Vec* Trait327_v = newVec();

int Trait327_S327child1_trait327(Class* self_) {
  S327child1* self = ((S327child1*)self_);
{
    return (self -> a);
  }
} 
Trait327* newTrait327_S327child1() {
  Trait327 (* impl) = (new Trait327());
  setVec(Trait327_v, S327child1_classId, ((void*)impl));
  ((impl -> trait327) = (& Trait327_S327child1_trait327));
  return impl;
} 
Trait327* Trait327_S327child1_ = newTrait327_S327child1();

int Trait173_S327child1_trait173(Class* self_) {
  S327child1* self = ((S327child1*)self_);
{
    return (self -> a);
  }
} 
Trait173* newTrait173_S327child1() {
  Trait173 (* impl) = (new Trait173());
  setVec(Trait173_v, S327child1_classId, ((void*)impl));
  ((impl -> trait173) = (& Trait173_S327child1_trait173));
  return impl;
} 
Trait173* Trait173_S327child1_ = newTrait173_S327child1();

int S328_classId = Class_genId();
struct S328{

  int id;
  S328 ():id(S328_classId){

  }
};


int S328child1_classId = Class_genId();
struct S328child1:S328{

  S328child1 (int a):a(a){
    (id = S328child1_classId);
{

    }
  }
  int a;
};


int S328child2_classId = Class_genId();
struct S328child2:S328{

  S328child2 (int a):a(a){
    (id = S328child2_classId);
{

    }
  }
  int a;
};


int S328child3_classId = Class_genId();
struct S328child3:S328{

  S328child3 (int a):a(a){
    (id = S328child3_classId);
{

    }
  }
  int a;
};


struct Trait328{

  int(*trait328)(Class*);
};

Vec* Trait328_v = newVec();

int Trait328_S328child1_trait328(Class* self_) {
  S328child1* self = ((S328child1*)self_);
{
    return (self -> a);
  }
} 
Trait328* newTrait328_S328child1() {
  Trait328 (* impl) = (new Trait328());
  setVec(Trait328_v, S328child1_classId, ((void*)impl));
  ((impl -> trait328) = (& Trait328_S328child1_trait328));
  return impl;
} 
Trait328* Trait328_S328child1_ = newTrait328_S328child1();

int Trait328_S328child2_trait328(Class* self_) {
  S328child2* self = ((S328child2*)self_);
{
    return (self -> a);
  }
} 
Trait328* newTrait328_S328child2() {
  Trait328 (* impl) = (new Trait328());
  setVec(Trait328_v, S328child2_classId, ((void*)impl));
  ((impl -> trait328) = (& Trait328_S328child2_trait328));
  return impl;
} 
Trait328* Trait328_S328child2_ = newTrait328_S328child2();

int Trait328_S328child3_trait328(Class* self_) {
  S328child3* self = ((S328child3*)self_);
{
    return (self -> a);
  }
} 
Trait328* newTrait328_S328child3() {
  Trait328 (* impl) = (new Trait328());
  setVec(Trait328_v, S328child3_classId, ((void*)impl));
  ((impl -> trait328) = (& Trait328_S328child3_trait328));
  return impl;
} 
Trait328* Trait328_S328child3_ = newTrait328_S328child3();

int Trait316_S328child1_trait316(Class* self_) {
  S328child1* self = ((S328child1*)self_);
{
    return (self -> a);
  }
} 
Trait316* newTrait316_S328child1() {
  Trait316 (* impl) = (new Trait316());
  setVec(Trait316_v, S328child1_classId, ((void*)impl));
  ((impl -> trait316) = (& Trait316_S328child1_trait316));
  return impl;
} 
Trait316* Trait316_S328child1_ = newTrait316_S328child1();

int S329_classId = Class_genId();
struct S329{

  int id;
  S329 ():id(S329_classId){

  }
};


int S329child1_classId = Class_genId();
struct S329child1:S329{

  S329child1 (int a):a(a){
    (id = S329child1_classId);
{

    }
  }
  int a;
};


int S329child2_classId = Class_genId();
struct S329child2:S329{

  S329child2 (int a):a(a){
    (id = S329child2_classId);
{

    }
  }
  int a;
};


struct Trait329{

  int(*trait329)(Class*);
};

Vec* Trait329_v = newVec();

int Trait329_S329child1_trait329(Class* self_) {
  S329child1* self = ((S329child1*)self_);
{
    return (self -> a);
  }
} 
Trait329* newTrait329_S329child1() {
  Trait329 (* impl) = (new Trait329());
  setVec(Trait329_v, S329child1_classId, ((void*)impl));
  ((impl -> trait329) = (& Trait329_S329child1_trait329));
  return impl;
} 
Trait329* Trait329_S329child1_ = newTrait329_S329child1();

int Trait260_S329child1_trait260(Class* self_) {
  S329child1* self = ((S329child1*)self_);
{
    return (self -> a);
  }
} 
Trait260* newTrait260_S329child1() {
  Trait260 (* impl) = (new Trait260());
  setVec(Trait260_v, S329child1_classId, ((void*)impl));
  ((impl -> trait260) = (& Trait260_S329child1_trait260));
  return impl;
} 
Trait260* Trait260_S329child1_ = newTrait260_S329child1();

int S330_classId = Class_genId();
struct S330{

  int id;
  S330 ():id(S330_classId){

  }
};


int S330child1_classId = Class_genId();
struct S330child1:S330{

  S330child1 (int a):a(a){
    (id = S330child1_classId);
{

    }
  }
  int a;
};


int S330child2_classId = Class_genId();
struct S330child2:S330{

  S330child2 (int a):a(a){
    (id = S330child2_classId);
{

    }
  }
  int a;
};


int S330child3_classId = Class_genId();
struct S330child3:S330{

  S330child3 (int a):a(a){
    (id = S330child3_classId);
{

    }
  }
  int a;
};


int S330child4_classId = Class_genId();
struct S330child4:S330{

  S330child4 (int a):a(a){
    (id = S330child4_classId);
{

    }
  }
  int a;
};


struct Trait330{

  int(*trait330)(Class*);
};

Vec* Trait330_v = newVec();

int Trait330_S330child1_trait330(Class* self_) {
  S330child1* self = ((S330child1*)self_);
{
    return (self -> a);
  }
} 
Trait330* newTrait330_S330child1() {
  Trait330 (* impl) = (new Trait330());
  setVec(Trait330_v, S330child1_classId, ((void*)impl));
  ((impl -> trait330) = (& Trait330_S330child1_trait330));
  return impl;
} 
Trait330* Trait330_S330child1_ = newTrait330_S330child1();

int Trait330_S330child2_trait330(Class* self_) {
  S330child2* self = ((S330child2*)self_);
{
    return (self -> a);
  }
} 
Trait330* newTrait330_S330child2() {
  Trait330 (* impl) = (new Trait330());
  setVec(Trait330_v, S330child2_classId, ((void*)impl));
  ((impl -> trait330) = (& Trait330_S330child2_trait330));
  return impl;
} 
Trait330* Trait330_S330child2_ = newTrait330_S330child2();

int Trait330_S330child3_trait330(Class* self_) {
  S330child3* self = ((S330child3*)self_);
{
    return (self -> a);
  }
} 
Trait330* newTrait330_S330child3() {
  Trait330 (* impl) = (new Trait330());
  setVec(Trait330_v, S330child3_classId, ((void*)impl));
  ((impl -> trait330) = (& Trait330_S330child3_trait330));
  return impl;
} 
Trait330* Trait330_S330child3_ = newTrait330_S330child3();

int Trait330_S330child4_trait330(Class* self_) {
  S330child4* self = ((S330child4*)self_);
{
    return (self -> a);
  }
} 
Trait330* newTrait330_S330child4() {
  Trait330 (* impl) = (new Trait330());
  setVec(Trait330_v, S330child4_classId, ((void*)impl));
  ((impl -> trait330) = (& Trait330_S330child4_trait330));
  return impl;
} 
Trait330* Trait330_S330child4_ = newTrait330_S330child4();

int Trait127_S330child1_trait127(Class* self_) {
  S330child1* self = ((S330child1*)self_);
{
    return (self -> a);
  }
} 
Trait127* newTrait127_S330child1() {
  Trait127 (* impl) = (new Trait127());
  setVec(Trait127_v, S330child1_classId, ((void*)impl));
  ((impl -> trait127) = (& Trait127_S330child1_trait127));
  return impl;
} 
Trait127* Trait127_S330child1_ = newTrait127_S330child1();

int S331_classId = Class_genId();
struct S331{

  int id;
  S331 ():id(S331_classId){

  }
};


int S331child1_classId = Class_genId();
struct S331child1:S331{

  S331child1 (int a):a(a){
    (id = S331child1_classId);
{

    }
  }
  int a;
};


struct Trait331{

  int(*trait331)(Class*);
};

Vec* Trait331_v = newVec();

int Trait331_S331child1_trait331(Class* self_) {
  S331child1* self = ((S331child1*)self_);
{
    return (self -> a);
  }
} 
Trait331* newTrait331_S331child1() {
  Trait331 (* impl) = (new Trait331());
  setVec(Trait331_v, S331child1_classId, ((void*)impl));
  ((impl -> trait331) = (& Trait331_S331child1_trait331));
  return impl;
} 
Trait331* Trait331_S331child1_ = newTrait331_S331child1();

int Trait216_S331child1_trait216(Class* self_) {
  S331child1* self = ((S331child1*)self_);
{
    return (self -> a);
  }
} 
Trait216* newTrait216_S331child1() {
  Trait216 (* impl) = (new Trait216());
  setVec(Trait216_v, S331child1_classId, ((void*)impl));
  ((impl -> trait216) = (& Trait216_S331child1_trait216));
  return impl;
} 
Trait216* Trait216_S331child1_ = newTrait216_S331child1();

int S332_classId = Class_genId();
struct S332{

  int id;
  S332 ():id(S332_classId){

  }
};


int S332child1_classId = Class_genId();
struct S332child1:S332{

  S332child1 (int a):a(a){
    (id = S332child1_classId);
{

    }
  }
  int a;
};


int S332child2_classId = Class_genId();
struct S332child2:S332{

  S332child2 (int a):a(a){
    (id = S332child2_classId);
{

    }
  }
  int a;
};


int S332child3_classId = Class_genId();
struct S332child3:S332{

  S332child3 (int a):a(a){
    (id = S332child3_classId);
{

    }
  }
  int a;
};


int S332child4_classId = Class_genId();
struct S332child4:S332{

  S332child4 (int a):a(a){
    (id = S332child4_classId);
{

    }
  }
  int a;
};


int S332child5_classId = Class_genId();
struct S332child5:S332{

  S332child5 (int a):a(a){
    (id = S332child5_classId);
{

    }
  }
  int a;
};


struct Trait332{

  int(*trait332)(Class*);
};

Vec* Trait332_v = newVec();

int Trait332_S332child1_trait332(Class* self_) {
  S332child1* self = ((S332child1*)self_);
{
    return (self -> a);
  }
} 
Trait332* newTrait332_S332child1() {
  Trait332 (* impl) = (new Trait332());
  setVec(Trait332_v, S332child1_classId, ((void*)impl));
  ((impl -> trait332) = (& Trait332_S332child1_trait332));
  return impl;
} 
Trait332* Trait332_S332child1_ = newTrait332_S332child1();

int Trait272_S332child1_trait272(Class* self_) {
  S332child1* self = ((S332child1*)self_);
{
    return (self -> a);
  }
} 
Trait272* newTrait272_S332child1() {
  Trait272 (* impl) = (new Trait272());
  setVec(Trait272_v, S332child1_classId, ((void*)impl));
  ((impl -> trait272) = (& Trait272_S332child1_trait272));
  return impl;
} 
Trait272* Trait272_S332child1_ = newTrait272_S332child1();

int S333_classId = Class_genId();
struct S333{

  int id;
  S333 ():id(S333_classId){

  }
};


int S333child1_classId = Class_genId();
struct S333child1:S333{

  S333child1 (int a):a(a){
    (id = S333child1_classId);
{

    }
  }
  int a;
};


struct Trait333{

  int(*trait333)(Class*);
};

Vec* Trait333_v = newVec();

int Trait333_S333child1_trait333(Class* self_) {
  S333child1* self = ((S333child1*)self_);
{
    return (self -> a);
  }
} 
Trait333* newTrait333_S333child1() {
  Trait333 (* impl) = (new Trait333());
  setVec(Trait333_v, S333child1_classId, ((void*)impl));
  ((impl -> trait333) = (& Trait333_S333child1_trait333));
  return impl;
} 
Trait333* Trait333_S333child1_ = newTrait333_S333child1();

int Trait48_S333child1_trait48(Class* self_) {
  S333child1* self = ((S333child1*)self_);
{
    return (self -> a);
  }
} 
Trait48* newTrait48_S333child1() {
  Trait48 (* impl) = (new Trait48());
  setVec(Trait48_v, S333child1_classId, ((void*)impl));
  ((impl -> trait48) = (& Trait48_S333child1_trait48));
  return impl;
} 
Trait48* Trait48_S333child1_ = newTrait48_S333child1();

int S334_classId = Class_genId();
struct S334{

  int id;
  S334 ():id(S334_classId){

  }
};


int S334child1_classId = Class_genId();
struct S334child1:S334{

  S334child1 (int a):a(a){
    (id = S334child1_classId);
{

    }
  }
  int a;
};


struct Trait334{

  int(*trait334)(Class*);
};

Vec* Trait334_v = newVec();

int Trait334_S334child1_trait334(Class* self_) {
  S334child1* self = ((S334child1*)self_);
{
    return (self -> a);
  }
} 
Trait334* newTrait334_S334child1() {
  Trait334 (* impl) = (new Trait334());
  setVec(Trait334_v, S334child1_classId, ((void*)impl));
  ((impl -> trait334) = (& Trait334_S334child1_trait334));
  return impl;
} 
Trait334* Trait334_S334child1_ = newTrait334_S334child1();

int Trait249_S334child1_trait249(Class* self_) {
  S334child1* self = ((S334child1*)self_);
{
    return (self -> a);
  }
} 
Trait249* newTrait249_S334child1() {
  Trait249 (* impl) = (new Trait249());
  setVec(Trait249_v, S334child1_classId, ((void*)impl));
  ((impl -> trait249) = (& Trait249_S334child1_trait249));
  return impl;
} 
Trait249* Trait249_S334child1_ = newTrait249_S334child1();

int S335_classId = Class_genId();
struct S335{

  int id;
  S335 ():id(S335_classId){

  }
};


int S335child1_classId = Class_genId();
struct S335child1:S335{

  S335child1 (int a):a(a){
    (id = S335child1_classId);
{

    }
  }
  int a;
};


struct Trait335{

  int(*trait335)(Class*);
};

Vec* Trait335_v = newVec();

int Trait335_S335child1_trait335(Class* self_) {
  S335child1* self = ((S335child1*)self_);
{
    return (self -> a);
  }
} 
Trait335* newTrait335_S335child1() {
  Trait335 (* impl) = (new Trait335());
  setVec(Trait335_v, S335child1_classId, ((void*)impl));
  ((impl -> trait335) = (& Trait335_S335child1_trait335));
  return impl;
} 
Trait335* Trait335_S335child1_ = newTrait335_S335child1();

int Trait24_S335child1_trait24(Class* self_) {
  S335child1* self = ((S335child1*)self_);
{
    return (self -> a);
  }
} 
Trait24* newTrait24_S335child1() {
  Trait24 (* impl) = (new Trait24());
  setVec(Trait24_v, S335child1_classId, ((void*)impl));
  ((impl -> trait24) = (& Trait24_S335child1_trait24));
  return impl;
} 
Trait24* Trait24_S335child1_ = newTrait24_S335child1();

int S336_classId = Class_genId();
struct S336{

  int id;
  S336 ():id(S336_classId){

  }
};


int S336child1_classId = Class_genId();
struct S336child1:S336{

  S336child1 (int a):a(a){
    (id = S336child1_classId);
{

    }
  }
  int a;
};


int S336child2_classId = Class_genId();
struct S336child2:S336{

  S336child2 (int a):a(a){
    (id = S336child2_classId);
{

    }
  }
  int a;
};


int S336child3_classId = Class_genId();
struct S336child3:S336{

  S336child3 (int a):a(a){
    (id = S336child3_classId);
{

    }
  }
  int a;
};


struct Trait336{

  int(*trait336)(Class*);
};

Vec* Trait336_v = newVec();

int Trait336_S336child1_trait336(Class* self_) {
  S336child1* self = ((S336child1*)self_);
{
    return (self -> a);
  }
} 
Trait336* newTrait336_S336child1() {
  Trait336 (* impl) = (new Trait336());
  setVec(Trait336_v, S336child1_classId, ((void*)impl));
  ((impl -> trait336) = (& Trait336_S336child1_trait336));
  return impl;
} 
Trait336* Trait336_S336child1_ = newTrait336_S336child1();

int Trait336_S336child2_trait336(Class* self_) {
  S336child2* self = ((S336child2*)self_);
{
    return (self -> a);
  }
} 
Trait336* newTrait336_S336child2() {
  Trait336 (* impl) = (new Trait336());
  setVec(Trait336_v, S336child2_classId, ((void*)impl));
  ((impl -> trait336) = (& Trait336_S336child2_trait336));
  return impl;
} 
Trait336* Trait336_S336child2_ = newTrait336_S336child2();

int Trait336_S336child3_trait336(Class* self_) {
  S336child3* self = ((S336child3*)self_);
{
    return (self -> a);
  }
} 
Trait336* newTrait336_S336child3() {
  Trait336 (* impl) = (new Trait336());
  setVec(Trait336_v, S336child3_classId, ((void*)impl));
  ((impl -> trait336) = (& Trait336_S336child3_trait336));
  return impl;
} 
Trait336* Trait336_S336child3_ = newTrait336_S336child3();

int Trait196_S336child1_trait196(Class* self_) {
  S336child1* self = ((S336child1*)self_);
{
    return (self -> a);
  }
} 
Trait196* newTrait196_S336child1() {
  Trait196 (* impl) = (new Trait196());
  setVec(Trait196_v, S336child1_classId, ((void*)impl));
  ((impl -> trait196) = (& Trait196_S336child1_trait196));
  return impl;
} 
Trait196* Trait196_S336child1_ = newTrait196_S336child1();

int S337_classId = Class_genId();
struct S337{

  int id;
  S337 ():id(S337_classId){

  }
};


int S337child1_classId = Class_genId();
struct S337child1:S337{

  S337child1 (int a):a(a){
    (id = S337child1_classId);
{

    }
  }
  int a;
};


int S337child2_classId = Class_genId();
struct S337child2:S337{

  S337child2 (int a):a(a){
    (id = S337child2_classId);
{

    }
  }
  int a;
};


int S337child3_classId = Class_genId();
struct S337child3:S337{

  S337child3 (int a):a(a){
    (id = S337child3_classId);
{

    }
  }
  int a;
};


struct Trait337{

  int(*trait337)(Class*);
};

Vec* Trait337_v = newVec();

int Trait337_S337child1_trait337(Class* self_) {
  S337child1* self = ((S337child1*)self_);
{
    return (self -> a);
  }
} 
Trait337* newTrait337_S337child1() {
  Trait337 (* impl) = (new Trait337());
  setVec(Trait337_v, S337child1_classId, ((void*)impl));
  ((impl -> trait337) = (& Trait337_S337child1_trait337));
  return impl;
} 
Trait337* Trait337_S337child1_ = newTrait337_S337child1();

int Trait337_S337child2_trait337(Class* self_) {
  S337child2* self = ((S337child2*)self_);
{
    return (self -> a);
  }
} 
Trait337* newTrait337_S337child2() {
  Trait337 (* impl) = (new Trait337());
  setVec(Trait337_v, S337child2_classId, ((void*)impl));
  ((impl -> trait337) = (& Trait337_S337child2_trait337));
  return impl;
} 
Trait337* Trait337_S337child2_ = newTrait337_S337child2();

int Trait337_S337child3_trait337(Class* self_) {
  S337child3* self = ((S337child3*)self_);
{
    return (self -> a);
  }
} 
Trait337* newTrait337_S337child3() {
  Trait337 (* impl) = (new Trait337());
  setVec(Trait337_v, S337child3_classId, ((void*)impl));
  ((impl -> trait337) = (& Trait337_S337child3_trait337));
  return impl;
} 
Trait337* Trait337_S337child3_ = newTrait337_S337child3();

int Trait255_S337child1_trait255(Class* self_) {
  S337child1* self = ((S337child1*)self_);
{
    return (self -> a);
  }
} 
Trait255* newTrait255_S337child1() {
  Trait255 (* impl) = (new Trait255());
  setVec(Trait255_v, S337child1_classId, ((void*)impl));
  ((impl -> trait255) = (& Trait255_S337child1_trait255));
  return impl;
} 
Trait255* Trait255_S337child1_ = newTrait255_S337child1();

int S338_classId = Class_genId();
struct S338{

  int id;
  S338 ():id(S338_classId){

  }
};


int S338child1_classId = Class_genId();
struct S338child1:S338{

  S338child1 (int a):a(a){
    (id = S338child1_classId);
{

    }
  }
  int a;
};


int S338child2_classId = Class_genId();
struct S338child2:S338{

  S338child2 (int a):a(a){
    (id = S338child2_classId);
{

    }
  }
  int a;
};


int S338child3_classId = Class_genId();
struct S338child3:S338{

  S338child3 (int a):a(a){
    (id = S338child3_classId);
{

    }
  }
  int a;
};


int S338child4_classId = Class_genId();
struct S338child4:S338{

  S338child4 (int a):a(a){
    (id = S338child4_classId);
{

    }
  }
  int a;
};


struct Trait338{

  int(*trait338)(Class*);
};

Vec* Trait338_v = newVec();

int Trait338_S338child1_trait338(Class* self_) {
  S338child1* self = ((S338child1*)self_);
{
    return (self -> a);
  }
} 
Trait338* newTrait338_S338child1() {
  Trait338 (* impl) = (new Trait338());
  setVec(Trait338_v, S338child1_classId, ((void*)impl));
  ((impl -> trait338) = (& Trait338_S338child1_trait338));
  return impl;
} 
Trait338* Trait338_S338child1_ = newTrait338_S338child1();

int Trait338_S338child2_trait338(Class* self_) {
  S338child2* self = ((S338child2*)self_);
{
    return (self -> a);
  }
} 
Trait338* newTrait338_S338child2() {
  Trait338 (* impl) = (new Trait338());
  setVec(Trait338_v, S338child2_classId, ((void*)impl));
  ((impl -> trait338) = (& Trait338_S338child2_trait338));
  return impl;
} 
Trait338* Trait338_S338child2_ = newTrait338_S338child2();

int Trait272_S338child1_trait272(Class* self_) {
  S338child1* self = ((S338child1*)self_);
{
    return (self -> a);
  }
} 
Trait272* newTrait272_S338child1() {
  Trait272 (* impl) = (new Trait272());
  setVec(Trait272_v, S338child1_classId, ((void*)impl));
  ((impl -> trait272) = (& Trait272_S338child1_trait272));
  return impl;
} 
Trait272* Trait272_S338child1_ = newTrait272_S338child1();

int S339_classId = Class_genId();
struct S339{

  int id;
  S339 ():id(S339_classId){

  }
};


int S339child1_classId = Class_genId();
struct S339child1:S339{

  S339child1 (int a):a(a){
    (id = S339child1_classId);
{

    }
  }
  int a;
};


int S339child2_classId = Class_genId();
struct S339child2:S339{

  S339child2 (int a):a(a){
    (id = S339child2_classId);
{

    }
  }
  int a;
};


struct Trait339{

  int(*trait339)(Class*);
};

Vec* Trait339_v = newVec();

int Trait339_S339child1_trait339(Class* self_) {
  S339child1* self = ((S339child1*)self_);
{
    return (self -> a);
  }
} 
Trait339* newTrait339_S339child1() {
  Trait339 (* impl) = (new Trait339());
  setVec(Trait339_v, S339child1_classId, ((void*)impl));
  ((impl -> trait339) = (& Trait339_S339child1_trait339));
  return impl;
} 
Trait339* Trait339_S339child1_ = newTrait339_S339child1();

int Trait339_S339child2_trait339(Class* self_) {
  S339child2* self = ((S339child2*)self_);
{
    return (self -> a);
  }
} 
Trait339* newTrait339_S339child2() {
  Trait339 (* impl) = (new Trait339());
  setVec(Trait339_v, S339child2_classId, ((void*)impl));
  ((impl -> trait339) = (& Trait339_S339child2_trait339));
  return impl;
} 
Trait339* Trait339_S339child2_ = newTrait339_S339child2();

int Trait114_S339child1_trait114(Class* self_) {
  S339child1* self = ((S339child1*)self_);
{
    return (self -> a);
  }
} 
Trait114* newTrait114_S339child1() {
  Trait114 (* impl) = (new Trait114());
  setVec(Trait114_v, S339child1_classId, ((void*)impl));
  ((impl -> trait114) = (& Trait114_S339child1_trait114));
  return impl;
} 
Trait114* Trait114_S339child1_ = newTrait114_S339child1();

int S340_classId = Class_genId();
struct S340{

  int id;
  S340 ():id(S340_classId){

  }
};


int S340child1_classId = Class_genId();
struct S340child1:S340{

  S340child1 (int a):a(a){
    (id = S340child1_classId);
{

    }
  }
  int a;
};


int S340child2_classId = Class_genId();
struct S340child2:S340{

  S340child2 (int a):a(a){
    (id = S340child2_classId);
{

    }
  }
  int a;
};


int S340child3_classId = Class_genId();
struct S340child3:S340{

  S340child3 (int a):a(a){
    (id = S340child3_classId);
{

    }
  }
  int a;
};


int S340child4_classId = Class_genId();
struct S340child4:S340{

  S340child4 (int a):a(a){
    (id = S340child4_classId);
{

    }
  }
  int a;
};


struct Trait340{

  int(*trait340)(Class*);
};

Vec* Trait340_v = newVec();

int Trait340_S340child1_trait340(Class* self_) {
  S340child1* self = ((S340child1*)self_);
{
    return (self -> a);
  }
} 
Trait340* newTrait340_S340child1() {
  Trait340 (* impl) = (new Trait340());
  setVec(Trait340_v, S340child1_classId, ((void*)impl));
  ((impl -> trait340) = (& Trait340_S340child1_trait340));
  return impl;
} 
Trait340* Trait340_S340child1_ = newTrait340_S340child1();

int Trait174_S340child1_trait174(Class* self_) {
  S340child1* self = ((S340child1*)self_);
{
    return (self -> a);
  }
} 
Trait174* newTrait174_S340child1() {
  Trait174 (* impl) = (new Trait174());
  setVec(Trait174_v, S340child1_classId, ((void*)impl));
  ((impl -> trait174) = (& Trait174_S340child1_trait174));
  return impl;
} 
Trait174* Trait174_S340child1_ = newTrait174_S340child1();

int S341_classId = Class_genId();
struct S341{

  int id;
  S341 ():id(S341_classId){

  }
};


int S341child1_classId = Class_genId();
struct S341child1:S341{

  S341child1 (int a):a(a){
    (id = S341child1_classId);
{

    }
  }
  int a;
};


int S341child2_classId = Class_genId();
struct S341child2:S341{

  S341child2 (int a):a(a){
    (id = S341child2_classId);
{

    }
  }
  int a;
};


int S341child3_classId = Class_genId();
struct S341child3:S341{

  S341child3 (int a):a(a){
    (id = S341child3_classId);
{

    }
  }
  int a;
};


struct Trait341{

  int(*trait341)(Class*);
};

Vec* Trait341_v = newVec();

int Trait341_S341child1_trait341(Class* self_) {
  S341child1* self = ((S341child1*)self_);
{
    return (self -> a);
  }
} 
Trait341* newTrait341_S341child1() {
  Trait341 (* impl) = (new Trait341());
  setVec(Trait341_v, S341child1_classId, ((void*)impl));
  ((impl -> trait341) = (& Trait341_S341child1_trait341));
  return impl;
} 
Trait341* Trait341_S341child1_ = newTrait341_S341child1();

int Trait341_S341child2_trait341(Class* self_) {
  S341child2* self = ((S341child2*)self_);
{
    return (self -> a);
  }
} 
Trait341* newTrait341_S341child2() {
  Trait341 (* impl) = (new Trait341());
  setVec(Trait341_v, S341child2_classId, ((void*)impl));
  ((impl -> trait341) = (& Trait341_S341child2_trait341));
  return impl;
} 
Trait341* Trait341_S341child2_ = newTrait341_S341child2();

int Trait63_S341child1_trait63(Class* self_) {
  S341child1* self = ((S341child1*)self_);
{
    return (self -> a);
  }
} 
Trait63* newTrait63_S341child1() {
  Trait63 (* impl) = (new Trait63());
  setVec(Trait63_v, S341child1_classId, ((void*)impl));
  ((impl -> trait63) = (& Trait63_S341child1_trait63));
  return impl;
} 
Trait63* Trait63_S341child1_ = newTrait63_S341child1();

int S342_classId = Class_genId();
struct S342{

  int id;
  S342 ():id(S342_classId){

  }
};


int S342child1_classId = Class_genId();
struct S342child1:S342{

  S342child1 (int a):a(a){
    (id = S342child1_classId);
{

    }
  }
  int a;
};


struct Trait342{

  int(*trait342)(Class*);
};

Vec* Trait342_v = newVec();

int Trait342_S342child1_trait342(Class* self_) {
  S342child1* self = ((S342child1*)self_);
{
    return (self -> a);
  }
} 
Trait342* newTrait342_S342child1() {
  Trait342 (* impl) = (new Trait342());
  setVec(Trait342_v, S342child1_classId, ((void*)impl));
  ((impl -> trait342) = (& Trait342_S342child1_trait342));
  return impl;
} 
Trait342* Trait342_S342child1_ = newTrait342_S342child1();

int Trait39_S342child1_trait39(Class* self_) {
  S342child1* self = ((S342child1*)self_);
{
    return (self -> a);
  }
} 
Trait39* newTrait39_S342child1() {
  Trait39 (* impl) = (new Trait39());
  setVec(Trait39_v, S342child1_classId, ((void*)impl));
  ((impl -> trait39) = (& Trait39_S342child1_trait39));
  return impl;
} 
Trait39* Trait39_S342child1_ = newTrait39_S342child1();

int S343_classId = Class_genId();
struct S343{

  int id;
  S343 ():id(S343_classId){

  }
};


int S343child1_classId = Class_genId();
struct S343child1:S343{

  S343child1 (int a):a(a){
    (id = S343child1_classId);
{

    }
  }
  int a;
};


int S343child2_classId = Class_genId();
struct S343child2:S343{

  S343child2 (int a):a(a){
    (id = S343child2_classId);
{

    }
  }
  int a;
};


int S343child3_classId = Class_genId();
struct S343child3:S343{

  S343child3 (int a):a(a){
    (id = S343child3_classId);
{

    }
  }
  int a;
};


struct Trait343{

  int(*trait343)(Class*);
};

Vec* Trait343_v = newVec();

int Trait343_S343child1_trait343(Class* self_) {
  S343child1* self = ((S343child1*)self_);
{
    return (self -> a);
  }
} 
Trait343* newTrait343_S343child1() {
  Trait343 (* impl) = (new Trait343());
  setVec(Trait343_v, S343child1_classId, ((void*)impl));
  ((impl -> trait343) = (& Trait343_S343child1_trait343));
  return impl;
} 
Trait343* Trait343_S343child1_ = newTrait343_S343child1();

int Trait274_S343child1_trait274(Class* self_) {
  S343child1* self = ((S343child1*)self_);
{
    return (self -> a);
  }
} 
Trait274* newTrait274_S343child1() {
  Trait274 (* impl) = (new Trait274());
  setVec(Trait274_v, S343child1_classId, ((void*)impl));
  ((impl -> trait274) = (& Trait274_S343child1_trait274));
  return impl;
} 
Trait274* Trait274_S343child1_ = newTrait274_S343child1();

int S344_classId = Class_genId();
struct S344{

  int id;
  S344 ():id(S344_classId){

  }
};


int S344child1_classId = Class_genId();
struct S344child1:S344{

  S344child1 (int a):a(a){
    (id = S344child1_classId);
{

    }
  }
  int a;
};


int S344child2_classId = Class_genId();
struct S344child2:S344{

  S344child2 (int a):a(a){
    (id = S344child2_classId);
{

    }
  }
  int a;
};


struct Trait344{

  int(*trait344)(Class*);
};

Vec* Trait344_v = newVec();

int Trait344_S344child1_trait344(Class* self_) {
  S344child1* self = ((S344child1*)self_);
{
    return (self -> a);
  }
} 
Trait344* newTrait344_S344child1() {
  Trait344 (* impl) = (new Trait344());
  setVec(Trait344_v, S344child1_classId, ((void*)impl));
  ((impl -> trait344) = (& Trait344_S344child1_trait344));
  return impl;
} 
Trait344* Trait344_S344child1_ = newTrait344_S344child1();

int Trait344_S344child2_trait344(Class* self_) {
  S344child2* self = ((S344child2*)self_);
{
    return (self -> a);
  }
} 
Trait344* newTrait344_S344child2() {
  Trait344 (* impl) = (new Trait344());
  setVec(Trait344_v, S344child2_classId, ((void*)impl));
  ((impl -> trait344) = (& Trait344_S344child2_trait344));
  return impl;
} 
Trait344* Trait344_S344child2_ = newTrait344_S344child2();

int Trait36_S344child1_trait36(Class* self_) {
  S344child1* self = ((S344child1*)self_);
{
    return (self -> a);
  }
} 
Trait36* newTrait36_S344child1() {
  Trait36 (* impl) = (new Trait36());
  setVec(Trait36_v, S344child1_classId, ((void*)impl));
  ((impl -> trait36) = (& Trait36_S344child1_trait36));
  return impl;
} 
Trait36* Trait36_S344child1_ = newTrait36_S344child1();

int S345_classId = Class_genId();
struct S345{

  int id;
  S345 ():id(S345_classId){

  }
};


int S345child1_classId = Class_genId();
struct S345child1:S345{

  S345child1 (int a):a(a){
    (id = S345child1_classId);
{

    }
  }
  int a;
};


struct Trait345{

  int(*trait345)(Class*);
};

Vec* Trait345_v = newVec();

int Trait345_S345child1_trait345(Class* self_) {
  S345child1* self = ((S345child1*)self_);
{
    return (self -> a);
  }
} 
Trait345* newTrait345_S345child1() {
  Trait345 (* impl) = (new Trait345());
  setVec(Trait345_v, S345child1_classId, ((void*)impl));
  ((impl -> trait345) = (& Trait345_S345child1_trait345));
  return impl;
} 
Trait345* Trait345_S345child1_ = newTrait345_S345child1();

int Trait295_S345child1_trait295(Class* self_) {
  S345child1* self = ((S345child1*)self_);
{
    return (self -> a);
  }
} 
Trait295* newTrait295_S345child1() {
  Trait295 (* impl) = (new Trait295());
  setVec(Trait295_v, S345child1_classId, ((void*)impl));
  ((impl -> trait295) = (& Trait295_S345child1_trait295));
  return impl;
} 
Trait295* Trait295_S345child1_ = newTrait295_S345child1();

int S346_classId = Class_genId();
struct S346{

  int id;
  S346 ():id(S346_classId){

  }
};


int S346child1_classId = Class_genId();
struct S346child1:S346{

  S346child1 (int a):a(a){
    (id = S346child1_classId);
{

    }
  }
  int a;
};


int S346child2_classId = Class_genId();
struct S346child2:S346{

  S346child2 (int a):a(a){
    (id = S346child2_classId);
{

    }
  }
  int a;
};


struct Trait346{

  int(*trait346)(Class*);
};

Vec* Trait346_v = newVec();

int Trait346_S346child1_trait346(Class* self_) {
  S346child1* self = ((S346child1*)self_);
{
    return (self -> a);
  }
} 
Trait346* newTrait346_S346child1() {
  Trait346 (* impl) = (new Trait346());
  setVec(Trait346_v, S346child1_classId, ((void*)impl));
  ((impl -> trait346) = (& Trait346_S346child1_trait346));
  return impl;
} 
Trait346* Trait346_S346child1_ = newTrait346_S346child1();

int Trait180_S346child1_trait180(Class* self_) {
  S346child1* self = ((S346child1*)self_);
{
    return (self -> a);
  }
} 
Trait180* newTrait180_S346child1() {
  Trait180 (* impl) = (new Trait180());
  setVec(Trait180_v, S346child1_classId, ((void*)impl));
  ((impl -> trait180) = (& Trait180_S346child1_trait180));
  return impl;
} 
Trait180* Trait180_S346child1_ = newTrait180_S346child1();

int S347_classId = Class_genId();
struct S347{

  int id;
  S347 ():id(S347_classId){

  }
};


int S347child1_classId = Class_genId();
struct S347child1:S347{

  S347child1 (int a):a(a){
    (id = S347child1_classId);
{

    }
  }
  int a;
};


int S347child2_classId = Class_genId();
struct S347child2:S347{

  S347child2 (int a):a(a){
    (id = S347child2_classId);
{

    }
  }
  int a;
};


int S347child3_classId = Class_genId();
struct S347child3:S347{

  S347child3 (int a):a(a){
    (id = S347child3_classId);
{

    }
  }
  int a;
};


int S347child4_classId = Class_genId();
struct S347child4:S347{

  S347child4 (int a):a(a){
    (id = S347child4_classId);
{

    }
  }
  int a;
};


struct Trait347{

  int(*trait347)(Class*);
};

Vec* Trait347_v = newVec();

int Trait347_S347child1_trait347(Class* self_) {
  S347child1* self = ((S347child1*)self_);
{
    return (self -> a);
  }
} 
Trait347* newTrait347_S347child1() {
  Trait347 (* impl) = (new Trait347());
  setVec(Trait347_v, S347child1_classId, ((void*)impl));
  ((impl -> trait347) = (& Trait347_S347child1_trait347));
  return impl;
} 
Trait347* Trait347_S347child1_ = newTrait347_S347child1();

int Trait347_S347child2_trait347(Class* self_) {
  S347child2* self = ((S347child2*)self_);
{
    return (self -> a);
  }
} 
Trait347* newTrait347_S347child2() {
  Trait347 (* impl) = (new Trait347());
  setVec(Trait347_v, S347child2_classId, ((void*)impl));
  ((impl -> trait347) = (& Trait347_S347child2_trait347));
  return impl;
} 
Trait347* Trait347_S347child2_ = newTrait347_S347child2();

int Trait347_S347child3_trait347(Class* self_) {
  S347child3* self = ((S347child3*)self_);
{
    return (self -> a);
  }
} 
Trait347* newTrait347_S347child3() {
  Trait347 (* impl) = (new Trait347());
  setVec(Trait347_v, S347child3_classId, ((void*)impl));
  ((impl -> trait347) = (& Trait347_S347child3_trait347));
  return impl;
} 
Trait347* Trait347_S347child3_ = newTrait347_S347child3();

int Trait153_S347child1_trait153(Class* self_) {
  S347child1* self = ((S347child1*)self_);
{
    return (self -> a);
  }
} 
Trait153* newTrait153_S347child1() {
  Trait153 (* impl) = (new Trait153());
  setVec(Trait153_v, S347child1_classId, ((void*)impl));
  ((impl -> trait153) = (& Trait153_S347child1_trait153));
  return impl;
} 
Trait153* Trait153_S347child1_ = newTrait153_S347child1();

int S348_classId = Class_genId();
struct S348{

  int id;
  S348 ():id(S348_classId){

  }
};


int S348child1_classId = Class_genId();
struct S348child1:S348{

  S348child1 (int a):a(a){
    (id = S348child1_classId);
{

    }
  }
  int a;
};


int S348child2_classId = Class_genId();
struct S348child2:S348{

  S348child2 (int a):a(a){
    (id = S348child2_classId);
{

    }
  }
  int a;
};


struct Trait348{

  int(*trait348)(Class*);
};

Vec* Trait348_v = newVec();

int Trait348_S348child1_trait348(Class* self_) {
  S348child1* self = ((S348child1*)self_);
{
    return (self -> a);
  }
} 
Trait348* newTrait348_S348child1() {
  Trait348 (* impl) = (new Trait348());
  setVec(Trait348_v, S348child1_classId, ((void*)impl));
  ((impl -> trait348) = (& Trait348_S348child1_trait348));
  return impl;
} 
Trait348* Trait348_S348child1_ = newTrait348_S348child1();

int Trait130_S348child1_trait130(Class* self_) {
  S348child1* self = ((S348child1*)self_);
{
    return (self -> a);
  }
} 
Trait130* newTrait130_S348child1() {
  Trait130 (* impl) = (new Trait130());
  setVec(Trait130_v, S348child1_classId, ((void*)impl));
  ((impl -> trait130) = (& Trait130_S348child1_trait130));
  return impl;
} 
Trait130* Trait130_S348child1_ = newTrait130_S348child1();

int S349_classId = Class_genId();
struct S349{

  int id;
  S349 ():id(S349_classId){

  }
};


int S349child1_classId = Class_genId();
struct S349child1:S349{

  S349child1 (int a):a(a){
    (id = S349child1_classId);
{

    }
  }
  int a;
};


int S349child2_classId = Class_genId();
struct S349child2:S349{

  S349child2 (int a):a(a){
    (id = S349child2_classId);
{

    }
  }
  int a;
};


struct Trait349{

  int(*trait349)(Class*);
};

Vec* Trait349_v = newVec();

int Trait349_S349child1_trait349(Class* self_) {
  S349child1* self = ((S349child1*)self_);
{
    return (self -> a);
  }
} 
Trait349* newTrait349_S349child1() {
  Trait349 (* impl) = (new Trait349());
  setVec(Trait349_v, S349child1_classId, ((void*)impl));
  ((impl -> trait349) = (& Trait349_S349child1_trait349));
  return impl;
} 
Trait349* Trait349_S349child1_ = newTrait349_S349child1();

int Trait92_S349child1_trait92(Class* self_) {
  S349child1* self = ((S349child1*)self_);
{
    return (self -> a);
  }
} 
Trait92* newTrait92_S349child1() {
  Trait92 (* impl) = (new Trait92());
  setVec(Trait92_v, S349child1_classId, ((void*)impl));
  ((impl -> trait92) = (& Trait92_S349child1_trait92));
  return impl;
} 
Trait92* Trait92_S349child1_ = newTrait92_S349child1();

int S350_classId = Class_genId();
struct S350{

  int id;
  S350 ():id(S350_classId){

  }
};


int S350child1_classId = Class_genId();
struct S350child1:S350{

  S350child1 (int a):a(a){
    (id = S350child1_classId);
{

    }
  }
  int a;
};


int S350child2_classId = Class_genId();
struct S350child2:S350{

  S350child2 (int a):a(a){
    (id = S350child2_classId);
{

    }
  }
  int a;
};


struct Trait350{

  int(*trait350)(Class*);
};

Vec* Trait350_v = newVec();

int Trait350_S350child1_trait350(Class* self_) {
  S350child1* self = ((S350child1*)self_);
{
    return (self -> a);
  }
} 
Trait350* newTrait350_S350child1() {
  Trait350 (* impl) = (new Trait350());
  setVec(Trait350_v, S350child1_classId, ((void*)impl));
  ((impl -> trait350) = (& Trait350_S350child1_trait350));
  return impl;
} 
Trait350* Trait350_S350child1_ = newTrait350_S350child1();

int Trait149_S350child1_trait149(Class* self_) {
  S350child1* self = ((S350child1*)self_);
{
    return (self -> a);
  }
} 
Trait149* newTrait149_S350child1() {
  Trait149 (* impl) = (new Trait149());
  setVec(Trait149_v, S350child1_classId, ((void*)impl));
  ((impl -> trait149) = (& Trait149_S350child1_trait149));
  return impl;
} 
Trait149* Trait149_S350child1_ = newTrait149_S350child1();

int S351_classId = Class_genId();
struct S351{

  int id;
  S351 ():id(S351_classId){

  }
};


int S351child1_classId = Class_genId();
struct S351child1:S351{

  S351child1 (int a):a(a){
    (id = S351child1_classId);
{

    }
  }
  int a;
};


int S351child2_classId = Class_genId();
struct S351child2:S351{

  S351child2 (int a):a(a){
    (id = S351child2_classId);
{

    }
  }
  int a;
};


int S351child3_classId = Class_genId();
struct S351child3:S351{

  S351child3 (int a):a(a){
    (id = S351child3_classId);
{

    }
  }
  int a;
};


int S351child4_classId = Class_genId();
struct S351child4:S351{

  S351child4 (int a):a(a){
    (id = S351child4_classId);
{

    }
  }
  int a;
};


struct Trait351{

  int(*trait351)(Class*);
};

Vec* Trait351_v = newVec();

int Trait351_S351child1_trait351(Class* self_) {
  S351child1* self = ((S351child1*)self_);
{
    return (self -> a);
  }
} 
Trait351* newTrait351_S351child1() {
  Trait351 (* impl) = (new Trait351());
  setVec(Trait351_v, S351child1_classId, ((void*)impl));
  ((impl -> trait351) = (& Trait351_S351child1_trait351));
  return impl;
} 
Trait351* Trait351_S351child1_ = newTrait351_S351child1();

int Trait351_S351child2_trait351(Class* self_) {
  S351child2* self = ((S351child2*)self_);
{
    return (self -> a);
  }
} 
Trait351* newTrait351_S351child2() {
  Trait351 (* impl) = (new Trait351());
  setVec(Trait351_v, S351child2_classId, ((void*)impl));
  ((impl -> trait351) = (& Trait351_S351child2_trait351));
  return impl;
} 
Trait351* Trait351_S351child2_ = newTrait351_S351child2();

int Trait107_S351child1_trait107(Class* self_) {
  S351child1* self = ((S351child1*)self_);
{
    return (self -> a);
  }
} 
Trait107* newTrait107_S351child1() {
  Trait107 (* impl) = (new Trait107());
  setVec(Trait107_v, S351child1_classId, ((void*)impl));
  ((impl -> trait107) = (& Trait107_S351child1_trait107));
  return impl;
} 
Trait107* Trait107_S351child1_ = newTrait107_S351child1();

int S352_classId = Class_genId();
struct S352{

  int id;
  S352 ():id(S352_classId){

  }
};


int S352child1_classId = Class_genId();
struct S352child1:S352{

  S352child1 (int a):a(a){
    (id = S352child1_classId);
{

    }
  }
  int a;
};


int S352child2_classId = Class_genId();
struct S352child2:S352{

  S352child2 (int a):a(a){
    (id = S352child2_classId);
{

    }
  }
  int a;
};


int S352child3_classId = Class_genId();
struct S352child3:S352{

  S352child3 (int a):a(a){
    (id = S352child3_classId);
{

    }
  }
  int a;
};


struct Trait352{

  int(*trait352)(Class*);
};

Vec* Trait352_v = newVec();

int Trait352_S352child1_trait352(Class* self_) {
  S352child1* self = ((S352child1*)self_);
{
    return (self -> a);
  }
} 
Trait352* newTrait352_S352child1() {
  Trait352 (* impl) = (new Trait352());
  setVec(Trait352_v, S352child1_classId, ((void*)impl));
  ((impl -> trait352) = (& Trait352_S352child1_trait352));
  return impl;
} 
Trait352* Trait352_S352child1_ = newTrait352_S352child1();

int Trait352_S352child2_trait352(Class* self_) {
  S352child2* self = ((S352child2*)self_);
{
    return (self -> a);
  }
} 
Trait352* newTrait352_S352child2() {
  Trait352 (* impl) = (new Trait352());
  setVec(Trait352_v, S352child2_classId, ((void*)impl));
  ((impl -> trait352) = (& Trait352_S352child2_trait352));
  return impl;
} 
Trait352* Trait352_S352child2_ = newTrait352_S352child2();

int Trait215_S352child1_trait215(Class* self_) {
  S352child1* self = ((S352child1*)self_);
{
    return (self -> a);
  }
} 
Trait215* newTrait215_S352child1() {
  Trait215 (* impl) = (new Trait215());
  setVec(Trait215_v, S352child1_classId, ((void*)impl));
  ((impl -> trait215) = (& Trait215_S352child1_trait215));
  return impl;
} 
Trait215* Trait215_S352child1_ = newTrait215_S352child1();

int S353_classId = Class_genId();
struct S353{

  int id;
  S353 ():id(S353_classId){

  }
};


int S353child1_classId = Class_genId();
struct S353child1:S353{

  S353child1 (int a):a(a){
    (id = S353child1_classId);
{

    }
  }
  int a;
};


int S353child2_classId = Class_genId();
struct S353child2:S353{

  S353child2 (int a):a(a){
    (id = S353child2_classId);
{

    }
  }
  int a;
};


int S353child3_classId = Class_genId();
struct S353child3:S353{

  S353child3 (int a):a(a){
    (id = S353child3_classId);
{

    }
  }
  int a;
};


struct Trait353{

  int(*trait353)(Class*);
};

Vec* Trait353_v = newVec();

int Trait353_S353child1_trait353(Class* self_) {
  S353child1* self = ((S353child1*)self_);
{
    return (self -> a);
  }
} 
Trait353* newTrait353_S353child1() {
  Trait353 (* impl) = (new Trait353());
  setVec(Trait353_v, S353child1_classId, ((void*)impl));
  ((impl -> trait353) = (& Trait353_S353child1_trait353));
  return impl;
} 
Trait353* Trait353_S353child1_ = newTrait353_S353child1();

int Trait353_S353child2_trait353(Class* self_) {
  S353child2* self = ((S353child2*)self_);
{
    return (self -> a);
  }
} 
Trait353* newTrait353_S353child2() {
  Trait353 (* impl) = (new Trait353());
  setVec(Trait353_v, S353child2_classId, ((void*)impl));
  ((impl -> trait353) = (& Trait353_S353child2_trait353));
  return impl;
} 
Trait353* Trait353_S353child2_ = newTrait353_S353child2();

int Trait353_S353child3_trait353(Class* self_) {
  S353child3* self = ((S353child3*)self_);
{
    return (self -> a);
  }
} 
Trait353* newTrait353_S353child3() {
  Trait353 (* impl) = (new Trait353());
  setVec(Trait353_v, S353child3_classId, ((void*)impl));
  ((impl -> trait353) = (& Trait353_S353child3_trait353));
  return impl;
} 
Trait353* Trait353_S353child3_ = newTrait353_S353child3();

int Trait170_S353child1_trait170(Class* self_) {
  S353child1* self = ((S353child1*)self_);
{
    return (self -> a);
  }
} 
Trait170* newTrait170_S353child1() {
  Trait170 (* impl) = (new Trait170());
  setVec(Trait170_v, S353child1_classId, ((void*)impl));
  ((impl -> trait170) = (& Trait170_S353child1_trait170));
  return impl;
} 
Trait170* Trait170_S353child1_ = newTrait170_S353child1();

int S354_classId = Class_genId();
struct S354{

  int id;
  S354 ():id(S354_classId){

  }
};


int S354child1_classId = Class_genId();
struct S354child1:S354{

  S354child1 (int a):a(a){
    (id = S354child1_classId);
{

    }
  }
  int a;
};


int S354child2_classId = Class_genId();
struct S354child2:S354{

  S354child2 (int a):a(a){
    (id = S354child2_classId);
{

    }
  }
  int a;
};


int S354child3_classId = Class_genId();
struct S354child3:S354{

  S354child3 (int a):a(a){
    (id = S354child3_classId);
{

    }
  }
  int a;
};


int S354child4_classId = Class_genId();
struct S354child4:S354{

  S354child4 (int a):a(a){
    (id = S354child4_classId);
{

    }
  }
  int a;
};


int S354child5_classId = Class_genId();
struct S354child5:S354{

  S354child5 (int a):a(a){
    (id = S354child5_classId);
{

    }
  }
  int a;
};


struct Trait354{

  int(*trait354)(Class*);
};

Vec* Trait354_v = newVec();

int Trait354_S354child1_trait354(Class* self_) {
  S354child1* self = ((S354child1*)self_);
{
    return (self -> a);
  }
} 
Trait354* newTrait354_S354child1() {
  Trait354 (* impl) = (new Trait354());
  setVec(Trait354_v, S354child1_classId, ((void*)impl));
  ((impl -> trait354) = (& Trait354_S354child1_trait354));
  return impl;
} 
Trait354* Trait354_S354child1_ = newTrait354_S354child1();

int Trait354_S354child2_trait354(Class* self_) {
  S354child2* self = ((S354child2*)self_);
{
    return (self -> a);
  }
} 
Trait354* newTrait354_S354child2() {
  Trait354 (* impl) = (new Trait354());
  setVec(Trait354_v, S354child2_classId, ((void*)impl));
  ((impl -> trait354) = (& Trait354_S354child2_trait354));
  return impl;
} 
Trait354* Trait354_S354child2_ = newTrait354_S354child2();

int Trait354_S354child3_trait354(Class* self_) {
  S354child3* self = ((S354child3*)self_);
{
    return (self -> a);
  }
} 
Trait354* newTrait354_S354child3() {
  Trait354 (* impl) = (new Trait354());
  setVec(Trait354_v, S354child3_classId, ((void*)impl));
  ((impl -> trait354) = (& Trait354_S354child3_trait354));
  return impl;
} 
Trait354* Trait354_S354child3_ = newTrait354_S354child3();

int Trait151_S354child1_trait151(Class* self_) {
  S354child1* self = ((S354child1*)self_);
{
    return (self -> a);
  }
} 
Trait151* newTrait151_S354child1() {
  Trait151 (* impl) = (new Trait151());
  setVec(Trait151_v, S354child1_classId, ((void*)impl));
  ((impl -> trait151) = (& Trait151_S354child1_trait151));
  return impl;
} 
Trait151* Trait151_S354child1_ = newTrait151_S354child1();

int S355_classId = Class_genId();
struct S355{

  int id;
  S355 ():id(S355_classId){

  }
};


int S355child1_classId = Class_genId();
struct S355child1:S355{

  S355child1 (int a):a(a){
    (id = S355child1_classId);
{

    }
  }
  int a;
};


int S355child2_classId = Class_genId();
struct S355child2:S355{

  S355child2 (int a):a(a){
    (id = S355child2_classId);
{

    }
  }
  int a;
};


int S355child3_classId = Class_genId();
struct S355child3:S355{

  S355child3 (int a):a(a){
    (id = S355child3_classId);
{

    }
  }
  int a;
};


struct Trait355{

  int(*trait355)(Class*);
};

Vec* Trait355_v = newVec();

int Trait355_S355child1_trait355(Class* self_) {
  S355child1* self = ((S355child1*)self_);
{
    return (self -> a);
  }
} 
Trait355* newTrait355_S355child1() {
  Trait355 (* impl) = (new Trait355());
  setVec(Trait355_v, S355child1_classId, ((void*)impl));
  ((impl -> trait355) = (& Trait355_S355child1_trait355));
  return impl;
} 
Trait355* Trait355_S355child1_ = newTrait355_S355child1();

int Trait355_S355child2_trait355(Class* self_) {
  S355child2* self = ((S355child2*)self_);
{
    return (self -> a);
  }
} 
Trait355* newTrait355_S355child2() {
  Trait355 (* impl) = (new Trait355());
  setVec(Trait355_v, S355child2_classId, ((void*)impl));
  ((impl -> trait355) = (& Trait355_S355child2_trait355));
  return impl;
} 
Trait355* Trait355_S355child2_ = newTrait355_S355child2();

int Trait233_S355child1_trait233(Class* self_) {
  S355child1* self = ((S355child1*)self_);
{
    return (self -> a);
  }
} 
Trait233* newTrait233_S355child1() {
  Trait233 (* impl) = (new Trait233());
  setVec(Trait233_v, S355child1_classId, ((void*)impl));
  ((impl -> trait233) = (& Trait233_S355child1_trait233));
  return impl;
} 
Trait233* Trait233_S355child1_ = newTrait233_S355child1();

int S356_classId = Class_genId();
struct S356{

  int id;
  S356 ():id(S356_classId){

  }
};


int S356child1_classId = Class_genId();
struct S356child1:S356{

  S356child1 (int a):a(a){
    (id = S356child1_classId);
{

    }
  }
  int a;
};


struct Trait356{

  int(*trait356)(Class*);
};

Vec* Trait356_v = newVec();

int Trait356_S356child1_trait356(Class* self_) {
  S356child1* self = ((S356child1*)self_);
{
    return (self -> a);
  }
} 
Trait356* newTrait356_S356child1() {
  Trait356 (* impl) = (new Trait356());
  setVec(Trait356_v, S356child1_classId, ((void*)impl));
  ((impl -> trait356) = (& Trait356_S356child1_trait356));
  return impl;
} 
Trait356* Trait356_S356child1_ = newTrait356_S356child1();

int Trait16_S356child1_trait16(Class* self_) {
  S356child1* self = ((S356child1*)self_);
{
    return (self -> a);
  }
} 
Trait16* newTrait16_S356child1() {
  Trait16 (* impl) = (new Trait16());
  setVec(Trait16_v, S356child1_classId, ((void*)impl));
  ((impl -> trait16) = (& Trait16_S356child1_trait16));
  return impl;
} 
Trait16* Trait16_S356child1_ = newTrait16_S356child1();

int S357_classId = Class_genId();
struct S357{

  int id;
  S357 ():id(S357_classId){

  }
};


int S357child1_classId = Class_genId();
struct S357child1:S357{

  S357child1 (int a):a(a){
    (id = S357child1_classId);
{

    }
  }
  int a;
};


int S357child2_classId = Class_genId();
struct S357child2:S357{

  S357child2 (int a):a(a){
    (id = S357child2_classId);
{

    }
  }
  int a;
};


struct Trait357{

  int(*trait357)(Class*);
};

Vec* Trait357_v = newVec();

int Trait357_S357child1_trait357(Class* self_) {
  S357child1* self = ((S357child1*)self_);
{
    return (self -> a);
  }
} 
Trait357* newTrait357_S357child1() {
  Trait357 (* impl) = (new Trait357());
  setVec(Trait357_v, S357child1_classId, ((void*)impl));
  ((impl -> trait357) = (& Trait357_S357child1_trait357));
  return impl;
} 
Trait357* Trait357_S357child1_ = newTrait357_S357child1();

int Trait357_S357child2_trait357(Class* self_) {
  S357child2* self = ((S357child2*)self_);
{
    return (self -> a);
  }
} 
Trait357* newTrait357_S357child2() {
  Trait357 (* impl) = (new Trait357());
  setVec(Trait357_v, S357child2_classId, ((void*)impl));
  ((impl -> trait357) = (& Trait357_S357child2_trait357));
  return impl;
} 
Trait357* Trait357_S357child2_ = newTrait357_S357child2();

int Trait198_S357child1_trait198(Class* self_) {
  S357child1* self = ((S357child1*)self_);
{
    return (self -> a);
  }
} 
Trait198* newTrait198_S357child1() {
  Trait198 (* impl) = (new Trait198());
  setVec(Trait198_v, S357child1_classId, ((void*)impl));
  ((impl -> trait198) = (& Trait198_S357child1_trait198));
  return impl;
} 
Trait198* Trait198_S357child1_ = newTrait198_S357child1();

int S358_classId = Class_genId();
struct S358{

  int id;
  S358 ():id(S358_classId){

  }
};


int S358child1_classId = Class_genId();
struct S358child1:S358{

  S358child1 (int a):a(a){
    (id = S358child1_classId);
{

    }
  }
  int a;
};


struct Trait358{

  int(*trait358)(Class*);
};

Vec* Trait358_v = newVec();

int Trait358_S358child1_trait358(Class* self_) {
  S358child1* self = ((S358child1*)self_);
{
    return (self -> a);
  }
} 
Trait358* newTrait358_S358child1() {
  Trait358 (* impl) = (new Trait358());
  setVec(Trait358_v, S358child1_classId, ((void*)impl));
  ((impl -> trait358) = (& Trait358_S358child1_trait358));
  return impl;
} 
Trait358* Trait358_S358child1_ = newTrait358_S358child1();

int Trait340_S358child1_trait340(Class* self_) {
  S358child1* self = ((S358child1*)self_);
{
    return (self -> a);
  }
} 
Trait340* newTrait340_S358child1() {
  Trait340 (* impl) = (new Trait340());
  setVec(Trait340_v, S358child1_classId, ((void*)impl));
  ((impl -> trait340) = (& Trait340_S358child1_trait340));
  return impl;
} 
Trait340* Trait340_S358child1_ = newTrait340_S358child1();

int S359_classId = Class_genId();
struct S359{

  int id;
  S359 ():id(S359_classId){

  }
};


int S359child1_classId = Class_genId();
struct S359child1:S359{

  S359child1 (int a):a(a){
    (id = S359child1_classId);
{

    }
  }
  int a;
};


int S359child2_classId = Class_genId();
struct S359child2:S359{

  S359child2 (int a):a(a){
    (id = S359child2_classId);
{

    }
  }
  int a;
};


int S359child3_classId = Class_genId();
struct S359child3:S359{

  S359child3 (int a):a(a){
    (id = S359child3_classId);
{

    }
  }
  int a;
};


struct Trait359{

  int(*trait359)(Class*);
};

Vec* Trait359_v = newVec();

int Trait359_S359child1_trait359(Class* self_) {
  S359child1* self = ((S359child1*)self_);
{
    return (self -> a);
  }
} 
Trait359* newTrait359_S359child1() {
  Trait359 (* impl) = (new Trait359());
  setVec(Trait359_v, S359child1_classId, ((void*)impl));
  ((impl -> trait359) = (& Trait359_S359child1_trait359));
  return impl;
} 
Trait359* Trait359_S359child1_ = newTrait359_S359child1();

int Trait238_S359child1_trait238(Class* self_) {
  S359child1* self = ((S359child1*)self_);
{
    return (self -> a);
  }
} 
Trait238* newTrait238_S359child1() {
  Trait238 (* impl) = (new Trait238());
  setVec(Trait238_v, S359child1_classId, ((void*)impl));
  ((impl -> trait238) = (& Trait238_S359child1_trait238));
  return impl;
} 
Trait238* Trait238_S359child1_ = newTrait238_S359child1();

int S360_classId = Class_genId();
struct S360{

  int id;
  S360 ():id(S360_classId){

  }
};


int S360child1_classId = Class_genId();
struct S360child1:S360{

  S360child1 (int a):a(a){
    (id = S360child1_classId);
{

    }
  }
  int a;
};


int S360child2_classId = Class_genId();
struct S360child2:S360{

  S360child2 (int a):a(a){
    (id = S360child2_classId);
{

    }
  }
  int a;
};


int S360child3_classId = Class_genId();
struct S360child3:S360{

  S360child3 (int a):a(a){
    (id = S360child3_classId);
{

    }
  }
  int a;
};


struct Trait360{

  int(*trait360)(Class*);
};

Vec* Trait360_v = newVec();

int Trait360_S360child1_trait360(Class* self_) {
  S360child1* self = ((S360child1*)self_);
{
    return (self -> a);
  }
} 
Trait360* newTrait360_S360child1() {
  Trait360 (* impl) = (new Trait360());
  setVec(Trait360_v, S360child1_classId, ((void*)impl));
  ((impl -> trait360) = (& Trait360_S360child1_trait360));
  return impl;
} 
Trait360* Trait360_S360child1_ = newTrait360_S360child1();

int Trait360_S360child2_trait360(Class* self_) {
  S360child2* self = ((S360child2*)self_);
{
    return (self -> a);
  }
} 
Trait360* newTrait360_S360child2() {
  Trait360 (* impl) = (new Trait360());
  setVec(Trait360_v, S360child2_classId, ((void*)impl));
  ((impl -> trait360) = (& Trait360_S360child2_trait360));
  return impl;
} 
Trait360* Trait360_S360child2_ = newTrait360_S360child2();

int Trait40_S360child1_trait40(Class* self_) {
  S360child1* self = ((S360child1*)self_);
{
    return (self -> a);
  }
} 
Trait40* newTrait40_S360child1() {
  Trait40 (* impl) = (new Trait40());
  setVec(Trait40_v, S360child1_classId, ((void*)impl));
  ((impl -> trait40) = (& Trait40_S360child1_trait40));
  return impl;
} 
Trait40* Trait40_S360child1_ = newTrait40_S360child1();

int S361_classId = Class_genId();
struct S361{

  int id;
  S361 ():id(S361_classId){

  }
};


int S361child1_classId = Class_genId();
struct S361child1:S361{

  S361child1 (int a):a(a){
    (id = S361child1_classId);
{

    }
  }
  int a;
};


int S361child2_classId = Class_genId();
struct S361child2:S361{

  S361child2 (int a):a(a){
    (id = S361child2_classId);
{

    }
  }
  int a;
};


int S361child3_classId = Class_genId();
struct S361child3:S361{

  S361child3 (int a):a(a){
    (id = S361child3_classId);
{

    }
  }
  int a;
};


struct Trait361{

  int(*trait361)(Class*);
};

Vec* Trait361_v = newVec();

int Trait361_S361child1_trait361(Class* self_) {
  S361child1* self = ((S361child1*)self_);
{
    return (self -> a);
  }
} 
Trait361* newTrait361_S361child1() {
  Trait361 (* impl) = (new Trait361());
  setVec(Trait361_v, S361child1_classId, ((void*)impl));
  ((impl -> trait361) = (& Trait361_S361child1_trait361));
  return impl;
} 
Trait361* Trait361_S361child1_ = newTrait361_S361child1();

int Trait273_S361child1_trait273(Class* self_) {
  S361child1* self = ((S361child1*)self_);
{
    return (self -> a);
  }
} 
Trait273* newTrait273_S361child1() {
  Trait273 (* impl) = (new Trait273());
  setVec(Trait273_v, S361child1_classId, ((void*)impl));
  ((impl -> trait273) = (& Trait273_S361child1_trait273));
  return impl;
} 
Trait273* Trait273_S361child1_ = newTrait273_S361child1();

int S362_classId = Class_genId();
struct S362{

  int id;
  S362 ():id(S362_classId){

  }
};


int S362child1_classId = Class_genId();
struct S362child1:S362{

  S362child1 (int a):a(a){
    (id = S362child1_classId);
{

    }
  }
  int a;
};


struct Trait362{

  int(*trait362)(Class*);
};

Vec* Trait362_v = newVec();

int Trait362_S362child1_trait362(Class* self_) {
  S362child1* self = ((S362child1*)self_);
{
    return (self -> a);
  }
} 
Trait362* newTrait362_S362child1() {
  Trait362 (* impl) = (new Trait362());
  setVec(Trait362_v, S362child1_classId, ((void*)impl));
  ((impl -> trait362) = (& Trait362_S362child1_trait362));
  return impl;
} 
Trait362* Trait362_S362child1_ = newTrait362_S362child1();

int Trait94_S362child1_trait94(Class* self_) {
  S362child1* self = ((S362child1*)self_);
{
    return (self -> a);
  }
} 
Trait94* newTrait94_S362child1() {
  Trait94 (* impl) = (new Trait94());
  setVec(Trait94_v, S362child1_classId, ((void*)impl));
  ((impl -> trait94) = (& Trait94_S362child1_trait94));
  return impl;
} 
Trait94* Trait94_S362child1_ = newTrait94_S362child1();

int S363_classId = Class_genId();
struct S363{

  int id;
  S363 ():id(S363_classId){

  }
};


int S363child1_classId = Class_genId();
struct S363child1:S363{

  S363child1 (int a):a(a){
    (id = S363child1_classId);
{

    }
  }
  int a;
};


int S363child2_classId = Class_genId();
struct S363child2:S363{

  S363child2 (int a):a(a){
    (id = S363child2_classId);
{

    }
  }
  int a;
};


struct Trait363{

  int(*trait363)(Class*);
};

Vec* Trait363_v = newVec();

int Trait363_S363child1_trait363(Class* self_) {
  S363child1* self = ((S363child1*)self_);
{
    return (self -> a);
  }
} 
Trait363* newTrait363_S363child1() {
  Trait363 (* impl) = (new Trait363());
  setVec(Trait363_v, S363child1_classId, ((void*)impl));
  ((impl -> trait363) = (& Trait363_S363child1_trait363));
  return impl;
} 
Trait363* Trait363_S363child1_ = newTrait363_S363child1();

int Trait184_S363child1_trait184(Class* self_) {
  S363child1* self = ((S363child1*)self_);
{
    return (self -> a);
  }
} 
Trait184* newTrait184_S363child1() {
  Trait184 (* impl) = (new Trait184());
  setVec(Trait184_v, S363child1_classId, ((void*)impl));
  ((impl -> trait184) = (& Trait184_S363child1_trait184));
  return impl;
} 
Trait184* Trait184_S363child1_ = newTrait184_S363child1();

int S364_classId = Class_genId();
struct S364{

  int id;
  S364 ():id(S364_classId){

  }
};


int S364child1_classId = Class_genId();
struct S364child1:S364{

  S364child1 (int a):a(a){
    (id = S364child1_classId);
{

    }
  }
  int a;
};


int S364child2_classId = Class_genId();
struct S364child2:S364{

  S364child2 (int a):a(a){
    (id = S364child2_classId);
{

    }
  }
  int a;
};


struct Trait364{

  int(*trait364)(Class*);
};

Vec* Trait364_v = newVec();

int Trait364_S364child1_trait364(Class* self_) {
  S364child1* self = ((S364child1*)self_);
{
    return (self -> a);
  }
} 
Trait364* newTrait364_S364child1() {
  Trait364 (* impl) = (new Trait364());
  setVec(Trait364_v, S364child1_classId, ((void*)impl));
  ((impl -> trait364) = (& Trait364_S364child1_trait364));
  return impl;
} 
Trait364* Trait364_S364child1_ = newTrait364_S364child1();

int Trait265_S364child1_trait265(Class* self_) {
  S364child1* self = ((S364child1*)self_);
{
    return (self -> a);
  }
} 
Trait265* newTrait265_S364child1() {
  Trait265 (* impl) = (new Trait265());
  setVec(Trait265_v, S364child1_classId, ((void*)impl));
  ((impl -> trait265) = (& Trait265_S364child1_trait265));
  return impl;
} 
Trait265* Trait265_S364child1_ = newTrait265_S364child1();

int S365_classId = Class_genId();
struct S365{

  int id;
  S365 ():id(S365_classId){

  }
};


int S365child1_classId = Class_genId();
struct S365child1:S365{

  S365child1 (int a):a(a){
    (id = S365child1_classId);
{

    }
  }
  int a;
};


struct Trait365{

  int(*trait365)(Class*);
};

Vec* Trait365_v = newVec();

int Trait365_S365child1_trait365(Class* self_) {
  S365child1* self = ((S365child1*)self_);
{
    return (self -> a);
  }
} 
Trait365* newTrait365_S365child1() {
  Trait365 (* impl) = (new Trait365());
  setVec(Trait365_v, S365child1_classId, ((void*)impl));
  ((impl -> trait365) = (& Trait365_S365child1_trait365));
  return impl;
} 
Trait365* Trait365_S365child1_ = newTrait365_S365child1();

int Trait229_S365child1_trait229(Class* self_) {
  S365child1* self = ((S365child1*)self_);
{
    return (self -> a);
  }
} 
Trait229* newTrait229_S365child1() {
  Trait229 (* impl) = (new Trait229());
  setVec(Trait229_v, S365child1_classId, ((void*)impl));
  ((impl -> trait229) = (& Trait229_S365child1_trait229));
  return impl;
} 
Trait229* Trait229_S365child1_ = newTrait229_S365child1();

int S366_classId = Class_genId();
struct S366{

  int id;
  S366 ():id(S366_classId){

  }
};


int S366child1_classId = Class_genId();
struct S366child1:S366{

  S366child1 (int a):a(a){
    (id = S366child1_classId);
{

    }
  }
  int a;
};


struct Trait366{

  int(*trait366)(Class*);
};

Vec* Trait366_v = newVec();

int Trait366_S366child1_trait366(Class* self_) {
  S366child1* self = ((S366child1*)self_);
{
    return (self -> a);
  }
} 
Trait366* newTrait366_S366child1() {
  Trait366 (* impl) = (new Trait366());
  setVec(Trait366_v, S366child1_classId, ((void*)impl));
  ((impl -> trait366) = (& Trait366_S366child1_trait366));
  return impl;
} 
Trait366* Trait366_S366child1_ = newTrait366_S366child1();

int Trait167_S366child1_trait167(Class* self_) {
  S366child1* self = ((S366child1*)self_);
{
    return (self -> a);
  }
} 
Trait167* newTrait167_S366child1() {
  Trait167 (* impl) = (new Trait167());
  setVec(Trait167_v, S366child1_classId, ((void*)impl));
  ((impl -> trait167) = (& Trait167_S366child1_trait167));
  return impl;
} 
Trait167* Trait167_S366child1_ = newTrait167_S366child1();

int S367_classId = Class_genId();
struct S367{

  int id;
  S367 ():id(S367_classId){

  }
};


int S367child1_classId = Class_genId();
struct S367child1:S367{

  S367child1 (int a):a(a){
    (id = S367child1_classId);
{

    }
  }
  int a;
};


int S367child2_classId = Class_genId();
struct S367child2:S367{

  S367child2 (int a):a(a){
    (id = S367child2_classId);
{

    }
  }
  int a;
};


int S367child3_classId = Class_genId();
struct S367child3:S367{

  S367child3 (int a):a(a){
    (id = S367child3_classId);
{

    }
  }
  int a;
};


struct Trait367{

  int(*trait367)(Class*);
};

Vec* Trait367_v = newVec();

int Trait367_S367child1_trait367(Class* self_) {
  S367child1* self = ((S367child1*)self_);
{
    return (self -> a);
  }
} 
Trait367* newTrait367_S367child1() {
  Trait367 (* impl) = (new Trait367());
  setVec(Trait367_v, S367child1_classId, ((void*)impl));
  ((impl -> trait367) = (& Trait367_S367child1_trait367));
  return impl;
} 
Trait367* Trait367_S367child1_ = newTrait367_S367child1();

int Trait367_S367child2_trait367(Class* self_) {
  S367child2* self = ((S367child2*)self_);
{
    return (self -> a);
  }
} 
Trait367* newTrait367_S367child2() {
  Trait367 (* impl) = (new Trait367());
  setVec(Trait367_v, S367child2_classId, ((void*)impl));
  ((impl -> trait367) = (& Trait367_S367child2_trait367));
  return impl;
} 
Trait367* Trait367_S367child2_ = newTrait367_S367child2();

int Trait367_S367child3_trait367(Class* self_) {
  S367child3* self = ((S367child3*)self_);
{
    return (self -> a);
  }
} 
Trait367* newTrait367_S367child3() {
  Trait367 (* impl) = (new Trait367());
  setVec(Trait367_v, S367child3_classId, ((void*)impl));
  ((impl -> trait367) = (& Trait367_S367child3_trait367));
  return impl;
} 
Trait367* Trait367_S367child3_ = newTrait367_S367child3();

int Trait5_S367child1_trait5(Class* self_) {
  S367child1* self = ((S367child1*)self_);
{
    return (self -> a);
  }
} 
Trait5* newTrait5_S367child1() {
  Trait5 (* impl) = (new Trait5());
  setVec(Trait5_v, S367child1_classId, ((void*)impl));
  ((impl -> trait5) = (& Trait5_S367child1_trait5));
  return impl;
} 
Trait5* Trait5_S367child1_ = newTrait5_S367child1();

int S368_classId = Class_genId();
struct S368{

  int id;
  S368 ():id(S368_classId){

  }
};


int S368child1_classId = Class_genId();
struct S368child1:S368{

  S368child1 (int a):a(a){
    (id = S368child1_classId);
{

    }
  }
  int a;
};


int S368child2_classId = Class_genId();
struct S368child2:S368{

  S368child2 (int a):a(a){
    (id = S368child2_classId);
{

    }
  }
  int a;
};


struct Trait368{

  int(*trait368)(Class*);
};

Vec* Trait368_v = newVec();

int Trait368_S368child1_trait368(Class* self_) {
  S368child1* self = ((S368child1*)self_);
{
    return (self -> a);
  }
} 
Trait368* newTrait368_S368child1() {
  Trait368 (* impl) = (new Trait368());
  setVec(Trait368_v, S368child1_classId, ((void*)impl));
  ((impl -> trait368) = (& Trait368_S368child1_trait368));
  return impl;
} 
Trait368* Trait368_S368child1_ = newTrait368_S368child1();

int Trait269_S368child1_trait269(Class* self_) {
  S368child1* self = ((S368child1*)self_);
{
    return (self -> a);
  }
} 
Trait269* newTrait269_S368child1() {
  Trait269 (* impl) = (new Trait269());
  setVec(Trait269_v, S368child1_classId, ((void*)impl));
  ((impl -> trait269) = (& Trait269_S368child1_trait269));
  return impl;
} 
Trait269* Trait269_S368child1_ = newTrait269_S368child1();

int S369_classId = Class_genId();
struct S369{

  int id;
  S369 ():id(S369_classId){

  }
};


int S369child1_classId = Class_genId();
struct S369child1:S369{

  S369child1 (int a):a(a){
    (id = S369child1_classId);
{

    }
  }
  int a;
};


int S369child2_classId = Class_genId();
struct S369child2:S369{

  S369child2 (int a):a(a){
    (id = S369child2_classId);
{

    }
  }
  int a;
};


struct Trait369{

  int(*trait369)(Class*);
};

Vec* Trait369_v = newVec();

int Trait369_S369child1_trait369(Class* self_) {
  S369child1* self = ((S369child1*)self_);
{
    return (self -> a);
  }
} 
Trait369* newTrait369_S369child1() {
  Trait369 (* impl) = (new Trait369());
  setVec(Trait369_v, S369child1_classId, ((void*)impl));
  ((impl -> trait369) = (& Trait369_S369child1_trait369));
  return impl;
} 
Trait369* Trait369_S369child1_ = newTrait369_S369child1();

int Trait369_S369child2_trait369(Class* self_) {
  S369child2* self = ((S369child2*)self_);
{
    return (self -> a);
  }
} 
Trait369* newTrait369_S369child2() {
  Trait369 (* impl) = (new Trait369());
  setVec(Trait369_v, S369child2_classId, ((void*)impl));
  ((impl -> trait369) = (& Trait369_S369child2_trait369));
  return impl;
} 
Trait369* Trait369_S369child2_ = newTrait369_S369child2();

int Trait28_S369child1_trait28(Class* self_) {
  S369child1* self = ((S369child1*)self_);
{
    return (self -> a);
  }
} 
Trait28* newTrait28_S369child1() {
  Trait28 (* impl) = (new Trait28());
  setVec(Trait28_v, S369child1_classId, ((void*)impl));
  ((impl -> trait28) = (& Trait28_S369child1_trait28));
  return impl;
} 
Trait28* Trait28_S369child1_ = newTrait28_S369child1();

int S370_classId = Class_genId();
struct S370{

  int id;
  S370 ():id(S370_classId){

  }
};


int S370child1_classId = Class_genId();
struct S370child1:S370{

  S370child1 (int a):a(a){
    (id = S370child1_classId);
{

    }
  }
  int a;
};


int S370child2_classId = Class_genId();
struct S370child2:S370{

  S370child2 (int a):a(a){
    (id = S370child2_classId);
{

    }
  }
  int a;
};


int S370child3_classId = Class_genId();
struct S370child3:S370{

  S370child3 (int a):a(a){
    (id = S370child3_classId);
{

    }
  }
  int a;
};


int S370child4_classId = Class_genId();
struct S370child4:S370{

  S370child4 (int a):a(a){
    (id = S370child4_classId);
{

    }
  }
  int a;
};


int S370child5_classId = Class_genId();
struct S370child5:S370{

  S370child5 (int a):a(a){
    (id = S370child5_classId);
{

    }
  }
  int a;
};


struct Trait370{

  int(*trait370)(Class*);
};

Vec* Trait370_v = newVec();

int Trait370_S370child1_trait370(Class* self_) {
  S370child1* self = ((S370child1*)self_);
{
    return (self -> a);
  }
} 
Trait370* newTrait370_S370child1() {
  Trait370 (* impl) = (new Trait370());
  setVec(Trait370_v, S370child1_classId, ((void*)impl));
  ((impl -> trait370) = (& Trait370_S370child1_trait370));
  return impl;
} 
Trait370* Trait370_S370child1_ = newTrait370_S370child1();

int Trait175_S370child1_trait175(Class* self_) {
  S370child1* self = ((S370child1*)self_);
{
    return (self -> a);
  }
} 
Trait175* newTrait175_S370child1() {
  Trait175 (* impl) = (new Trait175());
  setVec(Trait175_v, S370child1_classId, ((void*)impl));
  ((impl -> trait175) = (& Trait175_S370child1_trait175));
  return impl;
} 
Trait175* Trait175_S370child1_ = newTrait175_S370child1();

int S371_classId = Class_genId();
struct S371{

  int id;
  S371 ():id(S371_classId){

  }
};


int S371child1_classId = Class_genId();
struct S371child1:S371{

  S371child1 (int a):a(a){
    (id = S371child1_classId);
{

    }
  }
  int a;
};


struct Trait371{

  int(*trait371)(Class*);
};

Vec* Trait371_v = newVec();

int Trait371_S371child1_trait371(Class* self_) {
  S371child1* self = ((S371child1*)self_);
{
    return (self -> a);
  }
} 
Trait371* newTrait371_S371child1() {
  Trait371 (* impl) = (new Trait371());
  setVec(Trait371_v, S371child1_classId, ((void*)impl));
  ((impl -> trait371) = (& Trait371_S371child1_trait371));
  return impl;
} 
Trait371* Trait371_S371child1_ = newTrait371_S371child1();

int Trait297_S371child1_trait297(Class* self_) {
  S371child1* self = ((S371child1*)self_);
{
    return (self -> a);
  }
} 
Trait297* newTrait297_S371child1() {
  Trait297 (* impl) = (new Trait297());
  setVec(Trait297_v, S371child1_classId, ((void*)impl));
  ((impl -> trait297) = (& Trait297_S371child1_trait297));
  return impl;
} 
Trait297* Trait297_S371child1_ = newTrait297_S371child1();

int S372_classId = Class_genId();
struct S372{

  int id;
  S372 ():id(S372_classId){

  }
};


int S372child1_classId = Class_genId();
struct S372child1:S372{

  S372child1 (int a):a(a){
    (id = S372child1_classId);
{

    }
  }
  int a;
};


struct Trait372{

  int(*trait372)(Class*);
};

Vec* Trait372_v = newVec();

int Trait372_S372child1_trait372(Class* self_) {
  S372child1* self = ((S372child1*)self_);
{
    return (self -> a);
  }
} 
Trait372* newTrait372_S372child1() {
  Trait372 (* impl) = (new Trait372());
  setVec(Trait372_v, S372child1_classId, ((void*)impl));
  ((impl -> trait372) = (& Trait372_S372child1_trait372));
  return impl;
} 
Trait372* Trait372_S372child1_ = newTrait372_S372child1();

int Trait73_S372child1_trait73(Class* self_) {
  S372child1* self = ((S372child1*)self_);
{
    return (self -> a);
  }
} 
Trait73* newTrait73_S372child1() {
  Trait73 (* impl) = (new Trait73());
  setVec(Trait73_v, S372child1_classId, ((void*)impl));
  ((impl -> trait73) = (& Trait73_S372child1_trait73));
  return impl;
} 
Trait73* Trait73_S372child1_ = newTrait73_S372child1();

int S373_classId = Class_genId();
struct S373{

  int id;
  S373 ():id(S373_classId){

  }
};


int S373child1_classId = Class_genId();
struct S373child1:S373{

  S373child1 (int a):a(a){
    (id = S373child1_classId);
{

    }
  }
  int a;
};


struct Trait373{

  int(*trait373)(Class*);
};

Vec* Trait373_v = newVec();

int Trait373_S373child1_trait373(Class* self_) {
  S373child1* self = ((S373child1*)self_);
{
    return (self -> a);
  }
} 
Trait373* newTrait373_S373child1() {
  Trait373 (* impl) = (new Trait373());
  setVec(Trait373_v, S373child1_classId, ((void*)impl));
  ((impl -> trait373) = (& Trait373_S373child1_trait373));
  return impl;
} 
Trait373* Trait373_S373child1_ = newTrait373_S373child1();

int Trait345_S373child1_trait345(Class* self_) {
  S373child1* self = ((S373child1*)self_);
{
    return (self -> a);
  }
} 
Trait345* newTrait345_S373child1() {
  Trait345 (* impl) = (new Trait345());
  setVec(Trait345_v, S373child1_classId, ((void*)impl));
  ((impl -> trait345) = (& Trait345_S373child1_trait345));
  return impl;
} 
Trait345* Trait345_S373child1_ = newTrait345_S373child1();

int S374_classId = Class_genId();
struct S374{

  int id;
  S374 ():id(S374_classId){

  }
};


int S374child1_classId = Class_genId();
struct S374child1:S374{

  S374child1 (int a):a(a){
    (id = S374child1_classId);
{

    }
  }
  int a;
};


struct Trait374{

  int(*trait374)(Class*);
};

Vec* Trait374_v = newVec();

int Trait374_S374child1_trait374(Class* self_) {
  S374child1* self = ((S374child1*)self_);
{
    return (self -> a);
  }
} 
Trait374* newTrait374_S374child1() {
  Trait374 (* impl) = (new Trait374());
  setVec(Trait374_v, S374child1_classId, ((void*)impl));
  ((impl -> trait374) = (& Trait374_S374child1_trait374));
  return impl;
} 
Trait374* Trait374_S374child1_ = newTrait374_S374child1();

int Trait369_S374child1_trait369(Class* self_) {
  S374child1* self = ((S374child1*)self_);
{
    return (self -> a);
  }
} 
Trait369* newTrait369_S374child1() {
  Trait369 (* impl) = (new Trait369());
  setVec(Trait369_v, S374child1_classId, ((void*)impl));
  ((impl -> trait369) = (& Trait369_S374child1_trait369));
  return impl;
} 
Trait369* Trait369_S374child1_ = newTrait369_S374child1();

int S375_classId = Class_genId();
struct S375{

  int id;
  S375 ():id(S375_classId){

  }
};


int S375child1_classId = Class_genId();
struct S375child1:S375{

  S375child1 (int a):a(a){
    (id = S375child1_classId);
{

    }
  }
  int a;
};


int S375child2_classId = Class_genId();
struct S375child2:S375{

  S375child2 (int a):a(a){
    (id = S375child2_classId);
{

    }
  }
  int a;
};


struct Trait375{

  int(*trait375)(Class*);
};

Vec* Trait375_v = newVec();

int Trait375_S375child1_trait375(Class* self_) {
  S375child1* self = ((S375child1*)self_);
{
    return (self -> a);
  }
} 
Trait375* newTrait375_S375child1() {
  Trait375 (* impl) = (new Trait375());
  setVec(Trait375_v, S375child1_classId, ((void*)impl));
  ((impl -> trait375) = (& Trait375_S375child1_trait375));
  return impl;
} 
Trait375* Trait375_S375child1_ = newTrait375_S375child1();

int Trait375_S375child2_trait375(Class* self_) {
  S375child2* self = ((S375child2*)self_);
{
    return (self -> a);
  }
} 
Trait375* newTrait375_S375child2() {
  Trait375 (* impl) = (new Trait375());
  setVec(Trait375_v, S375child2_classId, ((void*)impl));
  ((impl -> trait375) = (& Trait375_S375child2_trait375));
  return impl;
} 
Trait375* Trait375_S375child2_ = newTrait375_S375child2();

int Trait7_S375child1_trait7(Class* self_) {
  S375child1* self = ((S375child1*)self_);
{
    return (self -> a);
  }
} 
Trait7* newTrait7_S375child1() {
  Trait7 (* impl) = (new Trait7());
  setVec(Trait7_v, S375child1_classId, ((void*)impl));
  ((impl -> trait7) = (& Trait7_S375child1_trait7));
  return impl;
} 
Trait7* Trait7_S375child1_ = newTrait7_S375child1();

int S376_classId = Class_genId();
struct S376{

  int id;
  S376 ():id(S376_classId){

  }
};


int S376child1_classId = Class_genId();
struct S376child1:S376{

  S376child1 (int a):a(a){
    (id = S376child1_classId);
{

    }
  }
  int a;
};


int S376child2_classId = Class_genId();
struct S376child2:S376{

  S376child2 (int a):a(a){
    (id = S376child2_classId);
{

    }
  }
  int a;
};


struct Trait376{

  int(*trait376)(Class*);
};

Vec* Trait376_v = newVec();

int Trait376_S376child1_trait376(Class* self_) {
  S376child1* self = ((S376child1*)self_);
{
    return (self -> a);
  }
} 
Trait376* newTrait376_S376child1() {
  Trait376 (* impl) = (new Trait376());
  setVec(Trait376_v, S376child1_classId, ((void*)impl));
  ((impl -> trait376) = (& Trait376_S376child1_trait376));
  return impl;
} 
Trait376* Trait376_S376child1_ = newTrait376_S376child1();

int Trait133_S376child1_trait133(Class* self_) {
  S376child1* self = ((S376child1*)self_);
{
    return (self -> a);
  }
} 
Trait133* newTrait133_S376child1() {
  Trait133 (* impl) = (new Trait133());
  setVec(Trait133_v, S376child1_classId, ((void*)impl));
  ((impl -> trait133) = (& Trait133_S376child1_trait133));
  return impl;
} 
Trait133* Trait133_S376child1_ = newTrait133_S376child1();

int S377_classId = Class_genId();
struct S377{

  int id;
  S377 ():id(S377_classId){

  }
};


int S377child1_classId = Class_genId();
struct S377child1:S377{

  S377child1 (int a):a(a){
    (id = S377child1_classId);
{

    }
  }
  int a;
};


int S377child2_classId = Class_genId();
struct S377child2:S377{

  S377child2 (int a):a(a){
    (id = S377child2_classId);
{

    }
  }
  int a;
};


struct Trait377{

  int(*trait377)(Class*);
};

Vec* Trait377_v = newVec();

int Trait377_S377child1_trait377(Class* self_) {
  S377child1* self = ((S377child1*)self_);
{
    return (self -> a);
  }
} 
Trait377* newTrait377_S377child1() {
  Trait377 (* impl) = (new Trait377());
  setVec(Trait377_v, S377child1_classId, ((void*)impl));
  ((impl -> trait377) = (& Trait377_S377child1_trait377));
  return impl;
} 
Trait377* Trait377_S377child1_ = newTrait377_S377child1();

int Trait377_S377child2_trait377(Class* self_) {
  S377child2* self = ((S377child2*)self_);
{
    return (self -> a);
  }
} 
Trait377* newTrait377_S377child2() {
  Trait377 (* impl) = (new Trait377());
  setVec(Trait377_v, S377child2_classId, ((void*)impl));
  ((impl -> trait377) = (& Trait377_S377child2_trait377));
  return impl;
} 
Trait377* Trait377_S377child2_ = newTrait377_S377child2();

int Trait224_S377child1_trait224(Class* self_) {
  S377child1* self = ((S377child1*)self_);
{
    return (self -> a);
  }
} 
Trait224* newTrait224_S377child1() {
  Trait224 (* impl) = (new Trait224());
  setVec(Trait224_v, S377child1_classId, ((void*)impl));
  ((impl -> trait224) = (& Trait224_S377child1_trait224));
  return impl;
} 
Trait224* Trait224_S377child1_ = newTrait224_S377child1();

int S378_classId = Class_genId();
struct S378{

  int id;
  S378 ():id(S378_classId){

  }
};


int S378child1_classId = Class_genId();
struct S378child1:S378{

  S378child1 (int a):a(a){
    (id = S378child1_classId);
{

    }
  }
  int a;
};


int S378child2_classId = Class_genId();
struct S378child2:S378{

  S378child2 (int a):a(a){
    (id = S378child2_classId);
{

    }
  }
  int a;
};


int S378child3_classId = Class_genId();
struct S378child3:S378{

  S378child3 (int a):a(a){
    (id = S378child3_classId);
{

    }
  }
  int a;
};


struct Trait378{

  int(*trait378)(Class*);
};

Vec* Trait378_v = newVec();

int Trait378_S378child1_trait378(Class* self_) {
  S378child1* self = ((S378child1*)self_);
{
    return (self -> a);
  }
} 
Trait378* newTrait378_S378child1() {
  Trait378 (* impl) = (new Trait378());
  setVec(Trait378_v, S378child1_classId, ((void*)impl));
  ((impl -> trait378) = (& Trait378_S378child1_trait378));
  return impl;
} 
Trait378* Trait378_S378child1_ = newTrait378_S378child1();

int Trait378_S378child2_trait378(Class* self_) {
  S378child2* self = ((S378child2*)self_);
{
    return (self -> a);
  }
} 
Trait378* newTrait378_S378child2() {
  Trait378 (* impl) = (new Trait378());
  setVec(Trait378_v, S378child2_classId, ((void*)impl));
  ((impl -> trait378) = (& Trait378_S378child2_trait378));
  return impl;
} 
Trait378* Trait378_S378child2_ = newTrait378_S378child2();

int Trait378_S378child3_trait378(Class* self_) {
  S378child3* self = ((S378child3*)self_);
{
    return (self -> a);
  }
} 
Trait378* newTrait378_S378child3() {
  Trait378 (* impl) = (new Trait378());
  setVec(Trait378_v, S378child3_classId, ((void*)impl));
  ((impl -> trait378) = (& Trait378_S378child3_trait378));
  return impl;
} 
Trait378* Trait378_S378child3_ = newTrait378_S378child3();

int Trait246_S378child1_trait246(Class* self_) {
  S378child1* self = ((S378child1*)self_);
{
    return (self -> a);
  }
} 
Trait246* newTrait246_S378child1() {
  Trait246 (* impl) = (new Trait246());
  setVec(Trait246_v, S378child1_classId, ((void*)impl));
  ((impl -> trait246) = (& Trait246_S378child1_trait246));
  return impl;
} 
Trait246* Trait246_S378child1_ = newTrait246_S378child1();

int S379_classId = Class_genId();
struct S379{

  int id;
  S379 ():id(S379_classId){

  }
};


int S379child1_classId = Class_genId();
struct S379child1:S379{

  S379child1 (int a):a(a){
    (id = S379child1_classId);
{

    }
  }
  int a;
};


int S379child2_classId = Class_genId();
struct S379child2:S379{

  S379child2 (int a):a(a){
    (id = S379child2_classId);
{

    }
  }
  int a;
};


struct Trait379{

  int(*trait379)(Class*);
};

Vec* Trait379_v = newVec();

int Trait379_S379child1_trait379(Class* self_) {
  S379child1* self = ((S379child1*)self_);
{
    return (self -> a);
  }
} 
Trait379* newTrait379_S379child1() {
  Trait379 (* impl) = (new Trait379());
  setVec(Trait379_v, S379child1_classId, ((void*)impl));
  ((impl -> trait379) = (& Trait379_S379child1_trait379));
  return impl;
} 
Trait379* Trait379_S379child1_ = newTrait379_S379child1();

int Trait379_S379child2_trait379(Class* self_) {
  S379child2* self = ((S379child2*)self_);
{
    return (self -> a);
  }
} 
Trait379* newTrait379_S379child2() {
  Trait379 (* impl) = (new Trait379());
  setVec(Trait379_v, S379child2_classId, ((void*)impl));
  ((impl -> trait379) = (& Trait379_S379child2_trait379));
  return impl;
} 
Trait379* Trait379_S379child2_ = newTrait379_S379child2();

int Trait205_S379child1_trait205(Class* self_) {
  S379child1* self = ((S379child1*)self_);
{
    return (self -> a);
  }
} 
Trait205* newTrait205_S379child1() {
  Trait205 (* impl) = (new Trait205());
  setVec(Trait205_v, S379child1_classId, ((void*)impl));
  ((impl -> trait205) = (& Trait205_S379child1_trait205));
  return impl;
} 
Trait205* Trait205_S379child1_ = newTrait205_S379child1();

int S380_classId = Class_genId();
struct S380{

  int id;
  S380 ():id(S380_classId){

  }
};


int S380child1_classId = Class_genId();
struct S380child1:S380{

  S380child1 (int a):a(a){
    (id = S380child1_classId);
{

    }
  }
  int a;
};


int S380child2_classId = Class_genId();
struct S380child2:S380{

  S380child2 (int a):a(a){
    (id = S380child2_classId);
{

    }
  }
  int a;
};


int S380child3_classId = Class_genId();
struct S380child3:S380{

  S380child3 (int a):a(a){
    (id = S380child3_classId);
{

    }
  }
  int a;
};


struct Trait380{

  int(*trait380)(Class*);
};

Vec* Trait380_v = newVec();

int Trait380_S380child1_trait380(Class* self_) {
  S380child1* self = ((S380child1*)self_);
{
    return (self -> a);
  }
} 
Trait380* newTrait380_S380child1() {
  Trait380 (* impl) = (new Trait380());
  setVec(Trait380_v, S380child1_classId, ((void*)impl));
  ((impl -> trait380) = (& Trait380_S380child1_trait380));
  return impl;
} 
Trait380* Trait380_S380child1_ = newTrait380_S380child1();

int Trait380_S380child2_trait380(Class* self_) {
  S380child2* self = ((S380child2*)self_);
{
    return (self -> a);
  }
} 
Trait380* newTrait380_S380child2() {
  Trait380 (* impl) = (new Trait380());
  setVec(Trait380_v, S380child2_classId, ((void*)impl));
  ((impl -> trait380) = (& Trait380_S380child2_trait380));
  return impl;
} 
Trait380* Trait380_S380child2_ = newTrait380_S380child2();

int Trait380_S380child3_trait380(Class* self_) {
  S380child3* self = ((S380child3*)self_);
{
    return (self -> a);
  }
} 
Trait380* newTrait380_S380child3() {
  Trait380 (* impl) = (new Trait380());
  setVec(Trait380_v, S380child3_classId, ((void*)impl));
  ((impl -> trait380) = (& Trait380_S380child3_trait380));
  return impl;
} 
Trait380* Trait380_S380child3_ = newTrait380_S380child3();

int Trait249_S380child1_trait249(Class* self_) {
  S380child1* self = ((S380child1*)self_);
{
    return (self -> a);
  }
} 
Trait249* newTrait249_S380child1() {
  Trait249 (* impl) = (new Trait249());
  setVec(Trait249_v, S380child1_classId, ((void*)impl));
  ((impl -> trait249) = (& Trait249_S380child1_trait249));
  return impl;
} 
Trait249* Trait249_S380child1_ = newTrait249_S380child1();

int S381_classId = Class_genId();
struct S381{

  int id;
  S381 ():id(S381_classId){

  }
};


int S381child1_classId = Class_genId();
struct S381child1:S381{

  S381child1 (int a):a(a){
    (id = S381child1_classId);
{

    }
  }
  int a;
};


int S381child2_classId = Class_genId();
struct S381child2:S381{

  S381child2 (int a):a(a){
    (id = S381child2_classId);
{

    }
  }
  int a;
};


struct Trait381{

  int(*trait381)(Class*);
};

Vec* Trait381_v = newVec();

int Trait381_S381child1_trait381(Class* self_) {
  S381child1* self = ((S381child1*)self_);
{
    return (self -> a);
  }
} 
Trait381* newTrait381_S381child1() {
  Trait381 (* impl) = (new Trait381());
  setVec(Trait381_v, S381child1_classId, ((void*)impl));
  ((impl -> trait381) = (& Trait381_S381child1_trait381));
  return impl;
} 
Trait381* Trait381_S381child1_ = newTrait381_S381child1();

int Trait331_S381child1_trait331(Class* self_) {
  S381child1* self = ((S381child1*)self_);
{
    return (self -> a);
  }
} 
Trait331* newTrait331_S381child1() {
  Trait331 (* impl) = (new Trait331());
  setVec(Trait331_v, S381child1_classId, ((void*)impl));
  ((impl -> trait331) = (& Trait331_S381child1_trait331));
  return impl;
} 
Trait331* Trait331_S381child1_ = newTrait331_S381child1();

int S382_classId = Class_genId();
struct S382{

  int id;
  S382 ():id(S382_classId){

  }
};


int S382child1_classId = Class_genId();
struct S382child1:S382{

  S382child1 (int a):a(a){
    (id = S382child1_classId);
{

    }
  }
  int a;
};


int S382child2_classId = Class_genId();
struct S382child2:S382{

  S382child2 (int a):a(a){
    (id = S382child2_classId);
{

    }
  }
  int a;
};


int S382child3_classId = Class_genId();
struct S382child3:S382{

  S382child3 (int a):a(a){
    (id = S382child3_classId);
{

    }
  }
  int a;
};


int S382child4_classId = Class_genId();
struct S382child4:S382{

  S382child4 (int a):a(a){
    (id = S382child4_classId);
{

    }
  }
  int a;
};


int S382child5_classId = Class_genId();
struct S382child5:S382{

  S382child5 (int a):a(a){
    (id = S382child5_classId);
{

    }
  }
  int a;
};


struct Trait382{

  int(*trait382)(Class*);
};

Vec* Trait382_v = newVec();

int Trait382_S382child1_trait382(Class* self_) {
  S382child1* self = ((S382child1*)self_);
{
    return (self -> a);
  }
} 
Trait382* newTrait382_S382child1() {
  Trait382 (* impl) = (new Trait382());
  setVec(Trait382_v, S382child1_classId, ((void*)impl));
  ((impl -> trait382) = (& Trait382_S382child1_trait382));
  return impl;
} 
Trait382* Trait382_S382child1_ = newTrait382_S382child1();

int Trait382_S382child2_trait382(Class* self_) {
  S382child2* self = ((S382child2*)self_);
{
    return (self -> a);
  }
} 
Trait382* newTrait382_S382child2() {
  Trait382 (* impl) = (new Trait382());
  setVec(Trait382_v, S382child2_classId, ((void*)impl));
  ((impl -> trait382) = (& Trait382_S382child2_trait382));
  return impl;
} 
Trait382* Trait382_S382child2_ = newTrait382_S382child2();

int Trait70_S382child1_trait70(Class* self_) {
  S382child1* self = ((S382child1*)self_);
{
    return (self -> a);
  }
} 
Trait70* newTrait70_S382child1() {
  Trait70 (* impl) = (new Trait70());
  setVec(Trait70_v, S382child1_classId, ((void*)impl));
  ((impl -> trait70) = (& Trait70_S382child1_trait70));
  return impl;
} 
Trait70* Trait70_S382child1_ = newTrait70_S382child1();

int S383_classId = Class_genId();
struct S383{

  int id;
  S383 ():id(S383_classId){

  }
};


int S383child1_classId = Class_genId();
struct S383child1:S383{

  S383child1 (int a):a(a){
    (id = S383child1_classId);
{

    }
  }
  int a;
};


int S383child2_classId = Class_genId();
struct S383child2:S383{

  S383child2 (int a):a(a){
    (id = S383child2_classId);
{

    }
  }
  int a;
};


int S383child3_classId = Class_genId();
struct S383child3:S383{

  S383child3 (int a):a(a){
    (id = S383child3_classId);
{

    }
  }
  int a;
};


struct Trait383{

  int(*trait383)(Class*);
};

Vec* Trait383_v = newVec();

int Trait383_S383child1_trait383(Class* self_) {
  S383child1* self = ((S383child1*)self_);
{
    return (self -> a);
  }
} 
Trait383* newTrait383_S383child1() {
  Trait383 (* impl) = (new Trait383());
  setVec(Trait383_v, S383child1_classId, ((void*)impl));
  ((impl -> trait383) = (& Trait383_S383child1_trait383));
  return impl;
} 
Trait383* Trait383_S383child1_ = newTrait383_S383child1();

int Trait331_S383child1_trait331(Class* self_) {
  S383child1* self = ((S383child1*)self_);
{
    return (self -> a);
  }
} 
Trait331* newTrait331_S383child1() {
  Trait331 (* impl) = (new Trait331());
  setVec(Trait331_v, S383child1_classId, ((void*)impl));
  ((impl -> trait331) = (& Trait331_S383child1_trait331));
  return impl;
} 
Trait331* Trait331_S383child1_ = newTrait331_S383child1();

int S384_classId = Class_genId();
struct S384{

  int id;
  S384 ():id(S384_classId){

  }
};


int S384child1_classId = Class_genId();
struct S384child1:S384{

  S384child1 (int a):a(a){
    (id = S384child1_classId);
{

    }
  }
  int a;
};


int S384child2_classId = Class_genId();
struct S384child2:S384{

  S384child2 (int a):a(a){
    (id = S384child2_classId);
{

    }
  }
  int a;
};


struct Trait384{

  int(*trait384)(Class*);
};

Vec* Trait384_v = newVec();

int Trait384_S384child1_trait384(Class* self_) {
  S384child1* self = ((S384child1*)self_);
{
    return (self -> a);
  }
} 
Trait384* newTrait384_S384child1() {
  Trait384 (* impl) = (new Trait384());
  setVec(Trait384_v, S384child1_classId, ((void*)impl));
  ((impl -> trait384) = (& Trait384_S384child1_trait384));
  return impl;
} 
Trait384* Trait384_S384child1_ = newTrait384_S384child1();

int Trait384_S384child2_trait384(Class* self_) {
  S384child2* self = ((S384child2*)self_);
{
    return (self -> a);
  }
} 
Trait384* newTrait384_S384child2() {
  Trait384 (* impl) = (new Trait384());
  setVec(Trait384_v, S384child2_classId, ((void*)impl));
  ((impl -> trait384) = (& Trait384_S384child2_trait384));
  return impl;
} 
Trait384* Trait384_S384child2_ = newTrait384_S384child2();

int Trait26_S384child1_trait26(Class* self_) {
  S384child1* self = ((S384child1*)self_);
{
    return (self -> a);
  }
} 
Trait26* newTrait26_S384child1() {
  Trait26 (* impl) = (new Trait26());
  setVec(Trait26_v, S384child1_classId, ((void*)impl));
  ((impl -> trait26) = (& Trait26_S384child1_trait26));
  return impl;
} 
Trait26* Trait26_S384child1_ = newTrait26_S384child1();

int S385_classId = Class_genId();
struct S385{

  int id;
  S385 ():id(S385_classId){

  }
};


int S385child1_classId = Class_genId();
struct S385child1:S385{

  S385child1 (int a):a(a){
    (id = S385child1_classId);
{

    }
  }
  int a;
};


int S385child2_classId = Class_genId();
struct S385child2:S385{

  S385child2 (int a):a(a){
    (id = S385child2_classId);
{

    }
  }
  int a;
};


struct Trait385{

  int(*trait385)(Class*);
};

Vec* Trait385_v = newVec();

int Trait385_S385child1_trait385(Class* self_) {
  S385child1* self = ((S385child1*)self_);
{
    return (self -> a);
  }
} 
Trait385* newTrait385_S385child1() {
  Trait385 (* impl) = (new Trait385());
  setVec(Trait385_v, S385child1_classId, ((void*)impl));
  ((impl -> trait385) = (& Trait385_S385child1_trait385));
  return impl;
} 
Trait385* Trait385_S385child1_ = newTrait385_S385child1();

int Trait130_S385child1_trait130(Class* self_) {
  S385child1* self = ((S385child1*)self_);
{
    return (self -> a);
  }
} 
Trait130* newTrait130_S385child1() {
  Trait130 (* impl) = (new Trait130());
  setVec(Trait130_v, S385child1_classId, ((void*)impl));
  ((impl -> trait130) = (& Trait130_S385child1_trait130));
  return impl;
} 
Trait130* Trait130_S385child1_ = newTrait130_S385child1();

int S386_classId = Class_genId();
struct S386{

  int id;
  S386 ():id(S386_classId){

  }
};


int S386child1_classId = Class_genId();
struct S386child1:S386{

  S386child1 (int a):a(a){
    (id = S386child1_classId);
{

    }
  }
  int a;
};


int S386child2_classId = Class_genId();
struct S386child2:S386{

  S386child2 (int a):a(a){
    (id = S386child2_classId);
{

    }
  }
  int a;
};


int S386child3_classId = Class_genId();
struct S386child3:S386{

  S386child3 (int a):a(a){
    (id = S386child3_classId);
{

    }
  }
  int a;
};


struct Trait386{

  int(*trait386)(Class*);
};

Vec* Trait386_v = newVec();

int Trait386_S386child1_trait386(Class* self_) {
  S386child1* self = ((S386child1*)self_);
{
    return (self -> a);
  }
} 
Trait386* newTrait386_S386child1() {
  Trait386 (* impl) = (new Trait386());
  setVec(Trait386_v, S386child1_classId, ((void*)impl));
  ((impl -> trait386) = (& Trait386_S386child1_trait386));
  return impl;
} 
Trait386* Trait386_S386child1_ = newTrait386_S386child1();

int Trait386_S386child2_trait386(Class* self_) {
  S386child2* self = ((S386child2*)self_);
{
    return (self -> a);
  }
} 
Trait386* newTrait386_S386child2() {
  Trait386 (* impl) = (new Trait386());
  setVec(Trait386_v, S386child2_classId, ((void*)impl));
  ((impl -> trait386) = (& Trait386_S386child2_trait386));
  return impl;
} 
Trait386* Trait386_S386child2_ = newTrait386_S386child2();

int Trait63_S386child1_trait63(Class* self_) {
  S386child1* self = ((S386child1*)self_);
{
    return (self -> a);
  }
} 
Trait63* newTrait63_S386child1() {
  Trait63 (* impl) = (new Trait63());
  setVec(Trait63_v, S386child1_classId, ((void*)impl));
  ((impl -> trait63) = (& Trait63_S386child1_trait63));
  return impl;
} 
Trait63* Trait63_S386child1_ = newTrait63_S386child1();

int S387_classId = Class_genId();
struct S387{

  int id;
  S387 ():id(S387_classId){

  }
};


int S387child1_classId = Class_genId();
struct S387child1:S387{

  S387child1 (int a):a(a){
    (id = S387child1_classId);
{

    }
  }
  int a;
};


int S387child2_classId = Class_genId();
struct S387child2:S387{

  S387child2 (int a):a(a){
    (id = S387child2_classId);
{

    }
  }
  int a;
};


struct Trait387{

  int(*trait387)(Class*);
};

Vec* Trait387_v = newVec();

int Trait387_S387child1_trait387(Class* self_) {
  S387child1* self = ((S387child1*)self_);
{
    return (self -> a);
  }
} 
Trait387* newTrait387_S387child1() {
  Trait387 (* impl) = (new Trait387());
  setVec(Trait387_v, S387child1_classId, ((void*)impl));
  ((impl -> trait387) = (& Trait387_S387child1_trait387));
  return impl;
} 
Trait387* Trait387_S387child1_ = newTrait387_S387child1();

int Trait387_S387child2_trait387(Class* self_) {
  S387child2* self = ((S387child2*)self_);
{
    return (self -> a);
  }
} 
Trait387* newTrait387_S387child2() {
  Trait387 (* impl) = (new Trait387());
  setVec(Trait387_v, S387child2_classId, ((void*)impl));
  ((impl -> trait387) = (& Trait387_S387child2_trait387));
  return impl;
} 
Trait387* Trait387_S387child2_ = newTrait387_S387child2();

int Trait368_S387child1_trait368(Class* self_) {
  S387child1* self = ((S387child1*)self_);
{
    return (self -> a);
  }
} 
Trait368* newTrait368_S387child1() {
  Trait368 (* impl) = (new Trait368());
  setVec(Trait368_v, S387child1_classId, ((void*)impl));
  ((impl -> trait368) = (& Trait368_S387child1_trait368));
  return impl;
} 
Trait368* Trait368_S387child1_ = newTrait368_S387child1();

int S388_classId = Class_genId();
struct S388{

  int id;
  S388 ():id(S388_classId){

  }
};


int S388child1_classId = Class_genId();
struct S388child1:S388{

  S388child1 (int a):a(a){
    (id = S388child1_classId);
{

    }
  }
  int a;
};


int S388child2_classId = Class_genId();
struct S388child2:S388{

  S388child2 (int a):a(a){
    (id = S388child2_classId);
{

    }
  }
  int a;
};


int S388child3_classId = Class_genId();
struct S388child3:S388{

  S388child3 (int a):a(a){
    (id = S388child3_classId);
{

    }
  }
  int a;
};


struct Trait388{

  int(*trait388)(Class*);
};

Vec* Trait388_v = newVec();

int Trait388_S388child1_trait388(Class* self_) {
  S388child1* self = ((S388child1*)self_);
{
    return (self -> a);
  }
} 
Trait388* newTrait388_S388child1() {
  Trait388 (* impl) = (new Trait388());
  setVec(Trait388_v, S388child1_classId, ((void*)impl));
  ((impl -> trait388) = (& Trait388_S388child1_trait388));
  return impl;
} 
Trait388* Trait388_S388child1_ = newTrait388_S388child1();

int Trait53_S388child1_trait53(Class* self_) {
  S388child1* self = ((S388child1*)self_);
{
    return (self -> a);
  }
} 
Trait53* newTrait53_S388child1() {
  Trait53 (* impl) = (new Trait53());
  setVec(Trait53_v, S388child1_classId, ((void*)impl));
  ((impl -> trait53) = (& Trait53_S388child1_trait53));
  return impl;
} 
Trait53* Trait53_S388child1_ = newTrait53_S388child1();

int S389_classId = Class_genId();
struct S389{

  int id;
  S389 ():id(S389_classId){

  }
};


int S389child1_classId = Class_genId();
struct S389child1:S389{

  S389child1 (int a):a(a){
    (id = S389child1_classId);
{

    }
  }
  int a;
};


int S389child2_classId = Class_genId();
struct S389child2:S389{

  S389child2 (int a):a(a){
    (id = S389child2_classId);
{

    }
  }
  int a;
};


struct Trait389{

  int(*trait389)(Class*);
};

Vec* Trait389_v = newVec();

int Trait389_S389child1_trait389(Class* self_) {
  S389child1* self = ((S389child1*)self_);
{
    return (self -> a);
  }
} 
Trait389* newTrait389_S389child1() {
  Trait389 (* impl) = (new Trait389());
  setVec(Trait389_v, S389child1_classId, ((void*)impl));
  ((impl -> trait389) = (& Trait389_S389child1_trait389));
  return impl;
} 
Trait389* Trait389_S389child1_ = newTrait389_S389child1();

int Trait185_S389child1_trait185(Class* self_) {
  S389child1* self = ((S389child1*)self_);
{
    return (self -> a);
  }
} 
Trait185* newTrait185_S389child1() {
  Trait185 (* impl) = (new Trait185());
  setVec(Trait185_v, S389child1_classId, ((void*)impl));
  ((impl -> trait185) = (& Trait185_S389child1_trait185));
  return impl;
} 
Trait185* Trait185_S389child1_ = newTrait185_S389child1();

int S390_classId = Class_genId();
struct S390{

  int id;
  S390 ():id(S390_classId){

  }
};


int S390child1_classId = Class_genId();
struct S390child1:S390{

  S390child1 (int a):a(a){
    (id = S390child1_classId);
{

    }
  }
  int a;
};


int S390child2_classId = Class_genId();
struct S390child2:S390{

  S390child2 (int a):a(a){
    (id = S390child2_classId);
{

    }
  }
  int a;
};


int S390child3_classId = Class_genId();
struct S390child3:S390{

  S390child3 (int a):a(a){
    (id = S390child3_classId);
{

    }
  }
  int a;
};


int S390child4_classId = Class_genId();
struct S390child4:S390{

  S390child4 (int a):a(a){
    (id = S390child4_classId);
{

    }
  }
  int a;
};


struct Trait390{

  int(*trait390)(Class*);
};

Vec* Trait390_v = newVec();

int Trait390_S390child1_trait390(Class* self_) {
  S390child1* self = ((S390child1*)self_);
{
    return (self -> a);
  }
} 
Trait390* newTrait390_S390child1() {
  Trait390 (* impl) = (new Trait390());
  setVec(Trait390_v, S390child1_classId, ((void*)impl));
  ((impl -> trait390) = (& Trait390_S390child1_trait390));
  return impl;
} 
Trait390* Trait390_S390child1_ = newTrait390_S390child1();

int Trait274_S390child1_trait274(Class* self_) {
  S390child1* self = ((S390child1*)self_);
{
    return (self -> a);
  }
} 
Trait274* newTrait274_S390child1() {
  Trait274 (* impl) = (new Trait274());
  setVec(Trait274_v, S390child1_classId, ((void*)impl));
  ((impl -> trait274) = (& Trait274_S390child1_trait274));
  return impl;
} 
Trait274* Trait274_S390child1_ = newTrait274_S390child1();

int S391_classId = Class_genId();
struct S391{

  int id;
  S391 ():id(S391_classId){

  }
};


int S391child1_classId = Class_genId();
struct S391child1:S391{

  S391child1 (int a):a(a){
    (id = S391child1_classId);
{

    }
  }
  int a;
};


int S391child2_classId = Class_genId();
struct S391child2:S391{

  S391child2 (int a):a(a){
    (id = S391child2_classId);
{

    }
  }
  int a;
};


int S391child3_classId = Class_genId();
struct S391child3:S391{

  S391child3 (int a):a(a){
    (id = S391child3_classId);
{

    }
  }
  int a;
};


struct Trait391{

  int(*trait391)(Class*);
};

Vec* Trait391_v = newVec();

int Trait391_S391child1_trait391(Class* self_) {
  S391child1* self = ((S391child1*)self_);
{
    return (self -> a);
  }
} 
Trait391* newTrait391_S391child1() {
  Trait391 (* impl) = (new Trait391());
  setVec(Trait391_v, S391child1_classId, ((void*)impl));
  ((impl -> trait391) = (& Trait391_S391child1_trait391));
  return impl;
} 
Trait391* Trait391_S391child1_ = newTrait391_S391child1();

int Trait84_S391child1_trait84(Class* self_) {
  S391child1* self = ((S391child1*)self_);
{
    return (self -> a);
  }
} 
Trait84* newTrait84_S391child1() {
  Trait84 (* impl) = (new Trait84());
  setVec(Trait84_v, S391child1_classId, ((void*)impl));
  ((impl -> trait84) = (& Trait84_S391child1_trait84));
  return impl;
} 
Trait84* Trait84_S391child1_ = newTrait84_S391child1();

int S392_classId = Class_genId();
struct S392{

  int id;
  S392 ():id(S392_classId){

  }
};


int S392child1_classId = Class_genId();
struct S392child1:S392{

  S392child1 (int a):a(a){
    (id = S392child1_classId);
{

    }
  }
  int a;
};


int S392child2_classId = Class_genId();
struct S392child2:S392{

  S392child2 (int a):a(a){
    (id = S392child2_classId);
{

    }
  }
  int a;
};


struct Trait392{

  int(*trait392)(Class*);
};

Vec* Trait392_v = newVec();

int Trait392_S392child1_trait392(Class* self_) {
  S392child1* self = ((S392child1*)self_);
{
    return (self -> a);
  }
} 
Trait392* newTrait392_S392child1() {
  Trait392 (* impl) = (new Trait392());
  setVec(Trait392_v, S392child1_classId, ((void*)impl));
  ((impl -> trait392) = (& Trait392_S392child1_trait392));
  return impl;
} 
Trait392* Trait392_S392child1_ = newTrait392_S392child1();

int Trait392_S392child2_trait392(Class* self_) {
  S392child2* self = ((S392child2*)self_);
{
    return (self -> a);
  }
} 
Trait392* newTrait392_S392child2() {
  Trait392 (* impl) = (new Trait392());
  setVec(Trait392_v, S392child2_classId, ((void*)impl));
  ((impl -> trait392) = (& Trait392_S392child2_trait392));
  return impl;
} 
Trait392* Trait392_S392child2_ = newTrait392_S392child2();

int Trait108_S392child1_trait108(Class* self_) {
  S392child1* self = ((S392child1*)self_);
{
    return (self -> a);
  }
} 
Trait108* newTrait108_S392child1() {
  Trait108 (* impl) = (new Trait108());
  setVec(Trait108_v, S392child1_classId, ((void*)impl));
  ((impl -> trait108) = (& Trait108_S392child1_trait108));
  return impl;
} 
Trait108* Trait108_S392child1_ = newTrait108_S392child1();

int S393_classId = Class_genId();
struct S393{

  int id;
  S393 ():id(S393_classId){

  }
};


int S393child1_classId = Class_genId();
struct S393child1:S393{

  S393child1 (int a):a(a){
    (id = S393child1_classId);
{

    }
  }
  int a;
};


struct Trait393{

  int(*trait393)(Class*);
};

Vec* Trait393_v = newVec();

int Trait393_S393child1_trait393(Class* self_) {
  S393child1* self = ((S393child1*)self_);
{
    return (self -> a);
  }
} 
Trait393* newTrait393_S393child1() {
  Trait393 (* impl) = (new Trait393());
  setVec(Trait393_v, S393child1_classId, ((void*)impl));
  ((impl -> trait393) = (& Trait393_S393child1_trait393));
  return impl;
} 
Trait393* Trait393_S393child1_ = newTrait393_S393child1();

int Trait173_S393child1_trait173(Class* self_) {
  S393child1* self = ((S393child1*)self_);
{
    return (self -> a);
  }
} 
Trait173* newTrait173_S393child1() {
  Trait173 (* impl) = (new Trait173());
  setVec(Trait173_v, S393child1_classId, ((void*)impl));
  ((impl -> trait173) = (& Trait173_S393child1_trait173));
  return impl;
} 
Trait173* Trait173_S393child1_ = newTrait173_S393child1();

int S394_classId = Class_genId();
struct S394{

  int id;
  S394 ():id(S394_classId){

  }
};


int S394child1_classId = Class_genId();
struct S394child1:S394{

  S394child1 (int a):a(a){
    (id = S394child1_classId);
{

    }
  }
  int a;
};


int S394child2_classId = Class_genId();
struct S394child2:S394{

  S394child2 (int a):a(a){
    (id = S394child2_classId);
{

    }
  }
  int a;
};


int S394child3_classId = Class_genId();
struct S394child3:S394{

  S394child3 (int a):a(a){
    (id = S394child3_classId);
{

    }
  }
  int a;
};


struct Trait394{

  int(*trait394)(Class*);
};

Vec* Trait394_v = newVec();

int Trait394_S394child1_trait394(Class* self_) {
  S394child1* self = ((S394child1*)self_);
{
    return (self -> a);
  }
} 
Trait394* newTrait394_S394child1() {
  Trait394 (* impl) = (new Trait394());
  setVec(Trait394_v, S394child1_classId, ((void*)impl));
  ((impl -> trait394) = (& Trait394_S394child1_trait394));
  return impl;
} 
Trait394* Trait394_S394child1_ = newTrait394_S394child1();

int Trait355_S394child1_trait355(Class* self_) {
  S394child1* self = ((S394child1*)self_);
{
    return (self -> a);
  }
} 
Trait355* newTrait355_S394child1() {
  Trait355 (* impl) = (new Trait355());
  setVec(Trait355_v, S394child1_classId, ((void*)impl));
  ((impl -> trait355) = (& Trait355_S394child1_trait355));
  return impl;
} 
Trait355* Trait355_S394child1_ = newTrait355_S394child1();

int S395_classId = Class_genId();
struct S395{

  int id;
  S395 ():id(S395_classId){

  }
};


int S395child1_classId = Class_genId();
struct S395child1:S395{

  S395child1 (int a):a(a){
    (id = S395child1_classId);
{

    }
  }
  int a;
};


int S395child2_classId = Class_genId();
struct S395child2:S395{

  S395child2 (int a):a(a){
    (id = S395child2_classId);
{

    }
  }
  int a;
};


struct Trait395{

  int(*trait395)(Class*);
};

Vec* Trait395_v = newVec();

int Trait395_S395child1_trait395(Class* self_) {
  S395child1* self = ((S395child1*)self_);
{
    return (self -> a);
  }
} 
Trait395* newTrait395_S395child1() {
  Trait395 (* impl) = (new Trait395());
  setVec(Trait395_v, S395child1_classId, ((void*)impl));
  ((impl -> trait395) = (& Trait395_S395child1_trait395));
  return impl;
} 
Trait395* Trait395_S395child1_ = newTrait395_S395child1();

int Trait395_S395child2_trait395(Class* self_) {
  S395child2* self = ((S395child2*)self_);
{
    return (self -> a);
  }
} 
Trait395* newTrait395_S395child2() {
  Trait395 (* impl) = (new Trait395());
  setVec(Trait395_v, S395child2_classId, ((void*)impl));
  ((impl -> trait395) = (& Trait395_S395child2_trait395));
  return impl;
} 
Trait395* Trait395_S395child2_ = newTrait395_S395child2();

int Trait57_S395child1_trait57(Class* self_) {
  S395child1* self = ((S395child1*)self_);
{
    return (self -> a);
  }
} 
Trait57* newTrait57_S395child1() {
  Trait57 (* impl) = (new Trait57());
  setVec(Trait57_v, S395child1_classId, ((void*)impl));
  ((impl -> trait57) = (& Trait57_S395child1_trait57));
  return impl;
} 
Trait57* Trait57_S395child1_ = newTrait57_S395child1();

int S396_classId = Class_genId();
struct S396{

  int id;
  S396 ():id(S396_classId){

  }
};


int S396child1_classId = Class_genId();
struct S396child1:S396{

  S396child1 (int a):a(a){
    (id = S396child1_classId);
{

    }
  }
  int a;
};


int S396child2_classId = Class_genId();
struct S396child2:S396{

  S396child2 (int a):a(a){
    (id = S396child2_classId);
{

    }
  }
  int a;
};


int S396child3_classId = Class_genId();
struct S396child3:S396{

  S396child3 (int a):a(a){
    (id = S396child3_classId);
{

    }
  }
  int a;
};


struct Trait396{

  int(*trait396)(Class*);
};

Vec* Trait396_v = newVec();

int Trait396_S396child1_trait396(Class* self_) {
  S396child1* self = ((S396child1*)self_);
{
    return (self -> a);
  }
} 
Trait396* newTrait396_S396child1() {
  Trait396 (* impl) = (new Trait396());
  setVec(Trait396_v, S396child1_classId, ((void*)impl));
  ((impl -> trait396) = (& Trait396_S396child1_trait396));
  return impl;
} 
Trait396* Trait396_S396child1_ = newTrait396_S396child1();

int Trait396_S396child2_trait396(Class* self_) {
  S396child2* self = ((S396child2*)self_);
{
    return (self -> a);
  }
} 
Trait396* newTrait396_S396child2() {
  Trait396 (* impl) = (new Trait396());
  setVec(Trait396_v, S396child2_classId, ((void*)impl));
  ((impl -> trait396) = (& Trait396_S396child2_trait396));
  return impl;
} 
Trait396* Trait396_S396child2_ = newTrait396_S396child2();

int Trait133_S396child1_trait133(Class* self_) {
  S396child1* self = ((S396child1*)self_);
{
    return (self -> a);
  }
} 
Trait133* newTrait133_S396child1() {
  Trait133 (* impl) = (new Trait133());
  setVec(Trait133_v, S396child1_classId, ((void*)impl));
  ((impl -> trait133) = (& Trait133_S396child1_trait133));
  return impl;
} 
Trait133* Trait133_S396child1_ = newTrait133_S396child1();

int S397_classId = Class_genId();
struct S397{

  int id;
  S397 ():id(S397_classId){

  }
};


int S397child1_classId = Class_genId();
struct S397child1:S397{

  S397child1 (int a):a(a){
    (id = S397child1_classId);
{

    }
  }
  int a;
};


struct Trait397{

  int(*trait397)(Class*);
};

Vec* Trait397_v = newVec();

int Trait397_S397child1_trait397(Class* self_) {
  S397child1* self = ((S397child1*)self_);
{
    return (self -> a);
  }
} 
Trait397* newTrait397_S397child1() {
  Trait397 (* impl) = (new Trait397());
  setVec(Trait397_v, S397child1_classId, ((void*)impl));
  ((impl -> trait397) = (& Trait397_S397child1_trait397));
  return impl;
} 
Trait397* Trait397_S397child1_ = newTrait397_S397child1();

int Trait371_S397child1_trait371(Class* self_) {
  S397child1* self = ((S397child1*)self_);
{
    return (self -> a);
  }
} 
Trait371* newTrait371_S397child1() {
  Trait371 (* impl) = (new Trait371());
  setVec(Trait371_v, S397child1_classId, ((void*)impl));
  ((impl -> trait371) = (& Trait371_S397child1_trait371));
  return impl;
} 
Trait371* Trait371_S397child1_ = newTrait371_S397child1();

int S398_classId = Class_genId();
struct S398{

  int id;
  S398 ():id(S398_classId){

  }
};


int S398child1_classId = Class_genId();
struct S398child1:S398{

  S398child1 (int a):a(a){
    (id = S398child1_classId);
{

    }
  }
  int a;
};


int S398child2_classId = Class_genId();
struct S398child2:S398{

  S398child2 (int a):a(a){
    (id = S398child2_classId);
{

    }
  }
  int a;
};


int S398child3_classId = Class_genId();
struct S398child3:S398{

  S398child3 (int a):a(a){
    (id = S398child3_classId);
{

    }
  }
  int a;
};


int S398child4_classId = Class_genId();
struct S398child4:S398{

  S398child4 (int a):a(a){
    (id = S398child4_classId);
{

    }
  }
  int a;
};


struct Trait398{

  int(*trait398)(Class*);
};

Vec* Trait398_v = newVec();

int Trait398_S398child1_trait398(Class* self_) {
  S398child1* self = ((S398child1*)self_);
{
    return (self -> a);
  }
} 
Trait398* newTrait398_S398child1() {
  Trait398 (* impl) = (new Trait398());
  setVec(Trait398_v, S398child1_classId, ((void*)impl));
  ((impl -> trait398) = (& Trait398_S398child1_trait398));
  return impl;
} 
Trait398* Trait398_S398child1_ = newTrait398_S398child1();

int Trait398_S398child2_trait398(Class* self_) {
  S398child2* self = ((S398child2*)self_);
{
    return (self -> a);
  }
} 
Trait398* newTrait398_S398child2() {
  Trait398 (* impl) = (new Trait398());
  setVec(Trait398_v, S398child2_classId, ((void*)impl));
  ((impl -> trait398) = (& Trait398_S398child2_trait398));
  return impl;
} 
Trait398* Trait398_S398child2_ = newTrait398_S398child2();

int Trait296_S398child1_trait296(Class* self_) {
  S398child1* self = ((S398child1*)self_);
{
    return (self -> a);
  }
} 
Trait296* newTrait296_S398child1() {
  Trait296 (* impl) = (new Trait296());
  setVec(Trait296_v, S398child1_classId, ((void*)impl));
  ((impl -> trait296) = (& Trait296_S398child1_trait296));
  return impl;
} 
Trait296* Trait296_S398child1_ = newTrait296_S398child1();

int S399_classId = Class_genId();
struct S399{

  int id;
  S399 ():id(S399_classId){

  }
};


int S399child1_classId = Class_genId();
struct S399child1:S399{

  S399child1 (int a):a(a){
    (id = S399child1_classId);
{

    }
  }
  int a;
};


int S399child2_classId = Class_genId();
struct S399child2:S399{

  S399child2 (int a):a(a){
    (id = S399child2_classId);
{

    }
  }
  int a;
};


struct Trait399{

  int(*trait399)(Class*);
};

Vec* Trait399_v = newVec();

int Trait399_S399child1_trait399(Class* self_) {
  S399child1* self = ((S399child1*)self_);
{
    return (self -> a);
  }
} 
Trait399* newTrait399_S399child1() {
  Trait399 (* impl) = (new Trait399());
  setVec(Trait399_v, S399child1_classId, ((void*)impl));
  ((impl -> trait399) = (& Trait399_S399child1_trait399));
  return impl;
} 
Trait399* Trait399_S399child1_ = newTrait399_S399child1();

int Trait399_S399child2_trait399(Class* self_) {
  S399child2* self = ((S399child2*)self_);
{
    return (self -> a);
  }
} 
Trait399* newTrait399_S399child2() {
  Trait399 (* impl) = (new Trait399());
  setVec(Trait399_v, S399child2_classId, ((void*)impl));
  ((impl -> trait399) = (& Trait399_S399child2_trait399));
  return impl;
} 
Trait399* Trait399_S399child2_ = newTrait399_S399child2();

int Trait380_S399child1_trait380(Class* self_) {
  S399child1* self = ((S399child1*)self_);
{
    return (self -> a);
  }
} 
Trait380* newTrait380_S399child1() {
  Trait380 (* impl) = (new Trait380());
  setVec(Trait380_v, S399child1_classId, ((void*)impl));
  ((impl -> trait380) = (& Trait380_S399child1_trait380));
  return impl;
} 
Trait380* Trait380_S399child1_ = newTrait380_S399child1();

int S400_classId = Class_genId();
struct S400{

  int id;
  S400 ():id(S400_classId){

  }
};


int S400child1_classId = Class_genId();
struct S400child1:S400{

  S400child1 (int a):a(a){
    (id = S400child1_classId);
{

    }
  }
  int a;
};


int S400child2_classId = Class_genId();
struct S400child2:S400{

  S400child2 (int a):a(a){
    (id = S400child2_classId);
{

    }
  }
  int a;
};


int S400child3_classId = Class_genId();
struct S400child3:S400{

  S400child3 (int a):a(a){
    (id = S400child3_classId);
{

    }
  }
  int a;
};


struct Trait400{

  int(*trait400)(Class*);
};

Vec* Trait400_v = newVec();

int Trait400_S400child1_trait400(Class* self_) {
  S400child1* self = ((S400child1*)self_);
{
    return (self -> a);
  }
} 
Trait400* newTrait400_S400child1() {
  Trait400 (* impl) = (new Trait400());
  setVec(Trait400_v, S400child1_classId, ((void*)impl));
  ((impl -> trait400) = (& Trait400_S400child1_trait400));
  return impl;
} 
Trait400* Trait400_S400child1_ = newTrait400_S400child1();

int Trait400_S400child2_trait400(Class* self_) {
  S400child2* self = ((S400child2*)self_);
{
    return (self -> a);
  }
} 
Trait400* newTrait400_S400child2() {
  Trait400 (* impl) = (new Trait400());
  setVec(Trait400_v, S400child2_classId, ((void*)impl));
  ((impl -> trait400) = (& Trait400_S400child2_trait400));
  return impl;
} 
Trait400* Trait400_S400child2_ = newTrait400_S400child2();

int Trait12_S400child1_trait12(Class* self_) {
  S400child1* self = ((S400child1*)self_);
{
    return (self -> a);
  }
} 
Trait12* newTrait12_S400child1() {
  Trait12 (* impl) = (new Trait12());
  setVec(Trait12_v, S400child1_classId, ((void*)impl));
  ((impl -> trait12) = (& Trait12_S400child1_trait12));
  return impl;
} 
Trait12* Trait12_S400child1_ = newTrait12_S400child1();

int S401_classId = Class_genId();
struct S401{

  int id;
  S401 ():id(S401_classId){

  }
};


int S401child1_classId = Class_genId();
struct S401child1:S401{

  S401child1 (int a):a(a){
    (id = S401child1_classId);
{

    }
  }
  int a;
};


int S401child2_classId = Class_genId();
struct S401child2:S401{

  S401child2 (int a):a(a){
    (id = S401child2_classId);
{

    }
  }
  int a;
};


struct Trait401{

  int(*trait401)(Class*);
};

Vec* Trait401_v = newVec();

int Trait401_S401child1_trait401(Class* self_) {
  S401child1* self = ((S401child1*)self_);
{
    return (self -> a);
  }
} 
Trait401* newTrait401_S401child1() {
  Trait401 (* impl) = (new Trait401());
  setVec(Trait401_v, S401child1_classId, ((void*)impl));
  ((impl -> trait401) = (& Trait401_S401child1_trait401));
  return impl;
} 
Trait401* Trait401_S401child1_ = newTrait401_S401child1();

int Trait401_S401child2_trait401(Class* self_) {
  S401child2* self = ((S401child2*)self_);
{
    return (self -> a);
  }
} 
Trait401* newTrait401_S401child2() {
  Trait401 (* impl) = (new Trait401());
  setVec(Trait401_v, S401child2_classId, ((void*)impl));
  ((impl -> trait401) = (& Trait401_S401child2_trait401));
  return impl;
} 
Trait401* Trait401_S401child2_ = newTrait401_S401child2();

int Trait99_S401child1_trait99(Class* self_) {
  S401child1* self = ((S401child1*)self_);
{
    return (self -> a);
  }
} 
Trait99* newTrait99_S401child1() {
  Trait99 (* impl) = (new Trait99());
  setVec(Trait99_v, S401child1_classId, ((void*)impl));
  ((impl -> trait99) = (& Trait99_S401child1_trait99));
  return impl;
} 
Trait99* Trait99_S401child1_ = newTrait99_S401child1();

int S402_classId = Class_genId();
struct S402{

  int id;
  S402 ():id(S402_classId){

  }
};


int S402child1_classId = Class_genId();
struct S402child1:S402{

  S402child1 (int a):a(a){
    (id = S402child1_classId);
{

    }
  }
  int a;
};


int S402child2_classId = Class_genId();
struct S402child2:S402{

  S402child2 (int a):a(a){
    (id = S402child2_classId);
{

    }
  }
  int a;
};


struct Trait402{

  int(*trait402)(Class*);
};

Vec* Trait402_v = newVec();

int Trait402_S402child1_trait402(Class* self_) {
  S402child1* self = ((S402child1*)self_);
{
    return (self -> a);
  }
} 
Trait402* newTrait402_S402child1() {
  Trait402 (* impl) = (new Trait402());
  setVec(Trait402_v, S402child1_classId, ((void*)impl));
  ((impl -> trait402) = (& Trait402_S402child1_trait402));
  return impl;
} 
Trait402* Trait402_S402child1_ = newTrait402_S402child1();

int Trait99_S402child1_trait99(Class* self_) {
  S402child1* self = ((S402child1*)self_);
{
    return (self -> a);
  }
} 
Trait99* newTrait99_S402child1() {
  Trait99 (* impl) = (new Trait99());
  setVec(Trait99_v, S402child1_classId, ((void*)impl));
  ((impl -> trait99) = (& Trait99_S402child1_trait99));
  return impl;
} 
Trait99* Trait99_S402child1_ = newTrait99_S402child1();

int S403_classId = Class_genId();
struct S403{

  int id;
  S403 ():id(S403_classId){

  }
};


int S403child1_classId = Class_genId();
struct S403child1:S403{

  S403child1 (int a):a(a){
    (id = S403child1_classId);
{

    }
  }
  int a;
};


int S403child2_classId = Class_genId();
struct S403child2:S403{

  S403child2 (int a):a(a){
    (id = S403child2_classId);
{

    }
  }
  int a;
};


int S403child3_classId = Class_genId();
struct S403child3:S403{

  S403child3 (int a):a(a){
    (id = S403child3_classId);
{

    }
  }
  int a;
};


struct Trait403{

  int(*trait403)(Class*);
};

Vec* Trait403_v = newVec();

int Trait403_S403child1_trait403(Class* self_) {
  S403child1* self = ((S403child1*)self_);
{
    return (self -> a);
  }
} 
Trait403* newTrait403_S403child1() {
  Trait403 (* impl) = (new Trait403());
  setVec(Trait403_v, S403child1_classId, ((void*)impl));
  ((impl -> trait403) = (& Trait403_S403child1_trait403));
  return impl;
} 
Trait403* Trait403_S403child1_ = newTrait403_S403child1();

int Trait96_S403child1_trait96(Class* self_) {
  S403child1* self = ((S403child1*)self_);
{
    return (self -> a);
  }
} 
Trait96* newTrait96_S403child1() {
  Trait96 (* impl) = (new Trait96());
  setVec(Trait96_v, S403child1_classId, ((void*)impl));
  ((impl -> trait96) = (& Trait96_S403child1_trait96));
  return impl;
} 
Trait96* Trait96_S403child1_ = newTrait96_S403child1();

int S404_classId = Class_genId();
struct S404{

  int id;
  S404 ():id(S404_classId){

  }
};


int S404child1_classId = Class_genId();
struct S404child1:S404{

  S404child1 (int a):a(a){
    (id = S404child1_classId);
{

    }
  }
  int a;
};


int S404child2_classId = Class_genId();
struct S404child2:S404{

  S404child2 (int a):a(a){
    (id = S404child2_classId);
{

    }
  }
  int a;
};


struct Trait404{

  int(*trait404)(Class*);
};

Vec* Trait404_v = newVec();

int Trait404_S404child1_trait404(Class* self_) {
  S404child1* self = ((S404child1*)self_);
{
    return (self -> a);
  }
} 
Trait404* newTrait404_S404child1() {
  Trait404 (* impl) = (new Trait404());
  setVec(Trait404_v, S404child1_classId, ((void*)impl));
  ((impl -> trait404) = (& Trait404_S404child1_trait404));
  return impl;
} 
Trait404* Trait404_S404child1_ = newTrait404_S404child1();

int Trait2_S404child1_trait2(Class* self_) {
  S404child1* self = ((S404child1*)self_);
{
    return (self -> a);
  }
} 
Trait2* newTrait2_S404child1() {
  Trait2 (* impl) = (new Trait2());
  setVec(Trait2_v, S404child1_classId, ((void*)impl));
  ((impl -> trait2) = (& Trait2_S404child1_trait2));
  return impl;
} 
Trait2* Trait2_S404child1_ = newTrait2_S404child1();

int S405_classId = Class_genId();
struct S405{

  int id;
  S405 ():id(S405_classId){

  }
};


int S405child1_classId = Class_genId();
struct S405child1:S405{

  S405child1 (int a):a(a){
    (id = S405child1_classId);
{

    }
  }
  int a;
};


int S405child2_classId = Class_genId();
struct S405child2:S405{

  S405child2 (int a):a(a){
    (id = S405child2_classId);
{

    }
  }
  int a;
};


int S405child3_classId = Class_genId();
struct S405child3:S405{

  S405child3 (int a):a(a){
    (id = S405child3_classId);
{

    }
  }
  int a;
};


int S405child4_classId = Class_genId();
struct S405child4:S405{

  S405child4 (int a):a(a){
    (id = S405child4_classId);
{

    }
  }
  int a;
};


struct Trait405{

  int(*trait405)(Class*);
};

Vec* Trait405_v = newVec();

int Trait405_S405child1_trait405(Class* self_) {
  S405child1* self = ((S405child1*)self_);
{
    return (self -> a);
  }
} 
Trait405* newTrait405_S405child1() {
  Trait405 (* impl) = (new Trait405());
  setVec(Trait405_v, S405child1_classId, ((void*)impl));
  ((impl -> trait405) = (& Trait405_S405child1_trait405));
  return impl;
} 
Trait405* Trait405_S405child1_ = newTrait405_S405child1();

int Trait405_S405child2_trait405(Class* self_) {
  S405child2* self = ((S405child2*)self_);
{
    return (self -> a);
  }
} 
Trait405* newTrait405_S405child2() {
  Trait405 (* impl) = (new Trait405());
  setVec(Trait405_v, S405child2_classId, ((void*)impl));
  ((impl -> trait405) = (& Trait405_S405child2_trait405));
  return impl;
} 
Trait405* Trait405_S405child2_ = newTrait405_S405child2();

int Trait405_S405child3_trait405(Class* self_) {
  S405child3* self = ((S405child3*)self_);
{
    return (self -> a);
  }
} 
Trait405* newTrait405_S405child3() {
  Trait405 (* impl) = (new Trait405());
  setVec(Trait405_v, S405child3_classId, ((void*)impl));
  ((impl -> trait405) = (& Trait405_S405child3_trait405));
  return impl;
} 
Trait405* Trait405_S405child3_ = newTrait405_S405child3();

int Trait200_S405child1_trait200(Class* self_) {
  S405child1* self = ((S405child1*)self_);
{
    return (self -> a);
  }
} 
Trait200* newTrait200_S405child1() {
  Trait200 (* impl) = (new Trait200());
  setVec(Trait200_v, S405child1_classId, ((void*)impl));
  ((impl -> trait200) = (& Trait200_S405child1_trait200));
  return impl;
} 
Trait200* Trait200_S405child1_ = newTrait200_S405child1();

int S406_classId = Class_genId();
struct S406{

  int id;
  S406 ():id(S406_classId){

  }
};


int S406child1_classId = Class_genId();
struct S406child1:S406{

  S406child1 (int a):a(a){
    (id = S406child1_classId);
{

    }
  }
  int a;
};


int S406child2_classId = Class_genId();
struct S406child2:S406{

  S406child2 (int a):a(a){
    (id = S406child2_classId);
{

    }
  }
  int a;
};


int S406child3_classId = Class_genId();
struct S406child3:S406{

  S406child3 (int a):a(a){
    (id = S406child3_classId);
{

    }
  }
  int a;
};


int S406child4_classId = Class_genId();
struct S406child4:S406{

  S406child4 (int a):a(a){
    (id = S406child4_classId);
{

    }
  }
  int a;
};


struct Trait406{

  int(*trait406)(Class*);
};

Vec* Trait406_v = newVec();

int Trait406_S406child1_trait406(Class* self_) {
  S406child1* self = ((S406child1*)self_);
{
    return (self -> a);
  }
} 
Trait406* newTrait406_S406child1() {
  Trait406 (* impl) = (new Trait406());
  setVec(Trait406_v, S406child1_classId, ((void*)impl));
  ((impl -> trait406) = (& Trait406_S406child1_trait406));
  return impl;
} 
Trait406* Trait406_S406child1_ = newTrait406_S406child1();

int Trait7_S406child1_trait7(Class* self_) {
  S406child1* self = ((S406child1*)self_);
{
    return (self -> a);
  }
} 
Trait7* newTrait7_S406child1() {
  Trait7 (* impl) = (new Trait7());
  setVec(Trait7_v, S406child1_classId, ((void*)impl));
  ((impl -> trait7) = (& Trait7_S406child1_trait7));
  return impl;
} 
Trait7* Trait7_S406child1_ = newTrait7_S406child1();

int S407_classId = Class_genId();
struct S407{

  int id;
  S407 ():id(S407_classId){

  }
};


int S407child1_classId = Class_genId();
struct S407child1:S407{

  S407child1 (int a):a(a){
    (id = S407child1_classId);
{

    }
  }
  int a;
};


int S407child2_classId = Class_genId();
struct S407child2:S407{

  S407child2 (int a):a(a){
    (id = S407child2_classId);
{

    }
  }
  int a;
};


struct Trait407{

  int(*trait407)(Class*);
};

Vec* Trait407_v = newVec();

int Trait407_S407child1_trait407(Class* self_) {
  S407child1* self = ((S407child1*)self_);
{
    return (self -> a);
  }
} 
Trait407* newTrait407_S407child1() {
  Trait407 (* impl) = (new Trait407());
  setVec(Trait407_v, S407child1_classId, ((void*)impl));
  ((impl -> trait407) = (& Trait407_S407child1_trait407));
  return impl;
} 
Trait407* Trait407_S407child1_ = newTrait407_S407child1();

int Trait407_S407child2_trait407(Class* self_) {
  S407child2* self = ((S407child2*)self_);
{
    return (self -> a);
  }
} 
Trait407* newTrait407_S407child2() {
  Trait407 (* impl) = (new Trait407());
  setVec(Trait407_v, S407child2_classId, ((void*)impl));
  ((impl -> trait407) = (& Trait407_S407child2_trait407));
  return impl;
} 
Trait407* Trait407_S407child2_ = newTrait407_S407child2();

int Trait29_S407child1_trait29(Class* self_) {
  S407child1* self = ((S407child1*)self_);
{
    return (self -> a);
  }
} 
Trait29* newTrait29_S407child1() {
  Trait29 (* impl) = (new Trait29());
  setVec(Trait29_v, S407child1_classId, ((void*)impl));
  ((impl -> trait29) = (& Trait29_S407child1_trait29));
  return impl;
} 
Trait29* Trait29_S407child1_ = newTrait29_S407child1();

int S408_classId = Class_genId();
struct S408{

  int id;
  S408 ():id(S408_classId){

  }
};


int S408child1_classId = Class_genId();
struct S408child1:S408{

  S408child1 (int a):a(a){
    (id = S408child1_classId);
{

    }
  }
  int a;
};


int S408child2_classId = Class_genId();
struct S408child2:S408{

  S408child2 (int a):a(a){
    (id = S408child2_classId);
{

    }
  }
  int a;
};


struct Trait408{

  int(*trait408)(Class*);
};

Vec* Trait408_v = newVec();

int Trait408_S408child1_trait408(Class* self_) {
  S408child1* self = ((S408child1*)self_);
{
    return (self -> a);
  }
} 
Trait408* newTrait408_S408child1() {
  Trait408 (* impl) = (new Trait408());
  setVec(Trait408_v, S408child1_classId, ((void*)impl));
  ((impl -> trait408) = (& Trait408_S408child1_trait408));
  return impl;
} 
Trait408* Trait408_S408child1_ = newTrait408_S408child1();

int Trait107_S408child1_trait107(Class* self_) {
  S408child1* self = ((S408child1*)self_);
{
    return (self -> a);
  }
} 
Trait107* newTrait107_S408child1() {
  Trait107 (* impl) = (new Trait107());
  setVec(Trait107_v, S408child1_classId, ((void*)impl));
  ((impl -> trait107) = (& Trait107_S408child1_trait107));
  return impl;
} 
Trait107* Trait107_S408child1_ = newTrait107_S408child1();

int S409_classId = Class_genId();
struct S409{

  int id;
  S409 ():id(S409_classId){

  }
};


int S409child1_classId = Class_genId();
struct S409child1:S409{

  S409child1 (int a):a(a){
    (id = S409child1_classId);
{

    }
  }
  int a;
};


int S409child2_classId = Class_genId();
struct S409child2:S409{

  S409child2 (int a):a(a){
    (id = S409child2_classId);
{

    }
  }
  int a;
};


struct Trait409{

  int(*trait409)(Class*);
};

Vec* Trait409_v = newVec();

int Trait409_S409child1_trait409(Class* self_) {
  S409child1* self = ((S409child1*)self_);
{
    return (self -> a);
  }
} 
Trait409* newTrait409_S409child1() {
  Trait409 (* impl) = (new Trait409());
  setVec(Trait409_v, S409child1_classId, ((void*)impl));
  ((impl -> trait409) = (& Trait409_S409child1_trait409));
  return impl;
} 
Trait409* Trait409_S409child1_ = newTrait409_S409child1();

int Trait122_S409child1_trait122(Class* self_) {
  S409child1* self = ((S409child1*)self_);
{
    return (self -> a);
  }
} 
Trait122* newTrait122_S409child1() {
  Trait122 (* impl) = (new Trait122());
  setVec(Trait122_v, S409child1_classId, ((void*)impl));
  ((impl -> trait122) = (& Trait122_S409child1_trait122));
  return impl;
} 
Trait122* Trait122_S409child1_ = newTrait122_S409child1();

int S410_classId = Class_genId();
struct S410{

  int id;
  S410 ():id(S410_classId){

  }
};


int S410child1_classId = Class_genId();
struct S410child1:S410{

  S410child1 (int a):a(a){
    (id = S410child1_classId);
{

    }
  }
  int a;
};


int S410child2_classId = Class_genId();
struct S410child2:S410{

  S410child2 (int a):a(a){
    (id = S410child2_classId);
{

    }
  }
  int a;
};


int S410child3_classId = Class_genId();
struct S410child3:S410{

  S410child3 (int a):a(a){
    (id = S410child3_classId);
{

    }
  }
  int a;
};


struct Trait410{

  int(*trait410)(Class*);
};

Vec* Trait410_v = newVec();

int Trait410_S410child1_trait410(Class* self_) {
  S410child1* self = ((S410child1*)self_);
{
    return (self -> a);
  }
} 
Trait410* newTrait410_S410child1() {
  Trait410 (* impl) = (new Trait410());
  setVec(Trait410_v, S410child1_classId, ((void*)impl));
  ((impl -> trait410) = (& Trait410_S410child1_trait410));
  return impl;
} 
Trait410* Trait410_S410child1_ = newTrait410_S410child1();

int Trait410_S410child2_trait410(Class* self_) {
  S410child2* self = ((S410child2*)self_);
{
    return (self -> a);
  }
} 
Trait410* newTrait410_S410child2() {
  Trait410 (* impl) = (new Trait410());
  setVec(Trait410_v, S410child2_classId, ((void*)impl));
  ((impl -> trait410) = (& Trait410_S410child2_trait410));
  return impl;
} 
Trait410* Trait410_S410child2_ = newTrait410_S410child2();

int Trait237_S410child1_trait237(Class* self_) {
  S410child1* self = ((S410child1*)self_);
{
    return (self -> a);
  }
} 
Trait237* newTrait237_S410child1() {
  Trait237 (* impl) = (new Trait237());
  setVec(Trait237_v, S410child1_classId, ((void*)impl));
  ((impl -> trait237) = (& Trait237_S410child1_trait237));
  return impl;
} 
Trait237* Trait237_S410child1_ = newTrait237_S410child1();

int S411_classId = Class_genId();
struct S411{

  int id;
  S411 ():id(S411_classId){

  }
};


int S411child1_classId = Class_genId();
struct S411child1:S411{

  S411child1 (int a):a(a){
    (id = S411child1_classId);
{

    }
  }
  int a;
};


int S411child2_classId = Class_genId();
struct S411child2:S411{

  S411child2 (int a):a(a){
    (id = S411child2_classId);
{

    }
  }
  int a;
};


int S411child3_classId = Class_genId();
struct S411child3:S411{

  S411child3 (int a):a(a){
    (id = S411child3_classId);
{

    }
  }
  int a;
};


struct Trait411{

  int(*trait411)(Class*);
};

Vec* Trait411_v = newVec();

int Trait411_S411child1_trait411(Class* self_) {
  S411child1* self = ((S411child1*)self_);
{
    return (self -> a);
  }
} 
Trait411* newTrait411_S411child1() {
  Trait411 (* impl) = (new Trait411());
  setVec(Trait411_v, S411child1_classId, ((void*)impl));
  ((impl -> trait411) = (& Trait411_S411child1_trait411));
  return impl;
} 
Trait411* Trait411_S411child1_ = newTrait411_S411child1();

int Trait102_S411child1_trait102(Class* self_) {
  S411child1* self = ((S411child1*)self_);
{
    return (self -> a);
  }
} 
Trait102* newTrait102_S411child1() {
  Trait102 (* impl) = (new Trait102());
  setVec(Trait102_v, S411child1_classId, ((void*)impl));
  ((impl -> trait102) = (& Trait102_S411child1_trait102));
  return impl;
} 
Trait102* Trait102_S411child1_ = newTrait102_S411child1();

int S412_classId = Class_genId();
struct S412{

  int id;
  S412 ():id(S412_classId){

  }
};


int S412child1_classId = Class_genId();
struct S412child1:S412{

  S412child1 (int a):a(a){
    (id = S412child1_classId);
{

    }
  }
  int a;
};


int S412child2_classId = Class_genId();
struct S412child2:S412{

  S412child2 (int a):a(a){
    (id = S412child2_classId);
{

    }
  }
  int a;
};


struct Trait412{

  int(*trait412)(Class*);
};

Vec* Trait412_v = newVec();

int Trait412_S412child1_trait412(Class* self_) {
  S412child1* self = ((S412child1*)self_);
{
    return (self -> a);
  }
} 
Trait412* newTrait412_S412child1() {
  Trait412 (* impl) = (new Trait412());
  setVec(Trait412_v, S412child1_classId, ((void*)impl));
  ((impl -> trait412) = (& Trait412_S412child1_trait412));
  return impl;
} 
Trait412* Trait412_S412child1_ = newTrait412_S412child1();

int Trait412_S412child2_trait412(Class* self_) {
  S412child2* self = ((S412child2*)self_);
{
    return (self -> a);
  }
} 
Trait412* newTrait412_S412child2() {
  Trait412 (* impl) = (new Trait412());
  setVec(Trait412_v, S412child2_classId, ((void*)impl));
  ((impl -> trait412) = (& Trait412_S412child2_trait412));
  return impl;
} 
Trait412* Trait412_S412child2_ = newTrait412_S412child2();

int Trait387_S412child1_trait387(Class* self_) {
  S412child1* self = ((S412child1*)self_);
{
    return (self -> a);
  }
} 
Trait387* newTrait387_S412child1() {
  Trait387 (* impl) = (new Trait387());
  setVec(Trait387_v, S412child1_classId, ((void*)impl));
  ((impl -> trait387) = (& Trait387_S412child1_trait387));
  return impl;
} 
Trait387* Trait387_S412child1_ = newTrait387_S412child1();

int S413_classId = Class_genId();
struct S413{

  int id;
  S413 ():id(S413_classId){

  }
};


int S413child1_classId = Class_genId();
struct S413child1:S413{

  S413child1 (int a):a(a){
    (id = S413child1_classId);
{

    }
  }
  int a;
};


int S413child2_classId = Class_genId();
struct S413child2:S413{

  S413child2 (int a):a(a){
    (id = S413child2_classId);
{

    }
  }
  int a;
};


struct Trait413{

  int(*trait413)(Class*);
};

Vec* Trait413_v = newVec();

int Trait413_S413child1_trait413(Class* self_) {
  S413child1* self = ((S413child1*)self_);
{
    return (self -> a);
  }
} 
Trait413* newTrait413_S413child1() {
  Trait413 (* impl) = (new Trait413());
  setVec(Trait413_v, S413child1_classId, ((void*)impl));
  ((impl -> trait413) = (& Trait413_S413child1_trait413));
  return impl;
} 
Trait413* Trait413_S413child1_ = newTrait413_S413child1();

int Trait413_S413child2_trait413(Class* self_) {
  S413child2* self = ((S413child2*)self_);
{
    return (self -> a);
  }
} 
Trait413* newTrait413_S413child2() {
  Trait413 (* impl) = (new Trait413());
  setVec(Trait413_v, S413child2_classId, ((void*)impl));
  ((impl -> trait413) = (& Trait413_S413child2_trait413));
  return impl;
} 
Trait413* Trait413_S413child2_ = newTrait413_S413child2();

int Trait166_S413child1_trait166(Class* self_) {
  S413child1* self = ((S413child1*)self_);
{
    return (self -> a);
  }
} 
Trait166* newTrait166_S413child1() {
  Trait166 (* impl) = (new Trait166());
  setVec(Trait166_v, S413child1_classId, ((void*)impl));
  ((impl -> trait166) = (& Trait166_S413child1_trait166));
  return impl;
} 
Trait166* Trait166_S413child1_ = newTrait166_S413child1();

int S414_classId = Class_genId();
struct S414{

  int id;
  S414 ():id(S414_classId){

  }
};


int S414child1_classId = Class_genId();
struct S414child1:S414{

  S414child1 (int a):a(a){
    (id = S414child1_classId);
{

    }
  }
  int a;
};


int S414child2_classId = Class_genId();
struct S414child2:S414{

  S414child2 (int a):a(a){
    (id = S414child2_classId);
{

    }
  }
  int a;
};


int S414child3_classId = Class_genId();
struct S414child3:S414{

  S414child3 (int a):a(a){
    (id = S414child3_classId);
{

    }
  }
  int a;
};


struct Trait414{

  int(*trait414)(Class*);
};

Vec* Trait414_v = newVec();

int Trait414_S414child1_trait414(Class* self_) {
  S414child1* self = ((S414child1*)self_);
{
    return (self -> a);
  }
} 
Trait414* newTrait414_S414child1() {
  Trait414 (* impl) = (new Trait414());
  setVec(Trait414_v, S414child1_classId, ((void*)impl));
  ((impl -> trait414) = (& Trait414_S414child1_trait414));
  return impl;
} 
Trait414* Trait414_S414child1_ = newTrait414_S414child1();

int Trait414_S414child2_trait414(Class* self_) {
  S414child2* self = ((S414child2*)self_);
{
    return (self -> a);
  }
} 
Trait414* newTrait414_S414child2() {
  Trait414 (* impl) = (new Trait414());
  setVec(Trait414_v, S414child2_classId, ((void*)impl));
  ((impl -> trait414) = (& Trait414_S414child2_trait414));
  return impl;
} 
Trait414* Trait414_S414child2_ = newTrait414_S414child2();

int Trait414_S414child3_trait414(Class* self_) {
  S414child3* self = ((S414child3*)self_);
{
    return (self -> a);
  }
} 
Trait414* newTrait414_S414child3() {
  Trait414 (* impl) = (new Trait414());
  setVec(Trait414_v, S414child3_classId, ((void*)impl));
  ((impl -> trait414) = (& Trait414_S414child3_trait414));
  return impl;
} 
Trait414* Trait414_S414child3_ = newTrait414_S414child3();

int Trait98_S414child1_trait98(Class* self_) {
  S414child1* self = ((S414child1*)self_);
{
    return (self -> a);
  }
} 
Trait98* newTrait98_S414child1() {
  Trait98 (* impl) = (new Trait98());
  setVec(Trait98_v, S414child1_classId, ((void*)impl));
  ((impl -> trait98) = (& Trait98_S414child1_trait98));
  return impl;
} 
Trait98* Trait98_S414child1_ = newTrait98_S414child1();

int S415_classId = Class_genId();
struct S415{

  int id;
  S415 ():id(S415_classId){

  }
};


int S415child1_classId = Class_genId();
struct S415child1:S415{

  S415child1 (int a):a(a){
    (id = S415child1_classId);
{

    }
  }
  int a;
};


struct Trait415{

  int(*trait415)(Class*);
};

Vec* Trait415_v = newVec();

int Trait415_S415child1_trait415(Class* self_) {
  S415child1* self = ((S415child1*)self_);
{
    return (self -> a);
  }
} 
Trait415* newTrait415_S415child1() {
  Trait415 (* impl) = (new Trait415());
  setVec(Trait415_v, S415child1_classId, ((void*)impl));
  ((impl -> trait415) = (& Trait415_S415child1_trait415));
  return impl;
} 
Trait415* Trait415_S415child1_ = newTrait415_S415child1();

int Trait137_S415child1_trait137(Class* self_) {
  S415child1* self = ((S415child1*)self_);
{
    return (self -> a);
  }
} 
Trait137* newTrait137_S415child1() {
  Trait137 (* impl) = (new Trait137());
  setVec(Trait137_v, S415child1_classId, ((void*)impl));
  ((impl -> trait137) = (& Trait137_S415child1_trait137));
  return impl;
} 
Trait137* Trait137_S415child1_ = newTrait137_S415child1();

int S416_classId = Class_genId();
struct S416{

  int id;
  S416 ():id(S416_classId){

  }
};


int S416child1_classId = Class_genId();
struct S416child1:S416{

  S416child1 (int a):a(a){
    (id = S416child1_classId);
{

    }
  }
  int a;
};


int S416child2_classId = Class_genId();
struct S416child2:S416{

  S416child2 (int a):a(a){
    (id = S416child2_classId);
{

    }
  }
  int a;
};


int S416child3_classId = Class_genId();
struct S416child3:S416{

  S416child3 (int a):a(a){
    (id = S416child3_classId);
{

    }
  }
  int a;
};


int S416child4_classId = Class_genId();
struct S416child4:S416{

  S416child4 (int a):a(a){
    (id = S416child4_classId);
{

    }
  }
  int a;
};


struct Trait416{

  int(*trait416)(Class*);
};

Vec* Trait416_v = newVec();

int Trait416_S416child1_trait416(Class* self_) {
  S416child1* self = ((S416child1*)self_);
{
    return (self -> a);
  }
} 
Trait416* newTrait416_S416child1() {
  Trait416 (* impl) = (new Trait416());
  setVec(Trait416_v, S416child1_classId, ((void*)impl));
  ((impl -> trait416) = (& Trait416_S416child1_trait416));
  return impl;
} 
Trait416* Trait416_S416child1_ = newTrait416_S416child1();

int Trait416_S416child2_trait416(Class* self_) {
  S416child2* self = ((S416child2*)self_);
{
    return (self -> a);
  }
} 
Trait416* newTrait416_S416child2() {
  Trait416 (* impl) = (new Trait416());
  setVec(Trait416_v, S416child2_classId, ((void*)impl));
  ((impl -> trait416) = (& Trait416_S416child2_trait416));
  return impl;
} 
Trait416* Trait416_S416child2_ = newTrait416_S416child2();

int Trait22_S416child1_trait22(Class* self_) {
  S416child1* self = ((S416child1*)self_);
{
    return (self -> a);
  }
} 
Trait22* newTrait22_S416child1() {
  Trait22 (* impl) = (new Trait22());
  setVec(Trait22_v, S416child1_classId, ((void*)impl));
  ((impl -> trait22) = (& Trait22_S416child1_trait22));
  return impl;
} 
Trait22* Trait22_S416child1_ = newTrait22_S416child1();

int S417_classId = Class_genId();
struct S417{

  int id;
  S417 ():id(S417_classId){

  }
};


int S417child1_classId = Class_genId();
struct S417child1:S417{

  S417child1 (int a):a(a){
    (id = S417child1_classId);
{

    }
  }
  int a;
};


int S417child2_classId = Class_genId();
struct S417child2:S417{

  S417child2 (int a):a(a){
    (id = S417child2_classId);
{

    }
  }
  int a;
};


struct Trait417{

  int(*trait417)(Class*);
};

Vec* Trait417_v = newVec();

int Trait417_S417child1_trait417(Class* self_) {
  S417child1* self = ((S417child1*)self_);
{
    return (self -> a);
  }
} 
Trait417* newTrait417_S417child1() {
  Trait417 (* impl) = (new Trait417());
  setVec(Trait417_v, S417child1_classId, ((void*)impl));
  ((impl -> trait417) = (& Trait417_S417child1_trait417));
  return impl;
} 
Trait417* Trait417_S417child1_ = newTrait417_S417child1();

int Trait417_S417child2_trait417(Class* self_) {
  S417child2* self = ((S417child2*)self_);
{
    return (self -> a);
  }
} 
Trait417* newTrait417_S417child2() {
  Trait417 (* impl) = (new Trait417());
  setVec(Trait417_v, S417child2_classId, ((void*)impl));
  ((impl -> trait417) = (& Trait417_S417child2_trait417));
  return impl;
} 
Trait417* Trait417_S417child2_ = newTrait417_S417child2();

int Trait4_S417child1_trait4(Class* self_) {
  S417child1* self = ((S417child1*)self_);
{
    return (self -> a);
  }
} 
Trait4* newTrait4_S417child1() {
  Trait4 (* impl) = (new Trait4());
  setVec(Trait4_v, S417child1_classId, ((void*)impl));
  ((impl -> trait4) = (& Trait4_S417child1_trait4));
  return impl;
} 
Trait4* Trait4_S417child1_ = newTrait4_S417child1();

int S418_classId = Class_genId();
struct S418{

  int id;
  S418 ():id(S418_classId){

  }
};


int S418child1_classId = Class_genId();
struct S418child1:S418{

  S418child1 (int a):a(a){
    (id = S418child1_classId);
{

    }
  }
  int a;
};


int S418child2_classId = Class_genId();
struct S418child2:S418{

  S418child2 (int a):a(a){
    (id = S418child2_classId);
{

    }
  }
  int a;
};


int S418child3_classId = Class_genId();
struct S418child3:S418{

  S418child3 (int a):a(a){
    (id = S418child3_classId);
{

    }
  }
  int a;
};


struct Trait418{

  int(*trait418)(Class*);
};

Vec* Trait418_v = newVec();

int Trait418_S418child1_trait418(Class* self_) {
  S418child1* self = ((S418child1*)self_);
{
    return (self -> a);
  }
} 
Trait418* newTrait418_S418child1() {
  Trait418 (* impl) = (new Trait418());
  setVec(Trait418_v, S418child1_classId, ((void*)impl));
  ((impl -> trait418) = (& Trait418_S418child1_trait418));
  return impl;
} 
Trait418* Trait418_S418child1_ = newTrait418_S418child1();

int Trait114_S418child1_trait114(Class* self_) {
  S418child1* self = ((S418child1*)self_);
{
    return (self -> a);
  }
} 
Trait114* newTrait114_S418child1() {
  Trait114 (* impl) = (new Trait114());
  setVec(Trait114_v, S418child1_classId, ((void*)impl));
  ((impl -> trait114) = (& Trait114_S418child1_trait114));
  return impl;
} 
Trait114* Trait114_S418child1_ = newTrait114_S418child1();

int S419_classId = Class_genId();
struct S419{

  int id;
  S419 ():id(S419_classId){

  }
};


int S419child1_classId = Class_genId();
struct S419child1:S419{

  S419child1 (int a):a(a){
    (id = S419child1_classId);
{

    }
  }
  int a;
};


int S419child2_classId = Class_genId();
struct S419child2:S419{

  S419child2 (int a):a(a){
    (id = S419child2_classId);
{

    }
  }
  int a;
};


int S419child3_classId = Class_genId();
struct S419child3:S419{

  S419child3 (int a):a(a){
    (id = S419child3_classId);
{

    }
  }
  int a;
};


int S419child4_classId = Class_genId();
struct S419child4:S419{

  S419child4 (int a):a(a){
    (id = S419child4_classId);
{

    }
  }
  int a;
};


struct Trait419{

  int(*trait419)(Class*);
};

Vec* Trait419_v = newVec();

int Trait419_S419child1_trait419(Class* self_) {
  S419child1* self = ((S419child1*)self_);
{
    return (self -> a);
  }
} 
Trait419* newTrait419_S419child1() {
  Trait419 (* impl) = (new Trait419());
  setVec(Trait419_v, S419child1_classId, ((void*)impl));
  ((impl -> trait419) = (& Trait419_S419child1_trait419));
  return impl;
} 
Trait419* Trait419_S419child1_ = newTrait419_S419child1();

int Trait141_S419child1_trait141(Class* self_) {
  S419child1* self = ((S419child1*)self_);
{
    return (self -> a);
  }
} 
Trait141* newTrait141_S419child1() {
  Trait141 (* impl) = (new Trait141());
  setVec(Trait141_v, S419child1_classId, ((void*)impl));
  ((impl -> trait141) = (& Trait141_S419child1_trait141));
  return impl;
} 
Trait141* Trait141_S419child1_ = newTrait141_S419child1();

int S420_classId = Class_genId();
struct S420{

  int id;
  S420 ():id(S420_classId){

  }
};


int S420child1_classId = Class_genId();
struct S420child1:S420{

  S420child1 (int a):a(a){
    (id = S420child1_classId);
{

    }
  }
  int a;
};


int S420child2_classId = Class_genId();
struct S420child2:S420{

  S420child2 (int a):a(a){
    (id = S420child2_classId);
{

    }
  }
  int a;
};


int S420child3_classId = Class_genId();
struct S420child3:S420{

  S420child3 (int a):a(a){
    (id = S420child3_classId);
{

    }
  }
  int a;
};


struct Trait420{

  int(*trait420)(Class*);
};

Vec* Trait420_v = newVec();

int Trait420_S420child1_trait420(Class* self_) {
  S420child1* self = ((S420child1*)self_);
{
    return (self -> a);
  }
} 
Trait420* newTrait420_S420child1() {
  Trait420 (* impl) = (new Trait420());
  setVec(Trait420_v, S420child1_classId, ((void*)impl));
  ((impl -> trait420) = (& Trait420_S420child1_trait420));
  return impl;
} 
Trait420* Trait420_S420child1_ = newTrait420_S420child1();

int Trait420_S420child2_trait420(Class* self_) {
  S420child2* self = ((S420child2*)self_);
{
    return (self -> a);
  }
} 
Trait420* newTrait420_S420child2() {
  Trait420 (* impl) = (new Trait420());
  setVec(Trait420_v, S420child2_classId, ((void*)impl));
  ((impl -> trait420) = (& Trait420_S420child2_trait420));
  return impl;
} 
Trait420* Trait420_S420child2_ = newTrait420_S420child2();

int Trait241_S420child1_trait241(Class* self_) {
  S420child1* self = ((S420child1*)self_);
{
    return (self -> a);
  }
} 
Trait241* newTrait241_S420child1() {
  Trait241 (* impl) = (new Trait241());
  setVec(Trait241_v, S420child1_classId, ((void*)impl));
  ((impl -> trait241) = (& Trait241_S420child1_trait241));
  return impl;
} 
Trait241* Trait241_S420child1_ = newTrait241_S420child1();

int S421_classId = Class_genId();
struct S421{

  int id;
  S421 ():id(S421_classId){

  }
};


int S421child1_classId = Class_genId();
struct S421child1:S421{

  S421child1 (int a):a(a){
    (id = S421child1_classId);
{

    }
  }
  int a;
};


struct Trait421{

  int(*trait421)(Class*);
};

Vec* Trait421_v = newVec();

int Trait421_S421child1_trait421(Class* self_) {
  S421child1* self = ((S421child1*)self_);
{
    return (self -> a);
  }
} 
Trait421* newTrait421_S421child1() {
  Trait421 (* impl) = (new Trait421());
  setVec(Trait421_v, S421child1_classId, ((void*)impl));
  ((impl -> trait421) = (& Trait421_S421child1_trait421));
  return impl;
} 
Trait421* Trait421_S421child1_ = newTrait421_S421child1();

int Trait290_S421child1_trait290(Class* self_) {
  S421child1* self = ((S421child1*)self_);
{
    return (self -> a);
  }
} 
Trait290* newTrait290_S421child1() {
  Trait290 (* impl) = (new Trait290());
  setVec(Trait290_v, S421child1_classId, ((void*)impl));
  ((impl -> trait290) = (& Trait290_S421child1_trait290));
  return impl;
} 
Trait290* Trait290_S421child1_ = newTrait290_S421child1();

int S422_classId = Class_genId();
struct S422{

  int id;
  S422 ():id(S422_classId){

  }
};


int S422child1_classId = Class_genId();
struct S422child1:S422{

  S422child1 (int a):a(a){
    (id = S422child1_classId);
{

    }
  }
  int a;
};


int S422child2_classId = Class_genId();
struct S422child2:S422{

  S422child2 (int a):a(a){
    (id = S422child2_classId);
{

    }
  }
  int a;
};


int S422child3_classId = Class_genId();
struct S422child3:S422{

  S422child3 (int a):a(a){
    (id = S422child3_classId);
{

    }
  }
  int a;
};


struct Trait422{

  int(*trait422)(Class*);
};

Vec* Trait422_v = newVec();

int Trait422_S422child1_trait422(Class* self_) {
  S422child1* self = ((S422child1*)self_);
{
    return (self -> a);
  }
} 
Trait422* newTrait422_S422child1() {
  Trait422 (* impl) = (new Trait422());
  setVec(Trait422_v, S422child1_classId, ((void*)impl));
  ((impl -> trait422) = (& Trait422_S422child1_trait422));
  return impl;
} 
Trait422* Trait422_S422child1_ = newTrait422_S422child1();

int Trait422_S422child2_trait422(Class* self_) {
  S422child2* self = ((S422child2*)self_);
{
    return (self -> a);
  }
} 
Trait422* newTrait422_S422child2() {
  Trait422 (* impl) = (new Trait422());
  setVec(Trait422_v, S422child2_classId, ((void*)impl));
  ((impl -> trait422) = (& Trait422_S422child2_trait422));
  return impl;
} 
Trait422* Trait422_S422child2_ = newTrait422_S422child2();

int Trait422_S422child3_trait422(Class* self_) {
  S422child3* self = ((S422child3*)self_);
{
    return (self -> a);
  }
} 
Trait422* newTrait422_S422child3() {
  Trait422 (* impl) = (new Trait422());
  setVec(Trait422_v, S422child3_classId, ((void*)impl));
  ((impl -> trait422) = (& Trait422_S422child3_trait422));
  return impl;
} 
Trait422* Trait422_S422child3_ = newTrait422_S422child3();

int Trait174_S422child1_trait174(Class* self_) {
  S422child1* self = ((S422child1*)self_);
{
    return (self -> a);
  }
} 
Trait174* newTrait174_S422child1() {
  Trait174 (* impl) = (new Trait174());
  setVec(Trait174_v, S422child1_classId, ((void*)impl));
  ((impl -> trait174) = (& Trait174_S422child1_trait174));
  return impl;
} 
Trait174* Trait174_S422child1_ = newTrait174_S422child1();

int S423_classId = Class_genId();
struct S423{

  int id;
  S423 ():id(S423_classId){

  }
};


int S423child1_classId = Class_genId();
struct S423child1:S423{

  S423child1 (int a):a(a){
    (id = S423child1_classId);
{

    }
  }
  int a;
};


struct Trait423{

  int(*trait423)(Class*);
};

Vec* Trait423_v = newVec();

int Trait423_S423child1_trait423(Class* self_) {
  S423child1* self = ((S423child1*)self_);
{
    return (self -> a);
  }
} 
Trait423* newTrait423_S423child1() {
  Trait423 (* impl) = (new Trait423());
  setVec(Trait423_v, S423child1_classId, ((void*)impl));
  ((impl -> trait423) = (& Trait423_S423child1_trait423));
  return impl;
} 
Trait423* Trait423_S423child1_ = newTrait423_S423child1();

int Trait18_S423child1_trait18(Class* self_) {
  S423child1* self = ((S423child1*)self_);
{
    return (self -> a);
  }
} 
Trait18* newTrait18_S423child1() {
  Trait18 (* impl) = (new Trait18());
  setVec(Trait18_v, S423child1_classId, ((void*)impl));
  ((impl -> trait18) = (& Trait18_S423child1_trait18));
  return impl;
} 
Trait18* Trait18_S423child1_ = newTrait18_S423child1();

int S424_classId = Class_genId();
struct S424{

  int id;
  S424 ():id(S424_classId){

  }
};


int S424child1_classId = Class_genId();
struct S424child1:S424{

  S424child1 (int a):a(a){
    (id = S424child1_classId);
{

    }
  }
  int a;
};


int S424child2_classId = Class_genId();
struct S424child2:S424{

  S424child2 (int a):a(a){
    (id = S424child2_classId);
{

    }
  }
  int a;
};


struct Trait424{

  int(*trait424)(Class*);
};

Vec* Trait424_v = newVec();

int Trait424_S424child1_trait424(Class* self_) {
  S424child1* self = ((S424child1*)self_);
{
    return (self -> a);
  }
} 
Trait424* newTrait424_S424child1() {
  Trait424 (* impl) = (new Trait424());
  setVec(Trait424_v, S424child1_classId, ((void*)impl));
  ((impl -> trait424) = (& Trait424_S424child1_trait424));
  return impl;
} 
Trait424* Trait424_S424child1_ = newTrait424_S424child1();

int Trait424_S424child2_trait424(Class* self_) {
  S424child2* self = ((S424child2*)self_);
{
    return (self -> a);
  }
} 
Trait424* newTrait424_S424child2() {
  Trait424 (* impl) = (new Trait424());
  setVec(Trait424_v, S424child2_classId, ((void*)impl));
  ((impl -> trait424) = (& Trait424_S424child2_trait424));
  return impl;
} 
Trait424* Trait424_S424child2_ = newTrait424_S424child2();

int Trait247_S424child1_trait247(Class* self_) {
  S424child1* self = ((S424child1*)self_);
{
    return (self -> a);
  }
} 
Trait247* newTrait247_S424child1() {
  Trait247 (* impl) = (new Trait247());
  setVec(Trait247_v, S424child1_classId, ((void*)impl));
  ((impl -> trait247) = (& Trait247_S424child1_trait247));
  return impl;
} 
Trait247* Trait247_S424child1_ = newTrait247_S424child1();

int S425_classId = Class_genId();
struct S425{

  int id;
  S425 ():id(S425_classId){

  }
};


int S425child1_classId = Class_genId();
struct S425child1:S425{

  S425child1 (int a):a(a){
    (id = S425child1_classId);
{

    }
  }
  int a;
};


struct Trait425{

  int(*trait425)(Class*);
};

Vec* Trait425_v = newVec();

int Trait425_S425child1_trait425(Class* self_) {
  S425child1* self = ((S425child1*)self_);
{
    return (self -> a);
  }
} 
Trait425* newTrait425_S425child1() {
  Trait425 (* impl) = (new Trait425());
  setVec(Trait425_v, S425child1_classId, ((void*)impl));
  ((impl -> trait425) = (& Trait425_S425child1_trait425));
  return impl;
} 
Trait425* Trait425_S425child1_ = newTrait425_S425child1();

int Trait265_S425child1_trait265(Class* self_) {
  S425child1* self = ((S425child1*)self_);
{
    return (self -> a);
  }
} 
Trait265* newTrait265_S425child1() {
  Trait265 (* impl) = (new Trait265());
  setVec(Trait265_v, S425child1_classId, ((void*)impl));
  ((impl -> trait265) = (& Trait265_S425child1_trait265));
  return impl;
} 
Trait265* Trait265_S425child1_ = newTrait265_S425child1();

int S426_classId = Class_genId();
struct S426{

  int id;
  S426 ():id(S426_classId){

  }
};


int S426child1_classId = Class_genId();
struct S426child1:S426{

  S426child1 (int a):a(a){
    (id = S426child1_classId);
{

    }
  }
  int a;
};


int S426child2_classId = Class_genId();
struct S426child2:S426{

  S426child2 (int a):a(a){
    (id = S426child2_classId);
{

    }
  }
  int a;
};


int S426child3_classId = Class_genId();
struct S426child3:S426{

  S426child3 (int a):a(a){
    (id = S426child3_classId);
{

    }
  }
  int a;
};


struct Trait426{

  int(*trait426)(Class*);
};

Vec* Trait426_v = newVec();

int Trait426_S426child1_trait426(Class* self_) {
  S426child1* self = ((S426child1*)self_);
{
    return (self -> a);
  }
} 
Trait426* newTrait426_S426child1() {
  Trait426 (* impl) = (new Trait426());
  setVec(Trait426_v, S426child1_classId, ((void*)impl));
  ((impl -> trait426) = (& Trait426_S426child1_trait426));
  return impl;
} 
Trait426* Trait426_S426child1_ = newTrait426_S426child1();

int Trait426_S426child2_trait426(Class* self_) {
  S426child2* self = ((S426child2*)self_);
{
    return (self -> a);
  }
} 
Trait426* newTrait426_S426child2() {
  Trait426 (* impl) = (new Trait426());
  setVec(Trait426_v, S426child2_classId, ((void*)impl));
  ((impl -> trait426) = (& Trait426_S426child2_trait426));
  return impl;
} 
Trait426* Trait426_S426child2_ = newTrait426_S426child2();

int Trait426_S426child3_trait426(Class* self_) {
  S426child3* self = ((S426child3*)self_);
{
    return (self -> a);
  }
} 
Trait426* newTrait426_S426child3() {
  Trait426 (* impl) = (new Trait426());
  setVec(Trait426_v, S426child3_classId, ((void*)impl));
  ((impl -> trait426) = (& Trait426_S426child3_trait426));
  return impl;
} 
Trait426* Trait426_S426child3_ = newTrait426_S426child3();

int Trait393_S426child1_trait393(Class* self_) {
  S426child1* self = ((S426child1*)self_);
{
    return (self -> a);
  }
} 
Trait393* newTrait393_S426child1() {
  Trait393 (* impl) = (new Trait393());
  setVec(Trait393_v, S426child1_classId, ((void*)impl));
  ((impl -> trait393) = (& Trait393_S426child1_trait393));
  return impl;
} 
Trait393* Trait393_S426child1_ = newTrait393_S426child1();

int S427_classId = Class_genId();
struct S427{

  int id;
  S427 ():id(S427_classId){

  }
};


int S427child1_classId = Class_genId();
struct S427child1:S427{

  S427child1 (int a):a(a){
    (id = S427child1_classId);
{

    }
  }
  int a;
};


int S427child2_classId = Class_genId();
struct S427child2:S427{

  S427child2 (int a):a(a){
    (id = S427child2_classId);
{

    }
  }
  int a;
};


int S427child3_classId = Class_genId();
struct S427child3:S427{

  S427child3 (int a):a(a){
    (id = S427child3_classId);
{

    }
  }
  int a;
};


struct Trait427{

  int(*trait427)(Class*);
};

Vec* Trait427_v = newVec();

int Trait427_S427child1_trait427(Class* self_) {
  S427child1* self = ((S427child1*)self_);
{
    return (self -> a);
  }
} 
Trait427* newTrait427_S427child1() {
  Trait427 (* impl) = (new Trait427());
  setVec(Trait427_v, S427child1_classId, ((void*)impl));
  ((impl -> trait427) = (& Trait427_S427child1_trait427));
  return impl;
} 
Trait427* Trait427_S427child1_ = newTrait427_S427child1();

int Trait427_S427child2_trait427(Class* self_) {
  S427child2* self = ((S427child2*)self_);
{
    return (self -> a);
  }
} 
Trait427* newTrait427_S427child2() {
  Trait427 (* impl) = (new Trait427());
  setVec(Trait427_v, S427child2_classId, ((void*)impl));
  ((impl -> trait427) = (& Trait427_S427child2_trait427));
  return impl;
} 
Trait427* Trait427_S427child2_ = newTrait427_S427child2();

int Trait411_S427child1_trait411(Class* self_) {
  S427child1* self = ((S427child1*)self_);
{
    return (self -> a);
  }
} 
Trait411* newTrait411_S427child1() {
  Trait411 (* impl) = (new Trait411());
  setVec(Trait411_v, S427child1_classId, ((void*)impl));
  ((impl -> trait411) = (& Trait411_S427child1_trait411));
  return impl;
} 
Trait411* Trait411_S427child1_ = newTrait411_S427child1();

int S428_classId = Class_genId();
struct S428{

  int id;
  S428 ():id(S428_classId){

  }
};


int S428child1_classId = Class_genId();
struct S428child1:S428{

  S428child1 (int a):a(a){
    (id = S428child1_classId);
{

    }
  }
  int a;
};


int S428child2_classId = Class_genId();
struct S428child2:S428{

  S428child2 (int a):a(a){
    (id = S428child2_classId);
{

    }
  }
  int a;
};


struct Trait428{

  int(*trait428)(Class*);
};

Vec* Trait428_v = newVec();

int Trait428_S428child1_trait428(Class* self_) {
  S428child1* self = ((S428child1*)self_);
{
    return (self -> a);
  }
} 
Trait428* newTrait428_S428child1() {
  Trait428 (* impl) = (new Trait428());
  setVec(Trait428_v, S428child1_classId, ((void*)impl));
  ((impl -> trait428) = (& Trait428_S428child1_trait428));
  return impl;
} 
Trait428* Trait428_S428child1_ = newTrait428_S428child1();

int Trait428_S428child2_trait428(Class* self_) {
  S428child2* self = ((S428child2*)self_);
{
    return (self -> a);
  }
} 
Trait428* newTrait428_S428child2() {
  Trait428 (* impl) = (new Trait428());
  setVec(Trait428_v, S428child2_classId, ((void*)impl));
  ((impl -> trait428) = (& Trait428_S428child2_trait428));
  return impl;
} 
Trait428* Trait428_S428child2_ = newTrait428_S428child2();

int Trait48_S428child1_trait48(Class* self_) {
  S428child1* self = ((S428child1*)self_);
{
    return (self -> a);
  }
} 
Trait48* newTrait48_S428child1() {
  Trait48 (* impl) = (new Trait48());
  setVec(Trait48_v, S428child1_classId, ((void*)impl));
  ((impl -> trait48) = (& Trait48_S428child1_trait48));
  return impl;
} 
Trait48* Trait48_S428child1_ = newTrait48_S428child1();

int S429_classId = Class_genId();
struct S429{

  int id;
  S429 ():id(S429_classId){

  }
};


int S429child1_classId = Class_genId();
struct S429child1:S429{

  S429child1 (int a):a(a){
    (id = S429child1_classId);
{

    }
  }
  int a;
};


int S429child2_classId = Class_genId();
struct S429child2:S429{

  S429child2 (int a):a(a){
    (id = S429child2_classId);
{

    }
  }
  int a;
};


int S429child3_classId = Class_genId();
struct S429child3:S429{

  S429child3 (int a):a(a){
    (id = S429child3_classId);
{

    }
  }
  int a;
};


struct Trait429{

  int(*trait429)(Class*);
};

Vec* Trait429_v = newVec();

int Trait429_S429child1_trait429(Class* self_) {
  S429child1* self = ((S429child1*)self_);
{
    return (self -> a);
  }
} 
Trait429* newTrait429_S429child1() {
  Trait429 (* impl) = (new Trait429());
  setVec(Trait429_v, S429child1_classId, ((void*)impl));
  ((impl -> trait429) = (& Trait429_S429child1_trait429));
  return impl;
} 
Trait429* Trait429_S429child1_ = newTrait429_S429child1();

int Trait429_S429child2_trait429(Class* self_) {
  S429child2* self = ((S429child2*)self_);
{
    return (self -> a);
  }
} 
Trait429* newTrait429_S429child2() {
  Trait429 (* impl) = (new Trait429());
  setVec(Trait429_v, S429child2_classId, ((void*)impl));
  ((impl -> trait429) = (& Trait429_S429child2_trait429));
  return impl;
} 
Trait429* Trait429_S429child2_ = newTrait429_S429child2();

int Trait429_S429child3_trait429(Class* self_) {
  S429child3* self = ((S429child3*)self_);
{
    return (self -> a);
  }
} 
Trait429* newTrait429_S429child3() {
  Trait429 (* impl) = (new Trait429());
  setVec(Trait429_v, S429child3_classId, ((void*)impl));
  ((impl -> trait429) = (& Trait429_S429child3_trait429));
  return impl;
} 
Trait429* Trait429_S429child3_ = newTrait429_S429child3();

int Trait401_S429child1_trait401(Class* self_) {
  S429child1* self = ((S429child1*)self_);
{
    return (self -> a);
  }
} 
Trait401* newTrait401_S429child1() {
  Trait401 (* impl) = (new Trait401());
  setVec(Trait401_v, S429child1_classId, ((void*)impl));
  ((impl -> trait401) = (& Trait401_S429child1_trait401));
  return impl;
} 
Trait401* Trait401_S429child1_ = newTrait401_S429child1();

int S430_classId = Class_genId();
struct S430{

  int id;
  S430 ():id(S430_classId){

  }
};


int S430child1_classId = Class_genId();
struct S430child1:S430{

  S430child1 (int a):a(a){
    (id = S430child1_classId);
{

    }
  }
  int a;
};


int S430child2_classId = Class_genId();
struct S430child2:S430{

  S430child2 (int a):a(a){
    (id = S430child2_classId);
{

    }
  }
  int a;
};


struct Trait430{

  int(*trait430)(Class*);
};

Vec* Trait430_v = newVec();

int Trait430_S430child1_trait430(Class* self_) {
  S430child1* self = ((S430child1*)self_);
{
    return (self -> a);
  }
} 
Trait430* newTrait430_S430child1() {
  Trait430 (* impl) = (new Trait430());
  setVec(Trait430_v, S430child1_classId, ((void*)impl));
  ((impl -> trait430) = (& Trait430_S430child1_trait430));
  return impl;
} 
Trait430* Trait430_S430child1_ = newTrait430_S430child1();

int Trait430_S430child2_trait430(Class* self_) {
  S430child2* self = ((S430child2*)self_);
{
    return (self -> a);
  }
} 
Trait430* newTrait430_S430child2() {
  Trait430 (* impl) = (new Trait430());
  setVec(Trait430_v, S430child2_classId, ((void*)impl));
  ((impl -> trait430) = (& Trait430_S430child2_trait430));
  return impl;
} 
Trait430* Trait430_S430child2_ = newTrait430_S430child2();

int Trait389_S430child1_trait389(Class* self_) {
  S430child1* self = ((S430child1*)self_);
{
    return (self -> a);
  }
} 
Trait389* newTrait389_S430child1() {
  Trait389 (* impl) = (new Trait389());
  setVec(Trait389_v, S430child1_classId, ((void*)impl));
  ((impl -> trait389) = (& Trait389_S430child1_trait389));
  return impl;
} 
Trait389* Trait389_S430child1_ = newTrait389_S430child1();

int S431_classId = Class_genId();
struct S431{

  int id;
  S431 ():id(S431_classId){

  }
};


int S431child1_classId = Class_genId();
struct S431child1:S431{

  S431child1 (int a):a(a){
    (id = S431child1_classId);
{

    }
  }
  int a;
};


struct Trait431{

  int(*trait431)(Class*);
};

Vec* Trait431_v = newVec();

int Trait431_S431child1_trait431(Class* self_) {
  S431child1* self = ((S431child1*)self_);
{
    return (self -> a);
  }
} 
Trait431* newTrait431_S431child1() {
  Trait431 (* impl) = (new Trait431());
  setVec(Trait431_v, S431child1_classId, ((void*)impl));
  ((impl -> trait431) = (& Trait431_S431child1_trait431));
  return impl;
} 
Trait431* Trait431_S431child1_ = newTrait431_S431child1();

int Trait353_S431child1_trait353(Class* self_) {
  S431child1* self = ((S431child1*)self_);
{
    return (self -> a);
  }
} 
Trait353* newTrait353_S431child1() {
  Trait353 (* impl) = (new Trait353());
  setVec(Trait353_v, S431child1_classId, ((void*)impl));
  ((impl -> trait353) = (& Trait353_S431child1_trait353));
  return impl;
} 
Trait353* Trait353_S431child1_ = newTrait353_S431child1();

int S432_classId = Class_genId();
struct S432{

  int id;
  S432 ():id(S432_classId){

  }
};


int S432child1_classId = Class_genId();
struct S432child1:S432{

  S432child1 (int a):a(a){
    (id = S432child1_classId);
{

    }
  }
  int a;
};


int S432child2_classId = Class_genId();
struct S432child2:S432{

  S432child2 (int a):a(a){
    (id = S432child2_classId);
{

    }
  }
  int a;
};


int S432child3_classId = Class_genId();
struct S432child3:S432{

  S432child3 (int a):a(a){
    (id = S432child3_classId);
{

    }
  }
  int a;
};


struct Trait432{

  int(*trait432)(Class*);
};

Vec* Trait432_v = newVec();

int Trait432_S432child1_trait432(Class* self_) {
  S432child1* self = ((S432child1*)self_);
{
    return (self -> a);
  }
} 
Trait432* newTrait432_S432child1() {
  Trait432 (* impl) = (new Trait432());
  setVec(Trait432_v, S432child1_classId, ((void*)impl));
  ((impl -> trait432) = (& Trait432_S432child1_trait432));
  return impl;
} 
Trait432* Trait432_S432child1_ = newTrait432_S432child1();

int Trait254_S432child1_trait254(Class* self_) {
  S432child1* self = ((S432child1*)self_);
{
    return (self -> a);
  }
} 
Trait254* newTrait254_S432child1() {
  Trait254 (* impl) = (new Trait254());
  setVec(Trait254_v, S432child1_classId, ((void*)impl));
  ((impl -> trait254) = (& Trait254_S432child1_trait254));
  return impl;
} 
Trait254* Trait254_S432child1_ = newTrait254_S432child1();

int S433_classId = Class_genId();
struct S433{

  int id;
  S433 ():id(S433_classId){

  }
};


int S433child1_classId = Class_genId();
struct S433child1:S433{

  S433child1 (int a):a(a){
    (id = S433child1_classId);
{

    }
  }
  int a;
};


struct Trait433{

  int(*trait433)(Class*);
};

Vec* Trait433_v = newVec();

int Trait433_S433child1_trait433(Class* self_) {
  S433child1* self = ((S433child1*)self_);
{
    return (self -> a);
  }
} 
Trait433* newTrait433_S433child1() {
  Trait433 (* impl) = (new Trait433());
  setVec(Trait433_v, S433child1_classId, ((void*)impl));
  ((impl -> trait433) = (& Trait433_S433child1_trait433));
  return impl;
} 
Trait433* Trait433_S433child1_ = newTrait433_S433child1();

int Trait262_S433child1_trait262(Class* self_) {
  S433child1* self = ((S433child1*)self_);
{
    return (self -> a);
  }
} 
Trait262* newTrait262_S433child1() {
  Trait262 (* impl) = (new Trait262());
  setVec(Trait262_v, S433child1_classId, ((void*)impl));
  ((impl -> trait262) = (& Trait262_S433child1_trait262));
  return impl;
} 
Trait262* Trait262_S433child1_ = newTrait262_S433child1();

int S434_classId = Class_genId();
struct S434{

  int id;
  S434 ():id(S434_classId){

  }
};


int S434child1_classId = Class_genId();
struct S434child1:S434{

  S434child1 (int a):a(a){
    (id = S434child1_classId);
{

    }
  }
  int a;
};


int S434child2_classId = Class_genId();
struct S434child2:S434{

  S434child2 (int a):a(a){
    (id = S434child2_classId);
{

    }
  }
  int a;
};


int S434child3_classId = Class_genId();
struct S434child3:S434{

  S434child3 (int a):a(a){
    (id = S434child3_classId);
{

    }
  }
  int a;
};


struct Trait434{

  int(*trait434)(Class*);
};

Vec* Trait434_v = newVec();

int Trait434_S434child1_trait434(Class* self_) {
  S434child1* self = ((S434child1*)self_);
{
    return (self -> a);
  }
} 
Trait434* newTrait434_S434child1() {
  Trait434 (* impl) = (new Trait434());
  setVec(Trait434_v, S434child1_classId, ((void*)impl));
  ((impl -> trait434) = (& Trait434_S434child1_trait434));
  return impl;
} 
Trait434* Trait434_S434child1_ = newTrait434_S434child1();

int Trait434_S434child2_trait434(Class* self_) {
  S434child2* self = ((S434child2*)self_);
{
    return (self -> a);
  }
} 
Trait434* newTrait434_S434child2() {
  Trait434 (* impl) = (new Trait434());
  setVec(Trait434_v, S434child2_classId, ((void*)impl));
  ((impl -> trait434) = (& Trait434_S434child2_trait434));
  return impl;
} 
Trait434* Trait434_S434child2_ = newTrait434_S434child2();

int Trait425_S434child1_trait425(Class* self_) {
  S434child1* self = ((S434child1*)self_);
{
    return (self -> a);
  }
} 
Trait425* newTrait425_S434child1() {
  Trait425 (* impl) = (new Trait425());
  setVec(Trait425_v, S434child1_classId, ((void*)impl));
  ((impl -> trait425) = (& Trait425_S434child1_trait425));
  return impl;
} 
Trait425* Trait425_S434child1_ = newTrait425_S434child1();

int S435_classId = Class_genId();
struct S435{

  int id;
  S435 ():id(S435_classId){

  }
};


int S435child1_classId = Class_genId();
struct S435child1:S435{

  S435child1 (int a):a(a){
    (id = S435child1_classId);
{

    }
  }
  int a;
};


struct Trait435{

  int(*trait435)(Class*);
};

Vec* Trait435_v = newVec();

int Trait435_S435child1_trait435(Class* self_) {
  S435child1* self = ((S435child1*)self_);
{
    return (self -> a);
  }
} 
Trait435* newTrait435_S435child1() {
  Trait435 (* impl) = (new Trait435());
  setVec(Trait435_v, S435child1_classId, ((void*)impl));
  ((impl -> trait435) = (& Trait435_S435child1_trait435));
  return impl;
} 
Trait435* Trait435_S435child1_ = newTrait435_S435child1();

int Trait68_S435child1_trait68(Class* self_) {
  S435child1* self = ((S435child1*)self_);
{
    return (self -> a);
  }
} 
Trait68* newTrait68_S435child1() {
  Trait68 (* impl) = (new Trait68());
  setVec(Trait68_v, S435child1_classId, ((void*)impl));
  ((impl -> trait68) = (& Trait68_S435child1_trait68));
  return impl;
} 
Trait68* Trait68_S435child1_ = newTrait68_S435child1();

int S436_classId = Class_genId();
struct S436{

  int id;
  S436 ():id(S436_classId){

  }
};


int S436child1_classId = Class_genId();
struct S436child1:S436{

  S436child1 (int a):a(a){
    (id = S436child1_classId);
{

    }
  }
  int a;
};


struct Trait436{

  int(*trait436)(Class*);
};

Vec* Trait436_v = newVec();

int Trait436_S436child1_trait436(Class* self_) {
  S436child1* self = ((S436child1*)self_);
{
    return (self -> a);
  }
} 
Trait436* newTrait436_S436child1() {
  Trait436 (* impl) = (new Trait436());
  setVec(Trait436_v, S436child1_classId, ((void*)impl));
  ((impl -> trait436) = (& Trait436_S436child1_trait436));
  return impl;
} 
Trait436* Trait436_S436child1_ = newTrait436_S436child1();

int Trait71_S436child1_trait71(Class* self_) {
  S436child1* self = ((S436child1*)self_);
{
    return (self -> a);
  }
} 
Trait71* newTrait71_S436child1() {
  Trait71 (* impl) = (new Trait71());
  setVec(Trait71_v, S436child1_classId, ((void*)impl));
  ((impl -> trait71) = (& Trait71_S436child1_trait71));
  return impl;
} 
Trait71* Trait71_S436child1_ = newTrait71_S436child1();

int S437_classId = Class_genId();
struct S437{

  int id;
  S437 ():id(S437_classId){

  }
};


int S437child1_classId = Class_genId();
struct S437child1:S437{

  S437child1 (int a):a(a){
    (id = S437child1_classId);
{

    }
  }
  int a;
};


int S437child2_classId = Class_genId();
struct S437child2:S437{

  S437child2 (int a):a(a){
    (id = S437child2_classId);
{

    }
  }
  int a;
};


int S437child3_classId = Class_genId();
struct S437child3:S437{

  S437child3 (int a):a(a){
    (id = S437child3_classId);
{

    }
  }
  int a;
};


int S437child4_classId = Class_genId();
struct S437child4:S437{

  S437child4 (int a):a(a){
    (id = S437child4_classId);
{

    }
  }
  int a;
};


struct Trait437{

  int(*trait437)(Class*);
};

Vec* Trait437_v = newVec();

int Trait437_S437child1_trait437(Class* self_) {
  S437child1* self = ((S437child1*)self_);
{
    return (self -> a);
  }
} 
Trait437* newTrait437_S437child1() {
  Trait437 (* impl) = (new Trait437());
  setVec(Trait437_v, S437child1_classId, ((void*)impl));
  ((impl -> trait437) = (& Trait437_S437child1_trait437));
  return impl;
} 
Trait437* Trait437_S437child1_ = newTrait437_S437child1();

int Trait69_S437child1_trait69(Class* self_) {
  S437child1* self = ((S437child1*)self_);
{
    return (self -> a);
  }
} 
Trait69* newTrait69_S437child1() {
  Trait69 (* impl) = (new Trait69());
  setVec(Trait69_v, S437child1_classId, ((void*)impl));
  ((impl -> trait69) = (& Trait69_S437child1_trait69));
  return impl;
} 
Trait69* Trait69_S437child1_ = newTrait69_S437child1();

int S438_classId = Class_genId();
struct S438{

  int id;
  S438 ():id(S438_classId){

  }
};


int S438child1_classId = Class_genId();
struct S438child1:S438{

  S438child1 (int a):a(a){
    (id = S438child1_classId);
{

    }
  }
  int a;
};


struct Trait438{

  int(*trait438)(Class*);
};

Vec* Trait438_v = newVec();

int Trait438_S438child1_trait438(Class* self_) {
  S438child1* self = ((S438child1*)self_);
{
    return (self -> a);
  }
} 
Trait438* newTrait438_S438child1() {
  Trait438 (* impl) = (new Trait438());
  setVec(Trait438_v, S438child1_classId, ((void*)impl));
  ((impl -> trait438) = (& Trait438_S438child1_trait438));
  return impl;
} 
Trait438* Trait438_S438child1_ = newTrait438_S438child1();

int Trait295_S438child1_trait295(Class* self_) {
  S438child1* self = ((S438child1*)self_);
{
    return (self -> a);
  }
} 
Trait295* newTrait295_S438child1() {
  Trait295 (* impl) = (new Trait295());
  setVec(Trait295_v, S438child1_classId, ((void*)impl));
  ((impl -> trait295) = (& Trait295_S438child1_trait295));
  return impl;
} 
Trait295* Trait295_S438child1_ = newTrait295_S438child1();

int S439_classId = Class_genId();
struct S439{

  int id;
  S439 ():id(S439_classId){

  }
};


int S439child1_classId = Class_genId();
struct S439child1:S439{

  S439child1 (int a):a(a){
    (id = S439child1_classId);
{

    }
  }
  int a;
};


struct Trait439{

  int(*trait439)(Class*);
};

Vec* Trait439_v = newVec();

int Trait439_S439child1_trait439(Class* self_) {
  S439child1* self = ((S439child1*)self_);
{
    return (self -> a);
  }
} 
Trait439* newTrait439_S439child1() {
  Trait439 (* impl) = (new Trait439());
  setVec(Trait439_v, S439child1_classId, ((void*)impl));
  ((impl -> trait439) = (& Trait439_S439child1_trait439));
  return impl;
} 
Trait439* Trait439_S439child1_ = newTrait439_S439child1();

int Trait194_S439child1_trait194(Class* self_) {
  S439child1* self = ((S439child1*)self_);
{
    return (self -> a);
  }
} 
Trait194* newTrait194_S439child1() {
  Trait194 (* impl) = (new Trait194());
  setVec(Trait194_v, S439child1_classId, ((void*)impl));
  ((impl -> trait194) = (& Trait194_S439child1_trait194));
  return impl;
} 
Trait194* Trait194_S439child1_ = newTrait194_S439child1();

int S440_classId = Class_genId();
struct S440{

  int id;
  S440 ():id(S440_classId){

  }
};


int S440child1_classId = Class_genId();
struct S440child1:S440{

  S440child1 (int a):a(a){
    (id = S440child1_classId);
{

    }
  }
  int a;
};


int S440child2_classId = Class_genId();
struct S440child2:S440{

  S440child2 (int a):a(a){
    (id = S440child2_classId);
{

    }
  }
  int a;
};


int S440child3_classId = Class_genId();
struct S440child3:S440{

  S440child3 (int a):a(a){
    (id = S440child3_classId);
{

    }
  }
  int a;
};


int S440child4_classId = Class_genId();
struct S440child4:S440{

  S440child4 (int a):a(a){
    (id = S440child4_classId);
{

    }
  }
  int a;
};


int S440child5_classId = Class_genId();
struct S440child5:S440{

  S440child5 (int a):a(a){
    (id = S440child5_classId);
{

    }
  }
  int a;
};


struct Trait440{

  int(*trait440)(Class*);
};

Vec* Trait440_v = newVec();

int Trait440_S440child1_trait440(Class* self_) {
  S440child1* self = ((S440child1*)self_);
{
    return (self -> a);
  }
} 
Trait440* newTrait440_S440child1() {
  Trait440 (* impl) = (new Trait440());
  setVec(Trait440_v, S440child1_classId, ((void*)impl));
  ((impl -> trait440) = (& Trait440_S440child1_trait440));
  return impl;
} 
Trait440* Trait440_S440child1_ = newTrait440_S440child1();

int Trait105_S440child1_trait105(Class* self_) {
  S440child1* self = ((S440child1*)self_);
{
    return (self -> a);
  }
} 
Trait105* newTrait105_S440child1() {
  Trait105 (* impl) = (new Trait105());
  setVec(Trait105_v, S440child1_classId, ((void*)impl));
  ((impl -> trait105) = (& Trait105_S440child1_trait105));
  return impl;
} 
Trait105* Trait105_S440child1_ = newTrait105_S440child1();

int S441_classId = Class_genId();
struct S441{

  int id;
  S441 ():id(S441_classId){

  }
};


int S441child1_classId = Class_genId();
struct S441child1:S441{

  S441child1 (int a):a(a){
    (id = S441child1_classId);
{

    }
  }
  int a;
};


int S441child2_classId = Class_genId();
struct S441child2:S441{

  S441child2 (int a):a(a){
    (id = S441child2_classId);
{

    }
  }
  int a;
};


int S441child3_classId = Class_genId();
struct S441child3:S441{

  S441child3 (int a):a(a){
    (id = S441child3_classId);
{

    }
  }
  int a;
};


struct Trait441{

  int(*trait441)(Class*);
};

Vec* Trait441_v = newVec();

int Trait441_S441child1_trait441(Class* self_) {
  S441child1* self = ((S441child1*)self_);
{
    return (self -> a);
  }
} 
Trait441* newTrait441_S441child1() {
  Trait441 (* impl) = (new Trait441());
  setVec(Trait441_v, S441child1_classId, ((void*)impl));
  ((impl -> trait441) = (& Trait441_S441child1_trait441));
  return impl;
} 
Trait441* Trait441_S441child1_ = newTrait441_S441child1();

int Trait63_S441child1_trait63(Class* self_) {
  S441child1* self = ((S441child1*)self_);
{
    return (self -> a);
  }
} 
Trait63* newTrait63_S441child1() {
  Trait63 (* impl) = (new Trait63());
  setVec(Trait63_v, S441child1_classId, ((void*)impl));
  ((impl -> trait63) = (& Trait63_S441child1_trait63));
  return impl;
} 
Trait63* Trait63_S441child1_ = newTrait63_S441child1();

int S442_classId = Class_genId();
struct S442{

  int id;
  S442 ():id(S442_classId){

  }
};


int S442child1_classId = Class_genId();
struct S442child1:S442{

  S442child1 (int a):a(a){
    (id = S442child1_classId);
{

    }
  }
  int a;
};


int S442child2_classId = Class_genId();
struct S442child2:S442{

  S442child2 (int a):a(a){
    (id = S442child2_classId);
{

    }
  }
  int a;
};


struct Trait442{

  int(*trait442)(Class*);
};

Vec* Trait442_v = newVec();

int Trait442_S442child1_trait442(Class* self_) {
  S442child1* self = ((S442child1*)self_);
{
    return (self -> a);
  }
} 
Trait442* newTrait442_S442child1() {
  Trait442 (* impl) = (new Trait442());
  setVec(Trait442_v, S442child1_classId, ((void*)impl));
  ((impl -> trait442) = (& Trait442_S442child1_trait442));
  return impl;
} 
Trait442* Trait442_S442child1_ = newTrait442_S442child1();

int Trait442_S442child2_trait442(Class* self_) {
  S442child2* self = ((S442child2*)self_);
{
    return (self -> a);
  }
} 
Trait442* newTrait442_S442child2() {
  Trait442 (* impl) = (new Trait442());
  setVec(Trait442_v, S442child2_classId, ((void*)impl));
  ((impl -> trait442) = (& Trait442_S442child2_trait442));
  return impl;
} 
Trait442* Trait442_S442child2_ = newTrait442_S442child2();

int Trait290_S442child1_trait290(Class* self_) {
  S442child1* self = ((S442child1*)self_);
{
    return (self -> a);
  }
} 
Trait290* newTrait290_S442child1() {
  Trait290 (* impl) = (new Trait290());
  setVec(Trait290_v, S442child1_classId, ((void*)impl));
  ((impl -> trait290) = (& Trait290_S442child1_trait290));
  return impl;
} 
Trait290* Trait290_S442child1_ = newTrait290_S442child1();

int S443_classId = Class_genId();
struct S443{

  int id;
  S443 ():id(S443_classId){

  }
};


int S443child1_classId = Class_genId();
struct S443child1:S443{

  S443child1 (int a):a(a){
    (id = S443child1_classId);
{

    }
  }
  int a;
};


int S443child2_classId = Class_genId();
struct S443child2:S443{

  S443child2 (int a):a(a){
    (id = S443child2_classId);
{

    }
  }
  int a;
};


struct Trait443{

  int(*trait443)(Class*);
};

Vec* Trait443_v = newVec();

int Trait443_S443child1_trait443(Class* self_) {
  S443child1* self = ((S443child1*)self_);
{
    return (self -> a);
  }
} 
Trait443* newTrait443_S443child1() {
  Trait443 (* impl) = (new Trait443());
  setVec(Trait443_v, S443child1_classId, ((void*)impl));
  ((impl -> trait443) = (& Trait443_S443child1_trait443));
  return impl;
} 
Trait443* Trait443_S443child1_ = newTrait443_S443child1();

int Trait255_S443child1_trait255(Class* self_) {
  S443child1* self = ((S443child1*)self_);
{
    return (self -> a);
  }
} 
Trait255* newTrait255_S443child1() {
  Trait255 (* impl) = (new Trait255());
  setVec(Trait255_v, S443child1_classId, ((void*)impl));
  ((impl -> trait255) = (& Trait255_S443child1_trait255));
  return impl;
} 
Trait255* Trait255_S443child1_ = newTrait255_S443child1();

int S444_classId = Class_genId();
struct S444{

  int id;
  S444 ():id(S444_classId){

  }
};


int S444child1_classId = Class_genId();
struct S444child1:S444{

  S444child1 (int a):a(a){
    (id = S444child1_classId);
{

    }
  }
  int a;
};


int S444child2_classId = Class_genId();
struct S444child2:S444{

  S444child2 (int a):a(a){
    (id = S444child2_classId);
{

    }
  }
  int a;
};


int S444child3_classId = Class_genId();
struct S444child3:S444{

  S444child3 (int a):a(a){
    (id = S444child3_classId);
{

    }
  }
  int a;
};


int S444child4_classId = Class_genId();
struct S444child4:S444{

  S444child4 (int a):a(a){
    (id = S444child4_classId);
{

    }
  }
  int a;
};


struct Trait444{

  int(*trait444)(Class*);
};

Vec* Trait444_v = newVec();

int Trait444_S444child1_trait444(Class* self_) {
  S444child1* self = ((S444child1*)self_);
{
    return (self -> a);
  }
} 
Trait444* newTrait444_S444child1() {
  Trait444 (* impl) = (new Trait444());
  setVec(Trait444_v, S444child1_classId, ((void*)impl));
  ((impl -> trait444) = (& Trait444_S444child1_trait444));
  return impl;
} 
Trait444* Trait444_S444child1_ = newTrait444_S444child1();

int Trait444_S444child2_trait444(Class* self_) {
  S444child2* self = ((S444child2*)self_);
{
    return (self -> a);
  }
} 
Trait444* newTrait444_S444child2() {
  Trait444 (* impl) = (new Trait444());
  setVec(Trait444_v, S444child2_classId, ((void*)impl));
  ((impl -> trait444) = (& Trait444_S444child2_trait444));
  return impl;
} 
Trait444* Trait444_S444child2_ = newTrait444_S444child2();

int Trait444_S444child3_trait444(Class* self_) {
  S444child3* self = ((S444child3*)self_);
{
    return (self -> a);
  }
} 
Trait444* newTrait444_S444child3() {
  Trait444 (* impl) = (new Trait444());
  setVec(Trait444_v, S444child3_classId, ((void*)impl));
  ((impl -> trait444) = (& Trait444_S444child3_trait444));
  return impl;
} 
Trait444* Trait444_S444child3_ = newTrait444_S444child3();

int Trait331_S444child1_trait331(Class* self_) {
  S444child1* self = ((S444child1*)self_);
{
    return (self -> a);
  }
} 
Trait331* newTrait331_S444child1() {
  Trait331 (* impl) = (new Trait331());
  setVec(Trait331_v, S444child1_classId, ((void*)impl));
  ((impl -> trait331) = (& Trait331_S444child1_trait331));
  return impl;
} 
Trait331* Trait331_S444child1_ = newTrait331_S444child1();

int S445_classId = Class_genId();
struct S445{

  int id;
  S445 ():id(S445_classId){

  }
};


int S445child1_classId = Class_genId();
struct S445child1:S445{

  S445child1 (int a):a(a){
    (id = S445child1_classId);
{

    }
  }
  int a;
};


int S445child2_classId = Class_genId();
struct S445child2:S445{

  S445child2 (int a):a(a){
    (id = S445child2_classId);
{

    }
  }
  int a;
};


struct Trait445{

  int(*trait445)(Class*);
};

Vec* Trait445_v = newVec();

int Trait445_S445child1_trait445(Class* self_) {
  S445child1* self = ((S445child1*)self_);
{
    return (self -> a);
  }
} 
Trait445* newTrait445_S445child1() {
  Trait445 (* impl) = (new Trait445());
  setVec(Trait445_v, S445child1_classId, ((void*)impl));
  ((impl -> trait445) = (& Trait445_S445child1_trait445));
  return impl;
} 
Trait445* Trait445_S445child1_ = newTrait445_S445child1();

int Trait445_S445child2_trait445(Class* self_) {
  S445child2* self = ((S445child2*)self_);
{
    return (self -> a);
  }
} 
Trait445* newTrait445_S445child2() {
  Trait445 (* impl) = (new Trait445());
  setVec(Trait445_v, S445child2_classId, ((void*)impl));
  ((impl -> trait445) = (& Trait445_S445child2_trait445));
  return impl;
} 
Trait445* Trait445_S445child2_ = newTrait445_S445child2();

int Trait311_S445child1_trait311(Class* self_) {
  S445child1* self = ((S445child1*)self_);
{
    return (self -> a);
  }
} 
Trait311* newTrait311_S445child1() {
  Trait311 (* impl) = (new Trait311());
  setVec(Trait311_v, S445child1_classId, ((void*)impl));
  ((impl -> trait311) = (& Trait311_S445child1_trait311));
  return impl;
} 
Trait311* Trait311_S445child1_ = newTrait311_S445child1();

int S446_classId = Class_genId();
struct S446{

  int id;
  S446 ():id(S446_classId){

  }
};


int S446child1_classId = Class_genId();
struct S446child1:S446{

  S446child1 (int a):a(a){
    (id = S446child1_classId);
{

    }
  }
  int a;
};


int S446child2_classId = Class_genId();
struct S446child2:S446{

  S446child2 (int a):a(a){
    (id = S446child2_classId);
{

    }
  }
  int a;
};


struct Trait446{

  int(*trait446)(Class*);
};

Vec* Trait446_v = newVec();

int Trait446_S446child1_trait446(Class* self_) {
  S446child1* self = ((S446child1*)self_);
{
    return (self -> a);
  }
} 
Trait446* newTrait446_S446child1() {
  Trait446 (* impl) = (new Trait446());
  setVec(Trait446_v, S446child1_classId, ((void*)impl));
  ((impl -> trait446) = (& Trait446_S446child1_trait446));
  return impl;
} 
Trait446* Trait446_S446child1_ = newTrait446_S446child1();

int Trait446_S446child2_trait446(Class* self_) {
  S446child2* self = ((S446child2*)self_);
{
    return (self -> a);
  }
} 
Trait446* newTrait446_S446child2() {
  Trait446 (* impl) = (new Trait446());
  setVec(Trait446_v, S446child2_classId, ((void*)impl));
  ((impl -> trait446) = (& Trait446_S446child2_trait446));
  return impl;
} 
Trait446* Trait446_S446child2_ = newTrait446_S446child2();

int Trait14_S446child1_trait14(Class* self_) {
  S446child1* self = ((S446child1*)self_);
{
    return (self -> a);
  }
} 
Trait14* newTrait14_S446child1() {
  Trait14 (* impl) = (new Trait14());
  setVec(Trait14_v, S446child1_classId, ((void*)impl));
  ((impl -> trait14) = (& Trait14_S446child1_trait14));
  return impl;
} 
Trait14* Trait14_S446child1_ = newTrait14_S446child1();

int S447_classId = Class_genId();
struct S447{

  int id;
  S447 ():id(S447_classId){

  }
};


int S447child1_classId = Class_genId();
struct S447child1:S447{

  S447child1 (int a):a(a){
    (id = S447child1_classId);
{

    }
  }
  int a;
};


int S447child2_classId = Class_genId();
struct S447child2:S447{

  S447child2 (int a):a(a){
    (id = S447child2_classId);
{

    }
  }
  int a;
};


struct Trait447{

  int(*trait447)(Class*);
};

Vec* Trait447_v = newVec();

int Trait447_S447child1_trait447(Class* self_) {
  S447child1* self = ((S447child1*)self_);
{
    return (self -> a);
  }
} 
Trait447* newTrait447_S447child1() {
  Trait447 (* impl) = (new Trait447());
  setVec(Trait447_v, S447child1_classId, ((void*)impl));
  ((impl -> trait447) = (& Trait447_S447child1_trait447));
  return impl;
} 
Trait447* Trait447_S447child1_ = newTrait447_S447child1();

int Trait447_S447child2_trait447(Class* self_) {
  S447child2* self = ((S447child2*)self_);
{
    return (self -> a);
  }
} 
Trait447* newTrait447_S447child2() {
  Trait447 (* impl) = (new Trait447());
  setVec(Trait447_v, S447child2_classId, ((void*)impl));
  ((impl -> trait447) = (& Trait447_S447child2_trait447));
  return impl;
} 
Trait447* Trait447_S447child2_ = newTrait447_S447child2();

int Trait239_S447child1_trait239(Class* self_) {
  S447child1* self = ((S447child1*)self_);
{
    return (self -> a);
  }
} 
Trait239* newTrait239_S447child1() {
  Trait239 (* impl) = (new Trait239());
  setVec(Trait239_v, S447child1_classId, ((void*)impl));
  ((impl -> trait239) = (& Trait239_S447child1_trait239));
  return impl;
} 
Trait239* Trait239_S447child1_ = newTrait239_S447child1();

int S448_classId = Class_genId();
struct S448{

  int id;
  S448 ():id(S448_classId){

  }
};


int S448child1_classId = Class_genId();
struct S448child1:S448{

  S448child1 (int a):a(a){
    (id = S448child1_classId);
{

    }
  }
  int a;
};


int S448child2_classId = Class_genId();
struct S448child2:S448{

  S448child2 (int a):a(a){
    (id = S448child2_classId);
{

    }
  }
  int a;
};


struct Trait448{

  int(*trait448)(Class*);
};

Vec* Trait448_v = newVec();

int Trait448_S448child1_trait448(Class* self_) {
  S448child1* self = ((S448child1*)self_);
{
    return (self -> a);
  }
} 
Trait448* newTrait448_S448child1() {
  Trait448 (* impl) = (new Trait448());
  setVec(Trait448_v, S448child1_classId, ((void*)impl));
  ((impl -> trait448) = (& Trait448_S448child1_trait448));
  return impl;
} 
Trait448* Trait448_S448child1_ = newTrait448_S448child1();

int Trait448_S448child2_trait448(Class* self_) {
  S448child2* self = ((S448child2*)self_);
{
    return (self -> a);
  }
} 
Trait448* newTrait448_S448child2() {
  Trait448 (* impl) = (new Trait448());
  setVec(Trait448_v, S448child2_classId, ((void*)impl));
  ((impl -> trait448) = (& Trait448_S448child2_trait448));
  return impl;
} 
Trait448* Trait448_S448child2_ = newTrait448_S448child2();

int Trait59_S448child1_trait59(Class* self_) {
  S448child1* self = ((S448child1*)self_);
{
    return (self -> a);
  }
} 
Trait59* newTrait59_S448child1() {
  Trait59 (* impl) = (new Trait59());
  setVec(Trait59_v, S448child1_classId, ((void*)impl));
  ((impl -> trait59) = (& Trait59_S448child1_trait59));
  return impl;
} 
Trait59* Trait59_S448child1_ = newTrait59_S448child1();

int S449_classId = Class_genId();
struct S449{

  int id;
  S449 ():id(S449_classId){

  }
};


int S449child1_classId = Class_genId();
struct S449child1:S449{

  S449child1 (int a):a(a){
    (id = S449child1_classId);
{

    }
  }
  int a;
};


struct Trait449{

  int(*trait449)(Class*);
};

Vec* Trait449_v = newVec();

int Trait449_S449child1_trait449(Class* self_) {
  S449child1* self = ((S449child1*)self_);
{
    return (self -> a);
  }
} 
Trait449* newTrait449_S449child1() {
  Trait449 (* impl) = (new Trait449());
  setVec(Trait449_v, S449child1_classId, ((void*)impl));
  ((impl -> trait449) = (& Trait449_S449child1_trait449));
  return impl;
} 
Trait449* Trait449_S449child1_ = newTrait449_S449child1();

int Trait261_S449child1_trait261(Class* self_) {
  S449child1* self = ((S449child1*)self_);
{
    return (self -> a);
  }
} 
Trait261* newTrait261_S449child1() {
  Trait261 (* impl) = (new Trait261());
  setVec(Trait261_v, S449child1_classId, ((void*)impl));
  ((impl -> trait261) = (& Trait261_S449child1_trait261));
  return impl;
} 
Trait261* Trait261_S449child1_ = newTrait261_S449child1();

int S450_classId = Class_genId();
struct S450{

  int id;
  S450 ():id(S450_classId){

  }
};


int S450child1_classId = Class_genId();
struct S450child1:S450{

  S450child1 (int a):a(a){
    (id = S450child1_classId);
{

    }
  }
  int a;
};


int S450child2_classId = Class_genId();
struct S450child2:S450{

  S450child2 (int a):a(a){
    (id = S450child2_classId);
{

    }
  }
  int a;
};


struct Trait450{

  int(*trait450)(Class*);
};

Vec* Trait450_v = newVec();

int Trait450_S450child1_trait450(Class* self_) {
  S450child1* self = ((S450child1*)self_);
{
    return (self -> a);
  }
} 
Trait450* newTrait450_S450child1() {
  Trait450 (* impl) = (new Trait450());
  setVec(Trait450_v, S450child1_classId, ((void*)impl));
  ((impl -> trait450) = (& Trait450_S450child1_trait450));
  return impl;
} 
Trait450* Trait450_S450child1_ = newTrait450_S450child1();

int Trait132_S450child1_trait132(Class* self_) {
  S450child1* self = ((S450child1*)self_);
{
    return (self -> a);
  }
} 
Trait132* newTrait132_S450child1() {
  Trait132 (* impl) = (new Trait132());
  setVec(Trait132_v, S450child1_classId, ((void*)impl));
  ((impl -> trait132) = (& Trait132_S450child1_trait132));
  return impl;
} 
Trait132* Trait132_S450child1_ = newTrait132_S450child1();

int S451_classId = Class_genId();
struct S451{

  int id;
  S451 ():id(S451_classId){

  }
};


int S451child1_classId = Class_genId();
struct S451child1:S451{

  S451child1 (int a):a(a){
    (id = S451child1_classId);
{

    }
  }
  int a;
};


int S451child2_classId = Class_genId();
struct S451child2:S451{

  S451child2 (int a):a(a){
    (id = S451child2_classId);
{

    }
  }
  int a;
};


int S451child3_classId = Class_genId();
struct S451child3:S451{

  S451child3 (int a):a(a){
    (id = S451child3_classId);
{

    }
  }
  int a;
};


struct Trait451{

  int(*trait451)(Class*);
};

Vec* Trait451_v = newVec();

int Trait451_S451child1_trait451(Class* self_) {
  S451child1* self = ((S451child1*)self_);
{
    return (self -> a);
  }
} 
Trait451* newTrait451_S451child1() {
  Trait451 (* impl) = (new Trait451());
  setVec(Trait451_v, S451child1_classId, ((void*)impl));
  ((impl -> trait451) = (& Trait451_S451child1_trait451));
  return impl;
} 
Trait451* Trait451_S451child1_ = newTrait451_S451child1();

int Trait451_S451child2_trait451(Class* self_) {
  S451child2* self = ((S451child2*)self_);
{
    return (self -> a);
  }
} 
Trait451* newTrait451_S451child2() {
  Trait451 (* impl) = (new Trait451());
  setVec(Trait451_v, S451child2_classId, ((void*)impl));
  ((impl -> trait451) = (& Trait451_S451child2_trait451));
  return impl;
} 
Trait451* Trait451_S451child2_ = newTrait451_S451child2();

int Trait451_S451child3_trait451(Class* self_) {
  S451child3* self = ((S451child3*)self_);
{
    return (self -> a);
  }
} 
Trait451* newTrait451_S451child3() {
  Trait451 (* impl) = (new Trait451());
  setVec(Trait451_v, S451child3_classId, ((void*)impl));
  ((impl -> trait451) = (& Trait451_S451child3_trait451));
  return impl;
} 
Trait451* Trait451_S451child3_ = newTrait451_S451child3();

int Trait430_S451child1_trait430(Class* self_) {
  S451child1* self = ((S451child1*)self_);
{
    return (self -> a);
  }
} 
Trait430* newTrait430_S451child1() {
  Trait430 (* impl) = (new Trait430());
  setVec(Trait430_v, S451child1_classId, ((void*)impl));
  ((impl -> trait430) = (& Trait430_S451child1_trait430));
  return impl;
} 
Trait430* Trait430_S451child1_ = newTrait430_S451child1();

int S452_classId = Class_genId();
struct S452{

  int id;
  S452 ():id(S452_classId){

  }
};


int S452child1_classId = Class_genId();
struct S452child1:S452{

  S452child1 (int a):a(a){
    (id = S452child1_classId);
{

    }
  }
  int a;
};


int S452child2_classId = Class_genId();
struct S452child2:S452{

  S452child2 (int a):a(a){
    (id = S452child2_classId);
{

    }
  }
  int a;
};


int S452child3_classId = Class_genId();
struct S452child3:S452{

  S452child3 (int a):a(a){
    (id = S452child3_classId);
{

    }
  }
  int a;
};


struct Trait452{

  int(*trait452)(Class*);
};

Vec* Trait452_v = newVec();

int Trait452_S452child1_trait452(Class* self_) {
  S452child1* self = ((S452child1*)self_);
{
    return (self -> a);
  }
} 
Trait452* newTrait452_S452child1() {
  Trait452 (* impl) = (new Trait452());
  setVec(Trait452_v, S452child1_classId, ((void*)impl));
  ((impl -> trait452) = (& Trait452_S452child1_trait452));
  return impl;
} 
Trait452* Trait452_S452child1_ = newTrait452_S452child1();

int Trait276_S452child1_trait276(Class* self_) {
  S452child1* self = ((S452child1*)self_);
{
    return (self -> a);
  }
} 
Trait276* newTrait276_S452child1() {
  Trait276 (* impl) = (new Trait276());
  setVec(Trait276_v, S452child1_classId, ((void*)impl));
  ((impl -> trait276) = (& Trait276_S452child1_trait276));
  return impl;
} 
Trait276* Trait276_S452child1_ = newTrait276_S452child1();

int S453_classId = Class_genId();
struct S453{

  int id;
  S453 ():id(S453_classId){

  }
};


int S453child1_classId = Class_genId();
struct S453child1:S453{

  S453child1 (int a):a(a){
    (id = S453child1_classId);
{

    }
  }
  int a;
};


int S453child2_classId = Class_genId();
struct S453child2:S453{

  S453child2 (int a):a(a){
    (id = S453child2_classId);
{

    }
  }
  int a;
};


struct Trait453{

  int(*trait453)(Class*);
};

Vec* Trait453_v = newVec();

int Trait453_S453child1_trait453(Class* self_) {
  S453child1* self = ((S453child1*)self_);
{
    return (self -> a);
  }
} 
Trait453* newTrait453_S453child1() {
  Trait453 (* impl) = (new Trait453());
  setVec(Trait453_v, S453child1_classId, ((void*)impl));
  ((impl -> trait453) = (& Trait453_S453child1_trait453));
  return impl;
} 
Trait453* Trait453_S453child1_ = newTrait453_S453child1();

int Trait453_S453child2_trait453(Class* self_) {
  S453child2* self = ((S453child2*)self_);
{
    return (self -> a);
  }
} 
Trait453* newTrait453_S453child2() {
  Trait453 (* impl) = (new Trait453());
  setVec(Trait453_v, S453child2_classId, ((void*)impl));
  ((impl -> trait453) = (& Trait453_S453child2_trait453));
  return impl;
} 
Trait453* Trait453_S453child2_ = newTrait453_S453child2();

int Trait418_S453child1_trait418(Class* self_) {
  S453child1* self = ((S453child1*)self_);
{
    return (self -> a);
  }
} 
Trait418* newTrait418_S453child1() {
  Trait418 (* impl) = (new Trait418());
  setVec(Trait418_v, S453child1_classId, ((void*)impl));
  ((impl -> trait418) = (& Trait418_S453child1_trait418));
  return impl;
} 
Trait418* Trait418_S453child1_ = newTrait418_S453child1();

int S454_classId = Class_genId();
struct S454{

  int id;
  S454 ():id(S454_classId){

  }
};


int S454child1_classId = Class_genId();
struct S454child1:S454{

  S454child1 (int a):a(a){
    (id = S454child1_classId);
{

    }
  }
  int a;
};


int S454child2_classId = Class_genId();
struct S454child2:S454{

  S454child2 (int a):a(a){
    (id = S454child2_classId);
{

    }
  }
  int a;
};


struct Trait454{

  int(*trait454)(Class*);
};

Vec* Trait454_v = newVec();

int Trait454_S454child1_trait454(Class* self_) {
  S454child1* self = ((S454child1*)self_);
{
    return (self -> a);
  }
} 
Trait454* newTrait454_S454child1() {
  Trait454 (* impl) = (new Trait454());
  setVec(Trait454_v, S454child1_classId, ((void*)impl));
  ((impl -> trait454) = (& Trait454_S454child1_trait454));
  return impl;
} 
Trait454* Trait454_S454child1_ = newTrait454_S454child1();

int Trait416_S454child1_trait416(Class* self_) {
  S454child1* self = ((S454child1*)self_);
{
    return (self -> a);
  }
} 
Trait416* newTrait416_S454child1() {
  Trait416 (* impl) = (new Trait416());
  setVec(Trait416_v, S454child1_classId, ((void*)impl));
  ((impl -> trait416) = (& Trait416_S454child1_trait416));
  return impl;
} 
Trait416* Trait416_S454child1_ = newTrait416_S454child1();

int S455_classId = Class_genId();
struct S455{

  int id;
  S455 ():id(S455_classId){

  }
};


int S455child1_classId = Class_genId();
struct S455child1:S455{

  S455child1 (int a):a(a){
    (id = S455child1_classId);
{

    }
  }
  int a;
};


int S455child2_classId = Class_genId();
struct S455child2:S455{

  S455child2 (int a):a(a){
    (id = S455child2_classId);
{

    }
  }
  int a;
};


struct Trait455{

  int(*trait455)(Class*);
};

Vec* Trait455_v = newVec();

int Trait455_S455child1_trait455(Class* self_) {
  S455child1* self = ((S455child1*)self_);
{
    return (self -> a);
  }
} 
Trait455* newTrait455_S455child1() {
  Trait455 (* impl) = (new Trait455());
  setVec(Trait455_v, S455child1_classId, ((void*)impl));
  ((impl -> trait455) = (& Trait455_S455child1_trait455));
  return impl;
} 
Trait455* Trait455_S455child1_ = newTrait455_S455child1();

int Trait230_S455child1_trait230(Class* self_) {
  S455child1* self = ((S455child1*)self_);
{
    return (self -> a);
  }
} 
Trait230* newTrait230_S455child1() {
  Trait230 (* impl) = (new Trait230());
  setVec(Trait230_v, S455child1_classId, ((void*)impl));
  ((impl -> trait230) = (& Trait230_S455child1_trait230));
  return impl;
} 
Trait230* Trait230_S455child1_ = newTrait230_S455child1();

int S456_classId = Class_genId();
struct S456{

  int id;
  S456 ():id(S456_classId){

  }
};


int S456child1_classId = Class_genId();
struct S456child1:S456{

  S456child1 (int a):a(a){
    (id = S456child1_classId);
{

    }
  }
  int a;
};


int S456child2_classId = Class_genId();
struct S456child2:S456{

  S456child2 (int a):a(a){
    (id = S456child2_classId);
{

    }
  }
  int a;
};


int S456child3_classId = Class_genId();
struct S456child3:S456{

  S456child3 (int a):a(a){
    (id = S456child3_classId);
{

    }
  }
  int a;
};


struct Trait456{

  int(*trait456)(Class*);
};

Vec* Trait456_v = newVec();

int Trait456_S456child1_trait456(Class* self_) {
  S456child1* self = ((S456child1*)self_);
{
    return (self -> a);
  }
} 
Trait456* newTrait456_S456child1() {
  Trait456 (* impl) = (new Trait456());
  setVec(Trait456_v, S456child1_classId, ((void*)impl));
  ((impl -> trait456) = (& Trait456_S456child1_trait456));
  return impl;
} 
Trait456* Trait456_S456child1_ = newTrait456_S456child1();

int Trait444_S456child1_trait444(Class* self_) {
  S456child1* self = ((S456child1*)self_);
{
    return (self -> a);
  }
} 
Trait444* newTrait444_S456child1() {
  Trait444 (* impl) = (new Trait444());
  setVec(Trait444_v, S456child1_classId, ((void*)impl));
  ((impl -> trait444) = (& Trait444_S456child1_trait444));
  return impl;
} 
Trait444* Trait444_S456child1_ = newTrait444_S456child1();

int S457_classId = Class_genId();
struct S457{

  int id;
  S457 ():id(S457_classId){

  }
};


int S457child1_classId = Class_genId();
struct S457child1:S457{

  S457child1 (int a):a(a){
    (id = S457child1_classId);
{

    }
  }
  int a;
};


int S457child2_classId = Class_genId();
struct S457child2:S457{

  S457child2 (int a):a(a){
    (id = S457child2_classId);
{

    }
  }
  int a;
};


struct Trait457{

  int(*trait457)(Class*);
};

Vec* Trait457_v = newVec();

int Trait457_S457child1_trait457(Class* self_) {
  S457child1* self = ((S457child1*)self_);
{
    return (self -> a);
  }
} 
Trait457* newTrait457_S457child1() {
  Trait457 (* impl) = (new Trait457());
  setVec(Trait457_v, S457child1_classId, ((void*)impl));
  ((impl -> trait457) = (& Trait457_S457child1_trait457));
  return impl;
} 
Trait457* Trait457_S457child1_ = newTrait457_S457child1();

int Trait135_S457child1_trait135(Class* self_) {
  S457child1* self = ((S457child1*)self_);
{
    return (self -> a);
  }
} 
Trait135* newTrait135_S457child1() {
  Trait135 (* impl) = (new Trait135());
  setVec(Trait135_v, S457child1_classId, ((void*)impl));
  ((impl -> trait135) = (& Trait135_S457child1_trait135));
  return impl;
} 
Trait135* Trait135_S457child1_ = newTrait135_S457child1();

int S458_classId = Class_genId();
struct S458{

  int id;
  S458 ():id(S458_classId){

  }
};


int S458child1_classId = Class_genId();
struct S458child1:S458{

  S458child1 (int a):a(a){
    (id = S458child1_classId);
{

    }
  }
  int a;
};


int S458child2_classId = Class_genId();
struct S458child2:S458{

  S458child2 (int a):a(a){
    (id = S458child2_classId);
{

    }
  }
  int a;
};


struct Trait458{

  int(*trait458)(Class*);
};

Vec* Trait458_v = newVec();

int Trait458_S458child1_trait458(Class* self_) {
  S458child1* self = ((S458child1*)self_);
{
    return (self -> a);
  }
} 
Trait458* newTrait458_S458child1() {
  Trait458 (* impl) = (new Trait458());
  setVec(Trait458_v, S458child1_classId, ((void*)impl));
  ((impl -> trait458) = (& Trait458_S458child1_trait458));
  return impl;
} 
Trait458* Trait458_S458child1_ = newTrait458_S458child1();

int Trait458_S458child2_trait458(Class* self_) {
  S458child2* self = ((S458child2*)self_);
{
    return (self -> a);
  }
} 
Trait458* newTrait458_S458child2() {
  Trait458 (* impl) = (new Trait458());
  setVec(Trait458_v, S458child2_classId, ((void*)impl));
  ((impl -> trait458) = (& Trait458_S458child2_trait458));
  return impl;
} 
Trait458* Trait458_S458child2_ = newTrait458_S458child2();

int Trait75_S458child1_trait75(Class* self_) {
  S458child1* self = ((S458child1*)self_);
{
    return (self -> a);
  }
} 
Trait75* newTrait75_S458child1() {
  Trait75 (* impl) = (new Trait75());
  setVec(Trait75_v, S458child1_classId, ((void*)impl));
  ((impl -> trait75) = (& Trait75_S458child1_trait75));
  return impl;
} 
Trait75* Trait75_S458child1_ = newTrait75_S458child1();

int S459_classId = Class_genId();
struct S459{

  int id;
  S459 ():id(S459_classId){

  }
};


int S459child1_classId = Class_genId();
struct S459child1:S459{

  S459child1 (int a):a(a){
    (id = S459child1_classId);
{

    }
  }
  int a;
};


int S459child2_classId = Class_genId();
struct S459child2:S459{

  S459child2 (int a):a(a){
    (id = S459child2_classId);
{

    }
  }
  int a;
};


int S459child3_classId = Class_genId();
struct S459child3:S459{

  S459child3 (int a):a(a){
    (id = S459child3_classId);
{

    }
  }
  int a;
};


int S459child4_classId = Class_genId();
struct S459child4:S459{

  S459child4 (int a):a(a){
    (id = S459child4_classId);
{

    }
  }
  int a;
};


struct Trait459{

  int(*trait459)(Class*);
};

Vec* Trait459_v = newVec();

int Trait459_S459child1_trait459(Class* self_) {
  S459child1* self = ((S459child1*)self_);
{
    return (self -> a);
  }
} 
Trait459* newTrait459_S459child1() {
  Trait459 (* impl) = (new Trait459());
  setVec(Trait459_v, S459child1_classId, ((void*)impl));
  ((impl -> trait459) = (& Trait459_S459child1_trait459));
  return impl;
} 
Trait459* Trait459_S459child1_ = newTrait459_S459child1();

int Trait184_S459child1_trait184(Class* self_) {
  S459child1* self = ((S459child1*)self_);
{
    return (self -> a);
  }
} 
Trait184* newTrait184_S459child1() {
  Trait184 (* impl) = (new Trait184());
  setVec(Trait184_v, S459child1_classId, ((void*)impl));
  ((impl -> trait184) = (& Trait184_S459child1_trait184));
  return impl;
} 
Trait184* Trait184_S459child1_ = newTrait184_S459child1();

int S460_classId = Class_genId();
struct S460{

  int id;
  S460 ():id(S460_classId){

  }
};


int S460child1_classId = Class_genId();
struct S460child1:S460{

  S460child1 (int a):a(a){
    (id = S460child1_classId);
{

    }
  }
  int a;
};


int S460child2_classId = Class_genId();
struct S460child2:S460{

  S460child2 (int a):a(a){
    (id = S460child2_classId);
{

    }
  }
  int a;
};


int S460child3_classId = Class_genId();
struct S460child3:S460{

  S460child3 (int a):a(a){
    (id = S460child3_classId);
{

    }
  }
  int a;
};


int S460child4_classId = Class_genId();
struct S460child4:S460{

  S460child4 (int a):a(a){
    (id = S460child4_classId);
{

    }
  }
  int a;
};


struct Trait460{

  int(*trait460)(Class*);
};

Vec* Trait460_v = newVec();

int Trait460_S460child1_trait460(Class* self_) {
  S460child1* self = ((S460child1*)self_);
{
    return (self -> a);
  }
} 
Trait460* newTrait460_S460child1() {
  Trait460 (* impl) = (new Trait460());
  setVec(Trait460_v, S460child1_classId, ((void*)impl));
  ((impl -> trait460) = (& Trait460_S460child1_trait460));
  return impl;
} 
Trait460* Trait460_S460child1_ = newTrait460_S460child1();

int Trait333_S460child1_trait333(Class* self_) {
  S460child1* self = ((S460child1*)self_);
{
    return (self -> a);
  }
} 
Trait333* newTrait333_S460child1() {
  Trait333 (* impl) = (new Trait333());
  setVec(Trait333_v, S460child1_classId, ((void*)impl));
  ((impl -> trait333) = (& Trait333_S460child1_trait333));
  return impl;
} 
Trait333* Trait333_S460child1_ = newTrait333_S460child1();

int S461_classId = Class_genId();
struct S461{

  int id;
  S461 ():id(S461_classId){

  }
};


int S461child1_classId = Class_genId();
struct S461child1:S461{

  S461child1 (int a):a(a){
    (id = S461child1_classId);
{

    }
  }
  int a;
};


int S461child2_classId = Class_genId();
struct S461child2:S461{

  S461child2 (int a):a(a){
    (id = S461child2_classId);
{

    }
  }
  int a;
};


struct Trait461{

  int(*trait461)(Class*);
};

Vec* Trait461_v = newVec();

int Trait461_S461child1_trait461(Class* self_) {
  S461child1* self = ((S461child1*)self_);
{
    return (self -> a);
  }
} 
Trait461* newTrait461_S461child1() {
  Trait461 (* impl) = (new Trait461());
  setVec(Trait461_v, S461child1_classId, ((void*)impl));
  ((impl -> trait461) = (& Trait461_S461child1_trait461));
  return impl;
} 
Trait461* Trait461_S461child1_ = newTrait461_S461child1();

int Trait461_S461child2_trait461(Class* self_) {
  S461child2* self = ((S461child2*)self_);
{
    return (self -> a);
  }
} 
Trait461* newTrait461_S461child2() {
  Trait461 (* impl) = (new Trait461());
  setVec(Trait461_v, S461child2_classId, ((void*)impl));
  ((impl -> trait461) = (& Trait461_S461child2_trait461));
  return impl;
} 
Trait461* Trait461_S461child2_ = newTrait461_S461child2();

int Trait293_S461child1_trait293(Class* self_) {
  S461child1* self = ((S461child1*)self_);
{
    return (self -> a);
  }
} 
Trait293* newTrait293_S461child1() {
  Trait293 (* impl) = (new Trait293());
  setVec(Trait293_v, S461child1_classId, ((void*)impl));
  ((impl -> trait293) = (& Trait293_S461child1_trait293));
  return impl;
} 
Trait293* Trait293_S461child1_ = newTrait293_S461child1();

int S462_classId = Class_genId();
struct S462{

  int id;
  S462 ():id(S462_classId){

  }
};


int S462child1_classId = Class_genId();
struct S462child1:S462{

  S462child1 (int a):a(a){
    (id = S462child1_classId);
{

    }
  }
  int a;
};


int S462child2_classId = Class_genId();
struct S462child2:S462{

  S462child2 (int a):a(a){
    (id = S462child2_classId);
{

    }
  }
  int a;
};


struct Trait462{

  int(*trait462)(Class*);
};

Vec* Trait462_v = newVec();

int Trait462_S462child1_trait462(Class* self_) {
  S462child1* self = ((S462child1*)self_);
{
    return (self -> a);
  }
} 
Trait462* newTrait462_S462child1() {
  Trait462 (* impl) = (new Trait462());
  setVec(Trait462_v, S462child1_classId, ((void*)impl));
  ((impl -> trait462) = (& Trait462_S462child1_trait462));
  return impl;
} 
Trait462* Trait462_S462child1_ = newTrait462_S462child1();

int Trait462_S462child2_trait462(Class* self_) {
  S462child2* self = ((S462child2*)self_);
{
    return (self -> a);
  }
} 
Trait462* newTrait462_S462child2() {
  Trait462 (* impl) = (new Trait462());
  setVec(Trait462_v, S462child2_classId, ((void*)impl));
  ((impl -> trait462) = (& Trait462_S462child2_trait462));
  return impl;
} 
Trait462* Trait462_S462child2_ = newTrait462_S462child2();

int Trait232_S462child1_trait232(Class* self_) {
  S462child1* self = ((S462child1*)self_);
{
    return (self -> a);
  }
} 
Trait232* newTrait232_S462child1() {
  Trait232 (* impl) = (new Trait232());
  setVec(Trait232_v, S462child1_classId, ((void*)impl));
  ((impl -> trait232) = (& Trait232_S462child1_trait232));
  return impl;
} 
Trait232* Trait232_S462child1_ = newTrait232_S462child1();

int S463_classId = Class_genId();
struct S463{

  int id;
  S463 ():id(S463_classId){

  }
};


int S463child1_classId = Class_genId();
struct S463child1:S463{

  S463child1 (int a):a(a){
    (id = S463child1_classId);
{

    }
  }
  int a;
};


int S463child2_classId = Class_genId();
struct S463child2:S463{

  S463child2 (int a):a(a){
    (id = S463child2_classId);
{

    }
  }
  int a;
};


int S463child3_classId = Class_genId();
struct S463child3:S463{

  S463child3 (int a):a(a){
    (id = S463child3_classId);
{

    }
  }
  int a;
};


int S463child4_classId = Class_genId();
struct S463child4:S463{

  S463child4 (int a):a(a){
    (id = S463child4_classId);
{

    }
  }
  int a;
};


struct Trait463{

  int(*trait463)(Class*);
};

Vec* Trait463_v = newVec();

int Trait463_S463child1_trait463(Class* self_) {
  S463child1* self = ((S463child1*)self_);
{
    return (self -> a);
  }
} 
Trait463* newTrait463_S463child1() {
  Trait463 (* impl) = (new Trait463());
  setVec(Trait463_v, S463child1_classId, ((void*)impl));
  ((impl -> trait463) = (& Trait463_S463child1_trait463));
  return impl;
} 
Trait463* Trait463_S463child1_ = newTrait463_S463child1();

int Trait463_S463child2_trait463(Class* self_) {
  S463child2* self = ((S463child2*)self_);
{
    return (self -> a);
  }
} 
Trait463* newTrait463_S463child2() {
  Trait463 (* impl) = (new Trait463());
  setVec(Trait463_v, S463child2_classId, ((void*)impl));
  ((impl -> trait463) = (& Trait463_S463child2_trait463));
  return impl;
} 
Trait463* Trait463_S463child2_ = newTrait463_S463child2();

int Trait161_S463child1_trait161(Class* self_) {
  S463child1* self = ((S463child1*)self_);
{
    return (self -> a);
  }
} 
Trait161* newTrait161_S463child1() {
  Trait161 (* impl) = (new Trait161());
  setVec(Trait161_v, S463child1_classId, ((void*)impl));
  ((impl -> trait161) = (& Trait161_S463child1_trait161));
  return impl;
} 
Trait161* Trait161_S463child1_ = newTrait161_S463child1();

int S464_classId = Class_genId();
struct S464{

  int id;
  S464 ():id(S464_classId){

  }
};


int S464child1_classId = Class_genId();
struct S464child1:S464{

  S464child1 (int a):a(a){
    (id = S464child1_classId);
{

    }
  }
  int a;
};


int S464child2_classId = Class_genId();
struct S464child2:S464{

  S464child2 (int a):a(a){
    (id = S464child2_classId);
{

    }
  }
  int a;
};


struct Trait464{

  int(*trait464)(Class*);
};

Vec* Trait464_v = newVec();

int Trait464_S464child1_trait464(Class* self_) {
  S464child1* self = ((S464child1*)self_);
{
    return (self -> a);
  }
} 
Trait464* newTrait464_S464child1() {
  Trait464 (* impl) = (new Trait464());
  setVec(Trait464_v, S464child1_classId, ((void*)impl));
  ((impl -> trait464) = (& Trait464_S464child1_trait464));
  return impl;
} 
Trait464* Trait464_S464child1_ = newTrait464_S464child1();

int Trait464_S464child2_trait464(Class* self_) {
  S464child2* self = ((S464child2*)self_);
{
    return (self -> a);
  }
} 
Trait464* newTrait464_S464child2() {
  Trait464 (* impl) = (new Trait464());
  setVec(Trait464_v, S464child2_classId, ((void*)impl));
  ((impl -> trait464) = (& Trait464_S464child2_trait464));
  return impl;
} 
Trait464* Trait464_S464child2_ = newTrait464_S464child2();

int Trait455_S464child1_trait455(Class* self_) {
  S464child1* self = ((S464child1*)self_);
{
    return (self -> a);
  }
} 
Trait455* newTrait455_S464child1() {
  Trait455 (* impl) = (new Trait455());
  setVec(Trait455_v, S464child1_classId, ((void*)impl));
  ((impl -> trait455) = (& Trait455_S464child1_trait455));
  return impl;
} 
Trait455* Trait455_S464child1_ = newTrait455_S464child1();

int S465_classId = Class_genId();
struct S465{

  int id;
  S465 ():id(S465_classId){

  }
};


int S465child1_classId = Class_genId();
struct S465child1:S465{

  S465child1 (int a):a(a){
    (id = S465child1_classId);
{

    }
  }
  int a;
};


struct Trait465{

  int(*trait465)(Class*);
};

Vec* Trait465_v = newVec();

int Trait465_S465child1_trait465(Class* self_) {
  S465child1* self = ((S465child1*)self_);
{
    return (self -> a);
  }
} 
Trait465* newTrait465_S465child1() {
  Trait465 (* impl) = (new Trait465());
  setVec(Trait465_v, S465child1_classId, ((void*)impl));
  ((impl -> trait465) = (& Trait465_S465child1_trait465));
  return impl;
} 
Trait465* Trait465_S465child1_ = newTrait465_S465child1();

int Trait239_S465child1_trait239(Class* self_) {
  S465child1* self = ((S465child1*)self_);
{
    return (self -> a);
  }
} 
Trait239* newTrait239_S465child1() {
  Trait239 (* impl) = (new Trait239());
  setVec(Trait239_v, S465child1_classId, ((void*)impl));
  ((impl -> trait239) = (& Trait239_S465child1_trait239));
  return impl;
} 
Trait239* Trait239_S465child1_ = newTrait239_S465child1();

int S466_classId = Class_genId();
struct S466{

  int id;
  S466 ():id(S466_classId){

  }
};


int S466child1_classId = Class_genId();
struct S466child1:S466{

  S466child1 (int a):a(a){
    (id = S466child1_classId);
{

    }
  }
  int a;
};


int S466child2_classId = Class_genId();
struct S466child2:S466{

  S466child2 (int a):a(a){
    (id = S466child2_classId);
{

    }
  }
  int a;
};


struct Trait466{

  int(*trait466)(Class*);
};

Vec* Trait466_v = newVec();

int Trait466_S466child1_trait466(Class* self_) {
  S466child1* self = ((S466child1*)self_);
{
    return (self -> a);
  }
} 
Trait466* newTrait466_S466child1() {
  Trait466 (* impl) = (new Trait466());
  setVec(Trait466_v, S466child1_classId, ((void*)impl));
  ((impl -> trait466) = (& Trait466_S466child1_trait466));
  return impl;
} 
Trait466* Trait466_S466child1_ = newTrait466_S466child1();

int Trait136_S466child1_trait136(Class* self_) {
  S466child1* self = ((S466child1*)self_);
{
    return (self -> a);
  }
} 
Trait136* newTrait136_S466child1() {
  Trait136 (* impl) = (new Trait136());
  setVec(Trait136_v, S466child1_classId, ((void*)impl));
  ((impl -> trait136) = (& Trait136_S466child1_trait136));
  return impl;
} 
Trait136* Trait136_S466child1_ = newTrait136_S466child1();

int S467_classId = Class_genId();
struct S467{

  int id;
  S467 ():id(S467_classId){

  }
};


int S467child1_classId = Class_genId();
struct S467child1:S467{

  S467child1 (int a):a(a){
    (id = S467child1_classId);
{

    }
  }
  int a;
};


int S467child2_classId = Class_genId();
struct S467child2:S467{

  S467child2 (int a):a(a){
    (id = S467child2_classId);
{

    }
  }
  int a;
};


struct Trait467{

  int(*trait467)(Class*);
};

Vec* Trait467_v = newVec();

int Trait467_S467child1_trait467(Class* self_) {
  S467child1* self = ((S467child1*)self_);
{
    return (self -> a);
  }
} 
Trait467* newTrait467_S467child1() {
  Trait467 (* impl) = (new Trait467());
  setVec(Trait467_v, S467child1_classId, ((void*)impl));
  ((impl -> trait467) = (& Trait467_S467child1_trait467));
  return impl;
} 
Trait467* Trait467_S467child1_ = newTrait467_S467child1();

int Trait467_S467child2_trait467(Class* self_) {
  S467child2* self = ((S467child2*)self_);
{
    return (self -> a);
  }
} 
Trait467* newTrait467_S467child2() {
  Trait467 (* impl) = (new Trait467());
  setVec(Trait467_v, S467child2_classId, ((void*)impl));
  ((impl -> trait467) = (& Trait467_S467child2_trait467));
  return impl;
} 
Trait467* Trait467_S467child2_ = newTrait467_S467child2();

int Trait100_S467child1_trait100(Class* self_) {
  S467child1* self = ((S467child1*)self_);
{
    return (self -> a);
  }
} 
Trait100* newTrait100_S467child1() {
  Trait100 (* impl) = (new Trait100());
  setVec(Trait100_v, S467child1_classId, ((void*)impl));
  ((impl -> trait100) = (& Trait100_S467child1_trait100));
  return impl;
} 
Trait100* Trait100_S467child1_ = newTrait100_S467child1();

int S468_classId = Class_genId();
struct S468{

  int id;
  S468 ():id(S468_classId){

  }
};


int S468child1_classId = Class_genId();
struct S468child1:S468{

  S468child1 (int a):a(a){
    (id = S468child1_classId);
{

    }
  }
  int a;
};


int S468child2_classId = Class_genId();
struct S468child2:S468{

  S468child2 (int a):a(a){
    (id = S468child2_classId);
{

    }
  }
  int a;
};


struct Trait468{

  int(*trait468)(Class*);
};

Vec* Trait468_v = newVec();

int Trait468_S468child1_trait468(Class* self_) {
  S468child1* self = ((S468child1*)self_);
{
    return (self -> a);
  }
} 
Trait468* newTrait468_S468child1() {
  Trait468 (* impl) = (new Trait468());
  setVec(Trait468_v, S468child1_classId, ((void*)impl));
  ((impl -> trait468) = (& Trait468_S468child1_trait468));
  return impl;
} 
Trait468* Trait468_S468child1_ = newTrait468_S468child1();

int Trait236_S468child1_trait236(Class* self_) {
  S468child1* self = ((S468child1*)self_);
{
    return (self -> a);
  }
} 
Trait236* newTrait236_S468child1() {
  Trait236 (* impl) = (new Trait236());
  setVec(Trait236_v, S468child1_classId, ((void*)impl));
  ((impl -> trait236) = (& Trait236_S468child1_trait236));
  return impl;
} 
Trait236* Trait236_S468child1_ = newTrait236_S468child1();

int S469_classId = Class_genId();
struct S469{

  int id;
  S469 ():id(S469_classId){

  }
};


int S469child1_classId = Class_genId();
struct S469child1:S469{

  S469child1 (int a):a(a){
    (id = S469child1_classId);
{

    }
  }
  int a;
};


int S469child2_classId = Class_genId();
struct S469child2:S469{

  S469child2 (int a):a(a){
    (id = S469child2_classId);
{

    }
  }
  int a;
};


int S469child3_classId = Class_genId();
struct S469child3:S469{

  S469child3 (int a):a(a){
    (id = S469child3_classId);
{

    }
  }
  int a;
};


int S469child4_classId = Class_genId();
struct S469child4:S469{

  S469child4 (int a):a(a){
    (id = S469child4_classId);
{

    }
  }
  int a;
};


struct Trait469{

  int(*trait469)(Class*);
};

Vec* Trait469_v = newVec();

int Trait469_S469child1_trait469(Class* self_) {
  S469child1* self = ((S469child1*)self_);
{
    return (self -> a);
  }
} 
Trait469* newTrait469_S469child1() {
  Trait469 (* impl) = (new Trait469());
  setVec(Trait469_v, S469child1_classId, ((void*)impl));
  ((impl -> trait469) = (& Trait469_S469child1_trait469));
  return impl;
} 
Trait469* Trait469_S469child1_ = newTrait469_S469child1();

int Trait469_S469child2_trait469(Class* self_) {
  S469child2* self = ((S469child2*)self_);
{
    return (self -> a);
  }
} 
Trait469* newTrait469_S469child2() {
  Trait469 (* impl) = (new Trait469());
  setVec(Trait469_v, S469child2_classId, ((void*)impl));
  ((impl -> trait469) = (& Trait469_S469child2_trait469));
  return impl;
} 
Trait469* Trait469_S469child2_ = newTrait469_S469child2();

int Trait244_S469child1_trait244(Class* self_) {
  S469child1* self = ((S469child1*)self_);
{
    return (self -> a);
  }
} 
Trait244* newTrait244_S469child1() {
  Trait244 (* impl) = (new Trait244());
  setVec(Trait244_v, S469child1_classId, ((void*)impl));
  ((impl -> trait244) = (& Trait244_S469child1_trait244));
  return impl;
} 
Trait244* Trait244_S469child1_ = newTrait244_S469child1();

int S470_classId = Class_genId();
struct S470{

  int id;
  S470 ():id(S470_classId){

  }
};


int S470child1_classId = Class_genId();
struct S470child1:S470{

  S470child1 (int a):a(a){
    (id = S470child1_classId);
{

    }
  }
  int a;
};


int S470child2_classId = Class_genId();
struct S470child2:S470{

  S470child2 (int a):a(a){
    (id = S470child2_classId);
{

    }
  }
  int a;
};


struct Trait470{

  int(*trait470)(Class*);
};

Vec* Trait470_v = newVec();

int Trait470_S470child1_trait470(Class* self_) {
  S470child1* self = ((S470child1*)self_);
{
    return (self -> a);
  }
} 
Trait470* newTrait470_S470child1() {
  Trait470 (* impl) = (new Trait470());
  setVec(Trait470_v, S470child1_classId, ((void*)impl));
  ((impl -> trait470) = (& Trait470_S470child1_trait470));
  return impl;
} 
Trait470* Trait470_S470child1_ = newTrait470_S470child1();

int Trait470_S470child2_trait470(Class* self_) {
  S470child2* self = ((S470child2*)self_);
{
    return (self -> a);
  }
} 
Trait470* newTrait470_S470child2() {
  Trait470 (* impl) = (new Trait470());
  setVec(Trait470_v, S470child2_classId, ((void*)impl));
  ((impl -> trait470) = (& Trait470_S470child2_trait470));
  return impl;
} 
Trait470* Trait470_S470child2_ = newTrait470_S470child2();

int Trait263_S470child1_trait263(Class* self_) {
  S470child1* self = ((S470child1*)self_);
{
    return (self -> a);
  }
} 
Trait263* newTrait263_S470child1() {
  Trait263 (* impl) = (new Trait263());
  setVec(Trait263_v, S470child1_classId, ((void*)impl));
  ((impl -> trait263) = (& Trait263_S470child1_trait263));
  return impl;
} 
Trait263* Trait263_S470child1_ = newTrait263_S470child1();

int S471_classId = Class_genId();
struct S471{

  int id;
  S471 ():id(S471_classId){

  }
};


int S471child1_classId = Class_genId();
struct S471child1:S471{

  S471child1 (int a):a(a){
    (id = S471child1_classId);
{

    }
  }
  int a;
};


int S471child2_classId = Class_genId();
struct S471child2:S471{

  S471child2 (int a):a(a){
    (id = S471child2_classId);
{

    }
  }
  int a;
};


int S471child3_classId = Class_genId();
struct S471child3:S471{

  S471child3 (int a):a(a){
    (id = S471child3_classId);
{

    }
  }
  int a;
};


int S471child4_classId = Class_genId();
struct S471child4:S471{

  S471child4 (int a):a(a){
    (id = S471child4_classId);
{

    }
  }
  int a;
};


struct Trait471{

  int(*trait471)(Class*);
};

Vec* Trait471_v = newVec();

int Trait471_S471child1_trait471(Class* self_) {
  S471child1* self = ((S471child1*)self_);
{
    return (self -> a);
  }
} 
Trait471* newTrait471_S471child1() {
  Trait471 (* impl) = (new Trait471());
  setVec(Trait471_v, S471child1_classId, ((void*)impl));
  ((impl -> trait471) = (& Trait471_S471child1_trait471));
  return impl;
} 
Trait471* Trait471_S471child1_ = newTrait471_S471child1();

int Trait471_S471child2_trait471(Class* self_) {
  S471child2* self = ((S471child2*)self_);
{
    return (self -> a);
  }
} 
Trait471* newTrait471_S471child2() {
  Trait471 (* impl) = (new Trait471());
  setVec(Trait471_v, S471child2_classId, ((void*)impl));
  ((impl -> trait471) = (& Trait471_S471child2_trait471));
  return impl;
} 
Trait471* Trait471_S471child2_ = newTrait471_S471child2();

int Trait78_S471child1_trait78(Class* self_) {
  S471child1* self = ((S471child1*)self_);
{
    return (self -> a);
  }
} 
Trait78* newTrait78_S471child1() {
  Trait78 (* impl) = (new Trait78());
  setVec(Trait78_v, S471child1_classId, ((void*)impl));
  ((impl -> trait78) = (& Trait78_S471child1_trait78));
  return impl;
} 
Trait78* Trait78_S471child1_ = newTrait78_S471child1();

int S472_classId = Class_genId();
struct S472{

  int id;
  S472 ():id(S472_classId){

  }
};


int S472child1_classId = Class_genId();
struct S472child1:S472{

  S472child1 (int a):a(a){
    (id = S472child1_classId);
{

    }
  }
  int a;
};


struct Trait472{

  int(*trait472)(Class*);
};

Vec* Trait472_v = newVec();

int Trait472_S472child1_trait472(Class* self_) {
  S472child1* self = ((S472child1*)self_);
{
    return (self -> a);
  }
} 
Trait472* newTrait472_S472child1() {
  Trait472 (* impl) = (new Trait472());
  setVec(Trait472_v, S472child1_classId, ((void*)impl));
  ((impl -> trait472) = (& Trait472_S472child1_trait472));
  return impl;
} 
Trait472* Trait472_S472child1_ = newTrait472_S472child1();

int Trait143_S472child1_trait143(Class* self_) {
  S472child1* self = ((S472child1*)self_);
{
    return (self -> a);
  }
} 
Trait143* newTrait143_S472child1() {
  Trait143 (* impl) = (new Trait143());
  setVec(Trait143_v, S472child1_classId, ((void*)impl));
  ((impl -> trait143) = (& Trait143_S472child1_trait143));
  return impl;
} 
Trait143* Trait143_S472child1_ = newTrait143_S472child1();

int S473_classId = Class_genId();
struct S473{

  int id;
  S473 ():id(S473_classId){

  }
};


int S473child1_classId = Class_genId();
struct S473child1:S473{

  S473child1 (int a):a(a){
    (id = S473child1_classId);
{

    }
  }
  int a;
};


int S473child2_classId = Class_genId();
struct S473child2:S473{

  S473child2 (int a):a(a){
    (id = S473child2_classId);
{

    }
  }
  int a;
};


struct Trait473{

  int(*trait473)(Class*);
};

Vec* Trait473_v = newVec();

int Trait473_S473child1_trait473(Class* self_) {
  S473child1* self = ((S473child1*)self_);
{
    return (self -> a);
  }
} 
Trait473* newTrait473_S473child1() {
  Trait473 (* impl) = (new Trait473());
  setVec(Trait473_v, S473child1_classId, ((void*)impl));
  ((impl -> trait473) = (& Trait473_S473child1_trait473));
  return impl;
} 
Trait473* Trait473_S473child1_ = newTrait473_S473child1();

int Trait434_S473child1_trait434(Class* self_) {
  S473child1* self = ((S473child1*)self_);
{
    return (self -> a);
  }
} 
Trait434* newTrait434_S473child1() {
  Trait434 (* impl) = (new Trait434());
  setVec(Trait434_v, S473child1_classId, ((void*)impl));
  ((impl -> trait434) = (& Trait434_S473child1_trait434));
  return impl;
} 
Trait434* Trait434_S473child1_ = newTrait434_S473child1();

int S474_classId = Class_genId();
struct S474{

  int id;
  S474 ():id(S474_classId){

  }
};


int S474child1_classId = Class_genId();
struct S474child1:S474{

  S474child1 (int a):a(a){
    (id = S474child1_classId);
{

    }
  }
  int a;
};


struct Trait474{

  int(*trait474)(Class*);
};

Vec* Trait474_v = newVec();

int Trait474_S474child1_trait474(Class* self_) {
  S474child1* self = ((S474child1*)self_);
{
    return (self -> a);
  }
} 
Trait474* newTrait474_S474child1() {
  Trait474 (* impl) = (new Trait474());
  setVec(Trait474_v, S474child1_classId, ((void*)impl));
  ((impl -> trait474) = (& Trait474_S474child1_trait474));
  return impl;
} 
Trait474* Trait474_S474child1_ = newTrait474_S474child1();

int Trait70_S474child1_trait70(Class* self_) {
  S474child1* self = ((S474child1*)self_);
{
    return (self -> a);
  }
} 
Trait70* newTrait70_S474child1() {
  Trait70 (* impl) = (new Trait70());
  setVec(Trait70_v, S474child1_classId, ((void*)impl));
  ((impl -> trait70) = (& Trait70_S474child1_trait70));
  return impl;
} 
Trait70* Trait70_S474child1_ = newTrait70_S474child1();

int S475_classId = Class_genId();
struct S475{

  int id;
  S475 ():id(S475_classId){

  }
};


int S475child1_classId = Class_genId();
struct S475child1:S475{

  S475child1 (int a):a(a){
    (id = S475child1_classId);
{

    }
  }
  int a;
};


int S475child2_classId = Class_genId();
struct S475child2:S475{

  S475child2 (int a):a(a){
    (id = S475child2_classId);
{

    }
  }
  int a;
};


int S475child3_classId = Class_genId();
struct S475child3:S475{

  S475child3 (int a):a(a){
    (id = S475child3_classId);
{

    }
  }
  int a;
};


struct Trait475{

  int(*trait475)(Class*);
};

Vec* Trait475_v = newVec();

int Trait475_S475child1_trait475(Class* self_) {
  S475child1* self = ((S475child1*)self_);
{
    return (self -> a);
  }
} 
Trait475* newTrait475_S475child1() {
  Trait475 (* impl) = (new Trait475());
  setVec(Trait475_v, S475child1_classId, ((void*)impl));
  ((impl -> trait475) = (& Trait475_S475child1_trait475));
  return impl;
} 
Trait475* Trait475_S475child1_ = newTrait475_S475child1();

int Trait475_S475child2_trait475(Class* self_) {
  S475child2* self = ((S475child2*)self_);
{
    return (self -> a);
  }
} 
Trait475* newTrait475_S475child2() {
  Trait475 (* impl) = (new Trait475());
  setVec(Trait475_v, S475child2_classId, ((void*)impl));
  ((impl -> trait475) = (& Trait475_S475child2_trait475));
  return impl;
} 
Trait475* Trait475_S475child2_ = newTrait475_S475child2();

int Trait49_S475child1_trait49(Class* self_) {
  S475child1* self = ((S475child1*)self_);
{
    return (self -> a);
  }
} 
Trait49* newTrait49_S475child1() {
  Trait49 (* impl) = (new Trait49());
  setVec(Trait49_v, S475child1_classId, ((void*)impl));
  ((impl -> trait49) = (& Trait49_S475child1_trait49));
  return impl;
} 
Trait49* Trait49_S475child1_ = newTrait49_S475child1();

int S476_classId = Class_genId();
struct S476{

  int id;
  S476 ():id(S476_classId){

  }
};


int S476child1_classId = Class_genId();
struct S476child1:S476{

  S476child1 (int a):a(a){
    (id = S476child1_classId);
{

    }
  }
  int a;
};


struct Trait476{

  int(*trait476)(Class*);
};

Vec* Trait476_v = newVec();

int Trait476_S476child1_trait476(Class* self_) {
  S476child1* self = ((S476child1*)self_);
{
    return (self -> a);
  }
} 
Trait476* newTrait476_S476child1() {
  Trait476 (* impl) = (new Trait476());
  setVec(Trait476_v, S476child1_classId, ((void*)impl));
  ((impl -> trait476) = (& Trait476_S476child1_trait476));
  return impl;
} 
Trait476* Trait476_S476child1_ = newTrait476_S476child1();

int Trait316_S476child1_trait316(Class* self_) {
  S476child1* self = ((S476child1*)self_);
{
    return (self -> a);
  }
} 
Trait316* newTrait316_S476child1() {
  Trait316 (* impl) = (new Trait316());
  setVec(Trait316_v, S476child1_classId, ((void*)impl));
  ((impl -> trait316) = (& Trait316_S476child1_trait316));
  return impl;
} 
Trait316* Trait316_S476child1_ = newTrait316_S476child1();

int S477_classId = Class_genId();
struct S477{

  int id;
  S477 ():id(S477_classId){

  }
};


int S477child1_classId = Class_genId();
struct S477child1:S477{

  S477child1 (int a):a(a){
    (id = S477child1_classId);
{

    }
  }
  int a;
};


struct Trait477{

  int(*trait477)(Class*);
};

Vec* Trait477_v = newVec();

int Trait477_S477child1_trait477(Class* self_) {
  S477child1* self = ((S477child1*)self_);
{
    return (self -> a);
  }
} 
Trait477* newTrait477_S477child1() {
  Trait477 (* impl) = (new Trait477());
  setVec(Trait477_v, S477child1_classId, ((void*)impl));
  ((impl -> trait477) = (& Trait477_S477child1_trait477));
  return impl;
} 
Trait477* Trait477_S477child1_ = newTrait477_S477child1();

int Trait237_S477child1_trait237(Class* self_) {
  S477child1* self = ((S477child1*)self_);
{
    return (self -> a);
  }
} 
Trait237* newTrait237_S477child1() {
  Trait237 (* impl) = (new Trait237());
  setVec(Trait237_v, S477child1_classId, ((void*)impl));
  ((impl -> trait237) = (& Trait237_S477child1_trait237));
  return impl;
} 
Trait237* Trait237_S477child1_ = newTrait237_S477child1();

int S478_classId = Class_genId();
struct S478{

  int id;
  S478 ():id(S478_classId){

  }
};


int S478child1_classId = Class_genId();
struct S478child1:S478{

  S478child1 (int a):a(a){
    (id = S478child1_classId);
{

    }
  }
  int a;
};


int S478child2_classId = Class_genId();
struct S478child2:S478{

  S478child2 (int a):a(a){
    (id = S478child2_classId);
{

    }
  }
  int a;
};


struct Trait478{

  int(*trait478)(Class*);
};

Vec* Trait478_v = newVec();

int Trait478_S478child1_trait478(Class* self_) {
  S478child1* self = ((S478child1*)self_);
{
    return (self -> a);
  }
} 
Trait478* newTrait478_S478child1() {
  Trait478 (* impl) = (new Trait478());
  setVec(Trait478_v, S478child1_classId, ((void*)impl));
  ((impl -> trait478) = (& Trait478_S478child1_trait478));
  return impl;
} 
Trait478* Trait478_S478child1_ = newTrait478_S478child1();

int Trait478_S478child2_trait478(Class* self_) {
  S478child2* self = ((S478child2*)self_);
{
    return (self -> a);
  }
} 
Trait478* newTrait478_S478child2() {
  Trait478 (* impl) = (new Trait478());
  setVec(Trait478_v, S478child2_classId, ((void*)impl));
  ((impl -> trait478) = (& Trait478_S478child2_trait478));
  return impl;
} 
Trait478* Trait478_S478child2_ = newTrait478_S478child2();

int Trait303_S478child1_trait303(Class* self_) {
  S478child1* self = ((S478child1*)self_);
{
    return (self -> a);
  }
} 
Trait303* newTrait303_S478child1() {
  Trait303 (* impl) = (new Trait303());
  setVec(Trait303_v, S478child1_classId, ((void*)impl));
  ((impl -> trait303) = (& Trait303_S478child1_trait303));
  return impl;
} 
Trait303* Trait303_S478child1_ = newTrait303_S478child1();

int S479_classId = Class_genId();
struct S479{

  int id;
  S479 ():id(S479_classId){

  }
};


int S479child1_classId = Class_genId();
struct S479child1:S479{

  S479child1 (int a):a(a){
    (id = S479child1_classId);
{

    }
  }
  int a;
};


int S479child2_classId = Class_genId();
struct S479child2:S479{

  S479child2 (int a):a(a){
    (id = S479child2_classId);
{

    }
  }
  int a;
};


int S479child3_classId = Class_genId();
struct S479child3:S479{

  S479child3 (int a):a(a){
    (id = S479child3_classId);
{

    }
  }
  int a;
};


int S479child4_classId = Class_genId();
struct S479child4:S479{

  S479child4 (int a):a(a){
    (id = S479child4_classId);
{

    }
  }
  int a;
};


struct Trait479{

  int(*trait479)(Class*);
};

Vec* Trait479_v = newVec();

int Trait479_S479child1_trait479(Class* self_) {
  S479child1* self = ((S479child1*)self_);
{
    return (self -> a);
  }
} 
Trait479* newTrait479_S479child1() {
  Trait479 (* impl) = (new Trait479());
  setVec(Trait479_v, S479child1_classId, ((void*)impl));
  ((impl -> trait479) = (& Trait479_S479child1_trait479));
  return impl;
} 
Trait479* Trait479_S479child1_ = newTrait479_S479child1();

int Trait479_S479child2_trait479(Class* self_) {
  S479child2* self = ((S479child2*)self_);
{
    return (self -> a);
  }
} 
Trait479* newTrait479_S479child2() {
  Trait479 (* impl) = (new Trait479());
  setVec(Trait479_v, S479child2_classId, ((void*)impl));
  ((impl -> trait479) = (& Trait479_S479child2_trait479));
  return impl;
} 
Trait479* Trait479_S479child2_ = newTrait479_S479child2();

int Trait479_S479child3_trait479(Class* self_) {
  S479child3* self = ((S479child3*)self_);
{
    return (self -> a);
  }
} 
Trait479* newTrait479_S479child3() {
  Trait479 (* impl) = (new Trait479());
  setVec(Trait479_v, S479child3_classId, ((void*)impl));
  ((impl -> trait479) = (& Trait479_S479child3_trait479));
  return impl;
} 
Trait479* Trait479_S479child3_ = newTrait479_S479child3();

int Trait479_S479child4_trait479(Class* self_) {
  S479child4* self = ((S479child4*)self_);
{
    return (self -> a);
  }
} 
Trait479* newTrait479_S479child4() {
  Trait479 (* impl) = (new Trait479());
  setVec(Trait479_v, S479child4_classId, ((void*)impl));
  ((impl -> trait479) = (& Trait479_S479child4_trait479));
  return impl;
} 
Trait479* Trait479_S479child4_ = newTrait479_S479child4();

int Trait150_S479child1_trait150(Class* self_) {
  S479child1* self = ((S479child1*)self_);
{
    return (self -> a);
  }
} 
Trait150* newTrait150_S479child1() {
  Trait150 (* impl) = (new Trait150());
  setVec(Trait150_v, S479child1_classId, ((void*)impl));
  ((impl -> trait150) = (& Trait150_S479child1_trait150));
  return impl;
} 
Trait150* Trait150_S479child1_ = newTrait150_S479child1();

int S480_classId = Class_genId();
struct S480{

  int id;
  S480 ():id(S480_classId){

  }
};


int S480child1_classId = Class_genId();
struct S480child1:S480{

  S480child1 (int a):a(a){
    (id = S480child1_classId);
{

    }
  }
  int a;
};


int S480child2_classId = Class_genId();
struct S480child2:S480{

  S480child2 (int a):a(a){
    (id = S480child2_classId);
{

    }
  }
  int a;
};


int S480child3_classId = Class_genId();
struct S480child3:S480{

  S480child3 (int a):a(a){
    (id = S480child3_classId);
{

    }
  }
  int a;
};


struct Trait480{

  int(*trait480)(Class*);
};

Vec* Trait480_v = newVec();

int Trait480_S480child1_trait480(Class* self_) {
  S480child1* self = ((S480child1*)self_);
{
    return (self -> a);
  }
} 
Trait480* newTrait480_S480child1() {
  Trait480 (* impl) = (new Trait480());
  setVec(Trait480_v, S480child1_classId, ((void*)impl));
  ((impl -> trait480) = (& Trait480_S480child1_trait480));
  return impl;
} 
Trait480* Trait480_S480child1_ = newTrait480_S480child1();

int Trait243_S480child1_trait243(Class* self_) {
  S480child1* self = ((S480child1*)self_);
{
    return (self -> a);
  }
} 
Trait243* newTrait243_S480child1() {
  Trait243 (* impl) = (new Trait243());
  setVec(Trait243_v, S480child1_classId, ((void*)impl));
  ((impl -> trait243) = (& Trait243_S480child1_trait243));
  return impl;
} 
Trait243* Trait243_S480child1_ = newTrait243_S480child1();

int S481_classId = Class_genId();
struct S481{

  int id;
  S481 ():id(S481_classId){

  }
};


int S481child1_classId = Class_genId();
struct S481child1:S481{

  S481child1 (int a):a(a){
    (id = S481child1_classId);
{

    }
  }
  int a;
};


struct Trait481{

  int(*trait481)(Class*);
};

Vec* Trait481_v = newVec();

int Trait481_S481child1_trait481(Class* self_) {
  S481child1* self = ((S481child1*)self_);
{
    return (self -> a);
  }
} 
Trait481* newTrait481_S481child1() {
  Trait481 (* impl) = (new Trait481());
  setVec(Trait481_v, S481child1_classId, ((void*)impl));
  ((impl -> trait481) = (& Trait481_S481child1_trait481));
  return impl;
} 
Trait481* Trait481_S481child1_ = newTrait481_S481child1();

int Trait132_S481child1_trait132(Class* self_) {
  S481child1* self = ((S481child1*)self_);
{
    return (self -> a);
  }
} 
Trait132* newTrait132_S481child1() {
  Trait132 (* impl) = (new Trait132());
  setVec(Trait132_v, S481child1_classId, ((void*)impl));
  ((impl -> trait132) = (& Trait132_S481child1_trait132));
  return impl;
} 
Trait132* Trait132_S481child1_ = newTrait132_S481child1();

int S482_classId = Class_genId();
struct S482{

  int id;
  S482 ():id(S482_classId){

  }
};


int S482child1_classId = Class_genId();
struct S482child1:S482{

  S482child1 (int a):a(a){
    (id = S482child1_classId);
{

    }
  }
  int a;
};


struct Trait482{

  int(*trait482)(Class*);
};

Vec* Trait482_v = newVec();

int Trait482_S482child1_trait482(Class* self_) {
  S482child1* self = ((S482child1*)self_);
{
    return (self -> a);
  }
} 
Trait482* newTrait482_S482child1() {
  Trait482 (* impl) = (new Trait482());
  setVec(Trait482_v, S482child1_classId, ((void*)impl));
  ((impl -> trait482) = (& Trait482_S482child1_trait482));
  return impl;
} 
Trait482* Trait482_S482child1_ = newTrait482_S482child1();

int Trait243_S482child1_trait243(Class* self_) {
  S482child1* self = ((S482child1*)self_);
{
    return (self -> a);
  }
} 
Trait243* newTrait243_S482child1() {
  Trait243 (* impl) = (new Trait243());
  setVec(Trait243_v, S482child1_classId, ((void*)impl));
  ((impl -> trait243) = (& Trait243_S482child1_trait243));
  return impl;
} 
Trait243* Trait243_S482child1_ = newTrait243_S482child1();

int S483_classId = Class_genId();
struct S483{

  int id;
  S483 ():id(S483_classId){

  }
};


int S483child1_classId = Class_genId();
struct S483child1:S483{

  S483child1 (int a):a(a){
    (id = S483child1_classId);
{

    }
  }
  int a;
};


int S483child2_classId = Class_genId();
struct S483child2:S483{

  S483child2 (int a):a(a){
    (id = S483child2_classId);
{

    }
  }
  int a;
};


struct Trait483{

  int(*trait483)(Class*);
};

Vec* Trait483_v = newVec();

int Trait483_S483child1_trait483(Class* self_) {
  S483child1* self = ((S483child1*)self_);
{
    return (self -> a);
  }
} 
Trait483* newTrait483_S483child1() {
  Trait483 (* impl) = (new Trait483());
  setVec(Trait483_v, S483child1_classId, ((void*)impl));
  ((impl -> trait483) = (& Trait483_S483child1_trait483));
  return impl;
} 
Trait483* Trait483_S483child1_ = newTrait483_S483child1();

int Trait421_S483child1_trait421(Class* self_) {
  S483child1* self = ((S483child1*)self_);
{
    return (self -> a);
  }
} 
Trait421* newTrait421_S483child1() {
  Trait421 (* impl) = (new Trait421());
  setVec(Trait421_v, S483child1_classId, ((void*)impl));
  ((impl -> trait421) = (& Trait421_S483child1_trait421));
  return impl;
} 
Trait421* Trait421_S483child1_ = newTrait421_S483child1();

int S484_classId = Class_genId();
struct S484{

  int id;
  S484 ():id(S484_classId){

  }
};


int S484child1_classId = Class_genId();
struct S484child1:S484{

  S484child1 (int a):a(a){
    (id = S484child1_classId);
{

    }
  }
  int a;
};


int S484child2_classId = Class_genId();
struct S484child2:S484{

  S484child2 (int a):a(a){
    (id = S484child2_classId);
{

    }
  }
  int a;
};


int S484child3_classId = Class_genId();
struct S484child3:S484{

  S484child3 (int a):a(a){
    (id = S484child3_classId);
{

    }
  }
  int a;
};


int S484child4_classId = Class_genId();
struct S484child4:S484{

  S484child4 (int a):a(a){
    (id = S484child4_classId);
{

    }
  }
  int a;
};


struct Trait484{

  int(*trait484)(Class*);
};

Vec* Trait484_v = newVec();

int Trait484_S484child1_trait484(Class* self_) {
  S484child1* self = ((S484child1*)self_);
{
    return (self -> a);
  }
} 
Trait484* newTrait484_S484child1() {
  Trait484 (* impl) = (new Trait484());
  setVec(Trait484_v, S484child1_classId, ((void*)impl));
  ((impl -> trait484) = (& Trait484_S484child1_trait484));
  return impl;
} 
Trait484* Trait484_S484child1_ = newTrait484_S484child1();

int Trait484_S484child2_trait484(Class* self_) {
  S484child2* self = ((S484child2*)self_);
{
    return (self -> a);
  }
} 
Trait484* newTrait484_S484child2() {
  Trait484 (* impl) = (new Trait484());
  setVec(Trait484_v, S484child2_classId, ((void*)impl));
  ((impl -> trait484) = (& Trait484_S484child2_trait484));
  return impl;
} 
Trait484* Trait484_S484child2_ = newTrait484_S484child2();

int Trait33_S484child1_trait33(Class* self_) {
  S484child1* self = ((S484child1*)self_);
{
    return (self -> a);
  }
} 
Trait33* newTrait33_S484child1() {
  Trait33 (* impl) = (new Trait33());
  setVec(Trait33_v, S484child1_classId, ((void*)impl));
  ((impl -> trait33) = (& Trait33_S484child1_trait33));
  return impl;
} 
Trait33* Trait33_S484child1_ = newTrait33_S484child1();

int S485_classId = Class_genId();
struct S485{

  int id;
  S485 ():id(S485_classId){

  }
};


int S485child1_classId = Class_genId();
struct S485child1:S485{

  S485child1 (int a):a(a){
    (id = S485child1_classId);
{

    }
  }
  int a;
};


int S485child2_classId = Class_genId();
struct S485child2:S485{

  S485child2 (int a):a(a){
    (id = S485child2_classId);
{

    }
  }
  int a;
};


int S485child3_classId = Class_genId();
struct S485child3:S485{

  S485child3 (int a):a(a){
    (id = S485child3_classId);
{

    }
  }
  int a;
};


int S485child4_classId = Class_genId();
struct S485child4:S485{

  S485child4 (int a):a(a){
    (id = S485child4_classId);
{

    }
  }
  int a;
};


int S485child5_classId = Class_genId();
struct S485child5:S485{

  S485child5 (int a):a(a){
    (id = S485child5_classId);
{

    }
  }
  int a;
};


struct Trait485{

  int(*trait485)(Class*);
};

Vec* Trait485_v = newVec();

int Trait485_S485child1_trait485(Class* self_) {
  S485child1* self = ((S485child1*)self_);
{
    return (self -> a);
  }
} 
Trait485* newTrait485_S485child1() {
  Trait485 (* impl) = (new Trait485());
  setVec(Trait485_v, S485child1_classId, ((void*)impl));
  ((impl -> trait485) = (& Trait485_S485child1_trait485));
  return impl;
} 
Trait485* Trait485_S485child1_ = newTrait485_S485child1();

int Trait362_S485child1_trait362(Class* self_) {
  S485child1* self = ((S485child1*)self_);
{
    return (self -> a);
  }
} 
Trait362* newTrait362_S485child1() {
  Trait362 (* impl) = (new Trait362());
  setVec(Trait362_v, S485child1_classId, ((void*)impl));
  ((impl -> trait362) = (& Trait362_S485child1_trait362));
  return impl;
} 
Trait362* Trait362_S485child1_ = newTrait362_S485child1();

int S486_classId = Class_genId();
struct S486{

  int id;
  S486 ():id(S486_classId){

  }
};


int S486child1_classId = Class_genId();
struct S486child1:S486{

  S486child1 (int a):a(a){
    (id = S486child1_classId);
{

    }
  }
  int a;
};


int S486child2_classId = Class_genId();
struct S486child2:S486{

  S486child2 (int a):a(a){
    (id = S486child2_classId);
{

    }
  }
  int a;
};


int S486child3_classId = Class_genId();
struct S486child3:S486{

  S486child3 (int a):a(a){
    (id = S486child3_classId);
{

    }
  }
  int a;
};


int S486child4_classId = Class_genId();
struct S486child4:S486{

  S486child4 (int a):a(a){
    (id = S486child4_classId);
{

    }
  }
  int a;
};


struct Trait486{

  int(*trait486)(Class*);
};

Vec* Trait486_v = newVec();

int Trait486_S486child1_trait486(Class* self_) {
  S486child1* self = ((S486child1*)self_);
{
    return (self -> a);
  }
} 
Trait486* newTrait486_S486child1() {
  Trait486 (* impl) = (new Trait486());
  setVec(Trait486_v, S486child1_classId, ((void*)impl));
  ((impl -> trait486) = (& Trait486_S486child1_trait486));
  return impl;
} 
Trait486* Trait486_S486child1_ = newTrait486_S486child1();

int Trait199_S486child1_trait199(Class* self_) {
  S486child1* self = ((S486child1*)self_);
{
    return (self -> a);
  }
} 
Trait199* newTrait199_S486child1() {
  Trait199 (* impl) = (new Trait199());
  setVec(Trait199_v, S486child1_classId, ((void*)impl));
  ((impl -> trait199) = (& Trait199_S486child1_trait199));
  return impl;
} 
Trait199* Trait199_S486child1_ = newTrait199_S486child1();

int S487_classId = Class_genId();
struct S487{

  int id;
  S487 ():id(S487_classId){

  }
};


int S487child1_classId = Class_genId();
struct S487child1:S487{

  S487child1 (int a):a(a){
    (id = S487child1_classId);
{

    }
  }
  int a;
};


int S487child2_classId = Class_genId();
struct S487child2:S487{

  S487child2 (int a):a(a){
    (id = S487child2_classId);
{

    }
  }
  int a;
};


struct Trait487{

  int(*trait487)(Class*);
};

Vec* Trait487_v = newVec();

int Trait487_S487child1_trait487(Class* self_) {
  S487child1* self = ((S487child1*)self_);
{
    return (self -> a);
  }
} 
Trait487* newTrait487_S487child1() {
  Trait487 (* impl) = (new Trait487());
  setVec(Trait487_v, S487child1_classId, ((void*)impl));
  ((impl -> trait487) = (& Trait487_S487child1_trait487));
  return impl;
} 
Trait487* Trait487_S487child1_ = newTrait487_S487child1();

int Trait361_S487child1_trait361(Class* self_) {
  S487child1* self = ((S487child1*)self_);
{
    return (self -> a);
  }
} 
Trait361* newTrait361_S487child1() {
  Trait361 (* impl) = (new Trait361());
  setVec(Trait361_v, S487child1_classId, ((void*)impl));
  ((impl -> trait361) = (& Trait361_S487child1_trait361));
  return impl;
} 
Trait361* Trait361_S487child1_ = newTrait361_S487child1();

int S488_classId = Class_genId();
struct S488{

  int id;
  S488 ():id(S488_classId){

  }
};


int S488child1_classId = Class_genId();
struct S488child1:S488{

  S488child1 (int a):a(a){
    (id = S488child1_classId);
{

    }
  }
  int a;
};


struct Trait488{

  int(*trait488)(Class*);
};

Vec* Trait488_v = newVec();

int Trait488_S488child1_trait488(Class* self_) {
  S488child1* self = ((S488child1*)self_);
{
    return (self -> a);
  }
} 
Trait488* newTrait488_S488child1() {
  Trait488 (* impl) = (new Trait488());
  setVec(Trait488_v, S488child1_classId, ((void*)impl));
  ((impl -> trait488) = (& Trait488_S488child1_trait488));
  return impl;
} 
Trait488* Trait488_S488child1_ = newTrait488_S488child1();

int Trait314_S488child1_trait314(Class* self_) {
  S488child1* self = ((S488child1*)self_);
{
    return (self -> a);
  }
} 
Trait314* newTrait314_S488child1() {
  Trait314 (* impl) = (new Trait314());
  setVec(Trait314_v, S488child1_classId, ((void*)impl));
  ((impl -> trait314) = (& Trait314_S488child1_trait314));
  return impl;
} 
Trait314* Trait314_S488child1_ = newTrait314_S488child1();

int S489_classId = Class_genId();
struct S489{

  int id;
  S489 ():id(S489_classId){

  }
};


int S489child1_classId = Class_genId();
struct S489child1:S489{

  S489child1 (int a):a(a){
    (id = S489child1_classId);
{

    }
  }
  int a;
};


int S489child2_classId = Class_genId();
struct S489child2:S489{

  S489child2 (int a):a(a){
    (id = S489child2_classId);
{

    }
  }
  int a;
};


int S489child3_classId = Class_genId();
struct S489child3:S489{

  S489child3 (int a):a(a){
    (id = S489child3_classId);
{

    }
  }
  int a;
};


struct Trait489{

  int(*trait489)(Class*);
};

Vec* Trait489_v = newVec();

int Trait489_S489child1_trait489(Class* self_) {
  S489child1* self = ((S489child1*)self_);
{
    return (self -> a);
  }
} 
Trait489* newTrait489_S489child1() {
  Trait489 (* impl) = (new Trait489());
  setVec(Trait489_v, S489child1_classId, ((void*)impl));
  ((impl -> trait489) = (& Trait489_S489child1_trait489));
  return impl;
} 
Trait489* Trait489_S489child1_ = newTrait489_S489child1();

int Trait50_S489child1_trait50(Class* self_) {
  S489child1* self = ((S489child1*)self_);
{
    return (self -> a);
  }
} 
Trait50* newTrait50_S489child1() {
  Trait50 (* impl) = (new Trait50());
  setVec(Trait50_v, S489child1_classId, ((void*)impl));
  ((impl -> trait50) = (& Trait50_S489child1_trait50));
  return impl;
} 
Trait50* Trait50_S489child1_ = newTrait50_S489child1();

int S490_classId = Class_genId();
struct S490{

  int id;
  S490 ():id(S490_classId){

  }
};


int S490child1_classId = Class_genId();
struct S490child1:S490{

  S490child1 (int a):a(a){
    (id = S490child1_classId);
{

    }
  }
  int a;
};


int S490child2_classId = Class_genId();
struct S490child2:S490{

  S490child2 (int a):a(a){
    (id = S490child2_classId);
{

    }
  }
  int a;
};


int S490child3_classId = Class_genId();
struct S490child3:S490{

  S490child3 (int a):a(a){
    (id = S490child3_classId);
{

    }
  }
  int a;
};


int S490child4_classId = Class_genId();
struct S490child4:S490{

  S490child4 (int a):a(a){
    (id = S490child4_classId);
{

    }
  }
  int a;
};


struct Trait490{

  int(*trait490)(Class*);
};

Vec* Trait490_v = newVec();

int Trait490_S490child1_trait490(Class* self_) {
  S490child1* self = ((S490child1*)self_);
{
    return (self -> a);
  }
} 
Trait490* newTrait490_S490child1() {
  Trait490 (* impl) = (new Trait490());
  setVec(Trait490_v, S490child1_classId, ((void*)impl));
  ((impl -> trait490) = (& Trait490_S490child1_trait490));
  return impl;
} 
Trait490* Trait490_S490child1_ = newTrait490_S490child1();

int Trait490_S490child2_trait490(Class* self_) {
  S490child2* self = ((S490child2*)self_);
{
    return (self -> a);
  }
} 
Trait490* newTrait490_S490child2() {
  Trait490 (* impl) = (new Trait490());
  setVec(Trait490_v, S490child2_classId, ((void*)impl));
  ((impl -> trait490) = (& Trait490_S490child2_trait490));
  return impl;
} 
Trait490* Trait490_S490child2_ = newTrait490_S490child2();

int Trait444_S490child1_trait444(Class* self_) {
  S490child1* self = ((S490child1*)self_);
{
    return (self -> a);
  }
} 
Trait444* newTrait444_S490child1() {
  Trait444 (* impl) = (new Trait444());
  setVec(Trait444_v, S490child1_classId, ((void*)impl));
  ((impl -> trait444) = (& Trait444_S490child1_trait444));
  return impl;
} 
Trait444* Trait444_S490child1_ = newTrait444_S490child1();

int S491_classId = Class_genId();
struct S491{

  int id;
  S491 ():id(S491_classId){

  }
};


int S491child1_classId = Class_genId();
struct S491child1:S491{

  S491child1 (int a):a(a){
    (id = S491child1_classId);
{

    }
  }
  int a;
};


int S491child2_classId = Class_genId();
struct S491child2:S491{

  S491child2 (int a):a(a){
    (id = S491child2_classId);
{

    }
  }
  int a;
};


struct Trait491{

  int(*trait491)(Class*);
};

Vec* Trait491_v = newVec();

int Trait491_S491child1_trait491(Class* self_) {
  S491child1* self = ((S491child1*)self_);
{
    return (self -> a);
  }
} 
Trait491* newTrait491_S491child1() {
  Trait491 (* impl) = (new Trait491());
  setVec(Trait491_v, S491child1_classId, ((void*)impl));
  ((impl -> trait491) = (& Trait491_S491child1_trait491));
  return impl;
} 
Trait491* Trait491_S491child1_ = newTrait491_S491child1();

int Trait263_S491child1_trait263(Class* self_) {
  S491child1* self = ((S491child1*)self_);
{
    return (self -> a);
  }
} 
Trait263* newTrait263_S491child1() {
  Trait263 (* impl) = (new Trait263());
  setVec(Trait263_v, S491child1_classId, ((void*)impl));
  ((impl -> trait263) = (& Trait263_S491child1_trait263));
  return impl;
} 
Trait263* Trait263_S491child1_ = newTrait263_S491child1();

int S492_classId = Class_genId();
struct S492{

  int id;
  S492 ():id(S492_classId){

  }
};


int S492child1_classId = Class_genId();
struct S492child1:S492{

  S492child1 (int a):a(a){
    (id = S492child1_classId);
{

    }
  }
  int a;
};


int S492child2_classId = Class_genId();
struct S492child2:S492{

  S492child2 (int a):a(a){
    (id = S492child2_classId);
{

    }
  }
  int a;
};


int S492child3_classId = Class_genId();
struct S492child3:S492{

  S492child3 (int a):a(a){
    (id = S492child3_classId);
{

    }
  }
  int a;
};


int S492child4_classId = Class_genId();
struct S492child4:S492{

  S492child4 (int a):a(a){
    (id = S492child4_classId);
{

    }
  }
  int a;
};


struct Trait492{

  int(*trait492)(Class*);
};

Vec* Trait492_v = newVec();

int Trait492_S492child1_trait492(Class* self_) {
  S492child1* self = ((S492child1*)self_);
{
    return (self -> a);
  }
} 
Trait492* newTrait492_S492child1() {
  Trait492 (* impl) = (new Trait492());
  setVec(Trait492_v, S492child1_classId, ((void*)impl));
  ((impl -> trait492) = (& Trait492_S492child1_trait492));
  return impl;
} 
Trait492* Trait492_S492child1_ = newTrait492_S492child1();

int Trait492_S492child2_trait492(Class* self_) {
  S492child2* self = ((S492child2*)self_);
{
    return (self -> a);
  }
} 
Trait492* newTrait492_S492child2() {
  Trait492 (* impl) = (new Trait492());
  setVec(Trait492_v, S492child2_classId, ((void*)impl));
  ((impl -> trait492) = (& Trait492_S492child2_trait492));
  return impl;
} 
Trait492* Trait492_S492child2_ = newTrait492_S492child2();

int Trait216_S492child1_trait216(Class* self_) {
  S492child1* self = ((S492child1*)self_);
{
    return (self -> a);
  }
} 
Trait216* newTrait216_S492child1() {
  Trait216 (* impl) = (new Trait216());
  setVec(Trait216_v, S492child1_classId, ((void*)impl));
  ((impl -> trait216) = (& Trait216_S492child1_trait216));
  return impl;
} 
Trait216* Trait216_S492child1_ = newTrait216_S492child1();

int S493_classId = Class_genId();
struct S493{

  int id;
  S493 ():id(S493_classId){

  }
};


int S493child1_classId = Class_genId();
struct S493child1:S493{

  S493child1 (int a):a(a){
    (id = S493child1_classId);
{

    }
  }
  int a;
};


struct Trait493{

  int(*trait493)(Class*);
};

Vec* Trait493_v = newVec();

int Trait493_S493child1_trait493(Class* self_) {
  S493child1* self = ((S493child1*)self_);
{
    return (self -> a);
  }
} 
Trait493* newTrait493_S493child1() {
  Trait493 (* impl) = (new Trait493());
  setVec(Trait493_v, S493child1_classId, ((void*)impl));
  ((impl -> trait493) = (& Trait493_S493child1_trait493));
  return impl;
} 
Trait493* Trait493_S493child1_ = newTrait493_S493child1();

int Trait412_S493child1_trait412(Class* self_) {
  S493child1* self = ((S493child1*)self_);
{
    return (self -> a);
  }
} 
Trait412* newTrait412_S493child1() {
  Trait412 (* impl) = (new Trait412());
  setVec(Trait412_v, S493child1_classId, ((void*)impl));
  ((impl -> trait412) = (& Trait412_S493child1_trait412));
  return impl;
} 
Trait412* Trait412_S493child1_ = newTrait412_S493child1();

int S494_classId = Class_genId();
struct S494{

  int id;
  S494 ():id(S494_classId){

  }
};


int S494child1_classId = Class_genId();
struct S494child1:S494{

  S494child1 (int a):a(a){
    (id = S494child1_classId);
{

    }
  }
  int a;
};


int S494child2_classId = Class_genId();
struct S494child2:S494{

  S494child2 (int a):a(a){
    (id = S494child2_classId);
{

    }
  }
  int a;
};


int S494child3_classId = Class_genId();
struct S494child3:S494{

  S494child3 (int a):a(a){
    (id = S494child3_classId);
{

    }
  }
  int a;
};


int S494child4_classId = Class_genId();
struct S494child4:S494{

  S494child4 (int a):a(a){
    (id = S494child4_classId);
{

    }
  }
  int a;
};


struct Trait494{

  int(*trait494)(Class*);
};

Vec* Trait494_v = newVec();

int Trait494_S494child1_trait494(Class* self_) {
  S494child1* self = ((S494child1*)self_);
{
    return (self -> a);
  }
} 
Trait494* newTrait494_S494child1() {
  Trait494 (* impl) = (new Trait494());
  setVec(Trait494_v, S494child1_classId, ((void*)impl));
  ((impl -> trait494) = (& Trait494_S494child1_trait494));
  return impl;
} 
Trait494* Trait494_S494child1_ = newTrait494_S494child1();

int Trait494_S494child2_trait494(Class* self_) {
  S494child2* self = ((S494child2*)self_);
{
    return (self -> a);
  }
} 
Trait494* newTrait494_S494child2() {
  Trait494 (* impl) = (new Trait494());
  setVec(Trait494_v, S494child2_classId, ((void*)impl));
  ((impl -> trait494) = (& Trait494_S494child2_trait494));
  return impl;
} 
Trait494* Trait494_S494child2_ = newTrait494_S494child2();

int Trait377_S494child1_trait377(Class* self_) {
  S494child1* self = ((S494child1*)self_);
{
    return (self -> a);
  }
} 
Trait377* newTrait377_S494child1() {
  Trait377 (* impl) = (new Trait377());
  setVec(Trait377_v, S494child1_classId, ((void*)impl));
  ((impl -> trait377) = (& Trait377_S494child1_trait377));
  return impl;
} 
Trait377* Trait377_S494child1_ = newTrait377_S494child1();

int S495_classId = Class_genId();
struct S495{

  int id;
  S495 ():id(S495_classId){

  }
};


int S495child1_classId = Class_genId();
struct S495child1:S495{

  S495child1 (int a):a(a){
    (id = S495child1_classId);
{

    }
  }
  int a;
};


struct Trait495{

  int(*trait495)(Class*);
};

Vec* Trait495_v = newVec();

int Trait495_S495child1_trait495(Class* self_) {
  S495child1* self = ((S495child1*)self_);
{
    return (self -> a);
  }
} 
Trait495* newTrait495_S495child1() {
  Trait495 (* impl) = (new Trait495());
  setVec(Trait495_v, S495child1_classId, ((void*)impl));
  ((impl -> trait495) = (& Trait495_S495child1_trait495));
  return impl;
} 
Trait495* Trait495_S495child1_ = newTrait495_S495child1();

int Trait172_S495child1_trait172(Class* self_) {
  S495child1* self = ((S495child1*)self_);
{
    return (self -> a);
  }
} 
Trait172* newTrait172_S495child1() {
  Trait172 (* impl) = (new Trait172());
  setVec(Trait172_v, S495child1_classId, ((void*)impl));
  ((impl -> trait172) = (& Trait172_S495child1_trait172));
  return impl;
} 
Trait172* Trait172_S495child1_ = newTrait172_S495child1();

int S496_classId = Class_genId();
struct S496{

  int id;
  S496 ():id(S496_classId){

  }
};


int S496child1_classId = Class_genId();
struct S496child1:S496{

  S496child1 (int a):a(a){
    (id = S496child1_classId);
{

    }
  }
  int a;
};


int S496child2_classId = Class_genId();
struct S496child2:S496{

  S496child2 (int a):a(a){
    (id = S496child2_classId);
{

    }
  }
  int a;
};


int S496child3_classId = Class_genId();
struct S496child3:S496{

  S496child3 (int a):a(a){
    (id = S496child3_classId);
{

    }
  }
  int a;
};


struct Trait496{

  int(*trait496)(Class*);
};

Vec* Trait496_v = newVec();

int Trait496_S496child1_trait496(Class* self_) {
  S496child1* self = ((S496child1*)self_);
{
    return (self -> a);
  }
} 
Trait496* newTrait496_S496child1() {
  Trait496 (* impl) = (new Trait496());
  setVec(Trait496_v, S496child1_classId, ((void*)impl));
  ((impl -> trait496) = (& Trait496_S496child1_trait496));
  return impl;
} 
Trait496* Trait496_S496child1_ = newTrait496_S496child1();

int Trait496_S496child2_trait496(Class* self_) {
  S496child2* self = ((S496child2*)self_);
{
    return (self -> a);
  }
} 
Trait496* newTrait496_S496child2() {
  Trait496 (* impl) = (new Trait496());
  setVec(Trait496_v, S496child2_classId, ((void*)impl));
  ((impl -> trait496) = (& Trait496_S496child2_trait496));
  return impl;
} 
Trait496* Trait496_S496child2_ = newTrait496_S496child2();

int Trait496_S496child3_trait496(Class* self_) {
  S496child3* self = ((S496child3*)self_);
{
    return (self -> a);
  }
} 
Trait496* newTrait496_S496child3() {
  Trait496 (* impl) = (new Trait496());
  setVec(Trait496_v, S496child3_classId, ((void*)impl));
  ((impl -> trait496) = (& Trait496_S496child3_trait496));
  return impl;
} 
Trait496* Trait496_S496child3_ = newTrait496_S496child3();

int Trait233_S496child1_trait233(Class* self_) {
  S496child1* self = ((S496child1*)self_);
{
    return (self -> a);
  }
} 
Trait233* newTrait233_S496child1() {
  Trait233 (* impl) = (new Trait233());
  setVec(Trait233_v, S496child1_classId, ((void*)impl));
  ((impl -> trait233) = (& Trait233_S496child1_trait233));
  return impl;
} 
Trait233* Trait233_S496child1_ = newTrait233_S496child1();

int S497_classId = Class_genId();
struct S497{

  int id;
  S497 ():id(S497_classId){

  }
};


int S497child1_classId = Class_genId();
struct S497child1:S497{

  S497child1 (int a):a(a){
    (id = S497child1_classId);
{

    }
  }
  int a;
};


int S497child2_classId = Class_genId();
struct S497child2:S497{

  S497child2 (int a):a(a){
    (id = S497child2_classId);
{

    }
  }
  int a;
};


int S497child3_classId = Class_genId();
struct S497child3:S497{

  S497child3 (int a):a(a){
    (id = S497child3_classId);
{

    }
  }
  int a;
};


int S497child4_classId = Class_genId();
struct S497child4:S497{

  S497child4 (int a):a(a){
    (id = S497child4_classId);
{

    }
  }
  int a;
};


struct Trait497{

  int(*trait497)(Class*);
};

Vec* Trait497_v = newVec();

int Trait497_S497child1_trait497(Class* self_) {
  S497child1* self = ((S497child1*)self_);
{
    return (self -> a);
  }
} 
Trait497* newTrait497_S497child1() {
  Trait497 (* impl) = (new Trait497());
  setVec(Trait497_v, S497child1_classId, ((void*)impl));
  ((impl -> trait497) = (& Trait497_S497child1_trait497));
  return impl;
} 
Trait497* Trait497_S497child1_ = newTrait497_S497child1();

int Trait223_S497child1_trait223(Class* self_) {
  S497child1* self = ((S497child1*)self_);
{
    return (self -> a);
  }
} 
Trait223* newTrait223_S497child1() {
  Trait223 (* impl) = (new Trait223());
  setVec(Trait223_v, S497child1_classId, ((void*)impl));
  ((impl -> trait223) = (& Trait223_S497child1_trait223));
  return impl;
} 
Trait223* Trait223_S497child1_ = newTrait223_S497child1();

int S498_classId = Class_genId();
struct S498{

  int id;
  S498 ():id(S498_classId){

  }
};


int S498child1_classId = Class_genId();
struct S498child1:S498{

  S498child1 (int a):a(a){
    (id = S498child1_classId);
{

    }
  }
  int a;
};


int S498child2_classId = Class_genId();
struct S498child2:S498{

  S498child2 (int a):a(a){
    (id = S498child2_classId);
{

    }
  }
  int a;
};


struct Trait498{

  int(*trait498)(Class*);
};

Vec* Trait498_v = newVec();

int Trait498_S498child1_trait498(Class* self_) {
  S498child1* self = ((S498child1*)self_);
{
    return (self -> a);
  }
} 
Trait498* newTrait498_S498child1() {
  Trait498 (* impl) = (new Trait498());
  setVec(Trait498_v, S498child1_classId, ((void*)impl));
  ((impl -> trait498) = (& Trait498_S498child1_trait498));
  return impl;
} 
Trait498* Trait498_S498child1_ = newTrait498_S498child1();

int Trait498_S498child2_trait498(Class* self_) {
  S498child2* self = ((S498child2*)self_);
{
    return (self -> a);
  }
} 
Trait498* newTrait498_S498child2() {
  Trait498 (* impl) = (new Trait498());
  setVec(Trait498_v, S498child2_classId, ((void*)impl));
  ((impl -> trait498) = (& Trait498_S498child2_trait498));
  return impl;
} 
Trait498* Trait498_S498child2_ = newTrait498_S498child2();

int Trait35_S498child1_trait35(Class* self_) {
  S498child1* self = ((S498child1*)self_);
{
    return (self -> a);
  }
} 
Trait35* newTrait35_S498child1() {
  Trait35 (* impl) = (new Trait35());
  setVec(Trait35_v, S498child1_classId, ((void*)impl));
  ((impl -> trait35) = (& Trait35_S498child1_trait35));
  return impl;
} 
Trait35* Trait35_S498child1_ = newTrait35_S498child1();

int S499_classId = Class_genId();
struct S499{

  int id;
  S499 ():id(S499_classId){

  }
};


int S499child1_classId = Class_genId();
struct S499child1:S499{

  S499child1 (int a):a(a){
    (id = S499child1_classId);
{

    }
  }
  int a;
};


int S499child2_classId = Class_genId();
struct S499child2:S499{

  S499child2 (int a):a(a){
    (id = S499child2_classId);
{

    }
  }
  int a;
};


int S499child3_classId = Class_genId();
struct S499child3:S499{

  S499child3 (int a):a(a){
    (id = S499child3_classId);
{

    }
  }
  int a;
};


struct Trait499{

  int(*trait499)(Class*);
};

Vec* Trait499_v = newVec();

int Trait499_S499child1_trait499(Class* self_) {
  S499child1* self = ((S499child1*)self_);
{
    return (self -> a);
  }
} 
Trait499* newTrait499_S499child1() {
  Trait499 (* impl) = (new Trait499());
  setVec(Trait499_v, S499child1_classId, ((void*)impl));
  ((impl -> trait499) = (& Trait499_S499child1_trait499));
  return impl;
} 
Trait499* Trait499_S499child1_ = newTrait499_S499child1();

int Trait328_S499child1_trait328(Class* self_) {
  S499child1* self = ((S499child1*)self_);
{
    return (self -> a);
  }
} 
Trait328* newTrait328_S499child1() {
  Trait328 (* impl) = (new Trait328());
  setVec(Trait328_v, S499child1_classId, ((void*)impl));
  ((impl -> trait328) = (& Trait328_S499child1_trait328));
  return impl;
} 
Trait328* Trait328_S499child1_ = newTrait328_S499child1();

int S500_classId = Class_genId();
struct S500{

  int id;
  S500 ():id(S500_classId){

  }
};


int S500child1_classId = Class_genId();
struct S500child1:S500{

  S500child1 (int a):a(a){
    (id = S500child1_classId);
{

    }
  }
  int a;
};


int S500child2_classId = Class_genId();
struct S500child2:S500{

  S500child2 (int a):a(a){
    (id = S500child2_classId);
{

    }
  }
  int a;
};


int S500child3_classId = Class_genId();
struct S500child3:S500{

  S500child3 (int a):a(a){
    (id = S500child3_classId);
{

    }
  }
  int a;
};


struct Trait500{

  int(*trait500)(Class*);
};

Vec* Trait500_v = newVec();

int Trait500_S500child1_trait500(Class* self_) {
  S500child1* self = ((S500child1*)self_);
{
    return (self -> a);
  }
} 
Trait500* newTrait500_S500child1() {
  Trait500 (* impl) = (new Trait500());
  setVec(Trait500_v, S500child1_classId, ((void*)impl));
  ((impl -> trait500) = (& Trait500_S500child1_trait500));
  return impl;
} 
Trait500* Trait500_S500child1_ = newTrait500_S500child1();

int Trait201_S500child1_trait201(Class* self_) {
  S500child1* self = ((S500child1*)self_);
{
    return (self -> a);
  }
} 
Trait201* newTrait201_S500child1() {
  Trait201 (* impl) = (new Trait201());
  setVec(Trait201_v, S500child1_classId, ((void*)impl));
  ((impl -> trait201) = (& Trait201_S500child1_trait201));
  return impl;
} 
Trait201* Trait201_S500child1_ = newTrait201_S500child1();

int S501_classId = Class_genId();
struct S501{

  int id;
  S501 ():id(S501_classId){

  }
};


int S501child1_classId = Class_genId();
struct S501child1:S501{

  S501child1 (int a):a(a){
    (id = S501child1_classId);
{

    }
  }
  int a;
};


int S501child2_classId = Class_genId();
struct S501child2:S501{

  S501child2 (int a):a(a){
    (id = S501child2_classId);
{

    }
  }
  int a;
};


int S501child3_classId = Class_genId();
struct S501child3:S501{

  S501child3 (int a):a(a){
    (id = S501child3_classId);
{

    }
  }
  int a;
};


struct Trait501{

  int(*trait501)(Class*);
};

Vec* Trait501_v = newVec();

int Trait501_S501child1_trait501(Class* self_) {
  S501child1* self = ((S501child1*)self_);
{
    return (self -> a);
  }
} 
Trait501* newTrait501_S501child1() {
  Trait501 (* impl) = (new Trait501());
  setVec(Trait501_v, S501child1_classId, ((void*)impl));
  ((impl -> trait501) = (& Trait501_S501child1_trait501));
  return impl;
} 
Trait501* Trait501_S501child1_ = newTrait501_S501child1();

int Trait501_S501child2_trait501(Class* self_) {
  S501child2* self = ((S501child2*)self_);
{
    return (self -> a);
  }
} 
Trait501* newTrait501_S501child2() {
  Trait501 (* impl) = (new Trait501());
  setVec(Trait501_v, S501child2_classId, ((void*)impl));
  ((impl -> trait501) = (& Trait501_S501child2_trait501));
  return impl;
} 
Trait501* Trait501_S501child2_ = newTrait501_S501child2();

int Trait501_S501child3_trait501(Class* self_) {
  S501child3* self = ((S501child3*)self_);
{
    return (self -> a);
  }
} 
Trait501* newTrait501_S501child3() {
  Trait501 (* impl) = (new Trait501());
  setVec(Trait501_v, S501child3_classId, ((void*)impl));
  ((impl -> trait501) = (& Trait501_S501child3_trait501));
  return impl;
} 
Trait501* Trait501_S501child3_ = newTrait501_S501child3();

int Trait237_S501child1_trait237(Class* self_) {
  S501child1* self = ((S501child1*)self_);
{
    return (self -> a);
  }
} 
Trait237* newTrait237_S501child1() {
  Trait237 (* impl) = (new Trait237());
  setVec(Trait237_v, S501child1_classId, ((void*)impl));
  ((impl -> trait237) = (& Trait237_S501child1_trait237));
  return impl;
} 
Trait237* Trait237_S501child1_ = newTrait237_S501child1();

int S502_classId = Class_genId();
struct S502{

  int id;
  S502 ():id(S502_classId){

  }
};


int S502child1_classId = Class_genId();
struct S502child1:S502{

  S502child1 (int a):a(a){
    (id = S502child1_classId);
{

    }
  }
  int a;
};


struct Trait502{

  int(*trait502)(Class*);
};

Vec* Trait502_v = newVec();

int Trait502_S502child1_trait502(Class* self_) {
  S502child1* self = ((S502child1*)self_);
{
    return (self -> a);
  }
} 
Trait502* newTrait502_S502child1() {
  Trait502 (* impl) = (new Trait502());
  setVec(Trait502_v, S502child1_classId, ((void*)impl));
  ((impl -> trait502) = (& Trait502_S502child1_trait502));
  return impl;
} 
Trait502* Trait502_S502child1_ = newTrait502_S502child1();

int Trait173_S502child1_trait173(Class* self_) {
  S502child1* self = ((S502child1*)self_);
{
    return (self -> a);
  }
} 
Trait173* newTrait173_S502child1() {
  Trait173 (* impl) = (new Trait173());
  setVec(Trait173_v, S502child1_classId, ((void*)impl));
  ((impl -> trait173) = (& Trait173_S502child1_trait173));
  return impl;
} 
Trait173* Trait173_S502child1_ = newTrait173_S502child1();

int S503_classId = Class_genId();
struct S503{

  int id;
  S503 ():id(S503_classId){

  }
};


int S503child1_classId = Class_genId();
struct S503child1:S503{

  S503child1 (int a):a(a){
    (id = S503child1_classId);
{

    }
  }
  int a;
};


int S503child2_classId = Class_genId();
struct S503child2:S503{

  S503child2 (int a):a(a){
    (id = S503child2_classId);
{

    }
  }
  int a;
};


int S503child3_classId = Class_genId();
struct S503child3:S503{

  S503child3 (int a):a(a){
    (id = S503child3_classId);
{

    }
  }
  int a;
};


struct Trait503{

  int(*trait503)(Class*);
};

Vec* Trait503_v = newVec();

int Trait503_S503child1_trait503(Class* self_) {
  S503child1* self = ((S503child1*)self_);
{
    return (self -> a);
  }
} 
Trait503* newTrait503_S503child1() {
  Trait503 (* impl) = (new Trait503());
  setVec(Trait503_v, S503child1_classId, ((void*)impl));
  ((impl -> trait503) = (& Trait503_S503child1_trait503));
  return impl;
} 
Trait503* Trait503_S503child1_ = newTrait503_S503child1();

int Trait217_S503child1_trait217(Class* self_) {
  S503child1* self = ((S503child1*)self_);
{
    return (self -> a);
  }
} 
Trait217* newTrait217_S503child1() {
  Trait217 (* impl) = (new Trait217());
  setVec(Trait217_v, S503child1_classId, ((void*)impl));
  ((impl -> trait217) = (& Trait217_S503child1_trait217));
  return impl;
} 
Trait217* Trait217_S503child1_ = newTrait217_S503child1();

int S504_classId = Class_genId();
struct S504{

  int id;
  S504 ():id(S504_classId){

  }
};


int S504child1_classId = Class_genId();
struct S504child1:S504{

  S504child1 (int a):a(a){
    (id = S504child1_classId);
{

    }
  }
  int a;
};


int S504child2_classId = Class_genId();
struct S504child2:S504{

  S504child2 (int a):a(a){
    (id = S504child2_classId);
{

    }
  }
  int a;
};


struct Trait504{

  int(*trait504)(Class*);
};

Vec* Trait504_v = newVec();

int Trait504_S504child1_trait504(Class* self_) {
  S504child1* self = ((S504child1*)self_);
{
    return (self -> a);
  }
} 
Trait504* newTrait504_S504child1() {
  Trait504 (* impl) = (new Trait504());
  setVec(Trait504_v, S504child1_classId, ((void*)impl));
  ((impl -> trait504) = (& Trait504_S504child1_trait504));
  return impl;
} 
Trait504* Trait504_S504child1_ = newTrait504_S504child1();

int Trait314_S504child1_trait314(Class* self_) {
  S504child1* self = ((S504child1*)self_);
{
    return (self -> a);
  }
} 
Trait314* newTrait314_S504child1() {
  Trait314 (* impl) = (new Trait314());
  setVec(Trait314_v, S504child1_classId, ((void*)impl));
  ((impl -> trait314) = (& Trait314_S504child1_trait314));
  return impl;
} 
Trait314* Trait314_S504child1_ = newTrait314_S504child1();

int S505_classId = Class_genId();
struct S505{

  int id;
  S505 ():id(S505_classId){

  }
};


int S505child1_classId = Class_genId();
struct S505child1:S505{

  S505child1 (int a):a(a){
    (id = S505child1_classId);
{

    }
  }
  int a;
};


int S505child2_classId = Class_genId();
struct S505child2:S505{

  S505child2 (int a):a(a){
    (id = S505child2_classId);
{

    }
  }
  int a;
};


int S505child3_classId = Class_genId();
struct S505child3:S505{

  S505child3 (int a):a(a){
    (id = S505child3_classId);
{

    }
  }
  int a;
};


int S505child4_classId = Class_genId();
struct S505child4:S505{

  S505child4 (int a):a(a){
    (id = S505child4_classId);
{

    }
  }
  int a;
};


struct Trait505{

  int(*trait505)(Class*);
};

Vec* Trait505_v = newVec();

int Trait505_S505child1_trait505(Class* self_) {
  S505child1* self = ((S505child1*)self_);
{
    return (self -> a);
  }
} 
Trait505* newTrait505_S505child1() {
  Trait505 (* impl) = (new Trait505());
  setVec(Trait505_v, S505child1_classId, ((void*)impl));
  ((impl -> trait505) = (& Trait505_S505child1_trait505));
  return impl;
} 
Trait505* Trait505_S505child1_ = newTrait505_S505child1();

int Trait505_S505child2_trait505(Class* self_) {
  S505child2* self = ((S505child2*)self_);
{
    return (self -> a);
  }
} 
Trait505* newTrait505_S505child2() {
  Trait505 (* impl) = (new Trait505());
  setVec(Trait505_v, S505child2_classId, ((void*)impl));
  ((impl -> trait505) = (& Trait505_S505child2_trait505));
  return impl;
} 
Trait505* Trait505_S505child2_ = newTrait505_S505child2();

int Trait44_S505child1_trait44(Class* self_) {
  S505child1* self = ((S505child1*)self_);
{
    return (self -> a);
  }
} 
Trait44* newTrait44_S505child1() {
  Trait44 (* impl) = (new Trait44());
  setVec(Trait44_v, S505child1_classId, ((void*)impl));
  ((impl -> trait44) = (& Trait44_S505child1_trait44));
  return impl;
} 
Trait44* Trait44_S505child1_ = newTrait44_S505child1();

int S506_classId = Class_genId();
struct S506{

  int id;
  S506 ():id(S506_classId){

  }
};


int S506child1_classId = Class_genId();
struct S506child1:S506{

  S506child1 (int a):a(a){
    (id = S506child1_classId);
{

    }
  }
  int a;
};


int S506child2_classId = Class_genId();
struct S506child2:S506{

  S506child2 (int a):a(a){
    (id = S506child2_classId);
{

    }
  }
  int a;
};


int S506child3_classId = Class_genId();
struct S506child3:S506{

  S506child3 (int a):a(a){
    (id = S506child3_classId);
{

    }
  }
  int a;
};


struct Trait506{

  int(*trait506)(Class*);
};

Vec* Trait506_v = newVec();

int Trait506_S506child1_trait506(Class* self_) {
  S506child1* self = ((S506child1*)self_);
{
    return (self -> a);
  }
} 
Trait506* newTrait506_S506child1() {
  Trait506 (* impl) = (new Trait506());
  setVec(Trait506_v, S506child1_classId, ((void*)impl));
  ((impl -> trait506) = (& Trait506_S506child1_trait506));
  return impl;
} 
Trait506* Trait506_S506child1_ = newTrait506_S506child1();

int Trait506_S506child2_trait506(Class* self_) {
  S506child2* self = ((S506child2*)self_);
{
    return (self -> a);
  }
} 
Trait506* newTrait506_S506child2() {
  Trait506 (* impl) = (new Trait506());
  setVec(Trait506_v, S506child2_classId, ((void*)impl));
  ((impl -> trait506) = (& Trait506_S506child2_trait506));
  return impl;
} 
Trait506* Trait506_S506child2_ = newTrait506_S506child2();

int Trait484_S506child1_trait484(Class* self_) {
  S506child1* self = ((S506child1*)self_);
{
    return (self -> a);
  }
} 
Trait484* newTrait484_S506child1() {
  Trait484 (* impl) = (new Trait484());
  setVec(Trait484_v, S506child1_classId, ((void*)impl));
  ((impl -> trait484) = (& Trait484_S506child1_trait484));
  return impl;
} 
Trait484* Trait484_S506child1_ = newTrait484_S506child1();

int S507_classId = Class_genId();
struct S507{

  int id;
  S507 ():id(S507_classId){

  }
};


int S507child1_classId = Class_genId();
struct S507child1:S507{

  S507child1 (int a):a(a){
    (id = S507child1_classId);
{

    }
  }
  int a;
};


int S507child2_classId = Class_genId();
struct S507child2:S507{

  S507child2 (int a):a(a){
    (id = S507child2_classId);
{

    }
  }
  int a;
};


int S507child3_classId = Class_genId();
struct S507child3:S507{

  S507child3 (int a):a(a){
    (id = S507child3_classId);
{

    }
  }
  int a;
};


struct Trait507{

  int(*trait507)(Class*);
};

Vec* Trait507_v = newVec();

int Trait507_S507child1_trait507(Class* self_) {
  S507child1* self = ((S507child1*)self_);
{
    return (self -> a);
  }
} 
Trait507* newTrait507_S507child1() {
  Trait507 (* impl) = (new Trait507());
  setVec(Trait507_v, S507child1_classId, ((void*)impl));
  ((impl -> trait507) = (& Trait507_S507child1_trait507));
  return impl;
} 
Trait507* Trait507_S507child1_ = newTrait507_S507child1();

int Trait507_S507child2_trait507(Class* self_) {
  S507child2* self = ((S507child2*)self_);
{
    return (self -> a);
  }
} 
Trait507* newTrait507_S507child2() {
  Trait507 (* impl) = (new Trait507());
  setVec(Trait507_v, S507child2_classId, ((void*)impl));
  ((impl -> trait507) = (& Trait507_S507child2_trait507));
  return impl;
} 
Trait507* Trait507_S507child2_ = newTrait507_S507child2();

int Trait507_S507child3_trait507(Class* self_) {
  S507child3* self = ((S507child3*)self_);
{
    return (self -> a);
  }
} 
Trait507* newTrait507_S507child3() {
  Trait507 (* impl) = (new Trait507());
  setVec(Trait507_v, S507child3_classId, ((void*)impl));
  ((impl -> trait507) = (& Trait507_S507child3_trait507));
  return impl;
} 
Trait507* Trait507_S507child3_ = newTrait507_S507child3();

int Trait56_S507child1_trait56(Class* self_) {
  S507child1* self = ((S507child1*)self_);
{
    return (self -> a);
  }
} 
Trait56* newTrait56_S507child1() {
  Trait56 (* impl) = (new Trait56());
  setVec(Trait56_v, S507child1_classId, ((void*)impl));
  ((impl -> trait56) = (& Trait56_S507child1_trait56));
  return impl;
} 
Trait56* Trait56_S507child1_ = newTrait56_S507child1();

int S508_classId = Class_genId();
struct S508{

  int id;
  S508 ():id(S508_classId){

  }
};


int S508child1_classId = Class_genId();
struct S508child1:S508{

  S508child1 (int a):a(a){
    (id = S508child1_classId);
{

    }
  }
  int a;
};


struct Trait508{

  int(*trait508)(Class*);
};

Vec* Trait508_v = newVec();

int Trait508_S508child1_trait508(Class* self_) {
  S508child1* self = ((S508child1*)self_);
{
    return (self -> a);
  }
} 
Trait508* newTrait508_S508child1() {
  Trait508 (* impl) = (new Trait508());
  setVec(Trait508_v, S508child1_classId, ((void*)impl));
  ((impl -> trait508) = (& Trait508_S508child1_trait508));
  return impl;
} 
Trait508* Trait508_S508child1_ = newTrait508_S508child1();

int Trait320_S508child1_trait320(Class* self_) {
  S508child1* self = ((S508child1*)self_);
{
    return (self -> a);
  }
} 
Trait320* newTrait320_S508child1() {
  Trait320 (* impl) = (new Trait320());
  setVec(Trait320_v, S508child1_classId, ((void*)impl));
  ((impl -> trait320) = (& Trait320_S508child1_trait320));
  return impl;
} 
Trait320* Trait320_S508child1_ = newTrait320_S508child1();

int S509_classId = Class_genId();
struct S509{

  int id;
  S509 ():id(S509_classId){

  }
};


int S509child1_classId = Class_genId();
struct S509child1:S509{

  S509child1 (int a):a(a){
    (id = S509child1_classId);
{

    }
  }
  int a;
};


struct Trait509{

  int(*trait509)(Class*);
};

Vec* Trait509_v = newVec();

int Trait509_S509child1_trait509(Class* self_) {
  S509child1* self = ((S509child1*)self_);
{
    return (self -> a);
  }
} 
Trait509* newTrait509_S509child1() {
  Trait509 (* impl) = (new Trait509());
  setVec(Trait509_v, S509child1_classId, ((void*)impl));
  ((impl -> trait509) = (& Trait509_S509child1_trait509));
  return impl;
} 
Trait509* Trait509_S509child1_ = newTrait509_S509child1();

int Trait151_S509child1_trait151(Class* self_) {
  S509child1* self = ((S509child1*)self_);
{
    return (self -> a);
  }
} 
Trait151* newTrait151_S509child1() {
  Trait151 (* impl) = (new Trait151());
  setVec(Trait151_v, S509child1_classId, ((void*)impl));
  ((impl -> trait151) = (& Trait151_S509child1_trait151));
  return impl;
} 
Trait151* Trait151_S509child1_ = newTrait151_S509child1();

int S510_classId = Class_genId();
struct S510{

  int id;
  S510 ():id(S510_classId){

  }
};


int S510child1_classId = Class_genId();
struct S510child1:S510{

  S510child1 (int a):a(a){
    (id = S510child1_classId);
{

    }
  }
  int a;
};


int S510child2_classId = Class_genId();
struct S510child2:S510{

  S510child2 (int a):a(a){
    (id = S510child2_classId);
{

    }
  }
  int a;
};


int S510child3_classId = Class_genId();
struct S510child3:S510{

  S510child3 (int a):a(a){
    (id = S510child3_classId);
{

    }
  }
  int a;
};


struct Trait510{

  int(*trait510)(Class*);
};

Vec* Trait510_v = newVec();

int Trait510_S510child1_trait510(Class* self_) {
  S510child1* self = ((S510child1*)self_);
{
    return (self -> a);
  }
} 
Trait510* newTrait510_S510child1() {
  Trait510 (* impl) = (new Trait510());
  setVec(Trait510_v, S510child1_classId, ((void*)impl));
  ((impl -> trait510) = (& Trait510_S510child1_trait510));
  return impl;
} 
Trait510* Trait510_S510child1_ = newTrait510_S510child1();

int Trait393_S510child1_trait393(Class* self_) {
  S510child1* self = ((S510child1*)self_);
{
    return (self -> a);
  }
} 
Trait393* newTrait393_S510child1() {
  Trait393 (* impl) = (new Trait393());
  setVec(Trait393_v, S510child1_classId, ((void*)impl));
  ((impl -> trait393) = (& Trait393_S510child1_trait393));
  return impl;
} 
Trait393* Trait393_S510child1_ = newTrait393_S510child1();

int S511_classId = Class_genId();
struct S511{

  int id;
  S511 ():id(S511_classId){

  }
};


int S511child1_classId = Class_genId();
struct S511child1:S511{

  S511child1 (int a):a(a){
    (id = S511child1_classId);
{

    }
  }
  int a;
};


int S511child2_classId = Class_genId();
struct S511child2:S511{

  S511child2 (int a):a(a){
    (id = S511child2_classId);
{

    }
  }
  int a;
};


int S511child3_classId = Class_genId();
struct S511child3:S511{

  S511child3 (int a):a(a){
    (id = S511child3_classId);
{

    }
  }
  int a;
};


struct Trait511{

  int(*trait511)(Class*);
};

Vec* Trait511_v = newVec();

int Trait511_S511child1_trait511(Class* self_) {
  S511child1* self = ((S511child1*)self_);
{
    return (self -> a);
  }
} 
Trait511* newTrait511_S511child1() {
  Trait511 (* impl) = (new Trait511());
  setVec(Trait511_v, S511child1_classId, ((void*)impl));
  ((impl -> trait511) = (& Trait511_S511child1_trait511));
  return impl;
} 
Trait511* Trait511_S511child1_ = newTrait511_S511child1();

int Trait511_S511child2_trait511(Class* self_) {
  S511child2* self = ((S511child2*)self_);
{
    return (self -> a);
  }
} 
Trait511* newTrait511_S511child2() {
  Trait511 (* impl) = (new Trait511());
  setVec(Trait511_v, S511child2_classId, ((void*)impl));
  ((impl -> trait511) = (& Trait511_S511child2_trait511));
  return impl;
} 
Trait511* Trait511_S511child2_ = newTrait511_S511child2();

int Trait511_S511child3_trait511(Class* self_) {
  S511child3* self = ((S511child3*)self_);
{
    return (self -> a);
  }
} 
Trait511* newTrait511_S511child3() {
  Trait511 (* impl) = (new Trait511());
  setVec(Trait511_v, S511child3_classId, ((void*)impl));
  ((impl -> trait511) = (& Trait511_S511child3_trait511));
  return impl;
} 
Trait511* Trait511_S511child3_ = newTrait511_S511child3();

int Trait192_S511child1_trait192(Class* self_) {
  S511child1* self = ((S511child1*)self_);
{
    return (self -> a);
  }
} 
Trait192* newTrait192_S511child1() {
  Trait192 (* impl) = (new Trait192());
  setVec(Trait192_v, S511child1_classId, ((void*)impl));
  ((impl -> trait192) = (& Trait192_S511child1_trait192));
  return impl;
} 
Trait192* Trait192_S511child1_ = newTrait192_S511child1();

int S512_classId = Class_genId();
struct S512{

  int id;
  S512 ():id(S512_classId){

  }
};


int S512child1_classId = Class_genId();
struct S512child1:S512{

  S512child1 (int a):a(a){
    (id = S512child1_classId);
{

    }
  }
  int a;
};


struct Trait512{

  int(*trait512)(Class*);
};

Vec* Trait512_v = newVec();

int Trait512_S512child1_trait512(Class* self_) {
  S512child1* self = ((S512child1*)self_);
{
    return (self -> a);
  }
} 
Trait512* newTrait512_S512child1() {
  Trait512 (* impl) = (new Trait512());
  setVec(Trait512_v, S512child1_classId, ((void*)impl));
  ((impl -> trait512) = (& Trait512_S512child1_trait512));
  return impl;
} 
Trait512* Trait512_S512child1_ = newTrait512_S512child1();

int Trait136_S512child1_trait136(Class* self_) {
  S512child1* self = ((S512child1*)self_);
{
    return (self -> a);
  }
} 
Trait136* newTrait136_S512child1() {
  Trait136 (* impl) = (new Trait136());
  setVec(Trait136_v, S512child1_classId, ((void*)impl));
  ((impl -> trait136) = (& Trait136_S512child1_trait136));
  return impl;
} 
Trait136* Trait136_S512child1_ = newTrait136_S512child1();

int S513_classId = Class_genId();
struct S513{

  int id;
  S513 ():id(S513_classId){

  }
};


int S513child1_classId = Class_genId();
struct S513child1:S513{

  S513child1 (int a):a(a){
    (id = S513child1_classId);
{

    }
  }
  int a;
};


int S513child2_classId = Class_genId();
struct S513child2:S513{

  S513child2 (int a):a(a){
    (id = S513child2_classId);
{

    }
  }
  int a;
};


struct Trait513{

  int(*trait513)(Class*);
};

Vec* Trait513_v = newVec();

int Trait513_S513child1_trait513(Class* self_) {
  S513child1* self = ((S513child1*)self_);
{
    return (self -> a);
  }
} 
Trait513* newTrait513_S513child1() {
  Trait513 (* impl) = (new Trait513());
  setVec(Trait513_v, S513child1_classId, ((void*)impl));
  ((impl -> trait513) = (& Trait513_S513child1_trait513));
  return impl;
} 
Trait513* Trait513_S513child1_ = newTrait513_S513child1();

int Trait513_S513child2_trait513(Class* self_) {
  S513child2* self = ((S513child2*)self_);
{
    return (self -> a);
  }
} 
Trait513* newTrait513_S513child2() {
  Trait513 (* impl) = (new Trait513());
  setVec(Trait513_v, S513child2_classId, ((void*)impl));
  ((impl -> trait513) = (& Trait513_S513child2_trait513));
  return impl;
} 
Trait513* Trait513_S513child2_ = newTrait513_S513child2();

int Trait166_S513child1_trait166(Class* self_) {
  S513child1* self = ((S513child1*)self_);
{
    return (self -> a);
  }
} 
Trait166* newTrait166_S513child1() {
  Trait166 (* impl) = (new Trait166());
  setVec(Trait166_v, S513child1_classId, ((void*)impl));
  ((impl -> trait166) = (& Trait166_S513child1_trait166));
  return impl;
} 
Trait166* Trait166_S513child1_ = newTrait166_S513child1();

int S514_classId = Class_genId();
struct S514{

  int id;
  S514 ():id(S514_classId){

  }
};


int S514child1_classId = Class_genId();
struct S514child1:S514{

  S514child1 (int a):a(a){
    (id = S514child1_classId);
{

    }
  }
  int a;
};


struct Trait514{

  int(*trait514)(Class*);
};

Vec* Trait514_v = newVec();

int Trait514_S514child1_trait514(Class* self_) {
  S514child1* self = ((S514child1*)self_);
{
    return (self -> a);
  }
} 
Trait514* newTrait514_S514child1() {
  Trait514 (* impl) = (new Trait514());
  setVec(Trait514_v, S514child1_classId, ((void*)impl));
  ((impl -> trait514) = (& Trait514_S514child1_trait514));
  return impl;
} 
Trait514* Trait514_S514child1_ = newTrait514_S514child1();

int Trait192_S514child1_trait192(Class* self_) {
  S514child1* self = ((S514child1*)self_);
{
    return (self -> a);
  }
} 
Trait192* newTrait192_S514child1() {
  Trait192 (* impl) = (new Trait192());
  setVec(Trait192_v, S514child1_classId, ((void*)impl));
  ((impl -> trait192) = (& Trait192_S514child1_trait192));
  return impl;
} 
Trait192* Trait192_S514child1_ = newTrait192_S514child1();

int S515_classId = Class_genId();
struct S515{

  int id;
  S515 ():id(S515_classId){

  }
};


int S515child1_classId = Class_genId();
struct S515child1:S515{

  S515child1 (int a):a(a){
    (id = S515child1_classId);
{

    }
  }
  int a;
};


int S515child2_classId = Class_genId();
struct S515child2:S515{

  S515child2 (int a):a(a){
    (id = S515child2_classId);
{

    }
  }
  int a;
};


struct Trait515{

  int(*trait515)(Class*);
};

Vec* Trait515_v = newVec();

int Trait515_S515child1_trait515(Class* self_) {
  S515child1* self = ((S515child1*)self_);
{
    return (self -> a);
  }
} 
Trait515* newTrait515_S515child1() {
  Trait515 (* impl) = (new Trait515());
  setVec(Trait515_v, S515child1_classId, ((void*)impl));
  ((impl -> trait515) = (& Trait515_S515child1_trait515));
  return impl;
} 
Trait515* Trait515_S515child1_ = newTrait515_S515child1();

int Trait222_S515child1_trait222(Class* self_) {
  S515child1* self = ((S515child1*)self_);
{
    return (self -> a);
  }
} 
Trait222* newTrait222_S515child1() {
  Trait222 (* impl) = (new Trait222());
  setVec(Trait222_v, S515child1_classId, ((void*)impl));
  ((impl -> trait222) = (& Trait222_S515child1_trait222));
  return impl;
} 
Trait222* Trait222_S515child1_ = newTrait222_S515child1();

int S516_classId = Class_genId();
struct S516{

  int id;
  S516 ():id(S516_classId){

  }
};


int S516child1_classId = Class_genId();
struct S516child1:S516{

  S516child1 (int a):a(a){
    (id = S516child1_classId);
{

    }
  }
  int a;
};


int S516child2_classId = Class_genId();
struct S516child2:S516{

  S516child2 (int a):a(a){
    (id = S516child2_classId);
{

    }
  }
  int a;
};


int S516child3_classId = Class_genId();
struct S516child3:S516{

  S516child3 (int a):a(a){
    (id = S516child3_classId);
{

    }
  }
  int a;
};


int S516child4_classId = Class_genId();
struct S516child4:S516{

  S516child4 (int a):a(a){
    (id = S516child4_classId);
{

    }
  }
  int a;
};


struct Trait516{

  int(*trait516)(Class*);
};

Vec* Trait516_v = newVec();

int Trait516_S516child1_trait516(Class* self_) {
  S516child1* self = ((S516child1*)self_);
{
    return (self -> a);
  }
} 
Trait516* newTrait516_S516child1() {
  Trait516 (* impl) = (new Trait516());
  setVec(Trait516_v, S516child1_classId, ((void*)impl));
  ((impl -> trait516) = (& Trait516_S516child1_trait516));
  return impl;
} 
Trait516* Trait516_S516child1_ = newTrait516_S516child1();

int Trait516_S516child2_trait516(Class* self_) {
  S516child2* self = ((S516child2*)self_);
{
    return (self -> a);
  }
} 
Trait516* newTrait516_S516child2() {
  Trait516 (* impl) = (new Trait516());
  setVec(Trait516_v, S516child2_classId, ((void*)impl));
  ((impl -> trait516) = (& Trait516_S516child2_trait516));
  return impl;
} 
Trait516* Trait516_S516child2_ = newTrait516_S516child2();

int Trait516_S516child3_trait516(Class* self_) {
  S516child3* self = ((S516child3*)self_);
{
    return (self -> a);
  }
} 
Trait516* newTrait516_S516child3() {
  Trait516 (* impl) = (new Trait516());
  setVec(Trait516_v, S516child3_classId, ((void*)impl));
  ((impl -> trait516) = (& Trait516_S516child3_trait516));
  return impl;
} 
Trait516* Trait516_S516child3_ = newTrait516_S516child3();

int Trait282_S516child1_trait282(Class* self_) {
  S516child1* self = ((S516child1*)self_);
{
    return (self -> a);
  }
} 
Trait282* newTrait282_S516child1() {
  Trait282 (* impl) = (new Trait282());
  setVec(Trait282_v, S516child1_classId, ((void*)impl));
  ((impl -> trait282) = (& Trait282_S516child1_trait282));
  return impl;
} 
Trait282* Trait282_S516child1_ = newTrait282_S516child1();

int S517_classId = Class_genId();
struct S517{

  int id;
  S517 ():id(S517_classId){

  }
};


int S517child1_classId = Class_genId();
struct S517child1:S517{

  S517child1 (int a):a(a){
    (id = S517child1_classId);
{

    }
  }
  int a;
};


int S517child2_classId = Class_genId();
struct S517child2:S517{

  S517child2 (int a):a(a){
    (id = S517child2_classId);
{

    }
  }
  int a;
};


int S517child3_classId = Class_genId();
struct S517child3:S517{

  S517child3 (int a):a(a){
    (id = S517child3_classId);
{

    }
  }
  int a;
};


int S517child4_classId = Class_genId();
struct S517child4:S517{

  S517child4 (int a):a(a){
    (id = S517child4_classId);
{

    }
  }
  int a;
};


struct Trait517{

  int(*trait517)(Class*);
};

Vec* Trait517_v = newVec();

int Trait517_S517child1_trait517(Class* self_) {
  S517child1* self = ((S517child1*)self_);
{
    return (self -> a);
  }
} 
Trait517* newTrait517_S517child1() {
  Trait517 (* impl) = (new Trait517());
  setVec(Trait517_v, S517child1_classId, ((void*)impl));
  ((impl -> trait517) = (& Trait517_S517child1_trait517));
  return impl;
} 
Trait517* Trait517_S517child1_ = newTrait517_S517child1();

int Trait334_S517child1_trait334(Class* self_) {
  S517child1* self = ((S517child1*)self_);
{
    return (self -> a);
  }
} 
Trait334* newTrait334_S517child1() {
  Trait334 (* impl) = (new Trait334());
  setVec(Trait334_v, S517child1_classId, ((void*)impl));
  ((impl -> trait334) = (& Trait334_S517child1_trait334));
  return impl;
} 
Trait334* Trait334_S517child1_ = newTrait334_S517child1();

int S518_classId = Class_genId();
struct S518{

  int id;
  S518 ():id(S518_classId){

  }
};


int S518child1_classId = Class_genId();
struct S518child1:S518{

  S518child1 (int a):a(a){
    (id = S518child1_classId);
{

    }
  }
  int a;
};


int S518child2_classId = Class_genId();
struct S518child2:S518{

  S518child2 (int a):a(a){
    (id = S518child2_classId);
{

    }
  }
  int a;
};


struct Trait518{

  int(*trait518)(Class*);
};

Vec* Trait518_v = newVec();

int Trait518_S518child1_trait518(Class* self_) {
  S518child1* self = ((S518child1*)self_);
{
    return (self -> a);
  }
} 
Trait518* newTrait518_S518child1() {
  Trait518 (* impl) = (new Trait518());
  setVec(Trait518_v, S518child1_classId, ((void*)impl));
  ((impl -> trait518) = (& Trait518_S518child1_trait518));
  return impl;
} 
Trait518* Trait518_S518child1_ = newTrait518_S518child1();

int Trait188_S518child1_trait188(Class* self_) {
  S518child1* self = ((S518child1*)self_);
{
    return (self -> a);
  }
} 
Trait188* newTrait188_S518child1() {
  Trait188 (* impl) = (new Trait188());
  setVec(Trait188_v, S518child1_classId, ((void*)impl));
  ((impl -> trait188) = (& Trait188_S518child1_trait188));
  return impl;
} 
Trait188* Trait188_S518child1_ = newTrait188_S518child1();

int S519_classId = Class_genId();
struct S519{

  int id;
  S519 ():id(S519_classId){

  }
};


int S519child1_classId = Class_genId();
struct S519child1:S519{

  S519child1 (int a):a(a){
    (id = S519child1_classId);
{

    }
  }
  int a;
};


int S519child2_classId = Class_genId();
struct S519child2:S519{

  S519child2 (int a):a(a){
    (id = S519child2_classId);
{

    }
  }
  int a;
};


int S519child3_classId = Class_genId();
struct S519child3:S519{

  S519child3 (int a):a(a){
    (id = S519child3_classId);
{

    }
  }
  int a;
};


struct Trait519{

  int(*trait519)(Class*);
};

Vec* Trait519_v = newVec();

int Trait519_S519child1_trait519(Class* self_) {
  S519child1* self = ((S519child1*)self_);
{
    return (self -> a);
  }
} 
Trait519* newTrait519_S519child1() {
  Trait519 (* impl) = (new Trait519());
  setVec(Trait519_v, S519child1_classId, ((void*)impl));
  ((impl -> trait519) = (& Trait519_S519child1_trait519));
  return impl;
} 
Trait519* Trait519_S519child1_ = newTrait519_S519child1();

int Trait519_S519child2_trait519(Class* self_) {
  S519child2* self = ((S519child2*)self_);
{
    return (self -> a);
  }
} 
Trait519* newTrait519_S519child2() {
  Trait519 (* impl) = (new Trait519());
  setVec(Trait519_v, S519child2_classId, ((void*)impl));
  ((impl -> trait519) = (& Trait519_S519child2_trait519));
  return impl;
} 
Trait519* Trait519_S519child2_ = newTrait519_S519child2();

int Trait132_S519child1_trait132(Class* self_) {
  S519child1* self = ((S519child1*)self_);
{
    return (self -> a);
  }
} 
Trait132* newTrait132_S519child1() {
  Trait132 (* impl) = (new Trait132());
  setVec(Trait132_v, S519child1_classId, ((void*)impl));
  ((impl -> trait132) = (& Trait132_S519child1_trait132));
  return impl;
} 
Trait132* Trait132_S519child1_ = newTrait132_S519child1();

int S520_classId = Class_genId();
struct S520{

  int id;
  S520 ():id(S520_classId){

  }
};


int S520child1_classId = Class_genId();
struct S520child1:S520{

  S520child1 (int a):a(a){
    (id = S520child1_classId);
{

    }
  }
  int a;
};


int S520child2_classId = Class_genId();
struct S520child2:S520{

  S520child2 (int a):a(a){
    (id = S520child2_classId);
{

    }
  }
  int a;
};


struct Trait520{

  int(*trait520)(Class*);
};

Vec* Trait520_v = newVec();

int Trait520_S520child1_trait520(Class* self_) {
  S520child1* self = ((S520child1*)self_);
{
    return (self -> a);
  }
} 
Trait520* newTrait520_S520child1() {
  Trait520 (* impl) = (new Trait520());
  setVec(Trait520_v, S520child1_classId, ((void*)impl));
  ((impl -> trait520) = (& Trait520_S520child1_trait520));
  return impl;
} 
Trait520* Trait520_S520child1_ = newTrait520_S520child1();

int Trait312_S520child1_trait312(Class* self_) {
  S520child1* self = ((S520child1*)self_);
{
    return (self -> a);
  }
} 
Trait312* newTrait312_S520child1() {
  Trait312 (* impl) = (new Trait312());
  setVec(Trait312_v, S520child1_classId, ((void*)impl));
  ((impl -> trait312) = (& Trait312_S520child1_trait312));
  return impl;
} 
Trait312* Trait312_S520child1_ = newTrait312_S520child1();

int S521_classId = Class_genId();
struct S521{

  int id;
  S521 ():id(S521_classId){

  }
};


int S521child1_classId = Class_genId();
struct S521child1:S521{

  S521child1 (int a):a(a){
    (id = S521child1_classId);
{

    }
  }
  int a;
};


int S521child2_classId = Class_genId();
struct S521child2:S521{

  S521child2 (int a):a(a){
    (id = S521child2_classId);
{

    }
  }
  int a;
};


struct Trait521{

  int(*trait521)(Class*);
};

Vec* Trait521_v = newVec();

int Trait521_S521child1_trait521(Class* self_) {
  S521child1* self = ((S521child1*)self_);
{
    return (self -> a);
  }
} 
Trait521* newTrait521_S521child1() {
  Trait521 (* impl) = (new Trait521());
  setVec(Trait521_v, S521child1_classId, ((void*)impl));
  ((impl -> trait521) = (& Trait521_S521child1_trait521));
  return impl;
} 
Trait521* Trait521_S521child1_ = newTrait521_S521child1();

int Trait521_S521child2_trait521(Class* self_) {
  S521child2* self = ((S521child2*)self_);
{
    return (self -> a);
  }
} 
Trait521* newTrait521_S521child2() {
  Trait521 (* impl) = (new Trait521());
  setVec(Trait521_v, S521child2_classId, ((void*)impl));
  ((impl -> trait521) = (& Trait521_S521child2_trait521));
  return impl;
} 
Trait521* Trait521_S521child2_ = newTrait521_S521child2();

int Trait127_S521child1_trait127(Class* self_) {
  S521child1* self = ((S521child1*)self_);
{
    return (self -> a);
  }
} 
Trait127* newTrait127_S521child1() {
  Trait127 (* impl) = (new Trait127());
  setVec(Trait127_v, S521child1_classId, ((void*)impl));
  ((impl -> trait127) = (& Trait127_S521child1_trait127));
  return impl;
} 
Trait127* Trait127_S521child1_ = newTrait127_S521child1();

int S522_classId = Class_genId();
struct S522{

  int id;
  S522 ():id(S522_classId){

  }
};


int S522child1_classId = Class_genId();
struct S522child1:S522{

  S522child1 (int a):a(a){
    (id = S522child1_classId);
{

    }
  }
  int a;
};


int S522child2_classId = Class_genId();
struct S522child2:S522{

  S522child2 (int a):a(a){
    (id = S522child2_classId);
{

    }
  }
  int a;
};


int S522child3_classId = Class_genId();
struct S522child3:S522{

  S522child3 (int a):a(a){
    (id = S522child3_classId);
{

    }
  }
  int a;
};


struct Trait522{

  int(*trait522)(Class*);
};

Vec* Trait522_v = newVec();

int Trait522_S522child1_trait522(Class* self_) {
  S522child1* self = ((S522child1*)self_);
{
    return (self -> a);
  }
} 
Trait522* newTrait522_S522child1() {
  Trait522 (* impl) = (new Trait522());
  setVec(Trait522_v, S522child1_classId, ((void*)impl));
  ((impl -> trait522) = (& Trait522_S522child1_trait522));
  return impl;
} 
Trait522* Trait522_S522child1_ = newTrait522_S522child1();

int Trait522_S522child2_trait522(Class* self_) {
  S522child2* self = ((S522child2*)self_);
{
    return (self -> a);
  }
} 
Trait522* newTrait522_S522child2() {
  Trait522 (* impl) = (new Trait522());
  setVec(Trait522_v, S522child2_classId, ((void*)impl));
  ((impl -> trait522) = (& Trait522_S522child2_trait522));
  return impl;
} 
Trait522* Trait522_S522child2_ = newTrait522_S522child2();

int Trait350_S522child1_trait350(Class* self_) {
  S522child1* self = ((S522child1*)self_);
{
    return (self -> a);
  }
} 
Trait350* newTrait350_S522child1() {
  Trait350 (* impl) = (new Trait350());
  setVec(Trait350_v, S522child1_classId, ((void*)impl));
  ((impl -> trait350) = (& Trait350_S522child1_trait350));
  return impl;
} 
Trait350* Trait350_S522child1_ = newTrait350_S522child1();

int S523_classId = Class_genId();
struct S523{

  int id;
  S523 ():id(S523_classId){

  }
};


int S523child1_classId = Class_genId();
struct S523child1:S523{

  S523child1 (int a):a(a){
    (id = S523child1_classId);
{

    }
  }
  int a;
};


int S523child2_classId = Class_genId();
struct S523child2:S523{

  S523child2 (int a):a(a){
    (id = S523child2_classId);
{

    }
  }
  int a;
};


int S523child3_classId = Class_genId();
struct S523child3:S523{

  S523child3 (int a):a(a){
    (id = S523child3_classId);
{

    }
  }
  int a;
};


struct Trait523{

  int(*trait523)(Class*);
};

Vec* Trait523_v = newVec();

int Trait523_S523child1_trait523(Class* self_) {
  S523child1* self = ((S523child1*)self_);
{
    return (self -> a);
  }
} 
Trait523* newTrait523_S523child1() {
  Trait523 (* impl) = (new Trait523());
  setVec(Trait523_v, S523child1_classId, ((void*)impl));
  ((impl -> trait523) = (& Trait523_S523child1_trait523));
  return impl;
} 
Trait523* Trait523_S523child1_ = newTrait523_S523child1();

int Trait523_S523child2_trait523(Class* self_) {
  S523child2* self = ((S523child2*)self_);
{
    return (self -> a);
  }
} 
Trait523* newTrait523_S523child2() {
  Trait523 (* impl) = (new Trait523());
  setVec(Trait523_v, S523child2_classId, ((void*)impl));
  ((impl -> trait523) = (& Trait523_S523child2_trait523));
  return impl;
} 
Trait523* Trait523_S523child2_ = newTrait523_S523child2();

int Trait145_S523child1_trait145(Class* self_) {
  S523child1* self = ((S523child1*)self_);
{
    return (self -> a);
  }
} 
Trait145* newTrait145_S523child1() {
  Trait145 (* impl) = (new Trait145());
  setVec(Trait145_v, S523child1_classId, ((void*)impl));
  ((impl -> trait145) = (& Trait145_S523child1_trait145));
  return impl;
} 
Trait145* Trait145_S523child1_ = newTrait145_S523child1();

int S524_classId = Class_genId();
struct S524{

  int id;
  S524 ():id(S524_classId){

  }
};


int S524child1_classId = Class_genId();
struct S524child1:S524{

  S524child1 (int a):a(a){
    (id = S524child1_classId);
{

    }
  }
  int a;
};


int S524child2_classId = Class_genId();
struct S524child2:S524{

  S524child2 (int a):a(a){
    (id = S524child2_classId);
{

    }
  }
  int a;
};


int S524child3_classId = Class_genId();
struct S524child3:S524{

  S524child3 (int a):a(a){
    (id = S524child3_classId);
{

    }
  }
  int a;
};


int S524child4_classId = Class_genId();
struct S524child4:S524{

  S524child4 (int a):a(a){
    (id = S524child4_classId);
{

    }
  }
  int a;
};


int S524child5_classId = Class_genId();
struct S524child5:S524{

  S524child5 (int a):a(a){
    (id = S524child5_classId);
{

    }
  }
  int a;
};


struct Trait524{

  int(*trait524)(Class*);
};

Vec* Trait524_v = newVec();

int Trait524_S524child1_trait524(Class* self_) {
  S524child1* self = ((S524child1*)self_);
{
    return (self -> a);
  }
} 
Trait524* newTrait524_S524child1() {
  Trait524 (* impl) = (new Trait524());
  setVec(Trait524_v, S524child1_classId, ((void*)impl));
  ((impl -> trait524) = (& Trait524_S524child1_trait524));
  return impl;
} 
Trait524* Trait524_S524child1_ = newTrait524_S524child1();

int Trait524_S524child2_trait524(Class* self_) {
  S524child2* self = ((S524child2*)self_);
{
    return (self -> a);
  }
} 
Trait524* newTrait524_S524child2() {
  Trait524 (* impl) = (new Trait524());
  setVec(Trait524_v, S524child2_classId, ((void*)impl));
  ((impl -> trait524) = (& Trait524_S524child2_trait524));
  return impl;
} 
Trait524* Trait524_S524child2_ = newTrait524_S524child2();

int Trait208_S524child1_trait208(Class* self_) {
  S524child1* self = ((S524child1*)self_);
{
    return (self -> a);
  }
} 
Trait208* newTrait208_S524child1() {
  Trait208 (* impl) = (new Trait208());
  setVec(Trait208_v, S524child1_classId, ((void*)impl));
  ((impl -> trait208) = (& Trait208_S524child1_trait208));
  return impl;
} 
Trait208* Trait208_S524child1_ = newTrait208_S524child1();

int S525_classId = Class_genId();
struct S525{

  int id;
  S525 ():id(S525_classId){

  }
};


int S525child1_classId = Class_genId();
struct S525child1:S525{

  S525child1 (int a):a(a){
    (id = S525child1_classId);
{

    }
  }
  int a;
};


int S525child2_classId = Class_genId();
struct S525child2:S525{

  S525child2 (int a):a(a){
    (id = S525child2_classId);
{

    }
  }
  int a;
};


struct Trait525{

  int(*trait525)(Class*);
};

Vec* Trait525_v = newVec();

int Trait525_S525child1_trait525(Class* self_) {
  S525child1* self = ((S525child1*)self_);
{
    return (self -> a);
  }
} 
Trait525* newTrait525_S525child1() {
  Trait525 (* impl) = (new Trait525());
  setVec(Trait525_v, S525child1_classId, ((void*)impl));
  ((impl -> trait525) = (& Trait525_S525child1_trait525));
  return impl;
} 
Trait525* Trait525_S525child1_ = newTrait525_S525child1();

int Trait525_S525child2_trait525(Class* self_) {
  S525child2* self = ((S525child2*)self_);
{
    return (self -> a);
  }
} 
Trait525* newTrait525_S525child2() {
  Trait525 (* impl) = (new Trait525());
  setVec(Trait525_v, S525child2_classId, ((void*)impl));
  ((impl -> trait525) = (& Trait525_S525child2_trait525));
  return impl;
} 
Trait525* Trait525_S525child2_ = newTrait525_S525child2();

int Trait322_S525child1_trait322(Class* self_) {
  S525child1* self = ((S525child1*)self_);
{
    return (self -> a);
  }
} 
Trait322* newTrait322_S525child1() {
  Trait322 (* impl) = (new Trait322());
  setVec(Trait322_v, S525child1_classId, ((void*)impl));
  ((impl -> trait322) = (& Trait322_S525child1_trait322));
  return impl;
} 
Trait322* Trait322_S525child1_ = newTrait322_S525child1();

int S526_classId = Class_genId();
struct S526{

  int id;
  S526 ():id(S526_classId){

  }
};


int S526child1_classId = Class_genId();
struct S526child1:S526{

  S526child1 (int a):a(a){
    (id = S526child1_classId);
{

    }
  }
  int a;
};


struct Trait526{

  int(*trait526)(Class*);
};

Vec* Trait526_v = newVec();

int Trait526_S526child1_trait526(Class* self_) {
  S526child1* self = ((S526child1*)self_);
{
    return (self -> a);
  }
} 
Trait526* newTrait526_S526child1() {
  Trait526 (* impl) = (new Trait526());
  setVec(Trait526_v, S526child1_classId, ((void*)impl));
  ((impl -> trait526) = (& Trait526_S526child1_trait526));
  return impl;
} 
Trait526* Trait526_S526child1_ = newTrait526_S526child1();

int Trait196_S526child1_trait196(Class* self_) {
  S526child1* self = ((S526child1*)self_);
{
    return (self -> a);
  }
} 
Trait196* newTrait196_S526child1() {
  Trait196 (* impl) = (new Trait196());
  setVec(Trait196_v, S526child1_classId, ((void*)impl));
  ((impl -> trait196) = (& Trait196_S526child1_trait196));
  return impl;
} 
Trait196* Trait196_S526child1_ = newTrait196_S526child1();

int S527_classId = Class_genId();
struct S527{

  int id;
  S527 ():id(S527_classId){

  }
};


int S527child1_classId = Class_genId();
struct S527child1:S527{

  S527child1 (int a):a(a){
    (id = S527child1_classId);
{

    }
  }
  int a;
};


struct Trait527{

  int(*trait527)(Class*);
};

Vec* Trait527_v = newVec();

int Trait527_S527child1_trait527(Class* self_) {
  S527child1* self = ((S527child1*)self_);
{
    return (self -> a);
  }
} 
Trait527* newTrait527_S527child1() {
  Trait527 (* impl) = (new Trait527());
  setVec(Trait527_v, S527child1_classId, ((void*)impl));
  ((impl -> trait527) = (& Trait527_S527child1_trait527));
  return impl;
} 
Trait527* Trait527_S527child1_ = newTrait527_S527child1();

int Trait368_S527child1_trait368(Class* self_) {
  S527child1* self = ((S527child1*)self_);
{
    return (self -> a);
  }
} 
Trait368* newTrait368_S527child1() {
  Trait368 (* impl) = (new Trait368());
  setVec(Trait368_v, S527child1_classId, ((void*)impl));
  ((impl -> trait368) = (& Trait368_S527child1_trait368));
  return impl;
} 
Trait368* Trait368_S527child1_ = newTrait368_S527child1();

int S528_classId = Class_genId();
struct S528{

  int id;
  S528 ():id(S528_classId){

  }
};


int S528child1_classId = Class_genId();
struct S528child1:S528{

  S528child1 (int a):a(a){
    (id = S528child1_classId);
{

    }
  }
  int a;
};


int S528child2_classId = Class_genId();
struct S528child2:S528{

  S528child2 (int a):a(a){
    (id = S528child2_classId);
{

    }
  }
  int a;
};


struct Trait528{

  int(*trait528)(Class*);
};

Vec* Trait528_v = newVec();

int Trait528_S528child1_trait528(Class* self_) {
  S528child1* self = ((S528child1*)self_);
{
    return (self -> a);
  }
} 
Trait528* newTrait528_S528child1() {
  Trait528 (* impl) = (new Trait528());
  setVec(Trait528_v, S528child1_classId, ((void*)impl));
  ((impl -> trait528) = (& Trait528_S528child1_trait528));
  return impl;
} 
Trait528* Trait528_S528child1_ = newTrait528_S528child1();

int Trait528_S528child2_trait528(Class* self_) {
  S528child2* self = ((S528child2*)self_);
{
    return (self -> a);
  }
} 
Trait528* newTrait528_S528child2() {
  Trait528 (* impl) = (new Trait528());
  setVec(Trait528_v, S528child2_classId, ((void*)impl));
  ((impl -> trait528) = (& Trait528_S528child2_trait528));
  return impl;
} 
Trait528* Trait528_S528child2_ = newTrait528_S528child2();

int Trait309_S528child1_trait309(Class* self_) {
  S528child1* self = ((S528child1*)self_);
{
    return (self -> a);
  }
} 
Trait309* newTrait309_S528child1() {
  Trait309 (* impl) = (new Trait309());
  setVec(Trait309_v, S528child1_classId, ((void*)impl));
  ((impl -> trait309) = (& Trait309_S528child1_trait309));
  return impl;
} 
Trait309* Trait309_S528child1_ = newTrait309_S528child1();

int S529_classId = Class_genId();
struct S529{

  int id;
  S529 ():id(S529_classId){

  }
};


int S529child1_classId = Class_genId();
struct S529child1:S529{

  S529child1 (int a):a(a){
    (id = S529child1_classId);
{

    }
  }
  int a;
};


int S529child2_classId = Class_genId();
struct S529child2:S529{

  S529child2 (int a):a(a){
    (id = S529child2_classId);
{

    }
  }
  int a;
};


int S529child3_classId = Class_genId();
struct S529child3:S529{

  S529child3 (int a):a(a){
    (id = S529child3_classId);
{

    }
  }
  int a;
};


struct Trait529{

  int(*trait529)(Class*);
};

Vec* Trait529_v = newVec();

int Trait529_S529child1_trait529(Class* self_) {
  S529child1* self = ((S529child1*)self_);
{
    return (self -> a);
  }
} 
Trait529* newTrait529_S529child1() {
  Trait529 (* impl) = (new Trait529());
  setVec(Trait529_v, S529child1_classId, ((void*)impl));
  ((impl -> trait529) = (& Trait529_S529child1_trait529));
  return impl;
} 
Trait529* Trait529_S529child1_ = newTrait529_S529child1();

int Trait529_S529child2_trait529(Class* self_) {
  S529child2* self = ((S529child2*)self_);
{
    return (self -> a);
  }
} 
Trait529* newTrait529_S529child2() {
  Trait529 (* impl) = (new Trait529());
  setVec(Trait529_v, S529child2_classId, ((void*)impl));
  ((impl -> trait529) = (& Trait529_S529child2_trait529));
  return impl;
} 
Trait529* Trait529_S529child2_ = newTrait529_S529child2();

int Trait529_S529child3_trait529(Class* self_) {
  S529child3* self = ((S529child3*)self_);
{
    return (self -> a);
  }
} 
Trait529* newTrait529_S529child3() {
  Trait529 (* impl) = (new Trait529());
  setVec(Trait529_v, S529child3_classId, ((void*)impl));
  ((impl -> trait529) = (& Trait529_S529child3_trait529));
  return impl;
} 
Trait529* Trait529_S529child3_ = newTrait529_S529child3();

int Trait302_S529child1_trait302(Class* self_) {
  S529child1* self = ((S529child1*)self_);
{
    return (self -> a);
  }
} 
Trait302* newTrait302_S529child1() {
  Trait302 (* impl) = (new Trait302());
  setVec(Trait302_v, S529child1_classId, ((void*)impl));
  ((impl -> trait302) = (& Trait302_S529child1_trait302));
  return impl;
} 
Trait302* Trait302_S529child1_ = newTrait302_S529child1();

int S530_classId = Class_genId();
struct S530{

  int id;
  S530 ():id(S530_classId){

  }
};


int S530child1_classId = Class_genId();
struct S530child1:S530{

  S530child1 (int a):a(a){
    (id = S530child1_classId);
{

    }
  }
  int a;
};


struct Trait530{

  int(*trait530)(Class*);
};

Vec* Trait530_v = newVec();

int Trait530_S530child1_trait530(Class* self_) {
  S530child1* self = ((S530child1*)self_);
{
    return (self -> a);
  }
} 
Trait530* newTrait530_S530child1() {
  Trait530 (* impl) = (new Trait530());
  setVec(Trait530_v, S530child1_classId, ((void*)impl));
  ((impl -> trait530) = (& Trait530_S530child1_trait530));
  return impl;
} 
Trait530* Trait530_S530child1_ = newTrait530_S530child1();

int Trait333_S530child1_trait333(Class* self_) {
  S530child1* self = ((S530child1*)self_);
{
    return (self -> a);
  }
} 
Trait333* newTrait333_S530child1() {
  Trait333 (* impl) = (new Trait333());
  setVec(Trait333_v, S530child1_classId, ((void*)impl));
  ((impl -> trait333) = (& Trait333_S530child1_trait333));
  return impl;
} 
Trait333* Trait333_S530child1_ = newTrait333_S530child1();

int S531_classId = Class_genId();
struct S531{

  int id;
  S531 ():id(S531_classId){

  }
};


int S531child1_classId = Class_genId();
struct S531child1:S531{

  S531child1 (int a):a(a){
    (id = S531child1_classId);
{

    }
  }
  int a;
};


struct Trait531{

  int(*trait531)(Class*);
};

Vec* Trait531_v = newVec();

int Trait531_S531child1_trait531(Class* self_) {
  S531child1* self = ((S531child1*)self_);
{
    return (self -> a);
  }
} 
Trait531* newTrait531_S531child1() {
  Trait531 (* impl) = (new Trait531());
  setVec(Trait531_v, S531child1_classId, ((void*)impl));
  ((impl -> trait531) = (& Trait531_S531child1_trait531));
  return impl;
} 
Trait531* Trait531_S531child1_ = newTrait531_S531child1();

int Trait244_S531child1_trait244(Class* self_) {
  S531child1* self = ((S531child1*)self_);
{
    return (self -> a);
  }
} 
Trait244* newTrait244_S531child1() {
  Trait244 (* impl) = (new Trait244());
  setVec(Trait244_v, S531child1_classId, ((void*)impl));
  ((impl -> trait244) = (& Trait244_S531child1_trait244));
  return impl;
} 
Trait244* Trait244_S531child1_ = newTrait244_S531child1();

int S532_classId = Class_genId();
struct S532{

  int id;
  S532 ():id(S532_classId){

  }
};


int S532child1_classId = Class_genId();
struct S532child1:S532{

  S532child1 (int a):a(a){
    (id = S532child1_classId);
{

    }
  }
  int a;
};


int S532child2_classId = Class_genId();
struct S532child2:S532{

  S532child2 (int a):a(a){
    (id = S532child2_classId);
{

    }
  }
  int a;
};


int S532child3_classId = Class_genId();
struct S532child3:S532{

  S532child3 (int a):a(a){
    (id = S532child3_classId);
{

    }
  }
  int a;
};


int S532child4_classId = Class_genId();
struct S532child4:S532{

  S532child4 (int a):a(a){
    (id = S532child4_classId);
{

    }
  }
  int a;
};


struct Trait532{

  int(*trait532)(Class*);
};

Vec* Trait532_v = newVec();

int Trait532_S532child1_trait532(Class* self_) {
  S532child1* self = ((S532child1*)self_);
{
    return (self -> a);
  }
} 
Trait532* newTrait532_S532child1() {
  Trait532 (* impl) = (new Trait532());
  setVec(Trait532_v, S532child1_classId, ((void*)impl));
  ((impl -> trait532) = (& Trait532_S532child1_trait532));
  return impl;
} 
Trait532* Trait532_S532child1_ = newTrait532_S532child1();

int Trait532_S532child2_trait532(Class* self_) {
  S532child2* self = ((S532child2*)self_);
{
    return (self -> a);
  }
} 
Trait532* newTrait532_S532child2() {
  Trait532 (* impl) = (new Trait532());
  setVec(Trait532_v, S532child2_classId, ((void*)impl));
  ((impl -> trait532) = (& Trait532_S532child2_trait532));
  return impl;
} 
Trait532* Trait532_S532child2_ = newTrait532_S532child2();

int Trait532_S532child3_trait532(Class* self_) {
  S532child3* self = ((S532child3*)self_);
{
    return (self -> a);
  }
} 
Trait532* newTrait532_S532child3() {
  Trait532 (* impl) = (new Trait532());
  setVec(Trait532_v, S532child3_classId, ((void*)impl));
  ((impl -> trait532) = (& Trait532_S532child3_trait532));
  return impl;
} 
Trait532* Trait532_S532child3_ = newTrait532_S532child3();

int Trait206_S532child1_trait206(Class* self_) {
  S532child1* self = ((S532child1*)self_);
{
    return (self -> a);
  }
} 
Trait206* newTrait206_S532child1() {
  Trait206 (* impl) = (new Trait206());
  setVec(Trait206_v, S532child1_classId, ((void*)impl));
  ((impl -> trait206) = (& Trait206_S532child1_trait206));
  return impl;
} 
Trait206* Trait206_S532child1_ = newTrait206_S532child1();

int S533_classId = Class_genId();
struct S533{

  int id;
  S533 ():id(S533_classId){

  }
};


int S533child1_classId = Class_genId();
struct S533child1:S533{

  S533child1 (int a):a(a){
    (id = S533child1_classId);
{

    }
  }
  int a;
};


struct Trait533{

  int(*trait533)(Class*);
};

Vec* Trait533_v = newVec();

int Trait533_S533child1_trait533(Class* self_) {
  S533child1* self = ((S533child1*)self_);
{
    return (self -> a);
  }
} 
Trait533* newTrait533_S533child1() {
  Trait533 (* impl) = (new Trait533());
  setVec(Trait533_v, S533child1_classId, ((void*)impl));
  ((impl -> trait533) = (& Trait533_S533child1_trait533));
  return impl;
} 
Trait533* Trait533_S533child1_ = newTrait533_S533child1();

int Trait419_S533child1_trait419(Class* self_) {
  S533child1* self = ((S533child1*)self_);
{
    return (self -> a);
  }
} 
Trait419* newTrait419_S533child1() {
  Trait419 (* impl) = (new Trait419());
  setVec(Trait419_v, S533child1_classId, ((void*)impl));
  ((impl -> trait419) = (& Trait419_S533child1_trait419));
  return impl;
} 
Trait419* Trait419_S533child1_ = newTrait419_S533child1();

int S534_classId = Class_genId();
struct S534{

  int id;
  S534 ():id(S534_classId){

  }
};


int S534child1_classId = Class_genId();
struct S534child1:S534{

  S534child1 (int a):a(a){
    (id = S534child1_classId);
{

    }
  }
  int a;
};


int S534child2_classId = Class_genId();
struct S534child2:S534{

  S534child2 (int a):a(a){
    (id = S534child2_classId);
{

    }
  }
  int a;
};


int S534child3_classId = Class_genId();
struct S534child3:S534{

  S534child3 (int a):a(a){
    (id = S534child3_classId);
{

    }
  }
  int a;
};


struct Trait534{

  int(*trait534)(Class*);
};

Vec* Trait534_v = newVec();

int Trait534_S534child1_trait534(Class* self_) {
  S534child1* self = ((S534child1*)self_);
{
    return (self -> a);
  }
} 
Trait534* newTrait534_S534child1() {
  Trait534 (* impl) = (new Trait534());
  setVec(Trait534_v, S534child1_classId, ((void*)impl));
  ((impl -> trait534) = (& Trait534_S534child1_trait534));
  return impl;
} 
Trait534* Trait534_S534child1_ = newTrait534_S534child1();

int Trait534_S534child2_trait534(Class* self_) {
  S534child2* self = ((S534child2*)self_);
{
    return (self -> a);
  }
} 
Trait534* newTrait534_S534child2() {
  Trait534 (* impl) = (new Trait534());
  setVec(Trait534_v, S534child2_classId, ((void*)impl));
  ((impl -> trait534) = (& Trait534_S534child2_trait534));
  return impl;
} 
Trait534* Trait534_S534child2_ = newTrait534_S534child2();

int Trait444_S534child1_trait444(Class* self_) {
  S534child1* self = ((S534child1*)self_);
{
    return (self -> a);
  }
} 
Trait444* newTrait444_S534child1() {
  Trait444 (* impl) = (new Trait444());
  setVec(Trait444_v, S534child1_classId, ((void*)impl));
  ((impl -> trait444) = (& Trait444_S534child1_trait444));
  return impl;
} 
Trait444* Trait444_S534child1_ = newTrait444_S534child1();

int S535_classId = Class_genId();
struct S535{

  int id;
  S535 ():id(S535_classId){

  }
};


int S535child1_classId = Class_genId();
struct S535child1:S535{

  S535child1 (int a):a(a){
    (id = S535child1_classId);
{

    }
  }
  int a;
};


int S535child2_classId = Class_genId();
struct S535child2:S535{

  S535child2 (int a):a(a){
    (id = S535child2_classId);
{

    }
  }
  int a;
};


int S535child3_classId = Class_genId();
struct S535child3:S535{

  S535child3 (int a):a(a){
    (id = S535child3_classId);
{

    }
  }
  int a;
};


struct Trait535{

  int(*trait535)(Class*);
};

Vec* Trait535_v = newVec();

int Trait535_S535child1_trait535(Class* self_) {
  S535child1* self = ((S535child1*)self_);
{
    return (self -> a);
  }
} 
Trait535* newTrait535_S535child1() {
  Trait535 (* impl) = (new Trait535());
  setVec(Trait535_v, S535child1_classId, ((void*)impl));
  ((impl -> trait535) = (& Trait535_S535child1_trait535));
  return impl;
} 
Trait535* Trait535_S535child1_ = newTrait535_S535child1();

int Trait535_S535child2_trait535(Class* self_) {
  S535child2* self = ((S535child2*)self_);
{
    return (self -> a);
  }
} 
Trait535* newTrait535_S535child2() {
  Trait535 (* impl) = (new Trait535());
  setVec(Trait535_v, S535child2_classId, ((void*)impl));
  ((impl -> trait535) = (& Trait535_S535child2_trait535));
  return impl;
} 
Trait535* Trait535_S535child2_ = newTrait535_S535child2();

int Trait442_S535child1_trait442(Class* self_) {
  S535child1* self = ((S535child1*)self_);
{
    return (self -> a);
  }
} 
Trait442* newTrait442_S535child1() {
  Trait442 (* impl) = (new Trait442());
  setVec(Trait442_v, S535child1_classId, ((void*)impl));
  ((impl -> trait442) = (& Trait442_S535child1_trait442));
  return impl;
} 
Trait442* Trait442_S535child1_ = newTrait442_S535child1();

int S536_classId = Class_genId();
struct S536{

  int id;
  S536 ():id(S536_classId){

  }
};


int S536child1_classId = Class_genId();
struct S536child1:S536{

  S536child1 (int a):a(a){
    (id = S536child1_classId);
{

    }
  }
  int a;
};


struct Trait536{

  int(*trait536)(Class*);
};

Vec* Trait536_v = newVec();

int Trait536_S536child1_trait536(Class* self_) {
  S536child1* self = ((S536child1*)self_);
{
    return (self -> a);
  }
} 
Trait536* newTrait536_S536child1() {
  Trait536 (* impl) = (new Trait536());
  setVec(Trait536_v, S536child1_classId, ((void*)impl));
  ((impl -> trait536) = (& Trait536_S536child1_trait536));
  return impl;
} 
Trait536* Trait536_S536child1_ = newTrait536_S536child1();

int Trait428_S536child1_trait428(Class* self_) {
  S536child1* self = ((S536child1*)self_);
{
    return (self -> a);
  }
} 
Trait428* newTrait428_S536child1() {
  Trait428 (* impl) = (new Trait428());
  setVec(Trait428_v, S536child1_classId, ((void*)impl));
  ((impl -> trait428) = (& Trait428_S536child1_trait428));
  return impl;
} 
Trait428* Trait428_S536child1_ = newTrait428_S536child1();

int S537_classId = Class_genId();
struct S537{

  int id;
  S537 ():id(S537_classId){

  }
};


int S537child1_classId = Class_genId();
struct S537child1:S537{

  S537child1 (int a):a(a){
    (id = S537child1_classId);
{

    }
  }
  int a;
};


int S537child2_classId = Class_genId();
struct S537child2:S537{

  S537child2 (int a):a(a){
    (id = S537child2_classId);
{

    }
  }
  int a;
};


int S537child3_classId = Class_genId();
struct S537child3:S537{

  S537child3 (int a):a(a){
    (id = S537child3_classId);
{

    }
  }
  int a;
};


struct Trait537{

  int(*trait537)(Class*);
};

Vec* Trait537_v = newVec();

int Trait537_S537child1_trait537(Class* self_) {
  S537child1* self = ((S537child1*)self_);
{
    return (self -> a);
  }
} 
Trait537* newTrait537_S537child1() {
  Trait537 (* impl) = (new Trait537());
  setVec(Trait537_v, S537child1_classId, ((void*)impl));
  ((impl -> trait537) = (& Trait537_S537child1_trait537));
  return impl;
} 
Trait537* Trait537_S537child1_ = newTrait537_S537child1();

int Trait537_S537child2_trait537(Class* self_) {
  S537child2* self = ((S537child2*)self_);
{
    return (self -> a);
  }
} 
Trait537* newTrait537_S537child2() {
  Trait537 (* impl) = (new Trait537());
  setVec(Trait537_v, S537child2_classId, ((void*)impl));
  ((impl -> trait537) = (& Trait537_S537child2_trait537));
  return impl;
} 
Trait537* Trait537_S537child2_ = newTrait537_S537child2();

int Trait354_S537child1_trait354(Class* self_) {
  S537child1* self = ((S537child1*)self_);
{
    return (self -> a);
  }
} 
Trait354* newTrait354_S537child1() {
  Trait354 (* impl) = (new Trait354());
  setVec(Trait354_v, S537child1_classId, ((void*)impl));
  ((impl -> trait354) = (& Trait354_S537child1_trait354));
  return impl;
} 
Trait354* Trait354_S537child1_ = newTrait354_S537child1();

int S538_classId = Class_genId();
struct S538{

  int id;
  S538 ():id(S538_classId){

  }
};


int S538child1_classId = Class_genId();
struct S538child1:S538{

  S538child1 (int a):a(a){
    (id = S538child1_classId);
{

    }
  }
  int a;
};


int S538child2_classId = Class_genId();
struct S538child2:S538{

  S538child2 (int a):a(a){
    (id = S538child2_classId);
{

    }
  }
  int a;
};


int S538child3_classId = Class_genId();
struct S538child3:S538{

  S538child3 (int a):a(a){
    (id = S538child3_classId);
{

    }
  }
  int a;
};


int S538child4_classId = Class_genId();
struct S538child4:S538{

  S538child4 (int a):a(a){
    (id = S538child4_classId);
{

    }
  }
  int a;
};


struct Trait538{

  int(*trait538)(Class*);
};

Vec* Trait538_v = newVec();

int Trait538_S538child1_trait538(Class* self_) {
  S538child1* self = ((S538child1*)self_);
{
    return (self -> a);
  }
} 
Trait538* newTrait538_S538child1() {
  Trait538 (* impl) = (new Trait538());
  setVec(Trait538_v, S538child1_classId, ((void*)impl));
  ((impl -> trait538) = (& Trait538_S538child1_trait538));
  return impl;
} 
Trait538* Trait538_S538child1_ = newTrait538_S538child1();

int Trait538_S538child2_trait538(Class* self_) {
  S538child2* self = ((S538child2*)self_);
{
    return (self -> a);
  }
} 
Trait538* newTrait538_S538child2() {
  Trait538 (* impl) = (new Trait538());
  setVec(Trait538_v, S538child2_classId, ((void*)impl));
  ((impl -> trait538) = (& Trait538_S538child2_trait538));
  return impl;
} 
Trait538* Trait538_S538child2_ = newTrait538_S538child2();

int Trait538_S538child3_trait538(Class* self_) {
  S538child3* self = ((S538child3*)self_);
{
    return (self -> a);
  }
} 
Trait538* newTrait538_S538child3() {
  Trait538 (* impl) = (new Trait538());
  setVec(Trait538_v, S538child3_classId, ((void*)impl));
  ((impl -> trait538) = (& Trait538_S538child3_trait538));
  return impl;
} 
Trait538* Trait538_S538child3_ = newTrait538_S538child3();

int Trait336_S538child1_trait336(Class* self_) {
  S538child1* self = ((S538child1*)self_);
{
    return (self -> a);
  }
} 
Trait336* newTrait336_S538child1() {
  Trait336 (* impl) = (new Trait336());
  setVec(Trait336_v, S538child1_classId, ((void*)impl));
  ((impl -> trait336) = (& Trait336_S538child1_trait336));
  return impl;
} 
Trait336* Trait336_S538child1_ = newTrait336_S538child1();

int S539_classId = Class_genId();
struct S539{

  int id;
  S539 ():id(S539_classId){

  }
};


int S539child1_classId = Class_genId();
struct S539child1:S539{

  S539child1 (int a):a(a){
    (id = S539child1_classId);
{

    }
  }
  int a;
};


int S539child2_classId = Class_genId();
struct S539child2:S539{

  S539child2 (int a):a(a){
    (id = S539child2_classId);
{

    }
  }
  int a;
};


struct Trait539{

  int(*trait539)(Class*);
};

Vec* Trait539_v = newVec();

int Trait539_S539child1_trait539(Class* self_) {
  S539child1* self = ((S539child1*)self_);
{
    return (self -> a);
  }
} 
Trait539* newTrait539_S539child1() {
  Trait539 (* impl) = (new Trait539());
  setVec(Trait539_v, S539child1_classId, ((void*)impl));
  ((impl -> trait539) = (& Trait539_S539child1_trait539));
  return impl;
} 
Trait539* Trait539_S539child1_ = newTrait539_S539child1();

int Trait537_S539child1_trait537(Class* self_) {
  S539child1* self = ((S539child1*)self_);
{
    return (self -> a);
  }
} 
Trait537* newTrait537_S539child1() {
  Trait537 (* impl) = (new Trait537());
  setVec(Trait537_v, S539child1_classId, ((void*)impl));
  ((impl -> trait537) = (& Trait537_S539child1_trait537));
  return impl;
} 
Trait537* Trait537_S539child1_ = newTrait537_S539child1();

int S540_classId = Class_genId();
struct S540{

  int id;
  S540 ():id(S540_classId){

  }
};


int S540child1_classId = Class_genId();
struct S540child1:S540{

  S540child1 (int a):a(a){
    (id = S540child1_classId);
{

    }
  }
  int a;
};


int S540child2_classId = Class_genId();
struct S540child2:S540{

  S540child2 (int a):a(a){
    (id = S540child2_classId);
{

    }
  }
  int a;
};


int S540child3_classId = Class_genId();
struct S540child3:S540{

  S540child3 (int a):a(a){
    (id = S540child3_classId);
{

    }
  }
  int a;
};


int S540child4_classId = Class_genId();
struct S540child4:S540{

  S540child4 (int a):a(a){
    (id = S540child4_classId);
{

    }
  }
  int a;
};


struct Trait540{

  int(*trait540)(Class*);
};

Vec* Trait540_v = newVec();

int Trait540_S540child1_trait540(Class* self_) {
  S540child1* self = ((S540child1*)self_);
{
    return (self -> a);
  }
} 
Trait540* newTrait540_S540child1() {
  Trait540 (* impl) = (new Trait540());
  setVec(Trait540_v, S540child1_classId, ((void*)impl));
  ((impl -> trait540) = (& Trait540_S540child1_trait540));
  return impl;
} 
Trait540* Trait540_S540child1_ = newTrait540_S540child1();

int Trait540_S540child2_trait540(Class* self_) {
  S540child2* self = ((S540child2*)self_);
{
    return (self -> a);
  }
} 
Trait540* newTrait540_S540child2() {
  Trait540 (* impl) = (new Trait540());
  setVec(Trait540_v, S540child2_classId, ((void*)impl));
  ((impl -> trait540) = (& Trait540_S540child2_trait540));
  return impl;
} 
Trait540* Trait540_S540child2_ = newTrait540_S540child2();

int Trait40_S540child1_trait40(Class* self_) {
  S540child1* self = ((S540child1*)self_);
{
    return (self -> a);
  }
} 
Trait40* newTrait40_S540child1() {
  Trait40 (* impl) = (new Trait40());
  setVec(Trait40_v, S540child1_classId, ((void*)impl));
  ((impl -> trait40) = (& Trait40_S540child1_trait40));
  return impl;
} 
Trait40* Trait40_S540child1_ = newTrait40_S540child1();

int S541_classId = Class_genId();
struct S541{

  int id;
  S541 ():id(S541_classId){

  }
};


int S541child1_classId = Class_genId();
struct S541child1:S541{

  S541child1 (int a):a(a){
    (id = S541child1_classId);
{

    }
  }
  int a;
};


int S541child2_classId = Class_genId();
struct S541child2:S541{

  S541child2 (int a):a(a){
    (id = S541child2_classId);
{

    }
  }
  int a;
};


int S541child3_classId = Class_genId();
struct S541child3:S541{

  S541child3 (int a):a(a){
    (id = S541child3_classId);
{

    }
  }
  int a;
};


struct Trait541{

  int(*trait541)(Class*);
};

Vec* Trait541_v = newVec();

int Trait541_S541child1_trait541(Class* self_) {
  S541child1* self = ((S541child1*)self_);
{
    return (self -> a);
  }
} 
Trait541* newTrait541_S541child1() {
  Trait541 (* impl) = (new Trait541());
  setVec(Trait541_v, S541child1_classId, ((void*)impl));
  ((impl -> trait541) = (& Trait541_S541child1_trait541));
  return impl;
} 
Trait541* Trait541_S541child1_ = newTrait541_S541child1();

int Trait541_S541child2_trait541(Class* self_) {
  S541child2* self = ((S541child2*)self_);
{
    return (self -> a);
  }
} 
Trait541* newTrait541_S541child2() {
  Trait541 (* impl) = (new Trait541());
  setVec(Trait541_v, S541child2_classId, ((void*)impl));
  ((impl -> trait541) = (& Trait541_S541child2_trait541));
  return impl;
} 
Trait541* Trait541_S541child2_ = newTrait541_S541child2();

int Trait540_S541child1_trait540(Class* self_) {
  S541child1* self = ((S541child1*)self_);
{
    return (self -> a);
  }
} 
Trait540* newTrait540_S541child1() {
  Trait540 (* impl) = (new Trait540());
  setVec(Trait540_v, S541child1_classId, ((void*)impl));
  ((impl -> trait540) = (& Trait540_S541child1_trait540));
  return impl;
} 
Trait540* Trait540_S541child1_ = newTrait540_S541child1();

int S542_classId = Class_genId();
struct S542{

  int id;
  S542 ():id(S542_classId){

  }
};


int S542child1_classId = Class_genId();
struct S542child1:S542{

  S542child1 (int a):a(a){
    (id = S542child1_classId);
{

    }
  }
  int a;
};


int S542child2_classId = Class_genId();
struct S542child2:S542{

  S542child2 (int a):a(a){
    (id = S542child2_classId);
{

    }
  }
  int a;
};


struct Trait542{

  int(*trait542)(Class*);
};

Vec* Trait542_v = newVec();

int Trait542_S542child1_trait542(Class* self_) {
  S542child1* self = ((S542child1*)self_);
{
    return (self -> a);
  }
} 
Trait542* newTrait542_S542child1() {
  Trait542 (* impl) = (new Trait542());
  setVec(Trait542_v, S542child1_classId, ((void*)impl));
  ((impl -> trait542) = (& Trait542_S542child1_trait542));
  return impl;
} 
Trait542* Trait542_S542child1_ = newTrait542_S542child1();

int Trait397_S542child1_trait397(Class* self_) {
  S542child1* self = ((S542child1*)self_);
{
    return (self -> a);
  }
} 
Trait397* newTrait397_S542child1() {
  Trait397 (* impl) = (new Trait397());
  setVec(Trait397_v, S542child1_classId, ((void*)impl));
  ((impl -> trait397) = (& Trait397_S542child1_trait397));
  return impl;
} 
Trait397* Trait397_S542child1_ = newTrait397_S542child1();

int S543_classId = Class_genId();
struct S543{

  int id;
  S543 ():id(S543_classId){

  }
};


int S543child1_classId = Class_genId();
struct S543child1:S543{

  S543child1 (int a):a(a){
    (id = S543child1_classId);
{

    }
  }
  int a;
};


int S543child2_classId = Class_genId();
struct S543child2:S543{

  S543child2 (int a):a(a){
    (id = S543child2_classId);
{

    }
  }
  int a;
};


struct Trait543{

  int(*trait543)(Class*);
};

Vec* Trait543_v = newVec();

int Trait543_S543child1_trait543(Class* self_) {
  S543child1* self = ((S543child1*)self_);
{
    return (self -> a);
  }
} 
Trait543* newTrait543_S543child1() {
  Trait543 (* impl) = (new Trait543());
  setVec(Trait543_v, S543child1_classId, ((void*)impl));
  ((impl -> trait543) = (& Trait543_S543child1_trait543));
  return impl;
} 
Trait543* Trait543_S543child1_ = newTrait543_S543child1();

int Trait543_S543child2_trait543(Class* self_) {
  S543child2* self = ((S543child2*)self_);
{
    return (self -> a);
  }
} 
Trait543* newTrait543_S543child2() {
  Trait543 (* impl) = (new Trait543());
  setVec(Trait543_v, S543child2_classId, ((void*)impl));
  ((impl -> trait543) = (& Trait543_S543child2_trait543));
  return impl;
} 
Trait543* Trait543_S543child2_ = newTrait543_S543child2();

int Trait13_S543child1_trait13(Class* self_) {
  S543child1* self = ((S543child1*)self_);
{
    return (self -> a);
  }
} 
Trait13* newTrait13_S543child1() {
  Trait13 (* impl) = (new Trait13());
  setVec(Trait13_v, S543child1_classId, ((void*)impl));
  ((impl -> trait13) = (& Trait13_S543child1_trait13));
  return impl;
} 
Trait13* Trait13_S543child1_ = newTrait13_S543child1();

int S544_classId = Class_genId();
struct S544{

  int id;
  S544 ():id(S544_classId){

  }
};


int S544child1_classId = Class_genId();
struct S544child1:S544{

  S544child1 (int a):a(a){
    (id = S544child1_classId);
{

    }
  }
  int a;
};


int S544child2_classId = Class_genId();
struct S544child2:S544{

  S544child2 (int a):a(a){
    (id = S544child2_classId);
{

    }
  }
  int a;
};


int S544child3_classId = Class_genId();
struct S544child3:S544{

  S544child3 (int a):a(a){
    (id = S544child3_classId);
{

    }
  }
  int a;
};


int S544child4_classId = Class_genId();
struct S544child4:S544{

  S544child4 (int a):a(a){
    (id = S544child4_classId);
{

    }
  }
  int a;
};


struct Trait544{

  int(*trait544)(Class*);
};

Vec* Trait544_v = newVec();

int Trait544_S544child1_trait544(Class* self_) {
  S544child1* self = ((S544child1*)self_);
{
    return (self -> a);
  }
} 
Trait544* newTrait544_S544child1() {
  Trait544 (* impl) = (new Trait544());
  setVec(Trait544_v, S544child1_classId, ((void*)impl));
  ((impl -> trait544) = (& Trait544_S544child1_trait544));
  return impl;
} 
Trait544* Trait544_S544child1_ = newTrait544_S544child1();

int Trait544_S544child2_trait544(Class* self_) {
  S544child2* self = ((S544child2*)self_);
{
    return (self -> a);
  }
} 
Trait544* newTrait544_S544child2() {
  Trait544 (* impl) = (new Trait544());
  setVec(Trait544_v, S544child2_classId, ((void*)impl));
  ((impl -> trait544) = (& Trait544_S544child2_trait544));
  return impl;
} 
Trait544* Trait544_S544child2_ = newTrait544_S544child2();

int Trait287_S544child1_trait287(Class* self_) {
  S544child1* self = ((S544child1*)self_);
{
    return (self -> a);
  }
} 
Trait287* newTrait287_S544child1() {
  Trait287 (* impl) = (new Trait287());
  setVec(Trait287_v, S544child1_classId, ((void*)impl));
  ((impl -> trait287) = (& Trait287_S544child1_trait287));
  return impl;
} 
Trait287* Trait287_S544child1_ = newTrait287_S544child1();

int S545_classId = Class_genId();
struct S545{

  int id;
  S545 ():id(S545_classId){

  }
};


int S545child1_classId = Class_genId();
struct S545child1:S545{

  S545child1 (int a):a(a){
    (id = S545child1_classId);
{

    }
  }
  int a;
};


int S545child2_classId = Class_genId();
struct S545child2:S545{

  S545child2 (int a):a(a){
    (id = S545child2_classId);
{

    }
  }
  int a;
};


int S545child3_classId = Class_genId();
struct S545child3:S545{

  S545child3 (int a):a(a){
    (id = S545child3_classId);
{

    }
  }
  int a;
};


struct Trait545{

  int(*trait545)(Class*);
};

Vec* Trait545_v = newVec();

int Trait545_S545child1_trait545(Class* self_) {
  S545child1* self = ((S545child1*)self_);
{
    return (self -> a);
  }
} 
Trait545* newTrait545_S545child1() {
  Trait545 (* impl) = (new Trait545());
  setVec(Trait545_v, S545child1_classId, ((void*)impl));
  ((impl -> trait545) = (& Trait545_S545child1_trait545));
  return impl;
} 
Trait545* Trait545_S545child1_ = newTrait545_S545child1();

int Trait525_S545child1_trait525(Class* self_) {
  S545child1* self = ((S545child1*)self_);
{
    return (self -> a);
  }
} 
Trait525* newTrait525_S545child1() {
  Trait525 (* impl) = (new Trait525());
  setVec(Trait525_v, S545child1_classId, ((void*)impl));
  ((impl -> trait525) = (& Trait525_S545child1_trait525));
  return impl;
} 
Trait525* Trait525_S545child1_ = newTrait525_S545child1();

int S546_classId = Class_genId();
struct S546{

  int id;
  S546 ():id(S546_classId){

  }
};


int S546child1_classId = Class_genId();
struct S546child1:S546{

  S546child1 (int a):a(a){
    (id = S546child1_classId);
{

    }
  }
  int a;
};


struct Trait546{

  int(*trait546)(Class*);
};

Vec* Trait546_v = newVec();

int Trait546_S546child1_trait546(Class* self_) {
  S546child1* self = ((S546child1*)self_);
{
    return (self -> a);
  }
} 
Trait546* newTrait546_S546child1() {
  Trait546 (* impl) = (new Trait546());
  setVec(Trait546_v, S546child1_classId, ((void*)impl));
  ((impl -> trait546) = (& Trait546_S546child1_trait546));
  return impl;
} 
Trait546* Trait546_S546child1_ = newTrait546_S546child1();

int Trait111_S546child1_trait111(Class* self_) {
  S546child1* self = ((S546child1*)self_);
{
    return (self -> a);
  }
} 
Trait111* newTrait111_S546child1() {
  Trait111 (* impl) = (new Trait111());
  setVec(Trait111_v, S546child1_classId, ((void*)impl));
  ((impl -> trait111) = (& Trait111_S546child1_trait111));
  return impl;
} 
Trait111* Trait111_S546child1_ = newTrait111_S546child1();

int S547_classId = Class_genId();
struct S547{

  int id;
  S547 ():id(S547_classId){

  }
};


int S547child1_classId = Class_genId();
struct S547child1:S547{

  S547child1 (int a):a(a){
    (id = S547child1_classId);
{

    }
  }
  int a;
};


int S547child2_classId = Class_genId();
struct S547child2:S547{

  S547child2 (int a):a(a){
    (id = S547child2_classId);
{

    }
  }
  int a;
};


int S547child3_classId = Class_genId();
struct S547child3:S547{

  S547child3 (int a):a(a){
    (id = S547child3_classId);
{

    }
  }
  int a;
};


struct Trait547{

  int(*trait547)(Class*);
};

Vec* Trait547_v = newVec();

int Trait547_S547child1_trait547(Class* self_) {
  S547child1* self = ((S547child1*)self_);
{
    return (self -> a);
  }
} 
Trait547* newTrait547_S547child1() {
  Trait547 (* impl) = (new Trait547());
  setVec(Trait547_v, S547child1_classId, ((void*)impl));
  ((impl -> trait547) = (& Trait547_S547child1_trait547));
  return impl;
} 
Trait547* Trait547_S547child1_ = newTrait547_S547child1();

int Trait547_S547child2_trait547(Class* self_) {
  S547child2* self = ((S547child2*)self_);
{
    return (self -> a);
  }
} 
Trait547* newTrait547_S547child2() {
  Trait547 (* impl) = (new Trait547());
  setVec(Trait547_v, S547child2_classId, ((void*)impl));
  ((impl -> trait547) = (& Trait547_S547child2_trait547));
  return impl;
} 
Trait547* Trait547_S547child2_ = newTrait547_S547child2();

int Trait432_S547child1_trait432(Class* self_) {
  S547child1* self = ((S547child1*)self_);
{
    return (self -> a);
  }
} 
Trait432* newTrait432_S547child1() {
  Trait432 (* impl) = (new Trait432());
  setVec(Trait432_v, S547child1_classId, ((void*)impl));
  ((impl -> trait432) = (& Trait432_S547child1_trait432));
  return impl;
} 
Trait432* Trait432_S547child1_ = newTrait432_S547child1();

int S548_classId = Class_genId();
struct S548{

  int id;
  S548 ():id(S548_classId){

  }
};


int S548child1_classId = Class_genId();
struct S548child1:S548{

  S548child1 (int a):a(a){
    (id = S548child1_classId);
{

    }
  }
  int a;
};


int S548child2_classId = Class_genId();
struct S548child2:S548{

  S548child2 (int a):a(a){
    (id = S548child2_classId);
{

    }
  }
  int a;
};


int S548child3_classId = Class_genId();
struct S548child3:S548{

  S548child3 (int a):a(a){
    (id = S548child3_classId);
{

    }
  }
  int a;
};


struct Trait548{

  int(*trait548)(Class*);
};

Vec* Trait548_v = newVec();

int Trait548_S548child1_trait548(Class* self_) {
  S548child1* self = ((S548child1*)self_);
{
    return (self -> a);
  }
} 
Trait548* newTrait548_S548child1() {
  Trait548 (* impl) = (new Trait548());
  setVec(Trait548_v, S548child1_classId, ((void*)impl));
  ((impl -> trait548) = (& Trait548_S548child1_trait548));
  return impl;
} 
Trait548* Trait548_S548child1_ = newTrait548_S548child1();

int Trait548_S548child2_trait548(Class* self_) {
  S548child2* self = ((S548child2*)self_);
{
    return (self -> a);
  }
} 
Trait548* newTrait548_S548child2() {
  Trait548 (* impl) = (new Trait548());
  setVec(Trait548_v, S548child2_classId, ((void*)impl));
  ((impl -> trait548) = (& Trait548_S548child2_trait548));
  return impl;
} 
Trait548* Trait548_S548child2_ = newTrait548_S548child2();

int Trait206_S548child1_trait206(Class* self_) {
  S548child1* self = ((S548child1*)self_);
{
    return (self -> a);
  }
} 
Trait206* newTrait206_S548child1() {
  Trait206 (* impl) = (new Trait206());
  setVec(Trait206_v, S548child1_classId, ((void*)impl));
  ((impl -> trait206) = (& Trait206_S548child1_trait206));
  return impl;
} 
Trait206* Trait206_S548child1_ = newTrait206_S548child1();

int S549_classId = Class_genId();
struct S549{

  int id;
  S549 ():id(S549_classId){

  }
};


int S549child1_classId = Class_genId();
struct S549child1:S549{

  S549child1 (int a):a(a){
    (id = S549child1_classId);
{

    }
  }
  int a;
};


int S549child2_classId = Class_genId();
struct S549child2:S549{

  S549child2 (int a):a(a){
    (id = S549child2_classId);
{

    }
  }
  int a;
};


int S549child3_classId = Class_genId();
struct S549child3:S549{

  S549child3 (int a):a(a){
    (id = S549child3_classId);
{

    }
  }
  int a;
};


int S549child4_classId = Class_genId();
struct S549child4:S549{

  S549child4 (int a):a(a){
    (id = S549child4_classId);
{

    }
  }
  int a;
};


struct Trait549{

  int(*trait549)(Class*);
};

Vec* Trait549_v = newVec();

int Trait549_S549child1_trait549(Class* self_) {
  S549child1* self = ((S549child1*)self_);
{
    return (self -> a);
  }
} 
Trait549* newTrait549_S549child1() {
  Trait549 (* impl) = (new Trait549());
  setVec(Trait549_v, S549child1_classId, ((void*)impl));
  ((impl -> trait549) = (& Trait549_S549child1_trait549));
  return impl;
} 
Trait549* Trait549_S549child1_ = newTrait549_S549child1();

int Trait448_S549child1_trait448(Class* self_) {
  S549child1* self = ((S549child1*)self_);
{
    return (self -> a);
  }
} 
Trait448* newTrait448_S549child1() {
  Trait448 (* impl) = (new Trait448());
  setVec(Trait448_v, S549child1_classId, ((void*)impl));
  ((impl -> trait448) = (& Trait448_S549child1_trait448));
  return impl;
} 
Trait448* Trait448_S549child1_ = newTrait448_S549child1();

int S550_classId = Class_genId();
struct S550{

  int id;
  S550 ():id(S550_classId){

  }
};


int S550child1_classId = Class_genId();
struct S550child1:S550{

  S550child1 (int a):a(a){
    (id = S550child1_classId);
{

    }
  }
  int a;
};


struct Trait550{

  int(*trait550)(Class*);
};

Vec* Trait550_v = newVec();

int Trait550_S550child1_trait550(Class* self_) {
  S550child1* self = ((S550child1*)self_);
{
    return (self -> a);
  }
} 
Trait550* newTrait550_S550child1() {
  Trait550 (* impl) = (new Trait550());
  setVec(Trait550_v, S550child1_classId, ((void*)impl));
  ((impl -> trait550) = (& Trait550_S550child1_trait550));
  return impl;
} 
Trait550* Trait550_S550child1_ = newTrait550_S550child1();

int Trait338_S550child1_trait338(Class* self_) {
  S550child1* self = ((S550child1*)self_);
{
    return (self -> a);
  }
} 
Trait338* newTrait338_S550child1() {
  Trait338 (* impl) = (new Trait338());
  setVec(Trait338_v, S550child1_classId, ((void*)impl));
  ((impl -> trait338) = (& Trait338_S550child1_trait338));
  return impl;
} 
Trait338* Trait338_S550child1_ = newTrait338_S550child1();

int S551_classId = Class_genId();
struct S551{

  int id;
  S551 ():id(S551_classId){

  }
};


int S551child1_classId = Class_genId();
struct S551child1:S551{

  S551child1 (int a):a(a){
    (id = S551child1_classId);
{

    }
  }
  int a;
};


int S551child2_classId = Class_genId();
struct S551child2:S551{

  S551child2 (int a):a(a){
    (id = S551child2_classId);
{

    }
  }
  int a;
};


int S551child3_classId = Class_genId();
struct S551child3:S551{

  S551child3 (int a):a(a){
    (id = S551child3_classId);
{

    }
  }
  int a;
};


struct Trait551{

  int(*trait551)(Class*);
};

Vec* Trait551_v = newVec();

int Trait551_S551child1_trait551(Class* self_) {
  S551child1* self = ((S551child1*)self_);
{
    return (self -> a);
  }
} 
Trait551* newTrait551_S551child1() {
  Trait551 (* impl) = (new Trait551());
  setVec(Trait551_v, S551child1_classId, ((void*)impl));
  ((impl -> trait551) = (& Trait551_S551child1_trait551));
  return impl;
} 
Trait551* Trait551_S551child1_ = newTrait551_S551child1();

int Trait142_S551child1_trait142(Class* self_) {
  S551child1* self = ((S551child1*)self_);
{
    return (self -> a);
  }
} 
Trait142* newTrait142_S551child1() {
  Trait142 (* impl) = (new Trait142());
  setVec(Trait142_v, S551child1_classId, ((void*)impl));
  ((impl -> trait142) = (& Trait142_S551child1_trait142));
  return impl;
} 
Trait142* Trait142_S551child1_ = newTrait142_S551child1();

int S552_classId = Class_genId();
struct S552{

  int id;
  S552 ():id(S552_classId){

  }
};


int S552child1_classId = Class_genId();
struct S552child1:S552{

  S552child1 (int a):a(a){
    (id = S552child1_classId);
{

    }
  }
  int a;
};


int S552child2_classId = Class_genId();
struct S552child2:S552{

  S552child2 (int a):a(a){
    (id = S552child2_classId);
{

    }
  }
  int a;
};


struct Trait552{

  int(*trait552)(Class*);
};

Vec* Trait552_v = newVec();

int Trait552_S552child1_trait552(Class* self_) {
  S552child1* self = ((S552child1*)self_);
{
    return (self -> a);
  }
} 
Trait552* newTrait552_S552child1() {
  Trait552 (* impl) = (new Trait552());
  setVec(Trait552_v, S552child1_classId, ((void*)impl));
  ((impl -> trait552) = (& Trait552_S552child1_trait552));
  return impl;
} 
Trait552* Trait552_S552child1_ = newTrait552_S552child1();

int Trait449_S552child1_trait449(Class* self_) {
  S552child1* self = ((S552child1*)self_);
{
    return (self -> a);
  }
} 
Trait449* newTrait449_S552child1() {
  Trait449 (* impl) = (new Trait449());
  setVec(Trait449_v, S552child1_classId, ((void*)impl));
  ((impl -> trait449) = (& Trait449_S552child1_trait449));
  return impl;
} 
Trait449* Trait449_S552child1_ = newTrait449_S552child1();

int S553_classId = Class_genId();
struct S553{

  int id;
  S553 ():id(S553_classId){

  }
};


int S553child1_classId = Class_genId();
struct S553child1:S553{

  S553child1 (int a):a(a){
    (id = S553child1_classId);
{

    }
  }
  int a;
};


int S553child2_classId = Class_genId();
struct S553child2:S553{

  S553child2 (int a):a(a){
    (id = S553child2_classId);
{

    }
  }
  int a;
};


int S553child3_classId = Class_genId();
struct S553child3:S553{

  S553child3 (int a):a(a){
    (id = S553child3_classId);
{

    }
  }
  int a;
};


struct Trait553{

  int(*trait553)(Class*);
};

Vec* Trait553_v = newVec();

int Trait553_S553child1_trait553(Class* self_) {
  S553child1* self = ((S553child1*)self_);
{
    return (self -> a);
  }
} 
Trait553* newTrait553_S553child1() {
  Trait553 (* impl) = (new Trait553());
  setVec(Trait553_v, S553child1_classId, ((void*)impl));
  ((impl -> trait553) = (& Trait553_S553child1_trait553));
  return impl;
} 
Trait553* Trait553_S553child1_ = newTrait553_S553child1();

int Trait553_S553child2_trait553(Class* self_) {
  S553child2* self = ((S553child2*)self_);
{
    return (self -> a);
  }
} 
Trait553* newTrait553_S553child2() {
  Trait553 (* impl) = (new Trait553());
  setVec(Trait553_v, S553child2_classId, ((void*)impl));
  ((impl -> trait553) = (& Trait553_S553child2_trait553));
  return impl;
} 
Trait553* Trait553_S553child2_ = newTrait553_S553child2();

int Trait501_S553child1_trait501(Class* self_) {
  S553child1* self = ((S553child1*)self_);
{
    return (self -> a);
  }
} 
Trait501* newTrait501_S553child1() {
  Trait501 (* impl) = (new Trait501());
  setVec(Trait501_v, S553child1_classId, ((void*)impl));
  ((impl -> trait501) = (& Trait501_S553child1_trait501));
  return impl;
} 
Trait501* Trait501_S553child1_ = newTrait501_S553child1();

int S554_classId = Class_genId();
struct S554{

  int id;
  S554 ():id(S554_classId){

  }
};


int S554child1_classId = Class_genId();
struct S554child1:S554{

  S554child1 (int a):a(a){
    (id = S554child1_classId);
{

    }
  }
  int a;
};


int S554child2_classId = Class_genId();
struct S554child2:S554{

  S554child2 (int a):a(a){
    (id = S554child2_classId);
{

    }
  }
  int a;
};


struct Trait554{

  int(*trait554)(Class*);
};

Vec* Trait554_v = newVec();

int Trait554_S554child1_trait554(Class* self_) {
  S554child1* self = ((S554child1*)self_);
{
    return (self -> a);
  }
} 
Trait554* newTrait554_S554child1() {
  Trait554 (* impl) = (new Trait554());
  setVec(Trait554_v, S554child1_classId, ((void*)impl));
  ((impl -> trait554) = (& Trait554_S554child1_trait554));
  return impl;
} 
Trait554* Trait554_S554child1_ = newTrait554_S554child1();

int Trait335_S554child1_trait335(Class* self_) {
  S554child1* self = ((S554child1*)self_);
{
    return (self -> a);
  }
} 
Trait335* newTrait335_S554child1() {
  Trait335 (* impl) = (new Trait335());
  setVec(Trait335_v, S554child1_classId, ((void*)impl));
  ((impl -> trait335) = (& Trait335_S554child1_trait335));
  return impl;
} 
Trait335* Trait335_S554child1_ = newTrait335_S554child1();

int S555_classId = Class_genId();
struct S555{

  int id;
  S555 ():id(S555_classId){

  }
};


int S555child1_classId = Class_genId();
struct S555child1:S555{

  S555child1 (int a):a(a){
    (id = S555child1_classId);
{

    }
  }
  int a;
};


struct Trait555{

  int(*trait555)(Class*);
};

Vec* Trait555_v = newVec();

int Trait555_S555child1_trait555(Class* self_) {
  S555child1* self = ((S555child1*)self_);
{
    return (self -> a);
  }
} 
Trait555* newTrait555_S555child1() {
  Trait555 (* impl) = (new Trait555());
  setVec(Trait555_v, S555child1_classId, ((void*)impl));
  ((impl -> trait555) = (& Trait555_S555child1_trait555));
  return impl;
} 
Trait555* Trait555_S555child1_ = newTrait555_S555child1();

int Trait352_S555child1_trait352(Class* self_) {
  S555child1* self = ((S555child1*)self_);
{
    return (self -> a);
  }
} 
Trait352* newTrait352_S555child1() {
  Trait352 (* impl) = (new Trait352());
  setVec(Trait352_v, S555child1_classId, ((void*)impl));
  ((impl -> trait352) = (& Trait352_S555child1_trait352));
  return impl;
} 
Trait352* Trait352_S555child1_ = newTrait352_S555child1();

int S556_classId = Class_genId();
struct S556{

  int id;
  S556 ():id(S556_classId){

  }
};


int S556child1_classId = Class_genId();
struct S556child1:S556{

  S556child1 (int a):a(a){
    (id = S556child1_classId);
{

    }
  }
  int a;
};


struct Trait556{

  int(*trait556)(Class*);
};

Vec* Trait556_v = newVec();

int Trait556_S556child1_trait556(Class* self_) {
  S556child1* self = ((S556child1*)self_);
{
    return (self -> a);
  }
} 
Trait556* newTrait556_S556child1() {
  Trait556 (* impl) = (new Trait556());
  setVec(Trait556_v, S556child1_classId, ((void*)impl));
  ((impl -> trait556) = (& Trait556_S556child1_trait556));
  return impl;
} 
Trait556* Trait556_S556child1_ = newTrait556_S556child1();

int Trait29_S556child1_trait29(Class* self_) {
  S556child1* self = ((S556child1*)self_);
{
    return (self -> a);
  }
} 
Trait29* newTrait29_S556child1() {
  Trait29 (* impl) = (new Trait29());
  setVec(Trait29_v, S556child1_classId, ((void*)impl));
  ((impl -> trait29) = (& Trait29_S556child1_trait29));
  return impl;
} 
Trait29* Trait29_S556child1_ = newTrait29_S556child1();

int S557_classId = Class_genId();
struct S557{

  int id;
  S557 ():id(S557_classId){

  }
};


int S557child1_classId = Class_genId();
struct S557child1:S557{

  S557child1 (int a):a(a){
    (id = S557child1_classId);
{

    }
  }
  int a;
};


int S557child2_classId = Class_genId();
struct S557child2:S557{

  S557child2 (int a):a(a){
    (id = S557child2_classId);
{

    }
  }
  int a;
};


struct Trait557{

  int(*trait557)(Class*);
};

Vec* Trait557_v = newVec();

int Trait557_S557child1_trait557(Class* self_) {
  S557child1* self = ((S557child1*)self_);
{
    return (self -> a);
  }
} 
Trait557* newTrait557_S557child1() {
  Trait557 (* impl) = (new Trait557());
  setVec(Trait557_v, S557child1_classId, ((void*)impl));
  ((impl -> trait557) = (& Trait557_S557child1_trait557));
  return impl;
} 
Trait557* Trait557_S557child1_ = newTrait557_S557child1();

int Trait408_S557child1_trait408(Class* self_) {
  S557child1* self = ((S557child1*)self_);
{
    return (self -> a);
  }
} 
Trait408* newTrait408_S557child1() {
  Trait408 (* impl) = (new Trait408());
  setVec(Trait408_v, S557child1_classId, ((void*)impl));
  ((impl -> trait408) = (& Trait408_S557child1_trait408));
  return impl;
} 
Trait408* Trait408_S557child1_ = newTrait408_S557child1();

int S558_classId = Class_genId();
struct S558{

  int id;
  S558 ():id(S558_classId){

  }
};


int S558child1_classId = Class_genId();
struct S558child1:S558{

  S558child1 (int a):a(a){
    (id = S558child1_classId);
{

    }
  }
  int a;
};


int S558child2_classId = Class_genId();
struct S558child2:S558{

  S558child2 (int a):a(a){
    (id = S558child2_classId);
{

    }
  }
  int a;
};


struct Trait558{

  int(*trait558)(Class*);
};

Vec* Trait558_v = newVec();

int Trait558_S558child1_trait558(Class* self_) {
  S558child1* self = ((S558child1*)self_);
{
    return (self -> a);
  }
} 
Trait558* newTrait558_S558child1() {
  Trait558 (* impl) = (new Trait558());
  setVec(Trait558_v, S558child1_classId, ((void*)impl));
  ((impl -> trait558) = (& Trait558_S558child1_trait558));
  return impl;
} 
Trait558* Trait558_S558child1_ = newTrait558_S558child1();

int Trait205_S558child1_trait205(Class* self_) {
  S558child1* self = ((S558child1*)self_);
{
    return (self -> a);
  }
} 
Trait205* newTrait205_S558child1() {
  Trait205 (* impl) = (new Trait205());
  setVec(Trait205_v, S558child1_classId, ((void*)impl));
  ((impl -> trait205) = (& Trait205_S558child1_trait205));
  return impl;
} 
Trait205* Trait205_S558child1_ = newTrait205_S558child1();

int S559_classId = Class_genId();
struct S559{

  int id;
  S559 ():id(S559_classId){

  }
};


int S559child1_classId = Class_genId();
struct S559child1:S559{

  S559child1 (int a):a(a){
    (id = S559child1_classId);
{

    }
  }
  int a;
};


struct Trait559{

  int(*trait559)(Class*);
};

Vec* Trait559_v = newVec();

int Trait559_S559child1_trait559(Class* self_) {
  S559child1* self = ((S559child1*)self_);
{
    return (self -> a);
  }
} 
Trait559* newTrait559_S559child1() {
  Trait559 (* impl) = (new Trait559());
  setVec(Trait559_v, S559child1_classId, ((void*)impl));
  ((impl -> trait559) = (& Trait559_S559child1_trait559));
  return impl;
} 
Trait559* Trait559_S559child1_ = newTrait559_S559child1();

int Trait261_S559child1_trait261(Class* self_) {
  S559child1* self = ((S559child1*)self_);
{
    return (self -> a);
  }
} 
Trait261* newTrait261_S559child1() {
  Trait261 (* impl) = (new Trait261());
  setVec(Trait261_v, S559child1_classId, ((void*)impl));
  ((impl -> trait261) = (& Trait261_S559child1_trait261));
  return impl;
} 
Trait261* Trait261_S559child1_ = newTrait261_S559child1();

int S560_classId = Class_genId();
struct S560{

  int id;
  S560 ():id(S560_classId){

  }
};


int S560child1_classId = Class_genId();
struct S560child1:S560{

  S560child1 (int a):a(a){
    (id = S560child1_classId);
{

    }
  }
  int a;
};


int S560child2_classId = Class_genId();
struct S560child2:S560{

  S560child2 (int a):a(a){
    (id = S560child2_classId);
{

    }
  }
  int a;
};


int S560child3_classId = Class_genId();
struct S560child3:S560{

  S560child3 (int a):a(a){
    (id = S560child3_classId);
{

    }
  }
  int a;
};


struct Trait560{

  int(*trait560)(Class*);
};

Vec* Trait560_v = newVec();

int Trait560_S560child1_trait560(Class* self_) {
  S560child1* self = ((S560child1*)self_);
{
    return (self -> a);
  }
} 
Trait560* newTrait560_S560child1() {
  Trait560 (* impl) = (new Trait560());
  setVec(Trait560_v, S560child1_classId, ((void*)impl));
  ((impl -> trait560) = (& Trait560_S560child1_trait560));
  return impl;
} 
Trait560* Trait560_S560child1_ = newTrait560_S560child1();

int Trait560_S560child2_trait560(Class* self_) {
  S560child2* self = ((S560child2*)self_);
{
    return (self -> a);
  }
} 
Trait560* newTrait560_S560child2() {
  Trait560 (* impl) = (new Trait560());
  setVec(Trait560_v, S560child2_classId, ((void*)impl));
  ((impl -> trait560) = (& Trait560_S560child2_trait560));
  return impl;
} 
Trait560* Trait560_S560child2_ = newTrait560_S560child2();

int Trait493_S560child1_trait493(Class* self_) {
  S560child1* self = ((S560child1*)self_);
{
    return (self -> a);
  }
} 
Trait493* newTrait493_S560child1() {
  Trait493 (* impl) = (new Trait493());
  setVec(Trait493_v, S560child1_classId, ((void*)impl));
  ((impl -> trait493) = (& Trait493_S560child1_trait493));
  return impl;
} 
Trait493* Trait493_S560child1_ = newTrait493_S560child1();

int S561_classId = Class_genId();
struct S561{

  int id;
  S561 ():id(S561_classId){

  }
};


int S561child1_classId = Class_genId();
struct S561child1:S561{

  S561child1 (int a):a(a){
    (id = S561child1_classId);
{

    }
  }
  int a;
};


int S561child2_classId = Class_genId();
struct S561child2:S561{

  S561child2 (int a):a(a){
    (id = S561child2_classId);
{

    }
  }
  int a;
};


int S561child3_classId = Class_genId();
struct S561child3:S561{

  S561child3 (int a):a(a){
    (id = S561child3_classId);
{

    }
  }
  int a;
};


struct Trait561{

  int(*trait561)(Class*);
};

Vec* Trait561_v = newVec();

int Trait561_S561child1_trait561(Class* self_) {
  S561child1* self = ((S561child1*)self_);
{
    return (self -> a);
  }
} 
Trait561* newTrait561_S561child1() {
  Trait561 (* impl) = (new Trait561());
  setVec(Trait561_v, S561child1_classId, ((void*)impl));
  ((impl -> trait561) = (& Trait561_S561child1_trait561));
  return impl;
} 
Trait561* Trait561_S561child1_ = newTrait561_S561child1();

int Trait561_S561child2_trait561(Class* self_) {
  S561child2* self = ((S561child2*)self_);
{
    return (self -> a);
  }
} 
Trait561* newTrait561_S561child2() {
  Trait561 (* impl) = (new Trait561());
  setVec(Trait561_v, S561child2_classId, ((void*)impl));
  ((impl -> trait561) = (& Trait561_S561child2_trait561));
  return impl;
} 
Trait561* Trait561_S561child2_ = newTrait561_S561child2();

int Trait561_S561child3_trait561(Class* self_) {
  S561child3* self = ((S561child3*)self_);
{
    return (self -> a);
  }
} 
Trait561* newTrait561_S561child3() {
  Trait561 (* impl) = (new Trait561());
  setVec(Trait561_v, S561child3_classId, ((void*)impl));
  ((impl -> trait561) = (& Trait561_S561child3_trait561));
  return impl;
} 
Trait561* Trait561_S561child3_ = newTrait561_S561child3();

int Trait125_S561child1_trait125(Class* self_) {
  S561child1* self = ((S561child1*)self_);
{
    return (self -> a);
  }
} 
Trait125* newTrait125_S561child1() {
  Trait125 (* impl) = (new Trait125());
  setVec(Trait125_v, S561child1_classId, ((void*)impl));
  ((impl -> trait125) = (& Trait125_S561child1_trait125));
  return impl;
} 
Trait125* Trait125_S561child1_ = newTrait125_S561child1();

int S562_classId = Class_genId();
struct S562{

  int id;
  S562 ():id(S562_classId){

  }
};


int S562child1_classId = Class_genId();
struct S562child1:S562{

  S562child1 (int a):a(a){
    (id = S562child1_classId);
{

    }
  }
  int a;
};


int S562child2_classId = Class_genId();
struct S562child2:S562{

  S562child2 (int a):a(a){
    (id = S562child2_classId);
{

    }
  }
  int a;
};


int S562child3_classId = Class_genId();
struct S562child3:S562{

  S562child3 (int a):a(a){
    (id = S562child3_classId);
{

    }
  }
  int a;
};


struct Trait562{

  int(*trait562)(Class*);
};

Vec* Trait562_v = newVec();

int Trait562_S562child1_trait562(Class* self_) {
  S562child1* self = ((S562child1*)self_);
{
    return (self -> a);
  }
} 
Trait562* newTrait562_S562child1() {
  Trait562 (* impl) = (new Trait562());
  setVec(Trait562_v, S562child1_classId, ((void*)impl));
  ((impl -> trait562) = (& Trait562_S562child1_trait562));
  return impl;
} 
Trait562* Trait562_S562child1_ = newTrait562_S562child1();

int Trait541_S562child1_trait541(Class* self_) {
  S562child1* self = ((S562child1*)self_);
{
    return (self -> a);
  }
} 
Trait541* newTrait541_S562child1() {
  Trait541 (* impl) = (new Trait541());
  setVec(Trait541_v, S562child1_classId, ((void*)impl));
  ((impl -> trait541) = (& Trait541_S562child1_trait541));
  return impl;
} 
Trait541* Trait541_S562child1_ = newTrait541_S562child1();

int S563_classId = Class_genId();
struct S563{

  int id;
  S563 ():id(S563_classId){

  }
};


int S563child1_classId = Class_genId();
struct S563child1:S563{

  S563child1 (int a):a(a){
    (id = S563child1_classId);
{

    }
  }
  int a;
};


int S563child2_classId = Class_genId();
struct S563child2:S563{

  S563child2 (int a):a(a){
    (id = S563child2_classId);
{

    }
  }
  int a;
};


struct Trait563{

  int(*trait563)(Class*);
};

Vec* Trait563_v = newVec();

int Trait563_S563child1_trait563(Class* self_) {
  S563child1* self = ((S563child1*)self_);
{
    return (self -> a);
  }
} 
Trait563* newTrait563_S563child1() {
  Trait563 (* impl) = (new Trait563());
  setVec(Trait563_v, S563child1_classId, ((void*)impl));
  ((impl -> trait563) = (& Trait563_S563child1_trait563));
  return impl;
} 
Trait563* Trait563_S563child1_ = newTrait563_S563child1();

int Trait563_S563child2_trait563(Class* self_) {
  S563child2* self = ((S563child2*)self_);
{
    return (self -> a);
  }
} 
Trait563* newTrait563_S563child2() {
  Trait563 (* impl) = (new Trait563());
  setVec(Trait563_v, S563child2_classId, ((void*)impl));
  ((impl -> trait563) = (& Trait563_S563child2_trait563));
  return impl;
} 
Trait563* Trait563_S563child2_ = newTrait563_S563child2();

int Trait395_S563child1_trait395(Class* self_) {
  S563child1* self = ((S563child1*)self_);
{
    return (self -> a);
  }
} 
Trait395* newTrait395_S563child1() {
  Trait395 (* impl) = (new Trait395());
  setVec(Trait395_v, S563child1_classId, ((void*)impl));
  ((impl -> trait395) = (& Trait395_S563child1_trait395));
  return impl;
} 
Trait395* Trait395_S563child1_ = newTrait395_S563child1();

int S564_classId = Class_genId();
struct S564{

  int id;
  S564 ():id(S564_classId){

  }
};


int S564child1_classId = Class_genId();
struct S564child1:S564{

  S564child1 (int a):a(a){
    (id = S564child1_classId);
{

    }
  }
  int a;
};


int S564child2_classId = Class_genId();
struct S564child2:S564{

  S564child2 (int a):a(a){
    (id = S564child2_classId);
{

    }
  }
  int a;
};


struct Trait564{

  int(*trait564)(Class*);
};

Vec* Trait564_v = newVec();

int Trait564_S564child1_trait564(Class* self_) {
  S564child1* self = ((S564child1*)self_);
{
    return (self -> a);
  }
} 
Trait564* newTrait564_S564child1() {
  Trait564 (* impl) = (new Trait564());
  setVec(Trait564_v, S564child1_classId, ((void*)impl));
  ((impl -> trait564) = (& Trait564_S564child1_trait564));
  return impl;
} 
Trait564* Trait564_S564child1_ = newTrait564_S564child1();

int Trait554_S564child1_trait554(Class* self_) {
  S564child1* self = ((S564child1*)self_);
{
    return (self -> a);
  }
} 
Trait554* newTrait554_S564child1() {
  Trait554 (* impl) = (new Trait554());
  setVec(Trait554_v, S564child1_classId, ((void*)impl));
  ((impl -> trait554) = (& Trait554_S564child1_trait554));
  return impl;
} 
Trait554* Trait554_S564child1_ = newTrait554_S564child1();

int S565_classId = Class_genId();
struct S565{

  int id;
  S565 ():id(S565_classId){

  }
};


int S565child1_classId = Class_genId();
struct S565child1:S565{

  S565child1 (int a):a(a){
    (id = S565child1_classId);
{

    }
  }
  int a;
};


struct Trait565{

  int(*trait565)(Class*);
};

Vec* Trait565_v = newVec();

int Trait565_S565child1_trait565(Class* self_) {
  S565child1* self = ((S565child1*)self_);
{
    return (self -> a);
  }
} 
Trait565* newTrait565_S565child1() {
  Trait565 (* impl) = (new Trait565());
  setVec(Trait565_v, S565child1_classId, ((void*)impl));
  ((impl -> trait565) = (& Trait565_S565child1_trait565));
  return impl;
} 
Trait565* Trait565_S565child1_ = newTrait565_S565child1();

int Trait10_S565child1_trait10(Class* self_) {
  S565child1* self = ((S565child1*)self_);
{
    return (self -> a);
  }
} 
Trait10* newTrait10_S565child1() {
  Trait10 (* impl) = (new Trait10());
  setVec(Trait10_v, S565child1_classId, ((void*)impl));
  ((impl -> trait10) = (& Trait10_S565child1_trait10));
  return impl;
} 
Trait10* Trait10_S565child1_ = newTrait10_S565child1();

int S566_classId = Class_genId();
struct S566{

  int id;
  S566 ():id(S566_classId){

  }
};


int S566child1_classId = Class_genId();
struct S566child1:S566{

  S566child1 (int a):a(a){
    (id = S566child1_classId);
{

    }
  }
  int a;
};


int S566child2_classId = Class_genId();
struct S566child2:S566{

  S566child2 (int a):a(a){
    (id = S566child2_classId);
{

    }
  }
  int a;
};


int S566child3_classId = Class_genId();
struct S566child3:S566{

  S566child3 (int a):a(a){
    (id = S566child3_classId);
{

    }
  }
  int a;
};


struct Trait566{

  int(*trait566)(Class*);
};

Vec* Trait566_v = newVec();

int Trait566_S566child1_trait566(Class* self_) {
  S566child1* self = ((S566child1*)self_);
{
    return (self -> a);
  }
} 
Trait566* newTrait566_S566child1() {
  Trait566 (* impl) = (new Trait566());
  setVec(Trait566_v, S566child1_classId, ((void*)impl));
  ((impl -> trait566) = (& Trait566_S566child1_trait566));
  return impl;
} 
Trait566* Trait566_S566child1_ = newTrait566_S566child1();

int Trait325_S566child1_trait325(Class* self_) {
  S566child1* self = ((S566child1*)self_);
{
    return (self -> a);
  }
} 
Trait325* newTrait325_S566child1() {
  Trait325 (* impl) = (new Trait325());
  setVec(Trait325_v, S566child1_classId, ((void*)impl));
  ((impl -> trait325) = (& Trait325_S566child1_trait325));
  return impl;
} 
Trait325* Trait325_S566child1_ = newTrait325_S566child1();

int S567_classId = Class_genId();
struct S567{

  int id;
  S567 ():id(S567_classId){

  }
};


int S567child1_classId = Class_genId();
struct S567child1:S567{

  S567child1 (int a):a(a){
    (id = S567child1_classId);
{

    }
  }
  int a;
};


struct Trait567{

  int(*trait567)(Class*);
};

Vec* Trait567_v = newVec();

int Trait567_S567child1_trait567(Class* self_) {
  S567child1* self = ((S567child1*)self_);
{
    return (self -> a);
  }
} 
Trait567* newTrait567_S567child1() {
  Trait567 (* impl) = (new Trait567());
  setVec(Trait567_v, S567child1_classId, ((void*)impl));
  ((impl -> trait567) = (& Trait567_S567child1_trait567));
  return impl;
} 
Trait567* Trait567_S567child1_ = newTrait567_S567child1();

int Trait255_S567child1_trait255(Class* self_) {
  S567child1* self = ((S567child1*)self_);
{
    return (self -> a);
  }
} 
Trait255* newTrait255_S567child1() {
  Trait255 (* impl) = (new Trait255());
  setVec(Trait255_v, S567child1_classId, ((void*)impl));
  ((impl -> trait255) = (& Trait255_S567child1_trait255));
  return impl;
} 
Trait255* Trait255_S567child1_ = newTrait255_S567child1();

int S568_classId = Class_genId();
struct S568{

  int id;
  S568 ():id(S568_classId){

  }
};


int S568child1_classId = Class_genId();
struct S568child1:S568{

  S568child1 (int a):a(a){
    (id = S568child1_classId);
{

    }
  }
  int a;
};


int S568child2_classId = Class_genId();
struct S568child2:S568{

  S568child2 (int a):a(a){
    (id = S568child2_classId);
{

    }
  }
  int a;
};


struct Trait568{

  int(*trait568)(Class*);
};

Vec* Trait568_v = newVec();

int Trait568_S568child1_trait568(Class* self_) {
  S568child1* self = ((S568child1*)self_);
{
    return (self -> a);
  }
} 
Trait568* newTrait568_S568child1() {
  Trait568 (* impl) = (new Trait568());
  setVec(Trait568_v, S568child1_classId, ((void*)impl));
  ((impl -> trait568) = (& Trait568_S568child1_trait568));
  return impl;
} 
Trait568* Trait568_S568child1_ = newTrait568_S568child1();

int Trait568_S568child2_trait568(Class* self_) {
  S568child2* self = ((S568child2*)self_);
{
    return (self -> a);
  }
} 
Trait568* newTrait568_S568child2() {
  Trait568 (* impl) = (new Trait568());
  setVec(Trait568_v, S568child2_classId, ((void*)impl));
  ((impl -> trait568) = (& Trait568_S568child2_trait568));
  return impl;
} 
Trait568* Trait568_S568child2_ = newTrait568_S568child2();

int Trait377_S568child1_trait377(Class* self_) {
  S568child1* self = ((S568child1*)self_);
{
    return (self -> a);
  }
} 
Trait377* newTrait377_S568child1() {
  Trait377 (* impl) = (new Trait377());
  setVec(Trait377_v, S568child1_classId, ((void*)impl));
  ((impl -> trait377) = (& Trait377_S568child1_trait377));
  return impl;
} 
Trait377* Trait377_S568child1_ = newTrait377_S568child1();

int S569_classId = Class_genId();
struct S569{

  int id;
  S569 ():id(S569_classId){

  }
};


int S569child1_classId = Class_genId();
struct S569child1:S569{

  S569child1 (int a):a(a){
    (id = S569child1_classId);
{

    }
  }
  int a;
};


int S569child2_classId = Class_genId();
struct S569child2:S569{

  S569child2 (int a):a(a){
    (id = S569child2_classId);
{

    }
  }
  int a;
};


struct Trait569{

  int(*trait569)(Class*);
};

Vec* Trait569_v = newVec();

int Trait569_S569child1_trait569(Class* self_) {
  S569child1* self = ((S569child1*)self_);
{
    return (self -> a);
  }
} 
Trait569* newTrait569_S569child1() {
  Trait569 (* impl) = (new Trait569());
  setVec(Trait569_v, S569child1_classId, ((void*)impl));
  ((impl -> trait569) = (& Trait569_S569child1_trait569));
  return impl;
} 
Trait569* Trait569_S569child1_ = newTrait569_S569child1();

int Trait306_S569child1_trait306(Class* self_) {
  S569child1* self = ((S569child1*)self_);
{
    return (self -> a);
  }
} 
Trait306* newTrait306_S569child1() {
  Trait306 (* impl) = (new Trait306());
  setVec(Trait306_v, S569child1_classId, ((void*)impl));
  ((impl -> trait306) = (& Trait306_S569child1_trait306));
  return impl;
} 
Trait306* Trait306_S569child1_ = newTrait306_S569child1();

int S570_classId = Class_genId();
struct S570{

  int id;
  S570 ():id(S570_classId){

  }
};


int S570child1_classId = Class_genId();
struct S570child1:S570{

  S570child1 (int a):a(a){
    (id = S570child1_classId);
{

    }
  }
  int a;
};


int S570child2_classId = Class_genId();
struct S570child2:S570{

  S570child2 (int a):a(a){
    (id = S570child2_classId);
{

    }
  }
  int a;
};


int S570child3_classId = Class_genId();
struct S570child3:S570{

  S570child3 (int a):a(a){
    (id = S570child3_classId);
{

    }
  }
  int a;
};


struct Trait570{

  int(*trait570)(Class*);
};

Vec* Trait570_v = newVec();

int Trait570_S570child1_trait570(Class* self_) {
  S570child1* self = ((S570child1*)self_);
{
    return (self -> a);
  }
} 
Trait570* newTrait570_S570child1() {
  Trait570 (* impl) = (new Trait570());
  setVec(Trait570_v, S570child1_classId, ((void*)impl));
  ((impl -> trait570) = (& Trait570_S570child1_trait570));
  return impl;
} 
Trait570* Trait570_S570child1_ = newTrait570_S570child1();

int Trait570_S570child2_trait570(Class* self_) {
  S570child2* self = ((S570child2*)self_);
{
    return (self -> a);
  }
} 
Trait570* newTrait570_S570child2() {
  Trait570 (* impl) = (new Trait570());
  setVec(Trait570_v, S570child2_classId, ((void*)impl));
  ((impl -> trait570) = (& Trait570_S570child2_trait570));
  return impl;
} 
Trait570* Trait570_S570child2_ = newTrait570_S570child2();

int Trait570_S570child3_trait570(Class* self_) {
  S570child3* self = ((S570child3*)self_);
{
    return (self -> a);
  }
} 
Trait570* newTrait570_S570child3() {
  Trait570 (* impl) = (new Trait570());
  setVec(Trait570_v, S570child3_classId, ((void*)impl));
  ((impl -> trait570) = (& Trait570_S570child3_trait570));
  return impl;
} 
Trait570* Trait570_S570child3_ = newTrait570_S570child3();

int Trait364_S570child1_trait364(Class* self_) {
  S570child1* self = ((S570child1*)self_);
{
    return (self -> a);
  }
} 
Trait364* newTrait364_S570child1() {
  Trait364 (* impl) = (new Trait364());
  setVec(Trait364_v, S570child1_classId, ((void*)impl));
  ((impl -> trait364) = (& Trait364_S570child1_trait364));
  return impl;
} 
Trait364* Trait364_S570child1_ = newTrait364_S570child1();

int S571_classId = Class_genId();
struct S571{

  int id;
  S571 ():id(S571_classId){

  }
};


int S571child1_classId = Class_genId();
struct S571child1:S571{

  S571child1 (int a):a(a){
    (id = S571child1_classId);
{

    }
  }
  int a;
};


struct Trait571{

  int(*trait571)(Class*);
};

Vec* Trait571_v = newVec();

int Trait571_S571child1_trait571(Class* self_) {
  S571child1* self = ((S571child1*)self_);
{
    return (self -> a);
  }
} 
Trait571* newTrait571_S571child1() {
  Trait571 (* impl) = (new Trait571());
  setVec(Trait571_v, S571child1_classId, ((void*)impl));
  ((impl -> trait571) = (& Trait571_S571child1_trait571));
  return impl;
} 
Trait571* Trait571_S571child1_ = newTrait571_S571child1();

int Trait406_S571child1_trait406(Class* self_) {
  S571child1* self = ((S571child1*)self_);
{
    return (self -> a);
  }
} 
Trait406* newTrait406_S571child1() {
  Trait406 (* impl) = (new Trait406());
  setVec(Trait406_v, S571child1_classId, ((void*)impl));
  ((impl -> trait406) = (& Trait406_S571child1_trait406));
  return impl;
} 
Trait406* Trait406_S571child1_ = newTrait406_S571child1();

int S572_classId = Class_genId();
struct S572{

  int id;
  S572 ():id(S572_classId){

  }
};


int S572child1_classId = Class_genId();
struct S572child1:S572{

  S572child1 (int a):a(a){
    (id = S572child1_classId);
{

    }
  }
  int a;
};


int S572child2_classId = Class_genId();
struct S572child2:S572{

  S572child2 (int a):a(a){
    (id = S572child2_classId);
{

    }
  }
  int a;
};


struct Trait572{

  int(*trait572)(Class*);
};

Vec* Trait572_v = newVec();

int Trait572_S572child1_trait572(Class* self_) {
  S572child1* self = ((S572child1*)self_);
{
    return (self -> a);
  }
} 
Trait572* newTrait572_S572child1() {
  Trait572 (* impl) = (new Trait572());
  setVec(Trait572_v, S572child1_classId, ((void*)impl));
  ((impl -> trait572) = (& Trait572_S572child1_trait572));
  return impl;
} 
Trait572* Trait572_S572child1_ = newTrait572_S572child1();

int Trait572_S572child2_trait572(Class* self_) {
  S572child2* self = ((S572child2*)self_);
{
    return (self -> a);
  }
} 
Trait572* newTrait572_S572child2() {
  Trait572 (* impl) = (new Trait572());
  setVec(Trait572_v, S572child2_classId, ((void*)impl));
  ((impl -> trait572) = (& Trait572_S572child2_trait572));
  return impl;
} 
Trait572* Trait572_S572child2_ = newTrait572_S572child2();

int Trait303_S572child1_trait303(Class* self_) {
  S572child1* self = ((S572child1*)self_);
{
    return (self -> a);
  }
} 
Trait303* newTrait303_S572child1() {
  Trait303 (* impl) = (new Trait303());
  setVec(Trait303_v, S572child1_classId, ((void*)impl));
  ((impl -> trait303) = (& Trait303_S572child1_trait303));
  return impl;
} 
Trait303* Trait303_S572child1_ = newTrait303_S572child1();

int S573_classId = Class_genId();
struct S573{

  int id;
  S573 ():id(S573_classId){

  }
};


int S573child1_classId = Class_genId();
struct S573child1:S573{

  S573child1 (int a):a(a){
    (id = S573child1_classId);
{

    }
  }
  int a;
};


struct Trait573{

  int(*trait573)(Class*);
};

Vec* Trait573_v = newVec();

int Trait573_S573child1_trait573(Class* self_) {
  S573child1* self = ((S573child1*)self_);
{
    return (self -> a);
  }
} 
Trait573* newTrait573_S573child1() {
  Trait573 (* impl) = (new Trait573());
  setVec(Trait573_v, S573child1_classId, ((void*)impl));
  ((impl -> trait573) = (& Trait573_S573child1_trait573));
  return impl;
} 
Trait573* Trait573_S573child1_ = newTrait573_S573child1();

int Trait404_S573child1_trait404(Class* self_) {
  S573child1* self = ((S573child1*)self_);
{
    return (self -> a);
  }
} 
Trait404* newTrait404_S573child1() {
  Trait404 (* impl) = (new Trait404());
  setVec(Trait404_v, S573child1_classId, ((void*)impl));
  ((impl -> trait404) = (& Trait404_S573child1_trait404));
  return impl;
} 
Trait404* Trait404_S573child1_ = newTrait404_S573child1();

int S574_classId = Class_genId();
struct S574{

  int id;
  S574 ():id(S574_classId){

  }
};


int S574child1_classId = Class_genId();
struct S574child1:S574{

  S574child1 (int a):a(a){
    (id = S574child1_classId);
{

    }
  }
  int a;
};


int S574child2_classId = Class_genId();
struct S574child2:S574{

  S574child2 (int a):a(a){
    (id = S574child2_classId);
{

    }
  }
  int a;
};


int S574child3_classId = Class_genId();
struct S574child3:S574{

  S574child3 (int a):a(a){
    (id = S574child3_classId);
{

    }
  }
  int a;
};


struct Trait574{

  int(*trait574)(Class*);
};

Vec* Trait574_v = newVec();

int Trait574_S574child1_trait574(Class* self_) {
  S574child1* self = ((S574child1*)self_);
{
    return (self -> a);
  }
} 
Trait574* newTrait574_S574child1() {
  Trait574 (* impl) = (new Trait574());
  setVec(Trait574_v, S574child1_classId, ((void*)impl));
  ((impl -> trait574) = (& Trait574_S574child1_trait574));
  return impl;
} 
Trait574* Trait574_S574child1_ = newTrait574_S574child1();

int Trait574_S574child2_trait574(Class* self_) {
  S574child2* self = ((S574child2*)self_);
{
    return (self -> a);
  }
} 
Trait574* newTrait574_S574child2() {
  Trait574 (* impl) = (new Trait574());
  setVec(Trait574_v, S574child2_classId, ((void*)impl));
  ((impl -> trait574) = (& Trait574_S574child2_trait574));
  return impl;
} 
Trait574* Trait574_S574child2_ = newTrait574_S574child2();

int Trait518_S574child1_trait518(Class* self_) {
  S574child1* self = ((S574child1*)self_);
{
    return (self -> a);
  }
} 
Trait518* newTrait518_S574child1() {
  Trait518 (* impl) = (new Trait518());
  setVec(Trait518_v, S574child1_classId, ((void*)impl));
  ((impl -> trait518) = (& Trait518_S574child1_trait518));
  return impl;
} 
Trait518* Trait518_S574child1_ = newTrait518_S574child1();

int S575_classId = Class_genId();
struct S575{

  int id;
  S575 ():id(S575_classId){

  }
};


int S575child1_classId = Class_genId();
struct S575child1:S575{

  S575child1 (int a):a(a){
    (id = S575child1_classId);
{

    }
  }
  int a;
};


int S575child2_classId = Class_genId();
struct S575child2:S575{

  S575child2 (int a):a(a){
    (id = S575child2_classId);
{

    }
  }
  int a;
};


int S575child3_classId = Class_genId();
struct S575child3:S575{

  S575child3 (int a):a(a){
    (id = S575child3_classId);
{

    }
  }
  int a;
};


int S575child4_classId = Class_genId();
struct S575child4:S575{

  S575child4 (int a):a(a){
    (id = S575child4_classId);
{

    }
  }
  int a;
};


struct Trait575{

  int(*trait575)(Class*);
};

Vec* Trait575_v = newVec();

int Trait575_S575child1_trait575(Class* self_) {
  S575child1* self = ((S575child1*)self_);
{
    return (self -> a);
  }
} 
Trait575* newTrait575_S575child1() {
  Trait575 (* impl) = (new Trait575());
  setVec(Trait575_v, S575child1_classId, ((void*)impl));
  ((impl -> trait575) = (& Trait575_S575child1_trait575));
  return impl;
} 
Trait575* Trait575_S575child1_ = newTrait575_S575child1();

int Trait337_S575child1_trait337(Class* self_) {
  S575child1* self = ((S575child1*)self_);
{
    return (self -> a);
  }
} 
Trait337* newTrait337_S575child1() {
  Trait337 (* impl) = (new Trait337());
  setVec(Trait337_v, S575child1_classId, ((void*)impl));
  ((impl -> trait337) = (& Trait337_S575child1_trait337));
  return impl;
} 
Trait337* Trait337_S575child1_ = newTrait337_S575child1();

int S576_classId = Class_genId();
struct S576{

  int id;
  S576 ():id(S576_classId){

  }
};


int S576child1_classId = Class_genId();
struct S576child1:S576{

  S576child1 (int a):a(a){
    (id = S576child1_classId);
{

    }
  }
  int a;
};


struct Trait576{

  int(*trait576)(Class*);
};

Vec* Trait576_v = newVec();

int Trait576_S576child1_trait576(Class* self_) {
  S576child1* self = ((S576child1*)self_);
{
    return (self -> a);
  }
} 
Trait576* newTrait576_S576child1() {
  Trait576 (* impl) = (new Trait576());
  setVec(Trait576_v, S576child1_classId, ((void*)impl));
  ((impl -> trait576) = (& Trait576_S576child1_trait576));
  return impl;
} 
Trait576* Trait576_S576child1_ = newTrait576_S576child1();

int Trait569_S576child1_trait569(Class* self_) {
  S576child1* self = ((S576child1*)self_);
{
    return (self -> a);
  }
} 
Trait569* newTrait569_S576child1() {
  Trait569 (* impl) = (new Trait569());
  setVec(Trait569_v, S576child1_classId, ((void*)impl));
  ((impl -> trait569) = (& Trait569_S576child1_trait569));
  return impl;
} 
Trait569* Trait569_S576child1_ = newTrait569_S576child1();

int S577_classId = Class_genId();
struct S577{

  int id;
  S577 ():id(S577_classId){

  }
};


int S577child1_classId = Class_genId();
struct S577child1:S577{

  S577child1 (int a):a(a){
    (id = S577child1_classId);
{

    }
  }
  int a;
};


int S577child2_classId = Class_genId();
struct S577child2:S577{

  S577child2 (int a):a(a){
    (id = S577child2_classId);
{

    }
  }
  int a;
};


int S577child3_classId = Class_genId();
struct S577child3:S577{

  S577child3 (int a):a(a){
    (id = S577child3_classId);
{

    }
  }
  int a;
};


int S577child4_classId = Class_genId();
struct S577child4:S577{

  S577child4 (int a):a(a){
    (id = S577child4_classId);
{

    }
  }
  int a;
};


int S577child5_classId = Class_genId();
struct S577child5:S577{

  S577child5 (int a):a(a){
    (id = S577child5_classId);
{

    }
  }
  int a;
};


struct Trait577{

  int(*trait577)(Class*);
};

Vec* Trait577_v = newVec();

int Trait577_S577child1_trait577(Class* self_) {
  S577child1* self = ((S577child1*)self_);
{
    return (self -> a);
  }
} 
Trait577* newTrait577_S577child1() {
  Trait577 (* impl) = (new Trait577());
  setVec(Trait577_v, S577child1_classId, ((void*)impl));
  ((impl -> trait577) = (& Trait577_S577child1_trait577));
  return impl;
} 
Trait577* Trait577_S577child1_ = newTrait577_S577child1();

int Trait577_S577child2_trait577(Class* self_) {
  S577child2* self = ((S577child2*)self_);
{
    return (self -> a);
  }
} 
Trait577* newTrait577_S577child2() {
  Trait577 (* impl) = (new Trait577());
  setVec(Trait577_v, S577child2_classId, ((void*)impl));
  ((impl -> trait577) = (& Trait577_S577child2_trait577));
  return impl;
} 
Trait577* Trait577_S577child2_ = newTrait577_S577child2();

int Trait577_S577child3_trait577(Class* self_) {
  S577child3* self = ((S577child3*)self_);
{
    return (self -> a);
  }
} 
Trait577* newTrait577_S577child3() {
  Trait577 (* impl) = (new Trait577());
  setVec(Trait577_v, S577child3_classId, ((void*)impl));
  ((impl -> trait577) = (& Trait577_S577child3_trait577));
  return impl;
} 
Trait577* Trait577_S577child3_ = newTrait577_S577child3();

int Trait408_S577child1_trait408(Class* self_) {
  S577child1* self = ((S577child1*)self_);
{
    return (self -> a);
  }
} 
Trait408* newTrait408_S577child1() {
  Trait408 (* impl) = (new Trait408());
  setVec(Trait408_v, S577child1_classId, ((void*)impl));
  ((impl -> trait408) = (& Trait408_S577child1_trait408));
  return impl;
} 
Trait408* Trait408_S577child1_ = newTrait408_S577child1();

int S578_classId = Class_genId();
struct S578{

  int id;
  S578 ():id(S578_classId){

  }
};


int S578child1_classId = Class_genId();
struct S578child1:S578{

  S578child1 (int a):a(a){
    (id = S578child1_classId);
{

    }
  }
  int a;
};


struct Trait578{

  int(*trait578)(Class*);
};

Vec* Trait578_v = newVec();

int Trait578_S578child1_trait578(Class* self_) {
  S578child1* self = ((S578child1*)self_);
{
    return (self -> a);
  }
} 
Trait578* newTrait578_S578child1() {
  Trait578 (* impl) = (new Trait578());
  setVec(Trait578_v, S578child1_classId, ((void*)impl));
  ((impl -> trait578) = (& Trait578_S578child1_trait578));
  return impl;
} 
Trait578* Trait578_S578child1_ = newTrait578_S578child1();

int Trait438_S578child1_trait438(Class* self_) {
  S578child1* self = ((S578child1*)self_);
{
    return (self -> a);
  }
} 
Trait438* newTrait438_S578child1() {
  Trait438 (* impl) = (new Trait438());
  setVec(Trait438_v, S578child1_classId, ((void*)impl));
  ((impl -> trait438) = (& Trait438_S578child1_trait438));
  return impl;
} 
Trait438* Trait438_S578child1_ = newTrait438_S578child1();

int S579_classId = Class_genId();
struct S579{

  int id;
  S579 ():id(S579_classId){

  }
};


int S579child1_classId = Class_genId();
struct S579child1:S579{

  S579child1 (int a):a(a){
    (id = S579child1_classId);
{

    }
  }
  int a;
};


int S579child2_classId = Class_genId();
struct S579child2:S579{

  S579child2 (int a):a(a){
    (id = S579child2_classId);
{

    }
  }
  int a;
};


int S579child3_classId = Class_genId();
struct S579child3:S579{

  S579child3 (int a):a(a){
    (id = S579child3_classId);
{

    }
  }
  int a;
};


struct Trait579{

  int(*trait579)(Class*);
};

Vec* Trait579_v = newVec();

int Trait579_S579child1_trait579(Class* self_) {
  S579child1* self = ((S579child1*)self_);
{
    return (self -> a);
  }
} 
Trait579* newTrait579_S579child1() {
  Trait579 (* impl) = (new Trait579());
  setVec(Trait579_v, S579child1_classId, ((void*)impl));
  ((impl -> trait579) = (& Trait579_S579child1_trait579));
  return impl;
} 
Trait579* Trait579_S579child1_ = newTrait579_S579child1();

int Trait579_S579child2_trait579(Class* self_) {
  S579child2* self = ((S579child2*)self_);
{
    return (self -> a);
  }
} 
Trait579* newTrait579_S579child2() {
  Trait579 (* impl) = (new Trait579());
  setVec(Trait579_v, S579child2_classId, ((void*)impl));
  ((impl -> trait579) = (& Trait579_S579child2_trait579));
  return impl;
} 
Trait579* Trait579_S579child2_ = newTrait579_S579child2();

int Trait304_S579child1_trait304(Class* self_) {
  S579child1* self = ((S579child1*)self_);
{
    return (self -> a);
  }
} 
Trait304* newTrait304_S579child1() {
  Trait304 (* impl) = (new Trait304());
  setVec(Trait304_v, S579child1_classId, ((void*)impl));
  ((impl -> trait304) = (& Trait304_S579child1_trait304));
  return impl;
} 
Trait304* Trait304_S579child1_ = newTrait304_S579child1();

int S580_classId = Class_genId();
struct S580{

  int id;
  S580 ():id(S580_classId){

  }
};


int S580child1_classId = Class_genId();
struct S580child1:S580{

  S580child1 (int a):a(a){
    (id = S580child1_classId);
{

    }
  }
  int a;
};


int S580child2_classId = Class_genId();
struct S580child2:S580{

  S580child2 (int a):a(a){
    (id = S580child2_classId);
{

    }
  }
  int a;
};


int S580child3_classId = Class_genId();
struct S580child3:S580{

  S580child3 (int a):a(a){
    (id = S580child3_classId);
{

    }
  }
  int a;
};


int S580child4_classId = Class_genId();
struct S580child4:S580{

  S580child4 (int a):a(a){
    (id = S580child4_classId);
{

    }
  }
  int a;
};


int S580child5_classId = Class_genId();
struct S580child5:S580{

  S580child5 (int a):a(a){
    (id = S580child5_classId);
{

    }
  }
  int a;
};


struct Trait580{

  int(*trait580)(Class*);
};

Vec* Trait580_v = newVec();

int Trait580_S580child1_trait580(Class* self_) {
  S580child1* self = ((S580child1*)self_);
{
    return (self -> a);
  }
} 
Trait580* newTrait580_S580child1() {
  Trait580 (* impl) = (new Trait580());
  setVec(Trait580_v, S580child1_classId, ((void*)impl));
  ((impl -> trait580) = (& Trait580_S580child1_trait580));
  return impl;
} 
Trait580* Trait580_S580child1_ = newTrait580_S580child1();

int Trait580_S580child2_trait580(Class* self_) {
  S580child2* self = ((S580child2*)self_);
{
    return (self -> a);
  }
} 
Trait580* newTrait580_S580child2() {
  Trait580 (* impl) = (new Trait580());
  setVec(Trait580_v, S580child2_classId, ((void*)impl));
  ((impl -> trait580) = (& Trait580_S580child2_trait580));
  return impl;
} 
Trait580* Trait580_S580child2_ = newTrait580_S580child2();

int Trait580_S580child3_trait580(Class* self_) {
  S580child3* self = ((S580child3*)self_);
{
    return (self -> a);
  }
} 
Trait580* newTrait580_S580child3() {
  Trait580 (* impl) = (new Trait580());
  setVec(Trait580_v, S580child3_classId, ((void*)impl));
  ((impl -> trait580) = (& Trait580_S580child3_trait580));
  return impl;
} 
Trait580* Trait580_S580child3_ = newTrait580_S580child3();

int Trait161_S580child1_trait161(Class* self_) {
  S580child1* self = ((S580child1*)self_);
{
    return (self -> a);
  }
} 
Trait161* newTrait161_S580child1() {
  Trait161 (* impl) = (new Trait161());
  setVec(Trait161_v, S580child1_classId, ((void*)impl));
  ((impl -> trait161) = (& Trait161_S580child1_trait161));
  return impl;
} 
Trait161* Trait161_S580child1_ = newTrait161_S580child1();

int S581_classId = Class_genId();
struct S581{

  int id;
  S581 ():id(S581_classId){

  }
};


int S581child1_classId = Class_genId();
struct S581child1:S581{

  S581child1 (int a):a(a){
    (id = S581child1_classId);
{

    }
  }
  int a;
};


int S581child2_classId = Class_genId();
struct S581child2:S581{

  S581child2 (int a):a(a){
    (id = S581child2_classId);
{

    }
  }
  int a;
};


struct Trait581{

  int(*trait581)(Class*);
};

Vec* Trait581_v = newVec();

int Trait581_S581child1_trait581(Class* self_) {
  S581child1* self = ((S581child1*)self_);
{
    return (self -> a);
  }
} 
Trait581* newTrait581_S581child1() {
  Trait581 (* impl) = (new Trait581());
  setVec(Trait581_v, S581child1_classId, ((void*)impl));
  ((impl -> trait581) = (& Trait581_S581child1_trait581));
  return impl;
} 
Trait581* Trait581_S581child1_ = newTrait581_S581child1();

int Trait581_S581child2_trait581(Class* self_) {
  S581child2* self = ((S581child2*)self_);
{
    return (self -> a);
  }
} 
Trait581* newTrait581_S581child2() {
  Trait581 (* impl) = (new Trait581());
  setVec(Trait581_v, S581child2_classId, ((void*)impl));
  ((impl -> trait581) = (& Trait581_S581child2_trait581));
  return impl;
} 
Trait581* Trait581_S581child2_ = newTrait581_S581child2();

int Trait59_S581child1_trait59(Class* self_) {
  S581child1* self = ((S581child1*)self_);
{
    return (self -> a);
  }
} 
Trait59* newTrait59_S581child1() {
  Trait59 (* impl) = (new Trait59());
  setVec(Trait59_v, S581child1_classId, ((void*)impl));
  ((impl -> trait59) = (& Trait59_S581child1_trait59));
  return impl;
} 
Trait59* Trait59_S581child1_ = newTrait59_S581child1();

int S582_classId = Class_genId();
struct S582{

  int id;
  S582 ():id(S582_classId){

  }
};


int S582child1_classId = Class_genId();
struct S582child1:S582{

  S582child1 (int a):a(a){
    (id = S582child1_classId);
{

    }
  }
  int a;
};


int S582child2_classId = Class_genId();
struct S582child2:S582{

  S582child2 (int a):a(a){
    (id = S582child2_classId);
{

    }
  }
  int a;
};


struct Trait582{

  int(*trait582)(Class*);
};

Vec* Trait582_v = newVec();

int Trait582_S582child1_trait582(Class* self_) {
  S582child1* self = ((S582child1*)self_);
{
    return (self -> a);
  }
} 
Trait582* newTrait582_S582child1() {
  Trait582 (* impl) = (new Trait582());
  setVec(Trait582_v, S582child1_classId, ((void*)impl));
  ((impl -> trait582) = (& Trait582_S582child1_trait582));
  return impl;
} 
Trait582* Trait582_S582child1_ = newTrait582_S582child1();

int Trait217_S582child1_trait217(Class* self_) {
  S582child1* self = ((S582child1*)self_);
{
    return (self -> a);
  }
} 
Trait217* newTrait217_S582child1() {
  Trait217 (* impl) = (new Trait217());
  setVec(Trait217_v, S582child1_classId, ((void*)impl));
  ((impl -> trait217) = (& Trait217_S582child1_trait217));
  return impl;
} 
Trait217* Trait217_S582child1_ = newTrait217_S582child1();

int S583_classId = Class_genId();
struct S583{

  int id;
  S583 ():id(S583_classId){

  }
};


int S583child1_classId = Class_genId();
struct S583child1:S583{

  S583child1 (int a):a(a){
    (id = S583child1_classId);
{

    }
  }
  int a;
};


struct Trait583{

  int(*trait583)(Class*);
};

Vec* Trait583_v = newVec();

int Trait583_S583child1_trait583(Class* self_) {
  S583child1* self = ((S583child1*)self_);
{
    return (self -> a);
  }
} 
Trait583* newTrait583_S583child1() {
  Trait583 (* impl) = (new Trait583());
  setVec(Trait583_v, S583child1_classId, ((void*)impl));
  ((impl -> trait583) = (& Trait583_S583child1_trait583));
  return impl;
} 
Trait583* Trait583_S583child1_ = newTrait583_S583child1();

int Trait255_S583child1_trait255(Class* self_) {
  S583child1* self = ((S583child1*)self_);
{
    return (self -> a);
  }
} 
Trait255* newTrait255_S583child1() {
  Trait255 (* impl) = (new Trait255());
  setVec(Trait255_v, S583child1_classId, ((void*)impl));
  ((impl -> trait255) = (& Trait255_S583child1_trait255));
  return impl;
} 
Trait255* Trait255_S583child1_ = newTrait255_S583child1();

int S584_classId = Class_genId();
struct S584{

  int id;
  S584 ():id(S584_classId){

  }
};


int S584child1_classId = Class_genId();
struct S584child1:S584{

  S584child1 (int a):a(a){
    (id = S584child1_classId);
{

    }
  }
  int a;
};


int S584child2_classId = Class_genId();
struct S584child2:S584{

  S584child2 (int a):a(a){
    (id = S584child2_classId);
{

    }
  }
  int a;
};


struct Trait584{

  int(*trait584)(Class*);
};

Vec* Trait584_v = newVec();

int Trait584_S584child1_trait584(Class* self_) {
  S584child1* self = ((S584child1*)self_);
{
    return (self -> a);
  }
} 
Trait584* newTrait584_S584child1() {
  Trait584 (* impl) = (new Trait584());
  setVec(Trait584_v, S584child1_classId, ((void*)impl));
  ((impl -> trait584) = (& Trait584_S584child1_trait584));
  return impl;
} 
Trait584* Trait584_S584child1_ = newTrait584_S584child1();

int Trait434_S584child1_trait434(Class* self_) {
  S584child1* self = ((S584child1*)self_);
{
    return (self -> a);
  }
} 
Trait434* newTrait434_S584child1() {
  Trait434 (* impl) = (new Trait434());
  setVec(Trait434_v, S584child1_classId, ((void*)impl));
  ((impl -> trait434) = (& Trait434_S584child1_trait434));
  return impl;
} 
Trait434* Trait434_S584child1_ = newTrait434_S584child1();

int S585_classId = Class_genId();
struct S585{

  int id;
  S585 ():id(S585_classId){

  }
};


int S585child1_classId = Class_genId();
struct S585child1:S585{

  S585child1 (int a):a(a){
    (id = S585child1_classId);
{

    }
  }
  int a;
};


int S585child2_classId = Class_genId();
struct S585child2:S585{

  S585child2 (int a):a(a){
    (id = S585child2_classId);
{

    }
  }
  int a;
};


int S585child3_classId = Class_genId();
struct S585child3:S585{

  S585child3 (int a):a(a){
    (id = S585child3_classId);
{

    }
  }
  int a;
};


struct Trait585{

  int(*trait585)(Class*);
};

Vec* Trait585_v = newVec();

int Trait585_S585child1_trait585(Class* self_) {
  S585child1* self = ((S585child1*)self_);
{
    return (self -> a);
  }
} 
Trait585* newTrait585_S585child1() {
  Trait585 (* impl) = (new Trait585());
  setVec(Trait585_v, S585child1_classId, ((void*)impl));
  ((impl -> trait585) = (& Trait585_S585child1_trait585));
  return impl;
} 
Trait585* Trait585_S585child1_ = newTrait585_S585child1();

int Trait585_S585child2_trait585(Class* self_) {
  S585child2* self = ((S585child2*)self_);
{
    return (self -> a);
  }
} 
Trait585* newTrait585_S585child2() {
  Trait585 (* impl) = (new Trait585());
  setVec(Trait585_v, S585child2_classId, ((void*)impl));
  ((impl -> trait585) = (& Trait585_S585child2_trait585));
  return impl;
} 
Trait585* Trait585_S585child2_ = newTrait585_S585child2();

int Trait362_S585child1_trait362(Class* self_) {
  S585child1* self = ((S585child1*)self_);
{
    return (self -> a);
  }
} 
Trait362* newTrait362_S585child1() {
  Trait362 (* impl) = (new Trait362());
  setVec(Trait362_v, S585child1_classId, ((void*)impl));
  ((impl -> trait362) = (& Trait362_S585child1_trait362));
  return impl;
} 
Trait362* Trait362_S585child1_ = newTrait362_S585child1();

int S586_classId = Class_genId();
struct S586{

  int id;
  S586 ():id(S586_classId){

  }
};


int S586child1_classId = Class_genId();
struct S586child1:S586{

  S586child1 (int a):a(a){
    (id = S586child1_classId);
{

    }
  }
  int a;
};


int S586child2_classId = Class_genId();
struct S586child2:S586{

  S586child2 (int a):a(a){
    (id = S586child2_classId);
{

    }
  }
  int a;
};


struct Trait586{

  int(*trait586)(Class*);
};

Vec* Trait586_v = newVec();

int Trait586_S586child1_trait586(Class* self_) {
  S586child1* self = ((S586child1*)self_);
{
    return (self -> a);
  }
} 
Trait586* newTrait586_S586child1() {
  Trait586 (* impl) = (new Trait586());
  setVec(Trait586_v, S586child1_classId, ((void*)impl));
  ((impl -> trait586) = (& Trait586_S586child1_trait586));
  return impl;
} 
Trait586* Trait586_S586child1_ = newTrait586_S586child1();

int Trait586_S586child2_trait586(Class* self_) {
  S586child2* self = ((S586child2*)self_);
{
    return (self -> a);
  }
} 
Trait586* newTrait586_S586child2() {
  Trait586 (* impl) = (new Trait586());
  setVec(Trait586_v, S586child2_classId, ((void*)impl));
  ((impl -> trait586) = (& Trait586_S586child2_trait586));
  return impl;
} 
Trait586* Trait586_S586child2_ = newTrait586_S586child2();

int Trait497_S586child1_trait497(Class* self_) {
  S586child1* self = ((S586child1*)self_);
{
    return (self -> a);
  }
} 
Trait497* newTrait497_S586child1() {
  Trait497 (* impl) = (new Trait497());
  setVec(Trait497_v, S586child1_classId, ((void*)impl));
  ((impl -> trait497) = (& Trait497_S586child1_trait497));
  return impl;
} 
Trait497* Trait497_S586child1_ = newTrait497_S586child1();

int S587_classId = Class_genId();
struct S587{

  int id;
  S587 ():id(S587_classId){

  }
};


int S587child1_classId = Class_genId();
struct S587child1:S587{

  S587child1 (int a):a(a){
    (id = S587child1_classId);
{

    }
  }
  int a;
};


int S587child2_classId = Class_genId();
struct S587child2:S587{

  S587child2 (int a):a(a){
    (id = S587child2_classId);
{

    }
  }
  int a;
};


int S587child3_classId = Class_genId();
struct S587child3:S587{

  S587child3 (int a):a(a){
    (id = S587child3_classId);
{

    }
  }
  int a;
};


int S587child4_classId = Class_genId();
struct S587child4:S587{

  S587child4 (int a):a(a){
    (id = S587child4_classId);
{

    }
  }
  int a;
};


struct Trait587{

  int(*trait587)(Class*);
};

Vec* Trait587_v = newVec();

int Trait587_S587child1_trait587(Class* self_) {
  S587child1* self = ((S587child1*)self_);
{
    return (self -> a);
  }
} 
Trait587* newTrait587_S587child1() {
  Trait587 (* impl) = (new Trait587());
  setVec(Trait587_v, S587child1_classId, ((void*)impl));
  ((impl -> trait587) = (& Trait587_S587child1_trait587));
  return impl;
} 
Trait587* Trait587_S587child1_ = newTrait587_S587child1();

int Trait587_S587child2_trait587(Class* self_) {
  S587child2* self = ((S587child2*)self_);
{
    return (self -> a);
  }
} 
Trait587* newTrait587_S587child2() {
  Trait587 (* impl) = (new Trait587());
  setVec(Trait587_v, S587child2_classId, ((void*)impl));
  ((impl -> trait587) = (& Trait587_S587child2_trait587));
  return impl;
} 
Trait587* Trait587_S587child2_ = newTrait587_S587child2();

int Trait587_S587child3_trait587(Class* self_) {
  S587child3* self = ((S587child3*)self_);
{
    return (self -> a);
  }
} 
Trait587* newTrait587_S587child3() {
  Trait587 (* impl) = (new Trait587());
  setVec(Trait587_v, S587child3_classId, ((void*)impl));
  ((impl -> trait587) = (& Trait587_S587child3_trait587));
  return impl;
} 
Trait587* Trait587_S587child3_ = newTrait587_S587child3();

int Trait238_S587child1_trait238(Class* self_) {
  S587child1* self = ((S587child1*)self_);
{
    return (self -> a);
  }
} 
Trait238* newTrait238_S587child1() {
  Trait238 (* impl) = (new Trait238());
  setVec(Trait238_v, S587child1_classId, ((void*)impl));
  ((impl -> trait238) = (& Trait238_S587child1_trait238));
  return impl;
} 
Trait238* Trait238_S587child1_ = newTrait238_S587child1();

int S588_classId = Class_genId();
struct S588{

  int id;
  S588 ():id(S588_classId){

  }
};


int S588child1_classId = Class_genId();
struct S588child1:S588{

  S588child1 (int a):a(a){
    (id = S588child1_classId);
{

    }
  }
  int a;
};


int S588child2_classId = Class_genId();
struct S588child2:S588{

  S588child2 (int a):a(a){
    (id = S588child2_classId);
{

    }
  }
  int a;
};


int S588child3_classId = Class_genId();
struct S588child3:S588{

  S588child3 (int a):a(a){
    (id = S588child3_classId);
{

    }
  }
  int a;
};


struct Trait588{

  int(*trait588)(Class*);
};

Vec* Trait588_v = newVec();

int Trait588_S588child1_trait588(Class* self_) {
  S588child1* self = ((S588child1*)self_);
{
    return (self -> a);
  }
} 
Trait588* newTrait588_S588child1() {
  Trait588 (* impl) = (new Trait588());
  setVec(Trait588_v, S588child1_classId, ((void*)impl));
  ((impl -> trait588) = (& Trait588_S588child1_trait588));
  return impl;
} 
Trait588* Trait588_S588child1_ = newTrait588_S588child1();

int Trait390_S588child1_trait390(Class* self_) {
  S588child1* self = ((S588child1*)self_);
{
    return (self -> a);
  }
} 
Trait390* newTrait390_S588child1() {
  Trait390 (* impl) = (new Trait390());
  setVec(Trait390_v, S588child1_classId, ((void*)impl));
  ((impl -> trait390) = (& Trait390_S588child1_trait390));
  return impl;
} 
Trait390* Trait390_S588child1_ = newTrait390_S588child1();

int S589_classId = Class_genId();
struct S589{

  int id;
  S589 ():id(S589_classId){

  }
};


int S589child1_classId = Class_genId();
struct S589child1:S589{

  S589child1 (int a):a(a){
    (id = S589child1_classId);
{

    }
  }
  int a;
};


int S589child2_classId = Class_genId();
struct S589child2:S589{

  S589child2 (int a):a(a){
    (id = S589child2_classId);
{

    }
  }
  int a;
};


int S589child3_classId = Class_genId();
struct S589child3:S589{

  S589child3 (int a):a(a){
    (id = S589child3_classId);
{

    }
  }
  int a;
};


struct Trait589{

  int(*trait589)(Class*);
};

Vec* Trait589_v = newVec();

int Trait589_S589child1_trait589(Class* self_) {
  S589child1* self = ((S589child1*)self_);
{
    return (self -> a);
  }
} 
Trait589* newTrait589_S589child1() {
  Trait589 (* impl) = (new Trait589());
  setVec(Trait589_v, S589child1_classId, ((void*)impl));
  ((impl -> trait589) = (& Trait589_S589child1_trait589));
  return impl;
} 
Trait589* Trait589_S589child1_ = newTrait589_S589child1();

int Trait107_S589child1_trait107(Class* self_) {
  S589child1* self = ((S589child1*)self_);
{
    return (self -> a);
  }
} 
Trait107* newTrait107_S589child1() {
  Trait107 (* impl) = (new Trait107());
  setVec(Trait107_v, S589child1_classId, ((void*)impl));
  ((impl -> trait107) = (& Trait107_S589child1_trait107));
  return impl;
} 
Trait107* Trait107_S589child1_ = newTrait107_S589child1();

int S590_classId = Class_genId();
struct S590{

  int id;
  S590 ():id(S590_classId){

  }
};


int S590child1_classId = Class_genId();
struct S590child1:S590{

  S590child1 (int a):a(a){
    (id = S590child1_classId);
{

    }
  }
  int a;
};


int S590child2_classId = Class_genId();
struct S590child2:S590{

  S590child2 (int a):a(a){
    (id = S590child2_classId);
{

    }
  }
  int a;
};


struct Trait590{

  int(*trait590)(Class*);
};

Vec* Trait590_v = newVec();

int Trait590_S590child1_trait590(Class* self_) {
  S590child1* self = ((S590child1*)self_);
{
    return (self -> a);
  }
} 
Trait590* newTrait590_S590child1() {
  Trait590 (* impl) = (new Trait590());
  setVec(Trait590_v, S590child1_classId, ((void*)impl));
  ((impl -> trait590) = (& Trait590_S590child1_trait590));
  return impl;
} 
Trait590* Trait590_S590child1_ = newTrait590_S590child1();

int Trait181_S590child1_trait181(Class* self_) {
  S590child1* self = ((S590child1*)self_);
{
    return (self -> a);
  }
} 
Trait181* newTrait181_S590child1() {
  Trait181 (* impl) = (new Trait181());
  setVec(Trait181_v, S590child1_classId, ((void*)impl));
  ((impl -> trait181) = (& Trait181_S590child1_trait181));
  return impl;
} 
Trait181* Trait181_S590child1_ = newTrait181_S590child1();

int S591_classId = Class_genId();
struct S591{

  int id;
  S591 ():id(S591_classId){

  }
};


int S591child1_classId = Class_genId();
struct S591child1:S591{

  S591child1 (int a):a(a){
    (id = S591child1_classId);
{

    }
  }
  int a;
};


int S591child2_classId = Class_genId();
struct S591child2:S591{

  S591child2 (int a):a(a){
    (id = S591child2_classId);
{

    }
  }
  int a;
};


int S591child3_classId = Class_genId();
struct S591child3:S591{

  S591child3 (int a):a(a){
    (id = S591child3_classId);
{

    }
  }
  int a;
};


struct Trait591{

  int(*trait591)(Class*);
};

Vec* Trait591_v = newVec();

int Trait591_S591child1_trait591(Class* self_) {
  S591child1* self = ((S591child1*)self_);
{
    return (self -> a);
  }
} 
Trait591* newTrait591_S591child1() {
  Trait591 (* impl) = (new Trait591());
  setVec(Trait591_v, S591child1_classId, ((void*)impl));
  ((impl -> trait591) = (& Trait591_S591child1_trait591));
  return impl;
} 
Trait591* Trait591_S591child1_ = newTrait591_S591child1();

int Trait272_S591child1_trait272(Class* self_) {
  S591child1* self = ((S591child1*)self_);
{
    return (self -> a);
  }
} 
Trait272* newTrait272_S591child1() {
  Trait272 (* impl) = (new Trait272());
  setVec(Trait272_v, S591child1_classId, ((void*)impl));
  ((impl -> trait272) = (& Trait272_S591child1_trait272));
  return impl;
} 
Trait272* Trait272_S591child1_ = newTrait272_S591child1();

int S592_classId = Class_genId();
struct S592{

  int id;
  S592 ():id(S592_classId){

  }
};


int S592child1_classId = Class_genId();
struct S592child1:S592{

  S592child1 (int a):a(a){
    (id = S592child1_classId);
{

    }
  }
  int a;
};


int S592child2_classId = Class_genId();
struct S592child2:S592{

  S592child2 (int a):a(a){
    (id = S592child2_classId);
{

    }
  }
  int a;
};


int S592child3_classId = Class_genId();
struct S592child3:S592{

  S592child3 (int a):a(a){
    (id = S592child3_classId);
{

    }
  }
  int a;
};


struct Trait592{

  int(*trait592)(Class*);
};

Vec* Trait592_v = newVec();

int Trait592_S592child1_trait592(Class* self_) {
  S592child1* self = ((S592child1*)self_);
{
    return (self -> a);
  }
} 
Trait592* newTrait592_S592child1() {
  Trait592 (* impl) = (new Trait592());
  setVec(Trait592_v, S592child1_classId, ((void*)impl));
  ((impl -> trait592) = (& Trait592_S592child1_trait592));
  return impl;
} 
Trait592* Trait592_S592child1_ = newTrait592_S592child1();

int Trait364_S592child1_trait364(Class* self_) {
  S592child1* self = ((S592child1*)self_);
{
    return (self -> a);
  }
} 
Trait364* newTrait364_S592child1() {
  Trait364 (* impl) = (new Trait364());
  setVec(Trait364_v, S592child1_classId, ((void*)impl));
  ((impl -> trait364) = (& Trait364_S592child1_trait364));
  return impl;
} 
Trait364* Trait364_S592child1_ = newTrait364_S592child1();

int S593_classId = Class_genId();
struct S593{

  int id;
  S593 ():id(S593_classId){

  }
};


int S593child1_classId = Class_genId();
struct S593child1:S593{

  S593child1 (int a):a(a){
    (id = S593child1_classId);
{

    }
  }
  int a;
};


int S593child2_classId = Class_genId();
struct S593child2:S593{

  S593child2 (int a):a(a){
    (id = S593child2_classId);
{

    }
  }
  int a;
};


int S593child3_classId = Class_genId();
struct S593child3:S593{

  S593child3 (int a):a(a){
    (id = S593child3_classId);
{

    }
  }
  int a;
};


struct Trait593{

  int(*trait593)(Class*);
};

Vec* Trait593_v = newVec();

int Trait593_S593child1_trait593(Class* self_) {
  S593child1* self = ((S593child1*)self_);
{
    return (self -> a);
  }
} 
Trait593* newTrait593_S593child1() {
  Trait593 (* impl) = (new Trait593());
  setVec(Trait593_v, S593child1_classId, ((void*)impl));
  ((impl -> trait593) = (& Trait593_S593child1_trait593));
  return impl;
} 
Trait593* Trait593_S593child1_ = newTrait593_S593child1();

int Trait593_S593child2_trait593(Class* self_) {
  S593child2* self = ((S593child2*)self_);
{
    return (self -> a);
  }
} 
Trait593* newTrait593_S593child2() {
  Trait593 (* impl) = (new Trait593());
  setVec(Trait593_v, S593child2_classId, ((void*)impl));
  ((impl -> trait593) = (& Trait593_S593child2_trait593));
  return impl;
} 
Trait593* Trait593_S593child2_ = newTrait593_S593child2();

int Trait593_S593child3_trait593(Class* self_) {
  S593child3* self = ((S593child3*)self_);
{
    return (self -> a);
  }
} 
Trait593* newTrait593_S593child3() {
  Trait593 (* impl) = (new Trait593());
  setVec(Trait593_v, S593child3_classId, ((void*)impl));
  ((impl -> trait593) = (& Trait593_S593child3_trait593));
  return impl;
} 
Trait593* Trait593_S593child3_ = newTrait593_S593child3();

int Trait531_S593child1_trait531(Class* self_) {
  S593child1* self = ((S593child1*)self_);
{
    return (self -> a);
  }
} 
Trait531* newTrait531_S593child1() {
  Trait531 (* impl) = (new Trait531());
  setVec(Trait531_v, S593child1_classId, ((void*)impl));
  ((impl -> trait531) = (& Trait531_S593child1_trait531));
  return impl;
} 
Trait531* Trait531_S593child1_ = newTrait531_S593child1();

int S594_classId = Class_genId();
struct S594{

  int id;
  S594 ():id(S594_classId){

  }
};


int S594child1_classId = Class_genId();
struct S594child1:S594{

  S594child1 (int a):a(a){
    (id = S594child1_classId);
{

    }
  }
  int a;
};


int S594child2_classId = Class_genId();
struct S594child2:S594{

  S594child2 (int a):a(a){
    (id = S594child2_classId);
{

    }
  }
  int a;
};


struct Trait594{

  int(*trait594)(Class*);
};

Vec* Trait594_v = newVec();

int Trait594_S594child1_trait594(Class* self_) {
  S594child1* self = ((S594child1*)self_);
{
    return (self -> a);
  }
} 
Trait594* newTrait594_S594child1() {
  Trait594 (* impl) = (new Trait594());
  setVec(Trait594_v, S594child1_classId, ((void*)impl));
  ((impl -> trait594) = (& Trait594_S594child1_trait594));
  return impl;
} 
Trait594* Trait594_S594child1_ = newTrait594_S594child1();

int Trait594_S594child2_trait594(Class* self_) {
  S594child2* self = ((S594child2*)self_);
{
    return (self -> a);
  }
} 
Trait594* newTrait594_S594child2() {
  Trait594 (* impl) = (new Trait594());
  setVec(Trait594_v, S594child2_classId, ((void*)impl));
  ((impl -> trait594) = (& Trait594_S594child2_trait594));
  return impl;
} 
Trait594* Trait594_S594child2_ = newTrait594_S594child2();

int Trait413_S594child1_trait413(Class* self_) {
  S594child1* self = ((S594child1*)self_);
{
    return (self -> a);
  }
} 
Trait413* newTrait413_S594child1() {
  Trait413 (* impl) = (new Trait413());
  setVec(Trait413_v, S594child1_classId, ((void*)impl));
  ((impl -> trait413) = (& Trait413_S594child1_trait413));
  return impl;
} 
Trait413* Trait413_S594child1_ = newTrait413_S594child1();

int S595_classId = Class_genId();
struct S595{

  int id;
  S595 ():id(S595_classId){

  }
};


int S595child1_classId = Class_genId();
struct S595child1:S595{

  S595child1 (int a):a(a){
    (id = S595child1_classId);
{

    }
  }
  int a;
};


int S595child2_classId = Class_genId();
struct S595child2:S595{

  S595child2 (int a):a(a){
    (id = S595child2_classId);
{

    }
  }
  int a;
};


int S595child3_classId = Class_genId();
struct S595child3:S595{

  S595child3 (int a):a(a){
    (id = S595child3_classId);
{

    }
  }
  int a;
};


int S595child4_classId = Class_genId();
struct S595child4:S595{

  S595child4 (int a):a(a){
    (id = S595child4_classId);
{

    }
  }
  int a;
};


struct Trait595{

  int(*trait595)(Class*);
};

Vec* Trait595_v = newVec();

int Trait595_S595child1_trait595(Class* self_) {
  S595child1* self = ((S595child1*)self_);
{
    return (self -> a);
  }
} 
Trait595* newTrait595_S595child1() {
  Trait595 (* impl) = (new Trait595());
  setVec(Trait595_v, S595child1_classId, ((void*)impl));
  ((impl -> trait595) = (& Trait595_S595child1_trait595));
  return impl;
} 
Trait595* Trait595_S595child1_ = newTrait595_S595child1();

int Trait308_S595child1_trait308(Class* self_) {
  S595child1* self = ((S595child1*)self_);
{
    return (self -> a);
  }
} 
Trait308* newTrait308_S595child1() {
  Trait308 (* impl) = (new Trait308());
  setVec(Trait308_v, S595child1_classId, ((void*)impl));
  ((impl -> trait308) = (& Trait308_S595child1_trait308));
  return impl;
} 
Trait308* Trait308_S595child1_ = newTrait308_S595child1();

int S596_classId = Class_genId();
struct S596{

  int id;
  S596 ():id(S596_classId){

  }
};


int S596child1_classId = Class_genId();
struct S596child1:S596{

  S596child1 (int a):a(a){
    (id = S596child1_classId);
{

    }
  }
  int a;
};


int S596child2_classId = Class_genId();
struct S596child2:S596{

  S596child2 (int a):a(a){
    (id = S596child2_classId);
{

    }
  }
  int a;
};


struct Trait596{

  int(*trait596)(Class*);
};

Vec* Trait596_v = newVec();

int Trait596_S596child1_trait596(Class* self_) {
  S596child1* self = ((S596child1*)self_);
{
    return (self -> a);
  }
} 
Trait596* newTrait596_S596child1() {
  Trait596 (* impl) = (new Trait596());
  setVec(Trait596_v, S596child1_classId, ((void*)impl));
  ((impl -> trait596) = (& Trait596_S596child1_trait596));
  return impl;
} 
Trait596* Trait596_S596child1_ = newTrait596_S596child1();

int Trait147_S596child1_trait147(Class* self_) {
  S596child1* self = ((S596child1*)self_);
{
    return (self -> a);
  }
} 
Trait147* newTrait147_S596child1() {
  Trait147 (* impl) = (new Trait147());
  setVec(Trait147_v, S596child1_classId, ((void*)impl));
  ((impl -> trait147) = (& Trait147_S596child1_trait147));
  return impl;
} 
Trait147* Trait147_S596child1_ = newTrait147_S596child1();

int S597_classId = Class_genId();
struct S597{

  int id;
  S597 ():id(S597_classId){

  }
};


int S597child1_classId = Class_genId();
struct S597child1:S597{

  S597child1 (int a):a(a){
    (id = S597child1_classId);
{

    }
  }
  int a;
};


int S597child2_classId = Class_genId();
struct S597child2:S597{

  S597child2 (int a):a(a){
    (id = S597child2_classId);
{

    }
  }
  int a;
};


int S597child3_classId = Class_genId();
struct S597child3:S597{

  S597child3 (int a):a(a){
    (id = S597child3_classId);
{

    }
  }
  int a;
};


int S597child4_classId = Class_genId();
struct S597child4:S597{

  S597child4 (int a):a(a){
    (id = S597child4_classId);
{

    }
  }
  int a;
};


struct Trait597{

  int(*trait597)(Class*);
};

Vec* Trait597_v = newVec();

int Trait597_S597child1_trait597(Class* self_) {
  S597child1* self = ((S597child1*)self_);
{
    return (self -> a);
  }
} 
Trait597* newTrait597_S597child1() {
  Trait597 (* impl) = (new Trait597());
  setVec(Trait597_v, S597child1_classId, ((void*)impl));
  ((impl -> trait597) = (& Trait597_S597child1_trait597));
  return impl;
} 
Trait597* Trait597_S597child1_ = newTrait597_S597child1();

int Trait372_S597child1_trait372(Class* self_) {
  S597child1* self = ((S597child1*)self_);
{
    return (self -> a);
  }
} 
Trait372* newTrait372_S597child1() {
  Trait372 (* impl) = (new Trait372());
  setVec(Trait372_v, S597child1_classId, ((void*)impl));
  ((impl -> trait372) = (& Trait372_S597child1_trait372));
  return impl;
} 
Trait372* Trait372_S597child1_ = newTrait372_S597child1();

int S598_classId = Class_genId();
struct S598{

  int id;
  S598 ():id(S598_classId){

  }
};


int S598child1_classId = Class_genId();
struct S598child1:S598{

  S598child1 (int a):a(a){
    (id = S598child1_classId);
{

    }
  }
  int a;
};


struct Trait598{

  int(*trait598)(Class*);
};

Vec* Trait598_v = newVec();

int Trait598_S598child1_trait598(Class* self_) {
  S598child1* self = ((S598child1*)self_);
{
    return (self -> a);
  }
} 
Trait598* newTrait598_S598child1() {
  Trait598 (* impl) = (new Trait598());
  setVec(Trait598_v, S598child1_classId, ((void*)impl));
  ((impl -> trait598) = (& Trait598_S598child1_trait598));
  return impl;
} 
Trait598* Trait598_S598child1_ = newTrait598_S598child1();

int Trait102_S598child1_trait102(Class* self_) {
  S598child1* self = ((S598child1*)self_);
{
    return (self -> a);
  }
} 
Trait102* newTrait102_S598child1() {
  Trait102 (* impl) = (new Trait102());
  setVec(Trait102_v, S598child1_classId, ((void*)impl));
  ((impl -> trait102) = (& Trait102_S598child1_trait102));
  return impl;
} 
Trait102* Trait102_S598child1_ = newTrait102_S598child1();

int S599_classId = Class_genId();
struct S599{

  int id;
  S599 ():id(S599_classId){

  }
};


int S599child1_classId = Class_genId();
struct S599child1:S599{

  S599child1 (int a):a(a){
    (id = S599child1_classId);
{

    }
  }
  int a;
};


int S599child2_classId = Class_genId();
struct S599child2:S599{

  S599child2 (int a):a(a){
    (id = S599child2_classId);
{

    }
  }
  int a;
};


struct Trait599{

  int(*trait599)(Class*);
};

Vec* Trait599_v = newVec();

int Trait599_S599child1_trait599(Class* self_) {
  S599child1* self = ((S599child1*)self_);
{
    return (self -> a);
  }
} 
Trait599* newTrait599_S599child1() {
  Trait599 (* impl) = (new Trait599());
  setVec(Trait599_v, S599child1_classId, ((void*)impl));
  ((impl -> trait599) = (& Trait599_S599child1_trait599));
  return impl;
} 
Trait599* Trait599_S599child1_ = newTrait599_S599child1();

int Trait306_S599child1_trait306(Class* self_) {
  S599child1* self = ((S599child1*)self_);
{
    return (self -> a);
  }
} 
Trait306* newTrait306_S599child1() {
  Trait306 (* impl) = (new Trait306());
  setVec(Trait306_v, S599child1_classId, ((void*)impl));
  ((impl -> trait306) = (& Trait306_S599child1_trait306));
  return impl;
} 
Trait306* Trait306_S599child1_ = newTrait306_S599child1();

int S600_classId = Class_genId();
struct S600{

  int id;
  S600 ():id(S600_classId){

  }
};


int S600child1_classId = Class_genId();
struct S600child1:S600{

  S600child1 (int a):a(a){
    (id = S600child1_classId);
{

    }
  }
  int a;
};


int S600child2_classId = Class_genId();
struct S600child2:S600{

  S600child2 (int a):a(a){
    (id = S600child2_classId);
{

    }
  }
  int a;
};


int S600child3_classId = Class_genId();
struct S600child3:S600{

  S600child3 (int a):a(a){
    (id = S600child3_classId);
{

    }
  }
  int a;
};


struct Trait600{

  int(*trait600)(Class*);
};

Vec* Trait600_v = newVec();

int Trait600_S600child1_trait600(Class* self_) {
  S600child1* self = ((S600child1*)self_);
{
    return (self -> a);
  }
} 
Trait600* newTrait600_S600child1() {
  Trait600 (* impl) = (new Trait600());
  setVec(Trait600_v, S600child1_classId, ((void*)impl));
  ((impl -> trait600) = (& Trait600_S600child1_trait600));
  return impl;
} 
Trait600* Trait600_S600child1_ = newTrait600_S600child1();

int Trait416_S600child1_trait416(Class* self_) {
  S600child1* self = ((S600child1*)self_);
{
    return (self -> a);
  }
} 
Trait416* newTrait416_S600child1() {
  Trait416 (* impl) = (new Trait416());
  setVec(Trait416_v, S600child1_classId, ((void*)impl));
  ((impl -> trait416) = (& Trait416_S600child1_trait416));
  return impl;
} 
Trait416* Trait416_S600child1_ = newTrait416_S600child1();

int S601_classId = Class_genId();
struct S601{

  int id;
  S601 ():id(S601_classId){

  }
};


int S601child1_classId = Class_genId();
struct S601child1:S601{

  S601child1 (int a):a(a){
    (id = S601child1_classId);
{

    }
  }
  int a;
};


struct Trait601{

  int(*trait601)(Class*);
};

Vec* Trait601_v = newVec();

int Trait601_S601child1_trait601(Class* self_) {
  S601child1* self = ((S601child1*)self_);
{
    return (self -> a);
  }
} 
Trait601* newTrait601_S601child1() {
  Trait601 (* impl) = (new Trait601());
  setVec(Trait601_v, S601child1_classId, ((void*)impl));
  ((impl -> trait601) = (& Trait601_S601child1_trait601));
  return impl;
} 
Trait601* Trait601_S601child1_ = newTrait601_S601child1();

int Trait207_S601child1_trait207(Class* self_) {
  S601child1* self = ((S601child1*)self_);
{
    return (self -> a);
  }
} 
Trait207* newTrait207_S601child1() {
  Trait207 (* impl) = (new Trait207());
  setVec(Trait207_v, S601child1_classId, ((void*)impl));
  ((impl -> trait207) = (& Trait207_S601child1_trait207));
  return impl;
} 
Trait207* Trait207_S601child1_ = newTrait207_S601child1();

int S602_classId = Class_genId();
struct S602{

  int id;
  S602 ():id(S602_classId){

  }
};


int S602child1_classId = Class_genId();
struct S602child1:S602{

  S602child1 (int a):a(a){
    (id = S602child1_classId);
{

    }
  }
  int a;
};


int S602child2_classId = Class_genId();
struct S602child2:S602{

  S602child2 (int a):a(a){
    (id = S602child2_classId);
{

    }
  }
  int a;
};


int S602child3_classId = Class_genId();
struct S602child3:S602{

  S602child3 (int a):a(a){
    (id = S602child3_classId);
{

    }
  }
  int a;
};


int S602child4_classId = Class_genId();
struct S602child4:S602{

  S602child4 (int a):a(a){
    (id = S602child4_classId);
{

    }
  }
  int a;
};


struct Trait602{

  int(*trait602)(Class*);
};

Vec* Trait602_v = newVec();

int Trait602_S602child1_trait602(Class* self_) {
  S602child1* self = ((S602child1*)self_);
{
    return (self -> a);
  }
} 
Trait602* newTrait602_S602child1() {
  Trait602 (* impl) = (new Trait602());
  setVec(Trait602_v, S602child1_classId, ((void*)impl));
  ((impl -> trait602) = (& Trait602_S602child1_trait602));
  return impl;
} 
Trait602* Trait602_S602child1_ = newTrait602_S602child1();

int Trait602_S602child2_trait602(Class* self_) {
  S602child2* self = ((S602child2*)self_);
{
    return (self -> a);
  }
} 
Trait602* newTrait602_S602child2() {
  Trait602 (* impl) = (new Trait602());
  setVec(Trait602_v, S602child2_classId, ((void*)impl));
  ((impl -> trait602) = (& Trait602_S602child2_trait602));
  return impl;
} 
Trait602* Trait602_S602child2_ = newTrait602_S602child2();

int Trait602_S602child3_trait602(Class* self_) {
  S602child3* self = ((S602child3*)self_);
{
    return (self -> a);
  }
} 
Trait602* newTrait602_S602child3() {
  Trait602 (* impl) = (new Trait602());
  setVec(Trait602_v, S602child3_classId, ((void*)impl));
  ((impl -> trait602) = (& Trait602_S602child3_trait602));
  return impl;
} 
Trait602* Trait602_S602child3_ = newTrait602_S602child3();

int Trait540_S602child1_trait540(Class* self_) {
  S602child1* self = ((S602child1*)self_);
{
    return (self -> a);
  }
} 
Trait540* newTrait540_S602child1() {
  Trait540 (* impl) = (new Trait540());
  setVec(Trait540_v, S602child1_classId, ((void*)impl));
  ((impl -> trait540) = (& Trait540_S602child1_trait540));
  return impl;
} 
Trait540* Trait540_S602child1_ = newTrait540_S602child1();

int S603_classId = Class_genId();
struct S603{

  int id;
  S603 ():id(S603_classId){

  }
};


int S603child1_classId = Class_genId();
struct S603child1:S603{

  S603child1 (int a):a(a){
    (id = S603child1_classId);
{

    }
  }
  int a;
};


int S603child2_classId = Class_genId();
struct S603child2:S603{

  S603child2 (int a):a(a){
    (id = S603child2_classId);
{

    }
  }
  int a;
};


struct Trait603{

  int(*trait603)(Class*);
};

Vec* Trait603_v = newVec();

int Trait603_S603child1_trait603(Class* self_) {
  S603child1* self = ((S603child1*)self_);
{
    return (self -> a);
  }
} 
Trait603* newTrait603_S603child1() {
  Trait603 (* impl) = (new Trait603());
  setVec(Trait603_v, S603child1_classId, ((void*)impl));
  ((impl -> trait603) = (& Trait603_S603child1_trait603));
  return impl;
} 
Trait603* Trait603_S603child1_ = newTrait603_S603child1();

int Trait67_S603child1_trait67(Class* self_) {
  S603child1* self = ((S603child1*)self_);
{
    return (self -> a);
  }
} 
Trait67* newTrait67_S603child1() {
  Trait67 (* impl) = (new Trait67());
  setVec(Trait67_v, S603child1_classId, ((void*)impl));
  ((impl -> trait67) = (& Trait67_S603child1_trait67));
  return impl;
} 
Trait67* Trait67_S603child1_ = newTrait67_S603child1();

int S604_classId = Class_genId();
struct S604{

  int id;
  S604 ():id(S604_classId){

  }
};


int S604child1_classId = Class_genId();
struct S604child1:S604{

  S604child1 (int a):a(a){
    (id = S604child1_classId);
{

    }
  }
  int a;
};


struct Trait604{

  int(*trait604)(Class*);
};

Vec* Trait604_v = newVec();

int Trait604_S604child1_trait604(Class* self_) {
  S604child1* self = ((S604child1*)self_);
{
    return (self -> a);
  }
} 
Trait604* newTrait604_S604child1() {
  Trait604 (* impl) = (new Trait604());
  setVec(Trait604_v, S604child1_classId, ((void*)impl));
  ((impl -> trait604) = (& Trait604_S604child1_trait604));
  return impl;
} 
Trait604* Trait604_S604child1_ = newTrait604_S604child1();

int Trait71_S604child1_trait71(Class* self_) {
  S604child1* self = ((S604child1*)self_);
{
    return (self -> a);
  }
} 
Trait71* newTrait71_S604child1() {
  Trait71 (* impl) = (new Trait71());
  setVec(Trait71_v, S604child1_classId, ((void*)impl));
  ((impl -> trait71) = (& Trait71_S604child1_trait71));
  return impl;
} 
Trait71* Trait71_S604child1_ = newTrait71_S604child1();

int S605_classId = Class_genId();
struct S605{

  int id;
  S605 ():id(S605_classId){

  }
};


int S605child1_classId = Class_genId();
struct S605child1:S605{

  S605child1 (int a):a(a){
    (id = S605child1_classId);
{

    }
  }
  int a;
};


int S605child2_classId = Class_genId();
struct S605child2:S605{

  S605child2 (int a):a(a){
    (id = S605child2_classId);
{

    }
  }
  int a;
};


struct Trait605{

  int(*trait605)(Class*);
};

Vec* Trait605_v = newVec();

int Trait605_S605child1_trait605(Class* self_) {
  S605child1* self = ((S605child1*)self_);
{
    return (self -> a);
  }
} 
Trait605* newTrait605_S605child1() {
  Trait605 (* impl) = (new Trait605());
  setVec(Trait605_v, S605child1_classId, ((void*)impl));
  ((impl -> trait605) = (& Trait605_S605child1_trait605));
  return impl;
} 
Trait605* Trait605_S605child1_ = newTrait605_S605child1();

int Trait501_S605child1_trait501(Class* self_) {
  S605child1* self = ((S605child1*)self_);
{
    return (self -> a);
  }
} 
Trait501* newTrait501_S605child1() {
  Trait501 (* impl) = (new Trait501());
  setVec(Trait501_v, S605child1_classId, ((void*)impl));
  ((impl -> trait501) = (& Trait501_S605child1_trait501));
  return impl;
} 
Trait501* Trait501_S605child1_ = newTrait501_S605child1();

int S606_classId = Class_genId();
struct S606{

  int id;
  S606 ():id(S606_classId){

  }
};


int S606child1_classId = Class_genId();
struct S606child1:S606{

  S606child1 (int a):a(a){
    (id = S606child1_classId);
{

    }
  }
  int a;
};


int S606child2_classId = Class_genId();
struct S606child2:S606{

  S606child2 (int a):a(a){
    (id = S606child2_classId);
{

    }
  }
  int a;
};


int S606child3_classId = Class_genId();
struct S606child3:S606{

  S606child3 (int a):a(a){
    (id = S606child3_classId);
{

    }
  }
  int a;
};


int S606child4_classId = Class_genId();
struct S606child4:S606{

  S606child4 (int a):a(a){
    (id = S606child4_classId);
{

    }
  }
  int a;
};


struct Trait606{

  int(*trait606)(Class*);
};

Vec* Trait606_v = newVec();

int Trait606_S606child1_trait606(Class* self_) {
  S606child1* self = ((S606child1*)self_);
{
    return (self -> a);
  }
} 
Trait606* newTrait606_S606child1() {
  Trait606 (* impl) = (new Trait606());
  setVec(Trait606_v, S606child1_classId, ((void*)impl));
  ((impl -> trait606) = (& Trait606_S606child1_trait606));
  return impl;
} 
Trait606* Trait606_S606child1_ = newTrait606_S606child1();

int Trait606_S606child2_trait606(Class* self_) {
  S606child2* self = ((S606child2*)self_);
{
    return (self -> a);
  }
} 
Trait606* newTrait606_S606child2() {
  Trait606 (* impl) = (new Trait606());
  setVec(Trait606_v, S606child2_classId, ((void*)impl));
  ((impl -> trait606) = (& Trait606_S606child2_trait606));
  return impl;
} 
Trait606* Trait606_S606child2_ = newTrait606_S606child2();

int Trait258_S606child1_trait258(Class* self_) {
  S606child1* self = ((S606child1*)self_);
{
    return (self -> a);
  }
} 
Trait258* newTrait258_S606child1() {
  Trait258 (* impl) = (new Trait258());
  setVec(Trait258_v, S606child1_classId, ((void*)impl));
  ((impl -> trait258) = (& Trait258_S606child1_trait258));
  return impl;
} 
Trait258* Trait258_S606child1_ = newTrait258_S606child1();

int S607_classId = Class_genId();
struct S607{

  int id;
  S607 ():id(S607_classId){

  }
};


int S607child1_classId = Class_genId();
struct S607child1:S607{

  S607child1 (int a):a(a){
    (id = S607child1_classId);
{

    }
  }
  int a;
};


int S607child2_classId = Class_genId();
struct S607child2:S607{

  S607child2 (int a):a(a){
    (id = S607child2_classId);
{

    }
  }
  int a;
};


struct Trait607{

  int(*trait607)(Class*);
};

Vec* Trait607_v = newVec();

int Trait607_S607child1_trait607(Class* self_) {
  S607child1* self = ((S607child1*)self_);
{
    return (self -> a);
  }
} 
Trait607* newTrait607_S607child1() {
  Trait607 (* impl) = (new Trait607());
  setVec(Trait607_v, S607child1_classId, ((void*)impl));
  ((impl -> trait607) = (& Trait607_S607child1_trait607));
  return impl;
} 
Trait607* Trait607_S607child1_ = newTrait607_S607child1();

int Trait607_S607child2_trait607(Class* self_) {
  S607child2* self = ((S607child2*)self_);
{
    return (self -> a);
  }
} 
Trait607* newTrait607_S607child2() {
  Trait607 (* impl) = (new Trait607());
  setVec(Trait607_v, S607child2_classId, ((void*)impl));
  ((impl -> trait607) = (& Trait607_S607child2_trait607));
  return impl;
} 
Trait607* Trait607_S607child2_ = newTrait607_S607child2();

int Trait301_S607child1_trait301(Class* self_) {
  S607child1* self = ((S607child1*)self_);
{
    return (self -> a);
  }
} 
Trait301* newTrait301_S607child1() {
  Trait301 (* impl) = (new Trait301());
  setVec(Trait301_v, S607child1_classId, ((void*)impl));
  ((impl -> trait301) = (& Trait301_S607child1_trait301));
  return impl;
} 
Trait301* Trait301_S607child1_ = newTrait301_S607child1();

int S608_classId = Class_genId();
struct S608{

  int id;
  S608 ():id(S608_classId){

  }
};


int S608child1_classId = Class_genId();
struct S608child1:S608{

  S608child1 (int a):a(a){
    (id = S608child1_classId);
{

    }
  }
  int a;
};


struct Trait608{

  int(*trait608)(Class*);
};

Vec* Trait608_v = newVec();

int Trait608_S608child1_trait608(Class* self_) {
  S608child1* self = ((S608child1*)self_);
{
    return (self -> a);
  }
} 
Trait608* newTrait608_S608child1() {
  Trait608 (* impl) = (new Trait608());
  setVec(Trait608_v, S608child1_classId, ((void*)impl));
  ((impl -> trait608) = (& Trait608_S608child1_trait608));
  return impl;
} 
Trait608* Trait608_S608child1_ = newTrait608_S608child1();

int Trait586_S608child1_trait586(Class* self_) {
  S608child1* self = ((S608child1*)self_);
{
    return (self -> a);
  }
} 
Trait586* newTrait586_S608child1() {
  Trait586 (* impl) = (new Trait586());
  setVec(Trait586_v, S608child1_classId, ((void*)impl));
  ((impl -> trait586) = (& Trait586_S608child1_trait586));
  return impl;
} 
Trait586* Trait586_S608child1_ = newTrait586_S608child1();

int S609_classId = Class_genId();
struct S609{

  int id;
  S609 ():id(S609_classId){

  }
};


int S609child1_classId = Class_genId();
struct S609child1:S609{

  S609child1 (int a):a(a){
    (id = S609child1_classId);
{

    }
  }
  int a;
};


int S609child2_classId = Class_genId();
struct S609child2:S609{

  S609child2 (int a):a(a){
    (id = S609child2_classId);
{

    }
  }
  int a;
};


struct Trait609{

  int(*trait609)(Class*);
};

Vec* Trait609_v = newVec();

int Trait609_S609child1_trait609(Class* self_) {
  S609child1* self = ((S609child1*)self_);
{
    return (self -> a);
  }
} 
Trait609* newTrait609_S609child1() {
  Trait609 (* impl) = (new Trait609());
  setVec(Trait609_v, S609child1_classId, ((void*)impl));
  ((impl -> trait609) = (& Trait609_S609child1_trait609));
  return impl;
} 
Trait609* Trait609_S609child1_ = newTrait609_S609child1();

int Trait609_S609child2_trait609(Class* self_) {
  S609child2* self = ((S609child2*)self_);
{
    return (self -> a);
  }
} 
Trait609* newTrait609_S609child2() {
  Trait609 (* impl) = (new Trait609());
  setVec(Trait609_v, S609child2_classId, ((void*)impl));
  ((impl -> trait609) = (& Trait609_S609child2_trait609));
  return impl;
} 
Trait609* Trait609_S609child2_ = newTrait609_S609child2();

int Trait584_S609child1_trait584(Class* self_) {
  S609child1* self = ((S609child1*)self_);
{
    return (self -> a);
  }
} 
Trait584* newTrait584_S609child1() {
  Trait584 (* impl) = (new Trait584());
  setVec(Trait584_v, S609child1_classId, ((void*)impl));
  ((impl -> trait584) = (& Trait584_S609child1_trait584));
  return impl;
} 
Trait584* Trait584_S609child1_ = newTrait584_S609child1();

int S610_classId = Class_genId();
struct S610{

  int id;
  S610 ():id(S610_classId){

  }
};


int S610child1_classId = Class_genId();
struct S610child1:S610{

  S610child1 (int a):a(a){
    (id = S610child1_classId);
{

    }
  }
  int a;
};


struct Trait610{

  int(*trait610)(Class*);
};

Vec* Trait610_v = newVec();

int Trait610_S610child1_trait610(Class* self_) {
  S610child1* self = ((S610child1*)self_);
{
    return (self -> a);
  }
} 
Trait610* newTrait610_S610child1() {
  Trait610 (* impl) = (new Trait610());
  setVec(Trait610_v, S610child1_classId, ((void*)impl));
  ((impl -> trait610) = (& Trait610_S610child1_trait610));
  return impl;
} 
Trait610* Trait610_S610child1_ = newTrait610_S610child1();

int Trait272_S610child1_trait272(Class* self_) {
  S610child1* self = ((S610child1*)self_);
{
    return (self -> a);
  }
} 
Trait272* newTrait272_S610child1() {
  Trait272 (* impl) = (new Trait272());
  setVec(Trait272_v, S610child1_classId, ((void*)impl));
  ((impl -> trait272) = (& Trait272_S610child1_trait272));
  return impl;
} 
Trait272* Trait272_S610child1_ = newTrait272_S610child1();

int S611_classId = Class_genId();
struct S611{

  int id;
  S611 ():id(S611_classId){

  }
};


int S611child1_classId = Class_genId();
struct S611child1:S611{

  S611child1 (int a):a(a){
    (id = S611child1_classId);
{

    }
  }
  int a;
};


struct Trait611{

  int(*trait611)(Class*);
};

Vec* Trait611_v = newVec();

int Trait611_S611child1_trait611(Class* self_) {
  S611child1* self = ((S611child1*)self_);
{
    return (self -> a);
  }
} 
Trait611* newTrait611_S611child1() {
  Trait611 (* impl) = (new Trait611());
  setVec(Trait611_v, S611child1_classId, ((void*)impl));
  ((impl -> trait611) = (& Trait611_S611child1_trait611));
  return impl;
} 
Trait611* Trait611_S611child1_ = newTrait611_S611child1();

int Trait413_S611child1_trait413(Class* self_) {
  S611child1* self = ((S611child1*)self_);
{
    return (self -> a);
  }
} 
Trait413* newTrait413_S611child1() {
  Trait413 (* impl) = (new Trait413());
  setVec(Trait413_v, S611child1_classId, ((void*)impl));
  ((impl -> trait413) = (& Trait413_S611child1_trait413));
  return impl;
} 
Trait413* Trait413_S611child1_ = newTrait413_S611child1();

int S612_classId = Class_genId();
struct S612{

  int id;
  S612 ():id(S612_classId){

  }
};


int S612child1_classId = Class_genId();
struct S612child1:S612{

  S612child1 (int a):a(a){
    (id = S612child1_classId);
{

    }
  }
  int a;
};


struct Trait612{

  int(*trait612)(Class*);
};

Vec* Trait612_v = newVec();

int Trait612_S612child1_trait612(Class* self_) {
  S612child1* self = ((S612child1*)self_);
{
    return (self -> a);
  }
} 
Trait612* newTrait612_S612child1() {
  Trait612 (* impl) = (new Trait612());
  setVec(Trait612_v, S612child1_classId, ((void*)impl));
  ((impl -> trait612) = (& Trait612_S612child1_trait612));
  return impl;
} 
Trait612* Trait612_S612child1_ = newTrait612_S612child1();

int Trait237_S612child1_trait237(Class* self_) {
  S612child1* self = ((S612child1*)self_);
{
    return (self -> a);
  }
} 
Trait237* newTrait237_S612child1() {
  Trait237 (* impl) = (new Trait237());
  setVec(Trait237_v, S612child1_classId, ((void*)impl));
  ((impl -> trait237) = (& Trait237_S612child1_trait237));
  return impl;
} 
Trait237* Trait237_S612child1_ = newTrait237_S612child1();

int S613_classId = Class_genId();
struct S613{

  int id;
  S613 ():id(S613_classId){

  }
};


int S613child1_classId = Class_genId();
struct S613child1:S613{

  S613child1 (int a):a(a){
    (id = S613child1_classId);
{

    }
  }
  int a;
};


int S613child2_classId = Class_genId();
struct S613child2:S613{

  S613child2 (int a):a(a){
    (id = S613child2_classId);
{

    }
  }
  int a;
};


struct Trait613{

  int(*trait613)(Class*);
};

Vec* Trait613_v = newVec();

int Trait613_S613child1_trait613(Class* self_) {
  S613child1* self = ((S613child1*)self_);
{
    return (self -> a);
  }
} 
Trait613* newTrait613_S613child1() {
  Trait613 (* impl) = (new Trait613());
  setVec(Trait613_v, S613child1_classId, ((void*)impl));
  ((impl -> trait613) = (& Trait613_S613child1_trait613));
  return impl;
} 
Trait613* Trait613_S613child1_ = newTrait613_S613child1();

int Trait65_S613child1_trait65(Class* self_) {
  S613child1* self = ((S613child1*)self_);
{
    return (self -> a);
  }
} 
Trait65* newTrait65_S613child1() {
  Trait65 (* impl) = (new Trait65());
  setVec(Trait65_v, S613child1_classId, ((void*)impl));
  ((impl -> trait65) = (& Trait65_S613child1_trait65));
  return impl;
} 
Trait65* Trait65_S613child1_ = newTrait65_S613child1();

int S614_classId = Class_genId();
struct S614{

  int id;
  S614 ():id(S614_classId){

  }
};


int S614child1_classId = Class_genId();
struct S614child1:S614{

  S614child1 (int a):a(a){
    (id = S614child1_classId);
{

    }
  }
  int a;
};


int S614child2_classId = Class_genId();
struct S614child2:S614{

  S614child2 (int a):a(a){
    (id = S614child2_classId);
{

    }
  }
  int a;
};


struct Trait614{

  int(*trait614)(Class*);
};

Vec* Trait614_v = newVec();

int Trait614_S614child1_trait614(Class* self_) {
  S614child1* self = ((S614child1*)self_);
{
    return (self -> a);
  }
} 
Trait614* newTrait614_S614child1() {
  Trait614 (* impl) = (new Trait614());
  setVec(Trait614_v, S614child1_classId, ((void*)impl));
  ((impl -> trait614) = (& Trait614_S614child1_trait614));
  return impl;
} 
Trait614* Trait614_S614child1_ = newTrait614_S614child1();

int Trait87_S614child1_trait87(Class* self_) {
  S614child1* self = ((S614child1*)self_);
{
    return (self -> a);
  }
} 
Trait87* newTrait87_S614child1() {
  Trait87 (* impl) = (new Trait87());
  setVec(Trait87_v, S614child1_classId, ((void*)impl));
  ((impl -> trait87) = (& Trait87_S614child1_trait87));
  return impl;
} 
Trait87* Trait87_S614child1_ = newTrait87_S614child1();

int S615_classId = Class_genId();
struct S615{

  int id;
  S615 ():id(S615_classId){

  }
};


int S615child1_classId = Class_genId();
struct S615child1:S615{

  S615child1 (int a):a(a){
    (id = S615child1_classId);
{

    }
  }
  int a;
};


int S615child2_classId = Class_genId();
struct S615child2:S615{

  S615child2 (int a):a(a){
    (id = S615child2_classId);
{

    }
  }
  int a;
};


int S615child3_classId = Class_genId();
struct S615child3:S615{

  S615child3 (int a):a(a){
    (id = S615child3_classId);
{

    }
  }
  int a;
};


struct Trait615{

  int(*trait615)(Class*);
};

Vec* Trait615_v = newVec();

int Trait615_S615child1_trait615(Class* self_) {
  S615child1* self = ((S615child1*)self_);
{
    return (self -> a);
  }
} 
Trait615* newTrait615_S615child1() {
  Trait615 (* impl) = (new Trait615());
  setVec(Trait615_v, S615child1_classId, ((void*)impl));
  ((impl -> trait615) = (& Trait615_S615child1_trait615));
  return impl;
} 
Trait615* Trait615_S615child1_ = newTrait615_S615child1();

int Trait615_S615child2_trait615(Class* self_) {
  S615child2* self = ((S615child2*)self_);
{
    return (self -> a);
  }
} 
Trait615* newTrait615_S615child2() {
  Trait615 (* impl) = (new Trait615());
  setVec(Trait615_v, S615child2_classId, ((void*)impl));
  ((impl -> trait615) = (& Trait615_S615child2_trait615));
  return impl;
} 
Trait615* Trait615_S615child2_ = newTrait615_S615child2();

int Trait208_S615child1_trait208(Class* self_) {
  S615child1* self = ((S615child1*)self_);
{
    return (self -> a);
  }
} 
Trait208* newTrait208_S615child1() {
  Trait208 (* impl) = (new Trait208());
  setVec(Trait208_v, S615child1_classId, ((void*)impl));
  ((impl -> trait208) = (& Trait208_S615child1_trait208));
  return impl;
} 
Trait208* Trait208_S615child1_ = newTrait208_S615child1();

int S616_classId = Class_genId();
struct S616{

  int id;
  S616 ():id(S616_classId){

  }
};


int S616child1_classId = Class_genId();
struct S616child1:S616{

  S616child1 (int a):a(a){
    (id = S616child1_classId);
{

    }
  }
  int a;
};


int S616child2_classId = Class_genId();
struct S616child2:S616{

  S616child2 (int a):a(a){
    (id = S616child2_classId);
{

    }
  }
  int a;
};


int S616child3_classId = Class_genId();
struct S616child3:S616{

  S616child3 (int a):a(a){
    (id = S616child3_classId);
{

    }
  }
  int a;
};


struct Trait616{

  int(*trait616)(Class*);
};

Vec* Trait616_v = newVec();

int Trait616_S616child1_trait616(Class* self_) {
  S616child1* self = ((S616child1*)self_);
{
    return (self -> a);
  }
} 
Trait616* newTrait616_S616child1() {
  Trait616 (* impl) = (new Trait616());
  setVec(Trait616_v, S616child1_classId, ((void*)impl));
  ((impl -> trait616) = (& Trait616_S616child1_trait616));
  return impl;
} 
Trait616* Trait616_S616child1_ = newTrait616_S616child1();

int Trait15_S616child1_trait15(Class* self_) {
  S616child1* self = ((S616child1*)self_);
{
    return (self -> a);
  }
} 
Trait15* newTrait15_S616child1() {
  Trait15 (* impl) = (new Trait15());
  setVec(Trait15_v, S616child1_classId, ((void*)impl));
  ((impl -> trait15) = (& Trait15_S616child1_trait15));
  return impl;
} 
Trait15* Trait15_S616child1_ = newTrait15_S616child1();

int S617_classId = Class_genId();
struct S617{

  int id;
  S617 ():id(S617_classId){

  }
};


int S617child1_classId = Class_genId();
struct S617child1:S617{

  S617child1 (int a):a(a){
    (id = S617child1_classId);
{

    }
  }
  int a;
};


int S617child2_classId = Class_genId();
struct S617child2:S617{

  S617child2 (int a):a(a){
    (id = S617child2_classId);
{

    }
  }
  int a;
};


int S617child3_classId = Class_genId();
struct S617child3:S617{

  S617child3 (int a):a(a){
    (id = S617child3_classId);
{

    }
  }
  int a;
};


int S617child4_classId = Class_genId();
struct S617child4:S617{

  S617child4 (int a):a(a){
    (id = S617child4_classId);
{

    }
  }
  int a;
};


struct Trait617{

  int(*trait617)(Class*);
};

Vec* Trait617_v = newVec();

int Trait617_S617child1_trait617(Class* self_) {
  S617child1* self = ((S617child1*)self_);
{
    return (self -> a);
  }
} 
Trait617* newTrait617_S617child1() {
  Trait617 (* impl) = (new Trait617());
  setVec(Trait617_v, S617child1_classId, ((void*)impl));
  ((impl -> trait617) = (& Trait617_S617child1_trait617));
  return impl;
} 
Trait617* Trait617_S617child1_ = newTrait617_S617child1();

int Trait617_S617child2_trait617(Class* self_) {
  S617child2* self = ((S617child2*)self_);
{
    return (self -> a);
  }
} 
Trait617* newTrait617_S617child2() {
  Trait617 (* impl) = (new Trait617());
  setVec(Trait617_v, S617child2_classId, ((void*)impl));
  ((impl -> trait617) = (& Trait617_S617child2_trait617));
  return impl;
} 
Trait617* Trait617_S617child2_ = newTrait617_S617child2();

int Trait617_S617child3_trait617(Class* self_) {
  S617child3* self = ((S617child3*)self_);
{
    return (self -> a);
  }
} 
Trait617* newTrait617_S617child3() {
  Trait617 (* impl) = (new Trait617());
  setVec(Trait617_v, S617child3_classId, ((void*)impl));
  ((impl -> trait617) = (& Trait617_S617child3_trait617));
  return impl;
} 
Trait617* Trait617_S617child3_ = newTrait617_S617child3();

int Trait617_S617child4_trait617(Class* self_) {
  S617child4* self = ((S617child4*)self_);
{
    return (self -> a);
  }
} 
Trait617* newTrait617_S617child4() {
  Trait617 (* impl) = (new Trait617());
  setVec(Trait617_v, S617child4_classId, ((void*)impl));
  ((impl -> trait617) = (& Trait617_S617child4_trait617));
  return impl;
} 
Trait617* Trait617_S617child4_ = newTrait617_S617child4();

int Trait298_S617child1_trait298(Class* self_) {
  S617child1* self = ((S617child1*)self_);
{
    return (self -> a);
  }
} 
Trait298* newTrait298_S617child1() {
  Trait298 (* impl) = (new Trait298());
  setVec(Trait298_v, S617child1_classId, ((void*)impl));
  ((impl -> trait298) = (& Trait298_S617child1_trait298));
  return impl;
} 
Trait298* Trait298_S617child1_ = newTrait298_S617child1();

int S618_classId = Class_genId();
struct S618{

  int id;
  S618 ():id(S618_classId){

  }
};


int S618child1_classId = Class_genId();
struct S618child1:S618{

  S618child1 (int a):a(a){
    (id = S618child1_classId);
{

    }
  }
  int a;
};


struct Trait618{

  int(*trait618)(Class*);
};

Vec* Trait618_v = newVec();

int Trait618_S618child1_trait618(Class* self_) {
  S618child1* self = ((S618child1*)self_);
{
    return (self -> a);
  }
} 
Trait618* newTrait618_S618child1() {
  Trait618 (* impl) = (new Trait618());
  setVec(Trait618_v, S618child1_classId, ((void*)impl));
  ((impl -> trait618) = (& Trait618_S618child1_trait618));
  return impl;
} 
Trait618* Trait618_S618child1_ = newTrait618_S618child1();

int Trait162_S618child1_trait162(Class* self_) {
  S618child1* self = ((S618child1*)self_);
{
    return (self -> a);
  }
} 
Trait162* newTrait162_S618child1() {
  Trait162 (* impl) = (new Trait162());
  setVec(Trait162_v, S618child1_classId, ((void*)impl));
  ((impl -> trait162) = (& Trait162_S618child1_trait162));
  return impl;
} 
Trait162* Trait162_S618child1_ = newTrait162_S618child1();

int S619_classId = Class_genId();
struct S619{

  int id;
  S619 ():id(S619_classId){

  }
};


int S619child1_classId = Class_genId();
struct S619child1:S619{

  S619child1 (int a):a(a){
    (id = S619child1_classId);
{

    }
  }
  int a;
};


struct Trait619{

  int(*trait619)(Class*);
};

Vec* Trait619_v = newVec();

int Trait619_S619child1_trait619(Class* self_) {
  S619child1* self = ((S619child1*)self_);
{
    return (self -> a);
  }
} 
Trait619* newTrait619_S619child1() {
  Trait619 (* impl) = (new Trait619());
  setVec(Trait619_v, S619child1_classId, ((void*)impl));
  ((impl -> trait619) = (& Trait619_S619child1_trait619));
  return impl;
} 
Trait619* Trait619_S619child1_ = newTrait619_S619child1();

int Trait612_S619child1_trait612(Class* self_) {
  S619child1* self = ((S619child1*)self_);
{
    return (self -> a);
  }
} 
Trait612* newTrait612_S619child1() {
  Trait612 (* impl) = (new Trait612());
  setVec(Trait612_v, S619child1_classId, ((void*)impl));
  ((impl -> trait612) = (& Trait612_S619child1_trait612));
  return impl;
} 
Trait612* Trait612_S619child1_ = newTrait612_S619child1();

int S620_classId = Class_genId();
struct S620{

  int id;
  S620 ():id(S620_classId){

  }
};


int S620child1_classId = Class_genId();
struct S620child1:S620{

  S620child1 (int a):a(a){
    (id = S620child1_classId);
{

    }
  }
  int a;
};


struct Trait620{

  int(*trait620)(Class*);
};

Vec* Trait620_v = newVec();

int Trait620_S620child1_trait620(Class* self_) {
  S620child1* self = ((S620child1*)self_);
{
    return (self -> a);
  }
} 
Trait620* newTrait620_S620child1() {
  Trait620 (* impl) = (new Trait620());
  setVec(Trait620_v, S620child1_classId, ((void*)impl));
  ((impl -> trait620) = (& Trait620_S620child1_trait620));
  return impl;
} 
Trait620* Trait620_S620child1_ = newTrait620_S620child1();

int Trait553_S620child1_trait553(Class* self_) {
  S620child1* self = ((S620child1*)self_);
{
    return (self -> a);
  }
} 
Trait553* newTrait553_S620child1() {
  Trait553 (* impl) = (new Trait553());
  setVec(Trait553_v, S620child1_classId, ((void*)impl));
  ((impl -> trait553) = (& Trait553_S620child1_trait553));
  return impl;
} 
Trait553* Trait553_S620child1_ = newTrait553_S620child1();

int S621_classId = Class_genId();
struct S621{

  int id;
  S621 ():id(S621_classId){

  }
};


int S621child1_classId = Class_genId();
struct S621child1:S621{

  S621child1 (int a):a(a){
    (id = S621child1_classId);
{

    }
  }
  int a;
};


int S621child2_classId = Class_genId();
struct S621child2:S621{

  S621child2 (int a):a(a){
    (id = S621child2_classId);
{

    }
  }
  int a;
};


struct Trait621{

  int(*trait621)(Class*);
};

Vec* Trait621_v = newVec();

int Trait621_S621child1_trait621(Class* self_) {
  S621child1* self = ((S621child1*)self_);
{
    return (self -> a);
  }
} 
Trait621* newTrait621_S621child1() {
  Trait621 (* impl) = (new Trait621());
  setVec(Trait621_v, S621child1_classId, ((void*)impl));
  ((impl -> trait621) = (& Trait621_S621child1_trait621));
  return impl;
} 
Trait621* Trait621_S621child1_ = newTrait621_S621child1();

int Trait621_S621child2_trait621(Class* self_) {
  S621child2* self = ((S621child2*)self_);
{
    return (self -> a);
  }
} 
Trait621* newTrait621_S621child2() {
  Trait621 (* impl) = (new Trait621());
  setVec(Trait621_v, S621child2_classId, ((void*)impl));
  ((impl -> trait621) = (& Trait621_S621child2_trait621));
  return impl;
} 
Trait621* Trait621_S621child2_ = newTrait621_S621child2();

int Trait404_S621child1_trait404(Class* self_) {
  S621child1* self = ((S621child1*)self_);
{
    return (self -> a);
  }
} 
Trait404* newTrait404_S621child1() {
  Trait404 (* impl) = (new Trait404());
  setVec(Trait404_v, S621child1_classId, ((void*)impl));
  ((impl -> trait404) = (& Trait404_S621child1_trait404));
  return impl;
} 
Trait404* Trait404_S621child1_ = newTrait404_S621child1();

int S622_classId = Class_genId();
struct S622{

  int id;
  S622 ():id(S622_classId){

  }
};


int S622child1_classId = Class_genId();
struct S622child1:S622{

  S622child1 (int a):a(a){
    (id = S622child1_classId);
{

    }
  }
  int a;
};


int S622child2_classId = Class_genId();
struct S622child2:S622{

  S622child2 (int a):a(a){
    (id = S622child2_classId);
{

    }
  }
  int a;
};


struct Trait622{

  int(*trait622)(Class*);
};

Vec* Trait622_v = newVec();

int Trait622_S622child1_trait622(Class* self_) {
  S622child1* self = ((S622child1*)self_);
{
    return (self -> a);
  }
} 
Trait622* newTrait622_S622child1() {
  Trait622 (* impl) = (new Trait622());
  setVec(Trait622_v, S622child1_classId, ((void*)impl));
  ((impl -> trait622) = (& Trait622_S622child1_trait622));
  return impl;
} 
Trait622* Trait622_S622child1_ = newTrait622_S622child1();

int Trait622_S622child2_trait622(Class* self_) {
  S622child2* self = ((S622child2*)self_);
{
    return (self -> a);
  }
} 
Trait622* newTrait622_S622child2() {
  Trait622 (* impl) = (new Trait622());
  setVec(Trait622_v, S622child2_classId, ((void*)impl));
  ((impl -> trait622) = (& Trait622_S622child2_trait622));
  return impl;
} 
Trait622* Trait622_S622child2_ = newTrait622_S622child2();

int Trait362_S622child1_trait362(Class* self_) {
  S622child1* self = ((S622child1*)self_);
{
    return (self -> a);
  }
} 
Trait362* newTrait362_S622child1() {
  Trait362 (* impl) = (new Trait362());
  setVec(Trait362_v, S622child1_classId, ((void*)impl));
  ((impl -> trait362) = (& Trait362_S622child1_trait362));
  return impl;
} 
Trait362* Trait362_S622child1_ = newTrait362_S622child1();

int S623_classId = Class_genId();
struct S623{

  int id;
  S623 ():id(S623_classId){

  }
};


int S623child1_classId = Class_genId();
struct S623child1:S623{

  S623child1 (int a):a(a){
    (id = S623child1_classId);
{

    }
  }
  int a;
};


int S623child2_classId = Class_genId();
struct S623child2:S623{

  S623child2 (int a):a(a){
    (id = S623child2_classId);
{

    }
  }
  int a;
};


int S623child3_classId = Class_genId();
struct S623child3:S623{

  S623child3 (int a):a(a){
    (id = S623child3_classId);
{

    }
  }
  int a;
};


int S623child4_classId = Class_genId();
struct S623child4:S623{

  S623child4 (int a):a(a){
    (id = S623child4_classId);
{

    }
  }
  int a;
};


struct Trait623{

  int(*trait623)(Class*);
};

Vec* Trait623_v = newVec();

int Trait623_S623child1_trait623(Class* self_) {
  S623child1* self = ((S623child1*)self_);
{
    return (self -> a);
  }
} 
Trait623* newTrait623_S623child1() {
  Trait623 (* impl) = (new Trait623());
  setVec(Trait623_v, S623child1_classId, ((void*)impl));
  ((impl -> trait623) = (& Trait623_S623child1_trait623));
  return impl;
} 
Trait623* Trait623_S623child1_ = newTrait623_S623child1();

int Trait514_S623child1_trait514(Class* self_) {
  S623child1* self = ((S623child1*)self_);
{
    return (self -> a);
  }
} 
Trait514* newTrait514_S623child1() {
  Trait514 (* impl) = (new Trait514());
  setVec(Trait514_v, S623child1_classId, ((void*)impl));
  ((impl -> trait514) = (& Trait514_S623child1_trait514));
  return impl;
} 
Trait514* Trait514_S623child1_ = newTrait514_S623child1();

int S624_classId = Class_genId();
struct S624{

  int id;
  S624 ():id(S624_classId){

  }
};


int S624child1_classId = Class_genId();
struct S624child1:S624{

  S624child1 (int a):a(a){
    (id = S624child1_classId);
{

    }
  }
  int a;
};


struct Trait624{

  int(*trait624)(Class*);
};

Vec* Trait624_v = newVec();

int Trait624_S624child1_trait624(Class* self_) {
  S624child1* self = ((S624child1*)self_);
{
    return (self -> a);
  }
} 
Trait624* newTrait624_S624child1() {
  Trait624 (* impl) = (new Trait624());
  setVec(Trait624_v, S624child1_classId, ((void*)impl));
  ((impl -> trait624) = (& Trait624_S624child1_trait624));
  return impl;
} 
Trait624* Trait624_S624child1_ = newTrait624_S624child1();

int Trait129_S624child1_trait129(Class* self_) {
  S624child1* self = ((S624child1*)self_);
{
    return (self -> a);
  }
} 
Trait129* newTrait129_S624child1() {
  Trait129 (* impl) = (new Trait129());
  setVec(Trait129_v, S624child1_classId, ((void*)impl));
  ((impl -> trait129) = (& Trait129_S624child1_trait129));
  return impl;
} 
Trait129* Trait129_S624child1_ = newTrait129_S624child1();

int S625_classId = Class_genId();
struct S625{

  int id;
  S625 ():id(S625_classId){

  }
};


int S625child1_classId = Class_genId();
struct S625child1:S625{

  S625child1 (int a):a(a){
    (id = S625child1_classId);
{

    }
  }
  int a;
};


int S625child2_classId = Class_genId();
struct S625child2:S625{

  S625child2 (int a):a(a){
    (id = S625child2_classId);
{

    }
  }
  int a;
};


int S625child3_classId = Class_genId();
struct S625child3:S625{

  S625child3 (int a):a(a){
    (id = S625child3_classId);
{

    }
  }
  int a;
};


int S625child4_classId = Class_genId();
struct S625child4:S625{

  S625child4 (int a):a(a){
    (id = S625child4_classId);
{

    }
  }
  int a;
};


struct Trait625{

  int(*trait625)(Class*);
};

Vec* Trait625_v = newVec();

int Trait625_S625child1_trait625(Class* self_) {
  S625child1* self = ((S625child1*)self_);
{
    return (self -> a);
  }
} 
Trait625* newTrait625_S625child1() {
  Trait625 (* impl) = (new Trait625());
  setVec(Trait625_v, S625child1_classId, ((void*)impl));
  ((impl -> trait625) = (& Trait625_S625child1_trait625));
  return impl;
} 
Trait625* Trait625_S625child1_ = newTrait625_S625child1();

int Trait158_S625child1_trait158(Class* self_) {
  S625child1* self = ((S625child1*)self_);
{
    return (self -> a);
  }
} 
Trait158* newTrait158_S625child1() {
  Trait158 (* impl) = (new Trait158());
  setVec(Trait158_v, S625child1_classId, ((void*)impl));
  ((impl -> trait158) = (& Trait158_S625child1_trait158));
  return impl;
} 
Trait158* Trait158_S625child1_ = newTrait158_S625child1();

int S626_classId = Class_genId();
struct S626{

  int id;
  S626 ():id(S626_classId){

  }
};


int S626child1_classId = Class_genId();
struct S626child1:S626{

  S626child1 (int a):a(a){
    (id = S626child1_classId);
{

    }
  }
  int a;
};


int S626child2_classId = Class_genId();
struct S626child2:S626{

  S626child2 (int a):a(a){
    (id = S626child2_classId);
{

    }
  }
  int a;
};


int S626child3_classId = Class_genId();
struct S626child3:S626{

  S626child3 (int a):a(a){
    (id = S626child3_classId);
{

    }
  }
  int a;
};


struct Trait626{

  int(*trait626)(Class*);
};

Vec* Trait626_v = newVec();

int Trait626_S626child1_trait626(Class* self_) {
  S626child1* self = ((S626child1*)self_);
{
    return (self -> a);
  }
} 
Trait626* newTrait626_S626child1() {
  Trait626 (* impl) = (new Trait626());
  setVec(Trait626_v, S626child1_classId, ((void*)impl));
  ((impl -> trait626) = (& Trait626_S626child1_trait626));
  return impl;
} 
Trait626* Trait626_S626child1_ = newTrait626_S626child1();

int Trait626_S626child2_trait626(Class* self_) {
  S626child2* self = ((S626child2*)self_);
{
    return (self -> a);
  }
} 
Trait626* newTrait626_S626child2() {
  Trait626 (* impl) = (new Trait626());
  setVec(Trait626_v, S626child2_classId, ((void*)impl));
  ((impl -> trait626) = (& Trait626_S626child2_trait626));
  return impl;
} 
Trait626* Trait626_S626child2_ = newTrait626_S626child2();

int Trait58_S626child1_trait58(Class* self_) {
  S626child1* self = ((S626child1*)self_);
{
    return (self -> a);
  }
} 
Trait58* newTrait58_S626child1() {
  Trait58 (* impl) = (new Trait58());
  setVec(Trait58_v, S626child1_classId, ((void*)impl));
  ((impl -> trait58) = (& Trait58_S626child1_trait58));
  return impl;
} 
Trait58* Trait58_S626child1_ = newTrait58_S626child1();

int S627_classId = Class_genId();
struct S627{

  int id;
  S627 ():id(S627_classId){

  }
};


int S627child1_classId = Class_genId();
struct S627child1:S627{

  S627child1 (int a):a(a){
    (id = S627child1_classId);
{

    }
  }
  int a;
};


int S627child2_classId = Class_genId();
struct S627child2:S627{

  S627child2 (int a):a(a){
    (id = S627child2_classId);
{

    }
  }
  int a;
};


int S627child3_classId = Class_genId();
struct S627child3:S627{

  S627child3 (int a):a(a){
    (id = S627child3_classId);
{

    }
  }
  int a;
};


int S627child4_classId = Class_genId();
struct S627child4:S627{

  S627child4 (int a):a(a){
    (id = S627child4_classId);
{

    }
  }
  int a;
};


struct Trait627{

  int(*trait627)(Class*);
};

Vec* Trait627_v = newVec();

int Trait627_S627child1_trait627(Class* self_) {
  S627child1* self = ((S627child1*)self_);
{
    return (self -> a);
  }
} 
Trait627* newTrait627_S627child1() {
  Trait627 (* impl) = (new Trait627());
  setVec(Trait627_v, S627child1_classId, ((void*)impl));
  ((impl -> trait627) = (& Trait627_S627child1_trait627));
  return impl;
} 
Trait627* Trait627_S627child1_ = newTrait627_S627child1();

int Trait388_S627child1_trait388(Class* self_) {
  S627child1* self = ((S627child1*)self_);
{
    return (self -> a);
  }
} 
Trait388* newTrait388_S627child1() {
  Trait388 (* impl) = (new Trait388());
  setVec(Trait388_v, S627child1_classId, ((void*)impl));
  ((impl -> trait388) = (& Trait388_S627child1_trait388));
  return impl;
} 
Trait388* Trait388_S627child1_ = newTrait388_S627child1();

int S628_classId = Class_genId();
struct S628{

  int id;
  S628 ():id(S628_classId){

  }
};


int S628child1_classId = Class_genId();
struct S628child1:S628{

  S628child1 (int a):a(a){
    (id = S628child1_classId);
{

    }
  }
  int a;
};


struct Trait628{

  int(*trait628)(Class*);
};

Vec* Trait628_v = newVec();

int Trait628_S628child1_trait628(Class* self_) {
  S628child1* self = ((S628child1*)self_);
{
    return (self -> a);
  }
} 
Trait628* newTrait628_S628child1() {
  Trait628 (* impl) = (new Trait628());
  setVec(Trait628_v, S628child1_classId, ((void*)impl));
  ((impl -> trait628) = (& Trait628_S628child1_trait628));
  return impl;
} 
Trait628* Trait628_S628child1_ = newTrait628_S628child1();

int Trait509_S628child1_trait509(Class* self_) {
  S628child1* self = ((S628child1*)self_);
{
    return (self -> a);
  }
} 
Trait509* newTrait509_S628child1() {
  Trait509 (* impl) = (new Trait509());
  setVec(Trait509_v, S628child1_classId, ((void*)impl));
  ((impl -> trait509) = (& Trait509_S628child1_trait509));
  return impl;
} 
Trait509* Trait509_S628child1_ = newTrait509_S628child1();

int S629_classId = Class_genId();
struct S629{

  int id;
  S629 ():id(S629_classId){

  }
};


int S629child1_classId = Class_genId();
struct S629child1:S629{

  S629child1 (int a):a(a){
    (id = S629child1_classId);
{

    }
  }
  int a;
};


int S629child2_classId = Class_genId();
struct S629child2:S629{

  S629child2 (int a):a(a){
    (id = S629child2_classId);
{

    }
  }
  int a;
};


int S629child3_classId = Class_genId();
struct S629child3:S629{

  S629child3 (int a):a(a){
    (id = S629child3_classId);
{

    }
  }
  int a;
};


struct Trait629{

  int(*trait629)(Class*);
};

Vec* Trait629_v = newVec();

int Trait629_S629child1_trait629(Class* self_) {
  S629child1* self = ((S629child1*)self_);
{
    return (self -> a);
  }
} 
Trait629* newTrait629_S629child1() {
  Trait629 (* impl) = (new Trait629());
  setVec(Trait629_v, S629child1_classId, ((void*)impl));
  ((impl -> trait629) = (& Trait629_S629child1_trait629));
  return impl;
} 
Trait629* Trait629_S629child1_ = newTrait629_S629child1();

int Trait629_S629child2_trait629(Class* self_) {
  S629child2* self = ((S629child2*)self_);
{
    return (self -> a);
  }
} 
Trait629* newTrait629_S629child2() {
  Trait629 (* impl) = (new Trait629());
  setVec(Trait629_v, S629child2_classId, ((void*)impl));
  ((impl -> trait629) = (& Trait629_S629child2_trait629));
  return impl;
} 
Trait629* Trait629_S629child2_ = newTrait629_S629child2();

int Trait480_S629child1_trait480(Class* self_) {
  S629child1* self = ((S629child1*)self_);
{
    return (self -> a);
  }
} 
Trait480* newTrait480_S629child1() {
  Trait480 (* impl) = (new Trait480());
  setVec(Trait480_v, S629child1_classId, ((void*)impl));
  ((impl -> trait480) = (& Trait480_S629child1_trait480));
  return impl;
} 
Trait480* Trait480_S629child1_ = newTrait480_S629child1();

int S630_classId = Class_genId();
struct S630{

  int id;
  S630 ():id(S630_classId){

  }
};


int S630child1_classId = Class_genId();
struct S630child1:S630{

  S630child1 (int a):a(a){
    (id = S630child1_classId);
{

    }
  }
  int a;
};


int S630child2_classId = Class_genId();
struct S630child2:S630{

  S630child2 (int a):a(a){
    (id = S630child2_classId);
{

    }
  }
  int a;
};


int S630child3_classId = Class_genId();
struct S630child3:S630{

  S630child3 (int a):a(a){
    (id = S630child3_classId);
{

    }
  }
  int a;
};


int S630child4_classId = Class_genId();
struct S630child4:S630{

  S630child4 (int a):a(a){
    (id = S630child4_classId);
{

    }
  }
  int a;
};


struct Trait630{

  int(*trait630)(Class*);
};

Vec* Trait630_v = newVec();

int Trait630_S630child1_trait630(Class* self_) {
  S630child1* self = ((S630child1*)self_);
{
    return (self -> a);
  }
} 
Trait630* newTrait630_S630child1() {
  Trait630 (* impl) = (new Trait630());
  setVec(Trait630_v, S630child1_classId, ((void*)impl));
  ((impl -> trait630) = (& Trait630_S630child1_trait630));
  return impl;
} 
Trait630* Trait630_S630child1_ = newTrait630_S630child1();

int Trait481_S630child1_trait481(Class* self_) {
  S630child1* self = ((S630child1*)self_);
{
    return (self -> a);
  }
} 
Trait481* newTrait481_S630child1() {
  Trait481 (* impl) = (new Trait481());
  setVec(Trait481_v, S630child1_classId, ((void*)impl));
  ((impl -> trait481) = (& Trait481_S630child1_trait481));
  return impl;
} 
Trait481* Trait481_S630child1_ = newTrait481_S630child1();

int S631_classId = Class_genId();
struct S631{

  int id;
  S631 ():id(S631_classId){

  }
};


int S631child1_classId = Class_genId();
struct S631child1:S631{

  S631child1 (int a):a(a){
    (id = S631child1_classId);
{

    }
  }
  int a;
};


int S631child2_classId = Class_genId();
struct S631child2:S631{

  S631child2 (int a):a(a){
    (id = S631child2_classId);
{

    }
  }
  int a;
};


int S631child3_classId = Class_genId();
struct S631child3:S631{

  S631child3 (int a):a(a){
    (id = S631child3_classId);
{

    }
  }
  int a;
};


struct Trait631{

  int(*trait631)(Class*);
};

Vec* Trait631_v = newVec();

int Trait631_S631child1_trait631(Class* self_) {
  S631child1* self = ((S631child1*)self_);
{
    return (self -> a);
  }
} 
Trait631* newTrait631_S631child1() {
  Trait631 (* impl) = (new Trait631());
  setVec(Trait631_v, S631child1_classId, ((void*)impl));
  ((impl -> trait631) = (& Trait631_S631child1_trait631));
  return impl;
} 
Trait631* Trait631_S631child1_ = newTrait631_S631child1();

int Trait631_S631child2_trait631(Class* self_) {
  S631child2* self = ((S631child2*)self_);
{
    return (self -> a);
  }
} 
Trait631* newTrait631_S631child2() {
  Trait631 (* impl) = (new Trait631());
  setVec(Trait631_v, S631child2_classId, ((void*)impl));
  ((impl -> trait631) = (& Trait631_S631child2_trait631));
  return impl;
} 
Trait631* Trait631_S631child2_ = newTrait631_S631child2();

int Trait324_S631child1_trait324(Class* self_) {
  S631child1* self = ((S631child1*)self_);
{
    return (self -> a);
  }
} 
Trait324* newTrait324_S631child1() {
  Trait324 (* impl) = (new Trait324());
  setVec(Trait324_v, S631child1_classId, ((void*)impl));
  ((impl -> trait324) = (& Trait324_S631child1_trait324));
  return impl;
} 
Trait324* Trait324_S631child1_ = newTrait324_S631child1();

int S632_classId = Class_genId();
struct S632{

  int id;
  S632 ():id(S632_classId){

  }
};


int S632child1_classId = Class_genId();
struct S632child1:S632{

  S632child1 (int a):a(a){
    (id = S632child1_classId);
{

    }
  }
  int a;
};


struct Trait632{

  int(*trait632)(Class*);
};

Vec* Trait632_v = newVec();

int Trait632_S632child1_trait632(Class* self_) {
  S632child1* self = ((S632child1*)self_);
{
    return (self -> a);
  }
} 
Trait632* newTrait632_S632child1() {
  Trait632 (* impl) = (new Trait632());
  setVec(Trait632_v, S632child1_classId, ((void*)impl));
  ((impl -> trait632) = (& Trait632_S632child1_trait632));
  return impl;
} 

  S632child1* self = ((S632child1*)self_);