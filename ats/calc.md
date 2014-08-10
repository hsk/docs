# ATS2で書く四則演算

ATSは証明も書く事が出来るのですが、とりあえず証明は置いておいて普通のプログラミング言語として使ってみます。
Rustとの対比して見ると面白いと思います。

## 普通に多分、GCを使って書いたバージョン

メモリ管理は自動なので短く書けますが、GCが必要なのですバージョンです。
かなりSMLに近いです。
短くてよいのですが、GCが必要なので、シビアなリアルタイム性を求められると辛いです。

```
// patscc -DATS_MEMALLOC_LIBC calc.dats -o calc

#include "share/atspre_staload.hats"

datatype exp =
  | EInt of (int)
  | EAdd of (exp, exp)
  | ESub of (exp, exp)

fun eval(exp:exp):int =
  begin case exp of
    | EInt(i) => i
    | EAdd(a, b) => eval(a) + eval(b)
    | ESub(a, b) => eval(a) + eval(b)
  end

implement main0() = {
  val () = println!(eval(ESub(EAdd(EInt(1),EInt(2)),EInt(1))))
}
```

## リファレンスカウンタを使ったバージョン

これは、C++なんかでのリファレンスカウンタを使った方式で書いたバージョンです。
リファレンスカウンタの更新が面倒である分、面倒です。
デストラクタのような物がないので自分で消さないとダメです。
コピーはカウントアップするだけなのでお手軽ですね。
リファレンスカウンタは毎回更新する必要がある分計算量が増えてしまいますが、インクリメンタルにメモリを管理出来るので、停止時間が殆どないのが利点ですね。
ただし、相互参照があるとメモリリークしてしまうのでリークするような場合は、何かしら対策が必要になります。

```
// patscc -DATS_MEMALLOC_LIBC calc_ref.dats -o calc_ref

#include "share/atspre_staload.hats"

staload     "libats/SATS/refcount.sats"
staload _ = "libats/DATS/refcount.dats"

datavtype E =
  | EInt of int
  | EAdd of (Exp, Exp)
  | ESub of (Exp, Exp)
where Exp = refcnt(E)

fn eint(x: int): Exp = refcnt_make(EInt(x))
fn eadd(e1: Exp, e2: Exp) = refcnt_make(EAdd(e1, e2))
fn esub(e1: Exp, e2: Exp) = refcnt_make(ESub(e1, e2))

fn Exp_copy(e0: !Exp): Exp = (
  refcnt_incref(e0)
)

fun Exp_free(e: Exp): void = {
  var e2: Exp?
  val () = if refcnt_decref(e, e2) then {
    prval() = opt_unsome(e2)
    val () =
      begin case+ e2 of
        | ~EInt(x) => ()
        | ~EAdd(e1, e2) => (Exp_free(e1); Exp_free(e2))
        | ~ESub(e1, e2) => (Exp_free(e1); Exp_free(e2))
      end
  } else {
    prval() = opt_unnone(e2)
  }
}

extern fun Exp_print(!Exp): void
overload print with Exp_print
implement Exp_print(e) = {
  val (pf, fpf | p) = refcnt_vtakeout(e)
  val () =
    begin case+ !p of
      | EInt(x) => print!("EInt(", x, ")")
      | EAdd(e1, e2) => print!("EAdd(", e1, ", ", e2, ")")
      | ESub(e1, e2) => print!("ESub(", e1, ", ", e2, ")")
    end
  prval () = fpf(pf)
}

fun Exp_eval(e: !Exp):int = res where {
  val (pf, fpf | p) = refcnt_vtakeout(e)
  val res =
    begin case+ !p of
      | EInt(x) => x
      | EAdd(e1, e2) => Exp_eval(e1) + Exp_eval(e2)
      | ESub(e1, e2) => Exp_eval(e1) - Exp_eval(e2)
    end
  prval() = fpf(pf)
}

implement main0() = {
  val e = eadd(eint 1, eint 2)
  val e2 = esub(e, Exp_copy e)
  val ans = Exp_eval e2
  val() = println!("eval(", e2, ")")
  val() = Exp_free e2
  val() = println!ans
}
```

## コピーするバージョン

GCも、リファレンスカウンタも無い方式で書いた四則演算です。
ちょっと前までは、mallocしてfreeしてということを慎重に書く必要がありました。
それでは、恐いので、型に寄ってメモリの所有権をはっきり表して、解放漏れを防ぎ、所有権を渡したらもう使えなくしたり、借り物のデータの所有権は奪えないようにする事で安全性を高めています。

データは誰かが所持しています。
貸し借りするか、所有権を移動し、消費する必要があります。

借りるには!を使います。
譲り受けるには何もせず、datavtypeは~で消費します。
とにかく消費する関数は自動生成してくれないようなので、自作するしかないようです。

コピーを取るには、!で借りて来て、新しいデータを作る必要があるのでコストがかかります。このコストをいかにかけずに書くかで複数の書き方が考えられます。
コンパイラのような変換に次ぐ変換を行うような処理の場合は、変換したらもう使わないはずなので、消費するか、次に引き渡していき、デバッグ出力の時は借りればコピーレスで処理出来るのではないかと思います。リファレンスは持たずに、ID等で弱参照する感じにして型推論等はすると良いのかな？っと思います。

自動で消してくれないのがRustより面倒ですが、何処でどう解放するのかを明示的に書く事でブラックボックス化されない点が良い所でしょう。

```
// patscc -DATS_MEMALLOC_LIBC calc_cpy.dats -o calc_cpy

#include "share/atspre_staload.hats"

datavtype e =
  | EInt of (int)
  | EAdd of (e, e)
  | ESub of (e, e)

fun e_copy(e: !e): e =
  begin case+ e of
    | EInt(x) => EInt(x)
    | EAdd(e1, e2) => EAdd(e_copy e1, e_copy e2)
    | ESub(e1, e2) => ESub(e_copy e1, e_copy e2)
  end

fun e_free(e: e): void =
  begin case+ e of
    | ~EInt(x) => ()
    | ~EAdd(e1, e2) => (e_free e1; e_free e2)
    | ~ESub(e1, e2) => (e_free e1; e_free e2)
  end

fun e_eval(e: !e): int =
  begin case+ e of
    | EInt(x) => x
    | EAdd(e1, e2) => e_eval e1 + e_eval e2
    | ESub(e1, e2) => e_eval e1 - e_eval e2
  end

fun e_free_eval(e: e): int =
  begin case+ e of
    | ~EInt(x) => x
    | ~EAdd(e1, e2) => e_free_eval e1 + e_free_eval e2
    | ~ESub(e1, e2) => e_free_eval e1 - e_free_eval e2
  end

extern fun e_print(!e): void
overload print with e_print

implement e_print(e) =
  begin case+ e of
    | EInt(x) => print!("EInt(",x,")")
    | EAdd(e1, e2) => print!("EAdd(", e1 ,", ", e2,")")
    | ESub(e1, e2) => print!("ESub(", e1 ,", ", e2,")")
  end

implement main0() = {

  val e0 = EInt 1
  val e0_2 = e_copy e0
  val e1 = EAdd(e0, e0_2)
  val e1_2 = e_copy e1
  val e2 = EAdd(e1, e1_2)
  val e3 = e_copy e2

  val () = println!e2
  val () = println!(e_eval e2)

  val () = println!(e_free_eval e3)
  val () = e_free e2
}
```

上のプログラムをC言語で書いてみましょう。
C言語では只のポインタで誰が持ち主で誰が消さなきゃ行けないとかないので、良くわかりません。でも、リファレンスカウンタもGCもないけど、計算機は作れるし、ちゃんと書けばメモリリークも起こりません。

```
#include "stdio.h"
#include "stdlib.h"

struct e {
  enum { e_EInt,e_EAdd,e_ESub } tag;
  union {
    struct {
      int i;
    } EInt;
    struct {
      struct e* e1;
      struct e* e2;
    } EAdd;
    struct {
      struct e* e1;
      struct e* e2;
    } ESub;
  };
};

struct e* EInt(int i) {
  struct e *e = (struct e*)malloc(sizeof(struct e));
  e->tag = e_EInt;
  e->EInt.i = i;
  return e;
}

struct e* EAdd(struct e* e1, struct e* e2) {
  struct e *e = (struct e*)malloc(sizeof(struct e));
  e->tag = e_EAdd;
  e->EAdd.e1 = e1;
  e->EAdd.e2 = e2;
  return e;
}

struct e* ESub(struct e* e1, struct e* e2) {
  struct e *e = (struct e*)malloc(sizeof(struct e));
  e->tag = e_ESub;
  e->ESub.e1 = e1;
  e->ESub.e2 = e2;
  return e;
}

struct e* e_copy(struct e* e) {
  switch (e->tag) {
    case e_EInt: return EInt(e->EInt.i);
    case e_EAdd: return EAdd(e_copy(e->EAdd.e1), e_copy(e->EAdd.e2));
    case e_ESub: return ESub(e_copy(e->ESub.e1), e_copy(e->ESub.e2));
  }
}

void e_free(struct e* e) {
  switch (e->tag) {
    case e_EInt:
      {
        free(e);
        break;
      }
    case e_EAdd:
      {
        struct e* e1 = e->EAdd.e1;
        struct e* e2 = e->EAdd.e2;
        free(e);
        e_free(e1);
        e_free(e2);
        break;
      }
    case e_ESub:
      {
        struct e* e1 = e->ESub.e1;
        struct e* e2 = e->ESub.e2;
        free(e);
        e_free(e1);
        e_free(e2);
        break;
      }
  }
}


int e_eval(struct e* e) {
  switch (e->tag) {
    case e_EInt: return e->EInt.i;
    case e_EAdd: return e_eval(e->EAdd.e1) + e_eval(e->EAdd.e2);
    case e_ESub: return e_eval(e->ESub.e1) - e_eval(e->ESub.e2);
  }
}

int e_free_eval(struct e* e) {
  switch (e->tag) {
    case e_EInt:
      {
        int i = e->EInt.i;
        free(e);
        return i;
      }
    case e_EAdd:
      {
        struct e* e1 = e->EAdd.e1;
        struct e* e2 = e->EAdd.e2;
        free(e);
        return e_free_eval(e1) + e_free_eval(e2);
      }
    case e_ESub:
      {
        struct e* e1 = e->EAdd.e1;
        struct e* e2 = e->EAdd.e2;
        free(e);
        return e_free_eval(e1) - e_free_eval(e2);
      }
  }
}

void e_print(struct e* e) {
  switch (e->tag) {
    case e_EInt: printf("EInt(%d)", e->EInt.i); break;
    case e_EAdd:
      printf("EAdd(");
      e_print(e->EAdd.e1);
      printf(",");
      e_print(e->EAdd.e2);
      printf(")");
      break;
    case e_ESub:
      printf("EAdd(");
      e_print(e->EAdd.e1);
      printf(",");
      e_print(e->EAdd.e2);
      printf(")");
      break;
  }
}

int main() {
  
  struct e* e1 = EAdd(EInt(1),EInt(2));
  e_print(e1); printf("\n");
  printf("%d", e_eval(e1)); printf("\n");
  e_free(e1);  

  printf("%d", e_free_eval(EAdd(EInt(1),EInt(2)))); printf("\n");

  struct e* e2 = EAdd(EInt(1),EInt(2));
  e_print(e2); printf("\n");
  struct e* e3 = e_copy(e2);
  printf("%d", e_free_eval(e2)); printf("\n");
  printf("%d", e_free_eval(e3)); printf("\n");

  return 0;
}
```

