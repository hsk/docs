// patscc -DATS_MEMALLOC_LIBC calc_cpy.dats -o calc_cpy

#include "share/atspre_staload.hats"

datavtype e =
  | EInt of (int)
  | EAdd of (e, e)
  | ESub of (e, e)

// データを借りてコピーする
fun e_copy(e: !e): e =
  begin case+ e of
    | EInt(x) => EInt(x)
    | EAdd(e1, e2) => EAdd(e_copy e1, e_copy e2)
    | ESub(e1, e2) => ESub(e_copy e1, e_copy e2)
  end

// データを受け取って消費する
fun e_free(e: e): void =
  begin case+ e of
    | ~EInt(x) => ()
    | ~EAdd(e1, e2) => (e_free e1; e_free e2)
    | ~ESub(e1, e2) => (e_free e1; e_free e2)
  end

// データを借りて計算する
fun e_eval(e: !e): int =
  begin case+ e of
    | EInt(x) => x
    | EAdd(e1, e2) => e_eval e1 + e_eval e2
    | ESub(e1, e2) => e_eval e1 - e_eval e2
  end

// 計算しながら解放も行う
fun e_free_eval(e: e): int =
  begin case+ e of
    | ~EInt(x) => x
    | ~EAdd(e1, e2) => e_free_eval e1 + e_free_eval e2
    | ~ESub(e1, e2) => e_free_eval e1 - e_free_eval e2
  end

extern fun e_print(!e): void
overload print with e_print

// データを借りて来て出力する
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

