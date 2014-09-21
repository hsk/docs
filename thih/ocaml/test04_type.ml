open OUnit
open Thih
open Thih.Kind
open Thih.Type

let tes((t: type_), (k: kind), (t2: type_)) =
  assert_equal (typeKind(t)) k ~printer:Kind.show;
  assert_equal t t2 ~printer:Type.show

let _ =
  run_test_tt_main ("type">:::[
    "パラメータのない型">:: begin fun() ->
      tes(tUnit, Star, TCon(Tycon("()", Star)));
      tes(tChar, Star, TCon(Tycon("Char", Star)));
      tes(tInt, Star, TCon(Tycon("Int", Star)));
      tes(tInteger, Star, TCon(Tycon("Integer", Star)));
      tes(tFloat, Star, TCon(Tycon("Float", Star)));
      tes(tDouble, Star, TCon(Tycon("Double", Star)));
    end;

    "List[T] のようなパラメータが１つある型">:: begin fun() ->
      tes(tList, Kfun(Star, Star),
        TCon(Tycon("[]", Kfun(Star, Star))));
    end;

    "T=>F のようなパラメータが２つある型">:: begin fun() ->
      tes(tArrow, Kfun(Star, Kfun(Star, Star)),
        TCon(Tycon("(->)", Kfun(Star, Kfun(Star, Star)))));
    end;

    "カンマもT,Fみたいに２つのパラメータが必要">:: begin fun() ->
      tes(tTuple2, Kfun(Star, Kfun(Star, Star)),
        TCon(Tycon("(,)", Kfun(Star, Kfun(Star, Star)))));
    end;

    "fn関数で２つの型をしていして関数の型を生成出来る">:: begin fun() ->
      let fn_int_int = fn(tInt)(tInt) in

      (* TApが2つある。 *)
      tes(fn_int_int, Star,
        TAp(TAp(TCon(Tycon("(->)",
          Kfun(Star, Kfun(Star, Star)))),
          TCon(Tycon("Int", Star))), TCon(Tycon("Int", Star))));
    end;

    "１つの型を指定してリスト型を生成できる">:: begin fun() ->

      let list_int = list(tInt) in

      (* TApが1つある。 *)
      tes(list_int, Star,
        TAp(TCon(Tycon("[]", Kfun(Star, Star))),
          TCon(Tycon("Int", Star))));

    end;

    "tStringはCharのリスト型だ。">:: begin fun() ->
      (* TApが1つある。 *)
      tes(tString,
        Star,
        TAp(TCon(Tycon("[]", Kfun(Star, Star))),
          TCon(Tycon("Char", Star))));
    end;

    "ペアは2つの型をもつのでTApが２つある">:: begin fun() ->
      let pair_int_char = pair(tInt)(tChar) in
      tes(pair_int_char, Star,
        TAp(TAp(TCon(Tycon("(,)", Kfun(Star, Kfun(Star, Star)))),
          TCon(Tycon("Int", Star))), TCon(Tycon("Char", Star))));

      let pair_int = TAp(TCon(Tycon("(,)", Kfun(Star, Kfun(Star, Star)))), TCon(Tycon("Int", Star))) in

      tes(pair_int, Kfun(Star, Star),
        TAp(TCon(Tycon("(,)", Kfun(Star, Kfun(Star, Star)))),
          TCon(Tycon("Int", Star))));
      ()
    end
  ])
