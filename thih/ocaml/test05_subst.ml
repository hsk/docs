open OUnit
open Thih

open Thih.Type
open Thih.Subst
open Thih.Kind
let (=:=) = assert_equal

let _ =
  run_test_tt_main ("id">:::[

    "nullSubst">:: begin fun() ->
      assert_equal Subst.nullSubst Subst.nullSubst
    end;

    "+->">:: begin fun() ->

      (* substは+->演算子で作れる *)
      let subst = Tyvar("a", Star) +-> tInt in

      assert_equal subst [
        (Tyvar("a", Star), TCon(Tycon("Int", Star)))];

      let subst1 = Tyvar("b", Star) +-> tChar in

      (* substはリストなので ::: で結ß合出来る *)
      let subst2 = subst @ subst1 in

      assert_equal subst2 [
        (Tyvar("a", Star), TCon(Tycon("Int", Star)));
        (Tyvar("b", Star), TCon(Tycon("Char", Star)))];

      (* typeApplyはsubstを元に型変数がsubstにあれば置き換える。 *)

      let tva = TVar(Tyvar("a", Star)) in
      let tvb = TVar(Tyvar("b", Star)) in
      assert_equal (typeApply(subst)(tva)) (TCon(Tycon("Int", Star)));
      assert_equal (typeApply(subst)(tvb)) (TVar(Tyvar("b", Star)));

      (* TApの中身も置き換わる *)
      let tap = TAp(tva, tvb) in
      assert_equal (typeApply(subst)(tap))
        (TAp(TCon(Tycon("Int", Star)), TVar(Tyvar("b", Star))));

      let tap2 = TAp(tva, tva) in
      assert_equal (typeApply(subst)(tap2))
        (TAp(TCon(Tycon("Int", Star)), TCon(Tycon("Int", Star))));

      (* typeTvでは内部で使っている型変数のリストを返す *)
      typeTv(tva) =:= [Tyvar("a", Star)];
      typeTv(tvb) =:= [Tyvar("b", Star)];

      (* tapは2つの型を使っているのでaとbが返る *)
      typeTv(tap) =:= [Tyvar("a", Star); Tyvar("b", Star)];

      (* listApply
      def listApply[A,B](apply: Subst => A => B)(s : Subst)(xs:List[A]):List[B] = {
        xs.map(apply(s))
      } *)

      (* listApplyは複数の型を受け取って、展開する *)
      listApply(typeApply)(subst)([tva; tvb]) =:= [
        TCon(Tycon("Int", Star));
        TVar(Tyvar("b", Star))];

      (* listTvはlist全体の内部で使っている型変数を求める *)
      (* listTv
      def listTv[A](tv: A => List[Tyvar])(xs:List[A]) : List[Tyvar] = {
        Pre.nub(xs.map(tv).flatten)
      }
      *)

      listTv(typeTv)([tva; tap]) =:=
        [
          Tyvar("b", Star);
          Tyvar("a", Star)]

      (* (@@)
      implicit class SSubst(let s1: Subst) {
        def @@(s2 : Subst) : Subst = {
          s2.map {case (u, t) =>
            (u, typeApply(s1)(t))
          } ::: s1;
        }
      }
      assert_equal *)
    end;

    "@@">:: begin fun() ->

      let subst = Tyvar("a", Star) +-> tInt in
      let subst1 = Tyvar("b", Star) +-> TVar(Tyvar("a", Star)) @
                   Tyvar("a", Star) +-> tChar in
      let subst2 = Tyvar("b", Star) +-> TVar(Tyvar("a", Star)) @
                   Tyvar("a", Star) +-> tInt in

      (* @@演算子で2つのsubstを結合出来る。最初のsubstをs2に実行して結合する*)
      (subst @@ subst1) =:= [
        (Tyvar("b", Star), TCon(Tycon("Int", Star)));
        (Tyvar("a", Star), TCon(Tycon("Char", Star)));
        (Tyvar("a", Star), TCon(Tycon("Int", Star)))];

      (subst @@ subst2) =:= [
        (Tyvar("b", Star), TCon(Tycon("Int", Star)));
        (Tyvar("a", Star), TCon(Tycon("Int", Star)));
        (Tyvar("a", Star), TCon(Tycon("Int", Star)))];

      (* merge*)
      (*def merge(s1:Subst)(s2:Subst) : Subst = {
          let agree = {
            Pre.intersect(s1.map(_._1))(s2.map(_._1)).forall{ v =>
              typeApply(s1)(TVar(v)) == typeApply(s2)(TVar(v))
            }
          }
          if(agree) s1 ::: s2
          else throw new Exception("substitutions do not agree")
        }
      *)

      begin try
        (* @@と似ているのだけど、s1とs2でおかしいものがあったらエラーにする*)
        let _ = merge(subst)(subst1) in
        assert(false)
      with
        _ -> ()
      end;

      merge(subst)(subst2) =:= [
        (Tyvar("a", Star), TCon(Tycon("Int", Star)));
        (Tyvar("b", Star), TVar(Tyvar("a", Star)));
        (Tyvar("a", Star), TCon(Tycon("Int", Star)))
      ]

    end
  ])
