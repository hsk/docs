open OUnit
open Thih

open Thih.Type
open Thih.Subst
open Thih.Kind
open Thih.Unify

let (=:=) = assert_equal

let _ =
  let t1 = TVar(Tyvar("a", Star)) in
  let t2 = TVar(Tyvar("b", Star)) in
  let tv1 = Tyvar("a", Star) in
  let t3 = tInt in

  run_test_tt_main ("id">:::[

    "mgu">:: begin fun() ->
      let subst = mgu(t1)(t2) in
      subst =:=
        [(Tyvar("a", Star), TVar(Tyvar("b", Star)))];

      let subst2 = mgu(t1)(t3) in
      subst2 =:=
        [(Tyvar("a", Star), tInt)]
    end;

    "varBind">:: begin fun() ->
      let subst = varBind(tv1)(t1) in
      subst =:= [];

      let subst2 = varBind(tv1)(t3) in
      subst2 =:=
        [(Tyvar("a", Star), tInt)]
    end;

    "match_">:: begin fun() ->
      let subst = match_(t1)(t2) in
      subst =:=
        [(Tyvar("a", Star), TVar(Tyvar("b", Star)))];

      let subst2 = match_(t1)(t3) in
      subst2 =:=
        [(Tyvar("a", Star), tInt)]
    end
  ])
