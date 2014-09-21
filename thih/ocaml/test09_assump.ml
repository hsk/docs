open OUnit
open Thih
open Thih.Type
open Thih.Subst
open Thih.Kind
open Thih.Unify
open Thih.Pred
open Thih.Scheme
open Thih.Assump

let (=:=) = assert_equal

let _ =
  run_test_tt_main ("id">:::[

    "assump">:: begin fun() ->
      let t = TVar(Tyvar("a", Star)) in
      let assump = Assump("ABC", Forall([], Qual([], t))) in

      assump =:=
        Assump("ABC", Forall([], Qual([], TVar(Tyvar("a", Star)))))
    end;

    "assumpApply">:: begin fun() ->
      let t = TVar(Tyvar("a", Star)) in
      let assump = Assump("ABC", Forall([], Qual([], t))) in
      let subst: subst = [(Tyvar("a", Star), tInt)] in
      let assump2 = assumpApply(subst)(assump) in

      assump2 =:=
        Assump("ABC", Forall([], Qual([], TCon(Tycon("Int", Star)))))
    end;

    "assumpTv">:: begin fun() ->
      let t = TVar(Tyvar("a", Star)) in
      let assump = Assump("ABC", Forall([], Qual([], t))) in
      let tvs = assumpTv(assump) in

      tvs =:=
        [Tyvar("a", Star)]
    end;

    "assumpsApply">:: begin fun() ->
      let t = TVar(Tyvar("a", Star)) in
      let assump = Assump("ABC", Forall([], Qual([], t))) in
      let subst = [(Tyvar("a", Star), tInt)] in
      let assumps = assumpsApply(subst)([assump]) in

      assumps =:=
        [
          Assump("ABC",
            Forall([], Qual([], TCon(Tycon("Int", Star)))))]
    end;

    "assumpsTv">:: begin fun() ->
      let t = TVar(Tyvar("a", Star)) in
      let assump = Assump("ABC", Forall([], Qual([], t))) in
      let tvs = assumpsTv([assump]) in

      tvs =:=
        [Tyvar("a", Star)]
    end;

    "find">:: begin fun() ->
      let t = TVar(Tyvar("a", Star)) in
      let assump = Assump("ABC", Forall([], Qual([], t))) in
      let assump2 = Assump("ABC2", Forall([], Qual([], tInt))) in
      let sc = find("ABC")([assump;assump2]) in

      sc =:=
        Forall([], Qual([], TVar(Tyvar("a", Star))))
    end
  ])
