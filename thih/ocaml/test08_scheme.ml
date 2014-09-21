open OUnit
open Thih.Type
open Thih.Subst
open Thih.Kind
open Thih.Unify
open Thih.Pred
open Thih.Scheme

let (=:=) = assert_equal

let _ =
  run_test_tt_main ("id">:::[

    "scheme">:: begin fun() ->

      let ty = TVar(Tyvar("a", Star)) in
      let pred = IsIn("Num", ty) in
      let sc = Forall([], Qual([pred], ty)) in

      sc =:=
        Forall([],
          Qual(
            [IsIn("Num", TVar(Tyvar("a", Star)))],
            TVar(Tyvar("a", Star))))

    end;

    "schemeApply">:: begin fun() ->
      let ty = TVar(Tyvar("a", Star)) in
      let pred = IsIn("Num", ty) in
      let sc = Forall([], Qual([pred], ty)) in
      let subst = [(Tyvar("a", Star), tInt)] in
      let sc1 = schemeApply(subst)(sc) in

      sc1 =:=
        Forall([],
          Qual(
            [IsIn("Num", TCon(Tycon("Int", Star)))],
            TCon(Tycon("Int", Star))))

    end;

    "schemeTv">:: begin fun() ->
      let ty = TVar(Tyvar("a", Star)) in
      let pred = IsIn("Num", ty) in
      let sc = Forall([], Qual([pred], ty)) in
      let tvs = schemeTv(sc) in

      tvs =:=
        [Tyvar("a", Star)]

    end;

    "quantify">:: begin fun() ->
      let tyvar = Tyvar("a", Star) in
      let ty = TVar(Tyvar("a", Star)) in
      let pred = IsIn("Num", ty) in
      let qual = Qual([pred], fn(ty)(tInt)) in
      let sc = quantify([tyvar])(qual) in

      sc =:=
        Forall([Star],
          Qual([IsIn("Num", TGen(0))],
            TAp(
              TAp(
                TCon(Tycon("(->)", Kfun(Star, Kfun(Star, Star)))),
                TGen(0)),
              TCon(Tycon("Int", Star)))))
    end;

    "toScheme">:: begin fun() ->
      let ty = TVar(Tyvar("a", Star)) in
      let sc = toScheme(ty) in

      sc =:=
        Forall([], Qual([], TVar(Tyvar("a", Star))))

    end

  ])
