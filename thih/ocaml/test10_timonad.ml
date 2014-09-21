open OUnit
open Thih
open Thih.Type
open Thih.Subst
open Thih.Kind
open Thih.Unify
open Thih.Pred
open Thih.Scheme
open Thih.Assump
open Thih.TIMonad

let (=:=) = assert_equal

let _ =
  run_test_tt_main ("id">:::[

    "ti">:: begin fun() ->
      let subst = [(Tyvar("a", Star), tInt)] in
      let ti = (ref subst, ref 1) in

      ti =:=
        ({contents=[(Tyvar("a", Star), TCon(Tycon("Int", Star)))]}, {contents=1})
    end;

    "runTI">:: begin fun() ->
      let n = runTI begin fun (subst, n) ->
        n := !n + 1;
        !n
      end in

      n =:= 1
    end;

    "getSubst">:: begin fun() ->
      runTI begin fun ti ->
        let subst = getSubst(ti) in

        subst =:= []
      end
    end;

    "extSubst">:: begin fun() ->
      runTI begin fun ti ->
        let subst = [(Tyvar("a", Star), tInt)] in
        extSubst(ti)(subst);
        let subst2 = getSubst(ti) in

        subst2 =:= [(Tyvar("a", Star), TCon(Tycon("Int", Star)))]
      end
    end;

    "unify">:: begin fun() ->
      runTI begin fun ti ->
        let t1 = TVar(Tyvar("a", Star)) in
        unify(ti)(t1)(tInt);

        t1 =:= TVar(Tyvar("a", Star))
      end
    end;

    "newTVar">:: begin fun() ->
      runTI begin fun ti ->
        let t1 = newTVar(ti)(Star) in
        t1 =:= TVar(Tyvar("v0", Star));

        unify(ti)(t1)(tInt);

        t1 =:= TVar(Tyvar("v0", Star));

        let t2:type_ = typeApply(getSubst(ti))(t1) in
        t2 =:= tInt
      end
    end;

    "freshInst">:: begin fun() ->
      runTI begin fun ti ->
        let ty = TVar(Tyvar("a", Star)) in
        let sc = toScheme(ty) in

        let tq:type_ qual = freshInst(ti)(sc) in

        sc =:= Forall([], Qual([], TVar(Tyvar("a", Star))));
        tq =:= Qual([], TVar(Tyvar("a", Star)))
      end
    end

  ])
