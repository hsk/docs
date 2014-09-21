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
open Thih.Infer
open Thih.Lit
open Thih.Pat
open Big_int

let (=:=) = assert_equal

let _ =
  run_test_tt_main ("id">:::[

    "tiPat PVar">:: begin fun() ->
      runTI begin fun ti ->
        let pat = PVar("a") in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        preds =:= [];
        assumps =:=
          [
            Assump("a",Forall([],Qual([],TVar(Tyvar("v0",Star)))))];

        ty =:= TVar(Tyvar("v0", Star))
      end
    end;

    "tiPat PWildcard">:: begin fun() ->
      runTI begin fun ti ->
        let pat = PWildcard in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        preds =:= [];
        assumps =:= [];
        ty =:= TVar(Tyvar("v0", Star))
      end
    end;

    "tiPat PAs">:: begin fun() ->
      runTI begin fun ti ->
        let pat = PAs("a", PWildcard) in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        preds =:= [];
        assumps =:=
          [
            Assump("a",Forall([],Qual([],TVar(Tyvar("v0",Star)))))];

        ty =:= TVar(Tyvar("v0", Star))
      end
    end;

    "tiPat PLit">:: begin fun() ->
      runTI begin fun ti ->
        let pat = PLit(LitInt(big_int_of_string "123")) in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        preds =:=
          [IsIn("Num",TVar(Tyvar("v0",Star)))];
        assumps =:= [];
        ty =:= TVar(Tyvar("v0", Star))
      end
    end;

    "tiPat PNpk">:: begin fun() ->
      runTI begin fun ti ->
        let pat = PNpk("a",big_int_of_string "10") in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        preds =:=
          [IsIn("Integral",TVar(Tyvar("v0",Star)))];
        assumps =:=
          [
            Assump("a",Forall([],Qual([],TVar(Tyvar("v0",Star)))))];

        ty =:= TVar(Tyvar("v0", Star))
      end
    end;

    "tiPat PCon">:: begin fun() ->
      runTI begin fun ti ->
        let t = TVar(Tyvar("a", Star)) in
        let assump = Assump("ABC", Forall([], Qual([], t))) in

        let pat = PCon(assump,[]) in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        preds =:= [];
        assumps =:= [];
        ty =:= TVar(Tyvar("v0", Star))
      end
    end

  ])
