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
open Big_int

let (=:=) = assert_equal


let _ =
  run_test_tt_main ("id">:::[

    "enumId 1">:: begin fun() ->
      runTI begin fun ti ->
        let lit = LitInt (big_int_of_string "123") in
        let (preds, ty) = tiLit(ti)(lit) in

        preds =:=
          [IsIn("Num", TVar(Tyvar("v0", Star)))];

        ty =:=
          TVar(Tyvar("v0", Star));

        let subst = getSubst(ti) in
        let ty2 = Subst.typeApply(subst)(ty) in

        ty2 =:= ty;

        subst =:= []

      end

    end;
  ])
