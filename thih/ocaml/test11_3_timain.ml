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
open Thih.TIMain
let (=:=) = assert_equal

let _ =
  run_test_tt_main ("id">:::[

    "ambiguities">:: begin fun() ->
      let tvs = [Tyvar("a", Star)] in
      let preds = [IsIn("Num", tInt); IsIn("B", tInt)] in
      let ambs = ambiguities(tvs)(preds) in

      ambs =:= []
    end;

    "numClasses">:: begin fun() ->
      numClasses =:=
        ["Num"; "Integral"; "Floating"; "Fractional"; "Real"; "RealFloat"; "RealFrac"];

      (List.length numClasses) =:= 7
    end;

    "stdClasses">:: begin fun() ->
      stdClasses =:=
        ["Eq"; "Ord"; "Show"; "Read"; "Bounded"; "Enum";
          "Ix"; "Functor"; "Monad"; "MonadPlus"; "Num"; "Integral";
          "Floating"; "Fractional"; "Real"; "RealFloat"; "RealFrac"];

      List.length stdClasses =:= 17
    end;

    "test">:: begin fun() ->
      let tv = Tyvar("a", Star) in
      let preds = [IsIn("Num", tInt); IsIn("B", tInt)] in
      Printf.printf("a ----\n");
      let _ = (tv, preds) in
      Printf.printf("b ----\n");
      (*let ce = addNumClasses(initialEnv) in
      Printf.printf("c ----\n");
      let _ = candidates(ce)(amb) in
      Printf.printf("ts = %s\n", ts)
      *)
      ()
    end

  ])
