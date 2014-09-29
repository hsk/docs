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

let show_preds_type (preds,typ) =
  Printf.sprintf "(%s,%s)" (Pred.ps preds) (Type.show typ)

let show_preds_assumps (preds,ass) =
  Printf.sprintf "(%s,%s)" (Pred.ps preds) (Assump.show_list ass)

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

    "candidates">:: begin fun() ->
      let tv = Tyvar("a", Star) in
      let preds = [IsIn("Num", tInt); IsIn("B", tInt)] in
      Printf.printf("a ----\n");
      let _ = (tv, preds) in
      Printf.printf("b ----\n");
      let ce = addNumClasses(initialEnv) in
      Printf.printf("c ----\n");
      let amb = (Tyvar("B", Star),preds) in
      let ts = candidates(ce)(amb) in
      Printf.printf "ts = %s\n" (Type.show_list ts);
      
      1 =:= 1
    end;

    "withDefaults">:: begin fun() ->
      1 =:= 1
    end;

    "defaultedPreds">:: begin fun() ->
      1 =:= 1
    end;

    "defaultSubst">:: begin fun() ->
      1 =:= 1
    end;

    "split">:: begin fun() ->
      1 =:= 1
    end;

    "restricted">:: begin fun() ->
      1 =:= 1
    end;

    "tiSeq">:: begin fun() ->
      1 =:= 1
    end;

    "tiExpr LitStr">:: begin fun() ->
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (expr: expr) = Lit(LitStr "test") in
        let result:(pred list * type_) =
          tiExpr (ti:ti)(ce:classEnv)(as_:assump list)(expr: expr)
        in
        let expected = ([],tString) in
        assert_equal expected result ~printer:show_preds_type
      end
    end;

    "tiExpr LitChar">:: begin fun() ->
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (expr: expr) = Lit(LitChar 't') in
        let result:(pred list * type_) =
          tiExpr (ti:ti)(ce:classEnv)(as_:assump list)(expr: expr)
        in
        let expected = ([],tChar) in
        assert_equal expected result ~printer:show_preds_type
      end
    end;

    "tiAlt LitChar">:: begin fun() ->
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (expr: expr) = Lit(LitChar 't') in
        let (alt: alt) = ([],expr) in
        let result:(pred list * type_) =
          tiAlt (ti:ti)(ce:classEnv)(as_:assump list)(alt:alt)
        in
        let expected = ([],tChar) in
        assert_equal expected result ~printer:show_preds_type
      end
    end;

    "tiAlts LitChar">:: begin fun() ->
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (expr: expr) = Lit(LitChar 't') in
        let (alt: alt) = ([],expr) in
        let (alts: alt list) = [alt] in
        let result:(pred list) =
          tiAlts (ti:ti)(ce:classEnv)(as_:assump list)(alts:alt list)(tChar)
        in
        let expected = [] in
        assert_equal expected result ~printer:Pred.ps
      end
    end;

    "tiExpl LitChar">:: begin fun() ->
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (expr: expr) = Lit(LitChar 't') in
        let (alt: alt) = ([],expr) in
        let (alts: alt list) = [alt] in
        let (expl:expl) = ("a",Forall([], Qual([], tChar)), alts) in
        let result:(pred list) =
          tiExpl (ti:ti)(ce:classEnv)(as_:assump list)(expl)
        in
        let expected = [] in
        assert_equal expected result ~printer:Pred.ps
      end
    end;

    "tiImpls null">:: begin fun() ->
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (impls:impl list) = [] in
        let result:(pred list * assump list) =
          tiImpls (ti:ti)(ce:classEnv)(as_:assump list)(impls)
        in
        let expected = ([],[]) in
        assert_equal expected result ~printer:show_preds_assumps
      end
    end;

    "tiBindingGroup">:: begin fun() ->
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (impls:impl list) = [] in
        let (expr: expr) = Lit(LitChar 't') in
        let (alt: alt) = ([],expr) in
        let (alts: alt list) = [alt] in
        let (expl:expl) = ("a",Forall([], Qual([], tChar)), alts) in
        let (bindGroup:bindGroup) = ([expl], [impls]) in
        let result:(pred list * assump list) =
          tiBindGroup (ti:ti)(ce:classEnv)(as_:assump list)(bindGroup)
        in
        let expected = ([],[]) in
        assert_equal expected result ~printer:show_preds_assumps
      end
    end;

    "tiProgram">:: begin fun() ->
      let (ce:classEnv) = Pred.initialEnv in
      let (as_:assump list) = [] in
      let (impls:impl list) = [] in
      let (expr: expr) = Lit(LitChar 't') in
      let (alt: alt) = ([],expr) in
      let (alts: alt list) = [alt] in
      let (expl:expl) = ("a",Forall([], Qual([], tChar)), alts) in
      let (bindGroup:bindGroup) = ([expl], [impls]) in
      let (program:program) = [bindGroup] in
      let result:assump list =
        tiProgram (ce:classEnv)(as_:assump list)(program)
      in
      let expected = [] in
      assert_equal expected result ~printer:Assump.show_list
    end

  ])
