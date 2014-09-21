open OUnit
open Thih

open Thih.Type
open Thih.Subst
open Thih.Kind
open Thih.Unify
open Thih.Pred

let (=:=) = assert_equal

let _ =
  run_test_tt_main ("id">:::[

    "pred">:: begin fun() ->
      let ty = TVar(Tyvar("a", Star)) in
      let pred = IsIn("Num", ty) in

      pred =:=
        IsIn("Num", TVar(Tyvar("a", Star)))

    end;

    "pred list">:: begin fun() ->
      let ty = TVar(Tyvar("a", Star)) in
      let preds = [IsIn("Num", ty); IsIn("B", ty)] in

      preds =:=
        [
          IsIn("Num", TVar(Tyvar("a", Star)));
          IsIn("B", TVar(Tyvar("a", Star)))
        ]
    end;

    "pred & qual">:: begin fun() ->
      (* (Num a) => a -> Int *)

      let ty = TVar(Tyvar("a", Star)) in
      let pred = IsIn("Num", ty) in
      pred =:= IsIn("Num", TVar(Tyvar("a", Star)));

      (* Qual *)
      let q = Qual([pred], fn(ty)(tInt)) in
      q =:= Qual([IsIn("Num", TVar(Tyvar("a", Star)))],
        TAp(TAp(TCon(Tycon("(->)", Kfun(Star, Kfun(Star, Star)))),
          TVar(Tyvar("a", Star))), TCon(Tycon("Int", Star))))
    end;

    "predApply">:: begin fun() ->
      let subst = [(Tyvar("a", Star), tInt)] in
      let pred = IsIn("Num", TVar(Tyvar("a", Star))) in
      let pred2 = predApply(subst)(pred) in
      pred2 =:= IsIn("Num", TCon(Tycon("Int", Star)))

    end;

    "predTv">:: begin fun() ->
      let pred = IsIn("Num", TVar(Tyvar("a", Star))) in
      let tvs = predTv(pred) in
      tvs =:= [Tyvar("a", Star)]

    end;

    "predsApply">:: begin fun() ->
      let subst = [(Tyvar("a", Star), tInt)] in
      let preds = [IsIn("Num", TVar(Tyvar("a", Star)))] in
      let preds2 = predsApply(subst)(preds) in
      preds2 =:= [IsIn("Num", TCon(Tycon("Int", Star)))]

    end;

    "predsTv">:: begin fun() ->

      let preds = [IsIn("Num", TVar(Tyvar("a", Star)))] in
      let tvs = predsTv(preds) in
      tvs =:= [Tyvar("a", Star)]
    end;

    "qualTypeApply">:: begin fun() ->

      let subst = Tyvar("a", Star) +-> tInt in
      let ty = TVar(Tyvar("a", Star)) in
      let pred = IsIn("Num", ty) in
      let qual = Qual([pred], fn(ty)(tInt)) in
      let qual2 = qualTypeApply(subst)(qual) in

      qual =:= Qual(
        [
          IsIn("Num", TVar(Tyvar("a", Star)))],
        TAp(TAp(TCon(Tycon("(->)", Kfun(Star, Kfun(Star, Star)))),
          TVar(Tyvar("a", Star))), TCon(Tycon("Int", Star))));

      qual2 =:= Qual(
        [
          IsIn("Num", TCon(Tycon("Int", Star)))],
        TAp(TAp(TCon(Tycon("(->)", Kfun(Star, Kfun(Star, Star)))),
          TCon(Tycon("Int", Star))), TCon(Tycon("Int", Star))))

    end;
    "qualTypeTv">:: begin fun() ->

      let ty = TVar(Tyvar("a", Star)) in
      let pred = IsIn("Num", ty) in
      let qual = Qual([pred], fn(ty)(tInt)) in
      let tvs = qualTypeTv(qual) in
      tvs =:= [Tyvar("a", Star)]

    end;
    "mguPred">:: begin fun() ->
      let pred1 = IsIn("Num", TVar(Tyvar("a", Star))) in
      let pred2 = IsIn("Num", tInt) in

      let subst = mguPred(pred1)(pred2) in
      subst =:=
        [(Tyvar("a",Star),TCon(Tycon("Int",Star)))];

      let subst2 = mguPred(pred1)(pred1) in
      subst2 =:=
        []
    end;
    "matchPred">:: begin fun() ->
      let pred1 = IsIn("Num", TVar(Tyvar("a", Star))) in
      let pred2 = IsIn("Num", tInt) in

      let subst = matchPred(pred1)(pred2) in
      subst =:= [(Tyvar("a", Star), tInt)];

      let subst2 = matchPred(pred1)(pred1) in
      subst2 =:= [(Tyvar("a", Star), TVar(Tyvar("a", Star)))]
    end;

    "Inst">:: begin fun() ->
      let inst = Qual(
        [
          IsIn("Ord", tUnit);
          IsIn("Ord", tChar)],
        IsIn("Ord", tChar)) in

      inst =:= Qual(
        [
          IsIn("Ord", TCon(Tycon("()", Star)));
          IsIn("Ord", TCon(Tycon("Char", Star)))],
        IsIn("Ord", TCon(Tycon("Char", Star))))
    end;

    "class_ ==>">:: begin fun() ->
      let (_ :class_) = (
        ["Eq"],
        [
          [] ==> IsIn("Ord", tUnit);
          [] ==> IsIn("Ord", tChar);
          [] ==> IsIn("Ord",tInt);
          [
            IsIn("Ord",TVar(Tyvar("a", Star)));
            IsIn("Ord",TVar(Tyvar("b", Star)))
          ] ==>
          IsIn("Ord", (pair (TVar(Tyvar("a",Star))) (TVar(Tyvar("b",Star)))))
          
        ]
      ) in ()
    end;

    (* 7.2 Class Environments *)

    "modify">:: begin fun() ->
      let ce: classEnv = modify(initialEnv)("ABC")(["A"],
        [[] ==> IsIn("Ord", tUnit)]) in

      ce.defaults =:=
        [TCon(Tycon("Integer", Star)); TCon(Tycon("Double", Star))]

    end;

    "super">:: begin fun() ->
      let ce = modify(initialEnv)("ABC")(["A"],
        [[] ==> IsIn("Ord", tUnit)]) in
      let s = super(ce)("ABC") in
      s =:= ["A"]
    end;

    "insts">:: begin fun() ->
      let ce = modify(initialEnv)("ABC")(["A"],
        [[] ==> IsIn("Ord", tUnit)]) in
      let s = insts(ce)("ABC") in
      s =:= [Qual([], IsIn("Ord", TCon(Tycon("()", Star))))]

    end;

    "defined">:: begin fun() ->
      let ce = modify(initialEnv)("ABC")(["A"],
        [[] ==> IsIn("Ord", tUnit)]) in
      let s = defined(ce)("ABC") in
      s =:= true
    end;

    "addClass">:: begin fun() ->
      let et: envTransformer = addClass("Eq")([]) in
      let ce = et(initialEnv) in

      ce.defaults =:=
        [TCon(Tycon("Integer", Star)); TCon(Tycon("Double", Star))]
    end;

    "<:>">:: begin fun() ->
      let et1: envTransformer = addClass("Eq")([]) in
      let et2: envTransformer = addClass("Eq2")([]) in
      let et3: envTransformer = et1 <:> et2 in


      (et3 initialEnv).defaults =:=
        [TCon(Tycon("Integer", Star)); TCon(Tycon("Double", Star))];

      let et4: envTransformer = addClass("Eq")([]) <:>
        addClass("Eq2")([]) in

      (et4(initialEnv)).defaults =:=
        [TCon(Tycon("Integer", Star)); TCon(Tycon("Double", Star))]
    end;

    "overlap">:: begin fun() ->

      let pred1 = IsIn("Ord", tUnit) in
      let pred2 = IsIn("Ord", tChar) in
      overlap(pred1)(pred2) =:= false;
      overlap(pred1)(pred1) =:= true
    end;

    (* 7.3 Entailment *)

    "bySuper">:: begin fun() ->
      let preds = bySuper(exampleInsts(initialEnv))
        (IsIn("Num", TVar(Tyvar("a", Star)))) in
      preds =:= [
        IsIn("Num", TVar(Tyvar("a", Star)));
        IsIn("Eq", TVar(Tyvar("a", Star)));
        IsIn("Show", TVar(Tyvar("a", Star)))]

    end;
    "byInst">:: begin fun() ->
      let preds = byInst(exampleInsts(initialEnv))
        (IsIn("Num", TVar(Tyvar("a", Star)))) in
      preds =:= None
    end;

    "entail">:: begin fun() ->
      let p = IsIn("Num", TVar(Tyvar("a", Star))) in
      let ps = [p] in
      let result = entail(exampleInsts(initialEnv))(ps)(p) in
      result =:= true
    end;

    (* 7.4 Context Reduction *)

    "inHnf">:: begin fun() ->
      let r = inHnf(IsIn("Num", TVar(Tyvar("a", Star)))) in
      r =:= true;
      let r2 = inHnf(IsIn("Num", tInt)) in
      r2 =:= false
    end;

    "toHnfs">:: begin fun() ->
      let preds = [IsIn("Num", TVar(Tyvar("a", Star)))] in
      let preds2 = toHnfs(initialEnv)(preds) in
      preds2 =:= [IsIn("Num", TVar(Tyvar("a", Star)))]
    end;

    "toHnf">:: begin fun() ->
      let pred = IsIn("Num", TVar(Tyvar("a", Star))) in
      let preds = toHnf(initialEnv)(pred) in
      preds =:= [IsIn("Num", TVar(Tyvar("a", Star)))]
    end;

    "simplify">:: begin fun() ->
      let pred = IsIn("Num", TVar(Tyvar("a", Star))) in
      let preds = [pred] in
      let preds2 = simplify(exampleInsts(initialEnv))(preds) in
      preds2 =:= [IsIn("Num", TVar(Tyvar("a", Star)))]
    end;

    "reduce">:: begin fun() ->
      let pred = IsIn("Num", TVar(Tyvar("a", Star))) in
      let preds = [pred] in
      let preds2 = reduce(exampleInsts(initialEnv))(preds) in
      preds2 =:= [IsIn("Num", TVar(Tyvar("a", Star)))]
    end;

    "scEntail">:: begin fun() ->
      let pred = IsIn("Num", TVar(Tyvar("a", Star))) in
      let preds = [pred] in
      let result = scEntail(exampleInsts(initialEnv))(preds)(pred) in
      result =:= true
    end;

  ])
