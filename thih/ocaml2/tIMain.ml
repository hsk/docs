(* 11.3 Expressions
 * 11.4 Alternatives
 * 11.5 From Types to Type Schemes
 * 11.6 Binding Groups *)
(*|

# TIMain

    >>> open TIMain;;
    

*)

open List
open Kind
open Type
open Pred
open Subst_
open TIMonad
open Infer
open Lit
open Pat
open Scheme
open Assump

type ambiguity = tyvar * pred list

let ambiguities (vs:tyvar list) (ps:pred list) : ambiguity list =
  let vs' = Pre.diff (predsTv ps) vs in
  map begin fun v ->
    (v, filter begin fun p ->
      mem v (predTv p)
    end ps)
  end vs'

let numClasses : Id.id list = [
  "Num"; "Integral"; "Floating"; "Fractional"; "Real"; "RealFloat";
  "RealFrac"]

let stdClasses : Id.id list = [
  "Eq"; "Ord"; "Show"; "Read"; "Bounded"; "Enum"; "Ix"; "Functor"; "Monad";
  "MonadPlus"] @ numClasses

let candidates (ce:classEnv) ((v, qs) : ambiguity): type_ list =
  let is = map (fun (IsIn(i, _)) -> i) qs in
  let ts = map (fun (IsIn(_, t)) -> t) qs in
  if for_all (fun t -> t = TVar v) ts &&
    exists (fun i -> mem i numClasses) is &&
    for_all (fun i -> mem i stdClasses) is then
    let isCandidate t' =
      for_all (entail ce []) (map (fun i -> IsIn(i, t')) is) in
    filter isCandidate ce.defaults
  else []

let withDefaults (f:ambiguity list -> type_ list -> 'a)
  (ce:classEnv) (vs:tyvar list) (ps:pred list):'a =
  let vps = ambiguities vs ps in
  let tss = map (candidates ce) vps in
  if exists Pre.isEmpty tss then failwith "cannot resolve ambiguity"
  else f vps (map hd tss)

let defaultedPreds (ce:classEnv) (vs:tyvar list) (ps:pred list):pred list =
  withDefaults (fun vps ts -> concat (map snd vps)) ce vs ps

let defaultSubst (ce:classEnv) (vs:tyvar list) (ps:pred list): subst =
  withDefaults (fun vps ts -> combine (map fst vps) ts) ce vs ps

let split (ce:classEnv) (fs:tyvar list) (gs:tyvar list)
  (ps:pred list): pred list * pred list =
  let ps' = reduce ce ps in
  let (ds, rs) =
    partition begin fun p ->
      for_all begin fun t ->
        mem t fs
      end (predTv p)
    end ps' in
  let rs' = defaultedPreds ce (fs @ gs) rs in
  (ds, Pre.diff rs rs')

(*|

## mbiguities

    >>>
      let tvs = [Tyvar("a", Star)] in
      let preds = [IsIn("Num", tInt); IsIn("B", tInt)] in
      ambiguities(tvs)(preds)
    ;;
    - : TIMain.ambiguity list = []


## numClasses

    >>> numClasses ;;
    - : Id.id list = ["Num"; "Integral"; "Floating"; "Fractional"; "Real"; "RealFloat"; "RealFrac"]

    >>> List.length numClasses ;;
    - : int = 7

## stdClasses

    >>>
      stdClasses =
        ["Eq"; "Ord"; "Show"; "Read"; "Bounded"; "Enum";
          "Ix"; "Functor"; "Monad"; "MonadPlus"; "Num"; "Integral";
          "Floating"; "Fractional"; "Real"; "RealFloat"; "RealFrac"];
    ;;
    - : bool = true

    >>>
      List.length stdClasses = 17
    ;;
    - : bool = true

## candidates

    >>>
      let tv = Tyvar("a", Star) in
      let preds = [IsIn("Num", tInt); IsIn("B", tInt)] in
      Printf.printf("a ----\n");
      let _ = (tv, preds) in
      Printf.printf("b ----\n");
      let ce = addNumClasses(initialEnv) in
      Printf.printf("c ----\n");
      let amb = (Tyvar("B", Star),preds) in
      let ts = candidates(ce)(amb) in
      
      1 = 1
    ;;
    [A[A[A[A[A[A[A[A[A[A[A[A[A# let tv = Tyvar("a", Star) in let preds = [IsIn("Num", tInt); IsIn("B", tInt)] in Printf.printf("a ----\n"); let _ = (tv, preds) in Printf.printf("b ----\n"); let ce = addNumClasses(initialEnv) in Printf.printf("c ----\n"); let amb = (Tyvar("B", Star),preds) in let [4mts[24m = candidates(ce)(amb) in 1 = 1 ;; [24ma ---- b ---- Exception: Failure "superclass not defined".



## withDefaults

    >>>
      1 = 1
    ;;
    - : bool = true

## defaultedPreds

    >>>
      1 = 1
    ;;
    - : bool = true

## defaultSubst

  >>>
    1 = 1
  ;;
    - : bool = true

## split

    >>>
      1 = 1
    ;;
    - : bool = true

## restricted

    >>>
      1 = 1
    ;;
    - : bool = true

*)


(*|
## expr


var は変数ですね。

    >>> Var("test");;
    - : TIMain.expr = Var "test"

assumpはschemeに名前がついている物です。
schemeは型の元になるものです。

    >>> let ty = Tyvar ("a", Star);;
    val ty : Type.tyvar = Tyvar ("a", Star)

    >>> let t = TVar (ty) ;;
    val t : Type.type_ = TVar (Tyvar ("a", Star))

    >>> let assump = Assump("ABC", Forall([], [ty] ==> t)) ;;
    [A# let assump = Assump("ABC", Forall([], [[4mty[24m] ==> t)) ;; [24mError: This expression has type Type.tyvar but an expression was expected of type Pred.pred

    >>> Const(Assump("ABC", Forall([], Qual([], t)))) ;;
    - : TIMain.expr = Const (Assump ("ABC", Forall ([], Qual ([], TVar (Tyvar ("a", Star))))))

    >>> Ap(Var("test"),Var("test2")) ;;
    - : TIMain.expr = Ap (Var "test", Var "test2")

    >>> Let(([],[]), Var("test")) ;;
    - : TIMain.expr = Let (([], []), Var "test")

*)
type expr =
  | Var of Id.id
  | Lit of literal
  | Const of assump
  | Ap of expr * expr
  | Let of bindGroup * expr
  (* | Lam of alt*)
  (* | If of expr * expr * expr*)
  (* | Case of expr * (Pat * Expr) list*)

(*|
## alt

altは パターンのリストと式を組み合わせたものです。


*)
and alt = pat list * expr
and expl = Id.id * scheme * alt list
and impl = Id.id * alt list
and bindGroup = expl list * impl list list
let restricted (bs : impl list):bool =
  let simple (i, alts) = exists begin fun alt ->
    Pre.isEmpty (fst alt)
  end alts in
  exists simple bs

let rec tiSeq (f : ('bg, assump list) infer) : ('bg list, assump list) infer =
  fun ti ce as_ ->
    begin function
      | [] -> ([], [])
      | bs :: bss ->
        let (ps, as') = f ti ce as_ bs in
        let (qs, as'') = tiSeq f ti ce (as' @ as_) bss in
        (ps @ qs, as'' @ as')
    end

let rec tiExpr (ti:ti)(ce:classEnv)(as_:assump list)(expr: expr): pred list * type_ =
  begin match expr with
    | Var i ->
      let sc = find i as_ in
      let Qual(ps, t) = freshInst ti sc in
      (ps, t)
    | Const(Assump(_, sc)) ->
      let Qual(ps, t) = freshInst ti sc in
      (ps, t)
    | Lit l -> tiLit ti l
    | Ap(e, f) ->
      let (ps, te) = tiExpr ti ce as_ e in
      let (qs, tf) = tiExpr ti ce as_ f in
      let t = newTVar ti Star in
      unify ti (fn tf t) te;
      (ps @ qs, t)
    | Let(bg, e) ->
      let (ps, as') = tiBindGroup ti ce as_ bg in
      let (qs, t) = tiExpr ti ce (as' @ as_) e in
      (ps @ qs, t)
    (* | Lam(alt) -> tiAlt ti ce as_ alt *)
    (* | If(e, e1, e2) ->
      let (ps,t) = tiExpr ti ce as_ e in
      unify ti t tBool;
      let (ps1,t1) = tiExpr ti ce as_ e1 in
      let (ps2,t2) = tiExpr ti ce as_ e2 in
      unify ti t1 t2;
      (ps @ ps1 @ ps2, t1)*)
    (* | Case(e, branches) ->
      let (ps, t) = tiExpr ti ce as_ e in
      let v = newTVar Star in
      let tiBr (pat, f) =
        let (ps, _as',t') = tiPat pat in
        unify t t';
        let (qs, t'') = tiExpr ce (_as' @ _as) f in
        unify v t'';
        (ps @ qs)
      in
      let pss = mapM tiBr branches in
      (ps @ concat pss, v)
    *)
  end
and tiAlt : (alt, type_) infer =
  begin fun ti ce as_ (pats, e) ->
    let (ps, as', ts) = tiPats ti pats in
    let (qs, t) = tiExpr ti ce (as' @ as_) e in
    (ps @ qs, fold_right fn ts t)
  end
and tiAlts (ti:ti)(ce:classEnv)(as_:assump list)(alts:alt list)(t:type_):pred list =
  let (ps, ts) = List.split (map (tiAlt ti ce as_) alts) in
  iter (unify ti t) ts;
  concat ps
and tiExpl (ti:ti)(ce:classEnv)(as_:assump list)((i, sc, alts) : expl):pred list =
  let Qual(qs, t) = freshInst ti sc in
  let ps = tiAlts ti ce as_ alts t in
  let s = getSubst ti in
  let qs' = predsApply s qs in
  let t' = typeApply s t in
  let fs = assumpsTv (assumpsApply s as_) in
  let gs = Pre.diff (typeTv t') fs in
  let sc' = quantify gs (Qual(qs', t')) in
  let ps' = filter (fun p -> not (entail ce qs' p)) (predsApply s ps) in
  let (ds, rs) = split ce fs gs ps' in
  if sc <> sc' then failwith "signature too general"
  else if not (Pre.isEmpty rs) then failwith "context too weak"
  else ds
and tiImpls : (impl list, assump list) infer =
  begin fun ti ce as_ bs ->
    let ((bs),is,ts',gs,ds,rs) =
      let ((ce, bs), is, ps',ts',fs) =
        let ts = map (fun _ -> newTVar ti Star) bs in
        let (is, altss) = List.split bs in
        let scs = map toScheme ts in
        let as' = map2 (fun i sc -> Assump(i, sc)) is scs @ as_ in
        let pss = map2 (tiAlts ti ce as') altss ts in
        let s = getSubst ti in
        let ps' = map (predApply s) (concat pss) in
        let ts' = map (typeApply s) ts in
        let fs = assumpsTv (assumpsApply s as_) in
        ((ce, bs), is, ps',ts',fs)
      in
        Printf.printf "kore1\n";
      let vss = map typeTv ts' in
        Printf.printf "kore2\n";
      let gs = Pre.diff (Pre.fold_left1 Pre.union vss) fs in
        Printf.printf "kore3\n";
      let (ds, rs) = split ce fs (Pre.fold_left1 Pre.intersect vss) ps' in
        Printf.printf "kore4\n";
      ((bs), is, ts',gs,ds,rs)
    in
    if restricted bs then
      let gs' = Pre.diff gs (predsTv rs) in
      let scs' = map (fun t -> quantify gs' (Qual([], t))) ts' in
      (ds @ rs, map2 (fun i sc -> Assump(i, sc)) is scs')
    else
      let scs' = map (fun t -> quantify gs (Qual(rs, t))) ts' in
      (ds, map2 (fun i sc -> Assump(i, sc)) is scs')
  end
and tiBindGroup : (bindGroup, assump list) infer =
  begin fun ti ce as_ (es, iss) ->
    let as' = map (fun (v, sc, _) -> Assump(v, sc)) es in
    let (ps, as'') = tiSeq tiImpls ti ce (as' @ as_) iss in
    let qss = map (tiExpl ti ce (as'' @ as' @ as_)) es in
    (ps @ concat qss, as'' @ as')
  end

type program = bindGroup list

let tiProgram (ce:classEnv) (as_:assump list) (bgs : program):assump list =
  runTI begin fun ti ->
    let (ps, as2) = tiSeq tiBindGroup ti ce as_ bgs in
    let s = getSubst ti in
    let rs = reduce ce (predsApply s ps) in
    let s' = defaultSubst ce [] rs in
    assumpsApply (s' @@ s) as2
  end

(*|
## tiSeq

    >>>
      1 = 1
    ;;
    - : bool = true


## tiExpr LitStr

    >>>
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (expr: expr) = Lit(LitStr "test") in
        let result:(pred list * type_) =
          tiExpr (ti:ti)(ce:classEnv)(as_:assump list)(expr: expr)
        in
        let expected = ([],tString) in
        expected = result
      end
    ;;
    - : bool = true


## tiExpr LitChar

    >>>
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (expr: expr) = Lit(LitChar 't') in
        let result:(pred list * type_) =
          tiExpr (ti:ti)(ce:classEnv)(as_:assump list)(expr: expr)
        in
        let expected = ([],tChar) in
        expected = result
      end
    ;;
    - : bool = true


## tiAlt LitChar

    >>>
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (expr: expr) = Lit(LitChar 't') in
        let (alt: alt) = ([],expr) in
        let result:(pred list * type_) =
          tiAlt (ti:ti)(ce:classEnv)(as_:assump list)(alt:alt)
        in
        let expected = ([],tChar) in
        expected = result
      end
    ;;
    - : bool = true


## tiAlts LitChar

    >>>
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (expr: expr) = Lit(LitChar 't') in
        let (alt: alt) = ([],expr) in
        let (alts: alt list) = [alt] in
        tiAlts (ti:ti)(ce:classEnv)(as_:assump list)(alts:alt list)(tChar)
      end
    ;;
    - : Pred.pred list = []


## tiExpl LitChar

    >>>
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (expr: expr) = Lit(LitChar 't') in
        let (alt: alt) = ([],expr) in
        let (alts: alt list) = [alt] in
        let (expl:expl) = ("a",Forall([], Qual([], tChar)), alts) in
        tiExpl (ti:ti)(ce:classEnv)(as_:assump list)(expl)
      end
    ;;
    - : Pred.pred list = []


## tiImpls null

    >>>
      runTI begin fun (ti:ti) ->
        let (ce:classEnv) = Pred.initialEnv in
        let (as_:assump list) = [] in
        let (impls:impl list) = [] in
        tiImpls (ti:ti)(ce:classEnv)(as_:assump list)(impls)
      end
    ;;
    kore1 kore2 Exception: Invalid_argument "empty list".


## tiBindingGroup

    >>>
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
        (expected = result, result)
      end
    ;;
    kore1 kore2 Exception: Invalid_argument "empty list".


## tiProgram

    >>>
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
      expected = result
    ;;
    kore1 kore2 Exception: Invalid_argument "empty list".


*)