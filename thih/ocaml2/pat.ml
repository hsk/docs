(*|

# 11.2 Patterns

    >>> open Pat;;
    
*)
open Big_int
open List
open Kind
open Type
open Pred
open Scheme
open Assump
open TIMonad
open Infer
open Lit

type pat =
  | PVar of Id.id
  | PWildcard
  | PAs of Id.id * pat
  | PLit of literal
  | PNpk of Id.id * big_int
  | PCon of assump * pat list

let rec tiPat (ti:ti) (pat:pat):pred list * assump list * type_ =
  begin match pat with
    | PVar i ->
      let t = newTVar ti Star in
      ([], [Assump(i, toScheme t)], t)
    | PWildcard -> ([], [], newTVar ti Star)
    | PAs(i, pat) ->
      let (ps, as_, t) = tiPat ti pat in
      (ps, Assump(i, toScheme t) :: as_, t)
    | PLit l ->
      let (ps, t) = tiLit ti l in
      (ps, [], t)
    | PNpk(i, k) ->
      let t = newTVar ti Star in
      ([IsIn("Integral", t)], [Assump(i, toScheme t)], t)
    | PCon(Assump(i, sc), pats) ->
      let (ps, as_, ts) = tiPats ti pats in
      let t' = newTVar ti Star in
      let Qual(qs, t) = freshInst ti sc in
      unify ti t (fold_right fn ts t');
      (ps @ qs, as_, t')
  end

and tiPats (ti:ti) (pats:pat list):pred list * assump list * type_ list =
  let (pss, ass, ts) = Pre.split3 (map (tiPat ti) pats) in
  (concat pss, concat ass, ts)

(*|

    >> let pat = PVar("a") ;;


## tiPat PVar

    >>> runTI begin fun ti ->
        let pat = PVar("a") in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        (preds = [],
        assumps =
          [
            Assump("a",Forall([],Qual([],TVar(Tyvar("v0",Star)))))],

        ty = TVar(Tyvar("v0", Star)))
      end
    ;;
    - : bool * bool * bool = (true, true, true)


## tiPat PWildcard

    >>> runTI begin fun ti ->
        let pat = PWildcard in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        (preds = [],
        assumps = [],
        ty = TVar(Tyvar("v0", Star)))
      end
    ;;
    - : bool * bool * bool = (true, true, true)


## tiPat PAs

    >>> runTI begin fun ti ->
        let pat = PAs("a", PWildcard) in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        (preds = [],
        assumps =
          [
            Assump("a",Forall([],Qual([],TVar(Tyvar("v0",Star)))))],

        ty = TVar(Tyvar("v0", Star))
        )
      end
    ;;
    - : bool * bool * bool = (true, true, true)


## tiPat PLit

    >>> runTI begin fun ti ->
        let pat = PLit(LitInt(big_int_of_string "123")) in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        (preds =
          [IsIn("Num",TVar(Tyvar("v0",Star)))],
        assumps = [],
        ty = TVar(Tyvar("v0", Star))
        )
      end
    ;;
    - : bool * bool * bool = (true, true, true)


## tiPat PNpk

    >>> runTI begin fun ti ->
        let pat = PNpk("a",big_int_of_string "10") in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        (preds =
          [IsIn("Integral",TVar(Tyvar("v0",Star)))],
        assumps =
          [
            Assump("a",Forall([],Qual([],TVar(Tyvar("v0",Star)))))],

        ty = TVar(Tyvar("v0", Star))
        )
      end
    ;;
    - : bool * bool * bool = (true, true, true)


## tiPat PCon

    >>> runTI begin fun ti ->
        let t = TVar(Tyvar("a", Star)) in
        let assump = Assump("ABC", Forall([], Qual([], t))) in

        let pat = PCon(assump,[]) in
        let (preds, assumps, ty) = tiPat(ti)(pat) in

        (preds = [],
        assumps = [],
        ty = TVar(Tyvar("v0", Star))
        )

      end
    ;;
    - : bool * bool * bool = (true, true, true)


*)
