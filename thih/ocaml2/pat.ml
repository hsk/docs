(*|

# 11.2 Patterns

    >>> open Pat;;
    
*)
open List
open Kind
open Type
open Pred
open Scheme
open Assump
open TIMonad
open Infer
open Lit

(*|

## type pat パターン

パターンマッチで使う式

    case a of
    k -> k+1
    (_ as a) -> 1
    1 -> 10

のような式が会った場合に、 ->の左辺がpatで

kは

    >>> let pat = PVar("k") ;;
    val pat : Pat.pat = PVar "k"

\_ は

    >>> let pat = PWildcard ;;
    val pat : Pat.pat = PWildcard

\_ as a は

    >>> let pat = PAs("a", PWildcard) ;;
    val pat : Pat.pat = PAs ("a", PWildcard)

1 は

    >>> let pat = PLit(LitInt 1) ;;
    val pat : Pat.pat = PLit (LitInt 1)

と書きます。

PNpkとPConの２つは謎なので、tiPatで型推論するさいの結果から見てみましょう。

    >>> let pat = PNpk("a", 10) ;;
    val pat : Pat.pat = PNpk ("a", 10)

    >>> let assump = Assump("ABC", toScheme tInt) ;;
    val assump : Assump.assump = Assump ("ABC", Forall ([], Qual ([], TCon (Tycon ("Int", Star)))))

    >>> let pat = PCon(assump,[]);;
    val pat : Pat.pat = PCon (Assump ("ABC", Forall ([], Qual ([], TCon (Tycon ("Int", Star))))), [])

*)

type pat =
  | PVar of Id.id
  | PWildcard
  | PAs of Id.id * pat
  | PLit of literal
  | PNpk of Id.id * int
  | PCon of assump * pat list

(*|
## tiPat パターンの型推論

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
        let pat = PLit(LitInt 123) in
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
        let pat = PNpk("a", 10) in
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
        let assump = Assump("ABC", toScheme tInt) in

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
