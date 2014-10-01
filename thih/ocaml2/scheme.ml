(*|
# 8 Type Schemes

    >>> open Scheme;;
    
*)

open List
open Kind
open Type
open Pred

(*|

## scheme

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Type.type_ = TVar (Tyvar ("a", Star))

    >>> let pred = IsIn("Num", ty) ;;
    val pred : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let sc = Forall([], Qual([pred], ty)) ;;
    val sc : Scheme.scheme = Forall ([], Qual ([IsIn ("Num", TVar (Tyvar ("a", Star)))], TVar (Tyvar ("a", Star))))

    >>> sc =
        Forall([],
          Qual(
            [IsIn("Num", TVar(Tyvar("a", Star)))],
            TVar(Tyvar("a", Star))));;
    - : bool = true
*)
type scheme = Forall of kind list * type_ qual

(*|

## schemeApply

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Type.type_ = TVar (Tyvar ("a", Star))

    >>> let pred = IsIn("Num", ty) ;;
    val pred : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let sc = Forall([], Qual([pred], ty)) ;;
    val sc : Scheme.scheme = Forall ([], Qual ([IsIn ("Num", TVar (Tyvar ("a", Star)))], TVar (Tyvar ("a", Star))))

    >>> let subst = [(Tyvar("a", Star), tInt)] ;;
    val subst : (Type.tyvar * Type.type_) list = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> let sc1 = schemeApply(subst)(sc) ;;
    val sc1 : Scheme.scheme = Forall ([], Qual ([IsIn ("Num", TCon (Tycon ("Int", Star)))], TCon (Tycon ("Int", Star))))

    >>> sc1 =
        Forall([],
          Qual(
            [IsIn("Num", TCon(Tycon("Int", Star)))],
            TCon(Tycon("Int", Star))));;
    - : bool = true
*)

let schemeApply (s:Subst_.subst) (Forall(ks, qt):scheme):scheme =
  Forall(ks, qualTypeApply s qt)

(*|
## schemeTv

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Type.type_ = TVar (Tyvar ("a", Star))

    >>> let pred = IsIn("Num", ty) ;;
    val pred : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

    >>> let sc = Forall([], Qual([pred], ty)) ;;
    val sc : Scheme.scheme = Forall ([], Qual ([IsIn ("Num", TVar (Tyvar ("a", Star)))], TVar (Tyvar ("a", Star))))

    >>> let tvs = schemeTv(sc) ;;
    val tvs : Type.tyvar list = [Tyvar ("a", Star)]

    >>> tvs = [Tyvar("a", Star)];;
    - : bool = true

*)
let schemeTv (Forall(_, qt):scheme):tyvar list = qualTypeTv qt

(*|
## quantify

tyvarのリストと、type qualから量化されたスキームを作成します。
とても便利な関数なのですよ。

    >>> let tyvar = Tyvar("a", Star) ;;
    val tyvar : Type.tyvar = Tyvar ("a", Star)

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Type.type_ = TVar (Tyvar ("a", Star))

    >>> let pred = IsIn("Num", ty) ;;
    val pred : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))

Num a => a -> int のqual

    >>> let qual = [pred] ==> fn(ty)(tInt) ;;
    val qual : Type.type_ Pred.qual = Qual ([IsIn ("Num", TVar (Tyvar ("a", Star)))], TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TVar (Tyvar ("a", Star))), TCon (Tycon ("Int", Star))))

Num a => a -> int のqualからスキームを作る。

    >>> let sc = quantify([tyvar])(qual) ;;
    val sc : Scheme.scheme = Forall ([Star], Qual ([IsIn ("Num", TGen 0)], TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TGen 0), TCon (Tycon ("Int", Star)))))

schemeは forall [*] Num gen 0 => gen 0 -> int
のようなイメージの物になる。

    >>> sc =
        Forall([Star],
          Qual([IsIn("Num", TGen(0))],
            TAp(
              TAp(
                TCon(Tycon("(->)", Kfun(Star, Kfun(Star, Star)))),
                TGen(0)),
              TCon(Tycon("Int", Star)))));;
    - : bool = true
*)
let quantify(vs:tyvar list) (qt:type_ qual):scheme =
  let vs' = filter (fun v -> mem v vs) (qualTypeTv qt) in
  let ks = map tyvarKind vs' in
  let newGen v =
    let count = ref 0 in
    let t = TGen !count in
    incr count;
    (v, t) in
  let s = map newGen vs' in
  Forall(ks, qualTypeApply s qt)

(*|
## toScheme

型からスキームを作ります。型変数がないシンプルな型の為にあります。

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Type.type_ = TVar (Tyvar ("a", Star))

    >>> let sc = toScheme(ty) ;;
    val sc : Scheme.scheme = Forall ([], Qual ([], TVar (Tyvar ("a", Star))))

    >>> sc = Forall([], Qual([], TVar(Tyvar("a", Star))));;
    - : bool = true

*)
let toScheme (t:type_) :scheme = Forall([], (Qual([], t)))
