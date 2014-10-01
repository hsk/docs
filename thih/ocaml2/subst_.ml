(* 5 Substitutions *)
open Type

type subst = (tyvar * type_) list

let nullSubst : subst = []

let (+->) u t : subst = [(u, t)]

(* 型変数を展開する *)
let rec typeApply (s : subst) (t:type_):type_ = 
  begin match t with
    | TVar u as t ->
      begin try
        List.assoc u s
      with
        Not_found -> t
      end
    | TAp(l, r) -> TAp(typeApply s l, typeApply s r)
    | t -> t
  end

let rec typeTv (t:type_):tyvar list =
  begin match t with
    | TVar u -> [u]
    | TAp(l, r) -> Pre.union (typeTv l) (typeTv r)
    | _ -> []
  end

let listApply (apply : subst -> 'a -> 'b) (s : subst) (xs:'a list):'b list =
  List.map (apply s) xs

let listTv (tv:'a -> tyvar list) (xs:'a list) : tyvar list =
  Pre.nub (List.concat (List.map tv xs))

let (@@) (s1:subst) (s2 : subst) : subst =
  List.map begin fun (u, t) ->
    (u, typeApply s1 t)
  end s2 @ s1

let merge s1 s2 : subst =
  let agree =
    let agreeOnVar v =
      typeApply s1 (TVar v) = typeApply s2 (TVar v)
    in
    List.for_all agreeOnVar (Pre.intersect (List.map fst s1) (List.map fst s2))
  in
  if agree
  then s1 @ s2
  else failwith "substitutions do not agree"

(*|

# Subst

    >>> open Subst_;;

## nullSubst
    
    >>> Subst_.nullSubst;;
    - : Subst_.subst = []

## +->

substは+->演算子で作れる

    >>> let subst = Tyvar("a", Star) +-> tInt;;
    val subst : Subst_.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> let subst1 = Tyvar("b", Star) +-> tChar ;;
    val subst1 : Subst_.subst = [(Tyvar ("b", Star), TCon (Tycon ("Char", Star)))]

substはリストなので ::: で結ß合出来る

    >>> let subst2 = subst @ subst1 ;;
    val subst2 : (Type.tyvar * Type.type_) list = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star))); (Tyvar ("b", Star), TCon (Tycon ("Char", Star)))]

## typeApply

typeApplyはsubstを元に型変数がsubstにあれば置き換える。

    >>> let tva = TVar(Tyvar("a", Star)) ;;
    val tva : Type.type_ = TVar (Tyvar ("a", Star))

    >>> let tvb = TVar(Tyvar("b", Star)) ;;
    val tvb : Type.type_ = TVar (Tyvar ("b", Star))

    >>> (typeApply(subst)(tva));;
    - : Type.type_ = TCon (Tycon ("Int", Star))

    >>> (typeApply(subst)(tvb));;
    - : Type.type_ = TVar (Tyvar ("b", Star))

TApの中身も置き換わる

    >>> let tap = TAp(tva, tvb) ;;
    val tap : Type.type_ = TAp (TVar (Tyvar ("a", Star)), TVar (Tyvar ("b", Star)))

    >>> typeApply(subst)(tap) ;;
    - : Type.type_ = TAp (TCon (Tycon ("Int", Star)), TVar (Tyvar ("b", Star)))

    >>> let tap2 = TAp(tva, tva) ;;
    val tap2 : Type.type_ = TAp (TVar (Tyvar ("a", Star)), TVar (Tyvar ("a", Star)))

    >>> typeApply(subst)(tap2) ;;
    - : Type.type_ = TAp (TCon (Tycon ("Int", Star)), TCon (Tycon ("Int", Star)))

## typeTv

typeTvでは内部で使っている型変数のリストを返す

    >>> typeTv(tva) ;;
    - : Type.tyvar list = [Tyvar ("a", Star)]

    >>> typeTv(tvb) ;;
    - : Type.tyvar list = [Tyvar ("b", Star)]

tapは2つの型を使っているのでaとbが返る

    >>> typeTv(tap) ;;
    - : Type.tyvar list = [Tyvar ("a", Star); Tyvar ("b", Star)]

## listApply

listApplyは複数の型を受け取って、展開する

    >>> listApply(typeApply)(subst)([tva; tvb]);;
    - : Type.type_ list = [TCon (Tycon ("Int", Star)); TVar (Tyvar ("b", Star))]

## listTv

listTvはlist全体の内部で使っている型変数を求める

## +-> 2

    >>> let tva = TVar(Tyvar("a", Star)) ;;
    val tva : Type.type_ = TVar (Tyvar ("a", Star))

    >>> let tvb = TVar(Tyvar("b", Star)) ;;
    val tvb : Type.type_ = TVar (Tyvar ("b", Star))

TApの中身も置き換わる

    >>> let tap = TAp(tva, tvb) ;;
    val tap : Type.type_ = TAp (TVar (Tyvar ("a", Star)), TVar (Tyvar ("b", Star)))

    >>> listTv(typeTv)([tva; tap]) ;;
    - : Type.tyvar list = [Tyvar ("a", Star); Tyvar ("b", Star)]

## @@

    >>> let subst = Tyvar("a", Star) +-> tInt ;;
    val subst : Subst_.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> let subst1 = Tyvar("b", Star) +-> TVar(Tyvar("a", Star)) @ Tyvar("a", Star) +-> tChar ;;
    val subst1 : (Type.tyvar * Type.type_) list = [(Tyvar ("b", Star), TVar (Tyvar ("a", Star))); (Tyvar ("a", Star), TCon (Tycon ("Char", Star)))]

    >>> let subst2 = Tyvar("b", Star) +-> TVar(Tyvar("a", Star)) @ Tyvar("a", Star) +-> tInt ;;
    val subst2 : (Type.tyvar * Type.type_) list = [(Tyvar ("b", Star), TVar (Tyvar ("a", Star))); (Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

@@演算子で2つのsubstを結合出来る。最初のsubstをs2に実行して結合する

    >>> (subst @@ subst1) ;;
    - : Subst_.subst = [(Tyvar ("b", Star), TCon (Tycon ("Int", Star))); (Tyvar ("a", Star), TCon (Tycon ("Char", Star))); (Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> (subst @@ subst2) ;;
    - : Subst_.subst = [(Tyvar ("b", Star), TCon (Tycon ("Int", Star))); (Tyvar ("a", Star), TCon (Tycon ("Int", Star))); (Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

## merge

@@と似ているのだけど、s1とs2でおかしいものがあったらエラーにする

    >>> merge(subst)(subst1) ;;
    Exception: Failure "substitutions do not agree".

    >>> merge(subst)(subst2) ;;
    - : Subst_.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star))); (Tyvar ("b", Star), TVar (Tyvar ("a", Star))); (Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

*)
