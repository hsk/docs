(*|
# 6 Unification and Matching

    >>> open Unify ;;

*)

open List
open Kind
open Type
open Subst_

let rec mgu (t1:type_) (t2:type_):subst =
  match t1, t2 with
  | TAp(l, r), TAp(l', r') ->
    let s1 = mgu l l' in
    let s2 = mgu (typeApply s1 r) (typeApply s1 r') in
    s2 @@ s1
  | TVar u, t | t, TVar u -> varBind u t
  | TCon tc1, TCon tc2 when tc1 = tc2 -> nullSubst
  | _ -> failwith "types do not unify"

and varBind (u:tyvar) (t:type_):subst =
  match t with
  | _ when t = TVar u                -> nullSubst
  | _ when mem u (typeTv t)          -> failwith "occurs check fails"
  | _ when tyvarKind u <> typeKind t -> failwith "kinds do not match"
  | _                                -> u +-> t

let rec match_ (t1:type_) (t2:type_):subst =
  match t1, t2 with
  | TAp(l, r), TAp(l', r') ->
    let sl = match_ l l' in
    let sr = match_ r r' in
    merge sl sr
  | TVar u, t when tyvarKind u = typeKind t -> u +-> t
  | TCon tc1, TCon tc2 when tc1 = tc2 -> nullSubst
  | _ -> failwith "types do not match"

(*|
    
    >>> let t1 = TVar(Tyvar("a", Star)) ;;
    val t1 : Type.type_ = TVar (Tyvar ("a", Star))

    >>> let t2 = TVar(Tyvar("b", Star)) ;;
    val t2 : Type.type_ = TVar (Tyvar ("b", Star))

    >>> let tv1 = Tyvar("a", Star) ;;
    val tv1 : Type.tyvar = Tyvar ("a", Star)

    >>> let t3 = tInt ;;
    val t3 : Type.type_ = TCon (Tycon ("Int", Star))

## mgu

    >>> let subst = mgu(t1)(t2);;
    val subst : Subst_.subst = [(Tyvar ("a", Star), TVar (Tyvar ("b", Star)))]

    >>> subst =
        [(Tyvar("a", Star), TVar(Tyvar("b", Star)))];;
    - : bool = true

    >>> let subst2 = mgu(t1)(t3);;
    val subst2 : Subst_.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> subst2 =
        [(Tyvar("a", Star), tInt)];;
    - : bool = true

## varBind

    >>> let subst = varBind(tv1)(t1);;
    val subst : Subst_.subst = []

    >>> subst = [];;
    - : bool = true

    >>> let subst2 = varBind(tv1)(t3);;
    val subst2 : Subst_.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> subst2 =
        [(Tyvar("a", Star), tInt)];;
    - : bool = true

## match_

    >>> let subst = match_(t1)(t2);;
    val subst : Subst_.subst = [(Tyvar ("a", Star), TVar (Tyvar ("b", Star)))]

    >>> subst =
        [(Tyvar("a", Star), TVar(Tyvar("b", Star)))];;
    - : bool = true

    >>> let subst2 = match_(t1)(t3);;
    val subst2 : Subst_.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]

    >>> subst2 = [(Tyvar("a", Star), tInt)] ;;
    - : bool = true

*)
