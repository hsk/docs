(*

 This file is a translation and changing of the code pertaining to the article
 `Typing Haskell in Haskell' by Mark P. Jones [1], from Haskell to OCaml.
 Original OCaml file made by Cyril Soldani [2]. 

 1: http://web.cecs.pdx.edu/~mpj/thih/
 2: http://devmusings.legiasoft.com/_media/blog/2010/08/04/typinghaskellinml.ml
 
 Copyright (C) 1999 - 2000  Mark P. Jones
 Copyright (C) 2010 Cyril Soldani
 Copyrigtt (C) 2014 Hiroshi Sakurai

 Here follows the license (license of the original Haskell code from the
 article).

 `Typing Haskell in Haskell' is Copyright (c) Mark P Jones
 and the Oregon Graduate Institute of Science and Technology,
 1999-2000, All rights reserved, and is distributed as
 free software under the following license.

 Redistribution and use in source and binary forms, with or
 without modification, are permitted provided that the following
 conditions are met:

 - Redistributions of source code must retain the above copyright
 notice, this list of conditions and the following disclaimer.

 - Redistributions in binary form must reproduce the above
 copyright notice, this list of conditions and the following
 disclaimer in the documentation and/or other materials provided
 with the distribution.

 - Neither name of the copyright holders nor the names of its
 contributors may be used to endorse or promote products derived
 from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND THE
 CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES,
 INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS OR THE
 CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
 CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF
 ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *)

open Big_int
open List
open Num

(* 2 Preliminaries *)
(* 2 予備知識 *)
module Pre = struct
  (* 和集合 *)
  let union xs ys = filter (fun x -> not (mem x ys)) xs @ ys

  (* 積集合 *)
  let intersect xs ys = filter (fun x -> mem x ys) xs

  (* リストをセットにする。要素が１つずつにまとめる *)
  let nub xs =
    let addToSet ys y = if mem y ys then ys else y :: ys in
    fold_left addToSet [] xs

  (* 空チェック *)
  let isEmpty = function
    | [] -> true
    | _ -> false

  (* たぶんこれは、reduceじゃないのかな *)
  let fold_left1 f = function
    | [] -> invalid_arg "empty list"
    | [x] -> x
    | x :: xs -> fold_left f x xs

  (* リスト内の最初の1個目のxを削除する *)
  let rec deleteFirst x = function
    | [] -> []
    | y :: ys ->
      if x = y then ys
      else y :: deleteFirst x ys

  (* 最初のリストから2番目のリストの要素を消す *)
  let rec diff xs = function
    | [] -> xs
    | y :: ys -> diff (deleteFirst y xs) ys

  (*3つの多値を持っているリストを３つのリストに分割する *)
  let split3 xs =
    let rec loop (ws, xs, ys) = function
      | [] -> (rev ws, rev xs, rev ys)
      | (w, x, y) :: zs -> loop (w :: ws, x :: xs, y :: ys) zs in
    loop ([], [], []) xs
end

module Id = struct
  type id = string

  (* 数値に対するidを取得する *)
  let enumId n : id = "v" ^ string_of_int n
end

(* 3 Kinds *)
module Kind = struct
  type kind =
    | Star
    | Kfun of kind * kind
end

(* 4 Types *)
module Type = struct
  open Kind
  (* 型変数 *)
  type tyvar = Tyvar of Id.id * kind
  (* 型コンストラクタ *)
  type tycon = Tycon of Id.id * kind
  (* 型 *)
  type type_ =
    | TVar of tyvar
    | TCon of tycon
    | TAp of type_ * type_
    | TGen of int

  let tUnit = TCon(Tycon("()", Star))
  let tChar = TCon(Tycon("Char", Star))
  let tInt = TCon(Tycon("Int", Star))
  let tInteger = TCon(Tycon("Integer", Star))
  let tFloat = TCon(Tycon("Float", Star))
  let tDouble = TCon(Tycon("Double", Star))

  let tList = TCon(Tycon("[]", Kfun(Star, Star)))
  let tArrow = TCon(Tycon("(->)", Kfun(Star, Kfun(Star, Star))))
  let tTuple2 = TCon(Tycon("(,)", Kfun(Star, Kfun(Star, Star))))

  let fn a b = TAp(TAp(tArrow, a), b)

  let list t = TAp(tList, t)

  let tString = list tChar

  let pair a b = TAp(TAp(tTuple2, a), b)

  let tyvarKind (Tyvar(_, k)) = k
  let tyconKind (Tycon(_, k)) = k
  let rec typeKind = function
    | TCon tc -> tyconKind tc
    | TVar u -> tyvarKind u
    | TAp(t, _) ->
      begin match typeKind t with
        | Kfun(_, k) -> k
        | _ -> failwith "inconsistent type"
      end
    | TGen _ -> failwith "generic type variables have no kind"
end

(* 5 Substitutions *)
module Subst = struct
  open Type

  type subst = (tyvar * type_) list

  let nullSubst : subst = []

  let (+->) u t : subst = [(u, t)]

  let rec typeApply (s : subst) = function
    | TVar u as t ->
      begin try assoc u s
      with Not_found -> t
      end
    | TAp(l, r) -> TAp(typeApply s l, typeApply s r)
    | t -> t

  let rec typeTv = function
    | TVar u -> [u]
    | TAp(l, r) -> Pre.union (typeTv l) (typeTv r)
    | _ -> []

  let listApply (apply : subst -> 'a -> 'a) (s : subst) xs = map (apply s) xs
  let listTv tv xs : tyvar list = Pre.nub (concat (map tv xs))

  let (@@) s1 (s2 : subst) : subst =
     map (fun (u, t) -> (u, typeApply s1 t)) s2 @ s1

  let merge s1 s2 : subst =
    let agree =
      let agreeOnVar v = typeApply s1 (TVar v) = typeApply s2 (TVar v) in
      for_all agreeOnVar (Pre.intersect (map fst s1) (map fst s2)) in
    if agree then s1 @ s2 else failwith "substitutions do not agree"
end

(* 6 Unification and Matching *)
module Unify = struct
  open Kind
  open Type
  open Subst

  let rec mgu t1 t2 =
    match t1, t2 with
    | TAp(l, r), TAp(l', r') ->
      let s1 = mgu l l' in
      let s2 = mgu (typeApply s1 r) (typeApply s1 r') in
      s2 @@ s1
    | TVar u, t | t, TVar u -> varBind u t
    | TCon tc1, TCon tc2 when tc1 = tc2 -> nullSubst
    | _ -> failwith "types do not unify"

  and varBind u t =
    if t = TVar u then nullSubst
    else if mem u (typeTv t) then failwith "occurs check fails"
    else if tyvarKind u <> typeKind t then failwith "kinds do not match"
    else u +-> t

  let rec match_ t1 t2 =
    match t1, t2 with
    | TAp(l, r), TAp(l', r') ->
      let sl = match_ l l' in
      let sr = match_ r r' in
      merge sl sr
    | TVar u, t when tyvarKind u = typeKind t -> u +-> t
    | TCon tc1, TCon tc2 when tc1 = tc2 -> nullSubst
    | _ -> failwith "types do not match"
end

(* 7 Type Classes, Predicates and Qualified Types *)
module Pred = struct
  open Kind
  open Type

  (* 7.1 Basic definitions *)
  type pred = IsIn of Id.id * type_
  type 't qual = Qual of pred list * 't

  let predApply s (IsIn(i, t)) = IsIn(i, Subst.typeApply s t)
  let predsApply = Subst.listApply predApply

  let qualTypeApply s (Qual(ps, t)) = Qual(predsApply s ps, Subst.typeApply s t)

  let predTv (IsIn(_, t)) = Subst.typeTv t
  let predsTv = Subst.listTv predTv

  let qualTypeTv (Qual(ps, t)) = Pre.union (predsTv ps) (Subst.typeTv t)

  let lift m (IsIn(i, t)) (IsIn(i', t')) =
    if i = i' then m t t'
    else failwith "classes differ"

  let mguPred = lift Unify.mgu
  let matchPred = lift Unify.match_

  type inst = pred qual
  type class_ = Id.id list * inst list

  (* 7.2 Class Environments *)

  type classEnv = {
    classes : (Id.id -> class_);
    defaults : type_ list;
  }


  let super ce i = fst (ce.classes i)

  let insts ce i = snd (ce.classes i)

  let defined ce i =
    try
      ignore (ce.classes i);
      true
    with Not_found -> false

  let modify ce i c =
    { ce with classes = fun j -> if i = j then c else ce.classes j; }

  let initialEnv = {
    classes = (fun i -> raise Not_found);
    defaults = [tInteger; tDouble]
  }

  type envTransformer = classEnv -> classEnv

  let (<:>) (f : envTransformer) (g : envTransformer) : envTransformer =
    fun ce -> g (f ce)

  let addClass i is : envTransformer =
    fun ce ->
      if defined ce i then failwith "class already defined"
      else if exists (fun i -> not (defined ce i)) is then
        failwith "superclass not defined"
      else modify ce i (is, [])

  let addCoreClasses =
        addClass "Eq" []
    <:> addClass "Ord" ["Eq"]
    <:> addClass "Show" []
    <:> addClass "Read" []
    <:> addClass "Bounded" []
    <:> addClass "Enum" []
    <:> addClass "Functor" []
    <:> addClass "Monad" []

  let addNumClasses =
        addClass "Num" ["Eq"; "Show"]
    <:> addClass "Real" ["Num"; "Ord"]
    <:> addClass "Fractional" ["Num"]
    <:> addClass "Integral" ["Real"; "Enum"]
    <:> addClass "RealFrac" ["Real"; "Fractional"]
    <:> addClass "Floating" ["Fractional"]
    <:> addClass "RealFloat" ["RealFrac"; "Floating"]

  let addPreludeClasses = addCoreClasses <:> addNumClasses

  let overlap p q =
    try
      ignore (mguPred p q);
      true
    with _ -> false

  let addInst ps (IsIn(i, _) as p) : envTransformer =
    fun ce ->
      if not (defined ce i) then failwith "no class for instance"
      else
        let its = insts ce i in
        let qs = map (fun (Qual(_, q)) -> q) its in
        if exists (overlap p) qs then failwith "overlapping instance"
        else
          let c = super ce i, Qual(ps, p) :: its in
          modify ce i c

  let exampleInsts =
        addPreludeClasses
    <:> addInst [] (IsIn("Ord", tUnit))
    <:> addInst [] (IsIn("Ord", tChar))
    <:> addInst [] (IsIn("Ord", tInt))
    <:> addInst [IsIn("Ord", TVar(Tyvar("a", Star)));
                 IsIn("Ord", TVar(Tyvar("b", Star)))]
                (IsIn("Ord", pair (TVar(Tyvar("a", Star)))
                                  (TVar(Tyvar("b", Star)))))

  (* 7.3 Entailment *)


  let rec bySuper ce (IsIn(i, t) as p) =
    p :: concat (map (fun i' -> bySuper ce (IsIn(i', t))) (super ce i))

  let byInst ce (IsIn(i, t) as p) =
    let tryInst (Qual(ps, h)) =
      try
       let u = matchPred h p in
       Some (map (predApply u) ps)
      with _ -> None in
    let rec msum = function
      | [] -> None
      | None :: xs -> msum xs
      | x :: _ -> x in
    msum (map tryInst (insts ce i))

  let rec entail ce ps p =
    exists (mem p) (map (bySuper ce) ps) ||
    match byInst ce p with
    | None -> false
    | Some qs -> for_all (entail ce ps) qs

  (* 7.4 Context Reduction *)

  let inHnf (IsIn(_, t)) =
    let rec hnf = function
      | TVar _ -> true
      | TCon _ -> false
      | TAp(t, _) -> hnf t
      | TGen _ -> failwith "context reduction on generic variable" in
    hnf t

  let rec toHnfs ce ps = concat (map (toHnf ce) ps)
  and toHnf ce p =
    if inHnf p then [p]
    else
      match byInst ce p with
      | None -> failwith "context reduction"
      | Some ps -> toHnfs ce ps

  let simplify ce ps =
    let rec loop rs = function
      | [] -> rs
      | p :: ps ->
        if entail ce (rs @ ps) p then loop rs ps
        else loop (p :: rs) ps in
    loop [] ps

  let reduce ce ps = simplify ce (toHnfs ce ps)

  let scEntail ce ps p = exists (mem p) (map (bySuper ce) ps)
end

(* 8 Type Schemes *)
module Scheme = struct

  open Kind
  open Type
  open Pred

  type scheme = Forall of kind list * type_ qual

  let schemeApply s (Forall(ks, qt)) = Forall(ks, qualTypeApply s qt)

  let schemeTv (Forall(_, qt)) = qualTypeTv qt

  let quantify vs qt =
    let vs' = filter (fun v -> mem v vs) (qualTypeTv qt) in
    let ks = map tyvarKind vs' in
    let newGen v =
      let count = ref 0 in
      let t = TGen !count in
      incr count;
      (v, t) in
    let s = map newGen vs' in
    Forall(ks, qualTypeApply s qt)

  let toScheme t = Forall([], (Qual([], t)))
end

(* 9 Assumptions *)
module Assump = struct

  open Scheme

  type assump = Assump of Id.id * scheme

  let assumpApply s (Assump(i, sc)) = Assump(i, schemeApply s sc)

  let assumpTv (Assump(_, sc)) = schemeTv sc

  let assumpsApply = Subst.listApply assumpApply

  let assumpsTv = Subst.listTv assumpTv

  let find i as_ =
    let Assump(_, sc) = List.find (fun (Assump(i', _)) -> i = i') as_ in
    sc
end

(* 10 A Type Inference Monad *)
module TIMonad = struct

  open Kind
  open Type
  open Subst
  open Pred
  open Scheme

  type ti = subst ref * int ref

  let runTI (f : ti -> 'a) = f (ref nullSubst, ref 0)

  let getSubst ((s, _) : ti):subst = !s


  let extSubst ((s, _) : ti) u:unit = s := u @@ !s

  let unify (ti:ti) (t1:type_) (t2:type_) :unit=
    let s:subst = getSubst ti in
    let u = Unify.mgu (typeApply s t1) (typeApply s t2) in
    extSubst ti u

  let newTVar ((_, n) : ti) k =
    let v = Tyvar(Id.enumId !n, k) in
    incr n;
    TVar v

  let rec typeInst ts = function
    | TAp(l, r) -> TAp(typeInst ts l, typeInst ts r)
    | TGen n -> nth ts n
    | t -> t

  let listInst inst (ts : type_ list) (xs : 'a list) : 'a list = map (inst ts) xs

  let predInst ts (IsIn(c, t)) = IsIn(c, typeInst ts t)

  let qualTypeInst ts (Qual(ps, t)) = Qual(listInst predInst ts ps, typeInst ts t)

  let freshInst ti (Forall(ks, qt)) =
    let ts = map (newTVar ti) ks in
    qualTypeInst ts qt
end

(* 11 Type Inference *)
module Infer = struct
  open Pred
  open Assump
  open TIMonad

  type ('e, 't) infer = ti -> classEnv -> assump list -> 'e -> pred list * 't
end

(* 11.1 Literals *)
module Lit = struct
  open Kind
  open Type
  open Pred
  open TIMonad
  open Infer

  type literal =
    | LitInt of big_int
    | LitChar of char
    | LitRat of num
    | LitStr of string

  let tiLit ti = function
    | LitChar _ -> ([], tChar)
    | LitInt _ ->
      let v = newTVar ti Star in
      ([IsIn("Num", v)], v)
    | LitStr _ -> ([], tString)
    | LitRat _ ->
      let v = newTVar ti Star in
      ([IsIn("Fractional", v)], v)
end

(* 11.2 Patterns *)
module Pat = struct
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

  let rec tiPat ti = function
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
  and tiPats ti pats =
    let (pss, ass, ts) = Pre.split3 (map (tiPat ti) pats) in
    (concat pss, concat ass, ts)
end

(* 11.3 Expressions
 * 11.4 Alternatives
 * 11.5 From Types to Type Schemes
 * 11.6 Binding Groups *)
module TIMain = struct
  open Kind
  open Type
  open Pred
  open Subst
  open TIMonad
  open Infer
  open Lit
  open Pat
  open Scheme
  open Assump

  type ambiguity = tyvar * pred list

  let ambiguities vs ps : ambiguity list =
    let vs' = Pre.diff (predsTv ps) vs in
    map (fun v -> (v, filter (fun p -> mem v (predTv p)) ps)) vs'

  let numClasses : Id.id list = [
    "Num"; "Integral"; "Floating"; "Fractional"; "Real"; "RealFloat";
    "RealFrac"]

  let stdClasses : Id.id list = [
    "Eq"; "Ord"; "Show"; "Read"; "Bounded"; "Enum"; "Ix"; "Functor"; "Monad";
    "MonadPlus"] @ numClasses

  let candidates ce ((v, qs) : ambiguity) =
    let is = map (fun (IsIn(i, _)) -> i) qs in
    let ts = map (fun (IsIn(_, t)) -> t) qs in
    if for_all (fun t -> t = TVar v) ts &&
      exists (fun i -> mem i numClasses) is &&
      for_all (fun i -> mem i stdClasses) is then
      let isCandidate t' =
        for_all (entail ce []) (map (fun i -> IsIn(i, t')) is) in
      filter isCandidate ce.defaults
    else []

  let withDefaults f ce vs ps =
    let vps = ambiguities vs ps in
    let tss = map (candidates ce) vps in
    if exists Pre.isEmpty tss then failwith "cannot resolve ambiguity"
    else f vps (map hd tss)

  let defaultedPreds = withDefaults (fun vps ts -> concat (map snd vps))

  let defaultSubst ce vs ps : subst =
    withDefaults (fun vps ts -> combine (map fst vps) ts) ce vs ps

  let split ce fs gs ps =
    let ps' = reduce ce ps in
    let (ds, rs) =
      partition (fun p -> for_all (fun t -> mem t fs) (predTv p)) ps' in
    let rs' = defaultedPreds ce (fs @ gs) rs in
    (ds, Pre.diff rs rs')

  type alt = pat list * expr
  and expl = Id.id * scheme * alt list
  and impl = Id.id * alt list
  and bindGroup = expl list * impl list list
  and expr =
    | Var of Id.id
    | Lit of literal
    | Const of assump
    | Ap of expr * expr
    | Let of bindGroup * expr
    (*| Lam of alt*)
    (*| If of expr * expr * expr*)
    (*| Case of expr * (Pat * Expr) list*)

  let restricted (bs : impl list) =
    let simple (i, alts) = exists (fun alt -> Pre.isEmpty (fst alt)) alts in
    exists simple bs

  let rec tiSeq (f : ('bg, assump list) infer) : ('bg list, assump list) infer =
    fun ti ce as_ -> function
      | [] -> ([], [])
      | bs :: bss ->
        let (ps, as') = f ti ce as_ bs in
        let (qs, as'') = tiSeq f ti ce (as' @ as_) bss in
        (ps @ qs, as'' @ as')

  let rec tiExpr ti ce as_ = function
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
    (*| Lam(alt) -> tiAlt ti ce as_ alt *)
    | If(e, e1, e2) ->
      let (ps,t) = tiExpr ti ce as_ e in
      unify ti t tBool;
      let (ps1,t1) = tiExpr ti ce as_ e1 in
      let (ps2,t2) = tiExpr ti ce as_ e2 in
      unify ti t1 t2;
      (ps @ ps1 @ ps2, t1)
    (*| Case(e, branches) ->
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

  and tiAlt : (alt, type_) infer =
    fun ti ce as_ (pats, e) ->
      let (ps, as', ts) = tiPats ti pats in
      let (qs, t) = tiExpr ti ce (as' @ as_) e in
      (ps @ qs, fold_right fn ts t)
  and tiAlts ti ce as_ alts t =
    let (ps, ts) = List.split (map (tiAlt ti ce as_) alts) in
    iter (unify ti t) ts;
    concat ps
  and tiExpl ti ce as_ ((i, sc, alts) : expl) =
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
    fun ti ce as_ bs ->
      let ts = map (fun _ -> newTVar ti Star) bs in
      let (is, altss) = List.split bs in
      let scs = map toScheme ts in
      let as' = map2 (fun i sc -> Assump(i, sc)) is scs @ as_ in
      let pss = map2 (tiAlts ti ce as') altss ts in
      let s = getSubst ti in
      let ps' = map (predApply s) (concat pss) in
      let ts' = map (typeApply s) ts in
      let fs = assumpsTv (assumpsApply s as_) in
      let vss = map typeTv ts' in
      let gs = Pre.diff (Pre.fold_left1 Pre.union vss) fs in
      let (ds, rs) = split ce fs (Pre.fold_left1 Pre.intersect vss) ps' in
      if restricted bs then
        let gs' = Pre.diff gs (predsTv rs) in
        let scs' = map (fun t -> quantify gs' (Qual([], t))) ts' in
        (ds @ rs, map2 (fun i sc -> Assump(i, sc)) is scs')
      else
        let scs' = map (fun t -> quantify gs (Qual(rs, t))) ts' in
        (ds, map2 (fun i sc -> Assump(i, sc)) is scs')
  and tiBindGroup : (bindGroup, assump list) infer =
    fun ti ce as_ (es, iss) ->
      let as' = map (fun (v, sc, _) -> Assump(v, sc)) es in
      let (ps, as'') = tiSeq tiImpls ti ce (as' @ as_) iss in
      let qss = map (tiExpl ti ce (as'' @ as' @ as_)) es in
      (ps @ concat qss, as'' @ as')

  type program = bindGroup list

  let tiProgram ce as_ (bgs : program) =
    runTI (fun ti ->
            let (ps, as') = tiSeq tiBindGroup ti ce as_ bgs in
            let s = getSubst ti in
            let rs = reduce ce (predsApply s ps) in
            let s' = defaultSubst ce [] rs in
            assumpsApply (s' @@ s) as')
end

