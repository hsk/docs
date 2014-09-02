module Pre = struct
  (* 和集合 *)
  let union (xs: 'a list) (ys: 'a list):'a list =
    List.filter begin fun x ->
      not (List.mem x ys)
    end xs @ ys

  (* 積集合 *)
  let intersect (xs: 'a list) (ys: 'a list): 'a list =
    List.filter begin fun x ->
      List.mem x ys
    end xs

  (* リストをセットにする。要素が１つずつにまとめる *)
  let nub (xs : 'a list): 'a list =
    List.fold_left begin fun ys y ->
      if List.mem y ys
      then ys
      else y :: ys
    end [] xs

  let show_list show sep xs =
    begin
      let rec loop xs =
        begin match xs with
          | [] -> ""
          | [x] -> show x
          | x::xs -> show x ^ sep ^ loop xs
        end
    in
      Printf.sprintf "[%s]" (loop xs)
    end
  let show_int_list xs =
    show_list string_of_int "; " xs

  (* 空チェック *)
  let isEmpty(xs: 'a list):bool =
    begin match xs with
      | [] -> true
      | _ -> false
    end
  (* たぶんこれは、reduceじゃないのかな *)
  let fold_left1 (f:'a -> 'a -> 'a) (xs:'a list): 'a = 
    match xs with
    | [] -> invalid_arg "empty list"
    | [x] -> x
    | x :: xs -> List.fold_left f x xs

  (* リスト内の最初の1個目のxを削除する *)
  let rec deleteFirst (x:'a) (ys:'a list): 'a list = 
    begin match ys with
      | [] -> []
      | y :: ys ->
        if x = y then ys
        else y :: deleteFirst x ys
    end
  (* 最初のリストから2番目のリストの要素を消す *)
  let rec diff (xs:'a list) (ys:'a list): 'a list =
    begin match ys with
      | [] -> xs
      | y :: ys -> diff (deleteFirst y xs) ys
    end

  (*3つの多値を持っているリストを３つのリストに分割する *)
  let split3 (xs:('a * 'b * 'c)list):('a list * 'b list * 'c list) =
    let rec loop ((ws:'a list), (xs:'b list), (ys:'c list))
      (zs:('a * 'b * 'c)list):('a list * 'b list * 'c list) =
      begin match zs with
        | [] -> (List.rev ws, List.rev xs, List.rev ys)
        | (w, x, y) :: zs -> loop (w :: ws, x :: xs, y :: ys) zs
      end
    in
    loop ([], [], []) xs
end

module Id = struct
  type id = string

  (* 数値に対するidを取得する *)
  let enumId (n:int) : id =
    "v" ^ string_of_int n
end

(* 3 Kinds *)
module Kind = struct
  type kind =
    | Star
    | Kfun of kind * kind

  let rec show (k:kind):string =
    begin match k with
      | Star -> "*"
      | Kfun(Kfun _ as k1,k2) -> Printf.sprintf "(%s) -> %s" (show k1) (show k2) 
      | Kfun(k1,k2) -> Printf.sprintf "%s -> %s" (show k1) (show k2) 
    end

  let rec show_list (ks:kind list):string =
    Pre.show_list show ";" ks

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

  let tUnit :type_ = TCon(Tycon("()", Star))
  let tChar :type_ = TCon(Tycon("Char", Star))
  let tInt :type_ = TCon(Tycon("Int", Star))
  let tInteger :type_ = TCon(Tycon("Integer", Star))
  let tFloat :type_ = TCon(Tycon("Float", Star))
  let tDouble :type_ = TCon(Tycon("Double", Star))

  let tList :type_ = TCon(Tycon("[]", Kfun(Star, Star)))
  let tArrow :type_ = TCon(Tycon("(->)", Kfun(Star, Kfun(Star, Star))))
  let tTuple2 :type_ = TCon(Tycon("(,)", Kfun(Star, Kfun(Star, Star))))

  let fn (a:type_) (b:type_) :type_ = TAp(TAp(tArrow, a), b)

  let list t :type_ = TAp(tList, t)

  let tString :type_ = list tChar

  let pair a b :type_ = TAp(TAp(tTuple2, a), b)

  let tyvarKind (Tyvar(_, k)) :kind = k
  let tyconKind (Tycon(_, k)) :kind = k
  let rec typeKind t:kind =
    match t with
    | TCon tc -> tyconKind tc
    | TVar u -> tyvarKind u
    | TAp(t, _) ->
      begin match typeKind t with
        | Kfun(_, k) -> k
        | _ -> failwith "inconsistent type"
      end
    | TGen _ -> failwith "generic type variables have no kind"

  let rec show (t:type_): string =
    begin match t with
      | TVar(Tyvar(id,kind)) -> Printf.sprintf "TVar(Tyvar(%s,%s))" id (Kind.show kind)
      | TCon(Tycon(id,kind)) -> Printf.sprintf "TCon(Tycon(%s,%s))" id (Kind.show kind)
      | TAp(t1,t2)           -> Printf.sprintf "TAp(%s,%s)" (show t1) (show t2)
      | TGen(i)              -> Printf.sprintf "TGen(%d)" i
    end
  let show_list = Pre.show_list show ";"
end

(* 5 Substitutions *)
module Subst = struct
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

  let show (subst:subst):string =
    Pre.show_list begin fun (Tyvar(id,kind),type_) ->
      Printf.sprintf "Tyvar(%s,%s),%s" id (Kind.show kind) (Type.show type_)
    end "; " subst

  let rec show_tyvar(tv:tyvar): string = 
    begin match tv with
      | Tyvar(id,kind) -> Printf.sprintf "Tyvar(%s,%s)" id (Kind.show kind)
    end

  let show_tyvar_list xs :string =
    Pre.show_list begin fun (Tyvar(id,kind)) ->
      Printf.sprintf "Tyvar(%s,%s)" id (Kind.show kind)
    end "; " xs
end

(* 6 Unification and Matching *)
module Unify = struct
  open List
  open Kind
  open Type
  open Subst
  
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
end

(* 7 Type Classes, Predicates and Qualified Types *)
module Pred = struct
  open List
  open Kind
  open Type
  open Subst


  (* 7.1 Basic definitions *)
  type pred = IsIn of Id.id * type_

  let p (IsIn(s, t)) =
    s  ^ " " ^ (Type.show t)

  let ps pred =
    Pre.show_list p ", " pred

  type 't qual = Qual of pred list * 't

  let p_qual q =
    begin match q with
      | Qual(preds,ty) -> ps preds ^ " => " ^ Type.show ty
    end

  let predApply (s:subst) (pred:pred):pred =
    match pred with
    | IsIn(i, t) -> IsIn(i, Subst.typeApply s t)

  let predTv (pred:pred):tyvar list =
    match pred with
    | IsIn(_, t) -> Subst.typeTv t

  let predsApply (s:subst) (xs:pred list):pred list =
    Subst.listApply predApply s xs

  let predsTv (xs:'a list) : tyvar list =
    Subst.listTv predTv xs

  let qualTypeApply (s:subst) (qual:type_ qual):type_ qual =
    match qual with
    | Qual(ps, t) -> Qual(predsApply s ps, Subst.typeApply s t)

  let qualTypeTv qual =
    match qual with
    | Qual(ps, t) ->
      Pre.union (predsTv ps) (Subst.typeTv t)

  let lift (m:type_->type_->'a) (p:pred) (p':pred):'a =
    match (p, p') with
    | IsIn(i, t), IsIn(i', t') ->
      if i = i' then m t t'
      else failwith "classes differ"

  let mguPred = lift Unify.mgu

  let matchPred = lift Unify.match_

  type inst = pred qual

  let p_inst i =
    begin match i with
    | Qual(preds,pred) -> Printf.sprintf "Qual(%s,%s)" (ps preds) (p pred)
    end

  type class_ = Id.id list * inst list

  let (==>) ps p = Qual(ps, p)

  (* 7.2 Class Environments *)

  type classEnv = {
    classes : (Id.id -> class_);
    defaults : type_ list;
  }

  let initialEnv :classEnv = {
    classes = (fun i -> raise Not_found);
    defaults = [tInteger; tDouble]
  }

  let modify (ce:classEnv) i c =
    { ce with classes = fun j -> if i = j then c else ce.classes j; }

  let super (ce:classEnv) i = fst (ce.classes i)

  let insts (ce:classEnv) i = snd (ce.classes i)

  let defined (ce:classEnv) i =
    try
      ignore (ce.classes i);
      true
    with Not_found -> false

  type envTransformer = classEnv -> classEnv

  let addClass i is : envTransformer =
    fun (ce:classEnv) ->
      if defined ce i then failwith "class already defined"
      else if exists (fun i -> not (defined ce i)) is then
        failwith "superclass not defined"
      else modify ce i (is, [])

  let (<:>) (f : envTransformer) (g : envTransformer) : envTransformer =
    fun (ce:classEnv) -> g (f ce)

  let addCoreClasses :envTransformer =
        addClass "Eq" []
    <:> addClass "Ord" ["Eq"]
    <:> addClass "Show" []
    <:> addClass "Read" []
    <:> addClass "Bounded" []
    <:> addClass "Enum" []
    <:> addClass "Functor" []
    <:> addClass "Monad" []

  let addNumClasses :envTransformer =
        addClass "Num" ["Eq"; "Show"]
    <:> addClass "Real" ["Num"; "Ord"]
    <:> addClass "Fractional" ["Num"]
    <:> addClass "Integral" ["Real"; "Enum"]
    <:> addClass "RealFrac" ["Real"; "Fractional"]
    <:> addClass "Floating" ["Fractional"]
    <:> addClass "RealFloat" ["RealFrac"; "Floating"]

  let addPreludeClasses :envTransformer =
    addCoreClasses <:> addNumClasses

  let overlap (p:pred) (q:pred) : bool =
    try
      ignore (mguPred p q);
      true
    with _ -> false

  let addInst ps (IsIn(i, _) as p) : envTransformer =
    fun (ce:classEnv) ->
      if not (defined ce i) then failwith "no class for instance";
      let its = insts ce i in
      let qs = map (fun (Qual(_, q)) -> q) its in
      if exists (overlap p) qs then failwith "overlapping instance";      
      let c = super ce i, Qual(ps, p) :: its in
      modify ce i c

  let exampleInsts : envTransformer =
        addPreludeClasses
    <:> addInst [] (IsIn("Ord", tUnit))
    <:> addInst [] (IsIn("Ord", tChar))
    <:> addInst [] (IsIn("Ord", tInt))
    <:> addInst [IsIn("Ord", TVar(Tyvar("a", Star)));
                 IsIn("Ord", TVar(Tyvar("b", Star)))]
                (IsIn("Ord", pair (TVar(Tyvar("a", Star)))
                                  (TVar(Tyvar("b", Star)))))

  (* 7.3 Entailment *)

  let rec bySuper (ce:classEnv) (IsIn(i, t) as p) =
    p :: concat (map (fun i' -> bySuper ce (IsIn(i', t))) (super ce i))

  let byInst (ce:classEnv) (IsIn(i, t) as p) =
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

  let rec entail (ce:classEnv) ps p =
    exists (mem p) (map (bySuper ce) ps) ||
    match byInst ce p with
    | None -> false
    | Some qs -> for_all (entail ce ps) qs

  (* 7.4 Context Reduction *)

  let inHnf (p:pred):bool =
    match p with
    | IsIn(_, t) ->
      let rec hnf = function
        | TVar _ -> true
        | TCon _ -> false
        | TAp(t, _) -> hnf t
        | TGen _ -> failwith "context reduction on generic variable"
      in
      hnf t

  let rec toHnfs (ce:classEnv) ps = concat (map (toHnf ce) ps)
  and toHnf (ce:classEnv) p =
    if inHnf p then [p]
    else
      match byInst ce p with
      | None -> failwith "context reduction"
      | Some ps -> toHnfs ce ps

  let simplify (ce:classEnv) ps =
    let rec loop rs = function
      | [] -> rs
      | p :: ps ->
        if entail ce (rs @ ps) p then loop rs ps
        else loop (p :: rs) ps in
    loop [] ps

  let reduce (ce:classEnv) ps =
    simplify ce (toHnfs ce ps)

  let scEntail (ce:classEnv) ps p =
    exists (mem p) (map (bySuper ce) ps)

end

(* 8 Type Schemes *)
module Scheme = struct

  open List
  open Kind
  open Type
  open Pred

  type scheme = Forall of kind list * type_ qual

  let show (Forall(ks, qt):scheme) =
    Printf.sprintf "Forall(%s, %s)" (Kind.show_list ks) (Pred.p_qual qt)

  let schemeApply (s:Subst.subst) (Forall(ks, qt):scheme):scheme =
    Forall(ks, qualTypeApply s qt)

  let schemeTv (Forall(_, qt):scheme):tyvar list = qualTypeTv qt

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

  let toScheme (t:type_) :scheme = Forall([], (Qual([], t)))

end

(* 9 Assumptions *)
module Assump = struct

  open Scheme
  open Type
  open Kind
  
  type assump = Assump of Id.id * scheme

  let show (Assump(i, sc):assump) : string =
    Printf.sprintf "Assump(%s, %s)" i (Scheme.show sc)

  let show_list (assumps: assump list) : string =
    Pre.show_list show ";" assumps

  let assumpApply (s:Subst.subst) (Assump(i, sc):assump) : assump =
    Assump(i, schemeApply s sc)

  let assumpTv (Assump(_, sc):assump):Type.tyvar list =
    schemeTv sc

  let assumpsApply (s:Subst.subst) (ass:assump list): assump list =
    Subst.listApply assumpApply s ass

  let assumpsTv (ass:assump list): Type.tyvar list =
    Subst.listTv assumpTv ass

  let find (i:Id.id) (ass:assump list): scheme =
    let Assump(_, sc) = List.find begin fun (Assump(i', _)) ->
      i = i'
    end ass in
    sc

end


(* 10 A Type Inference Monad *)
module TIMonad = struct

  open Kind
  open Type
  open Subst
  open Pred
  open Scheme
  open List

  type ti = subst ref * int ref

  let show (({contents=subst},{contents=i}):ti) : string =
    Printf.sprintf "({contents=%s}, {contents=%d})" (Subst.show subst) i

  let runTI (f : ti -> 'a):'a =
    f (ref nullSubst, ref 0)

  let getSubst ((s, _) : ti):subst = !s

  let extSubst ((s, _) : ti) (u:subst) :unit = s := u @@ !s

  let unify (ti:ti) (t1:type_) (t2:type_) :unit=
    let s:subst = getSubst ti in
    let u = Unify.mgu (typeApply s t1) (typeApply s t2) in
    extSubst ti u

  let newTVar ((_, n) : ti) k : type_ =
    let v = Tyvar(Id.enumId !n, k) in
    incr n;
    TVar v

  let rec typeInst (ts:type_ list) (t:type_):type_ = 
    begin match t with
      | TAp(l, r) -> TAp(typeInst ts l, typeInst ts r)
      | TGen n -> nth ts n
      | t -> t
    end

  let listInst (inst: type_ list -> 'a -> 'a)
    (ts : type_ list) (xs : 'a list) : 'a list =
    map (inst ts) xs

  let predInst (ts: type_ list) (IsIn(c, t): pred):pred =
    IsIn(c, typeInst ts t)

  let qualTypeInst (ts:type_ list)
    (Qual(ps, t):type_ qual):type_ qual =
    Qual(listInst predInst ts ps, typeInst ts t)

  let freshInst (ti:ti) (Forall(ks, qt):scheme) : type_ qual =
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
#load "nums.cma"
module Lit = struct
  open Kind
  open Type
  open Pred
  open TIMonad
  open Infer
  open Big_int
  open Num
  type literal =
    | LitInt of big_int
    | LitChar of char
    | LitRat of num
    | LitStr of string

  let tiLit (ti:ti) (lit:literal):pred list * type_ =
    begin match lit with
      | LitChar _ -> ([], tChar)
      | LitInt _ ->
        let v = newTVar ti Star in
        ([IsIn("Num", v)], v)
      | LitStr _ -> ([], tString)
      | LitRat _ ->
        let v = newTVar ti Star in
        ([IsIn("Fractional", v)], v)
    end

  let _ =
    runTI begin fun ti ->
      let lit = LitInt (big_int_of_string "123") in
      let (preds,ty) = tiLit ti lit in
      Printf.printf "preds = %s ty = %s\n" (Pred.ps preds) (Type.show ty)
    end

end

(* 11.2 Patterns *)
module Pat = struct
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

  let _ =
    runTI begin fun ti ->
      let pat = PWildcard in
      let (preds, assumps, ty) = tiPat ti pat in
      Printf.printf "tiPat %s %s %s\n" (Pred.ps preds) (Assump.show_list assumps) (Type.show ty)
    end
end

(* 11.3 Expressions
 * 11.4 Alternatives
 * 11.5 From Types to Type Schemes
 * 11.6 Binding Groups *)
module TIMain = struct
  open List
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

  let show_amb ((tv,preds):ambiguity) =
    Printf.sprintf "ambibuity(%s, %s)" (Subst.show_tyvar tv) (Pred.ps preds)

  let show_ambs ambs =
    Pre.show_list show_amb ";" ambs

  let ambiguities (vs:tyvar list) (ps:pred list) : ambiguity list =
    let vs' = Pre.diff (predsTv ps) vs in
    map begin fun v ->
      (v, filter begin fun p ->
        mem v (predTv p)
      end ps)
    end vs'

  let _ =
    let tvs = [Tyvar("a", Star)] in
    let preds = [IsIn("Num", tInt);IsIn("B", tInt)] in
    let ambs = ambiguities tvs preds in
    Printf.printf "ambs %s\n" (show_ambs ambs)


  let numClasses : Id.id list = [
    "Num"; "Integral"; "Floating"; "Fractional"; "Real"; "RealFloat";
    "RealFrac"]

  let _ =
    Printf.printf "numClasses = \n";
    List.iter begin fun id ->
      Printf.printf "  %s\n" id
    end numClasses

  let stdClasses : Id.id list = [
    "Eq"; "Ord"; "Show"; "Read"; "Bounded"; "Enum"; "Ix"; "Functor"; "Monad";
    "MonadPlus"] @ numClasses

  let _ =
    Printf.printf "stdClasses = \n";
    List.iter begin fun id ->
      Printf.printf "  %s\n" id
    end stdClasses

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

  let _ =
    let tv = Tyvar("a", Star) in
    let preds = [IsIn("Num", tInt);IsIn("B", tInt)] in
    Printf.printf "a ----\n";
    let amb = (tv, preds) in
    Printf.printf "b ----\n";
    let ce = addNumClasses initialEnv in
    Printf.printf "c ----\n";
    let ts = candidates ce amb in
    Printf.printf "ts = %s\n" (Type.show_list ts)

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

  type expr =
    | Var of Id.id
    | Lit of literal
    | Const of assump
    | Ap of expr * expr
    | Let of bindGroup * expr
    (*| Lam of alt*)
    (*| If of expr * expr * expr*)
    (*| Case of expr * (Pat * Expr) list*)
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
      (*| Lam(alt) -> tiAlt ti ce as_ alt *)
      (*| If(e, e1, e2) ->
        let (ps,t) = tiExpr ti ce as_ e in
        unify ti t tBool;
        let (ps1,t1) = tiExpr ti ce as_ e1 in
        let (ps2,t2) = tiExpr ti ce as_ e2 in
        unify ti t1 t2;
        (ps @ ps1 @ ps2, t1)*)
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
        let vss = map typeTv ts' in
        let gs = Pre.diff (Pre.fold_left1 Pre.union vss) fs in
        let (ds, rs) = split ce fs (Pre.fold_left1 Pre.intersect vss) ps' in
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
end


