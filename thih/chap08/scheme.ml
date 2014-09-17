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

  let _ =
    let ty = TVar(Tyvar("a", Star)) in
    let pred = IsIn("Num", ty) in
    let sc = Forall([],Qual([pred],ty)) in
    Printf.printf "scheme = %s\n" (show sc)

  let schemeApply (s:Subst.subst) (Forall(ks, qt):scheme):scheme =
    Forall(ks, qualTypeApply s qt)

  let _ =
    let ty = TVar(Tyvar("a", Star)) in
    let pred = IsIn("Num", ty) in
    let sc = Forall([],Qual([pred],ty)) in
    let subst = [Tyvar("a", Star), tInt] in
    let sc = schemeApply subst sc in
    Printf.printf "scheme = %s\n" (show sc)

  let schemeTv (Forall(_, qt):scheme):tyvar list = qualTypeTv qt

  let _ =
    let ty = TVar(Tyvar("a", Star)) in
    let pred = IsIn("Num", ty) in
    let sc = Forall([],Qual([pred],ty)) in
    let tvs = schemeTv sc in
    Printf.printf "tvs = %s\n" (Subst.show_tyvar_list tvs)

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

  let _ =
    let tyvar = Tyvar("a", Star) in
    let ty = TVar(Tyvar("a", Star)) in
    let pred = IsIn("Num", ty) in
    let qual = Qual([pred], fn ty tInt) in
    let sc = quantify [tyvar] qual in
    Printf.printf "scheme = %s\n" (show sc)


  let toScheme (t:type_) :scheme = Forall([], (Qual([], t)))

  let _ =
    let ty = TVar(Tyvar("a", Star)) in
    let sc = toScheme ty in
    Printf.printf "scheme = %s\n" (show sc)
end
