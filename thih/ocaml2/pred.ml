(* 7 Type Classes, Predicates and Qualified Types *)

  open List
  open Kind
  open Type
  open Subst_

  (* 7.1 Basic definitions *)
  type pred = IsIn of Id.id * type_

  type 't qual = Qual of pred list * 't

  let predApply (s:subst) (pred:pred):pred =
    match pred with
    | IsIn(i, t) -> IsIn(i, Subst_.typeApply s t)

  let predTv (pred:pred):tyvar list =
    match pred with
    | IsIn(_, t) -> Subst_.typeTv t

  let predsApply (s:subst) (xs:pred list):pred list =
    Subst_.listApply predApply s xs

  let predsTv (xs:'a list) : tyvar list =
    Subst_.listTv predTv xs

  let qualTypeApply (s:subst) (qual:type_ qual):type_ qual =
    match qual with
    | Qual(ps, t) -> Qual(predsApply s ps, Subst_.typeApply s t)

  let qualTypeTv qual =
    match qual with
    | Qual(ps, t) ->
      Pre.union (predsTv ps) (Subst_.typeTv t)

  let lift (m:type_->type_->'a) (p:pred) (p':pred):'a =
    match (p, p') with
    | IsIn(i, t), IsIn(i', t') ->
      if i = i' then m t t'
      else failwith "classes differ"

  let mguPred = lift Unify.mgu

  let matchPred = lift Unify.match_

  type inst = pred qual

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

(*|

    >>> open Pred;;
    

## pred

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Type.type_ = TVar (Tyvar ("a", Star))


    >>> let pred = IsIn("Num", ty) ;;
    val pred : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))


    >>> pred = IsIn("Num", TVar(Tyvar("a", Star)));;
    - : bool = true


## pred list

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Type.type_ = TVar (Tyvar ("a", Star))


    >>> let preds = [IsIn("Num", ty); IsIn("B", ty)] ;;
    val preds : Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star))); IsIn ("B", TVar (Tyvar ("a", Star)))]


    >>> preds =
        [
          IsIn("Num", TVar(Tyvar("a", Star)));
          IsIn("B", TVar(Tyvar("a", Star)))
        ];;
    - : bool = true


## pred & qual

(Num a) => a -> Int

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Type.type_ = TVar (Tyvar ("a", Star))


    >>> let pred = IsIn("Num", ty) ;;
    val pred : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))


## Qual

    >>> let q = Qual([pred], fn(ty)(tInt)) ;;
    val q : Type.type_ Pred.qual = Qual ([IsIn ("Num", TVar (Tyvar ("a", Star)))], TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TVar (Tyvar ("a", Star))), TCon (Tycon ("Int", Star))))


    >>> let Qual(_, q2) = q ;;
    val q2 : Type.type_ = TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TVar (Tyvar ("a", Star))), TCon (Tycon ("Int", Star)))


    >>> q2 =
             TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))),
                  TVar (Tyvar ("a", Star))), TCon (Tycon ("Int", Star)));;
    - : bool = true


## predApply

    >>> let subst = [(Tyvar("a", Star), tInt)] ;;
    val subst : (Type.tyvar * Type.type_) list = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]


    >>> let pred = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val pred : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))


    >>> let pred2 = predApply(subst)(pred) ;;
    val pred2 : Pred.pred = IsIn ("Num", TCon (Tycon ("Int", Star)))


## predTv

    >>> let pred = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val pred : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))


    >>> let tvs = predTv(pred) ;;
    val tvs : Type.tyvar list = [Tyvar ("a", Star)]


## predsApply

    >>> let subst = [(Tyvar("a", Star), tInt)] ;;
    val subst : (Type.tyvar * Type.type_) list = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]


    >>> let preds = [IsIn("Num", TVar(Tyvar("a", Star)))] ;;
    val preds : Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]


    >>> let preds2 = predsApply(subst)(preds) ;;
    val preds2 : Pred.pred list = [IsIn ("Num", TCon (Tycon ("Int", Star)))]


    >>> preds2 = [IsIn("Num", TCon(Tycon("Int", Star)))];;
    - : bool = true


## predsTv

    >>> let preds = [IsIn("Num", TVar(Tyvar("a", Star)))] ;;
    val preds : Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]


    >>> let tvs = predsTv(preds) ;;
    val tvs : Type.tyvar list = [Tyvar ("a", Star)]


    >>> tvs = [Tyvar("a", Star)];;
    - : bool = true


## qualTypeApply


    >>> let subst = Tyvar("a", Star) +-> tInt ;;
    val subst : Subst_.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]


    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Type.type_ = TVar (Tyvar ("a", Star))


    >>> let pred = IsIn("Num", ty) ;;
    val pred : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))


    >>> let qual = Qual([pred], fn(ty)(tInt)) ;;
    val qual : Type.type_ Pred.qual = Qual ([IsIn ("Num", TVar (Tyvar ("a", Star)))], TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TVar (Tyvar ("a", Star))), TCon (Tycon ("Int", Star))))


    >>> let qual2 = qualTypeApply(subst)(qual) ;;
    val qual2 : Type.type_ Pred.qual = Qual ([IsIn ("Num", TCon (Tycon ("Int", Star)))], TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TCon (Tycon ("Int", Star))), TCon (Tycon ("Int", Star))))


    >>> qual = Qual(
        [
          IsIn("Num", TVar(Tyvar("a", Star)))],
        TAp(TAp(TCon(Tycon("(->)", Kfun(Star, Kfun(Star, Star)))),
          TVar(Tyvar("a", Star))), TCon(Tycon("Int", Star))));;
    - : bool = true


    >>> qual2 = Qual(
        [
          IsIn("Num", TCon(Tycon("Int", Star)))],
        TAp(TAp(TCon(Tycon("(->)", Kfun(Star, Kfun(Star, Star)))),
          TCon(Tycon("Int", Star))), TCon(Tycon("Int", Star))));;
    - : bool = true


## qualTypeTv

    >>> let ty = TVar(Tyvar("a", Star)) ;;
    val ty : Type.type_ = TVar (Tyvar ("a", Star))


    >>> let pred = IsIn("Num", ty) ;;
    val pred : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))


    >>> let qual = Qual([pred], fn(ty)(tInt)) ;;
    val qual : Type.type_ Pred.qual = Qual ([IsIn ("Num", TVar (Tyvar ("a", Star)))], TAp (TAp (TCon (Tycon ("(->)", Kfun (Star, Kfun (Star, Star)))), TVar (Tyvar ("a", Star))), TCon (Tycon ("Int", Star))))


    >>> let tvs = qualTypeTv(qual) ;;
    val tvs : Type.tyvar list = [Tyvar ("a", Star)]


    >>> tvs = [Tyvar("a", Star)] ;;
    - : bool = true


## mguPred

    >>> let pred1 = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val pred1 : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))


    >>> let pred2 = IsIn("Num", tInt) ;;
    val pred2 : Pred.pred = IsIn ("Num", TCon (Tycon ("Int", Star)))


    >>> let subst = mguPred(pred1)(pred2) ;;
    val subst : Subst_.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]


    >>> subst = [(Tyvar("a",Star),TCon(Tycon("Int",Star)))];;
    - : bool = true


    >>> let subst2 = mguPred(pred1)(pred1) ;;
    val subst2 : Subst_.subst = []


## matchPred

    >>> let pred1 = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val pred1 : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))


    >>> let pred2 = IsIn("Num", tInt) ;;
    val pred2 : Pred.pred = IsIn ("Num", TCon (Tycon ("Int", Star)))


    >>> let subst = matchPred(pred1)(pred2) ;;
    val subst : Subst_.subst = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]


    >>> subst = [(Tyvar("a", Star), tInt)];;
    - : bool = true


    >>> let subst2 = matchPred(pred1)(pred1) ;;
    val subst2 : Subst_.subst = [(Tyvar ("a", Star), TVar (Tyvar ("a", Star)))]


    >>> subst2 = [(Tyvar("a", Star), TVar(Tyvar("a", Star)))];;
    - : bool = true


## Inst

    >>> let inst = Qual([ IsIn("Ord", tUnit); IsIn("Ord", tChar)], IsIn("Ord", tChar)) ;;
    val inst : Pred.pred Pred.qual = Qual ([IsIn ("Ord", TCon (Tycon ("()", Star))); IsIn ("Ord", TCon (Tycon ("Char", Star)))], IsIn ("Ord", TCon (Tycon ("Char", Star))))


    >>> inst = Qual(
        [
          IsIn("Ord", TCon(Tycon("()", Star)));
          IsIn("Ord", TCon(Tycon("Char", Star)))],
        IsIn("Ord", TCon(Tycon("Char", Star))));;
    - : bool = true


## class_ ==>

    >>> let (cls:class_) = (
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
      ) ;;
    val cls : Pred.class_ = (["Eq"], [Qual ([], IsIn ("Ord", TCon (Tycon ("()", Star)))); Qual ([], IsIn ("Ord", TCon (Tycon ("Char", Star)))); Qual ([], IsIn ("Ord", TCon (Tycon ("Int", Star)))); Qual ([IsIn ("Ord", TVar (Tyvar ("a", Star))); IsIn ("Ord", TVar (Tyvar ("b", Star)))], IsIn ("Ord", TAp (TAp (TCon (Tycon ("(,)", Kfun (Star, Kfun (Star, Star)))), TVar (Tyvar ("a", Star))), TVar (Tyvar ("b", Star)))))])


# 7.2 Class Environments

## modify

    >>> let ce: classEnv = modify(initialEnv)("ABC")(["A"], [[] ==> IsIn("Ord", tUnit)]) ;;
    val ce : Pred.classEnv = {classes = <fun>; defaults = [TCon (Tycon ("Integer", Star)); TCon (Tycon ("Double", Star))]}


    >>> ce.defaults = [TCon(Tycon("Integer", Star)); TCon(Tycon("Double", Star))];;
    - : bool = true


## super

    >>> let ce = modify(initialEnv)("ABC")(["A"], [[] ==> IsIn("Ord", tUnit)]) ;;
    val ce : Pred.classEnv = {classes = <fun>; defaults = [TCon (Tycon ("Integer", Star)); TCon (Tycon ("Double", Star))]}


    >>> let s = super(ce)("ABC") ;;
    val s : Id.id list = ["A"]


    >>> s = ["A"];;
    - : bool = true


## insts

    >>> let ce = modify(initialEnv)("ABC")(["A"], [[] ==> IsIn("Ord", tUnit)]) ;;
    val ce : Pred.classEnv = {classes = <fun>; defaults = [TCon (Tycon ("Integer", Star)); TCon (Tycon ("Double", Star))]}


    >>> let s = insts(ce)("ABC") ;;
    val s : Pred.inst list = [Qual ([], IsIn ("Ord", TCon (Tycon ("()", Star))))]


## defined

    >>> let ce = modify(initialEnv)("ABC")(["A"], [[] ==> IsIn("Ord", tUnit)]) ;;
    val ce : Pred.classEnv = {classes = <fun>; defaults = [TCon (Tycon ("Integer", Star)); TCon (Tycon ("Double", Star))]}


    >>> let s = defined(ce)("ABC") ;;
    val s : bool = true


## addClass

    >>> let et: envTransformer = addClass("Eq")([]) ;;
    val et : Pred.envTransformer = <fun>


    >>> let ce = et(initialEnv) ;;
    val ce : Pred.classEnv = {classes = <fun>; defaults = [TCon (Tycon ("Integer", Star)); TCon (Tycon ("Double", Star))]}


    >>> ce.defaults ;;
    - : Type.type_ list = [TCon (Tycon ("Integer", Star)); TCon (Tycon ("Double", Star))]


## <:>

    >>> let et1: envTransformer = addClass("Eq")([]) ;;
    val et1 : Pred.envTransformer = <fun>


    >>> let et2: envTransformer = addClass("Eq2")([]) ;;
    val et2 : Pred.envTransformer = <fun>


    >>> let et3: envTransformer = et1 <:> et2 ;;
    val et3 : Pred.envTransformer = <fun>


    >>> (et3 initialEnv).defaults ;;
    - : Type.type_ list = [TCon (Tycon ("Integer", Star)); TCon (Tycon ("Double", Star))]


    >>> let et4: envTransformer = addClass("Eq")([]) <:> addClass("Eq2")([]) ;;
    val et4 : Pred.envTransformer = <fun>


    >>> (et4(initialEnv)).defaults ;;
    - : Type.type_ list = [TCon (Tycon ("Integer", Star)); TCon (Tycon ("Double", Star))]


## overlap

    >>> let pred1 = IsIn("Ord", tUnit) ;;
    val pred1 : Pred.pred = IsIn ("Ord", TCon (Tycon ("()", Star)))


    >>> let pred2 = IsIn("Ord", tChar) ;;
    val pred2 : Pred.pred = IsIn ("Ord", TCon (Tycon ("Char", Star)))


    >>> overlap(pred1)(pred2) ;;
    - : bool = false


    >>> overlap(pred1)(pred1) ;;
    - : bool = true


# 7.3 Entailment

## bySuper

    >>> let preds = bySuper(exampleInsts(initialEnv))(IsIn("Num", TVar(Tyvar("a", Star)))) ;;
    val preds : Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star))); IsIn ("Eq", TVar (Tyvar ("a", Star))); IsIn ("Show", TVar (Tyvar ("a", Star)))]


## byInst

    >>> let preds = byInst(exampleInsts(initialEnv))(IsIn("Num", TVar(Tyvar("a", Star)))) ;;
    val preds : Pred.pred list option = None


## entail

    >>> let p = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val p : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))


    >>> let ps = [p] ;;
    val ps : Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]


    >>> let result = entail(exampleInsts(initialEnv))(ps)(p) ;;
    val result : bool = true


# 7.4 Context Reduction

## inHnf

    >>> let r = inHnf(IsIn("Num", TVar(Tyvar("a", Star)))) ;;
    val r : bool = true


    >>> let r2 = inHnf(IsIn("Num", tInt)) ;;
    val r2 : bool = false


## toHnfs

    >>> let preds = [IsIn("Num", TVar(Tyvar("a", Star)))] ;;
    val preds : Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]


    >>> let preds2 = toHnfs(initialEnv)(preds) ;;
    val preds2 : Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]


## toHnf

    >>> let pred = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val pred : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))


    >>> let preds = toHnf(initialEnv)(pred) ;;
    val preds : Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]


## simplify

    >>> let pred = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val pred : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))


    >>> let preds = [pred] ;;
    val preds : Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]


    >>> let preds2 = simplify(exampleInsts(initialEnv))(preds) ;;
    val preds2 : Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]


## reduce

    >>> let pred = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val pred : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))


    >>> let preds = [pred] ;;
    val preds : Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]


    >>> let preds2 = reduce(exampleInsts(initialEnv))(preds) ;;
    val preds2 : Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]


## scEntail

    >>> let pred = IsIn("Num", TVar(Tyvar("a", Star))) ;;
    val pred : Pred.pred = IsIn ("Num", TVar (Tyvar ("a", Star)))


    >>> let preds = [pred] ;;
    val preds : Pred.pred list = [IsIn ("Num", TVar (Tyvar ("a", Star)))]


    >>> let result = scEntail(exampleInsts(initialEnv))(preds)(pred) ;;
    val result : bool = true


*)