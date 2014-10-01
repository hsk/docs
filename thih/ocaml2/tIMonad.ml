(* 10 A Type Inference Monad *)
(*|
    >>> open TIMonad;;
    
*)

  open Kind
  open Type
  open Subst_
  open Pred
  open Scheme
  open List

  type ti = subst ref * int ref

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

(*|

## ti

    >>> let subst = [(Tyvar("a", Star), tInt)] ;;
    val subst : (Type.tyvar * Type.type_) list = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]


    >>> let ti = (ref subst, ref 1) ;;
    val ti : (Type.tyvar * Type.type_) list ref * int ref = ({contents = [(Tyvar ("a", Star), TCon (Tycon ("Int", Star)))]}, {contents = 1})


    >>> ti =
      ({contents=[(Tyvar("a", Star), TCon(Tycon("Int", Star)))]}, {contents=1});;
    - : bool = true


## runTI

    >>> let n = runTI begin fun (subst, n) ->
      n := !n + 1;
      !n
    end ;;
    val n : int = 1


    >>> n = 1;;
    - : bool = true


## getSubst

    >>> runTI begin fun ti ->
      let subst = getSubst(ti) in
      subst = []
    end;;
    - : bool = true


## extSubst

    >>> runTI begin fun ti ->
        let subst = [(Tyvar("a", Star), tInt)] in
        extSubst(ti)(subst);
        let subst2 = getSubst(ti) in

        subst2 = [(Tyvar("a", Star), TCon(Tycon("Int", Star)))]
      end;;
    - : bool = true


## unify

    >>> runTI begin fun ti ->
        let t1 = TVar(Tyvar("a", Star)) in
        unify(ti)(t1)(tInt);

        t1 = TVar(Tyvar("a", Star))
      end;;
    - : bool = true


## newTVar

    >>> runTI begin fun ti ->
        let t1 = newTVar(ti)(Star) in
        unify(ti)(t1)(tInt);
        let t2:type_ = typeApply(getSubst(ti))(t1) in
        (t1 = TVar(Tyvar("v0", Star)), t2 = tInt)
      end ;;
    - : bool * bool = (true, true)


## freshInst

    >>> runTI begin fun ti ->
        let ty = TVar(Tyvar("a", Star)) in
        let sc = toScheme(ty) in

        let tq:type_ qual = freshInst(ti)(sc) in

        (sc = Forall([], Qual([], TVar(Tyvar("a", Star)))),
        tq = Qual([], TVar(Tyvar("a", Star))))
      end ;;
    - : bool * bool = (true, true)


*)
