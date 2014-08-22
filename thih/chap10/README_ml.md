# 10 型推論モナド


### TIMonadモジュール

	(* 10 A Type Inference Monad *)
	module TIMonad = struct

	  open Kind
	  open Type
	  open Subst
	  open Pred
	  open Scheme
	  ...
	end

ここでは、1つの型tiと10個の関数runTI,getSubst,extSubst,unify,newTVar,typeInst,listInst,predInst,qualTypeInst,freshInstについて説明します。


### type ti

todo:説明を書く

	  type ti = subst ref * int ref

### runTI 関数

todo:説明を書く

	  let runTI (f : ti -> 'a):'a =
	    f (ref nullSubst, ref 0)

### getSubst 関数

todo:説明を書く

	  let getSubst ((s, _) : ti):subst = !s

### extSubst 関数

todo:説明を書く

	  let extSubst ((s, _) : ti) (u:subst) :unit = s := u @@ !s

### unify 関数

todo:説明を書く

	  let unify (ti:ti) (t1:type_) (t2:type_) :unit=
	    let s:subst = getSubst ti in
	    let u = Unify.mgu (typeApply s t1) (typeApply s t2) in
	    extSubst ti u

### newTVar 関数

todo:説明を書く

	  let newTVar ((_, n) : ti) k : type_ =
	    let v = Tyvar(Id.enumId !n, k) in
	    incr n;
	    TVar v

### typeInst 関数

todo:説明を書く

	  let rec typeInst (ts:type_ list) (t:type_):type_ = 
	    begin match t with
	      | TAp(l, r) -> TAp(typeInst ts l, typeInst ts r)
	      | TGen n -> nth ts n
	      | t -> t
	    end

### listInst 関数

todo:説明を書く

	  let listInst (inst: type_ list -> 'a -> 'a)
	    (ts : type_ list) (xs : 'a list) : 'a list =
	    map (inst ts) xs

### predInst 関数

todo:説明を書く

	  let predInst (ts: type_ list) (IsIn(c, t): pred):pred =
	    IsIn(c, typeInst ts t)

### qualTypeInst 関数

todo:説明を書く

	  let qualTypeInst (ts:type_ list)
	    (Qual(ps, t):type_ qual):type_ qual =
	    Qual(listInst predInst ts ps, typeInst ts t)

### freshInst 関数

todo:説明を書く

	  let freshInst (ti:ti) (Forall(ks, qt):scheme) : type_ qual =
	    let ts = map (newTVar ti) ks in
	    qualTypeInst ts qt

todo:使用例を書く
