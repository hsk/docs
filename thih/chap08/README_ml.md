# 8 型スキーム

### Scheme モジュール
	(* 8 Type Schemes *)
	module Scheme = struct

	  open List
	  open Kind
	  open Type
	  open Pred
	  ...
	end

ここでは、1つの型schemeと4つの関数schemeApply,schemeTv,quantify,toSchemeを読みます。


### type scheme

	  type scheme = Forall of kind list * type_ qual

型スキームはカインドのリストと型制限の対です。

### show 関数

	  let show (Forall(ks, qt):scheme) =
	    Printf.sprintf "Forall(%s, %s)" (Kind.show_list ks) (Pred.p_qual qt)

#### 使用例

	  let _ =
	    let ty = TVar(Tyvar("a", Star)) in
	    let pred = IsIn("Num", ty) in
	    let sc = Forall([],Qual([pred],ty)) in
	    Printf.printf "scheme = %s\n" (show sc)

### schemeApply 関数

todo:説明を書く

	  let schemeApply (s:Subst.subst) (Forall(ks, qt):scheme):scheme =
	    Forall(ks, qualTypeApply s qt)

#### 使用例

	  let _ =
	    let ty = TVar(Tyvar("a", Star)) in
	    let pred = IsIn("Num", ty) in
	    let sc = Forall([],Qual([pred],ty)) in
	    let subst = [Tyvar("a", Star), tInt] in
	    let sc = schemeApply subst sc in
	    Printf.printf "scheme = %s\n" (show sc)

### schemeTv 関数

todo:説明を書く

	  let schemeTv (Forall(_, qt):scheme):tyvar list = qualTypeTv qt

#### 使用例

	  let _ =
	    let ty = TVar(Tyvar("a", Star)) in
	    let pred = IsIn("Num", ty) in
	    let sc = Forall([],Qual([pred],ty)) in
	    let tvs = schemeTv sc in
	    Printf.printf "tvs = %s\n" (Subst.show_tyvar_list tvs)


### quantify 関数

todo:説明を書く

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

#### 使用例

	  let _ =
	    let tyvar = Tyvar("a", Star) in
	    let ty = TVar(Tyvar("a", Star)) in
	    let pred = IsIn("Num", ty) in
	    let qual = Qual([pred], fn ty tInt) in
	    let sc = quantify [tyvar] qual in
	    Printf.printf "scheme = %s\n" (show sc)

### toScheme 関数

	  let toScheme (t:type_) :scheme = Forall([], (Qual([], t)))

todo:説明を書く

#### 使用例

	  let _ =
	    let ty = TVar(Tyvar("a", Star)) in
	    let sc = toScheme ty in
	    Printf.printf "scheme = %s\n" (show sc)
