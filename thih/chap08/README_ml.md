# 8 型スキーム

### Scheme モジュール
	(* 8 Type Schemes *)
	module Scheme = struct

	  open Kind
	  open Type
	  open Pred
	  ...
	end

ここでは、1つの型schemeと4つの関数schemeApply,schemeTv,quantify,toSchemeについて説明します。


### type scheme

	  type scheme = Forall of kind list * type_ qual

型スキームはカインドのリストと型制限の対です。

#### 例


### schemeApply 関数

	  let schemeApply (s:Subst.subst) (Forall(ks, qt):scheme):scheme =
	    Forall(ks, qualTypeApply s qt)

### schemeTv 関数

	  let schemeTv (Forall(_, qt):scheme):tyvar list = qualTypeTv qt

### quantify 関数

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

### toScheme 関数

	  let toScheme (t:type_) :scheme = Forall([], (Qual([], t)))
