# 6 単一化とマッチング


## Unifyモジュール

	(* 6 Unification and Matching *)
	module Unify = struct
	  open List
	  open Kind
	  open Type
	  open Subst
	  ...
	end

ここでは、mgu,varBind,match_の３つの関数を説明します。

### mgu関数

todo:mguの説明を書く。

mguはvarBind関数とペアで計算します。

	  let rec mgu (t1:type_) (t2:type_):subst =
	    match t1, t2 with
	    | TAp(l, r), TAp(l', r') ->
	      let s1 = mgu l l' in
	      let s2 = mgu (typeApply s1 r) (typeApply s1 r') in
	      s2 @@ s1
	    | TVar u, t | t, TVar u -> varBind u t
	    | TCon tc1, TCon tc2 when tc1 = tc2 -> nullSubst
	    | _ -> failwith "types do not unify"

TVarのマッチングの箇所でvarBindは使われています。

	    | TVar u, t | t, TVar u -> varBind u t

todo:mguは逆方向にもマッチするはずなのでそれが分かると嬉しい。

### varBind関数

	  and varBind (u:tyvar) (t:type_):subst =
	    if t = TVar u then nullSubst
	    else if mem u (typeTv t) then failwith "occurs check fails"
	    else if tyvarKind u <> typeKind t then failwith "kinds do not match"
	    else u +-> t

todo:説明がないので書く。

### match_関数

match関数はmguに似ていますが、varBindを使わず、@@の代わりにmergeを使っています。

	  let rec match_ (t1:type_) (t2:type_):subst =
	    match t1, t2 with
	    | TAp(l, r), TAp(l', r') ->
	      let sl = match_ l l' in
	      let sr = match_ r r' in
	      merge sl sr
	    | TVar u, t when tyvarKind u = typeKind t -> u +-> t
	    | TCon tc1, TCon tc2 when tc1 = tc2 -> nullSubst
	    | _ -> failwith "types do not match"

todo:例がないと分からないよ。

### 使用例

	let _ =
	  (* mgu *)
	  (* varBind *)
	  (* match_ *)
	  ()

todo:例を追加する
