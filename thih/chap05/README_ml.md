# 5 置換

## Substモジュール

	(* 5 Substitutions *)
	module Subst = struct
	  open Type
	  ...
	end

ここでは、1つの型substと
1つの値nullSubstと
9つの関数(+->), typeApply, typeTv, listApply, listTv, (@@), merge, show, show_tyvar_listを読みます。


### type subst

	  type subst = (tyvar * type_) list

substは tyvarとtype_の対のリストなので、substは

	[Tyvar("v1", Star),tInt; Tyvar("v2", Star),tChar]

のような値を取ります。


### nullSubst

	  let nullSubst : subst = []

todo:説明を書く

### (+->) 関数

	  let (+->) u t : subst = [(u, t)]

todo:説明を書く

### typeApply関数

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

todo:説明を書く

### typeTv関数

	  let rec typeTv (t:type_):tyvar list =
	    begin match t with
	      | TVar u -> [u]
	      | TAp(l, r) -> Pre.union (typeTv l) (typeTv r)
	      | _ -> []
	    end

todo:説明を書く

### listApply関数

	  let listApply (apply : subst -> 'a -> 'b) (s : subst) (xs:'a list):'b list =
	    List.map (apply s) xs

todo:説明を書く

### listTv関数

	  let listTv (tv:'a -> tyvar list) (xs:'a list) : tyvar list =
	    Pre.nub (List.concat (List.map tv xs))

todo:説明を書く

### (@@) 関数

2つのsubstを１つにまとめる処理です。

	  let (@@) (s1:subst) (s2 : subst) : subst =
	    List.map begin fun (u, t) ->
	      (u, typeApply s1 t)
	    end s2 @ s1

s2のtにmapでtypeApply s1を実行し、s1とs2を@で結合します。例外は投げられません。

### merge関数

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

こちらは、s1とs2を @で繋げるだけです。s1とs2の左辺の積集合を求めて、重なる箇所の型をs1とs2から求めて同じならokですが、違う時はエラーです。

2つのsubstを１つにまとめる処理です。 @@も2つのsubstをまとめる処理ですが、動きが違います。

### show 関数

	  let show (subst:subst):string =
	    Pre.show_list begin fun (Tyvar(id,kind),type_) ->
	      Printf.sprintf "Tyvar(%s,%s),%s" id (Kind.show kind) (Type.show type_)
	    end "; " subst

todo:説明を書く

### show_tyvar_list 関数

	  let show_tyvar_list xs :string =
	    Pre.show_list begin fun (Tyvar(id,kind)) ->
	      Printf.sprintf "Tyvar(%s,%s)" id (Kind.show kind)
	    end "; " xs

todo:説明を書く

#### 使用例

	let _ =
	  Printf.printf "nullSubst = %s\n" (Subst.show Subst.nullSubst);

	module TEST = struct
	  open Kind
	  open Type
	  open Subst

	  let _ =

	    (* nullSubst *)
	    Printf.printf "nullSubst = %s\n" (Subst.show nullSubst);

	    (* +-> *)
	    let subst = Tyvar("a", Star) +-> tInt in
	    Printf.printf "a * +-> tInt = %s\n" (Subst.show subst);
	    let subst = subst @ Tyvar("b", Star) +-> tChar in
	    Printf.printf "subst = %s\n" (Subst.show subst);

	    let showApply t1 =  
	      let t2 = typeApply subst t1 in
	      Printf.printf "%s apply %s\n" (Type.show t1) (Type.show t2)
	    in
	    let tva = TVar(Tyvar("a", Star)) in
	    let tvb = TVar(Tyvar("b", Star)) in
	    showApply tva;
	    showApply tvb;
	    let tap = TAp(tva,tvb) in
	    showApply tap;
	    Printf.printf "tva typeTv = %s\n" (show_tyvar_list (typeTv tva));
	    Printf.printf "tvb typeTv = %s\n" (show_tyvar_list (typeTv tvb));
	    Printf.printf "tap typeTv = %s\n" (show_tyvar_list (typeTv tap));

	    (* listApply *)

	    (* listTv *)

	    (* (@@) *)

	    (* merge *)

	end

todo:細かく分ける。
todo:listApply以降も例を書く

