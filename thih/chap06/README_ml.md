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

todo:mguの説明これでいいのか？

mguとはmost general unifyの略で、unifyをちゃんとやるくらいの意味。

mguはvarBind関数とペアで計算します。

	  let rec mgu (t1:type_) (t2:type_):subst =
	    match t1, t2 with
	    | TAp(l, r), TAp(l', r') ->
	      let s1 = mgu l l' in
	      let s2 = mgu (typeApply s1 r) (typeApply s1 r') in
	      s2 @@ s1

TAp同士なら、左側のmguを再帰的に計算し、substを求め、右側の型で代入を実行した後、mguを呼び出してさらに代入を求めます。
最後に、 @@で代入をまとめて返します。

	    | TVar u, t | t, TVar u -> varBind u t

TVarのマッチングの箇所でvarBindは使って、新しい代入を作ります。

	    | TCon tc1, TCon tc2 when tc1 = tc2 -> nullSubst

TConなら、既に型は決まっているので、同じなら代入はありません。

	    | _ -> failwith "types do not unify"


それ以外なら、unifyに失敗なのでエラーです。


todo: @@の処理とmergeの処理を比べて考える。

#### 使用例

todo:使用例を書く

### varBind関数

mguから呼ばれる関数で、２つの型を１つにまとめる。

	  and varBind (u:tyvar) (t:type_):subst =
	    if t = TVar u then nullSubst
	    else if mem u (typeTv t) then failwith "occurs check fails"
	    else if tyvarKind u <> typeKind t then failwith "kinds do not match"
	    else u +-> t

tがTVar uだったら、既に同じ型であるから、新たな代入は産まれない。
typeTv tに uが含まれてたら、おかしいのでエラーである。occurs checkという。
tyvarKind uと typeKind tが違う場合は、ダメであるのでエラーである。同じ種類でないと行けない。
それ以外なら、u は tであるので、代入を作って返す。

### match_関数

match関数はmguに似ていますが、varBindを使わず、@@の代わりにmergeを使っています。

	  let rec match_ (t1:type_) (t2:type_):subst =
	    match t1, t2 with
	    | TAp(l, r), TAp(l', r') ->
	      let sl = match_ l l' in
	      let sr = match_ r r' in
	      merge sl sr

左のマッチをして、右のマッチをして、両方の代入をmergeでまとめて返します。

	    | TVar u, t when tyvarKind u = typeKind t -> u +-> t

左側がTVarなら、右側の型とのkindが同じなら、代入を作って返します。

	    | TCon tc1, TCon tc2 when tc1 = tc2 -> nullSubst

両方がTConで、中の値が同じなら代入なしです。

	    | _ -> failwith "types do not match"

それ以外はエラーとなります。左側がTVarの時は代入があり得るけど、右側がTVarならエラーです。

todo: mergeの処理を調べる。


### 使用例

	let _ =
	  (* mgu *)
	  (* varBind *)
	  (* match_ *)
	  ()

todo:例を追加する
