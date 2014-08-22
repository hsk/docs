## 7.1 基本的な定義

ここでは、2つの型pred,qualと
9つの関数をpredApply,predsApply,qualTypeApply,predTv,predsTv,qualTypeTv,lift,mguPred,matchPredを説明します。

### type pred

	  (* 7.1 Basic definitions *)
	  type pred = IsIn of Id.id * type_

#### 使用例

	let _ =
	  let ty = TVar(Tyvar("a", Star)) in
	  let pred = IsIn("Num", ty) in
	  Printf.printf "pred %s\n" (Pred.p pred);

### type qual

	  type 't qual = Qual of pred list * 't

#### 使用例

	let _ =

	  (* (Num a) => a -> Int *)

	  let ty = TVar(Tyvar("a", Star)) in
	  let pred = IsIn("Num", ty) in
	  Printf.printf "pred %s\n" (Pred.p pred);
	  (* Qual *)
	  let q = Qual([pred], fn ty tInt) in
	  Printf.printf "qual = %s\n" (p_qual q);

### predApply 関数

ここから説明する6つの関数は同じような処理です。

	  let predApply (s:subst) (pred:pred):pred =
	    begin match pred with
	      | IsIn(i, t) -> IsIn(i, Subst.typeApply s t)
	    end

### predsApply 関数


	  let predsApply (s:subst) (xs:pred list):pred list =
	    Subst.listApply predApply s xs

### qualTypeApply 関数

	  let qualTypeApply (s:subst) (qual:type_ qual):type_ qual =
	    begin match qual with
	      | Qual(ps, t) -> Qual(predsApply s ps, Subst.typeApply s t)
	    end

### predTv 関数

	  let predTv (pred:pred):tyvar list =
	    begin match pred with
	      | IsIn(_, t) -> Subst.typeTv t
	    end

### predsTv 関数

	  let predsTv (xs:'a list) : tyvar list =
	    Subst.listTv predTv xs

### qualTypeTv 関数

	  let qualTypeTv qual =
	    begin match qual with
	      | Qual(ps, t) ->
	        Pre.union (predsTv ps) (Subst.typeTv t)
	    end

### lift 関数

	  let lift (m:type_->type_->'a) (p:pred) (p':pred):'a =
	    begin match (p, p') with
	      | IsIn(i, t), IsIn(i', t') ->
	        if i = i' then m t t'
	        else failwith "classes differ"
	    end

２つの型を受け取って'aを返す関数と、2つのpredを受け取って'aを返します。
内部では、predのiを比較して違うならエラーにします。
iが同じなら、predのtを受け取った関数に渡して結果を返します。

### mguPred 関数

	  let mguPred = lift Unify.mgu

Unify.mguをliftしてるだけです。


### matchPred 関数

	  let matchPred = lift Unify.match_

Unify.match_をliftしてるだけです。
