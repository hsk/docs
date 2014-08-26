## 7.1 基本的な定義

ここでは、2つの型pred,qualと
9つの関数をpredApply,predsApply,qualTypeApply,predTv,predsTv,qualTypeTv,lift,mguPred,matchPredを説明します。

### type pred

	  type pred = IsIn of Id.id * type_

IsIn("Num", TVar(Tyvar("a", Star)))と書いて、 Num aの意味になるようなものがpredです。
述語でいいのかな？

### p 関数

predの印字関数です。

	  let p (IsIn(s, t)) =
	    s  ^ " " ^ (Type.show t)

#### 使用例

	  let _ =
	    let ty = TVar(Tyvar("a", Star)) in
	    let pred = IsIn("Num", ty) in
	    Printf.printf "pred %s\n" (p pred)

### ps 関数

pred listの印字関数です。

	  let ps preds =
	    Pre.show_list p ", " preds

#### 使用例

	let _ =
	  let ty = TVar(Tyvar("a", Star)) in
	  let preds = [IsIn("Num", ty);IsIn("B", ty)] in
	  Printf.printf "preds %s\n" (ps preds)

### type qual

	  type 't qual = Qual of pred list * 't

qualは制約みたいなやつで、predを使って例えば、以下のような関数の型を表せる物です。

	Num a => a -> a

制約付きの型と言えるのかな？predはリストなので、制約ない物も２個、３個と複数の制約も付けられます。



### p_qual 関数

qualの印字関数です。

	  let p_qual q =
	    begin match q with
	      | Qual(preds,ty) -> ps preds ^ " => " ^ Type.show ty
	    end

#### 使用例

	  let _ =

	    (* (Num a) => a -> Int *)

	    let ty = TVar(Tyvar("a", Star)) in
	    let pred = IsIn("Num", ty) in
	    Printf.printf "pred %s\n" (p pred);
	    (* Qual *)
	    let q = Qual([pred], fn ty tInt) in
	    Printf.printf "qual = %s\n" (p_qual q)

### predApply 関数

ここから説明する6つの関数は同じような処理です。
pred, pred list qual に対する Kind クラスの関数を定義しているだけです。

	  let predApply (s:subst) (pred:pred):pred =
	    begin match pred with
	      | IsIn(i, t) -> IsIn(i, Subst.typeApply s t)
	    end

pred に対して、 subst を渡して、型の箇所を Subst.typeApply で変更して返すだけです。

#### 使用例

	  let _ =
	    let s = [Tyvar("a", Star), tInt] in
	    let pred = IsIn("Num", TVar(Tyvar("a", Star))) in
	    let pred2 = predApply s pred in
	    Printf.printf "pred2 = %s\n" (p pred2)

### predsApply 関数

	  let predsApply (s:subst) (xs:pred list):pred list =
	    Subst.listApply predApply s xs

pred listに対して、substを渡して、型の箇所をSubst.typeApplyで変更して返すだけです。

#### 使用例

	  let _ =
	    let s = [Tyvar("a", Star), tInt] in
	    let preds = [IsIn("Num", TVar(Tyvar("a", Star)))] in
	    let preds2 = predsApply s preds in
	    Printf.printf "preds2 = %s\n" (ps preds2)

### qualTypeApply 関数

	  let qualTypeApply (s:subst) (qual:type_ qual):type_ qual =
	    begin match qual with
	      | Qual(ps, t) -> Qual(predsApply s ps, Subst.typeApply s t)
	    end

qualに対して、substを渡してやって、型の箇所をtypeApplyにpred listのところはpredsApplyを呼び出して型を適用するだけです。

	let _ =
	  let ty = TVar(Tyvar("a", Star)) in
	  let pred = IsIn("Num", ty) in
	  let q = Qual([pred], fn ty tInt) in
	  Printf.printf "qual = %s\n" (p_qual q);
	  let qual2 = qualTypeApply ((Tyvar("a", Star)) +-> tInt) q in
	  Printf.printf "qual2 = %s\n" (p_qual qual2)


### predTv 関数

ここから３つはtyvar listを取得する関数の定義です

predからtyvarのリストを取得します。Subst.typeTvに型を渡すだけです。

	  let predTv (pred: pred): tyvar list =
	    begin match pred with
	      | IsIn(_, t) -> Subst.typeTv t
	    end

#### 使用例

	  let _ =
	    let pred = IsIn("Num", TVar(Tyvar("a", Star))) in
	    let tvs = predTv pred in
	    Printf.printf "tvs = %s\n" (Subst.show_tyvar_list tvs)

### predsTv 関数

Subst.listTvにpredTvとリストを渡してるだけです。

	  let predsTv (xs: 'a list): tyvar list =
	    Subst.listTv predTv xs

#### 使用例

	  let _ =
	    let preds = [IsIn("Num", TVar(Tyvar("a", Star)))] in
	    let tvs = predsTv preds in
	    Printf.printf "tvs = %s\n" (Subst.show_tyvar_list tvs)

### qualTypeTv 関数

	  let qualTypeTv qual =
	    begin match qual with
	      | Qual(ps, t) ->
	        Pre.union (predsTv ps) (Subst.typeTv t)
	    end

predsTvにpsを渡し、Sybst.typeTvにtを渡して、そのユニオンを取って返しています。
同じtyvarはまとめるわけです。

#### 使用例

	  let _ =
	    let ty = TVar(Tyvar("a", Star)) in
	    let pred = IsIn("Num", ty) in
	    let q = Qual([pred], fn ty tInt) in
	    let tvs = qualTypeTv q in
	    Printf.printf "tvs = %s\n" (Subst.show_tyvar_list tvs)

### lift 関数

	  let lift (m: type_->type_->'a) (p: pred) (p': pred): 'a =
	    begin match (p, p') with
	      | IsIn(i, t), IsIn(i', t') ->
	        if i = i' then m t t'
	        else failwith "classes differ"
	    end

２つの型を受け取って'aを返す関数と、2つのpredを受け取って'aを返します。
内部では、predのiを比較して違うならエラーにします。
iが同じなら、predのtを受け取った関数に渡して結果を返します。

#### 使用例

以下のmguPredとmatchPredを参照してください。

### mguPred 関数

	  let mguPred = lift Unify.mgu

Unify.mguをliftしてるだけです。
Pred.overlapで使用しています。

#### 使用例

	  let _ =
	    let pred1 = IsIn("Num", TVar(Tyvar("a", Star))) in
	    let pred2 = IsIn("Num", TVar(Tyvar("a", Star))) in
	    let s = mguPred pred1 pred2 in
	    Printf.printf "mguPred = %s\n" (Subst.show s)


### matchPred 関数

	  let matchPred = lift Unify.match_

Unify.match_をliftしてるだけです。
Pred.byInstで使用しています。

#### 使用例

	  let _ =
	    let pred1 = IsIn("Num", TVar(Tyvar("a", Star))) in
	    let pred2 = IsIn("Num", TVar(Tyvar("a", Star))) in
	    let s = matchPred pred1 pred2 in
	    Printf.printf "matchPred = %s\n" (Subst.show s)
