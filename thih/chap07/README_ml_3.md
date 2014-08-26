## 7.3 Entailment

ここでは、3つの関数bySuper,byInst,entailについて説明します。

### bySuper 関数

bySuper関数はクラス環境とpredを受け取って、superからpred listを取得し返します。

	  let rec bySuper (ce:classEnv) (IsIn(i, t) as p):pred list =
	    let pss = map begin fun i' ->
	      bySuper ce (IsIn(i', t))
	    end (super ce i) in
	    p :: concat pss


#### 使用例

	let ps = bySuper (exampleInsts initialEnv) (IsIn("Num", TVar(Tyvar("a", Star)))) in
	Printf.printf "ps = %s" show_ps ps;

とかが動けばOKかな。

todo: ちゃんとやってみる。

### byInst 関数

クラス環境とpredを受け取り、instsからpred listを作成しpred listのoptionを返します。

	  let byInst (ce: classEnv) (IsIn(i, t) as p): pred list option =
	    let tryInst (Qual(ps, h)) =
	      begin try
	       let u = matchPred h p in
	       Some (map (predApply u) ps)
	      with
	        _ -> None
	      end
	    in
	    let rec msum =
	      begin function
	        | [] -> None
	        | None :: xs -> msum xs
	        | x :: _ -> x
	      end
	    in
	    msum (map tryInst (insts ce i))

#### 使用例

	let ps = byInst (exampleInsts initialEnv) (IsIn("Num", TVar(Tyvar("a", Star)))) in
	Printf.printf "ps = %s" show_ps ps;

とかが動けばOKかな。

todo: ちゃんとやってみる。

### entail 関数

クラス環境とpred listとpredを受け取ってpredがインスタンスなのかどうかを判定して返します。

	  let rec entail (ce:classEnv) (ps:pred list) (p:pred):bool =
	    exists (mem p) (map (bySuper ce) ps) ||
	    begin match byInst ce p with
	      | None -> false
	      | Some qs -> for_all (entail ce ps) qs
	    end

#### 使用例

	let ps = entail (exampleInsts initialEnv) (IsIn("Num", TVar(Tyvar("a", Star)))) in
	Printf.printf "ps = %s" show_ps ps;

とかが動けばOKかな。

todo: ちゃんとやってみる。
