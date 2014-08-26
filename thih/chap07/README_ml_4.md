## 7.4 Context Reduction

ここでは5つの関数inHnf,inHnfs,toHnf,simplify,reduceを説明します。

### inHnf 関数

TVarがpredに含まれているかどうかを返します。TGenがあったら例外です。

	  let inHnf (p:pred):bool =
	    begin match p with
	      | IsIn(_, t) ->
	        let rec hnf = begin function
	          | TVar _ -> true
	          | TCon _ -> false
	          | TAp(t, _) -> hnf t
	          | TGen _ -> failwith "context reduction on generic variable"
	        end
	        in
	        hnf t
	    end

#### 使用例

	  let _ =
	    let r = inHnf (IsIn("Num",TVar(Tyvar("a", Star)))) in
	    Printf.printf "inHnf %b\n" r; (* true *)
	    let r = inHnf (IsIn("Num",tInt)) in
	    Printf.printf "inHnf %b\n" r (* false *)

#### 結果

	inHnf true
	inHnf false

### inHnfs 関数

TConがpsに含まれているかどうかの結果をまとめて返します。
含まれていない場合は、byInstの結果の値を返します。

	  let rec toHnfs (ce:classEnv) ps = concat (map (toHnf ce) ps)

#### 使用例

	  let _ =
	    let preds = [IsIn("Num",TVar(Tyvar("a", Star)))] in
	    let preds = toHnfs initialEnv preds in
	    Printf.printf "toHnf %s\n" (ps preds)

#### 結果

	toHnf [Num TVar(Tyvar(a,*))]

### toHnf 関数

TConがpsに含まれているかどうかを調べて、あったらリストにして返します。
含まれていない場合は、byInstの結果を返します。

	  and toHnf (ce:classEnv) p =
	    if inHnf p then [p]
	    else
	      begin match byInst ce p with
	        | None -> failwith "context reduction"
	        | Some ps -> toHnfs ce ps
	      end

#### 使用例

	  let _ =
	    let pred = IsIn("Num",TVar(Tyvar("a", Star))) in
	    let preds = toHnf initialEnv pred in
	    Printf.printf "toHnf %s\n" (ps preds)

#### 結果

	toHnf [Num TVar(Tyvar(a,*))]

### simplify 関数

entailがfalseになるpredのみを返します。

	  let simplify (ce:classEnv) ps =
	    let rec loop rs = begin function
	      | [] -> rs
	      | p :: ps ->
	        if entail ce (rs @ ps) p then loop rs ps
	        else loop (p :: rs) ps
	    end
	    in
	    loop [] ps

#### 使用例

	  let _ =
	    let pred = IsIn("Num", TVar(Tyvar("a", Star))) in
	    let preds = [pred] in
	    let preds = simplify (exampleInsts initialEnv) preds in
	    Printf.printf "simplify = %s\n" (ps preds)

#### 結果

	simplify = [Num TVar(Tyvar(a,*))]

### reduce 関数

psをtoHnfsしたあとsimplifyして返します。

	  let reduce (ce:classEnv) ps =
	    simplify ce (toHnfs ce ps)

#### 使用例

	  let _ =
	    let pred = IsIn("Num", TVar(Tyvar("a", Star))) in
	    let preds = [pred] in
	    let preds = reduce (exampleInsts initialEnv) preds in
	    Printf.printf "reduce = %s\n" (ps preds)

#### 結果

	reduce = [Num TVar(Tyvar(a,*))]

### scEntail 関数

psにbySuperした結果のリストにpが含まれているかをチェックします。

	  let scEntail (ce:classEnv) ps p =
	    exists (mem p) (map (bySuper ce) ps)

#### 使用例

	  let _ =
	    let pred = IsIn("Num", TVar(Tyvar("a", Star))) in
	    let preds = [pred] in
	    let result = scEntail (exampleInsts initialEnv) preds pred in
	    Printf.printf "scEntail = %b\n" result

#### 結果

	scEntail = true
