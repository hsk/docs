## 7.4 Context Reduction

ここでは5つの関数inHnf,inHnfs,toHnf,simplify,reduceを説明します。

### inHnf 関数

TConがpredに含まれているかどうかを返します。

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

### inHnfs 関数

TConがpsに含まれているかどうかの結果をまとめて返します。
含まれていない場合は、byInstの結果の値を返します。

	  let rec toHnfs (ce:classEnv) ps = concat (map (toHnf ce) ps)

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

### reduce 関数

psをtoHnfsしたあとsimplifyして返します。

	  let reduce (ce:classEnv) ps =
	    simplify ce (toHnfs ce ps)

### scEntail 関数

psにbySuperした結果のリストにpが含まれているかをチェックします。

	  let scEntail (ce:classEnv) ps p =
	    exists (mem p) (map (bySuper ce) ps)

todo:使用例を書く

