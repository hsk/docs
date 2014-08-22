## 11.4 alt,expl,impl,bindGroup,expr

ここでは、6つの型alt,expr,impl,bindGroup,exprと
8つの関数restricted,tiSeq,tiExpr,tiAlt,tiAlts,tiExpl,tiImpls,tiBindGroupについて説明します。

### type alt

パターンリストと式をまとめた物がaltです。
Haskellの１つの式はいくつものパターンからなるわけです。

	  type alt = pat list * expr

### type expl

Idとスキームとaltのリストをまとめた物がexplです。

	  and expl = Id.id * scheme * alt list

1つの名前は、１つの型スキームと、いくつかのパターンから式へのリストで表せます。

### type impl

Idとaltのリストをまとめたものがimplです。

	  and impl = Id.id * alt list

実装は名前といくつかのパターンと式で表します。

### type bindGroup

explのリストとimplのリストのリストをまとめたものです。

	  and bindGroup = expl list * impl list list

### type expr

いわゆる式を表す型です。

	  and expr =
	    | Var of Id.id
	    | Lit of literal
	    | Const of assump
	    | Ap of expr * expr
	    | Let of bindGroup * expr
	    (*| Lam of alt*)
	    (*| If of expr * expr * expr*)
	    (*| Case of expr * (Pat * Expr) list*)

### restricted 関数

	  let restricted (bs : impl list):bool =
	    let simple (i, alts) = exists begin fun alt ->
	      Pre.isEmpty (fst alt)
	    end alts in
	    exists simple bs

### tiSeq 関数

リストに対する型推論を行います。tiBindGroupとtiProgramから使われます。

	  let rec tiSeq (f : ('bg, assump list) infer) : ('bg list, assump list) infer =
	    fun ti ce as_ ->
	      begin function
	        | [] -> ([], [])
	        | bs :: bss ->
	          let (ps, as') = f ti ce as_ bs in
	          let (qs, as'') = tiSeq f ti ce (as' @ as_) bss in
	          (ps @ qs, as'' @ as')
	      end

やっている事は再帰呼び出しして、リストに対して、fを呼びだし、返り値の'bg listと assump listをまとめて返すだけです。

#### tiSeqがここにある理由

この関数はHaskell版では、tiBindingGroupの後で説明されています。
しかし、OCamlでは前方参照ができません。
この関数をtiExpr等と並べてandで書くと、tiProgramから呼び出したときにエラーになってしまいます。
andで結合した関数群は１つの型のグループとして扱われるのです。

### tiExpr 関数

式に対する型推論で、tiExpr自身と、tiAltから使われます。

	  let rec tiExpr (ti:ti)(ce:classEnv)(as_:assump list)(expr: expr): pred list * type_ =
	    begin match expr with
	    ...
	    end

マッチングの中身は以下の通りです。

#### Var

	      | Var i ->
	        let sc = find i as_ in
	        let Qual(ps, t) = freshInst ti sc in
	        (ps, t)

変数は、as_からスキームを取得し、freshInstでインスタンスを作成し、結果を返します。

#### Const

	      | Const(Assump(_, sc)) ->
	        let Qual(ps, t) = freshInst ti sc in
	        (ps, t)

定数は、スキームから、freshInstを作って返します。

#### Lit

	      | Lit l -> tiLit ti l

リテラルはtiLitを呼び出して返します。

#### Ap

	      | Ap(e, f) ->
	        let (ps, te) = tiExpr ti ce as_ e in
	        let (qs, tf) = tiExpr ti ce as_ f in
	        let t = newTVar ti Star in
	        unify ti (fn tf t) te;
	        (ps @ qs, t)

関数適用は、eとf両方に対して、型推論し、
新しい型変数をtを作って、unifyで型を求めて返します。

#### Let

	      | Let(bg, e) ->
	        let (ps, as') = tiBindGroup ti ce as_ bg in
	        let (qs, t) = tiExpr ti ce (as' @ as_) e in
	        (ps @ qs, t)

LetはtiBindGroupで変数定義を型推論して、
tiExprで式を型推論し、返します。

#### Lam, If, Case

これ以降は省略します。

	      (*| Lam(alt) -> tiAlt ti ce as_ alt *)
	      (*| If(e, e1, e2) ->
	        let (ps,t) = tiExpr ti ce as_ e in
	        unify ti t tBool;
	        let (ps1,t1) = tiExpr ti ce as_ e1 in
	        let (ps2,t2) = tiExpr ti ce as_ e2 in
	        unify ti t1 t2;
	        (ps @ ps1 @ ps2, t1)*)
	      (*| Case(e, branches) ->
	        let (ps, t) = tiExpr ti ce as_ e in
	        let v = newTVar Star in
	        let tiBr (pat, f) =
	          let (ps, _as',t') = tiPat pat in
	          unify t t';
	          let (qs, t'') = tiExpr ce (_as' @ _as) f in
	          unify v t'';
	          (ps @ qs)
	        in
	        let pss = mapM tiBr branches in
	        (ps @ concat pss, v)
	      *)

これら３つはコメントアウトされています。

### tiAlt 関数

altに対する型推論で、tiAltsで使われています。

	  and tiAlt : (alt, type_) infer =
	    begin fun ti ce as_ (pats, e) ->
	      let (ps, as', ts) = tiPats ti pats in
	      let (qs, t) = tiExpr ti ce (as' @ as_) e in
	      (ps @ qs, fold_right fn ts t)
	    end

これは１つの関数で、パターンと式に対して型推論をして、型をまとめるときにはfnを使っています。

### tiAlts 関数

alt listに対する型推論でtiExplとtiImplsから使われています。

	  and tiAlts (ti:ti)(ce:classEnv)(as_:assump list)(alts:alt list)(t:type_):pred list =
	    let (ps, ts) = List.split (map (tiAlt ti ce as_) alts) in
	    iter (unify ti t) ts;
	    concat ps

mapでtiAltを呼び出し、List.splitで２つのリストに結果を分割します。
tsにはさらに、unifyしてます。
最後にリストのリストであるpsをconcatでまとめて返します。

### tiExpl 関数

explにたいする型推論で、tiBindGroupから使われています。

	  and tiExpl (ti:ti)(ce:classEnv)(as_:assump list)((i, sc, alts) : expl):pred list =
	    let Qual(qs, t) = freshInst ti sc in
	    let ps = tiAlts ti ce as_ alts t in
	    let s = getSubst ti in
	    let qs' = predsApply s qs in
	    let t' = typeApply s t in
	    let fs = assumpsTv (assumpsApply s as_) in
	    let gs = Pre.diff (typeTv t') fs in
	    let sc' = quantify gs (Qual(qs', t')) in
	    let ps' = filter (fun p -> not (entail ce qs' p)) (predsApply s ps) in
	    let (ds, rs) = split ce fs gs ps' in
	    if sc <> sc' then failwith "signature too general"
	    else if not (Pre.isEmpty rs) then failwith "context too weak"
	    else ds

型スキームから、新しいインスタンスを取得し、
tiAltsで型推論し、
substをgetSubstで取得し
predsApplyでqsに型を適用し、
typeApplyで型も適用し、
制約をassumpsApplyとassumpsTvで適用、
t'とfsの差分から、制約スキームを取り出し、うんぬん。これ以降理解してないので後で書く。

### tiImpls 関数

implsに対する型推論で、tiBindGroupから使われています。

	  and tiImpls : (impl list, assump list) infer =
	    begin fun ti ce as_ bs ->


この関数は長いので、最初に４つの変数を計算するようにします。

	      let (is, ps',ts',fs) =
	        let ts = map (fun _ -> newTVar ti Star) bs in
	        let (is, altss) = List.split bs in
	        let scs = map toScheme ts in
	        let as' = map2 (fun i sc -> Assump(i, sc)) is scs @ as_ in
	        let pss = map2 (tiAlts ti ce as') altss ts in

まず、bsに対して、必要な分の新しい型変数tsを作成します。
bsをList.splitでisとaltssに分割します。
tsは型スキームにscs変換します。
isとscsとas_をまとめたもので、mapしてAssumpのリストを作り、as'とします。
altssとtsとをmapでtiAltsを呼び出し方推論して、pssに入れます。

	        let s = getSubst ti in
	        let ps' = map (predApply s) (concat pss) in
	        let ts' = map (typeApply s) ts in
	        let fs = assumpsTv (assumpsApply s as_) in
	        (is, ps',ts',fs)
	      in

tiから代入を取り出しsに入れます。
pssをmapでpredApply sに適用して型推論結果を反映した、ps'を求めます。
tsをmapでtypeApply sに適用して型推論結果を反映した、ts'を求めます。
assumpsApplyでas_に型を適用して、assumpsTvを呼び出しfsを求めます。

なんというか、substを作って適用してと、面倒くさい事をしているなぁ。
これは、prolog的な型推論にすればもっと簡単になると思います。余裕があれば、後で作ってみよう。
続きを読みましょう。

	      let vss = map typeTv ts' in
	      let gs = Pre.diff (Pre.fold_left1 Pre.union vss) fs in
	      let (ds, rs) = split ce fs (Pre.fold_left1 Pre.intersect vss) ps' in

ts'にmapでtypeTvしてvssを求めます。
vssをunionしたものとfsのdiffをgsとします。
vssをintersectしたものとps'を、splitしたものをds,rsに入れます。

	      if restricted bs then

最後bsをrestrictedで判定してrestrictedなら
gsとrsのdiffをgs'に入れ
ts'を quantifyしてscs'を求め、
assumpして返します。

	        let gs' = Pre.diff gs (predsTv rs) in
	        let scs' = map (fun t -> quantify gs' (Qual([], t))) ts' in
	        (ds @ rs, map2 (fun i sc -> Assump(i, sc)) is scs')
	      else

最後bsをrestrictedで判定してrestrictedでないときは
diffは取らずに、
quantiyしてassumpして返します。

	        let scs' = map (fun t -> quantify gs (Qual(rs, t))) ts' in
	        (ds, map2 (fun i sc -> Assump(i, sc)) is scs')
	    end

rsの扱いが違うわけです。


### tiBindGroup 関数

bindGroupに対する型推論で、tiProgramとtiExprから使われています。

	  and tiBindGroup : (bindGroup, assump list) infer =
	    begin fun ti ce as_ (es, iss) ->
	      let as' = map (fun (v, sc, _) -> Assump(v, sc)) es in
	      let (ps, as'') = tiSeq tiImpls ti ce (as' @ as_) iss in
	      let qss = map (tiExpl ti ce (as'' @ as' @ as_)) es in
	      (ps @ concat qss, as'' @ as')
	    end

esからassumpを作り、
issをtiImplsで型推論し、
esをtiExplで型推論して、結果をまとめて返しています。
