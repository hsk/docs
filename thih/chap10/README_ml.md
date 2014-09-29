# 10 型推論モナド


### TIMonadモジュール

	(* 10 A Type Inference Monad *)
	module TIMonad = struct

	  open Kind
	  open Type
	  open Subst
	  open Pred
	  open Scheme
	  ...
	end

ここでは、1つの型tiと10個の関数runTI,getSubst,extSubst,unify,newTVar,typeInst,listInst,predInst,qualTypeInst,freshInstを読みます。

モナドを作って実行し、Substを取得更新し、型変数を作成し、新しいインスタンスを作ります。

### type ti

todo:説明を書く

	  type ti = subst ref * int ref

### show 関数

	  let show (({contents=subst},{contents=i}):ti) : string =
	    Printf.sprintf "({contents=%s}, {contents=%d})" (Subst.show subst) i

#### 使用例

	  let _ =
	    let subst = [Tyvar("a", Star), tInt] in
	    let ti = (ref subst, ref 1) in
	    Printf.printf "ti = %s\n" (show ti)

### runTI 関数

	OCamlでわざわざモナド作っているけど、中身は一意制約なかんじの只のref変数２つで、ユーザー関数には見せない

	  let runTI (f : ti -> 'a):'a =
	    f (ref nullSubst, ref 0)

#### 使用例

	  let _ =
	    let n = runTI begin fun (subst, n) ->
	      n := !n + 1;
	      !n
	    end in
	    Printf.printf "ti = %d\n" n

### getSubst 関数

モナド内のsubstを取り出して返します。

	  let getSubst ((s, _) : ti):subst = !s

#### 使用例

	  let _ =
	    runTI begin fun ti ->
	      let subst = getSubst ti in
	      Printf.printf "subst=%s\n" (Subst.show subst)
	    end

### extSubst 関数

モナド内のsubstを@@で更新します。

	  let extSubst ((s, _) : ti) (u:subst) :unit = s := u @@ !s

#### 使用例

	  let _ =
	    runTI begin fun ti ->
	      let subst = [Tyvar("a", Star), tInt] in
	      extSubst ti subst;
	      let subst = getSubst ti in
	      Printf.printf "subst=%s\n" (Subst.show subst)
	    end

### unify 関数

モナド内のsubstを使って、Unify.mguを呼び出し、モナド内のsubstを更新します。

	  let unify (ti:ti) (t1:type_) (t2:type_) :unit=
	    let s:subst = getSubst ti in
	    let u = Unify.mgu (typeApply s t1) (typeApply s t2) in
	    extSubst ti u

#### 使用例

	  let _ =
	    runTI begin fun ti ->
	      let t1 = TVar(Tyvar("a", Star)) in
	      unify ti t1 tInt;
	      Printf.printf "t1=%s\n" (Type.show t1)
	    end

### newTVar 関数


型変数を作成し、モナド内のカウンタを１つ進めます。

	  let newTVar ((_, n) : ti) k : type_ =
	    let v = Tyvar(Id.enumId !n, k) in
	    incr n;
	    TVar v

#### 使用例

	  let _ =
	    runTI begin fun ti ->
	      let t1 = newTVar ti Star in
	      unify ti t1 tInt;
	      Printf.printf "t1=%s\n" (Type.show t1)
	    end

ここまでが、TIMonadの基本操作です。

### typeInst 関数

ここからは、freshInstに関する関数です。

typeInstは、型をインスタンス化する関数です。
型スキームを実体化する型リストと、型を受け取って、型内のTGen nは、型のリストのn番目の型変数に変換して返します。
TApの場合は、再帰的に、それ以外の場合はそのまま返却します。

	  let rec typeInst (ts:type_ list) (t:type_):type_ = 
	    begin match t with
	      | TAp(l, r) -> TAp(typeInst ts l, typeInst ts r)
	      | TGen n -> nth ts n
	      | t -> t
	    end

#### 使用例

predInstと、qualTypeInstでのみ使用。
	
### listInst 関数

inst(型のリスト)とaを受け取ってaを返す関数に受け取ったリストとaをmapで適用して返却します。

	  let listInst (inst: type_ list -> 'a -> 'a)
	    (ts : type_ list) (xs : 'a list) : 'a list =
	    map (inst ts) xs

#### 使用例

次の次のqualTypeInstでのみ使用。

### predInst 関数

型のリストとpredを受け取って、predの型をtypeInstを適用して返却します。

	  let predInst (ts: type_ list) (IsIn(c, t): pred):pred =
	    IsIn(c, typeInst ts t)

#### 使用例

次のqualTypeInstでのみ使用。
	
### qualTypeInst 関数

型リストとtypeのQualを受け取って、QualにpredInstとtypeInstを適用して返します。

	  let qualTypeInst (ts:type_ list)
	    (Qual(ps, t):type_ qual):type_ qual =
	    Qual(listInst predInst ts ps, typeInst ts t)

#### 使用例

次のfreshInstでのみ使用。
	
### freshInst 関数

新しい、型変数をスキームから作成して、qualTypeInstを適用して返却します。
typeInstは型のインスタンス化をする処理で、型スキームからインスタンス化する必要があるわけです。

	  let freshInst (ti:ti) (Forall(ks, qt):scheme) : type_ qual =
	    let ts = map (newTVar ti) ks in
	    qualTypeInst ts qt

#### 使用例

	  let _ =
	    runTI begin fun ti ->
	      let ty = TVar(Tyvar("a", Star)) in
	      let sc = toScheme ty in
	      Printf.printf "scheme = %s\n" (Scheme.show sc);
	      let tq = freshInst ti sc in
	      Printf.printf "freshInst = %s\n" (Pred.p_qual tq)
	    end
