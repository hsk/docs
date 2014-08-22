## 7.2 クラス環境

ここでは、4つの型inst, class_, classEnv, envTransformerと
9つの関数super,insts,defined,modify,initialEnv,(<:>),addClass,overlap,addInst、
4つの環境トランスフォーマーaddCoreClasses,addNumClasses,addPreludeClasses,exampleInsts
を説明します。

環境トランスフォーマーは実は只のclassEnvを受け取ってclassEnvを返す関数です。

### type inst

	  type inst = pred qual

instはpredのqualです。

todo:説明がよくわからない。

### type class_

	  type class_ = Id.id list * inst list

class_はidリストとinstリストの対です。

#### 例

	  (["Eq"], [[] :=> IsIn "Ord" tUnit;
	            [] :=> IsIn "Ord" tChar;
	            [] :=> IsIn "Ord" tInt;
	            [IsIn "Ord" (TVar (Tyvar "a" Star));
	             IsIn "Ord" (TVar (Tyvar "b" Star))]
	               :=> IsIn "Ord" (pair (TVar (Tyvar "a" Star))
	                                    (TVar (Tyvar "b" Star)))])

todo:haskellのコードなので、ocamlにして実行してみる。

### type classEnv

	  type classEnv = {
	    classes : (Id.id -> class_);
	    defaults : type_ list;
	  }

クラスの環境を表す型です。

classesはidからクラスを求める関数です。
defaultsは型のリストです。

### super 関数

	  let super (ce:classEnv) i = fst (ce.classes i)

classEnvのclassesを呼び出して、class_を取得して、Id.id listを取得します。

### insts 関数

	  let insts (ce:classEnv) i = snd (ce.classes i)

classEnvのclassesを呼び出して、class_を取得して、inst listを取得します。

### defined 関数

	  let defined (ce:classEnv) i =
	    begin try
	      ignore (ce.classes i);
	      true
	    with
	      Not_found -> false
	    end

classEnv内を調べて、値があればtrueをなければfalseを返します。

### modify 関数

classEnvを更新を更新し返します。

	  let modify (ce:classEnv) i c =
	    { ce with classes = begin fun j ->
	        if i = j
	        then c
	        else ce.classes j
	      end ;
	    }

内部実装は、classes関数は元のclasses関数を置き換えて、i=jならその値を返す関数を設定します。
要するに関数のリストになっているわけです。

### initialEnv 関数

classEnvの初期値です。

	  let initialEnv :classEnv = {
	    classes = begin fun i ->
	      raise Not_found
	    end ;
	    defaults = [tInteger; tDouble]
	  }

classes関数が例外を投げます。
defaultsはintegerとdoubleの型が入っています。

### type envTransformer

	  type envTransformer = classEnv -> classEnv

classEnvを受け取って、classEnvを返す関数をenvTransformerという型であるとします。

### (<:>)演算子

環境トランスフォーマーを合成する演算子です

	  let (<:>) (f : envTransformer) (g : envTransformer) : envTransformer =
	    fun (ce:classEnv) -> g (f ce)

環境トランスフォーマーを2つ受け取って環境トランスフォーマーを返す関数で、
実装は、環境を受け取って、fを実行した後gを実行して返します。

### addClass関数

addClassは環境トランスフォーマーを作成する関数です。

	  let addClass i is : envTransformer =
	    begin fun (ce:classEnv) ->
	      if defined ce i then failwith "class already defined"
	      else if exists (fun i -> not (defined ce i)) is then
	        failwith "superclass not defined"
	      else modify ce i (is, [])
	    end

実装は、関数を作り、環境を受け取ります。
ceにiが定義されてたらエラーを発生させます。
isはスーパークラスのリストで、スーパークラスはceにすべて含まれているかをチェックします。
スーパークラスが１つでもなければエラーです。
エラーがなければ、環境を更新し返却します。

### addCoreClasses 環境トランスフォーマー

addCoreClassesはコアのクラスを追加する環境トランスフォーマーです。
要するに、環境を受け取って、コアの関数を追加して返す関数です。
addClassを使って環境トランスフォーマーを作り、<:>を使って環境トランスフォーマーを合成しているわけです。

	  let addCoreClasses :envTransformer =
	        addClass "Eq" []
	    <:> addClass "Ord" ["Eq"]
	    <:> addClass "Show" []
	    <:> addClass "Read" []
	    <:> addClass "Bounded" []
	    <:> addClass "Enum" []
	    <:> addClass "Functor" []
	    <:> addClass "Monad" []

### addNumClasses 環境トランスフォーマー

addNumClassesは数値に関するクラスを追加する環境トランスフォーマーです。

	  let addNumClasses :envTransformer =
	        addClass "Num" ["Eq"; "Show"]
	    <:> addClass "Real" ["Num"; "Ord"]
	    <:> addClass "Fractional" ["Num"]
	    <:> addClass "Integral" ["Real"; "Enum"]
	    <:> addClass "RealFrac" ["Real"; "Fractional"]
	    <:> addClass "Floating" ["Fractional"]
	    <:> addClass "RealFloat" ["RealFrac"; "Floating"]

### addPreludeClasses 環境トランスフォーマー

addPreludeClassesはコアのクラスと数値に関するクラスを追加する環境トランスフォーマーです。

	  let addPreludeClasses :envTransformer =
	    addCoreClasses <:> addNumClasses

要するに、このaddPreludeClassesを使って、initialEnvを変換して上げれば、プレリュードの環境が出来上がるわけです。

### overlap 関数

これは、2つのpredをmguPredして問題なければtrueを問題あればfalseを返します。

	  let overlap (p:pred) (q:pred) : bool =
	    begin try
	      ignore (mguPred p q);
	      true
	    with
	      _ -> false
	    end

### addInst 関数

addInstはPredのリストとQualを受け取って環境トランスフォーマーを返します。

	  let addInst ps (IsIn(i, _) as p) : envTransformer =
	    begin fun (ce:classEnv) ->
	      if not (defined ce i) then failwith "no class for instance";
	      let its = insts ce i in
	      let qs = map (fun (Qual(_, q)) -> q) its in
	      if exists (overlap p) qs then failwith "overlapping instance";      
	      let c = super ce i, Qual(ps, p) :: its in
	      modify ce i c
	    end

exampleInstsがその使用例です。

### exampleInsts 環境トランスフォーマー

	  let exampleInsts : envTransformer =
	        addPreludeClasses
	    <:> addInst [] (IsIn("Ord", tUnit))
	    <:> addInst [] (IsIn("Ord", tChar))
	    <:> addInst [] (IsIn("Ord", tInt))
	    <:> addInst [IsIn("Ord", TVar(Tyvar("a", Star)));
	                 IsIn("Ord", TVar(Tyvar("b", Star)))]
	                (IsIn("Ord", pair (TVar(Tyvar("a", Star)))
	                                  (TVar(Tyvar("b", Star)))))

