## 11.3 Expressions 式

Next we describe type inference for expressions, represented by the Expr datatype:

次に式の場合、Expr のデータ型によって表される型の推論を記述します。

	  data Expr = Var   Id
	            | Lit   Literal
	            | Const Assump
	            | Ap    Expr Expr
	            | Let   BindGroup Expr

The Var and Lit constructors are used to represent variables and literals, respectively.

Var と点灯のコンス トラクターは、変数とリテラル文字列を表す使用されます。

The Const constructor is used to deal with named constants, such as the constructor or selector functions associated with a particular datatype or the member functions that are associated with a particular class.

Const のコンス トラクターは、特定のデータ型または特定のクラスに関連付けられているメンバー関数に関連するコンス トラクターまたはセレクター関数などの名前付き定数に対処するために使用されます。

We use values of type Assump to supply a name and type scheme, which is all the information that we need for the purposes of type inference.

型 Assump の値を使用して、名前を指定をスキームは、型の推論の目的に必要なすべての情報を入力します。

Function application is represented using the Ap constructor, while Let is used to represent let expressions. (Note that the definition of the BindGroup type, used here to represent binding groups, will be delayed to Section 11.6.3.) Of course, Haskell has a much richer syntax of expressions-which includes l-abstractions, case expressions, conditionals, list comprehensions, and do-notation-but they all have simple translations into Expr values.

関数の適用は Let let 式を表すに使用される間 Ap コンス トラクターを使用して表されます。(セクション 11.6.3 にバインド グループを表すに使用される BindGroup 型の定義が遅れることに注意してください)。もちろん、Haskell より豊かな構文の式が含まれています l 抽象化、case 式、条件リストの内包表記と do 表記が、彼らはすべて Expr の値への簡単な翻訳があります。

For example, a l-expression like \x->e can be rewritten using a local definition as let f x = e in f, where f is a new variable.

たとえば、l 式ように \x->e としてローカル定義を使用して書き換えることができますように f x = e、f、f は、新しい変数の。

Type inference for expressions is quite straightforward:

式の型の推論はかなり簡単です。

	  tiExpr                       :: Infer Expr Type
	  tiExpr ce as (Var i)          = do sc         <- find i as
	                                     (ps :=> t) <- freshInst sc
	                                     return (ps, t)
	  tiExpr ce as (Const (i:>:sc)) = do (ps :=> t) <- freshInst sc
	                                     return (ps, t)
	  tiExpr ce as (Lit l)          = do (ps,t) <- tiLit l
	                                     return (ps, t)
	  tiExpr ce as (Ap e f)         = do (ps,te) <- tiExpr ce as e
	                                     (qs,tf) <- tiExpr ce as f
	                                     t       <- newTVar Star
	                                     unify (tf `fn` t) te
	                                     return (ps++qs, t)
	  tiExpr ce as (Let bg e)       = do (ps, as') <- tiBindGroup ce as bg
	                                     (qs, t)   <- tiExpr ce (as' ++ as) e
	                                     return (ps ++ qs, t)

The final case here, for Let expressions, uses the function tiBindGroup presented in Section 11.6.3, to generate a list of assumptions as' for the variables defined in bg.

最後のケースはここでは、Let 式を使用してセクション 11.6.3 で示した関数 tiBindGroup の前提条件のリストを生成するの bg で定義されている変数として。

All of these variables are in scope when we calculate a type t for the body e, which also serves as the type of the whole expression.

これらの変数はスコープ全体の式の型としても提供しています体 e 型 t を計算するとき。



