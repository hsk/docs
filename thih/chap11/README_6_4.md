### 11.6.4 Top-level Binding Groups トップレベル バインディング グループ

#### type Program

At the top-level, a Haskell program can be thought of as a list of binding groups:

トップレベルは、Haskell プログラムすることができます見なすグループをバインディングのリスト。

	  type Program = [BindGroup]

#### tiProgram

Even the definitions of member functions in class and instance declarations can be included in this representation; they can be translated into top-level, explicitly typed bindings.

クラスとインスタンスの宣言内のメンバー関数の定義もこの表現; で入れることができます。彼らは最上位レベル、明示的に型指定されたバインドに翻訳することができます。

The type inference process for a program takes a list of assumptions giving the types of any primitives, and returns a set of assumptions for any variables.

プログラムの型推論プロセスは、すべてのプリミティブの種類を与える前提の一覧とすべての変数のための前提条件のセットを返します。

	  tiProgram :: ClassEnv -> [Assump] -> Program -> [Assump]
	  tiProgram ce as bgs = runTI $
	                        do (ps, as') <- tiSeq tiBindGroup ce as bgs
	                           s         <- getSubst
	                           rs        <- reduce ce (apply s ps)
	                           s'        <- defaultSubst ce [] rs
	                           return (apply (s'@@s) as')

This completes our presentation of the Haskell type system.

これで Haskell の型システムのプレゼンテーションを完了します。
