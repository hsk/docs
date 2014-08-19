# 9 Assumptions 前提条件

Assumptions about the type of a variable are represented by values of the Assump datatype, each of which pairs a variable name with a type scheme:

変数の型についての仮定はペアの各変数の名前に型方式 Assump データ型の値によって表されます。

	  data Assump = Id :>: Scheme

Once again, we can extend the Types class to allow the application of a substitution to an assumption:

もう一度、我々 は仮定する代替のアプリケーションを許可する型クラスを拡張できます。

	  instance Types Assump where
	    apply s (i :>: sc) = i :>: (apply s sc)
	    tv (i :>: sc)      = tv sc

Thanks to the instance definition for Types on lists (Section 5), we can also use the apply and tv operators on the lists of assumptions that are used to record the type of each program variable during type inference.

インスタンス上の定義、型のリスト (セクション 5) のおかげで我々 演算子も使用できます、適用、テレビ型の推定中の各プログラム変数の型を記録するために使用の前提条件のリストに。

We will also use the following function to find the type of a particular variable in a given set of assumptions:

また次の関数を使用して仮定の与えられたセット内の特定の変数の型を見つけるには：

	  find                 :: Monad m => Id -> [Assump] -> m Scheme
	  find i []             = fail ("unbound identifier: " ++ i)
	  find i ((i':>:sc):as) = if i==i' then return sc else find i as

This definition allows for the possibility that the variable i might not appear in as.

この定義させることができます変数私は可能性がありますに表示されないこととして。

In practice, occurrences of unbound variables will probably have been detected in earlier compiler passes.

実習では、非連結変数の出現はおそらく検出されている以前のコンパイラ パスで。



sakurai:thih sakurai$ 
