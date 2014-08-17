# 3  Kinds

To ensure that they are valid, Haskell type constructors are classified into different kinds: the kind * (pronounced `star') represents the set of all simple (i.e., nullary) type expressions, like Int and Char -> Bool; kinds of the form k1 -> k2 represent type constructors that take an argument type of kind k1 to a result type of kind k2.

To ensure that Haskell型コンストラクタは違う下院殿中でクラスファイドされている彼らはバリッドです。
カインド * (pronounced 'star') はすべてのIntのようなシンプルな型式の集合を表し、
引数の型がカインドk1で、結果の型がカインドk2の
カインドk1 -> k2 (例えば Char -> Bool)で表される型コンストラクタを表します。

For example, the standard list, Maybe and IO constructors all have kind * -> *. 

例えば、標準的なリスト、MaybeとIOのコンストラクタはすべてカインド* -> *を持ちます。

Here, we will represent kinds as values of the following datatype:

カインドのデータタイプを以下に示します。

	  data Kind  = Star | Kfun Kind Kind
	               deriving Eq

Kinds play essentially the same role for type constructors as types do for values, but the kind system is clearly very primitive.

カインドは値の為の型の型コンストラクタと同じ役割のエッセンシャルでプレイしますが、カインドシステムは明確にとてもプリミティブです。

There are a number of extensions that would make interesting topics for future research, including polymorphic kinds, subkinding, and record/product kinds.

ポリモーフィックなカインド、サブカインド、そしてレコード／プロダクトカインドを含む、興味深い帰納の為の研究のいくつもの拡張が存在します。

A simple extension of the kind system-adding a new row kind-has already proved to be useful for the Trex implementation of extensible records in Hugs [ Gaster & Jones, 1996, Jones & Peterson, 1999].

カインドシステムを新しいrow kind-has を追加したシンプルなエクステンションは既にレコード拡張のTrex実装がHugsで使われ含まれています [ Gaster & Jones, 1996, Jones & Peterson, 1999]。



