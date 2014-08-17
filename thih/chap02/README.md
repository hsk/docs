# 2 前提知識
[](2 Preliminaries)

For simplicity, we present the code for our typechecker as a single Haskell module.

単純に、我々は我々の型検査をするための１つのHaskellモジュールのコードを提示します。

The program uses only a handful of standard prelude functions, like map, concat, all, any, mapM, etc., and a few operations from the List and Monad libraries:

プログラムはプレリュードのmap,concat,all,mapM等の関数と、ListとMonadライブラリの操作のみを使用していてます。

	module TypingHaskellInHaskell where
	import List(nub, (\\), intersect, union, partition)
	import Monad(msum)

For the most part, our choice of variable names follows the notational conventions set out in Figure 1.

他のパートの為に、我々は変換の型と変数名の記法変換表を図1に示します。

| 説明				| 識別子		| 型 		|
|-------------------|-----------|-----------|
| Description		| Symbol	| Type 		|
| kind				| k, ...	| Kind		|
| type constructor	| tc, ...	| Tycon		|
| type variable		| v, ...	| Tyvar		|
| - 'fixed'			| f, ...	| 			|
| - 'generic'		| g, ...	| 			|
| type				| t, ...	| Type		|
| class				| c, ...	| Class		|
| instance			| it, ...	| Inst		|
| predicate			| p, q, ...	| Pred		|
| - 'deferred'		| d, ...	| 			|
| - 'retained'		| r, ...	| 			|
| qualified type	| qt, ...	| QualType	|
| class environment	| ce, ...	| ClassEnv	|
| scheme			| sc, ...	| Scheme	|
| substitution		| s, ...	| Subst		|
| unifier			| u, ...	| Subst		|
| assumption		| a, ...	| Assump	|
| identifier		| i, ...	| Id		|
| literal			| l, ...	| Literal	|
| pattern			| pat, ...	| Pat		|
| expression		| e, f, ...	| Expr		|
| alternative		| alt, ...	| Alt		|
| binding group		| bg, ...	| BindGroup	|

<center>
Figure 1: Notational Conventions
</center>
<center>
図1: 記法変換表
</center>

A trailing s on a variable name usually indicates a list.

変数名の後ろにsが付いた物はlistになります。

Numeric suffices or primes are used as further decoration where necessary.

数字サフィックスかプライムは必要な場合の装飾のために使われます。

For example, we use k or k' for a kind, and ks or ks' for a list of kinds.

例えば、kかk'をカインドに使い、ksまたはks'をカインドのリストに使います。

The types and terms appearing in the table are described more fully in later sections.

表にある型(type)や項(term)は後のセクションで詳しく記述します。



To distinguish the code for the typechecker from program fragments that are used to discuss its behavior, we typeset the former in an italic font, and the latter in a typewriter font.

プログラムの断片から型検査のためのコードを 識別 することはこれらのビヘイビアを議論する為に使われ、我々は型集合をイタリックフォントでカキ、タイプライターフォントで書きます。
これらのキーワードは見た目がイタリックでタイプラーターフォンとかなんかで書きます。

Throughout this paper, we implement identifiers as strings, and assume that there is a simple way to generate identifiers from integers using the enumId function:

この論文では、スルーアウトします。我々は識別しを文字列で、assumeはシンプルにenumId関数を使って整数から生成した識別子を使って実装します:

		type Id  = String

		enumId  :: Int -> Id
		enumId n = "v" ++ show n

The enumId function will be used in the definition of the newTVar operator in Section 10 to describe the allocation of fresh type variables during type inference.

enumId関数はセクション10で定義されているnewTVarオペレータで説明しますが型推論をする際のフレッシュな型変数を割り当てに使います。

With the simple implementation shown here, we assume that variable names beginning with ''v'' do not appear in input programs.

ここで見るシンプルな実装では、我々は入力プログラムにvから始まる変数は出現しません。
