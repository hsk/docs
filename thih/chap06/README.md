# 6 Unification and Matching (ユニフィケーション)単一化とマッチング

The goal of unification is to find a substitution that makes two types equal-for example, to ensure that the domain type of a function matches up with the type of an argument value.

ユニフィケーションのゴールは、関数の型は引数変数の型とマッチ出来る事を補償する事で、２つの型が同じという例から作り出した、代入を見つける事です。

However, it is also important for unification to find as `small' a substitution as possible because that will lead to most general types.

しかし、一般的な型かもしれないからユニフィケーションの重要な所は、小さな代入を発見することで可能になります。

More formally, a substitution s is a unifier of two types t1 and t2 if apply s t1 = apply s t2.

さらに形式的には、代入 s は２つの型 t1 と t2 の単一化で、このとき apply s t1 = apply s t2 です。

A most general unifier, or mgu, of two such types is a unifier u with the property that any other unifier s can be written as s'@@u, for some substitution s'.

このような2つのタイプの中で最も一般的な単一化、またはMGUは、ユニファイヤ u でありuで使われる他の代入sは、いくつかの代入s'のために、s'@@uのように書くことができる性質を持ちます。

The syntax of Haskell types has been chosen to ensure that, if two types have any unifying substitutions, then they have a most general unifier, which can be calculated by a simple variant of Robinson's algorithm [ Robinson, 1965].

Haskell型の構文は、もしも２つの型がいくつもの代入の単一化ならそれらはmguを持つことを、計算する事が出来る Robinson'sアルゴリズム [ robinson, 1965]のシンプルなバリアントによって補償する事を選択出来ます。

One of the reasons for this is that there are no non-trivial equalities on types.

問題の１つは トライバルできない型がなくはないことです。

 Extending the type system with higher-order features (such as lambda expressions on types), or with other mechanisms that allow reductions or rewriting in the type language, could make unification undecidable, non-unitary (meaning that there may not be most general unifiers), or both.

高階機能(型上のλ式のような)やもしくは他のメカニズムで型言語によりリダクションか書き換えを許可する事で型システムを展開する事は、宣言のないユニフィケーションを作る事が出来るか、ユにたりでない(MGUではないって意味)か、その両方です。

This, for example, is why Haskell does not allow type synonyms to be partially applied (and interpreted as some restricted kind of lambda expression).

これは例えば、Haskellは型シノニムが部分的に適用される（そしてラムダ式のいくつかの制限されたようなものと解釈される）ことはできない理由です。

#### mgu関数

The calculation of most general unifiers is implemented by a pair of functions:

mguの計算は関数のペアで実装されます:

	  mgu     :: Monad m => Type -> Type -> m Subst
	  varBind :: Monad m => Tyvar -> Type -> m Subst

These functions return results in a monad, capturing the fact that unification is a partial function.

これらの関数は、単一化が部分関数あることを捕獲、モナドで結果を返します。

The main algorithm is described by mgu, using the structure of its arguments to guide the calculation:

メインアルゴリズムはMGUによってその引数の構造を使用して計算を導くために記述されます:

	  mgu (TAp l r) (TAp l' r') = do s1 <- mgu l l'
	                                 s2 <- mgu (apply s1 r) (apply s1 r')
	                                 return (s2 @@ s1)
	  mgu (TVar u) t        = varBind u t
	  mgu t (TVar u)        = varBind u t
	  mgu (TCon tc1) (TCon tc2)
	             | tc1==tc2 = return nullSubst
	  mgu t1 t2             = fail "types do not unify"

#### varBind関数

The varBind function is used for the special case of unifying a variable u with a type t.

varBind関数は型 t で変数 u の単一化の特別なケースで使っています。

At first glance, one might think that we could just use the substitution (u+->t) for this.

最初の見た目では、我々はこれを代入(u+->t)が出来る可能性があるように思えます。

 In practice, however, tests are required to ensure that this is valid, including an `occurs check' (u `elem` tv t) and a test to ensure that the substitution is kind-preserving:

実際上、しかしながら、テストはこれが有効であり代入が次のカインドを保存していることを保証するテストである1つの出現チェック(occurs check) (u `elem` tv t)が含んでいることを保証するように要求されます:

	  varBind u t | t == TVar u      = return nullSubst
	              | u `elem` tv t    = fail "occurs check fails"
	              | kind u /= kind t = fail "kinds do not match"
	              | otherwise        = return (u +-> t)

#### match関数

In the following sections, we will also make use of an operation called matching that is closely related to unification.

このセクションの最後に、我々は作成した関数を使うマッチングの操作を呼び出し、ユニフィケーションを完成させます。

Given two types t1 and t2, the goal of matching is to find a substitution s such that apply s t1 = t2.

2つの型 t1 と t2 が与えられたとき、マッチングのゴールは apply s t1 = t2 の代入 s を見つける事でした。

Because the substitution is applied only to one type, this operation is often described as one-way matching.

なぜなら、代入は1つの型を評価するだけで、この操作はいつも1方向のマッチングで説明出来るからです。

The calculation of matching substitutions is implemented by a function:

代入マッチングの計算は次の関数で実装されます:

	  match :: Monad m => Type -> Type -> m Subst

Matching follows the same pattern as unification, except that it uses merge rather than @@ to combine substitutions, and it does not allow binding of variables in t2:

@@よりもmargeを代入をまとめる操作に使い、t2の中に変数をバインドすることを許可しないことをのぞけば、マッチングは同じパターンのユニフィケーションです:

	  match (TAp l r) (TAp l' r') = do sl <- match l l'
	                                   sr <- match r r'
	                                   merge sl sr
	  match (TVar u)   t | kind u == kind t = return (u +-> t)
	  match (TCon tc1) (TCon tc2)
	           | tc1==tc2         = return nullSubst
	  match t1 t2                 = fail "types do not match"
