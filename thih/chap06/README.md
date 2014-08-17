# 6 Unification and Matching (ユニフィケーション)単一化とマッチング

The goal of unification is to find a substitution that makes two types equal-for example, to ensure that the domain type of a function matches up with the type of an argument value.

ゴールオブユニフィケーションは代入を見つける事である、だって、たとえば２つの型が同じって作れるから、補償する事、だって関数の型はマッチ出来る引数変数の型と。

However, it is also important for unification to find as `small' a substitution as possible because that will lead to most general types.

なぜなら、ユニフィケーションの重要な所は、小さな代入を発見することで可能になる、なぜならば一般的な型かもしれないから。

More formally, a substitution s is a unifier of two types t1 and t2 if apply s t1 = apply s t2.
さらに形式的には、代入sは２つの型t1とt2のユにファイアである、もしもapply s t1 = apply s t2と計算出来るなら。


 A most general unifier, or mgu, of two such types is a unifier u with the property that any other unifier s can be written as s'@@u, for some substitution s'.

さらなる一般的なユニファイアもしくは、mguオブ２つの型はユニファイヤu with プロパティである、だって いくつもの他のユニファイヤ sは s' @@ uと書ける、いくつかの代入s'から。

The syntax of Haskell types has been chosen to ensure that, if two types have any unifying substitutions, then they have a most general unifier, which can be calculated by a simple variant of Robinson's algorithm [ Robinson, 1965].

Haskell型の構文は補償する事を選べる、もしも２つの型がいくつもの代入の単一化ならかれらはmguを持つことを、計算する事が出来るRobinson'sアルゴリズムのシンプルなバリアントによって。

One of the reasons for this is that there are no non-trivial equalities on types.

問題の１つは トライバルできない型がないこと。

 Extending the type system with higher-order features (such as lambda expressions on types), or with other mechanisms that allow reductions or rewriting in the type language, could make unification undecidable, non-unitary (meaning that there may not be most general unifiers), or both.

型システムを展開する事 with 高階機能(型上のλ式のような)、もしくは他のメカニズムは型言語によりリダクションか書き換えを許可し、宣言のないユニフィケーションを作る事が出来て、ユにたりでない(MGUではないって意味)、などなど。

This, for example, is why Haskell does not allow type synonyms to be partially applied (and interpreted as some restricted kind of lambda expression).

これ、例えば、なぜHaskellはパーシャリー評価な型シノニムを許可してないかということ。(そしてインタプリテッドいくつかのリストりくテッドなrラムダ式のカインド)

The calculation of most general unifiers is implemented by a pair of functions:

mguの計算は関数のペアで実装される:


	  mgu     :: Monad m => Type -> Type -> m Subst
	  varBind :: Monad m => Tyvar -> Type -> m Subst

These functions return results in a monad, capturing the fact that unification is a partial function.

関数はmonadの中に結果をリターンし、キャプチャしたファクターであるところのユニフィケーションはパーシャル関数です。

The main algorithm is described by mgu, using the structure of its arguments to guide the calculation:

メインのアルゴリズムはmguによって説明され、この引数の構造を使って、計算のガイド:

	  mgu (TAp l r) (TAp l' r') = do s1 <- mgu l l'
	                                 s2 <- mgu (apply s1 r) (apply s1 r')
	                                 return (s2 @@ s1)
	  mgu (TVar u) t        = varBind u t
	  mgu t (TVar u)        = varBind u t
	  mgu (TCon tc1) (TCon tc2)
	             | tc1==tc2 = return nullSubst
	  mgu t1 t2             = fail "types do not unify"

The varBind function is used for the special case of unifying a variable u with a type t.

varBind関数は特別な変数uの型tで単一化のケースを使っています。

At first glance, one might think that we could just use the substitution (u+->t) for this.

最初の見た目、1つ考えられる我々はこれを代入(u+->t)でこれをできるように思える。

 In practice, however, tests are required to ensure that this is valid, including an `occurs check' (u `elem` tv t) and a test to ensure that the substitution is kind-preserving:

以下の練習では、なぜなら、補償する事が必要だった、これが正しい事を、出現チェック(occurs check) (u `elem` tv t) を含み、補償する事をテストする事で、代入はカインドを持っている。

	  varBind u t | t == TVar u      = return nullSubst
	              | u `elem` tv t    = fail "occurs check fails"
	              | kind u /= kind t = fail "kinds do not match"
	              | otherwise        = return (u +-> t)

In the following sections, we will also make use of an operation called matching that is closely related to unification. 

移行のセクションで、我々は作った関数を使うマッチングの操作を呼び出し、ユニフィケーションを完成させる。

Given two types t1 and t2, the goal of matching is to find a substitution s such that apply s t1 = t2.

2つの型t1とt2が与えられたとき、マッチングのゴールは代入sを見つける事だった。apply s t1 = t2 の。

Because the substitution is applied only to one type, this operation is often described as one-way matching. 

なぜなら、代入は1つのタイプを評価するだけで、この操作はいつも1方向のマッチングで説明出来る。

The calculation of matching substitutions is implemented by a function:

代入マッチングの計算は関数で実装出来る。

	  match :: Monad m => Type -> Type -> m Subst

Matching follows the same pattern as unification, except that it uses merge rather than @@ to combine substitutions, and it does not allow binding of variables in t2:

マッチングはいくつかのパターンのユニフィケーションで、予想される。これはmargeや@@を代入をまとめる操作に使い、そしてこれはt2の中に変数をバインディングを許可しない。

	  match (TAp l r) (TAp l' r') = do sl <- match l l'
	                                   sr <- match r r'
	                                   merge sl sr
	  match (TVar u)   t | kind u == kind t = return (u +-> t)
	  match (TCon tc1) (TCon tc2)
	           | tc1==tc2         = return nullSubst
	  match t1 t2                 = fail "types do not match"

