## 11.5 From Types to Type Schemes 型リストから型スキーム

We have seen how lists of predicates are accumulated during type inference; now we will describe how those predicates are used to construct inferred types.

型の推論中に述語のリストがどのように蓄積されるかを見ています。今我々 はこれらの述語を使用して、推論された型を構築する方法を説明します。

This process is sometimes referred to as generalization because the goal is always to calculate the most general types that are possible.

このプロセスは、目標は常に可能な最も一般的な種類を計算するので、時々 に汎化と呼ばれます。

In a standard Hindley-Milner system, we can usually calculate most general types by quantifying over all relevant type variables that do not appear in the assumptions.

システムでは標準ヒンドリー Milner、通常最も一般的な種類の前提条件に含まれていないすべての関連する型の変数を定量化することで計算できます。

In this section, we will describe how this process is modified to deal with the predicates in Haskell types.

このセクションでの種類の Haskell の述語に対処するためのこのプロセスの変更方法を説明します。

----

To understand the basic problem, suppose that we have run the type checker over the body of a function h to obtain a list of predicates ps and a type t.

基本的な問題を理解するには、h 述語 ps とタイプ t のリストを取得する関数の本体で型チェックを実行するいると仮定します。

At this point, to obtain the most general result, we could infer a type for h by forming the qualified type qt = (ps :=> t) and then quantifying over any variables in qt that do not appear in the assumptions.

この時点で、最も一般的な結果を取得する我々 が型を推論 h 修飾型 qt を形成することによって = （ps:=> t) および前提条件に含まれていない qt での任意の変数を量を示します。

While this is permitted by the theory of qualified types, it is often not the best thing to do in practice.

これは限定型の理論によって認められていますが、それは頻繁練習で行うには最高のものではないです。

For example:

たとえば:

- The list ps can often be simplified using the context reduction process described in Section 7.4.

	リスト ps 多くの場合、セクション 7.4 で説明されているコンテキストの削減プロセスを使用して簡潔にできます。

	This will also ensure that the syntactic restrictions of Haskell are met, requiring all predicates to be in head-normal form.

	これは Haskell の構文上の制限が満たされているヘッド ノーマル形式にすべての述語を必要とするも保証されます。

- Some of the predicates in ps may contain only `fixed' variables (i.e., variables appearing in the assumptions), so including those constraints in the inferred type will not be of any use [ Jones, 1992,Section 6.1.4].

	Ps の述部のいくつかは '固定' 変数 (すなわち、仮定で現われる変数) のみを含めることができます、任意の使用 [ジョーンズ、1992 年、セクション 6.1.4] されませんので、推論される型にこれらの制約を含みます。

	These predicates should be `deferred' to the enclosing bindings.

	これらの述語が '' まで延期外側のバインディング。

- Some of the predicates in ps could result in ambiguity, and require defaulting to avoid a type error.

	一部 ps の述部の可能性があります、あいまいさが発生し、型エラーを避けるために履行を怠るします。

	This aspect of Haskell's type system will be described shortly in Section 11.5.1.

	Haskell の型システムのこの側面はセクション 11.5.1 でまもなく説明予定します。

#### split 関数

In this paper we use a function called split to address these issues.

この論文では, これらの問題に対処する split と呼ばれる関数を使用します。

For the situation described previously where we have inferred a type t and a list of predicates ps for a function h, we can use split to rewrite and break ps into a pair (ds, rs) of deferred predicates ds and `retained' predicates rs.

ここで前に説明したような状況の型 t を推論したし、のリストの ps h を書き換えて、繰延述語 ds のペア ds （rs） の ps の分割分割を使用することができます、'保存' rs の述語関数の述語。

The predicates in rs will be used to form an inferred type (rs :=> t) for h, while the predicates in ds will be passed out as constraints to the enclosing scope.

Rs の述語推論型を形成する使用される (rs:=> t) h、ds で述語に渡されるの制約として、外側のスコープ中の。

We use the following definition for split:

我々 の分割次の定義を使用します。

	  split :: Monad m => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred]
	                        -> m ([Pred], [Pred])
	  split ce fs gs ps = do ps' <- reduce ce ps
	                         let (ds, rs) = partition (all (`elem` fs) . tv) ps'
	                         rs' <- defaultedPreds ce (fs++gs) rs
	                         return (ds, rs \\ rs')

In addition to a list of predicates ps, the split function is parameterized by two lists of type variables.

述語 ps のリストに加えて、split 関数は型の変数の 2 つのリストによってパラメーター化します。

The first, fs, specifies the set of `fixed' variables, which are just the variables appearing free in the assumptions.

最初は、fs、無料、仮定で現われる変数だけは '固定' 変数のセットを指定します。

The second, gs, specifies the set of variables over which we would like to quantify; for the example above, it would just be the variables in (tv t \\ fs).

第二に、gs をしたいと思います定量化; 変数のセットを指定します上記の例のためだけであろうで変数 (テレビ t \\ fs)。

It is possible for ps to contain variables that are not in either fs or gs (and hence not in the parameter (fs++gs) that is passed to defaultedPreds).

Ps fs か gs ではない変数を格納することは不可能です （と、それゆえ、パラメーターではなく (fs + + gs)、defaultedPreds に渡された)。

In Section 11.5.1 we will see that this is an indication of ambiguity.

セクション 11.5.1 で我々 はこれがあい昧性の徴候が表示されます。

------

There are three stages in the split function, corresponding directly to the three points listed previously.

前に示した 3 つのポイントに直接対応する split 関数で 3 つの段階があります。

The first stage uses reduce to perform context reduction.

コンテキストのリダクションを実行する最初の段階の使用を減らします。

The second stage uses the standard prelude function partition to identify the deferred predicates, ds; these are just the predicates in ps' that contain only fixed type variables.

第二段階標準プレリュード関数パーティションを使って識別繰延述語、ds;これらは ps のだけ固定型変数を含む述語だけです。

The third stage determines whether any of the predicates in rs should be eliminated using Haskell's defaulting mechanism, and produces a list of all such predicates in rs'.

第三段階かどうかを rs の述語のいずれかの Haskell の既定の機構を使用して削除する必要があります rs' にすべてこのような述語の一覧を生成します。

Hence the final set of retained predicates is produced by the expression rs \\ rs' in the last line of the definition.

剰余金の述語の最終的なセットと式 rs をプロデュースするそれ故に \\ rs' の定義の最後の行。

