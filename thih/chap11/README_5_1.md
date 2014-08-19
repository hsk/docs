### 11.5.1 Ambiguity and Defaults あい昧性および既定値

In the terminology of Haskell [ Peyton Jones & Hughes, 1999,Section 4.3.4], a type scheme ps => t is ambiguous if ps contains generic variables that do not also appear in t.

Haskell [ペイトン ・ ジョーンズ & ヒューズ、1999 年、セクション 5.4.3]、ps => 型方式の用語 t は ps t にも表示されないジェネリック変数が含まれている場合は、あいまいです。

This condition is important because theoretical studies [ Blott, 1991, Jones, 1992]have shown that, in the general case, we can only guarantee a well-defined semantics for a term if its most general type is not ambiguous.

この条件は重要であるので理論的研究 [Blott、1991 年に、ジョーンズ、1992年]、一般的なケースで保証することだけセマンティクスが明確に定義された用語の最も一般的な型があいまいな場合を示しています。

As a result, expressions with ambiguous types are considered ill-typed in Haskell and will result in a static error.

この結果、あいまいな型を持つ式 Haskell で病気に型指定されたと見なされます、静的エラーが発生します。

The following definition shows a fairly typical example illustrating how ambiguity problems can occur:

次の定義はあいまいさの問題がどのように発生するかを示す典型的な例を示しています。

	   stringInc x = show (read x + 1)

The intention here is that a string representation of a number will be parsed (using the prelude function read), incremented, and converted back to a string (using the prelude function show).

こちらの意図 (読むプレリュード関数を使用して)、数値の文字列表現は解析されるは、インクリメントし、(プレリュード関数表示を使用して) 文字列に変換されます。

But there is a genuine ambiguity because there is nothing to specify which type of number is intended, and because different choices can lead to different semantics.

ある本物のあいまいさを数種類のものを指定するものがあるので別の選択肢は異なるセマンティクスにつながることができますので。

For example, stringInc "1.5" might produce a result of "2.5" if floating point numbers are used, or a parse error (or perhaps a result of "2") if integers are used.

たとえば、stringInc「1.5」「2.5」浮動小数点数を使用している場合の結果または解析エラー (または「2」の結果おそらく) を生成可能性があります場合整数が使用されます。

This semantic ambiguity is reflected by a syntactic ambiguity in the inferred type of stringInc:

この意味の曖昧性は stringInc の推論された型の構文的曖昧性によって反映されます。

	   stringInc :: (Read a, Num a) => String -> String

(There is no Show a constraint here because Show is a superclass of Num.) A programmer can fix this particular problem quite easily by picking a particular type for a, and by adding an appropriate type annotation:

(制約がないショー、ここでショー数のスーパークラスであるので)プログラマはこの問題を修正特定非常に簡単のための特定の種類を選択することにより、および、適切な型の注釈を追加することによって。
	   stringInc x = show (read x + 1 :: Int)
Practical experience suggests that ambiguities like this tend to occur quite infrequently in real Haskell code.

実践的な経験が示すようにこのようなあいまいさは実際に Haskell コードでかなり頻繁に発生する傾向があります。

Moreover, when ambiguities are detected, the error diagnostics that are generated can often be useful in guiding programmers to genuine problems in their code.

さらに、あいまいさが検出されたときに生成されるエラーの診断しばしば有用であるプログラマがコード内の真の問題に導く。

However, the designers of Haskell felt that, in some situations involving numeric types-and particularly involving overloaded numeric literals-the potential for ambiguity was significant enough to become quite a burden on programmers.

しかし、Haskell のデザイナー感じたこと、数値型を含むいくつかの状況で- と特にオーバー ロードされた数値リテラルの潜在的なあいまいさはプログラマにはかなりの負担になることを十分に重要です。

Haskell's default mechanism was therefore introduced as a pragmatic compromise that is convenient-because it automates the task of picking types for otherwise ambiguous variables-but also dangerous-because it involves making choices about the semantics of a program in ways that are not always directly visible to programmers.

Haskell の既定の機構は便利です、実用的な妥協として導入された従って-それ以外の場合あいまいな変数の型を選ぶのタスクが自動化されるため- しかしまた危険-常にプログラマに直接表示されない方法で、プログラムのセマンティクスについての選択肢を作るため。

For this latter reason, the use of defaulting is restricted so that it will only apply under certain, fairly restrictive circumstances.

この後者の理由、かなり厳しい状況下でのみ適用されますように不履行の使用が制限されます。
The remainder of this section explains in more detail how ambiguities in Haskell programs can be detected and, when appropriate, eliminated by a suitable choice of defaults.

このセクションの残りの部分をどの Haskell プログラムであいまいさことができます検出され、適切な場合、デフォルトの適当な選択によって除去より詳細に説明します。

The first step is to identify any sources of ambiguity.

最初のステップは、あいまいさの任意のソースを識別します。

Suppose, for example, that we are about to qualify a type with a list of predicates ps and that vs lists all known variables, both fixed and generic.

たとえば、述語 ps のリストの型を修飾することおよび対の一覧すべての知られている変数、両方の固定およびジェネリックとします。

An ambiguity occurs precisely if there is a type variable that appears in ps but not in vs (i.e., in tv ps \\ vs).

Ps ではない対に表示される型の変数がある場合は正確にあいまいさが発生します (すなわち、テレビ ps で \\ 対)。

The goal of defaulting is to bind each ambiguous type variable v to a monotype t.

不履行の目標モノタイプ t に各変数 v のあいまいな型をバインドすることです。

The type t must be chosen so that all of the predicates in ps that involve v will be satisfied once t has been substituted for v.

型 t はので ps を含む v の述部のすべてが満たされる t の v 置換されている一度に選ばれなければなりません。

The following function calculates the list of ambiguous variables and pairs each one with the list of predicates that must be satisfied by any choice of a default:

次の関数のあいまいな変数の一覧を計算し、デフォルトの任意の選択で満たす必要のある述語のリストと各 1 つのペアします。

	  type Ambiguity       = (Tyvar, [Pred])
 
	  ambiguities         :: ClassEnv -> [Tyvar] -> [Pred] -> [Ambiguity]
	  ambiguities ce vs ps = [ (v, filter (elem v . tv) ps) | v <- tv ps \\ vs ]

Given one of these pairs (v,qs), and as specified by the Haskell report [ Peyton Jones & Hughes, 1999,Section 4.3.4], defaulting is permitted if, and only if, all of the following conditions are satisfied:

Haskell report [ペイトン ・ ジョーンズ & ヒューズ、1999 年、セクション 5.4.3] で指定された v （qs）、これらのペアの 1 つを与えられた不履行は許可され、次の条件すべてが満たされている場合にのみ。

All of the predicates in qs are of the form IsIn c (TVar v) for some class c.

Qs の述部のすべては、いくつかのクラス c の形式 IsIn c (TVar v です)。
At least one of the classes involved in qs is a standard numeric class.

Qs に関連するクラスの少なくとも 1 つは、標準の数値クラスです。

The list of these class names is provided by a constant:

これらのクラスの名前のリストは定数によって提供されます。

	  numClasses :: [Id]
	  numClasses  = ["Num", "Integral", "Floating", "Fractional",
	                 "Real", "RealFloat", "RealFrac"]

All of the classes involved in qs are standard classes, defined either in the standard prelude or standard libraries.

標準プレリュードまたは標準ライブラリで定義されている標準のクラスはすべて qs に関連するクラスです。

Again, the list of these class names is provided by a constant:

再度、これらのクラスの名前のリストは定数によって提供されます。

	  stdClasses :: [Id]
	  stdClasses  = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix",
	                 "Functor", "Monad", "MonadPlus"] ++ numClasses

That there is at least one type in the list of default types for the enclosing module that is an instance of all of the classes mentioned in qs.

少なくとも 1 つのタイプにあるすべての qs に記載されているクラスのインスタンスである外側のモジュールの既定の種類の一覧。

The first such type will be selected as the default.

最初このような型は、既定として選択されます。

The list of default types can be obtained from a class environment by using the defaults function that was described in Section 7.2.

セクション 7.2 で記述されていたデフォルト関数を使用してクラス環境から既定の種類の一覧を取得することができます。
These conditions are captured rather more succinctly in the following definition, which we use to calculate the candidates for resolving a particular ambiguity:

これらの条件がより簡潔にではなく特定のあいまいさを解決するための候補者の計算を用いて、次の定義でキャプチャされます。

	  candidates           :: ClassEnv -> Ambiguity -> [Type]
	  candidates ce (v, qs) = [ t' | let is = [ i | IsIn i t <- qs ]
	                                     ts = [ t | IsIn i t <- qs ],
	                                 all ((TVar v)==) ts,
	                                 any (`elem` numClasses) is,
	                                 all (`elem` stdClasses) is,
	                                 t' <- defaults ce,
	                                 all (entail ce []) [ IsIn i t' | i <- is ] ]

If candidates returns an empty list for a given ambiguity, then defaulting cannot be applied to the corresponding variable, and the ambiguity cannot be avoided.

候補者を返す場合、空のリストを指定した場合、あいまいな、デフォルトし、対応する変数には適用できず、あいまいさを避けることはできません。

On the other hand, if the result is a non-empty list ts, then we will be able to substitute head ts for v and remove the predicates in qs from ps.

その一方で、結果が空でないリスト ts 場合、しいたします頭 ts v を置き換えるし、qs の ps から述語を削除すること。

The calculations for the defaulting substitution, and for the list of predicates that it eliminates follow very similar patterns, which we capture by defining them in terms of a single, higher-order function:

不履行の置換とそれを排除する述語のリストについて計算単一、高次関数の観点から定義してキャプチャ我々 非常によく似たパターンに従ってください。

	  withDefaults :: Monad m => ([Ambiguity] -> [Type] -> a)
	                    -> ClassEnv -> [Tyvar] -> [Pred] -> m a
	  withDefaults f ce vs ps
	      | any null tss  = fail "cannot resolve ambiguity"
	      | otherwise     = return (f vps (map head tss))
	        where vps = ambiguities ce vs ps
	              tss = map (candidates ce) vps

The withDefaults function takes care of picking suitable defaults, and of checking whether there are any ambiguities that cannot be eliminated.

WithDefaults 関数は、適切なデフォルト値を選ぶのと排除することはできません任意のあいまいさがあるかどうかをチェックするの世話します。

If defaulting succeeds, then the list of predicates that can be eliminated is obtained by concatenating the predicates in each Ambiguity pair:

不履行が成功した場合のあいまいさの各ペアの述語を連結することによって除去できる述語の一覧が得られます。

	  defaultedPreds :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m [Pred]
	  defaultedPreds  = withDefaults (\vps ts -> concat (map snd vps))

In a similar way, the defaulting substitution can be obtained by zipping the list of variables together with the list of defaults:

同様の方法で不履行置換ビュンと既定値の一覧と共に変数のリストを取得できます。

	  defaultSubst   :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m Subst
	  defaultSubst    = withDefaults (\vps ts -> zip (map fst vps) ts)

One might wonder why the defaulting substitution is useful to us here; if the ambiguous variables don't appear anywhere in the assumptions or in the inferred types, then applying this substitution to those components would have no effect.

1 つだろうとなぜ不履行置換私たちに役に立つはここです。あいまいな変数、仮定または推論される型に任意の場所に表示されない場合、この置換コンポーネントを適用するそれらを解消しても効果はありません。

In fact, we will only need defaultSubst at the top-level, when type inference for an entire module is complete [ Peyton Jones & Hughes, 1999,Section 4.5.5, Rule 2].

実際には、モジュール全体に対して型の推論は完全な [ペイトン ・ ジョーンズ & ヒューズ、1999, 第 4.5.5 節、ルール 2] ときのトップレベル、defaultSubst が必要のみ。

In this case, it is possible that Haskell's infamous `monomorphism restriction' (see Section 11.6.2) may prevent generalization over some type variables.

この場合、それは不可能は Haskell の有名な '相性制限' いくつかの型の変数間の汎化を回避可能性があります (セクション 11.6.2 参照)。

But Haskell does not allow the types of top-level functions to contain unbound type variables.

しかし、Haskell はバインドされていない型の変数を格納する最上位の関数の型を使用できません。

Instead, any remaining variables are considered ambiguous, even if they appear in inferred types; the substitution is needed to ensure that they are bound correctly.

代わりに、任意の残りの変数もと見なされますあいまいな、推論された型が; で表示置換が正常にバインドされていることを確保するために必要です。
