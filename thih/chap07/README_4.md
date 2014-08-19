## 7.4 Context Reduction コンテキスト削減

Class environments also play an important role in an aspect of the Haskell type system that is known as context reduction.

クラスの環境もコンテキスト削減として知られている Haskell の型システムの面で重要な役割を再生します。

The basic goal of context reduction is to reduce a list of predicates to an equivalent but, in some sense, simpler list.

コンテキストの削減の基本的な目標を相当するが、いくつかの意味は、単純なリストでの述語の一覧を減らすです。

The Haskell report [ Peyton Jones & Hughes, 1999]provides only informal hints about this aspect of the Haskell typing, where both pragmatics and theory have important parts to play.

Haskell report [ペイトン ・ ジョーンズ & ヒューズ、1999年] は、Haskell を入力、語用論および理論ある再生する重要な部分のこの側面についてだけカジュアルなヒントを提供します。

We believe therefore that this is one of the areas where a more formal specification will be particularly valuable.

そのためどこより正式な仕様は、特に貴重ななりますエリアの 1 つであると考えます。
One way to simplify a list of predicates is to simplify the type components of individual predicates in the list.

述語の一覧を簡略化する 1 つの方法は、リスト内の個別の述語の型のコンポーネントを簡単に。

For example, given the instance declarations in the Haskell standard prelude, we could replace any occurrences of predicates like Eq [a], Eq (a,a), or Eq ([a],Int) with Eq a.

たとえば、Haskell 標準的なプレリュードのインスタンスの宣言を考えると、私たちが置換任意 Eq [a] Eq のような述語の （、）、または Eq ([a]、Int) Eq で。

This is valid because, for any choice of a, each one of these predicates holds if, and only if, Eq a holds.

これは有効ですので、任意の選択のため、これらの各 1 つの述語を保持するおよび場合のみ、Eq を保持します。

Notice that, in some cases, an attempt to simplify type components-for example, by replacing Eq (a, b) with (Eq a, Eq b)-may increase the number of predicates in the list.

型コンポーネントを簡単にしようといくつかのケースでいることがわかります-たとえば、Eq (a, b) を置き換えることによって (Eq Eq b)-リスト内の述語の数を増やすことがあります。

The extent to which simplifications like this are used in a system of qualified types has an impact on the implementation and performance of overloading in practical systems [ Jones, 1992,Chapter 7].

限定型のシステムでこのような簡素化を使用するエクステントは、実装や実用的なシステム [ジョーンズ、1992 年、章 7] オーバー ロードのパフォーマンスに影響を与える。

In Haskell, however, the decisions are made for us by a syntactic restriction that forces us to simplify predicates until we obtain types in a kind of `head-normal form'.

Haskell は、しかし、決定は私たちのため強制的に私たちを私たち、一種の 'ヘッド ノーマル フォーム' 内の型を取得するまでに述語を簡素化する構文の制限によって。

This terminology is motivated by similarities with the concept of head-normal forms in l-calculus.

この用語は l 微積分の頭標準形の概念との類似性によって動機づけられています。

More precisely, the syntax of Haskell requires class arguments to be of the form v t1 ... tn, where v is a type variable, and t1,...,tn are types (and n ³ 0).

Haskell の構文より正確にフォーム v t1. テネシー州、ところ v 型の変数は、t1,..., tn は型であるクラス引数が必要です (と ñ ³ 0)。

The following function allows us to determine whether a given predicate meets these restrictions:

次の関数は、指定された述語がこれらの制限を満たしているかどうかを判断することができます。

	  inHnf       :: Pred -> Bool
	  inHnf (IsIn c t) = hnf t
	   where hnf (TVar v)  = True
	         hnf (TCon tc) = False
	         hnf (TAp t _) = hnf t

Predicates that do not fit this pattern must be broken down using byInst.

このパターンに適合しない述語 byInst を使用して分解する必要があります。

In some cases, this will result in predicates being eliminated altogether.

In 述語を全体で除去されてこれがなります。

In others, where byInst fails, it will indicate that a predicate is unsatisfiable, and will trigger an error diagnostic.

他では、byInst が失敗した場所は、述語は不十分、および診断エラーをトリガーするを示します。

This process is captured in the following definition:

このプロセスは、次の定義にキャプチャされます。

	  toHnfs      :: Monad m => ClassEnv -> [Pred] -> m [Pred]
	  toHnfs ce ps = do pss <- mapM (toHnf ce) ps
	                    return (concat pss)
 
	  toHnf                 :: Monad m => ClassEnv -> Pred -> m [Pred]
	  toHnf ce p | inHnf p   = return [p]
	             | otherwise = case byInst ce p of
	                             Nothing -> fail "context reduction"
	                             Just ps -> toHnfs ce ps

Another way to simplify a list of predicates is to reduce the number of elements that it contains.

述語の一覧を簡略化する別の方法が含まれている要素の数を減らすことです。

There are several ways that this might be achieved: by removing duplicates (e.g., reducing (Eq a, Eq a) to Eq a); by eliminating predicates that are already known to hold (e.g., removing any occurrences of Num Int); or by using superclass information (e.g., reducing (Eq a, Ord a) to Ord a).

これを達成する可能性がありますいくつかの方法があります: 重複を削除することによって (などを減らす (Eq、Eq、) Eq を);保持するために既に知られている述語を排除することによって (例えば、Num Int の任意のオカレンスの除去);またはスーパークラス情報を使用して (例えば、削減 (Eq、Ord、) Ord を)。

In each case, the reduced list of predicates, is equivalent to the initial list, meaning that all the predicates in the first will be satisfied if, and only if, all of the predicates in the second are satisfied.

それぞれのケースで述語の減らされたリストおよびすべての 2 番目の述語が満たされている場合にのみ、最初にすべての述語が満たされることを意味最初のリストと同じです。

The simplification algorithm that we will use here is based on the observation that a predicate p in a list of predicates (p:ps) can be eliminated if p is entailed by ps.

我々 はここで使用する簡略化アルゴリズムは述語 p の述語 (p:ps) の一覧ですることができます観測に基づく p ps によって伴なわれる場合を排除します。

As a special case, this will eliminate duplicated predicates: if p is repeated in ps, then it will also be entailed by ps.

特別なケースとして、これは重複述語を排除する： p が、ps で繰り返されるなら、ps によってに伴意も。

These ideas are used in the following definition of the simplify function, which loops through each predicate in the list and uses an accumulating parameter to build up the final result.

これらのアイデアは、次の一覧で各述語をループ処理し、最終結果を構築する蓄積のパラメーターを使用して簡素化関数の定義で使用されます。

Each time we encounter a predicate that is entailed by the others, we remove it from the list.

我々 は、他の人によって伴なわれる述語が発生するたびに私たちリストから削除します。


	  simplify   :: ClassEnv -> [Pred] -> [Pred]
	  simplify ce = loop []
	   where loop rs []                            = rs
	         loop rs (p:ps) | entail ce (rs++ps) p = loop rs ps
	                        | otherwise            = loop (p:rs) ps

Now we can describe the particular form of context reduction used in Haskell as a combination of toHnfs and simplify.

今 toHnfs の組合せとして Haskell で使用されるコンテキスト削減の特定のフォームを記述して簡素化することができます。

Specifically, we use toHnfs to reduce the list of predicates to head-normal form, and then simplify the result:

具体的には、ヘッド ノーマル フォームへの述語の一覧を減らすために toHnfs を使用し、結果を簡素化します。

	  reduce      :: Monad m => ClassEnv -> [Pred] -> m [Pred]
	  reduce ce ps = do qs <- toHnfs ce ps
	                    return (simplify ce qs)

As a technical aside, we note that there is some redundancy in the definition of reduce.

として、技術はさておき、我々 削減の定義にいくつかの冗長性があること注意してください。

The simplify function is defined in terms of entail, which makes use of the information provided by both superclass and instance declarations.

簡素化関数が伴うが、スーパークラスとインスタンス宣言によって提供される情報を利用する定義します。

The predicates in qs, however, are guaranteed to be in head-normal form, and hence will not match instance declarations that satisfy the syntactic restrictions of Haskell.

Qs は、内の述語は、ただし、ヘッド ノーマル フォームであることは保証し、それ故に Haskell の構文上の制限を満たすインスタンス宣言を一致しません。

It follows that we could make do with a version of simplify that used only the following function in determining (superclass) entailments:

それ私たち作ることができるのバージョンでは次のとおりです使用を簡素化するだけ (スーパークラス) 含意 (entailment) を決定する次の関数。

	  scEntail        :: ClassEnv -> [Pred] -> Pred -> Bool
	  scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)
