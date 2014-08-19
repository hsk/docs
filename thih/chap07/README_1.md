## 7.1  Basic Definitions 基本的な定義

A Haskell type class can be thought of as a set of types (of some particular kind), each of which supports a certain collection of member functions that are specified as part of the class declaration.

Haskellの型クラスは、クラス宣言の一部として指定されたメンバ関数の特定のコレクションをサポートし、それぞれが（いくつかの特定の種類の）型の集合、と考えることができます。

The types in each class (known as instances) are specified by a collection of instance declarations.

（インスタンスとして知られている）各クラスの型はインスタンス宣言の集合によって指定されます。

Haskell types can be qualified by adding a (possibly empty) list of predicates, or class constraints, to restrict the ways in which type variables are instantiated4:

Haskellの型は型変数がインスタンス化される方法を制限する述部は、クラスの制約の（空）のリストを追加することで修飾することができます:

	  data Qual t = [Pred] :=> t
	                deriving Eq

In a value of the form ps :=> t, we refer to ps as the context and to t as the head.

ps :=> tの形の値では、コンテキストとして、psを参照し、ヘッドとしてtに。

Predicates themselves consist of a class identifier and a type; a predicate of the form IsIn i t asserts that t is a member of the class named i:

述語自身はクラスIDとタイプで構成され; IsIn i t 形式の述部は、tは名前iのクラスのメンバーであることを主張する。

	  data Pred   = IsIn Id Type
	                deriving Eq

For example, using the Qual and Pred datatypes, the type (Num a) => a -> Int can be represented by:

例えば、QualとPredデータ型を使用して、型 `(Num a) => a -> Int` は次式で表すことができます:


	  [IsIn "Num" (TVar (Tyvar "a" Star))] :=> (TVar (Tyvar "a" Star) `fn` tInt)

It would be easy to extend the Pred datatype to allow other forms of predicate, as is done with Trex records in Hugs [ Jones & Peterson, 1999].

Hugs[ジョーンズ＆ピーターソン、1999]でトレックス·レコードと行われているように、述語の他の形態を可能にするためにPREDデータ型を拡張するのは簡単でしょう。


Another frequently requested extension is to allow classes to accept multiple parameters, which would require a list of Types rather than the single Type in the definition above.

もう一つの頻繁に要求される拡張は、クラスは型のリストではなく、上記の定義内の単一のタイプを必要とする複数のパラメータを受け入れるようにすることです。

The extension of Types to the Qual and Pred datatypes is straightforward:

QualとPredデータ型への型の拡張は簡単です:

	  instance Types t => Types (Qual t) where
	    apply s (ps :=> t) = apply s ps :=> apply s t
	    tv (ps :=> t)      = tv ps `union` tv t
 	
	  instance Types Pred where
	    apply s (IsIn i t) = IsIn i (apply s t)
	    tv (IsIn i t)      = tv t

The tasks of calculating most general unifiers and matching substitutions on types also extend naturally to predicates:

最も一般的なユニファイアを計算し、タイプに関する置換はまた、述部に自然に拡張するマッチングのタスク：

	  mguPred, matchPred :: Pred -> Pred -> Maybe Subst
	  mguPred             = lift mgu
	  matchPred           = lift match
	 	
	  lift m (IsIn i t) (IsIn i' t')
	           | i == i'   = m t t'
	           | otherwise = fail "classes differ"

We will represent each class by a pair of lists, one containing the name of each superclass, and another containing an entry for each instance declaration:

私たちは、リストのペアで各クラスを表しますが、各スーパークラスの名前を含む1、各インスタンス宣言のエントリを含む別：

	  type Class    = ([Id], [Inst])
	  type Inst     = Qual Pred

For example, a simplified version of the standard Haskell class Ord might be described by the following value of type Class:

たとえば、標準Haskellのクラス Ord の簡易版は、Class型の次の値によって記述されることがあります。

	  (["Eq"], [[] :=> IsIn "Ord" tUnit,
	            [] :=> IsIn "Ord" tChar,
	            [] :=> IsIn "Ord" tInt,
	            [IsIn "Ord" (TVar (Tyvar "a" Star)),
	             IsIn "Ord" (TVar (Tyvar "b" Star))]
	               :=> IsIn "Ord" (pair (TVar (Tyvar "a" Star))
	                                    (TVar (Tyvar "b" Star)))])

This structure captures the fact that Eq is a superclass of Ord (the only one in fact), and lists four instance declarations for the unit, character, integer, and pair types (if a and b are in Ord, then (a,b) is also in Ord).

この構造は、式がオード（実際には一つだけ）のスーパークラスであるという事実をキャプチャして、aとbが、その後、オードに（A、Bである場合（単位、文字、整数、ペアタイプの4インスタンス宣言を一覧表示します））オードにもあります。

Of course, this is only a fraction of the list of Ord instances that are defined in the full Haskell prelude.

もちろん、これは完全なHaskellのプレリュードで定義されているオード·インスタンスのリストの一部のみである。

Only the details that are needed for type inference are included in these representations.

専用型推論のために必要とされる詳細はこれらの表現に含まれています。

A full Haskell implementation would need to store additional information for each declaration, such as the list of member functions for each class and details of their implementations in each particular instance.

フルHaskellの実装は、それぞれの特定のインスタンス内の各クラスとその実装の詳細については、メンバ関数のリストとして、各宣言のための追加情報を格納する必要があります。