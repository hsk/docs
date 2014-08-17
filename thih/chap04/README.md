# 4  Types

The next step is to define a representation for types.

次のステップは型を作って説明する。

Stripping away syntactic sugar, Haskell type expressions are either type variables or constants (each of which has an associated kind), or applications of one type to another: applying a type of kind k1 -> k2 to a type of kind k1 produces a type of kind k2:

シンタックスシュガーをはぎ取った、Haskell型式はeither型変数かコンスタント(each of which has an 連想カインド)、か１つから違う型への評価apply(カインドk1->k2からカインドk1がk2カインドを生成する)です。

	  data Type  = TVar Tyvar | TCon Tycon | TAp  Type Type | TGen Int
	               deriving Eq
	 
	  data Tyvar = Tyvar Id Kind
	               deriving Eq
	  data Tycon = Tycon Id Kind
	               deriving Eq

This definition also includes types of the form TGen n, which represent `generic' or quantified type variables.

この定義はTGen nを含んでいて、これはジェネリックもしくは量化された型変数を表します。

The only place where TGen values are used is in the representation of type schemes, which will be described in Section 8.

TGenの値の位置は型スキームの中でのみ使われ、セクション8で説明します。

The following examples show how standard primitive datatypes are represented as type constants:

例を以下に示します。標準的なデータタイプをどう表すか、は以下のように示す型を含みます:

	  tUnit    = TCon (Tycon "()" Star)
	  tChar    = TCon (Tycon "Char" Star)
	  tInt     = TCon (Tycon "Int" Star)
	  tInteger = TCon (Tycon "Integer" Star)
	  tFloat   = TCon (Tycon "Float" Star)
	  tDouble  = TCon (Tycon "Double" Star)
	 
	  tList    = TCon (Tycon "[]" (Kfun Star Star))
	  tArrow   = TCon (Tycon "(->)" (Kfun Star (Kfun Star Star)))
	  tTuple2  = TCon (Tycon "(,)" (Kfun Star (Kfun Star Star)))

A full Haskell compiler or interpreter might store additional information with each type constant-such as the the list of constructor functions for an algebraic datatype-but such details are not needed during typechecking.

フルセットのHaskellコンパイラもしくはインタプリタは追加情報を持つ力を持ってて、algebraicなデータタイプの為のコンスタントなリストの構築関数を持ってますが、詳細は型チェックには必要ないです。


More complex types are built up from constants and variables using the TAp constructor.

さらに複雑な型はコンスタントとTApコンストラクタを使った値で作り上げます。

For example, the representation for the type Int -> [a] is as follows:

例えば、型 Int -> [a] を以下に示します:

	TAp (TAp tArrow tInt) (TAp tList (TVar (Tyvar "a" Star)))

We do not provide a representation for type synonyms, assuming instead that they have been fully expanded before typechecking.

我々は、型シノニムを持ってないけど、型チェックをする前に完全にこれらは展開してしまいます。

For example, the String type-a synonym for [Char]-is represented as:

例えば、文字列型のシノニムは[Char]として表してしまいます:

	tString    :: Type
	tString     = list tChar

It is always possible for an implementation to expand synonyms in this way because Haskell prevents the use of a synonym without its full complement of arguments.

これは常に可能です。展開されたシノニムの実装、このような、なぜならHaskellはシノニムをpreventsだからだ。引数に完全コンプリート無しに。

Moreover, the process is guaranteed to terminate because recursive synonym definitions are prohibited.

さらにうえに、プロセスは守るってんです。ターミネートすることを、なぜなら再帰シノニム定義はプロヒビッテッドだ。

In practice, however, implementations are likely to expand synonyms more lazily: in some cases, type error diagnostics may be easier to understand if they display synonyms rather than expansions.

プラクティス上、なぜなら、実装はにている展開シノニム　とても遅延評価に: いくつかのケースでは、型エラー diagnostics 簡単に分かるかもしれない もしも彼らが表示したらシノニムを後で展開するより

We end this section with the definition of a few helper functions.

我々は残りのヘルパー関数の定義でこのセクションを終わらせる。

The first three provide simple ways to construct function, list, and pair types, respectively:

最初の３つはシンプルなコンストラクタである。関数、リスト、ペア型のね。以下のように：

	  infixr      4 `fn`
	  fn         :: Type -> Type -> Type
	  a `fn` b    = TAp (TAp tArrow a) b
	 
	  list       :: Type -> Type
	  list t      = TAp tList t
	  pair       :: Type -> Type -> Type
	  pair a b    = TAp (TAp tTuple2 a) b

We also define an overloaded function, kind, that can be used to determine the kind of a type variable, type constant, or type expression:

我々はオーバーロードしてる、kind関数を。だって、型変数のカインドを使って決めることが出来る。コンスタントな型や型式も。

	  class HasKind t where
	    kind :: t -> Kind
	  instance HasKind Tyvar where
	    kind (Tyvar v k) = k
	  instance HasKind Tycon where
	    kind (Tycon v k) = k
	  instance HasKind Type where
	    kind (TCon tc) = kind tc
	    kind (TVar u)  = kind u
	    kind (TAp t _) = case (kind t) of
	                       (Kfun _ k) -> k


Most of the cases here are straightforward.

ここのケースの重要なのはストレイトフォワードだ。

Notice, however, that we can calculate the kind of an application (TAp t t') using only the kind of its first argument t: Assuming that the type is well-formed, t must have a kind k'->k, where k' is the kind of t' and k is the kind of the whole application.

ノーティスだ、なぜなら、我々は計算出来る(TAp t t')を使っている評価のカインドを最初の引数tのカインドのみ使って: Assuming 型はウェルフォームドで、tはカインドk'->kを持たないと行けなくて、k'はt'のカインドでkはwhole評価のカインドだ。

This shows that we need only traverse the leftmost spine of a type expression to calculate its kind.

これは我々は型式のspineからトラバースするだけでこのカインドを計算することが必要であるというふうにみれる。


