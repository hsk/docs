# 4  Types

The next step is to define a representation for types.

次のステップでは型を作って説明します。

#### data Type

Stripping away syntactic sugar, Haskell type expressions are either type variables or constants (each of which has an associated kind), or applications of one type to another: applying a type of kind k1 -> k2 to a type of kind k1 produces a type of kind k2:

シンタックスシュガーをとりはずした、Haskellの型の式は型変数か、(それぞれが関連する種類を有する)定数か、１つの型から違う型への評価applyです: カインドk1->k2の評価は カインドk1からk2カインドを生成します:

	  data Type  =
	    | TVar Tyvar
	    | TCon Tycon
	    | TAp  Type Type
	    | TGen Int
	    deriving Eq

	  data Tyvar = Tyvar Id Kind
	               deriving Eq
	  data Tycon = Tycon Id Kind
	               deriving Eq

This definition also includes types of the form TGen n, which represent `generic' or quantified type variables.

この定義はTGen nを含んでいて、これはジェネリックもしくは量化された型変数を表します。

The only place where TGen values are used is in the representation of type schemes, which will be described in Section 8.

TGen変数は型スキームの中でのみ使われ、セクション8で説明します。

#### プリミティブデータ

The following examples show how standard primitive datatypes are represented as type constants:

例えば、標準的なプリミティブデータの型は以下のような型定数を含んで表されます:

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

フルセットのHaskellコンパイラやインタプリタは、定数など型チェックの際に必要とされない代数的データ型のような詳細をコンストラクター関数のリストとして、各タイプの追加情報を保存する可能性があります。

#### TAp

More complex types are built up from constants and variables using the TAp constructor.

さらに複雑な型は定数と変数を使ったTApコンストラクタで構築します。

For example, the representation for the type Int -> [a] is as follows:

例えば、型 Int -> [a] を以下に示します:

	TAp (TAp tArrow tInt) (TAp tList (TVar (Tyvar "a" Star)))

We do not provide a representation for type synonyms, assuming instead that they have been fully expanded before typechecking.

我々は、型シノニムを持っていないため、型チェックをする前に完全にこれらは展開してしまいます。

#### tString

For example, the String type-a synonym for [Char]-is represented as:

例えば、文字列型のシノニムは[Char]として表します:

	tString    :: Type
	tString     = list tChar

It is always possible for an implementation to expand synonyms in this way because Haskell prevents the use of a synonym without its full complement of arguments.

これは常に可能で、展開されたシノニムの実装、このような、なぜならHaskellはシノニムをpreventsだからだ。引数に完全コンプリート無しに。

Moreover, the process is guaranteed to terminate because recursive synonym definitions are prohibited.

さらに、再帰シノニム定義は禁止されているため処理が停止することが保証されています。

In practice, however, implementations are likely to expand synonyms more lazily: in some cases, type error diagnostics may be easier to understand if they display synonyms rather than expansions.

しかし、実際には、実装はより遅延してシノニムを拡張する可能性があります: いくつかのケースでは、型エラーの診断は、彼らがシノニムではなく、拡張を表す考えると理解しやすいかもしれません。

We end this section with the definition of a few helper functions.

我々は残りのヘルパー関数の定義でこのセクションを終わらせます。

#### infixr fn list pair

The first three provide simple ways to construct function, list, and pair types, respectively:

最初の３つは以下のように関数、リスト、ペア型のシンプルなコンストラクタです：

	  infixr      4 `fn`
	  fn         :: Type -> Type -> Type
	  a `fn` b    = TAp (TAp tArrow a) b

	  list       :: Type -> Type
	  list t      = TAp tList t
	  pair       :: Type -> Type -> Type
	  pair a b    = TAp (TAp tTuple2 a) b

#### HasKind

We also define an overloaded function, kind, that can be used to determine the kind of a type variable, type constant, or type expression:

我々はkind関数をオーバーロードしていて、コンスタントな型や型式も型変数のカインドを使って決めることが出来ます。

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

ここのケースの殆どはシンプルです。

Notice, however, that we can calculate the kind of an application (TAp t t') using only the kind of its first argument t: Assuming that the type is well-formed, t must have a kind k'->k, where k' is the kind of t' and k is the kind of the whole application.

しかし注意が必要で、我々は(TAp t t')を使っている評価のカインドを最初の引数tのカインドのみ使って計算出来ます: 型は整形されると仮定し、tはカインドk'->kを持つ必要があり、k'はt'のカインドでkは評価全体のカインドです。

This shows that we need only traverse the leftmost spine of a type expression to calculate its kind.

これは我々が型の式の骨の部分をトラバースさえすればこのカインドを計算することが出来る事を示しています。
