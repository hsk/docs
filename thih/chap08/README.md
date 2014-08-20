# 8 Type Schemes 型スキーム


#### data Scheme

Type schemes are used to describe polymorphic types, and are represented using a list of kinds and a qualified type:

型スキーマのポリモーフィックな型を記述するために使用し、種類と修飾された型の一覧を使用して表されます。

	  data Scheme = Forall [Kind] (Qual Type)
	                deriving Eq

There is no direct equivalent of Forall in the syntax of Haskell.

Haskell の構文では Forall の直接相当するものはありません。

Instead, implicit quantifiers are inserted as necessary to bind free type variables.

代わりに、暗黙的な量指定子は自由型の変数をバインドする必要に応じて挿入されます。

-------

In a type scheme Forall ks qt, each type of the form TGen n that appears in the qualified type qt represents a generic, or universally quantified type variable whose kind is given by ks!!n.

型スキーム Forall ks qt, で TGen n 修飾型 qt に表示されるフォームの種類ごとまたは一般的なを表しますその種類は ks によって与えられた型の変数を普遍的に定量化!n。

This is the only place where we will allow TGen values to appear in a type.

これは、TGen 値の種類で表示されるようになります唯一の場所です。

We had originally hoped that this restriction could be captured statically by a careful choice of the representation for types and type schemes.

もともとよぶ型スキーマの表現の注意深い選択によってこの制限を静的にキャプチャできる期待していた。

Unfortunately, we have not yet found a satisfactory way to enforce this, and, after considering several alternatives, we have settled for the representation shown here because it allows for simple implementations of equality and substitution.

残念ながら、まだは、これを強制する満足のいく方法を見つけていないと、いくつかの選択肢を考慮した後我々 の平等と置換の単純な実装できるため、ここに示す表現の定住しています。

#### SchemeのTypesクラスのインスタンス apply, tv

For example, the implementation of apply on Type values ignores TGen values, so we can be sure that there will be no variable capture problems in the following definition:

たとえば、型の実装の適用という問題があるない変数のキャプチャ、次の定義にことを必ずすることができますので、値は、TGen 値は無視されます。

	  instance Types Scheme where
	    apply s (Forall ks qt) = Forall ks (apply s qt)
	    tv (Forall ks qt)      = tv qt

#### quantify 関数

Type schemes are constructed by quantifying a qualified type qt with respect to a list of type variables vs:

型スキーマ型変数対のリストに関して修飾型 qt を定量化することで構成されます。

	  quantify      :: [Tyvar] -> Qual Type -> Scheme
	  quantify vs qt = Forall ks (apply s qt)
	   where vs' = [ v | v <- tv qt, v `elem` vs ]
	         ks  = map kind vs'
	         s   = zip vs' (map TGen [0..])

Note that the order of the kinds in ks is determined by the order in which the variables v appear in tv qt, and not by the order in which they appear in vs.

Ks の種類の順序が vs に表示される順序とないテレビ qt での変数 v の順序によって決定されることに注意してください。

So, for example, the leftmost quantified variable in a type scheme will always be represented by TGen 0.

したがって、たとえば、型の方式で左端限定化された変数は常に表される TGen 0 によって。

By insisting that type schemes are constructed in this way, we obtain a unique canonical form for Scheme values.

型スキーマはこの方法で構成されていると主張し、スキームの値をユニークな正規形を得る。

This is important because it means that we can test whether two type schemes are the same-for example, to determine whether an inferred type agrees with a declared type-using Haskell's derived equality, and without having to implement more complex tests for a-equivalence.

2 種類の方式が同じかどうかをテストすることができることを意味するので、これは重要です-たとえばを決定するかどうか推論型と同意するもの、宣言された型を使用して Haskell の派生平等と等しいかどうかより複雑なテストを実装することがなく。

#### toScheme 関数

In practice, we sometimes need to convert a Type into a Scheme without adding any qualifying predicates or quantified variables.

実習では、我々 は時々 予選述語を追加することがなく型スキームに変換する必要があります。 または変数を定量化しました。

For this special case, we can use the following function instead of quantify:

この特殊なケースの定量化のではなく、次の関数を使用できます。

	  toScheme      :: Type -> Scheme
	  toScheme t     = Forall [] ([] :=> t)

To complete our description of type schemes, we need to be able to instantiate the quantified variables in Scheme values.

型スキーマの私たちの記述を完了するには、スキームの値で定量化された変数のインスタンスを作成できるようにする必要があります。

In fact, for the purposes of type inference, we only need the special case that instantiates a type scheme with fresh type variables.

実際には、型の推論のために、我々 だけが必要な新鮮な型の変数の型のスキーマのインスタンスを作成する特殊なケース。

We therefore defer further description of instantiation to Section 10 where the mechanisms for generating fresh type variables are introduced.

したがって付けれさらに新鮮な型の変数を生成するためのメカニズムが導入されたセクション 10 にインスタンスの説明。


