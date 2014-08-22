## 7.2 Class Environments クラス環境

#### ClassEnv

The information provided by the class and instance declarations in a given program can be captured by a class environment of type:

与えられたプログラムのクラスとインスタンスの宣言によって提供される情報は、型のクラスの環境と捉える事ができます。

	  data ClassEnv = ClassEnv { classes  :: Id -> Maybe Class,
	                             defaults :: [Type] }

The classes component in a ClassEnv value is a partial function that maps identifiers to Class values (or to Nothing if there is no class corresponding to the specified identifier).

ClassEnv変数内のclassesは、識別子からClassの変数をマップする部分関数(もしくは指定した識別子に対応するクラスがない場合は Nothing)です。

#### super insts 関数

We define helper functions super and insts to extract the list of superclass identifiers, and the list of instances, respectively, for a class name i in a class environment ce:

クラス環境ce内でクラス名iについて、それぞれスーパークラス識別子のリスト、およびインスタンスのリストを抽出するために、我々はヘルパー関数のsuperとinstsを定義します。

	  super     :: ClassEnv -> Id -> [Id]
	  super ce i = case classes ce i of Just (is, its) -> is
	 
	  insts     :: ClassEnv -> Id -> [Inst]
	  insts ce i = case classes ce i of Just (is, its) -> its

These functions are intended to be used only in cases where it is known that the class i is defined in the environment ce.

これらの関数は、環境ce内でクラスiが定義されていることが分かっている場合にのみ使用されることが意図されています。

In some cases, this condition might be guaranteed by static analysis prior to type checking.

いくつかのケースでこの条件は型チェックの前に静的解析によって保証されます。

Alternatively, we can resort to a dynamic check by testing defined (classes ce i) before applying either function.

代わりに、我々はどちらの関数を適用する前に定義された（classes ce i）をテストすることにより、動的なチェックに頼ることができます。

#### defined 関数

The function defined used here is defined as follows [5]:

次のようにここで用いる関数 defined が定義されています [5]:

	  defined :: Maybe a -> Bool
	  defined (Just x) = True
	  defined Nothing  = False


#### modify 関数

We will also define a helper function, modify, to describe how a class environment can be updated to reflect a new binding of a Class value to a given identifier:

我々は、特定の識別子にクラス値の新しいバインドを反映するようにクラス環境の更新方法が記述してある、ヘルパー関数modifyも定義します。

	  modify       :: ClassEnv -> Id -> Class -> ClassEnv
	  modify ce i c = ce{classes = \j -> if i==j then Just c
	                                             else classes ce j}

The defaults component of a ClassEnv value is used to provide a list of types for defaulting, as described in Section 11.5.1.

ClassEnv変数のデフォルトコンポーネントはデフォルトの型のリストを提供するために使用され、セクション 11.5.1 で説明しています。

Haskell allows programmers to specify a value for this list using a default declaration; if no explicit declaration is given, then a default (Integer,Double) declaration is assumed.

Haskellはプログラマがデフォルト宣言を使用して、このリストの値を指定することができます; 明示的な宣言が指定されていない場合、デフォルト（Integer, Double）宣言が想定されます。

It is easy to describe this using the ClassEnv type.

ClassEnv 型を使用してこれを記述するは簡単です。

For example, cedefaults=[tInt] is the result of modifying a class environment ce to reflect the presence of a default (Int) declaration.

例えば、 cedefalts=[tInt]は、デフォルト（Int）宣言の存在を反映するために、クラス環境 ce を変更した結果です。

Further discussion of defaulting is deferred to Section 11.5.1.

デフォルトの詳細については、セクション 11.5.1 まで遅延されます。

#### initialEnv

In the remainder of this section, we will show how to build an appropriate class environment for a given program, starting from an (almost) empty class environment, and extending it as necessary to reflect the effect of each class or instance declaration in the program.

このセクションの残りで我々は（ほぼ）空のクラス環境から開始して各プログラム内のクラスまたはインスタンス宣言の効果を反映するために必要に応じて拡張する与えられたプログラムのための適切なクラス環境を構築する方法を見ます。

The initial class environment is defined as follows:

初期クラス環境は次のとおりです。

	  initialEnv :: ClassEnv
	  initialEnv  = ClassEnv { classes  = \i -> fail "class not defined",
	                           defaults = [tInteger, tDouble] }

#### EnvTransformer

As we process each class or instance declaration in a program, we transform the initial class environment to add entries, either for a new class, or for a new instance, respectively.

我々の処理プログラムで各クラスまたはインスタンスの宣言方法は、我々 はエントリを追加する、新しいクラスや新しいインスタンスそれぞれ初期クラス環境を変換します。

In either case, there is a possibility that the new declaration might be incompatible with the previous declarations, attempting, for example, to redefine an existing class or instance.

どちらの場合も、新しい宣言をしようとすると、たとえば、既存のクラスまたはインスタンスを再定義する前の宣言と互換性があるかもしれないという可能性があります。

For this reason, we will describe transformations of a class environment as functions of the EnvTransformer type, using a Maybe type to allow for the possibility of errors:

このような理由から、述べるクラス環境の変換エラーの可能性を許可するようにおそらくタイプを使用して、EnvTransformer 型の関数として。

	  type EnvTransformer = ClassEnv -> Maybe ClassEnv

#### (<:>)

The sequencing of multiple transformers can be described by a (forward) composition operator (<:>):

複数変圧器のシーケンスは、次の (前方) 構成の演算子 (<:>) で表現できます。

	  infixr 5 <:>
	  (<:>)       :: EnvTransformer -> EnvTransformer -> EnvTransformer
	  (f <:> g) ce = do ce' <- f ce
	                    g ce'

Some readers will recognize this as a special case of the more general Kleisli composition operator; without the type declaration, the definition given here would work for any monad and for any element types, not just for Maybe and ClassEnv.

何人かの読者はこのより一般的な Kleisli 合成演算子; の特殊なケースとして認識します型宣言を使用しないでここに与えられた定義任意モナドと、任意の種類の要素だけではなくのために働く多分と ClassEnv。

#### addClass

To add a new class to an environment, we must check that there is not already a class with the same name, and that all of the named superclasses are already defined.

環境に新しいクラスを追加するには、がない既に同じ名前を持つクラスとすべての名前付きのスーパークラスが既に定義されていることを確認する必要があります。

This is a simple way of enforcing Haskell's restriction that the superclass hierarchy be acyclic.

これはスーパークラス階層が非環式 Haskell の制限を強制する簡単な方法です。

Of course, in practice, it will be necessary to topologically sort the set of class declarations in a program to determine a suitable ordering; any cycles in the hierarchy will typically be detected at this stage.

もちろん、実際には、それする必要がありますトポロジー; 適切な順序を決定するプログラム内のクラス宣言のセットの並べ替え階層内の任意のサイクルは、通常この段階で検出されます。

	  addClass                              :: Id -> [Id] -> EnvTransformer
	  addClass i is ce
	   | defined (classes ce i)              = fail "class already defined"
	   | any (not . defined . classes ce) is = fail "superclass not defined"
	   | otherwise                           = return (modify ce i (is, []))

#### addPreludeClasses, addCoreClasses, addNumClasses 関数

For example, we can describe the effect of the class declarations in the Haskell prelude using the following transformer:

たとえば、次のトランスを用いた Haskell プレリュードでクラス宣言の効果について述べることができます:

	  addPreludeClasses :: EnvTransformer
	  addPreludeClasses  = addCoreClasses <:> addNumClasses

This definition breaks down the set of standard Haskell classes into two separate pieces.

この定義は 2 つの別個の部分に標準の Haskell のクラスのセットに分割します。

The core classes are described as follows:

コアクラスは次のとおりです:

	  addCoreClasses ::   EnvTransformer
	  addCoreClasses  =   addClass "Eq" []
	                  <:> addClass "Ord" ["Eq"]
	                  <:> addClass "Show" []
	                  <:> addClass "Read" []
	                  <:> addClass "Bounded" []
	                  <:> addClass "Enum" []
	                  <:> addClass "Functor" []
	                  <:> addClass "Monad" []

The hierarchy of numeric classes is captured separately in the following definition:

数値クラスの階層構造は次の定義で個別にキャプチャされます:

	  addNumClasses  ::   EnvTransformer
	  addNumClasses   =   addClass "Num" ["Eq", "Show"]
	                  <:> addClass "Real" ["Num", "Ord"]
	                  <:> addClass "Fractional" ["Num"]
	                  <:> addClass "Integral" ["Real", "Enum"]
	                  <:> addClass "RealFrac" ["Real", "Fractional"]
	                  <:> addClass "Floating" ["Fractional"]
	                  <:> addClass "RealFloat" ["RealFrac", "Floating"]

#### addInst関数

To add a new instance to a class, we must check that the class to which the instance applies is defined, and that the new instance does not overlap with any previously declared instance:

クラスに新しいインスタンスを追加するには、インスタンスが適用されるクラスが定義されていることと、新しいインスタンスが宣言済みのインスタンスと重複しないをチェックする必要があります:

	  addInst                        :: [Pred] -> Pred -> EnvTransformer
	  addInst ps p@(IsIn i _) ce
	   | not (defined (classes ce i)) = fail "no class for instance"
	   | any (overlap p) qs           = fail "overlapping instance"
	   | otherwise                    = return (modify ce i c)
	     where its = insts ce i
	           qs  = [ q | (_ :=> q) <- its ]
	           c   = (super ce i, (ps:=>p) : its)

Two instances for a class are said to overlap if there is some predicate that is a substitution instance of the heads of both instance declarations.

#### overlap関数

クラスの 2 つのインスタンスはインスタンスの宣言は両方の頭の置換インスタンスですいくつかの述語がある場合に重複と言われます。

It is easy to test for overlapping predicates using the functions that we have defined previously:

私たちが以前に定義した関数を使用して述語の重複のテストに簡単です:

	  overlap       :: Pred -> Pred -> Bool
	  overlap p q    = defined (mguPred p q)

This test covers simple cases where a program provides two instance declarations for the same type (for example, two declarations for Eq Int), but it also covers cases where more interesting overlaps occur (for example, between the predicates Eq [Int] and Eq [a], or between predicates Eq (a,Bool) and Eq (Int,b)).

このテストは単純なケースのプログラム (たとえば、2 つの宣言の Eq Int)、同じ型の 2 つのインスタンスの宣言を提供しますが、もっと面白い重複が (たとえば、[Int] Eq と Eq [a] 述語間または間述語 (Bool) Eq と Eq (Int, b)) が発生するところのケースをカバーしていますカバーしています。

In each case, the existence of an overlap indicates the possibility of a semantic ambiguity, with two applicable instance declarations, and no clear reason to prefer one over the other.

それぞれのケースでの重複の存在を他上の 1 つを好むが明確な理由と 2 つの該当するインスタンス宣言の意味的曖昧性の可能性を示します。

This is why Haskell treats such overlaps as an error.

これはなぜ Haskell はこのような重複がエラーとして扱われます。

Extensions to Haskell to support overlapping instances in certain special cases have been considered elsewhere; they appear to have interesting applications, but also have some potentially troublesome impact on program semantics [ Peyton Jones et al. , 1997].

ある特殊な場合重複するインスタンスをサポートする Haskell の拡張機能が別の場所でと考えられています。彼らは興味深いアプリケーションがまたいくつかの潜在的に厄介な影響プログラム セマンティクス [ペイトン ・ ジョーンズ et al. 1997] に表示されます。

We will not consider such issues further in this paper.

さらに本稿ではそのような問題は考慮されません。

#### exampleInsts

To illustrate how the addInst function might be used, the following definition shows how the standard prelude class environment can be extended to include the four instances for Ord from the example in Section 7.1:

AddInst 関数の使い方を説明するために次の定義は ord セクション 7.1 の例から 4 つのインスタンスを含めるための標準的なプレリュード クラス環境の拡張方法について示しています:

	  exampleInsts ::  EnvTransformer
	  exampleInsts =   addPreludeClasses
	               <:> addInst [] (IsIn "Ord" tUnit)
	               <:> addInst [] (IsIn "Ord" tChar)
	               <:> addInst [] (IsIn "Ord" tInt)
	               <:> addInst [IsIn "Ord" (TVar (Tyvar "a" Star)),
	                            IsIn "Ord" (TVar (Tyvar "b" Star))]
	                           (IsIn "Ord" (pair (TVar (Tyvar "a" Star))
	                                             (TVar (Tyvar "b" Star))))

The Haskell report imposes some further restrictions on class and instance declarations that are not enforced by the definitions of addClass and addInst.

Haskell report addClass と addInst の定義によって強制されないクラスとインスタンスの宣言にさらにいくつか制限を課しています。

For example, the superclasses of a class should have the same kind as the class itself; the parameters of any predicates in an instance context should be type variables, each of which should appear in the head of the instance; and the type appearing in the head of an instance should consist of a type constructor applied to a sequence of distinct type variable arguments.

たとえば、クラスのスーパークラス化必要は、同じようなクラス自体;インスタンスのコンテキストで任意の述語のパラメーター型の変数は、それぞれのインスタンス; の頭の中で表示する必要があります: 必要があります。インスタンスの頭の中に表示される型の型可変個の引数のシーケンスに適用される型のコンス トラクターに成るべきであります。

Because these conditions have no direct impact on type checking, and because they are straightforward but tedious to verify, we have chosen not to include tests for them here, and instead assume that they have been checked during static analysis prior to type checking.

これらの条件は型チェックに直接影響を有しないし、彼らはので簡単ですが退屈を確認する、含めない分野であるのでそれらのためのテストここと代わりに想定する型チェックの前に静的分析中に、彼らがチェックされたことです。

