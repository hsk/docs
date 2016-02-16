## 5 型推論

<!--original
## 5 Type Inference
-->

  我々は型推論の問題を、各プログラムの式に（おそらくオーバーロードされた）型を割り当てる問題と、プログラムコードが明示的な辞書のメソッド関数を取り出す辞書変換の問題に分離します。

<!--original
  We will separate the issues of type inference, in which each program expression is assigned a (possibly overloaded) type, and dictionary conversion, in which the program code is transformed to explicitly extract method functions from dictionaries.
-->

  MLスタイルの型推論を使用する実装は十分に文書化されており（例えば[4]を参照）、我々はここで、これを繰り返すことはしません。
  その代わり、型クラスをサポートするようにMLスタイルの型推論を拡張するために必要とされる比較的小さな変更に集中します。

<!--original
  The use and implementation of ML style type inference is well documented and we will not repeat this here (see [4] for example).
  Instead, we concentrate on the relatively minor changes that are needed to extend ML style type inference with support for type classes.
-->

  通常のMLの型検査と同様に、型変数とunifificationが中心的な役割を果たします。
  型変数は 'unknown'(未知)の型に対応し、最初は結合していません。
  型検査が進むにつれ、変数の型に割り当て可能な値の様々な制約は、例えば、値の型と実際に適用されるように指定された関数引数の型が同じであることを保証することとして表れます。
  これらの制約は、より正確な型に結合していない型の変数をインスタンス化することによって解決されます。
  型クラスは、各インスタンス生成型変数に追加フィールド（コンテキスト、（リストによって表される）クラスの集合）を必要とします。

<!--original
  As in ordinary ML typechecking, type variables and unification play a central role.
  Type variables are initially unbound, corresponding to `unknown' types.
  As type checking proceeds, various constraints on the values that can be assigned to type variables are exposed, for example by ensuring that the argument type of a given function is the same as the type of the value to which it is actually applied.
  These constraints are solved by instantiating unbound type variables to more accurate types.
  Type classes require an additional field in each uninstantiated type variable: the context, a set (represented by a list) of classes.
-->

  単一化は非常に単純な方法で影響を受けます: 型変数がインスタンス化されるとき、そのクラス制約がインスタンス化値に渡さなければなりません。
  これは別の型の変数である場合は、そのコンテキストがインスタンス化された変数のコンテキストによって、和集合を使用して、強化されています。
  コンテキストが型に渡された場合、コンストラクタコンテキストの還元が必要とされます。
  コンテキストの還元は、型変数のすべてのクラス制約を伝播するために、静的な型環境でインスタンス宣言を使用しています。

<!--original
  Unification is affected in a very simple way: when a type variable is instantiated, its class constraints must be passed on to the instantiated value.
  If this is another type variable, its context is augmented, using set union, by the context of the instantiated variable.
  When a context is passed on to a type constructor context reduction is required.
  Context reduction uses the instance declarations in the static type environment to propagate all class constraints to type variables.
-->

  コンテキスト還元により還元される型のコンストラクタは、還元クラスのインスタンスでなければなりません。
  そうでない場合、型検査は試行が対応するクラスのインスタンスではない型で、オーバーロード演算子を使用するためになされたもので、エラーで失敗します。
  インスタンス宣言は、データ型とクラスをリンク発見された場合、インスタンス宣言のコンテキストは型コンストラクタの引数に伝播します。
  このプロセスはコンテキストが型変数を排他的に伝播されるまで、継続します。

<!--original
  The type constructor being reduced by context reduction must be an instance of the reducing class.
  If not, type checking fails with an error that an attempt has been made to use an overloaded operator at a type that is not an instance of the corresponding class.
  If an instance declaration is found linking the data type and the class, the context of the instance declaration propagates to the type constructor arguments.
  This process continues until contexts have been propagated exclusively to type variables.
-->

  例えば、Eq a => a(Eq コンテキストを持つ型変数a)と型[Integer]の単一化を検討しましょう。
  型変数は、[Integer]にインスタンス化されます。
  コンテキスト還元する前では、結果の型は、Eq [Integer] => [Integer]です。
  リストデータの型のEqクラスのインスタンス宣言のオーバーロードが存在し（そうでなければ、型エラーが発生する）、そしてリスト型コンストラクタへの引数にコンテキストEqが伝播します。
  これは、型 Eq Integer ​​=> [Integer]につながります。
  ここで、我々はまた、上記プログラムがclass EqのインスタンスIntegerを作成するインスタンス宣言を含まなければならないことがわかります。
  これが真であると仮定すると、Integer型コンストラクタが引数を取らないので、結果の型として[Integer]以外の制約は存在しません。
  しかし、必要なインスタンス宣言が静的型環境で検出されなかった場合は、単一化が失敗することに、注意してください。
  同様の処理により、Eq a => aと[b]型の単一化は Eq b => [b] をもたらすでしょう。
  ここで、コンテキストは、結果の型変数に付属したままです。

<!--original
  As an example, consider the unification of Eq a => a, a type variable with an Eq context, and the type [Integer].
  The type variable is instantiated to [Integer].
  Before context reduction, the resulting type is Eq [Integer] => [Integer].
  The instance declaration for class Eq over the list data type exists (otherwise a type error occurs) and propagates the context Eq to the argument to the list type constructor.
  This leads to the type Eq Integer => [Integer].
  Now we can see that the program must also include an instance declaration that makes Integer an instance of the class Eq.
  Assuming that this is true, and since the Integer type constructor does not take any arguments, no further constraints can exist leaving only [Integer] as the resulting type.
  Note, however, that the unification would have failed if the required instance declarations were not found in the static type environment.
  By a similar process, unification of Eq a => a and [b] would yield the type Eq b => [b].
  Here, contexts remain attached to the resulting type variables.
-->

  次のコードでは型クラスがある型変数のインスタンス化を実装しています。
  各型変数は、（インスタンス生成されていない）nullか、またはインスタンス化された型が含まれているいずれかの値フィールドがあります。
  コンテキストのフ​​ィールドはインスタンス化されていない型変数に付属しているクラスのリストです。
  findInstanceContext関数は、選択されたクラスとデータ型を持つインスタンスの静的型環境を検索します。
  発見されていない場合、この関数は、型エラーを通知します。
  これは、各引数ごとにデータ型への引数が１つあるコンテキストのリストを返します。

<!--original
  The following code implements type variable instantiation in the presence of type classes.
  Each type variable has a value field which is either null (uninstantiated) or contains an instaniated type.
  The context field is a list of classes attached to uninstantiated type variables.
  The findInstanceContext function searches the static type environment for an instance with the selected class and data type.
  If not is found this function signals a type error.
  It returns a list of contexts, one for each argument to the data type.
-->

    instantiateTyvar(tyvar,type)
      tyvar.value := type
      propagateClasses(tyvar.context,type)

    propagateClasses(classes,type)
      if tyvar(type)
        then type.context := union(classes,type.context)
        else for each c in classes
              propagateClassTycon(c,type)

    propagateClassTycon(class,type)
      s = findInstanceContext(type.tycon,class)
      for each classSet in s, typeArg in tycon.args
       propagateClasses(classSet,typeArg)


  MLの型推論にもう一つの小さな変更が必要です。
  letrecが型検査されたときに、letrecにより定義されたすべての変数は共通のコンテキストを共有します。
  これは、8.3節で考察します。

<!--original
  One other minor change to ML type inference is required.
  When a letrec is typechecked all variables defined by the letrec share a common context.
  This will be discussed in Section 8.3.
-->

  コンテキスト還元は型クラスを含むHaskellプログラムの正しい型付けを推論するために必要なMLの型推論処理への唯一の重要な変更であり、強調する価値があります。
  一方、辞書変換（またはいくつかの同様のプロセス）は、次のセクションで説明され、型検査されたプログラムの最終的な実行可能ファイルのバージョンでオーバーロードを実現するために行われなければなりません。

<!--original
  It is worth emphasizing that context reduction is the only significant change to the ML type inference process necessary to infer correct typings for Haskell programs involving type classes.
  On the other hand, dictionary conversion, as described in the following section (or some similar process), must be carried out to implement overloading in the final executable version of the type checked program.
-->
