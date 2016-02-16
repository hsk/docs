# Implementing Type Classes

http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.53.3952&rep=rep1&type=pdf

の適当な翻訳


## 6 辞書変換

<!--original
## 6 Dictionary Conversion
-->


  辞書変換は、2つの方法で生成されたコードに影響を与えます。
  第一に、オーバーロード定義は辞書結合用の追加パラメータ変数を受け取ります。
  第二に、オーバーロード定義を参照するために辞書を渡す必要があります。
  （通常は関数ですが、任意の型であってもよい）オーバーロード定義の参照は、型が暗黙的な辞書引数が挿入されなければならないかチェックされます。したがって、型検査器は、2つの基本的な変更を必要とします。
  とき定義は（トップレベルまたはのletとletrecを使用して、ローカル定義のいずれかで）暗黙的な辞書引数は、実行時にオーバーロードを解決するために必要なすべての必要な辞書を結合するために挿入されている型付けされています。

  <!--original
  Dictionary conversion affects the generated code in two ways.
  First, overloaded definitions receive additional parameter variables to bind dictionaries.
  Second, a reference to an overloaded definition must be passed dictionaries.
  Thus the typechecker needs only two basic changes: when reference to an overloaded definition (which is usually a function but may be of any type) is type checked the hidden dictionary parameters must be inserted.
  When a definition (either at the top level or in a local definition using a let or letrec) is typed hidden dictionary arguments are inserted to bind any necessary dictionaries needed to resolve the overloading at run time.
  -->

  型シグネチャと辞書パラメータとの関係は単純です：コンテキストの各要素は、オーバーロードの定義によってに渡されるか、受け取った辞書に対応しています。
  例えば、型(Eq a、Text b) => a -> b の関数は2つの辞書、class Eqと他にTextを必要とするでしょう。
  コンテキストの順序は任意です; 辞書があれば、同じ順序が一貫して使用されるように、任意の順序で渡すことができます。

  <!--original
  The relation between a type signature and dictionary parameters is simple: each element of the context corresponds to a dictionary passed into or recieved by an overloaded definition.
  For example, a function with the type (Eq a, Text b) => a -> b would require two dictionaries, one for the class Eq and another for Text.
  The ordering of a context is arbitrary; dictionaries can be passed in any order so long as the same ordering is used consistently.
  -->

  standard MLの型検査器によって実行されるコードを舐めている間にプログラムに辞書通過コードを追加する事が、おそらく本質的な実装上の問題の対処方法です。
  式に関連する型が原因で型検査が進むにつれて単一化を変更することがあります。
  一般化でのみオーバーロードを解決するために必要な適切な辞書を決定できるので、一般化された式全体のすでにトラバースした箇所の判断ができません。
  一般化後のコードを二回走査する事を避けるために、我々はプレースホルダを使って解決不能なコードの必要な情報を保持します。
  プレースホルダは、型と型に基づいたオブジェクトの解決をキャプチャします。
  一般化されている箇所は、プレースホルダは必要な型依存コードに置換されます。

  <!--original
  Adding dictionary passing code to the program during the code walk performed by the standard ML typechecker is perhaps the essential implementation issue addressed here.
  The type associated with an expression may change due to unification as the type checker proceeds.
  Since types only stablize at generalization the appropriate dictionaries needed to resolve overloading cannot be determined until the entire expression being generalized has already been walked over.
  To avoid a second pass over the code after generalization, we will hold onto the necessary bits of unresolvable code using placeholders.
  A placeholder captures a type and an object to be resolved based on that type.
  During generalization, placeholders are replaced by the required type-dependent code.
  -->

### 6.1 プレースホルダの挿入

<!--original
### 6.1 Inserting Placeholders
-->

  型検査器が、オーバーロードされた変数、メソッド、またはletrecバインド変数のいずれかに遭遇したときにプレースホルダが挿入されます。
  わずかに異なる形のプレースホルダがそれぞれの場合で使われます。

  <!--original
  Placeholders are inserted when the type checker encounters either an overloaded variable, a method, or a letrec bound variable.
  Slightly different forms of placeholder are used in each case.
  -->

  オーバーロードされた変数は、最終的には、変数のコンテキストによって暗黙の辞書に置換され、プレースホルダーへの関数適用に書き換えられます。
  変数に関連する新たな型変数はプレースホルダにキャプチャされます。
  たとえば、fが型(Num a, Text b) => a -> b の場合、型検査器は、最初にf内の型変数を新たにインスタンス化し、(Num t1, Text t2) => t1 -> t2の型とします。
  型変数のこの新たなインスタンスは、通常のMLスタイルの型検査の一部です。
  値fは、以下の関数適用に書き換えられるでしょう: f <Num, t1> <Text, t2>。
  <object, type>表記は、プレースホルダを表すために使用されます。
  これらのプレースホルダは、fの他の引数の前に渡される追加の引数になります。
  プレースホルダに表れるTextクラスとNumクラスはプレースホルダがそのクラスの辞書をもたらす式として解決されなければならないことを示しています。

  <!--original
  Overloaded variables are rewritten as an application to placeholders that will ultimately be replaced by the dictionaries implied by the variable's context.
  The fresh type variables associated with the variable are captured in the placeholders.
  For example, if f has type (Num a, Text b) => a -> b, the type checker will first freshly instantiate the type variables in f, yielding a typing of (Num t1,Text t2) => t1 -> t2.
  This fresh instantiation of type variables is part of ordinary ML style type checking.
  The value f will be rewritten as an application: f <Num,t1> <Text,t2>.
  The <object,type> notation will be used to represent placeholders.
  These placeholders become additional arguments to f which will be placed ahead of any other arguments.
  The classes Text and Num which appear in the placeholders indicate that the placeholder must resolve to an expression yielding a dictionary for that class.
  -->


  ----

  メソッド関数は、直接プレースホルダに変換されます。
  プレースホルダ内の型変数は、クラス宣言内のクラス定義の型変数に対応しています。
  たとえば、Eqクラスで==メソッドは、Eq t1 => t1 -> t1 -> Boolで、プレースホルダ<==、t1>を返す、型の新たなインスタンスによって型検査されるでしょう。
  プレースホルダー内のオブジェクトがメソッドであるため、メソッドの特定の実装（型変数が具体的な型にインスタンス化されている場合）または、Eq 辞書から==関数を選択するコードのいずれかに解決されます。

  <!--original
  Method functions are converted directly to placeholders.
  The type variable in the placeholder corresponds to the type variable which defines the class in the class declaration.
  For example, the == method in class Eq would be typechecked by freshly instantiating its type, yielding Eq t1 => t1 -> t1 -> Bool, and returning the placeholder < == ,t1>.
  Since the object in the placeholder is a method, it will be resolved to either a specific implementation of the method (if the type variable becomes instantiated to a concrete type) or code to select a == function from an Eq dictionary.
  -->

  ----

  <!--
  Recursively defined variables cannot be converted until their type is known.
  References to such variables encountered before they are generalized are simply replaced by a placeholder until the correct context has been determined.
  For example, in a simple recursive definition such as member, the recursive call to member becomes a placeholder until its type is generalized.
  After generalization, it is treated as an ordinary overloaded variable.
  -->

  その型が分かるまで、再帰的に定義された変数は変換できません。
  正しいコンテキストが決定されるまで、それらが一般化される前に遭遇するような変数への参照は、単にプレースホルダーによって置き換えられます。
  例えば、その型が一般化されるまでは単純な再帰定義の中でこのようなメンバーとして、メンバーへの再帰呼び出しはプレースホルダになります。
  一般化した後、それは通常のオーバーロードされた変数として扱われます。


### 6.2 辞書パラメータの挿入

<!--
### 6.2 Inserting Dictionary Parameters
-->


  定義が型付けされると、定義内の型変数に関連付けられた任意のコンテキストは、オーバーロードの解決に必要な辞書を結合する辞書パラメータ変数を生成するために使用されます。
  これは、型推論の一般化部分の間に現れます。
  一般化は、定義の型のすべてのインスタンス生成型変数を収集し、これらの型変数内のすべてのコンテキストのすべての要素の新しい辞書変数を作成します。
  辞書に結合するラムダは、定義の本体にラップされ、引数環境が作成されます。
  この環境は、定義の型検査中に作成されたプレースホルダを解決するために使用されます。
  この環境は、辞書パラメータ変数にクラスと型変数のペアにマップします。

  <!--
  Once a definition has been typed, any context associated with the type variables in the definition is used to generate dictionary parameter variables which will bind the dictionaries needed to resolve the overloading.
  This occurs during the generalization portion of type inference.
  Generalization gathers all uninstantiated type variables in the type of a definition and creates a new dictionary variable for every element of every context in these type variables.
  A lambda which binds the dictionaries is wrapped around the body of the definition and a parameter environment is created.
  This environment is used to resolve placeholders created during typechecking of the definition.
  This environment maps a pair containing a class and type variable onto a dictionary parameter variable.
  -->

  簡単な例は、fの推論された型が(Num t1,Text t2) => t1 -> t2の場合、 fの定義はf = \d1 d2 -> f' に変更され、ここでf'はfの元の定義です。
  これは、次の引数環境を作成します: [((Num,t1),d1),((Text,t2),d2)]。

  <!--
  As a simple example, if the inferred type of f is (Num t1,Text t2) => t1 -> t2, then the definition of f is changed to f = \d1 d2 -> f' where f' is the original definition of f.
  This creates the following parameter environment: [((Num,t1),d1),((Text,t2),d2)].
  -->

### 6.3 プレースホルダの解決

<!--
### 6.3 Resolving Placeholders
-->

  一般化で、定義中に挿入されたプレースホルダを解決することができます。
  すべてのプレースホルダのリスト(それぞれの新しいプレースホルダが作成され上書きされたもの)は、プレースホルダ検索をするためにコードのトラバースする事を回避するために使えます。
  辞書パラメータが挿入された後に、各プレースホルダは検証されます。
  メソッドやクラスのいずれかに関連付けられているプレースホルダは、プレースホルダに関連付けられている型が、それが解決される方法を決定します。

  <!--
  At generalization, placeholders inserted into a definition can be resolved.
  A list of all placeholders, updated as each new placeholder is created, can be used to avoid walking through the code in search of placeholders.
  After dictionary parameters have been inserted, each placeholder is examined.
  For placeholders associated with either methods or classes, the type associated with the placeholder determines how it will be resolved.
  -->


  4つの可能性があります:

  - 1. 型は、引数環境内の型変数です。

    この場合のマッピングは実行時に辞書のキャリアとなる変数を定義します。
    クラスのプレースホルダは、辞書引数変数として解決されます; メソッドのプレースホルダは、辞書変数に適用するセレクタ関数を必要とします。

  - 2. 型は、型コンストラクタにインスタンス化されています。

    インスタンス宣言はこの型の与えるメソッドのプレースホルダのためのメソッド自体、またはクラスのプレースホルダの辞書変数のいずれかに関連付けられています。
    辞書やメソッドそれ自体はオーバーロードすることができるので、型検査器は、この追加のオーバーロードを解決するために再帰的にプレースホルダを生成する必要があるかもしれません。

  - 3. 型変数は、依然として外部の型環境で結合可能です。

    プレースホルダの処理は、外側の宣言に繰り越されなければなりません。

  - 4. 上記の条件のいずれもが成立しない場合、あいまいさが検出されます。

    あいまいさは、いくつかの言語固有のメカニズムによって解決されるか、単に型エラーを通知出来ます。

  <!--
  There are four possibilities:

  - 1. The type is a type variable in the parameter environment.

    In this case, the mapping defines a variable which will carry the dictionary at run-time.
    A class placeholder is resolved to the dictionary parameter variable; a method placeholder requires a selector function to be applied to the dictionary variable.
  - 2. The type has been instantiated to a type constructor.

    An instance declaration associated with this type supplies either the method itself for a method placeholder or a dictionary variable for a class placeholder.
    Since dictionaries or methods themselves may be overloaded the type checker may need to recursively generate placeholders to resolve this additional overloading.

  - 3. The type variable may still be bound in an outer type environment.

    The processing of the placeholder must be deferred to the outer declaration.
  - 4. If none of the above conditions hold, an ambiguity has been detected.

    The ambiguity may be resolved by some language specific mechanism or simply signal a type error.
  -->



  <!--
  Placeholders associated with recursive calls can be resolved in two different ways.
  The simplest way is to generate an overloaded variable reference which is no different than for other overloaded variables.
  This can only be done after generalization since the context of the recursive call is unknown until this time.
  However, since any dictionaries passed to a recursive call remain unchanged from the original entry to the function, the need to pass dictionaries to inner recursive calls can be eliminated by using an inner entry point where the dictionaries have already been bound.
  An example of this is shown in section 7.
  -->

  再帰呼び出しに関連付けられたプレースホルダは、2つの異なる方法で解決することができます。
  最も簡単な方法は、オーバーロードされた変数の参照を生成することで、他のオーバーロードされた変数と変わりありません。
  再帰呼び出しのコンテキストはこの時まで分からないので、この方法は一般化した後にだけ行うことができます。
  再帰呼び出しに渡される辞書は関数への元の引数と変わらないので、内側の再帰呼び出しに辞書を渡す必要性は辞書が既にバインドされている内側のエントリポイントを使用して除去することができます。
  この例はセクション7で示します。

<!--
## 7 例
-->

## 7 Examples

  我々は、それぞれが3つのコードツリーで構成された、２つの例で我々の型検査器の動作を説明します。
  最初のコードツリーは新たにインスタンス化された型変数(ti)と挿入されたプレースホルダを示しています。
  型変数と型テンプレートをインスタンス化するためのルールは、MLの型検査と同じです。
  第二の木は単一化した結果を示しています。
  型は、図中の線(訳の図では'...')に沿って対で単一化されています。
  最後は、一般化及びプレースホルダ分解の結果を示します。
  実際の型検査器は、継続的に代わりにすべての型の変数がinstatuatedされた後の単一化を行います;
  これらのステップは、明確にするために、ここで分離されています。

  <!--
  We will illustrate the operation of our type checker with a couple of examples, each of which consists of three code trees.
  The first code tree shows freshly instantiated type variables (the ti) and inserted placeholders.
  The rules for instantiating type variables and the type templates are the same as for ML type checking.
  The second tree shows the result of unification.
  Types are unified pairwise along the lines in the diagrams.
  Finally, the result of generalization and placeholder resolution will be shown.
  The actual type checker performs unification continuously instead of after all type variables have been instatuated;
  these steps are separated here for clarity.
  -->


  ----

  次の関数fは、メソッド、+、および自身への再帰呼び出しを使用しています。

  <!--
  The following function f uses a method, +, and a recursive call to itself.
  -->

  ----

  型変数ごとに関連付けられたコンテキストを書くのではなく、それは記載されているすべての型変数コンテキスト情報の側に表れます。

  <!--
  Rather than write the context associated with a type variable each time it is mentioned all type variable context information will be shown at the side.
  -->

    class Num a where
      (+) :: a -> a -> a
      f = \x -> x + f x

  型変数のインスタンス化とプレースホルダの挿入は、以下の式ツリーを生成します。
  @ノードはカリー化適用です。

  <!--
  Type variable instantiation and placeholder insertion produce the following expression tree.
  The @ nodes are curried applications.
  -->


    letrec f =
      (* Context: Num t6  t1 ... t2→t3 *)
      \x
        (* t3 ... t6 *)
        @
          (* t4 -> t5 -> t6 ... t7->t7->t7 *)
          <+,t7>
          (* t4 ... t2 *)
          x
          (* t5 ... t9 *)
          @
            (* t8->t9 ... t1 *)
            <f,t1>
            (* t8 ... t2 *)
            x

  単一化後、次のようになります:

  <!--
  After unification, this becomes:
  -->

    letrec f =
      (* Context: Num t2 t2->t2 *)
      \x
        (* t2 *)
        @
          (* t2->t2->t2 *)
          <+,t2>
          (* t2 *)
          x
          (* t2 *)
          @
            (* t2->t2 *)
            <f,t2>
            (* t2 *)
            x

  +に関連付けられたプレースホルダ内の型は、環境パラメータの一部です。
  これがfに渡される辞書はパラメータxのための+の適切なの実装が含まれていることを示します。
  実行時に、sel+関数は、辞書から、この加算関数を取得します。
  これは再帰呼び出しが辞書dは変化せずに引き渡す最も簡単な変換です。
  より良い選択は、dがバインドされた後、fの内側のエントリを作成し、繰り返しdを渡す事を避けるために、再帰呼び出しして使う事でしょう。

  <!--
  The type in the placeholder associated with + is part of the parameter environment.
  This indicates that a dictionary passed into f will contain the implementation of + appropriate for the parameter x.
  At execution time, the sel+ function will retrieve this addition function from the dictionary.
  This is the simplest translation in which the recursive call passes the dictionary d unchanged.
  A better choice would have been to create an inner entry to f after d is bound and use this for the recursive call to avoid passing d repeatedly.
  -->


    letrec f =
      \d
        \x
          @
            @
              sel+
              d
            x
            @
              @
                f
                d
              x

  -------

  The next example uses a previously defined overloaded function, length, with type [a] -> Int.
  The necessary class and instance declarations are included.
  We will use the convention that dictionaries are named d-class-type.

  次の例では、型[a] -> Intを使い、以前に定義されたオーバーロードされた関数lengthを、使用しています。
  必要なクラスとインスタンス宣言が含まれています。
  我々は、辞書はdクラス型と命名されている規則を使用します。

    class Text a where
      print :: a -> String

    instance (Text a, Text b) => Text (a, b) where
      print = print-tuple2

    instance Text Int where ....
    instance Text a => Text [a] where ....

    g = \x -> print (x, length x)

  After placeholder insertion and type variable instantiation:

  プレースホルダの挿入と型の変数インスタンス化後：

    (* Context: Text t5 *)
    let g =
      (* t1->t2 *)
      \x
        (* t2 ... t4 *)
        @
          (* t3->t4 ... t5->String *)
          <print,t5>
          (* t3 ... (t6, t7) *)
          2-tuple
            (* t6 ... t1 *)
            x
            (* t7 ... Int *)
            length
              (* [t8] ... t1 *)
              x


  After unification, this becomes:

  単一化後、次のようになります:

    (* Context: Text t5 *)
    let g =
      (* [t8]->String *)
      \x
        (* String *)
        @
          (* ([t8],Int)->String *)
          <print,([t8],Int)>
          (* ([t8], Int) *)
          2-tuple
            (* [t8] *)
            x
            (* Int *)
            length
              (* [t8] *)
              x

  The placeholder is resolved to a specific printer for 2-tuples.
  As this function is overloaded, further placeholder resolution is required for the types associated with the tuple components.

  プレースホルダは、2つのタプルのための特定のプリンタとして解決されます。
  この関数がオーバーロードされているように、さらにプレースホルダの分解は、タプルのコンポーネントに関連付けられている型のために必要とされます。

    let g =
      \d
        \x
          @
            @
              print-tuple2
              @
                d-Text-List
                d
              d-Text-Int
            2-tuple
              x
              length
                x

  -----

  別な書き方をしてみたバージョン

    (* Context: Text t5 *)
    let g =
      (\x(
        @(
          (<print,t5>:t3->t4 ... t5->String),
          (2-tuple(
            (x: t6 ... t1),
            (length(
              (x: [t8] ... t1)
            ):t7 ... Int)
          ):t3 ... (t6, t7))
        )
        :t2 ... t4
      ):t1->t2)

    (* Context: Text t5 *)
    let g =
      (\x(
        (@(
          (<print,([t8],Int)>:([t8],Int)->String),
          (2-tuple(
            (x:[t8]),
            (length(
              (x:[t8])
            ):Int)
          ):([t8], Int))
        ):String)
        ):[t8]->String)


    let g =
      \d(
        \x(
          @(
            @(
              print-tuple2,
              @(d-Text-List, d),
              d-Text-Int
            ),
            2-tuple(
              x,
              length(x)
            ))))

