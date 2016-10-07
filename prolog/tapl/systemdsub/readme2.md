# System D<:

<sup><sub>formulation 式、定式化 For the moment 現時点では</sub></sup>

<!--
Figure 1 summarizes our formulation of System D<:.
-->

図1は我々のSystem D<: の式を要約したものです。

<!--
Its term language is essentially the lambda calculus, with one additional form of value: A type tag {A = T} is a value that associates a label A with a type T.
-->

これらの項言語は本質的にラベルAと型Tが関連づいている型タグ{A=T}の値の形式が1つ追加されたラムダ計算です。

<!--
For the moment we need only a single type label, so A can be regarded as ranging over an alphabet with just one name. 
-->

現時点では、我々は一つの型ラベルだけが必要で、Aはわずか1つの名前でアルファベット上の範囲とみなすことができます。

<!--
This will be generalized later.
-->

これは、後に一般化されます。

----

<!--
Our description differs from Rompf and Amin (2015) in two aspects.
-->

我々の説明は二つの側面でRompfとAmin（2015）とは異なります。

<!--
First, terms are restricted to ANF form.
-->

まず、項はANF形式に制限されています。

<!--
That is, every intermediate value is abstracted out in a let binding.
-->

これは、すべての中間値はlet束縛の中で抽象化されています。

<!--
Second, evaluation is expressed by a small step reduction relation, as opposed to a big-step evaluator.
-->

次に、ビッグステップ評価とは対照的に、評価規則は小ステップ還元関係で表されます。

<!--
Reduction uses only variable/variable renamings instead of full substitution.
-->

還元は完全な置換の代わりに唯一の変数/変数のリネームを使用しています。

<!--
Instead of being copied by a substitution step, values stay in their let bindings.
-->

置換ステップによってコピーされる代わりに、値はそれらのlet束縛にとどまります。

<!--
This is similar to the techniques used in the call-by-need lambda calculus (Ariola et al., 1995).
-->

これは、call-by-need ラムダ計算(Ariola et al., 1995)に使用される技術に類似しています。

----

<!--
We use Barendregt’s Variable Convention throughout.
-->

我々は全体を通してBarendregtの変数規約を使用しています。

<!--
For example, in the third evaluation rule, which un-nests let-bindings, we assume that we can appropriately α-rename the variable y which changes scope so that it is not captured in the final term u.
-->

例えば、３つ目の評価ルールでは、ネストしていないlet束縛で、我々は適切にそれが最終的なｋ的なの項uで捕捉されないようにスコープを変更する変数yをα変換できることを前提としています。

----

<!--
The type assignment rules in Figure 1 define a straightforward dependent typing discipline.
-->

図1のタイプの割り当てルールは単純依存型付け規則を定義します。

<!--
A lambda abstraction has a dependent function type ∀(x:S)T.
-->

ラムダ抽象は、依存関数型∀(x:S)Tを持ちます。

<!--
This is like a dependent product Π(x:S)T in LF (Harper et al., 1993), but with the restriction that the variable x can be instantiated only with other variables, not general terms.
-->

これは、LF (Harper et al., 1993)の依存生成物Π(x:S)Tのようなものですが、一般的ではない項である制限変数xは唯一の他の変数をインスタンス化することができます。

<!--
Type tags have types of the form {A : S..U}, they represent types labeled A which are lower-bounded by S and upper-bounded by U.
-->

型タグは{A : S..U}の形式の型を持ち、Sで下有界がSかつ上有界がUであるAでラベル付けられた型を表します。

<!--
A type tag referring to one specific type is expressed by having the lower and upper bound coincide, as in {A : T..T}.
-->

一つの特定の型を参照する型タグは{A : T..T}のように、下限と上限を一致させることによって表現されます。

<!--
The type of a variable x referring to a type tag can be recovered with a type projection x.A.
-->

型タグを参照している変数xの型は、型の投影x.Aで取り出すことができます。

----

<!--
The subtyping rules in Figure 1 define a preorder `S <: T` between types with rules (Refl) and (Trans).
-->

図1のサブタイプのルールは、先行して`S <: T`型間の（Reful 反射）及び（Trans トランス）のルールを定義します。

<!--
They specify ⊤ and ⊥ as greatest and least types (Top), (Bot),
and make a type projection x.A a supertype of its lower bound (<:-Sel) and a subtype of its upper bound (Sel-<:).
-->

彼らは、⊤と⊥最大と最小のタイプ（Top）、（Bot）を明示し、
その下限(<:-Sel)とその上限(Sel-<:)のサブタイプの型の投影のスーパータイプx.Aを作ります。

<!--
Furthermore, the standard co/contravariant subtyping relationships are introduced between pairs of function types (All-<:-All) and tagged types (Typ-<:-Typ).
-->

さらに、標準的な共同/反変サブタイプの関係は、関数型(All-<:-All)とタグ付けされた型(Typ-<:-Typ)のペアの間に導入されました。

----

<!--
System D<: can encode System F<: as we will see in section 3.
-->

我々が3章で見るように System D<: は、System F<: をエンコードすることができます。

<!--
However, unlike System F<:, System D<: does not have type variables.
-->

しかし、 System F<: とは異なり、 System D<: は、型変数を持っていません。

<!--
Instead, type definitions, such as {A = T}, are first-class values of type {A : T..T}.
-->

代わりに、`{A = T}` のような型定義は、型 `{A : T..T}` のファーストクラス値です。

<!--
Combined with dependent functions, these path-dependent types can express the idioms of type variables, such as polymorphism.
-->

依存関数と組み合わせることで、これらの経路依存型は、ポリモーフィズムのような型変数のイディオムを表現することができます。

<!--
For example, take the polymorphic identity function in System F<: :
-->

例えば、System F<: の多型恒等関数は：

	⊢ Λ(α <: ⊤).λ(x : α).x : ∀(α <: ⊤).α → α

<!--
and in System D<: :
-->

であり、System D<:では以下のようになります :


	⊢ λ(a : {A : ⊥..⊤}).λ(x : a.A).x : ∀(a:{A : ⊥..⊤})∀(x:a.A)a.A

-----

<!--
Like in System F<:, we can apply the polymorphic identity function to some type, say T, to get the identity function on T:
-->

System F<: と同様、我々はいくつかの型に多型恒等関数を適用することができるように、Tを用いてT上の恒等関数を取得します：

	⊢ let f =... in let a = {A = T} in f a : ∀(x:T)T

<!--
The role of subtyping is essential: (1) the argument a of type {A : T..T} can be used for the parameter a of type {A : ⊥..⊤}, (2) the dependent result type ∀(x:a.A)a.A can be converted to ∀(x:T)T because T <: a.A <: T. 
-->

サブタイプの役割は本質的に:

（1）型 `{A：T..T}` の引数は型 `{A:⊥..⊤}`のパラメータを使用することが可能であり、
（2）依存結果型 `∀(x:a.A)a.A` は`T <: a.A <: T` であるため `∀(x:T)T` に変換することができます。
