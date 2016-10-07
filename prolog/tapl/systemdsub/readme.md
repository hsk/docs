# System D<:

<sup><sub>formulation 式、定式化 For the moment 現時点では</sub></sup>

<sub><sup>
Figure 1 summarizes our formulation of System D<:.
</sup></sub>

図1は我々のSystem D<: の式を要約したものです。

<sub><sup>
Its term language is essentially the lambda calculus, with one additional form of value: A type tag {A = T} is a value that associates a label A with a type T.
</sup></sub>

これらの項言語は本質的にラベルAと型Tが関連づいている型タグ{A=T}の値の形式が1つ追加されたラムダ計算です。

<sub><sup>
For the moment we need only a single type label, so A can be regarded as ranging over an alphabet with just one name. 
</sup></sub>

現時点では、我々は一つの型ラベルだけが必要で、Aはわずか1つの名前でアルファベット上の範囲とみなすことができます。

<sub><sup>
This will be generalized later.
</sup></sub>

これは、後に一般化されます。

----

<sub><sup>
Our description differs from Rompf and Amin (2015) in two aspects.
</sup></sub>

我々の説明は二つの側面でRompfとAmin（2015）とは異なります。

<sub><sup>
First, terms are restricted to ANF form.
</sup></sub>

まず、項はANF形式に制限されています。

<sub><sup>
That is, every intermediate value is abstracted out in a let binding.
</sup></sub>

これは、すべての中間値はlet束縛の中で抽象化されています。

<sub><sup>
Second, evaluation is expressed by a small step reduction relation, as opposed to a big-step evaluator.
</sup></sub>

次に、ビッグステップ評価とは対照的に、評価規則は小ステップ還元関係で表されます。

<sub><sup>
Reduction uses only variable/variable renamings instead of full substitution.
</sup></sub>

還元は完全な置換の代わりに唯一の変数/変数のリネームを使用しています。

<sub><sup>
Instead of being copied by a substitution step, values stay in their let bindings.
</sup></sub>

置換ステップによってコピーされる代わりに、値はそれらのlet束縛にとどまります。

<sub><sup>
This is similar to the techniques used in the call-by-need lambda calculus (Ariola et al., 1995).
</sup></sub>

これは、call-by-need ラムダ計算(Ariola et al., 1995)に使用される技術に類似しています。

----

<sub><sup>
We use Barendregt’s Variable Convention throughout.
</sup></sub>

我々は全体を通してBarendregtの変数規約を使用しています。

<sub><sup>
For example, in the third evaluation rule, which un-nests let-bindings, we assume that we can appropriately α-rename the variable y which changes scope so that it is not captured in the final term u.
</sup></sub>

例えば、３つ目の評価ルールでは、ネストしていないlet束縛で、我々は適切にそれが最終的に項uで捕捉されないようにスコープを変更する変数yをα変換できることを前提としています。

----

<sub><sup>
The type assignment rules in Figure 1 define a straightforward dependent typing discipline.
</sup></sub>

図1のタイプの割り当てルールは単純依存型付け規則を定義します。

<sub><sup>
A lambda abstraction has a dependent function type ∀(x:S)T.
</sup></sub>

ラムダ抽象は、依存関数型∀(x:S)Tを持ちます。

<sub><sup>
This is like a dependent product Π(x:S)T in LF (Harper et al., 1993), but with the restriction that the variable x can be instantiated only with other variables, not general terms.
</sup></sub>

これは、LF (Harper et al., 1993)の依存生成物Π(x:S)Tのようなものですが、一般的ではない項である制限変数xは唯一の他の変数をインスタンス化することができます。

<sub><sup>
Type tags have types of the form {A : S..U}, they represent types labeled A which are lower-bounded by S and upper-bounded by U.
</sup></sub>

型タグは{A : S..U}の形式の型を持ち、Sで下有界がSかつ上有界がUであるAでラベル付けられた型を表します。

<sub><sup>
A type tag referring to one specific type is expressed by having the lower and upper bound coincide, as in {A : T..T}.
</sup></sub>

一つの特定の型を参照する型タグは{A : T..T}のように、下限と上限を一致させることによって表現されます。

<sub><sup>
The type of a variable x referring to a type tag can be recovered with a type projection x.A.
</sup></sub>

型タグを参照している変数xの型は、型の投影x.Aで取り出すことができます。

----

<sub><sup>
The subtyping rules in Figure 1 define a preorder `S <: T` between types with rules (Refl) and (Trans).
</sup></sub>

図1のサブタイプのルールは、先行して`S <: T`型間の（Reful 反射）及び（Trans トランス）のルールを定義します。

<sub><sup>
They specify ⊤ and ⊥ as greatest and least types (Top), (Bot),
and make a type projection x.A a supertype of its lower bound (<:-Sel) and a subtype of its upper bound (Sel-<:).
</sup></sub>

彼らは、⊤と⊥最大と最小のタイプ（Top）、（Bot）を明示し、
その下限(<:-Sel)とその上限(Sel-<:)のサブタイプの型の投影のスーパータイプx.Aを作ります。

<sub><sup>
Furthermore, the standard co/contravariant subtyping relationships are introduced between pairs of function types (All-<:-All) and tagged types (Typ-<:-Typ).
</sup></sub>

さらに、標準的な共同/反変サブタイプの関係は、関数型(All-<:-All)とタグ付けされた型(Typ-<:-Typ)のペアの間に導入されました。

----

<sub><sup>
System D<: can encode System F<: as we will see in section 3.
</sup></sub>

我々が3章で見るように System D<: は、System F<: をエンコードすることができます。

<sub><sup>
However, unlike System F<:, System D<: does not have type variables.
</sup></sub>

しかし、 System F<: とは異なり、 System D<: は、型変数を持っていません。

<sub><sup>
Instead, type definitions, such as {A = T}, are first-class values of type {A : T..T}.
</sup></sub>

代わりに、`{A = T}` のような型定義は、型 `{A : T..T}` のファーストクラス値です。

<sub><sup>
Combined with dependent functions, these path-dependent types can express the idioms of type variables, such as polymorphism.
</sup></sub>

依存関数と組み合わせることで、これらの経路依存型は、ポリモーフィズムのような型変数のイディオムを表現することができます。

<sub><sup>
For example, take the polymorphic identity function in System F<: :
</sup></sub>

例えば、System F<: の多相的な恒等関数は：

	⊢ Λ(α <: ⊤).λ(x : α).x : ∀(α <: ⊤).α → α

<sub><sup>
and in System D<: :
</sup></sub>

であり、System D<:では以下のようになります :


	⊢ λ(a : {A : ⊥..⊤}).λ(x : a.A).x : ∀(a:{A : ⊥..⊤})∀(x:a.A)a.A

-----

<sub><sup>
Like in System F<:, we can apply the polymorphic identity function to some type, say T, to get the identity function on T:
</sup></sub>

System F<: と同様、我々はいくつかの型に多相的な恒等関数を適用することができるように、Tを用いてT上の恒等関数を取得します：

	⊢ let f =... in let a = {A = T} in f a : ∀(x:T)T

<sub><sup>
The role of subtyping is essential: (1) the argument a of type {A : T..T} can be used for the parameter a of type {A : ⊥..⊤}, (2) the dependent result type ∀(x:a.A)a.A can be converted to ∀(x:T)T because T <: a.A <: T. 
</sup></sub>

サブタイプの役割は本質的に:

（1）型 `{A：T..T}` の引数は型 `{A:⊥..⊤}`のパラメータを使用することが可能であり、
（2）依存結果型 `∀(x:a.A)a.A` は`T <: a.A <: T` であるため `∀(x:T)T` に変換することができます。


## 2.1 Example: Dependent Sums

## 2.1 例: 依存 Sums

Dependent sums can be encoded using dependent functions, through an encoding similar to that of existential types in System F.

System F における存在型のものと同様の符号化を通じて、依存 sums は 依存関数を使ってエンコードできます。

	                  Σ(x : S)T ≡ ∀(z :{A : ⊥..>})∀(f :∀(x:S)∀(y :T)z.A)z.A
	   pack [x, y] as Σ(x : S)T ≡ λ(z :{A : ⊥..>})λ(f :∀(x:S)∀(y :T)z.A)f x y
	unpack x : S, y : T = t in u ≡ let z1 = t in let z2 = {A = U} in
	                                let z3 = (λ(x:S)λ(y :T)u) in
	                                let z4 = z1 z2 in z4 z3
	                         z.1 ≡ unpack x : S, y : T = z in x
	                         z.2 ≡ unpack x : S, y : T = z in y

where U is the type of u.

ここで、`U`は`u`の型です。

The associated, admissible subtyping and typing rules are easy to derive and can be found in Appendix A.1.

関連する、許容サブタイピングと型付け規則は導出が容易であり、付録A.1に記載されています。

Note that

1. unpacking via unpack x, y = t in u is only allowed if x and y do not appear free in the type U of u,
2. similarly, the second projection operator −.2 may only be used if x does not appear free in T.
	In such cases, we have Σ(x : S)T = S × T, i.e. z is in fact an ordinary pair.

以下に注意してください:

1. `u`の型`U`内で`x`と`y`が自由に現れない場合、`unpack x, y = t in u`を経てunpackingは、許可されています。
2. 同様に、`T`内に`x`が自由に現れない場合は第2の射影演算子ー`.2`にのみ使用することができます。

These restrictions may come as a surprise: while they are similar to the hygiene conditions imposed on existential types in System F/F<:, they do not apply to dependent sums in (fully) dependently typed languages.

これらの制限は驚きとして来ることがあります: 彼らは、System F/F<:で存在型に課せられた衛生条件に似ていながら,彼らは（完全に）依存型付け言語に依存sumsには適用されません。

In such languages, the bound names x, y can be prevented from leaking into the overall type of an unpack statement by substituting the projections t.1 and t.2 for occurrences of x and y in U.

そのような言語では、バインドされた名前`x,y`は`U`内の`x`と`y`の出現のための射影`t.1`および`t.2`を置換することによりunpack文全体の型へ漏れるのを防止することができます。

The same is true for the return type of the second projection z.2, which would be [x := z.1]T.

`[x := z.1]T`となる第2の射影`z.2`の戻り値の型についても同様です。

Unfortunately, such substitutions are forbidden in D<: because types may only depend on variables, as opposed to arbitrary terms (like z.1 or t.2).

型は変数だけに依存する可能性があるため、残念ながら、このような置換はD <: で禁止されており、（`z.1`または`t.2`のような）任意の項とは対照的です。

For the same reason, the typing rule (Let) for let expressions features a similar hygiene condition.

同じ理由で、let式の型付け規則（Let）は、同様の衛生条件を備えています。

Finally, note that the above encoding allows for the unrestricted projection of “existential witnesses” via −.1, whereas no such operation exists on existential types in System F/F<:.

最後に、そのような操作は、System F/F<:.における存在型に存在していないのに対し、上記の符号化は、`-.1`を介した「存在証明」の無制限の射影を可能にすることに注意してください。
