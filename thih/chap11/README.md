# 11 Type Inference 型推論

With this section we have reached the heart of the paper, detailing our algorithm for type inference.

このセクションで我々 は達しての紙のハート型の推定アルゴリズムを詳述しました。

It is here that we finally see how the machinery that has been built up in earlier sections is actually put to use.

それはここで我々 は最後に使用しては以前のセクションで建て込んできた機械を配置する実際に見ることです。

We develop the complete algorithm in stages, working through the abstract syntax of the input language from the simplest part (literals) to the most complex (binding groups).

アルゴリズムを開発して完全な段階では、最も簡単な部分 (リテラル) から入力言語の抽象構文を介しての作業に最も複雑な (バインド グループ)。

Most of the typing rules are expressed by functions whose types are simple variants of the following synonym:

入力規則のほとんどは関数の型が次のシノニムの単純な変形によって表されます。

	  type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)

In more theoretical treatments, it would not be surprising to see the rules expressed in terms of judgments G;P | A\vdash e:t, where G is a class environment, P is a set of predicates, A is a set of assumptions, e is an expression, and t is a corresponding type [ Jones, 1992].

理論的な処置ではない判断 G; の面で表される規則を参照してくださいに驚くべきことP |A\vdash e:t、どこ G クラス環境、P 述語のセット、仮定のセットは、電子式であり、あり t は対応する [ジョーンズ、1992年] を入力します。

Judgments like this can be thought of as 5-tuples, and the typing rules themselves just correspond to a 5-place relation.

このことができると考えることの 5 組のような入力ルール自体だけ判断 5 場所関係に対応します。

Exactly the same structure shows up in types of the form Infer e t, except that, by using functions, we distinguish very clearly between input and output parameters.

同じ構造ショーの種類フォーム Infer e t のことを除いて、関数を使用して、我々 非常に間ではっきり区別入力パラメーターと出力パラメーター。
