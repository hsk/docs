# 10. The true complexity of generalization

1.

The let-generalization in OCaml is far more complex than what we have sketched earlier.
レット OCamlでは、一般化ははるかに複雑で、先ほどスケッチしたものを超えています。
This section is to help appreciate the true complexity of generalization.
このセクションでは、一般化の真の複雑さに感謝を支援することです。
The let-expression in OCaml has the general form
レット OCamlでは、式は一般的な形式を持っている

     let [rec] pattern = exp and pattern = exp ... in body

The let type checker type_let -- 160 lines of code in typecore.ml, not counting the type checking of patterns -- receives the list of pattern-expression pairs, and the recursion-flag.
のlet式チェッカーtype_let -のコードの160行typecore.ml、パターンの型チェックをカウントしていないが-パターン式のペアのリスト、および再帰フラグを受け取ります。
Here is the end of its code
ここでは、そのコードの終わりです

```
begin_def ();
...
let exp_list =
List.map2
  (fun (spat, sexp) (pat, slot) -> .... （* 式の型チェック *）
    type_expect exp_env sexp pat.pat_type)
  spat_sexp_list pat_slot_list in
...
end_def();
List.iter2
 (fun pat exp ->
    if not (is_nonexpansive exp) then
      iter_pattern (fun pat -> generalize_expansive env pat.pat_type) pat)
 pat_list exp_list;
List.iter
 (fun pat -> iter_pattern (fun pat -> generalize pat.pat_type) pat)
 pat_list;
(List.combine pat_list exp_list, new_env, unpacks)
```

私たちは、お馴染みのパターンを参照してください。
We see the familiar pattern:

```
begin_def(); ... newvar () ... end_def(); generalize
```

-----------

2.

But there is another traversal of the type, with generalize_expansive.
しかし、とのタイプの別のトラバーサルがあるgeneralize_expansiveは。
That function is invoked only if the expression is expansive, that is, may have a visible effect -- for example, it is an application.
例えば、それはアプリケーションであり、 -その機能はつまり、目に見える効果を有していてもよい、式が広大である場合にのみ呼び出される。
The function Ctype.generalize_expansive traverses its argument type_expression; when it comes across a constructed type Tconstr(p,args) (such as the list type, etc), and is about to traverse an arg, generalize_expansive checks the declaration of the type p for the variance of that argument.
機能Ctype.generalize_expansiveは 、引数横断type_expressionを、それが構築された型に遭遇するとき、Tconstr（P、引数）（リストタイプなどなど）、および横断しようとしている引数を、generalize_expansiveタイプの宣言小切手Pのためにその引数の分散。
If arg is covariant, generalize_expansive traverses arg and sets the levels of the components above the current_level to the generic_level.
場合はargが共変である、generalize_expansiveは横断引数を上記の成分のレベルに設定current_levelをするgeneric_level。
If arg is not covariant (e.g., the argument of ref and array type constructors), arg's components with the levels above the current are set to the current_level.
場合はargが共変ではありません（例えば、引数REFと配列型コンストラクタ）は、argに現在より上のレベルでのコンポーネントは次のように設定されてcurrent_level。
The subsequent generalize will leave those levels as they are.
その後の一般化は、そのままそれらのレベルのままになります。
This is how a so-called relaxed value restriction is implemented, which is responsible for inferring the polymorphic type for
これは、いわゆるリラックス値制限をするための多相型を推論するための責任がある、どのように実装されるかである

```
# let x = (fun y -> print_string "ok"; y) [];;
ok
val x : 'a list = []
```

Here, x is bound to an application, which is not a syntactically value and which is expansive.
ここで、xが構文的価値とその広大されないアプリケーションにバインドされています。
Its evaluation certainly has a visible effect.
その評価は確かに目に見える効果があります。
And yet the type of x is generalized because the list type is covariant in its argument.
そして、まだのタイプXがあるため、一般化されたリストの タイプは、その引数に共変である。
SML would not have.
SMLはなかったでしょう。
Without the relaxed value restriction, MetaOCaml will be hardly usable.
リラックスした値の制限なしに、MetaOCamlはほとんど使用可能になります。
For example,
例えば、

```
let x = .<1 + .~(...)>.
```

is an expression, so the classifier cannot be generalized and hence x cannot be run.
式であるため、分類器は一般化することはできないので、X実行することはできません。

#### References
#### 参考資料

Jacques Garrigue: Relaxing the Value Restriction

FLOPS 2004, pp. 196-213

ジャック・ガリグ：値の制限を緩和する

FLOPS 2004, 196-213ページ

Last updated March 7, 2013

This site's top page is http://okmij.org/ftp/

oleg-at-pobox.com or oleg-at-okmij.org

Your comments, problem reports, questions are very welcome!
