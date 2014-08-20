### 11.6.3 Combined Binding Groups 複合バインディング グループ

Haskell requires a process of dependency analysis to break down complete sets of bindings-either at the top-level of a program, or within a local definition-into the smallest possible groups of mutually recursive definitions, and ordered so that no group depends on the values defined in later groups.

Haskell バインディング-のいずれかで、プログラム、またはのローカル定義内でトップレベルの完全なセットを打破する依存関係の分析のプロセスが必要です-相互に再帰的な定義の最小の可能なグループにグループは後でグループで定義された値に依存ないように命じた。

This is necessary to obtain the most general types possible.

これは可能な最も一般的な種類を取得する必要です。

For example, consider the following fragment from a standard prelude for Haskell:

たとえば、標準の前奏曲から次のフラグメントは、Haskell のため。

	   foldr f a (x:xs) = f x (foldr f a xs)
	   foldr f a []     = a
	   and xs           = foldr (&&) True xs

If these definitions were placed in the same binding group, then we would not obtain the most general possible type for foldr; all occurrences of a variable are required to have the same type at each point within the defining binding group, which would lead to the following type for foldr:

これらの定義は、同じバインド グループ内に配置された場合、しない取得させて foldr; のための最も一般的な可能な型変数のすべての出現を foldr の次のタイプにつながる定義する結合のグループ内の各ポイントで、同じ型を持っている必要があります。
	   (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool
To avoid this problem, we need only notice that the definition of foldr does not depend in any way on &&, and hence we can place the two functions in separate binding groups, inferring first the most general type for foldr, and then the correct type for and.

お知らせ foldr の定義を何らかの方法で依存しない必要がありますこの問題を回避する &&、それゆえ私たち置くことができる 2 つの機能グループでは個別のバインド、最初 foldr の最も一般的な種類とその後の正しい型を推論し、。
In the presence of explicitly typed bindings, we can refine the dependency analysis process a little further.

明示的に型指定されたバインドは、存在を改良すること依存関係分析のプロセスは少しさらに。

For example, consider the following pair of bindings:

たとえば、以下のバインディングのペアを考慮してください。

	   f   :: Eq a => a -> Bool
	   f x  = (x==x) || g True
	   g y  = (y<=y) || f True

Although these bindings are mutually recursive, we do not need to infer types for f and g at the same time.

これらのバインディングは相互に再帰的ですが、我々 は同時に f と g の型を推測する必要はありません。

Instead, we can use the declared type of f to infer a type:

代わりに、型を推論するのに f の宣言された型を使用できます。

	   g   :: Ord a => a -> Bool

and then use this to check the body of f, ensuring that its declared type is correct.

これを使用して宣言された型が正しいことを確保する f のボディ チェックを。

#### type BindGroup

Motivated by these observations, we will represent Haskell binding groups using the following datatype:

これらの観測によって動機付けられて、私たちは Haskell バインディング グループの次のデータ型を使用してを表します。

	  type BindGroup  = ([Expl], [[Impl]])

The first component in each such pair lists any explicitly typed bindings in the group.

このような各ペアの最初のコンポーネントは、グループで明示的に型指定されたバインドを示します。

The second component provides an opportunity to break down the list of any implicitly typed bindings into several smaller lists, arranged in dependency order.

2 番目のコンポーネントは、暗黙的に任意のリストを打破する機会を依存関係の順序で整理されるいくつかの小さいリストにバインディングを入力を提供します。

In other words, if a binding group is represented by a pair (es,[is_1,...,is_n]), then the implicitly typed bindings in each is_i should depend only on the bindings in es, is_1, ..., is_i, and not on any bindings in is_j when j>i. (Bindings in es could depend on any of the bindings in the group, but will presumably depend on at least those in is_n, or else the group would not be minimal.

つまり、バインド グループのペアで表される場合 (es,[is_1,...,is_n])、それから各 is_i の暗黙的に型指定されたバインディングは、依存すべきで is_i、is_1、...、es のバインディングにのみではなく is_j でバインドとき j > i. (es でバインド可能性がありますに依存して、グループ内のバインディングのいずれか、しかしおそらくに依存する、少なくともこれらの is_n、または他のグループない最小限のでしょう。

Note also that if es is empty, then n must be 1.) In choosing this representation, we have assumed that dependency analysis has been carried out prior to type checking, and that the bindings in each group have been organized into values of type BindGroup as appropriate.

注: また es が空の場合、し n ある必要があります 1。)この形式を選択して、タイプをチェックインする前に依存関係の分析は行われてし、各グループでバインド編成されている適切な BindGroup 型の値を想定しています。

In particular, by separating out implicitly typed bindings as much as possible, we can potentially increase the degree of polymorphism in inferred types.

具体的には、限り、暗黙的に型指定されたバインドを分離すると、我々 は潜在的推論型多型性の度合いを増やすことができます。

For a correct implementation of the semantics specified in the Haskell report, a simpler but less flexible approach is required: all implicitly typed bindings must be placed in a single list, even if a more refined decomposition would be possible.

Haskell report で指定されているセマンティクスの正しい実装は、シンプルですより柔軟なアプローチが必要です: すべての暗黙的に型指定されたバインド置かれなければならない 1 つのリストより洗練された分解が可能になる場合でも。

In addition, if the group is restricted, then we must also ensure that none of the explicitly typed bindings in the same BindGroup have any predicates in their type, even though this is not strictly necessary.

さらに、グループが制限されますが、私たちする必要がありますもことを確認します同じ BindGroup で明示的に型指定されたバインドのいずれの場合がある述語の種類にもかかわらず、これは厳密には必要ではありません。

With hindsight, these are restrictions that we might prefer to avoid in any future revision of Haskell.

後知恵で、これら我々 Haskell の任意の将来の改正を避けるために好むかもしれない制限であります。

A more serious concern is that the Haskell report does not indicate clearly whether the previous example defining f and g should be valid.

深刻な懸念は、f と g の定義例を有効にするかどうか Haskell レポートは明確に示すないです。

At the time of writing, some implementations accept it, while others do not.

執筆の時に、いくつかの実装一方ではありません、それを受け入れます。

This is exactly the kind of problem that can occur when there is no precise, formal specification! Curiously, however, the report does indicate that a modification of the example to include an explicit type for g would be illegal.

これは正確に正確な正式な仕様がない場合に発生することができる問題の種類です ！不思議なことに、ただし、報告書が示すの g の明示的な型を含める例を変更では法的であること。

This is a consequence of a throw-away comment specifying that all explicit type signatures in a binding group must have the same context up to renaming of variables [ Peyton Jones & Hughes, 1999,Section 4.5.2].

これはバインディング グループのすべての明示的な型署名の変数 [ペイトン ・ ジョーンズ & ヒューズ、1999 年、セクション 4.5.2] の名前を変更するまで同じコンテキストである必要がありますを指定する使い捨てのコメントの結果です。

This is a syntactic restriction that can easily be checked prior to type checking.

これは型チェック前に簡単にチェックすることができます構文の制限です。

Our comments here, however, suggest that it is unnecessarily restrictive.

私たちのコメントをここでは、しかし、それが不必要に制限を提案します。

In addition to the function bindings that we have seen already, Haskell allows variables to be defined using pattern bindings of the form pat = expr.

関数のバインディングは、我々 はすでに見ている、に加えて Haskell フォーム パットのパターンのバインドを使用して定義する変数をことができます = expr。

We do not need to deal directly with such bindings because they are easily translated into the simpler framework used in this paper.

このペーパーで使用される単純なフレームワークに簡単に翻訳がのでこのようなバインディングを直接扱う必要はありません。

For example, a binding:

たとえば、バインディング:

	   (x,y) = expr

can be rewritten as:

として書き換えることができます。

	   nv = expr
	   x  = fst nv
	   y  = snd nv

where nv is a new variable.

nvは、新しい変数です。

The precise definition of the monomorphism restriction in Haskell makes specific reference to pattern bindings, treating any binding group that includes one as restricted.

Haskell で相性制限の正確な定義は任意結合を含むグループを 1 つの制限としての治療パターン バインディングに固有の参照になります。

So it may seem that the definition of restricted binding groups in this paper is not quite accurate.

だからこの論文では, 制限されたバインディング グループの定義が非常に正確であると思えるかもしれません。

However, if we use translations as suggested here, then it turns out to be equivalent: even if the programmer supplies explicit type signatures for x and y in the original program, the translation will still contain an implicitly typed binding for the new variable nv.

ただし、ここで提案された翻訳を使用、し、それが判明相当する: プログラマ提供 x に対して明示的な型署名元プログラム、翻訳で y が含まれていても、暗黙的に型指定されたバインド新しい変数nvの場合でも。

#### tiBindGroup

Now, at last, we are ready to present the algorithm for type inference of a complete binding group, as implemented by the following function:

今、最後に、我々 は完全なバインド グループの型の推論のアルゴリズムを提示する準備ができて次の関数によって実装されます。

	  tiBindGroup :: Infer BindGroup [Assump]
	  tiBindGroup ce as (es,iss) =
	    do let as' = [ v:>:sc | (v,sc,alts) <- es ]
	       (ps, as'') <- tiSeq tiImpls ce (as'++as) iss
	       qss        <- mapM (tiExpl ce (as''++as'++as)) es
	       return (ps++concat qss, as''++as')

The structure of this definition is quite straightforward.

この定義の構造はかなり簡単です。

First we form a list of assumptions as' for each of the explicitly typed bindings in the group.

前提条件のリストを形成する最初のグループに明示的に型指定されたバインドの各として。

Next, we use this to check each group of implicitly typed bindings, extending the assumption set further at each stage.

次に、我々 を暗黙的に型指定されたバインディングは、拡張セットを各段階でさらに仮定の各グループを確認するこれを使用してください。

#### tiSeq

Finally, we return to the explicitly typed bindings to verify that each of the declared types is acceptable.

最後に、我々 はそれぞれの宣言の種類が許容されることを確認する明示的に型指定されたバインドに戻ります。

In dealing with the list of implicitly typed binding groups, we use the following utility function, which typechecks a list of binding groups and accumulates assumptions as it runs through the list:

暗黙的に型指定されたバインド グループのリストを扱うのバインディングの一覧をグループし、蓄積それとして仮定する typechecks リストを実行、以下のユーティリティ関数を使用します。

	  tiSeq                  :: Infer bg [Assump] -> Infer [bg] [Assump]
	  tiSeq ti ce as []       = return ([],[])
	  tiSeq ti ce as (bs:bss) = do (ps,as')  <- ti ce as bs
	                               (qs,as'') <- tiSeq ti ce (as'++as) bss
	                               return (ps++qs, as''++as')

