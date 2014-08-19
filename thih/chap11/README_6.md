## 11.6 Binding Groups バインド グループ

Our last remaining technical challenge is to describe typechecking for binding groups.

私たち最後の残りの技術的課題は型検査グループのバインドを記述します。

This area is neglected in most theoretical treatments of type inference, often being regarded as a simple exercise in extending basic ideas.

このエリアは型の推論は、多くの中の最も理論的な処置で無視されます基本的なアイデアを拡張するの簡単な練習とみなされる。

In Haskell, at least, nothing could be further from the truth! With interactions between overloading, polymorphic recursion, and the mixing of both explicitly and implicitly typed bindings, this is the most complex, and most subtle component of type inference.

Haskell は、少なくとも、いいものは真実から遠い ！オーバー ロード、および相互作用"ポリモフィック型"再帰両方明示的および暗黙的に型指定されたバインドの混合、これは型の推論の最も複雑で最も繊細なコンポーネントです。

We will start by describing the treatment of explicitly typed bindings and implicitly typed bindings as separate cases, and then show how these can be combined.

別のケースとしてバインディングを暗黙的に型指定し、どのようにこれらを組み合わせて表示し私たち明示的に型指定されたバインドの治療を記述することで開始されます。


sakurai:thih sakurai$ 
