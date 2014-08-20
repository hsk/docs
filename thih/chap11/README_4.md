## 11.4 Alternatives 選択肢


#### type Alt

The representation of function bindings in following sections uses alternatives, represented by values of type Alt:

以下のセクションで関数の束縛の表現型 alt キーの値によって表される選択肢を使用してください。

	  type Alt = ([Pat], Expr)

An Alt specifies the left and right hand sides of a function definition.

Alt は、関数定義の左と右の手の側面を指定します。

With a more complete syntax for Expr, values of type Alt might also be used in the representation of lambda and case expressions.

Expr のより完全な構文と型 Alt 値ラムダとケース式の表現でされる可能性があります。

#### tiAlt

For type inference, we begin by using tiPats to infer a type for each of the patterns, and to build a new list as' of assumptions for any bound variables, as described in Section 11.2.

型の推定、我々 は、パターンの各型を推論し、新しいリストを作成する tiPats を使用して開始の仮定で説明されているセクション 11.2 として、バインド変数のとして。

Next, we calculate the type of the body in the scope of the bound variables, and combine this with the types of each pattern to obtain a single (function) type for the whole Alt:

次に、バインド先の変数のスコープに体の種類を計算し、各全体の Alt の単一 (機能) 型を取得するパターンの種類でこれを組み合わせる：

	  tiAlt                :: Infer Alt Type
	  tiAlt ce as (pats, e) = do (ps, as', ts) <- tiPats pats
	                             (qs,t)  <- tiExpr ce (as'++as) e
	                             return (ps++qs, foldr fn t ts)

#### tiAlts

In practice, we will often run the typechecker over a list of alternatives, alts, and check that the returned type in each case agrees with some known type t.

実習では、我々 はしばしば実行、typechecker の代わり、alt、およびいくつかの既知の型 t と一致する各場合で返される型チェックのリスト。

This process can be packaged up in the following definition:

このプロセスは、次の定義でパッケージ化することができます。

	  tiAlts             :: ClassEnv -> [Assump] -> [Alt] -> Type -> TI [Pred]
	  tiAlts ce as alts t = do psts <- mapM (tiAlt ce as) alts
	                           mapM (unify t) (map snd psts)
	                           return (concat (map fst psts))

Although we do not need it here, the signature for tiAlts would allow an implementation to push the type argument inside the checking of each Alt, interleaving unification with type inference instead of leaving it to the end.

我々 は必要ありませんがそれここで、tiAlts の署名ように各 Alt のチェック内の型引数をプッシュする実装最後に残してではなく、型の推論との統一をインタリーブします。

This is essential in extensions like the support for rank-2 polymorphism in Hugs where explicit type information plays a key role.

これは明示的な型情報は重要な役割を果たしている抱擁でランク 2 ポリモーフィズムの拡張機能のサポートのように不可欠です。

Even in an unextended Haskell implementation, this could still help to improve the quality of type error messages.

でも、拡張されていない Haskell の実装この助けることができるまだ型のエラー メッセージの質を向上させる。

Of course, we can still use tiAlts to infer a type from scratch.

もちろん、まだ使える tiAlts スクラッチから型を推論します。

All this requires is that we generate and pass in a fresh type variable v in the parameter t to tiAlts, and then inspect the value of v under the current substitution when it returns.

これが必要ですすべては我々 が生成されるとパラメーター t で新鮮なタイプの変数 v tiAlts に渡すし、それが返されるときにし、現在の置換の下での v の値を調べる。

