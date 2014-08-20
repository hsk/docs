### 11.6.2 Implicitly Typed Bindings 暗黙的型指定バインディング

Two complications occur when we deal with implicitly typed bindings.

2 つの合併症は暗黙的に型指定されたバインドを扱うときに発生します。

The first is that we must deal with groups of mutually recursive bindings as a single unit rather than inferring types for each binding one at a time.

最初は推論型ではなく、単一のユニットとして相互再帰バインドごとに 1 つずつバインドのグループとは、契約する必要があります。

The second is Haskell's monomorphism restriction, which restricts the use of overloading in certain cases.

2 番目は、Haskell の相性の制限は、特定のケースでのオーバー ロードの使用を制限します。

#### type Impl

A single implicitly typed binding is described by a pair containing the name of the variable and a list of alternatives:

1 つの暗黙的に型指定されたバインディングは変数の名前と選択肢のリストを含むペアで記述されます。

	  type Impl   = (Id, [Alt])

#### restricted

The monomorphism restriction is invoked when one or more of the entries in a list of implicitly typed bindings is simple, meaning that it has an alternative with no left-hand side patterns.

暗黙的に型指定されたバインドの一覧のエントリの 1 つ以上は単純なそれがないの左側にあるパターンを持つ代替の意味と相性制限が呼び出されます。

The following function provides a way to test for this:

次の関数はこれをテストする方法を提供します。

	  restricted   :: [Impl] -> Bool
	  restricted bs = any simple bs
	   where simple (i,alts) = any (null . fst) alts

#### tiImpls

Type inference for groups of mutually recursive, implicitly typed bindings is described by the following function:

相互再帰的で、暗黙的に型指定のグループのための推論の入力バインディングは、次の関数で記述されます。

	  tiImpls         :: Infer [Impl] [Assump]
	  tiImpls ce as bs = do ts <- mapM (\_ -> newTVar Star) bs
	                        let is    = map fst bs
	                            scs   = map toScheme ts
	                            as'   = zipWith (:>:) is scs ++ as
	                            altss = map snd bs
	                        pss <- sequence (zipWith (tiAlts ce as') altss ts)
	                        s   <- getSubst
	                        let ps'     = apply s (concat pss)
	                            ts'     = apply s ts
	                            fs      = tv (apply s as)
	                            vss     = map tv ts'
	                            gs      = foldr1 union vss \\ fs
	                        (ds,rs) <- split ce fs (foldr1 intersect vss) ps'
	                        if restricted bs then
	                            let gs'  = gs \\ tv rs
	                                scs' = map (quantify gs' . ([]:=>)) ts'
	                            in return (ds++rs, zipWith (:>:) is scs')
	                          else
	                            let scs' = map (quantify gs . (rs:=>)) ts'
	                            in return (ds, zipWith (:>:) is scs')

In the first part of this process, we extend as with assumptions binding each identifier defined in bs to a new type variable, and use these to type check each alternative in each binding.

このプロセスの最初の部分で拡張型の新しい変数に bs で定義されている各識別子のバインドの仮定と同様しこれらチェックを入力する代わりに各バインディングで使用する各.

This is necessary to ensure that each variable is used with the same type at every occurrence within the defining list of bindings. (Lifting this restriction makes type inference undecidable [ Henglein, 1993, Kfoury et al. , 1993].) Next we use split to break the inferred predicates in ps' into a list of deferred predicates ds and retained predicates rs.

これは、バインディングの定義リスト内で出現するたびに同じ型を持つ各変数が使用されるようにする必要です。(型の推論タイプチェッカー [Henglein、Kfoury et al., 1993年] がこの制限を持ち上げる)次に繰延述語 ds と保持された述語 rs のリストに ps ので推論される述語を分割を使用します。

The list gs collects all the generic variables that appear in one or more of the inferred types ts', but not in the list fs of fixed variables.

リスト gs は、推論された型 ts' の 1 つ以上ではない固定変数のリスト fs を表示すべての一般的な変数を収集します。

Note that a different list is passed to split, including only variables that appear in all of the inferred types.

別のリストが渡されることを分割、推論された型のすべての変数のみが表示されますを含む注意してください。

This is important because all of those types will eventually be qualified by the same set of predicates, and we do not want any of the resulting type schemes to be ambiguous.

すべてのこれらの種類は最終的に述語の同じセットで修飾されているし、我々 があいまい結果型スキームのいずれかをしたくないので、これは重要です。

The final step begins with a test to see if the monomorphism restriction should be applied, and then continues to calculate an assumption containing the principal types for each of the defined values.

最後のステップは相性制限を適用する定義済みの値の各プリンシパルの種類を格納している前提データの計算をし続けます場合を参照してくださいにテストから始まります。

For an unrestricted binding, this is simply a matter of qualifying over the retained predicates in rs and quantifying over the generic variables in gs.

無制限バインディングでは、これは単に rs の剰余金の述語を予選と gs でジェネリック変数上の定量化の問題です。

If the binding group is restricted, then we must defer the predicates in rs as well as those in ds, and hence we can only quantify over variables in gs that do not appear in rs.

バインド グループが制限されている場合私たちを rs の述語だけでなく、ds では、それらを延期する必要があり、rs で現われない gs の変数のみを数値化したがって。

