### 11.6.1 Explicitly Typed Bindings 型バインディングを明示的に指定

The simplest case is for explicitly typed bindings, each of which is described by the name of the function that is being defined, the declared type scheme, and the list of alternatives in its definition:

明示的に型指定されたバインディングは、それぞれの定義されている関数の名前で記述は、最も単純なケースは宣言型方式とその定義の選択肢のリスト。

	  type Expl = (Id, Scheme, [Alt])

Haskell requires that each Alt in the definition of a given identifier has the same number of left-hand side arguments, but we do not need to enforce that here.

Haskell では、各特定の識別子の定義で Alt が左側の引数の数が同じがここでそれを強制する必要はありませんが必要です。

Type inference for an explicitly typed binding is fairly easy; we need only check that the declared type is valid, and do not need to infer a type from first principles.

明示的に型指定されたバインディングの型の推論は簡単です。私たち、宣言された型は、有効をチェックするだけと第一原理計算から型を推論する必要はありません。

To support the use of polymorphic recursion [ Henglein, 1993, Kfoury et al. , 1993], we will assume that the declared typing for i is already included in the assumptions when we call the following function:

ポリモーフィックな再帰の使用をサポートするために [Henglein、1993 年、Kfoury et al.1993]、我々 と仮定されます既に前提条件に含まれて、次の関数を呼び出すときに、宣言を入力します。

	  tiExpl :: ClassEnv -> [Assump] -> Expl -> TI [Pred]
	  tiExpl ce as (i, sc, alts)
	          = do (qs :=> t) <- freshInst sc
	               ps         <- tiAlts ce as alts t
	               s          <- getSubst
	               let qs'     = apply s qs
	                   t'      = apply s t
	                   fs      = tv (apply s as)
	                   gs      = tv t' \\ fs
	                   sc'     = quantify gs (qs':=>t')
	                   ps'     = filter (not . entail ce qs') (apply s ps)
	               (ds,rs)    <- split ce fs gs ps'
	               if sc /= sc' then
	                   fail "signature too general"
	                 else if not (null rs) then
	                   fail "context too weak"
	                 else
	                   return ds

This code begins by instantiating the declared type scheme sc and checking each alternative against the resulting type t.

このコードは宣言型方式 sc をインスタンス化し、結果の型 t に対してそれぞれの選択肢のチェックを開始します。

When all of the alternatives have been processed, the inferred type for i is qs' :=> t'.

すべての選択肢が処理されたとき私は推論された型が qs の:=> t'。

If the type declaration is accurate, then this should be the same, up to renaming of generic variables, as the original type qs:=>t.

型宣言が正確ならこの元型 qs としての一般的な変数の名前を変更するまで同じべきである: t =>。

If the type signature is too general, then the calculation of sc' will result in a type scheme that is more specific than sc and an error will be reported.

型シグネチャが一般的すぎる場合はサウスカロライナの算定し ' sc よりも限定型方式になります、エラーが報告されます。

In the meantime, we must discharge any predicates that were generated while checking the list of alternatives.

一方で、我々 の選択肢の一覧を確認しながら生成された述語を放電する必要があります。

Predicates that are entailed by the context qs' can be eliminated without further ado.

さらに騒ぎがなければ、コンテキスト qs によって伴なわれる述語を排除できます。

Any remaining predicates are collected in ps' and passed as arguments to split along with the appropriate sets of fixed and generic variables.

残り述語 ps' で収集され、固定およびジェネリック変数の適切なセットと一緒に分割する引数として渡されます。

If there are any retained predicates after context reduction, then an error is reported, indicating that the declared context is too weak.

なら任意の剰余金の述語コンテキスト削減後宣言されたコンテキストが弱すぎることを示すエラーが報告されます。
