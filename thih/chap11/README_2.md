## 11.2 Patterns パターン

Patterns are used to inspect and deconstruct data values in lambda abstractions, function and pattern bindings, list comprehensions, do notation, and case expressions.

パターン検査し、ラムダの抽象化、関数とパターンのバインド、リストの内包表記、記法、および case 式内のデータ値を分解に使用されます。


#### data Pat

We will represent patterns using values of the Pat datatype:

パターン Pat データ型の値を使用して表現してみます。

	  data Pat        = PVar Id
	                  | PWildcard
	                  | PAs  Id Pat
	                  | PLit Literal
	                  | PNpk Id Integer
	                  | PCon Assump [Pat]

A PVari pattern matches any value and binds the result to the variablei.

PVar i パターンは任意の値と一致し、結果を変数 i にバインドします。

A PWildcard pattern, corresponding to an underscore _ in Haskell syntax, matches any value, but does not bind any variables.

PWildcard パターンは Haskell の構文の下線 _ に対応する任意の値に一致するが、任意の変数をバインドできません。

A pattern of the form (PAs i pat), known as an ``as-pattern'' and written using the syntax i@pat in Haskell, binds the variable i to any value that matches the pattern pat, while also binding any variables that appear in pat.

(PAs i pat) 形式のパターン、 'as-pattern' として、Haskell の構文 i@pat を使用して書かれたとして知られているいずれかに一致するパターン pat 、pat に表れる変数のバインディングも中値変数をバインドします。

(PAs i pat)形式のパターン、'asパターン'として知られているハスケルにおいて適切なシンタックスi@patを使用して書かれた、patに現われるあらゆる変数を拘束している間、変数 iをパターン pat と一致する任意の値に結び付けます。

A PLit l pattern matches only the particular value denoted by the literal l.

PLit l パターンのみリテラル l で示される特定の値と一致します。

A pattern (PNpk i k) is an (n+k) pattern, which matches any positive integral value m that is greater than or equal to k, and binds the variable i to the difference (m-k).

パターン （PNpk i k） k 以上であり、バインドされ、変数 i を任意の正の整数値 m と一致する (n + k) パターンは (m k) の違い。

Finally, a pattern of the form PCon a pats matches only values that were built using the constructor function a with a sequence of arguments that matches pats.

最後に、軽打は PCon フォームのパターン一致コンス トラクター関数を使用してビルドされた値のみ、pats を一致する引数の順序と。

We use values a of type Assump to represent constructor functions; all that we really need for typechecking is the type, although the name is useful for debugging.

値を使用して、型のコンス トラクター関数; を表現する Assump の私たちは本当に型検査のため必要があるすべては、型名はデバッグに役立ちます。

A full implementation would store additional details, such as arity, and use this to check that constructor functions in patterns are always fully applied.

完全な実装はアリティなどの詳細を格納を使用してこのパターンのコンス トラクター関数は常に完全に適用されることを確認してください。

Most Haskell patterns have a direct representation in Pat, but extensions would be needed to account for patterns using labeled fields.

Haskell のほとんどのパターン直接的な表現で、パットがラベル フィールドを使用してパターンに対応する拡張機能が必要になります。

This is not difficult, but adds some complexity, which we prefer to avoid in this presentation.

これ、難しくはありませんが、我々 はこのプレゼンテーションでは避けることを好む、いくつかの複雑さを追加します。


#### tiPat

Type inference for patterns has two goals: To calculate a type for each bound variable, and to determine what type of values the whole pattern might match.

型の推論のパターンは 2 つの目標： 各バインド変数の型を計算して全体のパターンに一致可能性がありますどのような値の型を決定します。

This leads us to look for a function:

これは、関数を探しに私たちをリード：

	  tiPat :: Pat -> TI ([Pred], [Assump], Type)

Note that we do not need to pass in a list of assumptions here; by definition, any occurrence of a variable in a pattern would hide rather than refer to a variable of the same name in an enclosing scope.

ここ; の前提条件のリストで渡す必要がないことに注意してください。定義では、パターン変数の発生は非表示ではなく、外側のスコープで同じ名前の変数を参照してください。

#### tiPat PVar

For a variable pattern, PVar i, we just return a new assumption, binding i to a fresh type variable.

変数パターン、PVar の私は、私達はちょうど新鮮な型の変数にバインドする私は新しい前提を返します。

	  tiPat (PVar i) = do v <- newTVar Star
	                      return ([], [i :>: toScheme v], v)

Haskell does not allow multiple use of any variable in a pattern, so we can be sure that this is the first and only occurrence of i that we will encounter in the pattern.

Haskell は、パターンでは、任意の変数の複数の使用を許可しないので、我々 はこれが最初で、パターンに遭遇することが私の 1 回だけ確認することができます。

#### tiPat PWrildcard

Wildcards are typed in the same way except that we do not need to create a new assumption:

ワイルドカードは、新しい前提を作成する必要はないことを除いて同じ方法で入力します。

	  tiPat PWildcard   = do v <- newTVar Star
	                         return ([], [], v)

#### tiPat PAs

To type an as-pattern PAs i pat, we calculate a set of assumptions and a type for the pat pattern, and then add an extra assumption to bind i:

私はパットをパターンとして PAs を入力するには、前提とパットのパターンの種類のセットを計算し、i: バインドする余分な前提データを追加します

	  tiPat (PAs i pat) = do (ps, as, t) <- tiPat pat
	                         return (ps, (i:>:toScheme t):as, t)

#### tiPat PLit

For literal patterns, we use tiLit from the previous section:

リテラル パターン、前のセクションから tiLit を使用します。

	  tiPat (PLit l) = do (ps, t) <- tiLit l
	                      return (ps, [], t)

#### tiPat PNpk

The rule for (n+k) patterns does not fix a type for the bound variable, but adds a predicate to constrain the choice to instances of the Integral class:

(N + k) パターンのルール バインド変数の型を解決しないが、積分クラスのインスタンスに選択肢を制限するための述語を追加します。

	  tiPat (PNpk i k)  = do t <- newTVar Star
	                         return ([IsIn "Integral" t], [i:>:toScheme t], t)

#### tiPat PCon

The case for constructed patterns is slightly more complex:

構築されたパターンの場合は多少複雑です：

	  tiPat (PCon (i:>:sc) pats) = do (ps,as,ts) <- tiPats pats
	                                  t'         <- newTVar Star
	                                  (qs :=> t) <- freshInst sc
	                                  unify t (foldr fn t' ts)
	                                  return (ps++qs, as, t')

First we use the tiPats function, defined below, to calculate types ts for each subpattern in pats together with corresponding lists of assumptions in as and predicates in ps.

まず我々 型 ts としての前提と ps 内の述語の対応するリストと共になで各サブパターンを計算するため、以下に定義、tiPats 関数を使用します。

Next, we generate a new type variable t' that will be used to capture the (as yet unknown) type of the whole pattern.

次に、新しい型 t 変数を生成する ' パターン全体の (未知) タイプをキャプチャに使用されます。

From this information, we would expect the constructor function at the head of the pattern to have type foldr fn t' ts.

この情報から、我々 が期待コンス トラクター関数を型 foldr fn t を持っているパターンの先頭に ' ts。

We can check that this is possible by instantiating the known type sc of the constructor and unifying.

我々 は、これが可能なコンス トラクターの既知の型 sc をインスタンス化し、統一により確認できます。

The tiPats function is a variation of tiPat that takes a list of patterns as input, and returns a list of types (together with a list of predicates and a list of assumptions) as its result.

#### tiPats

TiPats 関数は、パターンの一覧を入力として受け取り、その結果として (述語のリスト) および前提条件のリストと共に型の一覧を返しますヒントのバリエーション。

	  tiPats     :: [Pat] -> TI ([Pred], [Assump], [Type])
	  tiPats pats = do psasts <- mapM tiPat pats
	                   let ps = concat [ ps' | (ps',_,_) <- psasts ]
	                       as = concat [ as' | (_,as',_) <- psasts ]
	                       ts = [ t | (_,_,t) <- psasts ]
	                   return (ps, as, ts)

We have already seen how tiPats was used in the treatment of PCon patterns above.

我々 はすでに PCon パターン上記の治療に tiPats がどのように使用されたかを見てきました。

It is also useful in other situations where lists of patterns are used, such as on the left hand side of an equation in a function definition.

また、パターンのリストが使用される場所など、他の状況で便利です、関数定義内の数式の左側に。
