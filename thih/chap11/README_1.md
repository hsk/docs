## 11.1 Literals リテラル

#### data Literal

Like other languages, Haskell provides special syntax for constant values of certain primitive datatypes, including numerics, characters, and strings.

他の言語のような Haskell 数値、文字、および文字列を含む特定のプリミティブ データ型の定数値に対して特殊な構文を提供します。

We will represent these literal expressions as values of the Literal datatype:

我々 はリテラルのデータ型の値としてこれらのリテラル式を表します。

	  data Literal = LitInt  Integer
	               | LitChar Char
	               | LitRat  Rational
	               | LitStr  String

#### tiLit

Type inference for literals is straightforward.

リテラルの型の推論は簡単です。

For characters, we just return tChar.

文字は tChar を返すだけです。

For integers, we return a new type variable v together with a predicate to indicate that v must be an instance of the Num class.

整数は、新しい変数 v 型 Num クラスのインスタンスでなければなりませんが v であることを示すために述語と共に返します。

The cases for String and floating point literals follow similar patterns:

文字列と浮動小数点リテラルの場合は同様のパターンに従います:

	  tiLit            :: Literal -> TI ([Pred],Type)
	  tiLit (LitChar _) = return ([], tChar)
	  tiLit (LitInt _)  = do v <- newTVar Star
	                         return ([IsIn "Num" v], v)
	  tiLit (LitStr _)  = return ([], tString)
	  tiLit (LitRat _)  = do v <- newTVar Star
	                         return ([IsIn "Fractional" v], v)

