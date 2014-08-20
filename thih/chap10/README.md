# 10 A Type Inference Monad 型推論モナド

It is now quite standard to use monads as a way to hide certain aspects of `plumbing' and to draw attention instead to more important aspects of a program's design [ Wadler, 1992].

それは今かなり標準モナド '配管' の特定の側面を隠す代わりに、プログラムのデザイン [Wadler 1992] のより重要な側面に注意を喚起する方法として使用します。

The purpose of this section is to define the monad that will be used in the description of the main type inference algorithm in Section 11.

このセクションの目的はセクション 11 の主要な型推論アルゴリズムの説明に使用されるモナドを定義です。

Our choice of monad is motivated by the needs of maintaining a `current substitution' and of generating fresh type variables during typechecking.

モナドの私達の選択は '現在置換' を維持して型検査中に新鮮な型の変数を生成するのニーズによって動機づけられています。

In a more realistic implementation, we might also want to add error reporting facilities, but in this paper the crude but simple fail function from the Haskell prelude is all that we require.

実装より現実的なまたエラー報告施設、追加したい場合がありますがここでは、Haskell から関数を単純な失敗が原油プレリュードすべてのことが必要です。

#### newtype TI TIモナド、runTI

It follows that we need a simple state monad with only a substitution and an integer (from which we can generate new type variables) as its state:

状態としてのみ置換と （そこから生成できる新しい型の変数) の整数の単純な state モナド必要があるそれ次の：

	  newtype TI a = TI (Subst -> Int -> (Subst, Int, a))
 
	  instance Monad TI where
	    return x   = TI (\s n -> (s,n,x))
	    TI f >>= g = TI (\s n -> case f s n of
	                              (s',m,x) -> let TI gx = g x
	                                          in  gx s' m)
 
	  runTI       :: TI a -> a
	  runTI (TI f) = x where (s,n,x) = f nullSubst 0

#### getSubst unify

The getSubst operation returns the current substitution, while unify extends it with a most general unifier of its arguments:

GetSubst 操作統一その引数の最も一般的な単一化とそれを拡張しながら、現在の置換が返されます。

	  getSubst   :: TI Subst
	  getSubst    = TI (\s n -> (s,n,s))
 
	  unify      :: Type -> Type -> TI ()
	  unify t1 t2 = do s <- getSubst
	                   u <- mgu (apply s t1) (apply s t2)
	                   extSubst u

#### extSubst

For clarity, we define the operation that extends the substitution as a separate function, even though it is used only here in the definition of unify:

わかりやすいようにはにもかかわらず、それはここだけで統一の定義で使用される個別の関数としての置換を拡張する操作を定義します。

	  extSubst   :: Subst -> TI ()
	  extSubst s' = TI (\s n -> (s'@@s, n, ()))

Overall, the decision to hide the current substitution in the TI monad makes the presentation of type inference much clearer.

全体的にみて、TI モナドで現在の置換を非表示にする決定は大いにより明確な型推論のプレゼンテーションになります。

In particular, it avoids heavy use of apply every time an extension is (or might have been) computed.

特に、頻繁に使用を回避するたびに拡張は (または、されている可能性があります) を適用する計算されます。

#### newTVar

There is only one primitive that deals with the integer portion of the state, using it in combination with enumId to generate a new type variable of a specified kind:

指定した種類の新しい型の変数を生成する enumId との組み合わせでを使用して、状態の整数部分を扱うのみ 1 つのプリミティブがあります。

	  newTVar    :: Kind -> TI Type
	  newTVar k   = TI (\s n -> let v = Tyvar (enumId n) k
	                            in  (s, n+1, TVar v))

#### freshInst

One place where newTVar is useful is in instantiating a type scheme with new type variables of appropriate kinds:

NewTVar は便利な 1 つの場所の適切な種類の新しい型の変数を持つ型方式をインスタンス化するのには。

	  freshInst               :: Scheme -> TI (Qual Type)
	  freshInst (Forall ks qt) = do ts <- mapM newTVar ks
	                                return (inst ts qt)

The structure of this definition guarantees that ts has exactly the right number of type variables, and each with the right kind, to match ks.

Ts が正確、適切な数型の変数とそれぞれ ks を一致するように、右の種類のこの定義の構造を保証します。

Hence, if the type scheme is well-formed, then the qualified type returned by freshInst will not contain any unbound generics of the form TGen n.

したがって、型方式が整形場合、freshInst によって返される修飾型は、バインドされていないジェネリック フォーム TGen n を含まれません。

#### Instantiateクラスと Type, [a], Qual, Predの実装

The definition relies on an auxiliary function inst, which is a variation of apply that works on generic variables.

定義は、補助関数 inst 依存の変化はジェネリック変数にその作品を適用。

In other words, inst ts t replaces each occurrence of a generic variable TGen n in t with ts!!n.

つまり、inst ts t に置き換える各ジェネリック変数 TGen n t で ts!n。

It is convenient to build up the definition of inst using overloading:

オーバー ロードを使用して inst の定義を構築すると便利です。

	  class Instantiate t where
	    inst  :: [Type] -> t -> t
	  instance Instantiate Type where
	    inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
	    inst ts (TGen n)  = ts !! n
	    inst ts t         = t
	  instance Instantiate a => Instantiate [a] where
	    inst ts = map (inst ts)
	  instance Instantiate t => Instantiate (Qual t) where
	    inst ts (ps :=> t) = inst ts ps :=> inst ts t
	  instance Instantiate Pred where
	    inst ts (IsIn c t) = IsIn c (inst ts t)

