## 11.3  Expressions

Next we describe type inference for expressions, represented by the Expr datatype:
	  data Expr = Var   Id
	            | Lit   Literal
	            | Const Assump
	            | Ap    Expr Expr
	            | Let   BindGroup Expr
The Var and Lit constructors are used to represent variables and literals, respectively.

The Const constructor is used to deal with named constants, such as the constructor or selector functions associated with a particular datatype or the member functions that are associated with a particular class.

We use values of type Assump to supply a name and type scheme, which is all the information that we need for the purposes of type inference.

Function application is represented using the Ap constructor, while Let is used to represent let expressions. (Note that the definition of the BindGroup type, used here to represent binding groups, will be delayed to Section 11.6.3.) Of course, Haskell has a much richer syntax of expressions-which includes l-abstractions, case expressions, conditionals, list comprehensions, and do-notation-but they all have simple translations into Expr values.

For example, a l-expression like \x->e can be rewritten using a local definition as let f x = e in f, where f is a new variable.

Type inference for expressions is quite straightforward:

	  tiExpr                       :: Infer Expr Type
	  tiExpr ce as (Var i)          = do sc         <- find i as
	                                     (ps :=> t) <- freshInst sc
	                                     return (ps, t)
	  tiExpr ce as (Const (i:>:sc)) = do (ps :=> t) <- freshInst sc
	                                     return (ps, t)
	  tiExpr ce as (Lit l)          = do (ps,t) <- tiLit l
	                                     return (ps, t)
	  tiExpr ce as (Ap e f)         = do (ps,te) <- tiExpr ce as e
	                                     (qs,tf) <- tiExpr ce as f
	                                     t       <- newTVar Star
	                                     unify (tf `fn` t) te
	                                     return (ps++qs, t)
	  tiExpr ce as (Let bg e)       = do (ps, as') <- tiBindGroup ce as bg
	                                     (qs, t)   <- tiExpr ce (as' ++ as) e
	                                     return (ps ++ qs, t)
The final case here, for Let expressions, uses the function tiBindGroup presented in Section 11.6.3, to generate a list of assumptions as' for the variables defined in bg.

All of these variables are in scope when we calculate a type t for the body e, which also serves as the type of the whole expression.


