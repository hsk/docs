## 11.4  Alternatives

The representation of function bindings in following sections uses alternatives, represented by values of type Alt:
	  type Alt = ([Pat], Expr)
An Alt specifies the left and right hand sides of a function definition.

With a more complete syntax for Expr, values of type Alt might also be used in the representation of lambda and case expressions.

For type inference, we begin by using tiPats to infer a type for each of the patterns, and to build a new list as' of assumptions for any bound variables, as described in Section 11.2.

Next, we calculate the type of the body in the scope of the bound variables, and combine this with the types of each pattern to obtain a single (function) type for the whole Alt:

	  tiAlt                :: Infer Alt Type
	  tiAlt ce as (pats, e) = do (ps, as', ts) <- tiPats pats
	                             (qs,t)  <- tiExpr ce (as'++as) e
	                             return (ps++qs, foldr fn t ts)
In practice, we will often run the typechecker over a list of alternatives, alts, and check that the returned type in each case agrees with some known type t.

This process can be packaged up in the following definition:

	  tiAlts             :: ClassEnv -> [Assump] -> [Alt] -> Type -> TI [Pred]
	  tiAlts ce as alts t = do psts <- mapM (tiAlt ce as) alts
	                           mapM (unify t) (map snd psts)
	                           return (concat (map fst psts))
Although we do not need it here, the signature for tiAlts would allow an implementation to push the type argument inside the checking of each Alt, interleaving unification with type inference instead of leaving it to the end.

This is essential in extensions like the support for rank-2 polymorphism in Hugs where explicit type information plays a key role.

Even in an unextended Haskell implementation, this could still help to improve the quality of type error messages.

Of course, we can still use tiAlts to infer a type from scratch.

All this requires is that we generate and pass in a fresh type variable v in the parameter t to tiAlts, and then inspect the value of v under the current substitution when it returns.


