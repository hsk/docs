### 11.6.2  Implicitly Typed Bindings

Two complications occur when we deal with implicitly typed bindings.

The first is that we must deal with groups of mutually recursive bindings as a single unit rather than inferring types for each binding one at a time.

The second is Haskell's monomorphism restriction, which restricts the use of overloading in certain cases.
A single implicitly typed binding is described by a pair containing the name of the variable and a list of alternatives:

	  type Impl   = (Id, [Alt])
The monomorphism restriction is invoked when one or more of the entries in a list of implicitly typed bindings is simple, meaning that it has an alternative with no left-hand side patterns.

The following function provides a way to test for this:

	  restricted   :: [Impl] -> Bool
	  restricted bs = any simple bs
	   where simple (i,alts) = any (null . fst) alts
Type inference for groups of mutually recursive, implicitly typed bindings is described by the following function:

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

This is necessary to ensure that each variable is used with the same type at every occurrence within the defining list of bindings. (Lifting this restriction makes type inference undecidable [ Henglein, 1993, Kfoury et al. , 1993].) Next we use split to break the inferred predicates in ps' into a list of deferred predicates ds and retained predicates rs.

The list gs collects all the generic variables that appear in one or more of the inferred types ts', but not in the list fs of fixed variables.

Note that a different list is passed to split, including only variables that appear in all of the inferred types.

This is important because all of those types will eventually be qualified by the same set of predicates, and we do not want any of the resulting type schemes to be ambiguous.

The final step begins with a test to see if the monomorphism restriction should be applied, and then continues to calculate an assumption containing the principal types for each of the defined values.

For an unrestricted binding, this is simply a matter of qualifying over the retained predicates in rs and quantifying over the generic variables in gs.

If the binding group is restricted, then we must defer the predicates in rs as well as those in ds, and hence we can only quantify over variables in gs that do not appear in rs.

