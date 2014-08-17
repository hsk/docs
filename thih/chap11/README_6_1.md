### 11.6.1  Explicitly Typed Bindings

The simplest case is for explicitly typed bindings, each of which is described by the name of the function that is being defined, the declared type scheme, and the list of alternatives in its definition:
	  type Expl = (Id, Scheme, [Alt])
Haskell requires that each Alt in the definition of a given identifier has the same number of left-hand side arguments, but we do not need to enforce that here.

Type inference for an explicitly typed binding is fairly easy; we need only check that the declared type is valid, and do not need to infer a type from first principles.

To support the use of polymorphic recursion [ Henglein, 1993, Kfoury et al. , 1993], we will assume that the declared typing for i is already included in the assumptions when we call the following function:

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

When all of the alternatives have been processed, the inferred type for i is qs' :=> t'.

If the type declaration is accurate, then this should be the same, up to renaming of generic variables, as the original type qs:=>t.

If the type signature is too general, then the calculation of sc' will result in a type scheme that is more specific than sc and an error will be reported.

In the meantime, we must discharge any predicates that were generated while checking the list of alternatives.

Predicates that are entailed by the context qs' can be eliminated without further ado.

Any remaining predicates are collected in ps' and passed as arguments to split along with the appropriate sets of fixed and generic variables.

If there are any retained predicates after context reduction, then an error is reported, indicating that the declared context is too weak.

