## 11.5  From Types to Type Schemes

We have seen how lists of predicates are accumulated during type inference; now we will describe how those predicates are used to construct inferred types.

This process is sometimes referred to as generalization because the goal is always to calculate the most general types that are possible.

In a standard Hindley-Milner system, we can usually calculate most general types by quantifying over all relevant type variables that do not appear in the assumptions.

In this section, we will describe how this process is modified to deal with the predicates in Haskell types.
To understand the basic problem, suppose that we have run the type checker over the body of a function h to obtain a list of predicates ps and a type t.

At this point, to obtain the most general result, we could infer a type for h by forming the qualified type qt = (ps :=> t) and then quantifying over any variables in qt that do not appear in the assumptions.

While this is permitted by the theory of qualified types, it is often not the best thing to do in practice.

For example:

The list ps can often be simplified using the context reduction process described in Section 7.4.

This will also ensure that the syntactic restrictions of Haskell are met, requiring all predicates to be in head-normal form.

Some of the predicates in ps may contain only `fixed' variables (i.e., variables appearing in the assumptions), so including those constraints in the inferred type will not be of any use [ Jones, 1992,Section 6.1.4].

These predicates should be `deferred' to the enclosing bindings.

Some of the predicates in ps could result in ambiguity, and require defaulting to avoid a type error.

This aspect of Haskell's type system will be described shortly in Section 11.5.1.

In this paper we use a function called split to address these issues.

For the situation described previously where we have inferred a type t and a list of predicates ps for a function h, we can use split to rewrite and break ps into a pair (ds, rs) of deferred predicates ds and `retained' predicates rs.

The predicates in rs will be used to form an inferred type (rs :=> t) for h, while the predicates in ds will be passed out as constraints to the enclosing scope.

We use the following definition for split:

	  split :: Monad m => ClassEnv -> [Tyvar] -> [Tyvar] -> [Pred]
	                        -> m ([Pred], [Pred])
	  split ce fs gs ps = do ps' <- reduce ce ps
	                         let (ds, rs) = partition (all (`elem` fs) . tv) ps'
	                         rs' <- defaultedPreds ce (fs++gs) rs
	                         return (ds, rs \\ rs')
In addition to a list of predicates ps, the split function is parameterized by two lists of type variables.

The first, fs, specifies the set of `fixed' variables, which are just the variables appearing free in the assumptions.

The second, gs, specifies the set of variables over which we would like to quantify; for the example above, it would just be the variables in (tv t \\ fs).

It is possible for ps to contain variables that are not in either fs or gs (and hence not in the parameter (fs++gs) that is passed to defaultedPreds).

In Section 11.5.1 we will see that this is an indication of ambiguity.

There are three stages in the split function, corresponding directly to the three points listed previously.

The first stage uses reduce to perform context reduction.

The second stage uses the standard prelude function partition to identify the deferred predicates, ds; these are just the predicates in ps' that contain only fixed type variables.

The third stage determines whether any of the predicates in rs should be eliminated using Haskell's defaulting mechanism, and produces a list of all such predicates in rs'.

Hence the final set of retained predicates is produced by the expression rs \\ rs' in the last line of the definition.

