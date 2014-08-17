## 11.2  Patterns

Patterns are used to inspect and deconstruct data values in lambda abstractions, function and pattern bindings, list comprehensions, do notation, and case expressions.

We will represent patterns using values of the Pat datatype:
	  data Pat        = PVar Id
	                  | PWildcard
	                  | PAs  Id Pat
	                  | PLit Literal
	                  | PNpk Id Integer
	                  | PCon Assump [Pat]
A PVar i pattern matches any value and binds the result to the variable i.

A PWildcard pattern, corresponding to an underscore _ in Haskell syntax, matches any value, but does not bind any variables.

A pattern of the form (PAs i pat), known as an ``as-pattern'' and written using the syntax i@pat in Haskell, binds the variable i to any value that matches the pattern pat, while also binding any variables that appear in pat.

A PLit l pattern matches only the particular value denoted by the literal l.

A pattern (PNpk i k) is an (n+k) pattern, which matches any positive integral value m that is greater than or equal to k, and binds the variable i to the difference (m-k).

Finally, a pattern of the form PCon a pats matches only values that were built using the constructor function a with a sequence of arguments that matches pats.

We use values a of type Assump to represent constructor functions; all that we really need for typechecking is the type, although the name is useful for debugging.

A full implementation would store additional details, such as arity, and use this to check that constructor functions in patterns are always fully applied.

Most Haskell patterns have a direct representation in Pat, but extensions would be needed to account for patterns using labeled fields.

This is not difficult, but adds some complexity, which we prefer to avoid in this presentation.

Type inference for patterns has two goals: To calculate a type for each bound variable, and to determine what type of values the whole pattern might match.

This leads us to look for a function:

	  tiPat :: Pat -> TI ([Pred], [Assump], Type)
Note that we do not need to pass in a list of assumptions here; by definition, any occurrence of a variable in a pattern would hide rather than refer to a variable of the same name in an enclosing scope.

For a variable pattern, PVar i, we just return a new assumption, binding i to a fresh type variable.

	  tiPat (PVar i) = do v <- newTVar Star
	                      return ([], [i :>: toScheme v], v)
Haskell does not allow multiple use of any variable in a pattern, so we can be sure that this is the first and only occurrence of i that we will encounter in the pattern.

Wildcards are typed in the same way except that we do not need to create a new assumption:

	  tiPat PWildcard   = do v <- newTVar Star
	                         return ([], [], v)
To type an as-pattern PAs i pat, we calculate a set of assumptions and a type for the pat pattern, and then add an extra assumption to bind i:

	  tiPat (PAs i pat) = do (ps, as, t) <- tiPat pat
	                         return (ps, (i:>:toScheme t):as, t)
For literal patterns, we use tiLit from the previous section:

	  tiPat (PLit l) = do (ps, t) <- tiLit l
	                      return (ps, [], t)
The rule for (n+k) patterns does not fix a type for the bound variable, but adds a predicate to constrain the choice to instances of the Integral class:

	  tiPat (PNpk i k)  = do t <- newTVar Star
	                         return ([IsIn "Integral" t], [i:>:toScheme t], t)
The case for constructed patterns is slightly more complex:

	  tiPat (PCon (i:>:sc) pats) = do (ps,as,ts) <- tiPats pats
	                                  t'         <- newTVar Star
	                                  (qs :=> t) <- freshInst sc
	                                  unify t (foldr fn t' ts)
	                                  return (ps++qs, as, t')
First we use the tiPats function, defined below, to calculate types ts for each subpattern in pats together with corresponding lists of assumptions in as and predicates in ps.

Next, we generate a new type variable t' that will be used to capture the (as yet unknown) type of the whole pattern.

From this information, we would expect the constructor function at the head of the pattern to have type foldr fn t' ts.

We can check that this is possible by instantiating the known type sc of the constructor and unifying.

The tiPats function is a variation of tiPat that takes a list of patterns as input, and returns a list of types (together with a list of predicates and a list of assumptions) as its result.

	  tiPats     :: [Pat] -> TI ([Pred], [Assump], [Type])
	  tiPats pats = do psasts <- mapM tiPat pats
	                   let ps = concat [ ps' | (ps',_,_) <- psasts ]
	                       as = concat [ as' | (_,as',_) <- psasts ]
	                       ts = [ t | (_,_,t) <- psasts ]
	                   return (ps, as, ts)
We have already seen how tiPats was used in the treatment of PCon patterns above.

It is also useful in other situations where lists of patterns are used, such as on the left hand side of an equation in a function definition.


