## 7.4  Context Reduction

Class environments also play an important role in an aspect of the Haskell type system that is known as context reduction.

The basic goal of context reduction is to reduce a list of predicates to an equivalent but, in some sense, simpler list.

The Haskell report [ Peyton Jones & Hughes, 1999]provides only informal hints about this aspect of the Haskell typing, where both pragmatics and theory have important parts to play.

We believe therefore that this is one of the areas where a more formal specification will be particularly valuable.
One way to simplify a list of predicates is to simplify the type components of individual predicates in the list.

For example, given the instance declarations in the Haskell standard prelude, we could replace any occurrences of predicates like Eq [a], Eq (a,a), or Eq ([a],Int) with Eq a.

This is valid because, for any choice of a, each one of these predicates holds if, and only if, Eq a holds.

Notice that, in some cases, an attempt to simplify type components-for example, by replacing Eq (a, b) with (Eq a, Eq b)-may increase the number of predicates in the list.

The extent to which simplifications like this are used in a system of qualified types has an impact on the implementation and performance of overloading in practical systems [ Jones, 1992,Chapter 7].

In Haskell, however, the decisions are made for us by a syntactic restriction that forces us to simplify predicates until we obtain types in a kind of `head-normal form'.

This terminology is motivated by similarities with the concept of head-normal forms in l-calculus.

More precisely, the syntax of Haskell requires class arguments to be of the form v t1 ... tn, where v is a type variable, and t1,...,tn are types (and n ³ 0).

The following function allows us to determine whether a given predicate meets these restrictions:

	  inHnf       :: Pred -> Bool
	  inHnf (IsIn c t) = hnf t
	   where hnf (TVar v)  = True
	         hnf (TCon tc) = False
	         hnf (TAp t _) = hnf t

Predicates that do not fit this pattern must be broken down using byInst.

In some cases, this will result in predicates being eliminated altogether.

In others, where byInst fails, it will indicate that a predicate is unsatisfiable, and will trigger an error diagnostic.

This process is captured in the following definition:

	  toHnfs      :: Monad m => ClassEnv -> [Pred] -> m [Pred]
	  toHnfs ce ps = do pss <- mapM (toHnf ce) ps
	                    return (concat pss)
 
	  toHnf                 :: Monad m => ClassEnv -> Pred -> m [Pred]
	  toHnf ce p | inHnf p   = return [p]
	             | otherwise = case byInst ce p of
	                             Nothing -> fail "context reduction"
	                             Just ps -> toHnfs ce ps

Another way to simplify a list of predicates is to reduce the number of elements that it contains.

There are several ways that this might be achieved: by removing duplicates (e.g., reducing (Eq a, Eq a) to Eq a); by eliminating predicates that are already known to hold (e.g., removing any occurrences of Num Int); or by using superclass information (e.g., reducing (Eq a, Ord a) to Ord a).

In each case, the reduced list of predicates, is equivalent to the initial list, meaning that all the predicates in the first will be satisfied if, and only if, all of the predicates in the second are satisfied.

The simplification algorithm that we will use here is based on the observation that a predicate p in a list of predicates (p:ps) can be eliminated if p is entailed by ps.

As a special case, this will eliminate duplicated predicates: if p is repeated in ps, then it will also be entailed by ps.

These ideas are used in the following definition of the simplify function, which loops through each predicate in the list and uses an accumulating parameter to build up the final result.

Each time we encounter a predicate that is entailed by the others, we remove it from the list.


	  simplify   :: ClassEnv -> [Pred] -> [Pred]
	  simplify ce = loop []
	   where loop rs []                            = rs
	         loop rs (p:ps) | entail ce (rs++ps) p = loop rs ps
	                        | otherwise            = loop (p:rs) ps

Now we can describe the particular form of context reduction used in Haskell as a combination of toHnfs and simplify.

Specifically, we use toHnfs to reduce the list of predicates to head-normal form, and then simplify the result:

	  reduce      :: Monad m => ClassEnv -> [Pred] -> m [Pred]
	  reduce ce ps = do qs <- toHnfs ce ps
	                    return (simplify ce qs)

As a technical aside, we note that there is some redundancy in the definition of reduce.

The simplify function is defined in terms of entail, which makes use of the information provided by both superclass and instance declarations.

The predicates in qs, however, are guaranteed to be in head-normal form, and hence will not match instance declarations that satisfy the syntactic restrictions of Haskell.

It follows that we could make do with a version of simplify that used only the following function in determining (superclass) entailments:

	  scEntail        :: ClassEnv -> [Pred] -> Pred -> Bool
	  scEntail ce ps p = any (p `elem`) (map (bySuper ce) ps)

