### 11.6.3  Combined Binding Groups

Haskell requires a process of dependency analysis to break down complete sets of bindings-either at the top-level of a program, or within a local definition-into the smallest possible groups of mutually recursive definitions, and ordered so that no group depends on the values defined in later groups.

This is necessary to obtain the most general types possible.

For example, consider the following fragment from a standard prelude for Haskell:
	   foldr f a (x:xs) = f x (foldr f a xs)
	   foldr f a []     = a
	   and xs           = foldr (&&) True xs
If these definitions were placed in the same binding group, then we would not obtain the most general possible type for foldr; all occurrences of a variable are required to have the same type at each point within the defining binding group, which would lead to the following type for foldr:
	   (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool
To avoid this problem, we need only notice that the definition of foldr does not depend in any way on &&, and hence we can place the two functions in separate binding groups, inferring first the most general type for foldr, and then the correct type for and.
In the presence of explicitly typed bindings, we can refine the dependency analysis process a little further.

For example, consider the following pair of bindings:

	   f   :: Eq a => a -> Bool
	   f x  = (x==x) || g True
	   g y  = (y<=y) || f True
Although these bindings are mutually recursive, we do not need to infer types for f and g at the same time.

Instead, we can use the declared type of f to infer a type:
	   g   :: Ord a => a -> Bool
and then use this to check the body of f, ensuring that its declared type is correct.
Motivated by these observations, we will represent Haskell binding groups using the following datatype:

	  type BindGroup  = ([Expl], [[Impl]])
The first component in each such pair lists any explicitly typed bindings in the group.

The second component provides an opportunity to break down the list of any implicitly typed bindings into several smaller lists, arranged in dependency order.

In other words, if a binding group is represented by a pair (es,[is_1,...,is_n]), then the implicitly typed bindings in each is_i should depend only on the bindings in es, is_1, ..., is_i, and not on any bindings in is_j when j>i. (Bindings in es could depend on any of the bindings in the group, but will presumably depend on at least those in is_n, or else the group would not be minimal.

Note also that if es is empty, then n must be 1.) In choosing this representation, we have assumed that dependency analysis has been carried out prior to type checking, and that the bindings in each group have been organized into values of type BindGroup as appropriate.

In particular, by separating out implicitly typed bindings as much as possible, we can potentially increase the degree of polymorphism in inferred types.

For a correct implementation of the semantics specified in the Haskell report, a simpler but less flexible approach is required: all implicitly typed bindings must be placed in a single list, even if a more refined decomposition would be possible.

In addition, if the group is restricted, then we must also ensure that none of the explicitly typed bindings in the same BindGroup have any predicates in their type, even though this is not strictly necessary.

With hindsight, these are restrictions that we might prefer to avoid in any future revision of Haskell.

A more serious concern is that the Haskell report does not indicate clearly whether the previous example defining f and g should be valid.

At the time of writing, some implementations accept it, while others do not.

This is exactly the kind of problem that can occur when there is no precise, formal specification! Curiously, however, the report does indicate that a modification of the example to include an explicit type for g would be illegal.

This is a consequence of a throw-away comment specifying that all explicit type signatures in a binding group must have the same context up to renaming of variables [ Peyton Jones & Hughes, 1999,Section 4.5.2].

This is a syntactic restriction that can easily be checked prior to type checking.

Our comments here, however, suggest that it is unnecessarily restrictive.

In addition to the function bindings that we have seen already, Haskell allows variables to be defined using pattern bindings of the form pat = expr.

We do not need to deal directly with such bindings because they are easily translated into the simpler framework used in this paper.

For example, a binding:

	   (x,y) = expr
can be rewritten as:
	   nv = expr
	   x  = fst nv
	   y  = snd nv
where nv is a new variable.

The precise definition of the monomorphism restriction in Haskell makes specific reference to pattern bindings, treating any binding group that includes one as restricted.

So it may seem that the definition of restricted binding groups in this paper is not quite accurate.

However, if we use translations as suggested here, then it turns out to be equivalent: even if the programmer supplies explicit type signatures for x and y in the original program, the translation will still contain an implicitly typed binding for the new variable nv.
Now, at last, we are ready to present the algorithm for type inference of a complete binding group, as implemented by the following function:

	  tiBindGroup :: Infer BindGroup [Assump]
	  tiBindGroup ce as (es,iss) =
	    do let as' = [ v:>:sc | (v,sc,alts) <- es ]
	       (ps, as'') <- tiSeq tiImpls ce (as'++as) iss
	       qss        <- mapM (tiExpl ce (as''++as'++as)) es
	       return (ps++concat qss, as''++as')
The structure of this definition is quite straightforward.

First we form a list of assumptions as' for each of the explicitly typed bindings in the group.

Next, we use this to check each group of implicitly typed bindings, extending the assumption set further at each stage.

Finally, we return to the explicitly typed bindings to verify that each of the declared types is acceptable.

In dealing with the list of implicitly typed binding groups, we use the following utility function, which typechecks a list of binding groups and accumulates assumptions as it runs through the list:

	  tiSeq                  :: Infer bg [Assump] -> Infer [bg] [Assump]
	  tiSeq ti ce as []       = return ([],[])
	  tiSeq ti ce as (bs:bss) = do (ps,as')  <- ti ce as bs
	                               (qs,as'') <- tiSeq ti ce (as'++as) bss
	                               return (ps++qs, as''++as')

