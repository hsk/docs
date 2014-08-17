## 7.1  Basic Definitions

A Haskell type class can be thought of as a set of types (of some particular kind), each of which supports a certain collection of member functions that are specified as part of the class declaration.

The types in each class (known as instances) are specified by a collection of instance declarations.

Haskell types can be qualified by adding a (possibly empty) list of predicates, or class constraints, to restrict the ways in which type variables are instantiated4:

	  data Qual t = [Pred] :=> t
	                deriving Eq

In a value of the form ps :=> t, we refer to ps as the context and to t as the head.

Predicates themselves consist of a class identifier and a type; a predicate of the form IsIn i t asserts that t is a member of the class named i:

	  data Pred   = IsIn Id Type
	                deriving Eq

For example, using the Qual and Pred datatypes, the type (Num a) => a -> Int can be represented by:

	  [IsIn "Num" (TVar (Tyvar "a" Star))] :=> (TVar (Tyvar "a" Star) `fn` tInt)

It would be easy to extend the Pred datatype to allow other forms of predicate, as is done with Trex records in Hugs [ Jones & Peterson, 1999].

Another frequently requested extension is to allow classes to accept multiple parameters, which would require a list of Types rather than the single Type in the definition above.

The extension of Types to the Qual and Pred datatypes is straightforward:

	  instance Types t => Types (Qual t) where
	    apply s (ps :=> t) = apply s ps :=> apply s t
	    tv (ps :=> t)      = tv ps `union` tv t
 	
	  instance Types Pred where
	    apply s (IsIn i t) = IsIn i (apply s t)
	    tv (IsIn i t)      = tv t

The tasks of calculating most general unifiers and matching substitutions on types also extend naturally to predicates:

	  mguPred, matchPred :: Pred -> Pred -> Maybe Subst
	  mguPred             = lift mgu
	  matchPred           = lift match
	 	
	  lift m (IsIn i t) (IsIn i' t')
	           | i == i'   = m t t'
	           | otherwise = fail "classes differ"

We will represent each class by a pair of lists, one containing the name of each superclass, and another containing an entry for each instance declaration:

	  type Class    = ([Id], [Inst])
	  type Inst     = Qual Pred

For example, a simplified version of the standard Haskell class Ord might be described by the following value of type Class:

	  (["Eq"], [[] :=> IsIn "Ord" tUnit,
	            [] :=> IsIn "Ord" tChar,
	            [] :=> IsIn "Ord" tInt,
	            [IsIn "Ord" (TVar (Tyvar "a" Star)),
	             IsIn "Ord" (TVar (Tyvar "b" Star))]
	               :=> IsIn "Ord" (pair (TVar (Tyvar "a" Star))
	                                    (TVar (Tyvar "b" Star)))])

This structure captures the fact that Eq is a superclass of Ord (the only one in fact), and lists four instance declarations for the unit, character, integer, and pair types (if a and b are in Ord, then (a,b) is also in Ord).

Of course, this is only a fraction of the list of Ord instances that are defined in the full Haskell prelude.

Only the details that are needed for type inference are included in these representations.

A full Haskell implementation would need to store additional information for each declaration, such as the list of member functions for each class and details of their implementations in each particular instance.


