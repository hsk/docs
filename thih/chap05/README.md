# 5  Substitutions 代入操作

Substitutions-finite functions, mapping type variables to types-play a major role in type inference.

In this paper, we represent substitutions using association lists:

	  type Subst  = [(Tyvar, Type)]

To ensure that we work only with well-formed type expressions, we will be careful to construct only kind-preserving substitutions in which variables are mapped only to types of the same kind.

The simplest substitution is the null substitution, represented by the empty list, which is obviously kind-preserving:

	  nullSubst  :: Subst
	  nullSubst   = []

Almost as simple are the substitutions (u +-> t)3 that map a single variable u to a type t of the same kind:

	  (+->)      :: Tyvar -> Type -> Subst
	  u +-> t     = [(u, t)]

This is kind-preserving if, and only if, kind u = kind t.



Substitutions can be applied to types-and, in fact, to any other value with type components-in a natural way.

This suggests that we overload the operation to apply a substitution so that it can work on different types of object:

	  class Types t where
	    apply :: Subst -> t -> t
	    tv    :: t -> [Tyvar]

In each case, the purpose of applying a substitution is the same: To replace every occurrence of a type variable in the domain of the substitution with the corresponding type.

We also include a function tv that returns the set of type variables (i.e., Tyvars) appearing in its argument, listed in order of first occurrence (from left to right), with no duplicates.

The definitions of these operations for Type are as follows:


	  instance Types Type where
	    apply s (TVar u)  = case lookup u s of
	                         Just t  -> t
	                         Nothing -> TVar u
	    apply s (TAp l r) = TAp (apply s l) (apply s r)
	    apply s t         = t
	 
	    tv (TVar u)  = [u]
	    tv (TAp l r) = tv l `union` tv r
	    tv t         = []

It is straightforward (and useful!) to extend these operations to work on lists:

	  instance Types a => Types [a] where
	    apply s = map (apply s)
	    tv      = nub . concat . map tv

The apply function can be used to build more complex substitutions.

For example, composition of substitutions, satisfying apply (s1 @@ s2) = apply s1 . apply s2, can be defined using:

	  infixr 4 @@
	  (@@)       :: Subst -> Subst -> Subst
	  s1 @@ s2    = [ (u, apply s1 t) | (u,t) <- s2 ] ++ s1

We can also form a `parallel' composition s1++s2 of two substitutions s1 and s2, but the result is left-biased because bindings in s1 take precedence over any bindings for the same variables in s2.

For a more symmetric version of this operation, we use a merge function, which checks that the two substitutions agree at every variable in the domain of both and hence guarantees that apply (s1++s2) = apply (s2++s1).

Clearly, this is a partial function, which we reflect by arranging for merge to return its result in a monad, using the standard fail function to provide a string diagnostic in cases where the function is undefined.


	  merge      :: Monad m => Subst -> Subst -> m Subst
	  merge s1 s2 = if agree then return (s1++s2) else fail "merge fails"
	   where agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
	                     (map fst s1 `intersect` map fst s2)

It is easy to check that both (@@) and merge produce kind-preserving results from kind-preserving arguments.

In the next section, we will see how the first of these composition operators is used to describe unification, while the second is used in the formulation of a matching operation.

