### 11.5.1  Ambiguity and Defaults

In the terminology of Haskell [ Peyton Jones & Hughes, 1999,Section 4.3.4], a type scheme ps => t is ambiguous if ps contains generic variables that do not also appear in t.

This condition is important because theoretical studies [ Blott, 1991, Jones, 1992]have shown that, in the general case, we can only guarantee a well-defined semantics for a term if its most general type is not ambiguous.

As a result, expressions with ambiguous types are considered ill-typed in Haskell and will result in a static error.

The following definition shows a fairly typical example illustrating how ambiguity problems can occur:
	   stringInc x = show (read x + 1)
The intention here is that a string representation of a number will be parsed (using the prelude function read), incremented, and converted back to a string (using the prelude function show).

But there is a genuine ambiguity because there is nothing to specify which type of number is intended, and because different choices can lead to different semantics.

For example, stringInc "1.5" might produce a result of "2.5" if floating point numbers are used, or a parse error (or perhaps a result of "2") if integers are used.

This semantic ambiguity is reflected by a syntactic ambiguity in the inferred type of stringInc:
	   stringInc :: (Read a, Num a) => String -> String
(There is no Show a constraint here because Show is a superclass of Num.) A programmer can fix this particular problem quite easily by picking a particular type for a, and by adding an appropriate type annotation:
	   stringInc x = show (read x + 1 :: Int)
Practical experience suggests that ambiguities like this tend to occur quite infrequently in real Haskell code.

Moreover, when ambiguities are detected, the error diagnostics that are generated can often be useful in guiding programmers to genuine problems in their code.

However, the designers of Haskell felt that, in some situations involving numeric types-and particularly involving overloaded numeric literals-the potential for ambiguity was significant enough to become quite a burden on programmers.

Haskell's default mechanism was therefore introduced as a pragmatic compromise that is convenient-because it automates the task of picking types for otherwise ambiguous variables-but also dangerous-because it involves making choices about the semantics of a program in ways that are not always directly visible to programmers.

For this latter reason, the use of defaulting is restricted so that it will only apply under certain, fairly restrictive circumstances.
The remainder of this section explains in more detail how ambiguities in Haskell programs can be detected and, when appropriate, eliminated by a suitable choice of defaults.

The first step is to identify any sources of ambiguity.

Suppose, for example, that we are about to qualify a type with a list of predicates ps and that vs lists all known variables, both fixed and generic.

An ambiguity occurs precisely if there is a type variable that appears in ps but not in vs (i.e., in tv ps \\ vs).

The goal of defaulting is to bind each ambiguous type variable v to a monotype t.

The type t must be chosen so that all of the predicates in ps that involve v will be satisfied once t has been substituted for v.

The following function calculates the list of ambiguous variables and pairs each one with the list of predicates that must be satisfied by any choice of a default:

	  type Ambiguity       = (Tyvar, [Pred])
 
	  ambiguities         :: ClassEnv -> [Tyvar] -> [Pred] -> [Ambiguity]
	  ambiguities ce vs ps = [ (v, filter (elem v . tv) ps) | v <- tv ps \\ vs ]
Given one of these pairs (v,qs), and as specified by the Haskell report [ Peyton Jones & Hughes, 1999,Section 4.3.4], defaulting is permitted if, and only if, all of the following conditions are satisfied:

All of the predicates in qs are of the form IsIn c (TVar v) for some class c.
At least one of the classes involved in qs is a standard numeric class.

The list of these class names is provided by a constant:
	  numClasses :: [Id]
	  numClasses  = ["Num", "Integral", "Floating", "Fractional",
	                 "Real", "RealFloat", "RealFrac"]
All of the classes involved in qs are standard classes, defined either in the standard prelude or standard libraries.

Again, the list of these class names is provided by a constant:
	  stdClasses :: [Id]
	  stdClasses  = ["Eq", "Ord", "Show", "Read", "Bounded", "Enum", "Ix",
	                 "Functor", "Monad", "MonadPlus"] ++ numClasses
That there is at least one type in the list of default types for the enclosing module that is an instance of all of the classes mentioned in qs.

The first such type will be selected as the default.

The list of default types can be obtained from a class environment by using the defaults function that was described in Section 7.2.
These conditions are captured rather more succinctly in the following definition, which we use to calculate the candidates for resolving a particular ambiguity:
	  candidates           :: ClassEnv -> Ambiguity -> [Type]
	  candidates ce (v, qs) = [ t' | let is = [ i | IsIn i t <- qs ]
	                                     ts = [ t | IsIn i t <- qs ],
	                                 all ((TVar v)==) ts,
	                                 any (`elem` numClasses) is,
	                                 all (`elem` stdClasses) is,
	                                 t' <- defaults ce,
	                                 all (entail ce []) [ IsIn i t' | i <- is ] ]
If candidates returns an empty list for a given ambiguity, then defaulting cannot be applied to the corresponding variable, and the ambiguity cannot be avoided.

On the other hand, if the result is a non-empty list ts, then we will be able to substitute head ts for v and remove the predicates in qs from ps.

The calculations for the defaulting substitution, and for the list of predicates that it eliminates follow very similar patterns, which we capture by defining them in terms of a single, higher-order function:

	  withDefaults :: Monad m => ([Ambiguity] -> [Type] -> a)
	                    -> ClassEnv -> [Tyvar] -> [Pred] -> m a
	  withDefaults f ce vs ps
	      | any null tss  = fail "cannot resolve ambiguity"
	      | otherwise     = return (f vps (map head tss))
	        where vps = ambiguities ce vs ps
	              tss = map (candidates ce) vps
The withDefaults function takes care of picking suitable defaults, and of checking whether there are any ambiguities that cannot be eliminated.

If defaulting succeeds, then the list of predicates that can be eliminated is obtained by concatenating the predicates in each Ambiguity pair:

	  defaultedPreds :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m [Pred]
	  defaultedPreds  = withDefaults (\vps ts -> concat (map snd vps))
In a similar way, the defaulting substitution can be obtained by zipping the list of variables together with the list of defaults:

	  defaultSubst   :: Monad m => ClassEnv -> [Tyvar] -> [Pred] -> m Subst
	  defaultSubst    = withDefaults (\vps ts -> zip (map fst vps) ts)
One might wonder why the defaulting substitution is useful to us here; if the ambiguous variables don't appear anywhere in the assumptions or in the inferred types, then applying this substitution to those components would have no effect.

In fact, we will only need defaultSubst at the top-level, when type inference for an entire module is complete [ Peyton Jones & Hughes, 1999,Section 4.5.5, Rule 2].

In this case, it is possible that Haskell's infamous `monomorphism restriction' (see Section 11.6.2) may prevent generalization over some type variables.

But Haskell does not allow the types of top-level functions to contain unbound type variables.

Instead, any remaining variables are considered ambiguous, even if they appear in inferred types; the substitution is needed to ensure that they are bound correctly.


