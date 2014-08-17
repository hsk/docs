# 8  Type Schemes

Type schemes are used to describe polymorphic types, and are represented using a list of kinds and a qualified type:
	  data Scheme = Forall [Kind] (Qual Type)
	                deriving Eq
There is no direct equivalent of Forall in the syntax of Haskell.

Instead, implicit quantifiers are inserted as necessary to bind free type variables.

In a type scheme Forall ks qt, each type of the form TGen n that appears in the qualified type qt represents a generic, or universally quantified type variable whose kind is given by ks!!n.

This is the only place where we will allow TGen values to appear in a type.

We had originally hoped that this restriction could be captured statically by a careful choice of the representation for types and type schemes.

Unfortunately, we have not yet found a satisfactory way to enforce this, and, after considering several alternatives, we have settled for the representation shown here because it allows for simple implementations of equality and substitution.

For example, the implementation of apply on Type values ignores TGen values, so we can be sure that there will be no variable capture problems in the following definition:

	  instance Types Scheme where
	    apply s (Forall ks qt) = Forall ks (apply s qt)
	    tv (Forall ks qt)      = tv qt
Type schemes are constructed by quantifying a qualified type qt with respect to a list of type variables vs:

	  quantify      :: [Tyvar] -> Qual Type -> Scheme
	  quantify vs qt = Forall ks (apply s qt)
	   where vs' = [ v | v <- tv qt, v `elem` vs ]
	         ks  = map kind vs'
	         s   = zip vs' (map TGen [0..])
Note that the order of the kinds in ks is determined by the order in which the variables v appear in tv qt, and not by the order in which they appear in vs.

So, for example, the leftmost quantified variable in a type scheme will always be represented by TGen 0.

By insisting that type schemes are constructed in this way, we obtain a unique canonical form for Scheme values.

This is important because it means that we can test whether two type schemes are the same-for example, to determine whether an inferred type agrees with a declared type-using Haskell's derived equality, and without having to implement more complex tests for a-equivalence.

In practice, we sometimes need to convert a Type into a Scheme without adding any qualifying predicates or quantified variables.

For this special case, we can use the following function instead of quantify:

	  toScheme      :: Type -> Scheme
	  toScheme t     = Forall [] ([] :=> t)

To complete our description of type schemes, we need to be able to instantiate the quantified variables in Scheme values.

In fact, for the purposes of type inference, we only need the special case that instantiates a type scheme with fresh type variables.

We therefore defer further description of instantiation to Section 10 where the mechanisms for generating fresh type variables are introduced.


