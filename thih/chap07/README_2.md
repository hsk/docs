## 7.2  Class Environments

The information provided by the class and instance declarations in a given program can be captured by a class environment of type:

	  data ClassEnv = ClassEnv { classes  :: Id -> Maybe Class,
	                             defaults :: [Type] }

The classes component in a ClassEnv value is a partial function that maps identifiers to Class values (or to Nothing if there is no class corresponding to the specified identifier).

We define helper functions super and insts to extract the list of superclass identifiers, and the list of instances, respectively, for a class name i in a class environment ce:

	  super     :: ClassEnv -> Id -> [Id]
	  super ce i = case classes ce i of Just (is, its) -> is
	 
	  insts     :: ClassEnv -> Id -> [Inst]
	  insts ce i = case classes ce i of Just (is, its) -> its

These functions are intended to be used only in cases where it is known that the class i is defined in the environment ce.

In some cases, this condition might be guaranteed by static analysis prior to type checking.

Alternatively, we can resort to a dynamic check by testing defined (classes ce i) before applying either function.

The function defined used here is defined as follows5:

	  defined :: Maybe a -> Bool
	  defined (Just x) = True
	  defined Nothing  = False

We will also define a helper function, modify, to describe how a class environment can be updated to reflect a new binding of a Class value to a given identifier:

	  modify       :: ClassEnv -> Id -> Class -> ClassEnv
	  modify ce i c = ce{classes = \j -> if i==j then Just c
	                                             else classes ce j}

The defaults component of a ClassEnv value is used to provide a list of types for defaulting, as described in Section 11.5.1.

Haskell allows programmers to specify a value for this list using a default declaration; if no explicit declaration is given, then a default (Integer,Double) declaration is assumed.

It is easy to describe this using the ClassEnv type.

For example, cedefaults=[tInt] is the result of modifying a class environment ce to reflect the presence of a default (Int) declaration.

Further discussion of defaulting is deferred to Section 11.5.1.

In the remainder of this section, we will show how to build an appropriate class environment for a given program, starting from an (almost) empty class environment, and extending it as necessary to reflect the effect of each class or instance declaration in the program.

The initial class environment is defined as follows:

	  initialEnv :: ClassEnv
	  initialEnv  = ClassEnv { classes  = \i -> fail "class not defined",
	                           defaults = [tInteger, tDouble] }

As we process each class or instance declaration in a program, we transform the initial class environment to add entries, either for a new class, or for a new instance, respectively.

In either case, there is a possibility that the new declaration might be incompatible with the previous declarations, attempting, for example, to redefine an existing class or instance.

For this reason, we will describe transformations of a class environment as functions of the EnvTransformer type, using a Maybe type to allow for the possibility of errors:

  type EnvTransformer = ClassEnv -> Maybe ClassEnv
The sequencing of multiple transformers can be described by a (forward) composition operator (<:>):

	  infixr 5 <:>
	  (<:>)       :: EnvTransformer -> EnvTransformer -> EnvTransformer
	  (f <:> g) ce = do ce' <- f ce
	                    g ce'

Some readers will recognize this as a special case of the more general Kleisli composition operator; without the type declaration, the definition given here would work for any monad and for any element types, not just for Maybe and ClassEnv.

To add a new class to an environment, we must check that there is not already a class with the same name, and that all of the named superclasses are already defined.

This is a simple way of enforcing Haskell's restriction that the superclass hierarchy be acyclic.

Of course, in practice, it will be necessary to topologically sort the set of class declarations in a program to determine a suitable ordering; any cycles in the hierarchy will typically be detected at this stage.

	  addClass                              :: Id -> [Id] -> EnvTransformer
	  addClass i is ce
	   | defined (classes ce i)              = fail "class already defined"
	   | any (not . defined . classes ce) is = fail "superclass not defined"
	   | otherwise                           = return (modify ce i (is, []))

For example, we can describe the effect of the class declarations in the Haskell prelude using the following transformer:

	  addPreludeClasses :: EnvTransformer
	  addPreludeClasses  = addCoreClasses <:> addNumClasses

This definition breaks down the set of standard Haskell classes into two separate pieces.

The core classes are described as follows:

	  addCoreClasses ::   EnvTransformer
	  addCoreClasses  =   addClass "Eq" []
	                  <:> addClass "Ord" ["Eq"]
	                  <:> addClass "Show" []
	                  <:> addClass "Read" []
	                  <:> addClass "Bounded" []
	                  <:> addClass "Enum" []
	                  <:> addClass "Functor" []
	                  <:> addClass "Monad" []

The hierarchy of numeric classes is captured separately in the following definition:

	  addNumClasses  ::   EnvTransformer
	  addNumClasses   =   addClass "Num" ["Eq", "Show"]
	                  <:> addClass "Real" ["Num", "Ord"]
	                  <:> addClass "Fractional" ["Num"]
	                  <:> addClass "Integral" ["Real", "Enum"]
	                  <:> addClass "RealFrac" ["Real", "Fractional"]
	                  <:> addClass "Floating" ["Fractional"]
	                  <:> addClass "RealFloat" ["RealFrac", "Floating"]

To add a new instance to a class, we must check that the class to which the instance applies is defined, and that the new instance does not overlap with any previously declared instance:

	  addInst                        :: [Pred] -> Pred -> EnvTransformer
	  addInst ps p@(IsIn i _) ce
	   | not (defined (classes ce i)) = fail "no class for instance"
	   | any (overlap p) qs           = fail "overlapping instance"
	   | otherwise                    = return (modify ce i c)
	     where its = insts ce i
	           qs  = [ q | (_ :=> q) <- its ]
	           c   = (super ce i, (ps:=>p) : its)

Two instances for a class are said to overlap if there is some predicate that is a substitution instance of the heads of both instance declarations.

It is easy to test for overlapping predicates using the functions that we have defined previously:

	  overlap       :: Pred -> Pred -> Bool
	  overlap p q    = defined (mguPred p q)

This test covers simple cases where a program provides two instance declarations for the same type (for example, two declarations for Eq Int), but it also covers cases where more interesting overlaps occur (for example, between the predicates Eq [Int] and Eq [a], or between predicates Eq (a,Bool) and Eq (Int,b)).

In each case, the existence of an overlap indicates the possibility of a semantic ambiguity, with two applicable instance declarations, and no clear reason to prefer one over the other.

This is why Haskell treats such overlaps as an error.

Extensions to Haskell to support overlapping instances in certain special cases have been considered elsewhere; they appear to have interesting applications, but also have some potentially troublesome impact on program semantics [ Peyton Jones et al. , 1997].

We will not consider such issues further in this paper.

To illustrate how the addInst function might be used, the following definition shows how the standard prelude class environment can be extended to include the four instances for Ord from the example in Section 7.1:

	  exampleInsts ::  EnvTransformer
	  exampleInsts =   addPreludeClasses
	               <:> addInst [] (IsIn "Ord" tUnit)
	               <:> addInst [] (IsIn "Ord" tChar)
	               <:> addInst [] (IsIn "Ord" tInt)
	               <:> addInst [IsIn "Ord" (TVar (Tyvar "a" Star)),
	                            IsIn "Ord" (TVar (Tyvar "b" Star))]
	                           (IsIn "Ord" (pair (TVar (Tyvar "a" Star))
	                                             (TVar (Tyvar "b" Star))))

The Haskell report imposes some further restrictions on class and instance declarations that are not enforced by the definitions of addClass and addInst.

For example, the superclasses of a class should have the same kind as the class itself; the parameters of any predicates in an instance context should be type variables, each of which should appear in the head of the instance; and the type appearing in the head of an instance should consist of a type constructor applied to a sequence of distinct type variable arguments.

Because these conditions have no direct impact on type checking, and because they are straightforward but tedious to verify, we have chosen not to include tests for them here, and instead assume that they have been checked during static analysis prior to type checking.

