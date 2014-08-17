# 10  A Type Inference Monad

It is now quite standard to use monads as a way to hide certain aspects of `plumbing' and to draw attention instead to more important aspects of a program's design [ Wadler, 1992].

The purpose of this section is to define the monad that will be used in the description of the main type inference algorithm in Section 11.

Our choice of monad is motivated by the needs of maintaining a `current substitution' and of generating fresh type variables during typechecking.

In a more realistic implementation, we might also want to add error reporting facilities, but in this paper the crude but simple fail function from the Haskell prelude is all that we require.

It follows that we need a simple state monad with only a substitution and an integer (from which we can generate new type variables) as its state:
	  newtype TI a = TI (Subst -> Int -> (Subst, Int, a))
 
	  instance Monad TI where
	    return x   = TI (\s n -> (s,n,x))
	    TI f >>= g = TI (\s n -> case f s n of
	                              (s',m,x) -> let TI gx = g x
	                                          in  gx s' m)
 
	  runTI       :: TI a -> a
	  runTI (TI f) = x where (s,n,x) = f nullSubst 0
The getSubst operation returns the current substitution, while unify extends it with a most general unifier of its arguments:

	  getSubst   :: TI Subst
	  getSubst    = TI (\s n -> (s,n,s))
 
	  unify      :: Type -> Type -> TI ()
	  unify t1 t2 = do s <- getSubst
	                   u <- mgu (apply s t1) (apply s t2)
	                   extSubst u
For clarity, we define the operation that extends the substitution as a separate function, even though it is used only here in the definition of unify:

	  extSubst   :: Subst -> TI ()
	  extSubst s' = TI (\s n -> (s'@@s, n, ()))
Overall, the decision to hide the current substitution in the TI monad makes the presentation of type inference much clearer.

In particular, it avoids heavy use of apply every time an extension is (or might have been) computed.

There is only one primitive that deals with the integer portion of the state, using it in combination with enumId to generate a new type variable of a specified kind:

	  newTVar    :: Kind -> TI Type
	  newTVar k   = TI (\s n -> let v = Tyvar (enumId n) k
	                            in  (s, n+1, TVar v))
One place where newTVar is useful is in instantiating a type scheme with new type variables of appropriate kinds:

	  freshInst               :: Scheme -> TI (Qual Type)
	  freshInst (Forall ks qt) = do ts <- mapM newTVar ks
	                                return (inst ts qt)
The structure of this definition guarantees that ts has exactly the right number of type variables, and each with the right kind, to match ks.

Hence, if the type scheme is well-formed, then the qualified type returned by freshInst will not contain any unbound generics of the form TGen n.

The definition relies on an auxiliary function inst, which is a variation of apply that works on generic variables.

In other words, inst ts t replaces each occurrence of a generic variable TGen n in t with ts!!n.

It is convenient to build up the definition of inst using overloading:

	  class Instantiate t where
	    inst  :: [Type] -> t -> t
	  instance Instantiate Type where
	    inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
	    inst ts (TGen n)  = ts !! n
	    inst ts t         = t
	  instance Instantiate a => Instantiate [a] where
	    inst ts = map (inst ts)
	  instance Instantiate t => Instantiate (Qual t) where
	    inst ts (ps :=> t) = inst ts ps :=> inst ts t
	  instance Instantiate Pred where
	    inst ts (IsIn c t) = IsIn c (inst ts t)

