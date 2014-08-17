## 7.3  Entailment

In this section, we describe how class environments can be used to answer questions about which types are instances of particular classes.

More generally, we consider the treatment of entailment: given a predicate p and a list of predicates ps, our goal is to determine whether p will hold whenever all of the predicates in ps are satisfied.

In the special case where p = IsIn i t and ps = [], this amounts to determining whether t is an instance of the class i.

In the theory of qualified types [ Jones, 1992], assertions like this are captured using judgements of the form ps ||- p; we use a different notation here-the entail function that is defined at the end of this section-to make the dependence on a class environment explicit.

As a first step, we can ask how information about superclasses and instances can be used independently to help reason about entailments.

For example, if a type is an instance of a class i, then it must also be an instance of any superclasses of i.

Hence, using only superclass information, we can be sure that, if a given predicate p holds, then so too must all of the predicates in the list bySuper p:

	  bySuper :: ClassEnv -> Pred -> [Pred]
	  bySuper ce p@(IsIn i t)
	   = p : concat [ bySuper ce (IsIn i' t) | i' <- super ce i ]

The list bySuper ce p may contain duplicates, but it will always be finite because of the restriction that the superclass hierarchy is acyclic.

Next we consider how information about instances can be used.

Of course, for a given predicate p = IsIn i t, we can find all the directly relevant instances in a class environment ce by looking in insts ce i.

As we have seen, individual instance declarations are mapped into clauses of the form ps :=> h.

The head predicate h describes the general form of instances that can be constructed from this declaration, and we can use matchPred to determine whether this instance is applicable to the given predicate p.

If it is applicable, then matching will return a substitution u, and the remaining subgoals are the elements of map (apply u) ps.

The following function uses these ideas to determine the list of subgoals for a given predicate:

	  byInst                   :: ClassEnv -> Pred -> Maybe [Pred]
	  byInst ce p@(IsIn i t)    = msum [ tryInst it | it <- insts ce i ]
	   where tryInst (ps :=> h) = do u <- matchPred h p
	                                 Just (map (apply u) ps)

The msum function used here comes from the standard Monad library, and returns the first defined element in a list of Maybe values; if there are no defined elements in the list, then it returns Nothing.

Because Haskell prevents overlapping instances, there is at most one applicable instance for any given p, and we can be sure that the first defined element will actually be the only defined element in this list.

The bySuper and byInst functions can be used in combination to define a general entailment operator, entail.

Given a particular class environment ce, the intention here is that entail ce ps p will be True if, and only if, the predicate p will hold whenever all of the predicates in ps are satisfied:

	  entail        :: ClassEnv -> [Pred] -> Pred -> Bool
	  entail ce ps p = any (p `elem`) (map (bySuper ce) ps) ||
	                   case byInst ce p of
	                     Nothing -> False
	                     Just qs -> all (entail ce ps) qs

The first step here is to determine whether p can be deduced from ps using only superclasses.

If that fails, we look for a matching instance and generate a list of predicates qs as a new goal, each of which must, in turn, follow from ps.

Conditions specified in the Haskell report-namely that the class hierarchy is acyclic and that the types in any instance declaration are strictly smaller than those in the head-translate into conditions on the values for the ClassEnv that can be passed in as ce, and these are enough to guarantee that tests for entailment will terminate.

Completeness of the algorithm is also important: will entail ce ps p always return True whenever there is a way to prove p from ps? In fact our algorithm does not cover all possible cases: it does not test to see if p is a superclass of some other predicate q for which entail ce ps q is True.

Extending the algorithm to test for this would be very difficult because there is no obvious way to choose a particular q, and, in general, there will be infinitely many potential candidates to consider.

Fortunately, a technical condition in the Haskell report [ Peyton Jones & Hughes, 1999,Condition 1 on Page 47] reassures us that this is not necessary: if p can be obtained as an immediate superclass of some predicate q that was built using an instance declaration in an entailment entail ce ps q, then ps must already be strong enough to deduce p.

Thus, although we have not formally proved these properties, we believe that our algorithm is sound, complete, and guaranteed to terminate.

