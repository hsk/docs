### 11.6.4 Top-level Binding Groups

At the top-level, a Haskell program can be thought of as a list of binding groups:
	  type Program = [BindGroup]
Even the definitions of member functions in class and instance declarations can be included in this representation; they can be translated into top-level, explicitly typed bindings.

The type inference process for a program takes a list of assumptions giving the types of any primitives, and returns a set of assumptions for any variables.

	  tiProgram :: ClassEnv -> [Assump] -> Program -> [Assump]
	  tiProgram ce as bgs = runTI $
	                        do (ps, as') <- tiSeq tiBindGroup ce as bgs
	                           s         <- getSubst
	                           rs        <- reduce ce (apply s ps)
	                           s'        <- defaultSubst ce [] rs
	                           return (apply (s'@@s) as')
This completes our presentation of the Haskell type system.


