# 9  Assumptions

Assumptions about the type of a variable are represented by values of the Assump datatype, each of which pairs a variable name with a type scheme:
	  data Assump = Id :>: Scheme
Once again, we can extend the Types class to allow the application of a substitution to an assumption:

	  instance Types Assump where
	    apply s (i :>: sc) = i :>: (apply s sc)
	    tv (i :>: sc)      = tv sc
Thanks to the instance definition for Types on lists (Section 5), we can also use the apply and tv operators on the lists of assumptions that are used to record the type of each program variable during type inference.

We will also use the following function to find the type of a particular variable in a given set of assumptions:

	  find                 :: Monad m => Id -> [Assump] -> m Scheme
	  find i []             = fail ("unbound identifier: " ++ i)
	  find i ((i':>:sc):as) = if i==i' then return sc else find i as
This definition allows for the possibility that the variable i might not appear in as.

In practice, occurrences of unbound variables will probably have been detected in earlier compiler passes.


