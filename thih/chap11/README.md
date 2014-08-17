# 11  Type Inference

With this section we have reached the heart of the paper, detailing our algorithm for type inference.

It is here that we finally see how the machinery that has been built up in earlier sections is actually put to use.

We develop the complete algorithm in stages, working through the abstract syntax of the input language from the simplest part (literals) to the most complex (binding groups).

Most of the typing rules are expressed by functions whose types are simple variants of the following synonym:

	  type Infer e t = ClassEnv -> [Assump] -> e -> TI ([Pred], t)

In more theoretical treatments, it would not be surprising to see the rules expressed in terms of judgments G;P | A\vdash e:t, where G is a class environment, P is a set of predicates, A is a set of assumptions, e is an expression, and t is a corresponding type [ Jones, 1992].

Judgments like this can be thought of as 5-tuples, and the typing rules themselves just correspond to a 5-place relation.

Exactly the same structure shows up in types of the form Infer e t, except that, by using functions, we distinguish very clearly between input and output parameters.


