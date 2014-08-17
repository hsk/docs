# 12  Conclusions

We have presented a complete Haskell program that implements a type checker for the Haskell language.

In the process, we have clarified certain aspects of the current design, as well as identifying some ambiguities in the existing, informal specification.
The type checker has been developed, type-checked, and tested using the ``Haskell 98 mode'' of Hugs 98 [ Jones & Peterson, 1999].

The full program includes many additional functions, not shown in this paper, to ease the task of testing, debugging, and displaying results.

We have also translated several large Haskell programs-including the Standard Prelude, the Maybe and List libraries, and the source code for the type checker itself-into the representations described in Section 11, and successfully passed these through the type checker.

As a result of these and other experiments we have good evidence that the type checker is working as intended, and in accordance with the expectations of Haskell programmers.

We believe that this typechecker can play a useful role, both as a formal specification for the Haskell type system, and as a testbed for experimenting with future extensions.

