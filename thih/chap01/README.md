# 1  Introduction

Haskell2 benefits from one of the most sophisticated type systems of any widely used programming language.

Unfortunately, it also suffers because there is no formal specification of what the type system should be.

As a result:

- It is hard for Haskell implementors to be sure that their systems accept the same programs as other implementations.

	The informal specification in the Haskell report [ Peyton Jones & Hughes, 1999]leaves too much room for confusion and misinterpretation.

	This leads to genuine discrepancies between implementations, as subscribers to the Haskell mailing list will have seen.
- It is hard for Haskell programmers to understand the details of the type system and to appreciate why some programs are accepted when others are not.

	Formal presentations of most aspects of the type system are available, but they often abstract on specific features that are Haskell-like, but not Haskell-exact, and do not describe the complete type system.

	Moreover, these papers tend to use disparate and unfamiliar technical notation and concepts that may be difficult for some Haskell programmers to understand.

- It is hard for Haskell researchers to explore new type system extensions, or even to study usability issues that arise with the present type system such as the search for better type error diagnostics.

	Work in these areas requires a clear understanding of the type system and, ideally, a platform on which to build and experiment with prototype implementations.

	The existing Haskell implementations are not suitable for this (and were not intended to be): the nuts and bolts of a type system are easily obscured by the use of clever data structures and optimizations, or by the need to integrate smoothly with other parts of an implementation.

This paper presents a formal description of the Haskell type system using the notation of Haskell itself as a specification language.

Indeed, the source code for this paper is itself an executable Haskell program that is passed through a custom preprocessor and then through LATEX to obtain the typeset version.

The type checker is available in source form on the Internet at http://www.cse.ogi.edu/~mpj/thih/.

We hope that this will serve as a resource for the Haskell community, and that it will be a significant step in addressing the problems described previously.
One audience whose needs may not be particularly well met by this paper are researchers in programming language type systems who do not have experience of Haskell. (Of course, we encourage anyone in that position to learn more about Haskell!) Indeed, we do not follow the traditional route in such settings where the type system might first be presented in its purest form, and then related to a more concrete type inference algorithm by soundness and completeness theorems.

Here, we deal only with type inference.

It does not even make sense to ask if our algorithm computes `principal' types: such a question requires a comparison between two different presentations of a type system, and we only have one.

Nevertheless, we believe that our specification could be recast in a standard, type-theoretic manner and used to develop a presentation of Haskell typing in a more traditional style.

The code presented here can be executed with any Haskell system, but our primary goals have been clarity and simplicity, and the resulting code is not intended to be an efficient implementation of type inference.

Indeed, in some places, our choice of representation may lead to significant overheads and duplicated computation.

It would be interesting to try to derive a more efficient, but provably correct implementation from the specification given here.

We have not attempted to do this because we expect that it would obscure the key ideas that we want to emphasize.

It therefore remains as a topic for future work, and as a test to assess the applicability of program transformation and synthesis to modestly sized Haskell programs.

Another goal of this paper is to give as complete a description of the Haskell type system as possible, while also aiming for conciseness.

For this to be possible, we have assumed that certain transformations and checks will have been made prior to typechecking, and hence that we can work with a much simpler abstract syntax than the full source-level syntax of Haskell would suggest.

As we argue informally at various points in the paper, we do not believe that there would be any significant difficulty in extending our system to deal with the missing constructs.

All of the fundamental components, including the thorniest aspects of Haskell typing, are addressed in the framework that we present here.

Our specification does not attempt to deal with all of the issues that would occur in the implementation of a type checker in a full Haskell implementation.

We do not tackle the problems of interfacing a typechecker with compiler front ends (to track source code locations in error diagnostics, for example) or back ends (to describe the implementation of overloading, for example), nor do we attempt to formalize any of the extensions that are implemented in current Haskell systems.

This is one of things that makes our specification relatively concise (429 lines of Haskell code).

By comparison, the core parts of the Hugs typechecker take some 90+ pages of C code.

Some examples are included in the paper to illustrate the datatypes and representations that are used.

However, for reasons of space, the definitions of some constants that represent entities in the standard prelude, as well as the machinery that we use in testing to display the results of type inference, are included only in the electronic distribution, and not in the typeset version of the paper.

Apart from those details, this paper gives the full source code.

We expect the program described here to evolve in at least three different ways.

- Formal specifications are not immune to error, and so it is possible that changes will be required to correct bugs in the code presented here.

	On the other hand, by writing our specification as a program that can be typechecked and executed with existing Haskell implementations, we have a powerful facility for detecting simple bugs automatically and for testing to expose deeper problems.

- As it stands, this paper just provides one more interpretation of the Haskell type system.

	We believe that it is consistent with the official specification, but because the latter is given only informally, we cannot prove the correctness of our program in a rigorous manner.

	Instead, we hope that our code, perhaps with some modifications, will eventually serve as a precise definition of the Haskell type system, capturing a consensus within the Haskell community.

	There is some evidence that this goal is already within reach: no discrepancies or technical changes have been discovered or reported in more than a year since the first version of this program was released.

- Many extensions of the Haskell type system have been proposed, and several of these have already been implemented in one or more of the available Haskell systems.

	Some of the better known examples of this include multiple-parameter type classes, existential types, rank-2 polymorphism, and extensible records.

	We would like to obtain formal descriptions for as many of these proposals as possible by extending the core specification presented here.

It will come as no surprise to learn that some knowledge of Haskell will be required to read this paper.

That said, we have tried to keep the definitions and code as clear and simple as possible, and although we have made some use of Haskell overloading and do-notation, we have generally avoided using the more esoteric features of Haskell.

In addition, some experience with the basics of Hindley-Milner style type inference [ Hindley, 1969, Milner, 1978, Damas & Milner, 1982]will be needed to understand the algorithms presented here.

Although we have aimed to keep our presentation as simple as possible, some aspects of the problems that we are trying to address have inherent complexity or technical depth that cannot be side-stepped.

In short, this paper will probably not be useful as a tutorial introduction to Hindley-Milner style type inference!


