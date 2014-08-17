# 7  Type Classes, Predicates and Qualified Types

One of the most unusual features of the Haskell type system, at least in comparison to those of other polymorphically typed languages like ML, is the support that it provides for type classes.

Described by Wadler and Blott [ Wadler & Blott, 1989] as a general mechanism that subsumes several ad-hoc forms of overloading, type classes have found many uses (and, sometimes, abuses!) in the ten years since they were introduced.

A significant portion of the code presented in this paper, particularly in this section, is needed to describe the handling of type classes in Haskell.

(Of course, type classes are not the only source of complexity. 

The treatment of mixed implicit and explicit typing, mutually recursive bindings, and pattern matching-which are often elided in more theoretical presentations-are also significant contributors, as is the extra level of detail and precision that is needed in executable code.
)

