## 11.6  Binding Groups

Our last remaining technical challenge is to describe typechecking for binding groups.

This area is neglected in most theoretical treatments of type inference, often being regarded as a simple exercise in extending basic ideas.

In Haskell, at least, nothing could be further from the truth! With interactions between overloading, polymorphic recursion, and the mixing of both explicitly and implicitly typed bindings, this is the most complex, and most subtle component of type inference.

We will start by describing the treatment of explicitly typed bindings and implicitly typed bindings as separate cases, and then show how these can be combined.

