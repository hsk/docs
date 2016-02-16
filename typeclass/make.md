# How to make ad-hoc polymorphism less ad hoc

http://www.cse.iitk.ac.in/users/karkare/courses/2010/cs653/Papers/ad-hoc-polymorphism.pdf

# アドホック多相型下でのアドホックの作り方

Philip Wadler and Stephen Blott

University of Glasgow

October 1988

## Abstract

This paper presents type classes, a new approach to ad-hoc polymorphism.
Type classes permit Overloading of arithmetic operators such as multiplication, and generalise the "eqtype variables" of Standard ML.
Type classes extend the Hindley/Milner polymorphic type system, and provide a new approach to issues that arise in object-oriented programming, bounded type quantification, and abstract data types.
This paper provides an informal introduction to type classes, and defines them formally by means of type inference rules.

本論文では、型クラス、アドホック多型への新しいアプローチを提示します。
型クラスは、乗算などの算術演算子のオーバーロードを許可し、標準MLの「eqtype変数」を一般化します。
型クラスはヒンドリー/ミルナー多相型システムを拡張し、オブジェクト指向プログラミング、有界型定量化、および抽象データ型で発生する問題への新しいアプローチを提供します。
本稿では、クラスを入力するための非公式の導入を提供し、型推論規則によって正式にそれらを定義します。

## 1 Introduction

Strachey chose the adjectives ad-hoc and parametric to distinguish two varieties of polymorphism [Str67].
Ad-hoc polymorphism occurs when a function is defined Over several different types, acting in a different way for each type.
A typical example is overloaded multiplication:
the same symbol may be used to denote multiplication of integers (as in 3\*3) and multiplication of floating point values (as in 3.14\*3.14).

Parametric polymorphism occurs when a function is defined Over a range of types, acting in the same way for each type.
A typical example is the length function, which acts in the same way on a list of integers and a list of floating point numbers.

----

\*Authors' address: Department of Computing Science, University of Glasgow, Glasgow G12 8QQ), Scotland.
Electronic mail: wadler, blott@cs.glasgow .ac.uk.

Published in: 16'th ACM Symposium on Principles of Programming Languages, Austin, Texas, January 1989.

Permission to copy without fee all or part of this material is granted provided that the copies are not made or distributed for direct commercial advantage, the ACM copyright notice and the title of the publication and its date appear, and notice is given that copying is by permission of the Association for Computing Machinery.
To copy otherwise, or to republish, requires a fee and/or specific permission.

----

One widely accepted approach to parametric polymorphism is the Hindley/Milner type system [Hin69, Mil78, DM82], which is used in Standard ML [HMM86, Mil87], Miranda1[Tur85], and other languages.
On the other hand, there is no widely accepted approach to ad-hoc polymorphism,
and so its name is doubly appropriate.

This paper presents type classes, which extend the Hindley/Milner type system to include certain kinds of overloading, and thus bring together the two sorts of polymorphism that Strachey separated.

The type system presented here is a generalisation of the Hindley/Milner type system. As in that system, type declarations can be inferred, so explicit type declarations for functions are not required.
During the inference process, it is possible to translate a program using type classes to an equivalent program that does not use overloading.
The translated programs are typable in the (ungeneralised) Hindley/ Milner
type system.

The body of this paper gives an informal introduction to type classes and the translation rules, while an appendix gives formal rules for typing and translation, in the form of inference rules (as in [DM82]).
The translation rules provide a semantics for type classes.
They also provide one possible implementation technique: if desired, the new system could be added to an existing language with Hindley/Milner types simply by writing a pre-processor.

Two places where the issues of ad-hoc polymorphism arise are the definition of Operators for arithmetic and equality.
Below we examine the approaches to these three problems adopted by Standard ML and Miranda; not only do the approaches differ between the two languages, they also differ within a single language.
But as we shall see, type classes provide a uniform mechanism that can address these problems.

----

1 Miranda is a trademark of Research Software Limited.

----

This work grew out of the efforts of the Haskell committee to design a lazy functional programming language 2.
One of the goals of the Haskell committee was to adopt "off the shelf" solutions to problems wherever possible.
We were a little surprised to realise that arithmetic and equality were areas where no standard solution was available!
Type classes were developed as an attempt to find a better solution to these problems; the solution was judged successful enough to be included in the Haskell design.
However, type classes should be judged independently of Haskell; they could just as well be incorporated into another language, such as Standard ML.

Type classes appear to be closely related to issues that arise in object-oriented programming, bounded quantification of types, and abstract data types [CW85, MP85, Rey85].
Some of the connections are outlined below, but more work is required to understand these relations fully.

A type system very similar to Ours has been discovered independently by Stefan Kaes [Kae88].
Our work improves on Kaes' in several ways, notably by the introduction of type classes to group related operators, and by providing a better translation method.

This paper is divided into two parts: the body gives an informal introduction to type classes, while the appendix gives a more formal description.
Section 2 motivates the new system by describing limitations of ad-hoc polymorphism as it is used in Standard ML, and Miranda.
Section 3 introduces type classes by means of a simple example.
Section 4 illustrates how the example of Section 3 may be translated into an equivalent program without type classes.
Section 5 presents a second example, the definition of an overloaded equality function.
Section 6 describes subclasses.
Section 7 discusses related work and concludes.
Appendix A presents inference rules for typing and translation.

## 2 Limitations of ad-hoc polymorphism

This section motivates Our treatment of ad-hoc polymorphism, by examining problems that arise with arithmetic and equality in Standard ML and Miranda.

-------

\*The Haskell committee includes: Arvind, Brian Boutel, Jon Fairbairn,
Joe Fasel, Paul Hudak, John Hughes, Thomas Johnsson, Dick Kieburtz,
Simon Peyton Jones, Rishiyur Nikhil, Mike Reeve, Philip Wadler, David
Wise, and Jonathan Young.

-------

Arithmetic. In the simplest approach to overloading, basic operations such as addition and multiplication are overloaded, but functions defined in terms of them are not.
For example, although one can write 3\*3 and 3.14\*3.14, one cannot define

	square x = x * x

and then write terms such as

	square 3
	square 3.14

This is the approach taken in Standard ML.
(Incidentally, it is interesting to note that although Standard ML, includes overloading of arithmetic operators, its formal definition is deliberately ambiguous about how this overloading is resolved [HMT88, page 71], and different
versions of Standard ML resolve overloading in different ways.)

A more general approach is to allow the above equation to stand for the definition of two overloaded versions of square, with types Int -\> Int
and Float -\> Float.
But consider the function:

	squares (x, y, z)
	    = (square x, square y, square z)

Since each of x, y, and z might, independently, have either type Int or type Float, there are eight possible overloaded versions of this function.
In general, there may be exponential growth in the number of translations, and this is one reason why such solutions are not widely used.

In Miranda, this problem is side-stepped by not overloading arithmetic operations.
Miranda provides only the floating point type (named "num"), and there is no way to use the type system to indicate that an operation is restricted to integers.

Equality. The history of the equality operation is checkered: it has been treated as overloaded, fully polymorphic, and partly polymorphic.

The first approach to equality is to make it overloaded, just like multiplication.
In particular, equality may be overloaded on every monotype that admits equality, i.e., does not contain an abstract type or a function type.
In such a language, One may write 3\*4 == 12 to denote equality over integers, or 'a' == 'b' to denote equality over characters.
But one cannot define a function member by the equations

	member [] y       = False
	member (x : xs) y = (x == y) \/ member xs y

and then write terms such as

	member [1,2,3] 2
	member "Haskell" 'k'

(We abbreviate a list of characters ['a', 'b', 'c'] as "abc".)
This is the approach taken in the first version of Standard ML [Mil84].

A second approach is to make equality fully polymorphic.
In this case, its type is

	(==) :: a -> a -> Bool

where a is a type variable ranging over every type.
The type of the member function is now

	member :: [a] -> a -> Bool

(We write [a] for the type "list of a".)
This means that applying equality to functions or abstract types does not generate a type error.
This is the approach taken in Miranda: if equality is applied on a function type, the result is a run-time error; if equality is applied on an abstract type, the result is to test the representation for equality.
This last may be considered a bug, as it violates the principle of abstraction.
A third approach is to make equality polymorphic in a limited way.
In this case, its type is

	(==) :: a(==) -> a (==)-> Bool

where a(==) is a type variable ranging Only Over types that admit equality.
The type of the member function is now

	member :: [a(==)] -> a(==)-> Bool

Applying equality, or member, on a function type or abstract type is now a type error.
This is the approach currently taken in Standard ML, where a (==) is written ''a, and called an "eqtype variable".

Polymorphic equality places certain demands upon the implementor of the run-time system.
For instance, in Standard ML, reference types are tested for equality differently from other types, so it must be possible at run-time to distinguish references from other pointers.

Object-oriented programming.
It would be nice if polymorphic equality could be extended to include user-defined equality Operations over abstract types.
To implement this, we would need to require that every object carry with it a pointer to a method, a procedure for performing the equality test.
If we are to have more than One Operation with this property, then each object should carry with it a pointer to a dictionary of appropriate methods.
This is exactly the approach used in object-oriented programming [GR83].

In the case of polymorphic equality, this means that both arguments of the equality function will contain a pointer to the same dictionary
(since they are both of the same type).
This suggests that perhaps dictionaries should be passed around independently of objects; now polymorphic equality would be passed one dictionary and two objects (minus dictionaries).
This is the intuition behind type classes and the translation method described here.

## 3 An introductory example

We will now introduce type classes by means of an example.

Say that we wish to overload (+), (*), and negate (unary minus) on types Int and Float.
To do so, we intrOduce a new type class, called Num, as shown in the class declaration in Figure 1.
This declaration may be read as stating "a, type a belongs to class Num if there are functions named (+), (*), and negate, of the appropriate types, defined on it."

We may now declare instances of this class, as shown by the two instance declarations in Figure 1.
The assertion Num Int may be read "there are functions named (+), (*), and negate, of the appropriate types, defined on Int".
The instance declaration justifies this assertion by giving appropriate bindings for the three functions.
The type inference algorithm must verify that these bindings do have the appropriate type, i.e., that addint has type Int-\>Int-\>Int, and similarly for mulInt and neglint.
(We assume that addsnt, mulInt, and neglint are defined in the standard prelude.) The instance Num Float is declared similarly.

A word on notational conventions: Type class names and type constructor names begin with a capital letter, and type variable names begin with a small letter.
Here, Num is a type class, Int and Float are type constructors, and a is a type variable.

We may now define

	square x = x + x

There exists an algorithm that can infer the type of square from this definition (it is outlined in the appendix).
It derives the type:

	square :: Num a => a -> a

----

	class Num a where
	  (+), (*) :: a -> a -> a
	  negate   :: a -> a

	instance Num Int where
	  (+)    = addInt
	  (*)    = mulInt
	  negate = negInt

	instance Num Float where
	  (+)    = addFloat
	  (*)    = mulFloat
	  negate = negFloat
	  
	square   :: Num a => a -> a
	square x = x * х

	squares           :: Num a, Num b, Num c => (a, b, c) -> (a, b, c)
	squares (x, y, z) =  (square x, square y, square z)

Figure 1: Definition of arithmetic Operations

	data NumD a = NumDict (a -> a -> a) (a -> a -> a) (a -> a)

	add (NumDict a m n) = a
	mul (NumDict a m n) = m
	neg (NumDict a m n) = n
	
	numDInt   :: NumD Tnt
	numDInt   =  NumDict addInt mulInt negInt
	numDFloat :: MumD F1oat
	numDFloat =  NumDict addFloat mulFloat negFloat

	square'         :: NumD a -> a -> a
	square' numDa X =  mul numDa X X

	squares' :: (NumD a, NumD b, NumD c) -> (a,b,c) -> (a,b,c)

	squares' (numD a, numD b, numD c) (x, y, z)
	    = (square' numDa x, square' numDb y, square' numDc z)

Figure 2: Translation of arithmetic Operations

This is read, "square has type a -\> a, for every a such that a belongs to class Num (i.e., such that (+), (*), and negate are defined on a)."
We can now write terms such as

	square 3
	square 3.14

and an appropriate type will be derived for each (Int for the first expression, Float for the second).
On the other hand, writing square 'x' will yield a type error at compile time, because Char has not been asserted (via an instance declaration) to be a numeric type.

Finally, if we define the function squares mentioned previously, then the type given in Figure 1 will be inferred.
This type may be read, "squares has the type (a,b,c) -\> (a,b,c) for every a, b, and c such that a, b, and c belong to class Num".
(We write (a,b,c) for the type that is the cartesian product of a, b, and c.)
So squares has one type, not eight.
Terms such as

	squares (1, 2, 3.14)

are legal, and derive an appropriate type.

## 4 Translation

One feature of this form of overloading is that it is possible at compile-time to translate any program containing class and instance declarations to an equivalent program that does not.
The equivalent program will have a valid Hindley/Milner type.

The translation method will be illustrated by means of an example.
Figure 2 shows the translation of the declarations in Figure 1.

For each class declaration we introduce a new type, corresponding to an appropriate "method dictionary" for that class, and functions to access the methods in the dictionary.
In this case, corresponding to the class Num we introduce the type NumD as shown in Figure 2.
The data declaration defines NumD to be a type constructor for a new type.
Values of this type are created using the value constructor NumDict, and have three components of the types shown.
The functions add, mul, and neg take a value of type NumD and return its first, second, and third component, respectively.

Each instance of the class Num is translated into the declaration of a value of type NumD.
Thus, corresponding to the instance Num Int we declare a data structure of type NumD Int, and similarly for Float.

Each term of the form x+y, x\*y, and negate x is now replaced by a corresponding term, as follows:

	x+y      --> add numD x y
	х*у      –—> mul numD x y
	negate x --> neg numD x

where numD is an appropriate dictionary.
How is the appropriate dictionary determined?
By its type.
For example, we have the following translations:

	3 + 3
	  ——> mul numDInt 3 3

	​3.14 + 3.14
	  --> mul numDFloat 3.14 3.14

As an optimisation, it is easy for the compiler to perform beta reductions to transform these into mulInt 3 3 and mulPloat 3.14 3.14, respectively.

If the type of a function contains a class, then this is translated into a dictionary that is passed at runtime.
For example, here is the definition of square with its type

	square   :: Num a => a -> a
	square x =  x * x

This translates to

	square'        :: NumD a -> a -> a
	square' numD x =  mul numD x x

Each application of square must be translated to pass in the appropriate extra parameter:

	square 3
	  --> square' numDInt 3
	square 3.2
	  --> square' numDFloat 3

Finally, the translation of squares is also shown in Figure 2.
Just as there is one type, rather than eight, there is only one translation, rather than eight.
Exponential growth is avoided.

## 5 A further example: equality

This section shows how to define equality using class and instance declarations.
Type classes serve as a straightforward generalisation of the "eqtype variables" used in Standard ML.
Unlike Standard ML, this mechanism allows the user to extend equality over abstract types in a straightforward way.
And, unlike Standard ML, this mechanism can be translated out at compile time, so it places no special demands on the implementor of the run-time system.

	class Eq a where
	  (==) :: a -> a -> bool

	instance Eq Int where
	  (==) = eqInt

	instance Eq Char Where
	  (==) = eqChar

	member            :: Eq a => [a] -> a -> Bool
	member [] y       =  False
	member (x:xs) y   =  (x == y) \/ member xs y

	instance Eq a, Eq b => Eq (a,b) where
	  (u,v) == (x,y)  = (u == x) & (v == y)

	instance Eq a => Eq [a] where
	  [] == []        = True
	  [] == y:ys      = False
	  x:xs == []      = False
	  x:xs == y:ys    = (x == y) & (xs == ys)

	data Set a = MkSet [a]

	instance Eq a => Eq (Set a) where
		MkSet xs == MkSet ys = and (map (member xs) ys)
		                & and (map (member ys) xs)


Figure 3: Definition of equality

The definition is summarised in Figure 3.
We begin by declaring a class, Eq, containing a single operator, (==), and instances Eq Int and Eq Char of this class.

We then define the member function in the usual way, as shown in Figure 3.
The type of member need not be given explicitly, as it can be inferred.
The inferred type is:

	member :: Eq a => [a] -> a -> Bool

This is read "member has type [a] -\> a -\> Bool, for every type a such that a is in class Eq (i.e., such that equality is defined on a)" (This is exactly equivalent to the Standard ML type ''a list-\>''a-\>bool, where ''a is an "eqtype variable".)
We may now write terms such as

	member [1,2,3] 2
	member "Haskell" 'k'

which both evaluate to True.

Next, we give an instance defining equality over pairs.
The first line of this instance reads, "for every a and b such that a is in class Eq and b is in class Eq, the pair (a,b) is also in class Eq."
In other words, "if equality is defined on a and equality is defined on b, then equality is defined On (a,b)."
The instance defines equality on pairs in terms of equality on the two components, in the usual way.

Similarly, it is possible to define equality Over lists.
The first line of this instance reads, "if equality is defined on a, then equality is defined on type list of a'."
We may now write terms such as

	"hello" == "goodbye"
	[[1,2,3], [4,5,6]] == []
	member ["Haskell", "Alonzo"] "Moses"

which all evaluate to False.

The final data declaration defines a new type constructor Set and a new value constructor MkSet.
If a module exports Set but hides MkSet, then outside of the module the representation of Set will not be accessible;
this is the mechanism used in Haskell to define abstract data types.
The final instance defines equality over sets.
The first line of this instance reads, "if equality is defined on a, then equality is defined on type "set of a ."

	data EqD a    = EqDict (a -> a -> Bool)

	eq (EqDict e) = e

	eqDInt        :: EqD Int
	eqDInt        =  EqDict eqInt

	eqDChar       :: EqD Int
	eqDChar       =  EqDict eqChar

	member'                 :: EqD a -> [a] -> a -> Bool
	member' eqDa [] y       =  False
	member' eqDa (x : xs) y =  eq eqDa x y \/ member' eqDa xs y

	eqDPair                 :: (EqD a, EqD b) -> EqD (a,b)
	eqDPair (eqDa, eqDb)    =  EqDict (eqPair (eqDa, eqDb))


	eqPair                            :: (EqD a, EqD b) -> (a,b) -> (a,b) -> Bool
	eqPair (eqDa, eqDb) (x, y) (u, v) =  eq eqDa x u & eq eqDb y v
	
	eqDList                            :: EqD a -> EqD [a]
	eqDList eqDa                       =  EqDict (eqList eqDa)

	eqList :: EqD a -> [a] -> [a] -> Bool
	eqList eqDa [] []          = True
	eqList eqDa [] (y:ys)      = False
	eqList eqDa (x:xs) []      = False
	eqList eqDa (x:xs) (y:ys)  = eq eqDa x y & eq (eqDList eqDa) xs ys

Figure 4: Translation of equality

In this case, sets are represented in terms of lists, and two sets are taken to be equal if every member of the first is a member of the second, and vice-versa.
(The definition uses standard functions map, which applies a function to every element of a list, and and, which returns the conjunction of a list of booleans.)
Because set equality is defined in terms of member, and member uses overloaded equality, it is valid to apply equality to sets of integers, sets of lists of integers, and even sets of sets of integers.

This last example shows how the type class mechanism allows overloaded functions to be defined Over abstract data types in a natural way.
In particular, this provides an improvement Over the treatment of equality provided in Standard ML, or Miranda.

### 5.1 Translation of equality

We now consider how the translation mechanism applies to the equality example.

Figure 4 shows the translation of the declarations in Figure 3.
The first part of the translation introduces nothing new, and is similar to the translation in Section 4.

We begin by defining a dicitionary EqD corresponding to the class Eq.
In this case, the class contains Only one operation, (==), so the dictionary has only One entry.
The selector function eq takes a dictionary of type EqD a and returns the One entry, of type a-\>a-\>Bool.
Corresponding to the instances Eq Int and Eq Char we define two dictionaries of types EqD Int and EqD Char, containing the appropriate equality functions, and the function member is translated to member' in a straightforward way.

Here are three terms and their translations:

	3*4 == 12
	  -->  eq eqDInt (mul numDInt 3 4) 12

	member [1,2,3] 2
	  -->  member' eqDInt [1,2,3] 2

	member "Haskell" 'k'
	  --> member' eqDChar "Haskell" 'k'

The translation of the instance declaration for equality over lists is a little trickier.
Recall that the instance declaration begins

	instance Eq a => Eq [a] where
		...

This states that equality is defined over type [a] if equality is defined Over type a.
Corresponding to this, the instance dictionary for type [a] is parameterised by a dictionary for type a, and so has the type

	eqDList :: EqD a -> EqD [a]

The remainder of the translation is shown in Figure 4, as is the translation for equality over pairs.
Here are three terms and their translations:

	"hello" == "goodbye"
	  --> eq (eqDList eqDChar)
	         "hello"
	         "goodbye"

	[[1,2,3], [4,5,6]] == []
	  --> eq (eqDList (eqDList eqDInt))
	         [[1,2,3], [4,5,6]]
	         []

	member ["Haskell", "Alonzo"] "Moses"
	  --> member' (eqLList eqDChar)
	        ["Haskell", "Alonzo"]
	        "Moses"

As an Optimisation, it is easy for the compiler to perform beta reductions to transform terms of the form eq (eqDList eqD) into eqList eqD, where eqD is any dictionary for equality.
This optimisation may be applied to the first two examples above, and also to the definition of eqList itself in Figure 4.

It is worthwhile to compare the efficiency of this translation technique with polymorphic equality as found in Standard ML, or Miranda.
The individual operations, such as eqInt are slightly more efficient than polymorphic equality, because the type of the argument is known in advance.
On the other hand, operations such as member and eqList must explicitly pass an equality operation around, an overhead that polymorphic equality avoids.
Further experience is needed to asses the trade-off between these costs.

## 6 Subclasses

In the preceeding, Num and Eq were considered as completely separate classes.
If we want to use both numerical and equality operations, then these each appear in the type separately:

	memsq :: Eq a, Num a => [a] -> a -> Bool
	memsq xs x = member xs (square x)

As a practical matter, this seems a bit odd–we would expect every data type that has (+), (*), and negate defined on it to have (==) defined as well; but not the converse.
Thus it seems sensible tO make Num a subclass of Eq.

We can do this as follows:

	class Eq. a => Num a where
	  (+)    :: a -> a -> a
	  (*)    :: a -> a -> a
	  negate :: a -> a

This asserts that a may belong to class Num only if it also belongs to class Eq.
In other words, Num is a subclass of Eq, or, equivalently, Eq is a superclass of Num.
The instance declarations remain the same as before—but the instance declaration Num Int is only valid if there is also an instance declaration Eq Int active within the same scope.

From this it follows that whenever a type contains Num a it must also contain Eq a; therefore as a convenient abbreviation we permit Eq a to be omitted from a type whenever Num a is present.
Thus, for the type of memsa we could now write

	memsq :: Num a => [a] -> a -> Bool

The qualifier Eq a no longer needs to be mentioned, because it is implied by Num a.

In general, each class may have any number of sub or superclasses.
Here is a contrived example:

	class Top a Where
	  fun 1 :: a -> a

	class Top a => Left a where
	  fun2 :: a -> a

	class Top a => Right a where
	  fun3 :: a -> a

	class Left a, Right a => Bottom a
	  where
	  fun4 :: a -> a

The relationships among these types can be diagrammed as follows:

	    Top
	   /   \
	  /     \
	Left   Right
	  \     /
	   \   /
	   Bottom

Although multiple superclasses pose some problems for the usual means of implementing object-oriented languages, they pose no problems for the translation scheme outlined here.
The translation simply assures that the appropriate dictionaries are passed at run-time; no special hashing schemes are required, as in some object-oriented systems.

## 7 Conclusion

It is natural to think of adding assertions to the class declaration, specifying properties that each instance must satisfy:

	class Eq a Where
	  (==) :: a -> a -> Bool
	  % (==) is an equivalence relation

	class Num a Where
		zero, one :: a
		(+), (*) a -> a -> a
		negate a -> a
		% (zero, one, (+), (*), negate)
		%   form a ring

***

It is valid for any proof to rely on these properties, so long as one proves that they hold for each instance declaration. Here the assertions have simply been written as comments; a more sophisticated system could perhaps verify or use such assertions.
This suggests a relation between classes and object-oriented programming of a different sort, since class declarations now begin to resemble object declarations in OBJ [FGJM85].

It is possible to have Overloaded constants, such as zero and one in the above example.
However, unrestricted overloading of constants leads to situations where the overloading cannot be resolved without providing extra type information.
For instance, the expression one \* one is meaningless unless it is used in a context that specifies whether its result is an Int or a Float.
For this reason, we have been careful in this paper to use constants that are not overloaded: 3 has type Int, and 3.14 has type Float.
A more general treatment of constants seems to require coercion between subtypes.

It is reasonable to allow a class to apply to more than one type variable.
For instance, we might have

	class Coerce a b where
	  coerce :: a -> b

	instance Coerce Int Float where
	  coerce = convertIntToFloat

In this case, the assertion Coerce a b might be taken as equivalent to the assertion that a is a subtype of b.
This suggests a relation between this work and work on bounded quantification and on subtypes (see [CW85,
Rey85] for excellent surveys of work in this area, and [Wan87, Car88] for more recent work).

Type classes may be thought of as a kind of bounded quantifier, limiting the types that a type variable may instantiate to.
But unlike other approaches to bounded quantification, type classes do not introduce any implicit coercions
(such as from subtype Int to supertype Float, or from a record with fields x, y, and z to a record
with fields x and y).
Further exploration of the relationship between type classes and these other approaches is likely to be fruitful.

Type classes also may be thought of as a kind of abstract data type.
Each type class specifies a collection of functions and their types, but not how they are to be implemented.
In a way, each type class corresponds to an abstract data type with many implementations, one for each instance declaration.
Again, exploration of the relationship between type classes and current work on abstract data types [CW85,
MP85, Rey85] appears to be called for.

We have already referred to the work of Kaes.
One advance of our work over his is the conceptual and notational benefit of grouping Overloaded functions into classes.
In addition, our system is more general; Kaes cannot handle overloadings involving more than one type variable, such as the coerce example above.
Finally, our translation rules are an improvement over his.
Kaes outlines two sets of translation rules (which he calls "semantics"), one static and one dynamic.
His dynamic semantics is more limited in power than the language described here; his static semantics appears similar in power, but, unlike the translation described here, can greatly increase the size of a program.

One drawback of our translation method is that it introduces new parameters to be passed at runtime, corresponding to method dictionaries.
It may be possible to eliminate some of these costs by using partial evaluation [BEJ88] to generate versions of functions specialised for certain dictionaries; this would reduce run time at the cost of increasing code size.
Further work is needed to assess the trade-offs between our approach (with or without partial evaluation) and other techniques.

It is clear from the above that many issues remain to be explored, and many tradeoffs remain to be assessed.
We look forward to the practical experience with type classes that Haskell will provide.

Acknowledgements.
The important idea that Overloading might be reflected in the type of a function was suggested (in a rather different form) by Joe Fasel.
For discussion and comments, we are also grateful to: Luca Cardelli, Bob Harper, Paul Hudak, John Hughes, Stefan Kaes, John Launchbury, John Mitchell, Kevin Mitchell, Nick Rothwell, Mads Tofte, David Watt, the members of the Haskell committee, and the members of IFIP 2.8.

## A Typing and translation rules

This appendix presents the formal typing and translation rules, One set of rules performing both typing and translation.
The rules are an extension of those given by Damas and Milner [DM82].

この付録は形式的な型付けと変換規則を示し、規則の1セットがタイピングと変換を実行します。
規則は、ダマとミルナー［DM82］によって与えられたものの拡張です。

### A.1 Language

To present the typing and translation rules for overloading, it is helpful to use a slightly simpler language that captures the essential issues.
We will use a language with the usual constructs (identifiers, applications, lambda abstractions, and let expressions), plus two new constructs, over and inst expressions, that correspond to class and instance declarations, respectively.
The syntax of expressions and types is given in Figure 5.

タイピングとオーバーローディングに対する変換規則を提示するために、重要な問題を捕えるわずかにより単純な言語を使用することは、役に立ちます。
我々はもう一度、2つの新しい構成概念をプラスして、普通の構成概念（識別子、アプリケーション、ラムダ抽象概念とされた表現）で言語を使用します、そして、それぞれ、overとinst式はクラスと例宣言ととても一致します。
式と型の構文は、図5の中で与えられます。

An over expression

overの式

	over x :: σ in e

declares a to be an Overloaded identifier.
Within the scope of this declaration, there may be one or more corresponding inst expressions

宣言のために、オーバーロードされた識別子であってください。
この宣言の範囲内で、一つ以上の対応するinst式があるかもしれません

	inst x :: σ' = e0 in e1

where the type σ' is an instance of the type σ (a notion to be made precise later).
Unlike lambda and let expressions, the bound variables in over and inst expressions may not be redeclared in a smaller scope.
Also unlike lambda and let expressions, over and inst expressions must contain explicit types; the types in other expressions will be inferred by the rules given here.

ここで、型σ'は、型σ（後で正確になる概念）の実体(instance)です。
ラムダとlet式とは異なり、overとinst式中の束縛変数は、より小さい範囲で再宣言されないかもしれません。
また、ラムダとlet式とは異なり、overとinst式は明示的な型を含まなければなりません;他の式の型は、ここで与えられる規則によって推論されます。

As an example, a portion of the definition of equality given in Figure 3 is shown in Figure 6.
In this figure, and in the rest of this appendix, we use Eq τ as an abbreviation for the type τ -> τ -> Bool.

例えば、図3の中で与えられる等価式の一部の定義は、図6に示されます。
この図と残りのこの付録において、我々は型τ -> τ -> Boolの省略形として、Eq τを使います。

As a second example, a portion of the definition of arithmetic operators given in Figure 1 is shown in Figure 7.
In this figure we use Num τ as an abbreviation for the type

第2の例として、図1の中で与えられる算術演算子の一部の定義は、図7に示されます。
この数字では、我々はタイプの省略形として、Num τを使います。

	(τ -> τ -> τ, τ -> τ -> τ, τ -> τ)

In translating to the formal language, we have grouped the three operators together into a "dictionary".
This is straightforward, and independent of the central issue: how to resolve overloading.

形式言語に変換する際に、我々は「辞書」に3人のオペレーターを集めました。
これはまっすぐで、中心問題から独立しています：どのように、オーバーローディングを分解するために。

### A.2 Types

The Damas/Milner system distinguishes between types (written τ) and type schemes (written σ).
Our system adds a third syntactic group, predicated types.
The syntax of these is given in Figure 5.

ダマ/ミルナー・システムは（τと書かれた）型と（σと書かれた)型スキームを区別します。
我々のシステムは、第3の統語的なグループを加えると、タイプは断定しました。
これらの構文は、図5の中で与えられます。

In the full language, we wrote types such as
フルスペックの言語では、我々は型を以下のように書きました

	member :: Eq a => [a] -> a -> Bool

In the simplified language, we write this in the form
簡易な言語では、我々はこれを以下のように書きます

	member :: ∀α. (eq :: Eq α). [α] -> α -> Bool

The restriction Eq a can be read "equality is defined on type a" and the corresponding restriction (eq :: Eq α) can be read "eq must have an instance of type Eq α".

規制Eq、缶は「平等は、タイプ上で定められます、そして、対応する規制（eq：Eqα）「eqは、タイプEq αの例を持っていなければなりません」を読んで聞かせられることができる」を読んで聞かせられます。

In general, we refer to (x :: τ).ρ as a predicated type and (x :: τ) as a predicate.

一般に、我々は照会します（x：τ）.p 意味されたタイプとして、そして、（x ：τ）述部として。

We will give rules for deriving typings of the form

我々は、形のtypingsを引き出すことに対する規則を与えます

	Α |- e :: σ \ e^

This can be read as, "under the set of assumptions A, the expression e has well-typing a with translation e^".
Each typing also includes a translation, so the rules derive typing\\translation pairs.
It is possible to present the typing rules without reference to the translation, simply by deleting the '\\e^' portion from all rules.
It is not, however, possible to present the translation rules independently, since typing controls the translation.
For example, the introduction and elimination of predicates in types controls the introduction and elimination of lambda abstractions in translations.

これは、読まれることができます、「奪取Aのセットの下で、表現eは健康なタイピングをしますで変換e^」。
各々のタイピングも変換を含むので、規則はタイピング変換組を引き出します。
単にすべての規則から『\e^』部を削除することによって、変換に関係なくタイピング規則を提示することが、できます。
タイピングが変換をコントロールする時から、しかし、独立して変換規則を提示することができません。
たとえば、タイプの述部の導入と除去は、変換でラムダ抽象概念の導入と除去をコントロールします。

	Identifiers       x
	Expressions       e ::= x
	                      | e0 e1
	                      | λx. e
	                      | let a = e0 in e1
	                      | over x :: σ in e
	                      | inst x :: σ = e0 in e1
	Type variables    α
	Type Constructors χ
	Тypes             τ ::= (τ -> τ') | α | χ(τ1 ... τn)
	Predicated Types  ρ ::= (x :: τ).ρ | τ
	Type-schemes      σ ::= ∀α.σ | ρ

Figure 5: Syntax of expressions and types

図5：式と型の構文

	over eq :: ∀α. Eq α in
	inst eq :: Eq Int = eqInt in
	inst eq :: Eq Char = eqChar in
	inst eq :: ∀α.∀β.(eq :: Eq a).(eq :: Eq β). Eq (α, β)
	         = λp. λq. eq (fst p) (fst q) ∧ eq (snd p) (snd q) in
	eq (1, 'a') (2, 'b')

Figure 6: Definition of equality, formalised

図6：平等（正式にされる）の定義

	over numD :: ∀α. Num α in
	inst numD :: Num Int = (addInt, mulInt, negInt) in
	inst numD :: Num Float = (addFloat, mulFloat, negFloat) in
	let (+) = fst numD in
	let (*) = snd numD in
	let negate = thd numD in
	let square = λx. x * x in
	square 3

Figure 7: Definition of arithmetic Operations, formalised

図7：算術演算（正式にされる）の定義

	(eq ::o ∀α.Eq α),
	(eq ::i Eq Int  \ eq(Eq Int )),
	(eq ::i Eq Char \ eq(Eq Char)),
	(eq ::i ∀α.∀β.(eq :: Eq α).(eq :: Eq β).Eq (α, β) \ eq(∀α.∀β.(eq::Eq α).(eq::Eq β).Eq (α,β)) ),
	(eq :: Eq α \ eq (Eq α)),
	(eq :: Eq β \ eq (Eq β)),
	(p :: (α, β) \ p),
	(q :: (α, β) \ q)

Figure 8: Some assumptions

図8：いくらかの仮定

	TAUT A, (x :: σ \ x^) |- x :: σ \ x^

	TAUT A, (x ::i σ \ x^) |- x :: σ \ x^

	     A |- e :: ∀α. σ \ e^
	SPEC -----------------------------
	     A |- e :: [α \ τ]σ \ e^

	     A |- e :: σ \ e^
	     α not free in A
	GEN  -----------------------------
	     A |- e :: ∀α. σ \ e^

	     A |- e :: (τ' -> τ) \ e^
	     A |- e':: τ' \ e^'
	COMB -----------------------------
	     A |- (e e') :: τ \ (e^ e^')

	     Ax, (x :: τ' \ x) |- e :: τ \ e^
	ABS  ---------------------------------
	     A |- (λx.e) :: (τ' -> τ) \ (λx.e)

	     A |- e :: σ \ e^
	     Ax, (x :: σ \ x) |- e' :: τ \ e^'
	LET  -----------------------------------------------
	     A |- (let x = e in e') :: τ \ (let x = e^ in e^' )

Figure 9: Typing and translation rules, part 1

図9：タイピングと変換規則（一部1）

## A.3 Assumptions

A.3仮定

Typing is done in the context of a set of assumptions, A.
The assumptions bind typing and translation information to the free identifiers in an expression.
This includes identifiers bound in lambda and let expression, and overloaded identifiers.
Although we write them as sequences, assumptions are sets, and therefore the order is irrelevant.

タイピングは、一組の仮定、仮定がタイプすることを結びつけるA.と変換情報の前後関係で、expressionで無料の識別子にされます。これはラムダで結びつけられる識別子を含んで、表現として、識別子にオーバーロードしました。我々がシーケンスとして彼らに手紙を書くが、仮定はセットです、したがって、勲位は無関係です。

There are three forms of binding in an assumption list:

仮定リストで結合することの3つの形が、あります：

- (x ::o σ) is used for overloaded identifiers;
- (x ::o σ)はオーバーロードされた識別子のために使われます;
- (x ::i σ \ xσ) is used for declared instances of Overloaded identifiers; and
- (x ::i σ \ xσ)はオーバーロードされた識別子の宣言された実装のために使われます;そして、
- (x :: σ \ x^) is used for lambda and let bound variables, and assumed instances of overloaded identifiers.
- (x :: σ \ x^)はラムダとlet束縛変数のために使われて、オーバーロードされた識別子の実装とみなされます。


In (x :: σ \ x^) and (x ::i σ \ x^), the identifier x^ is the translation of x.
If x is not an overloaded identifier (that is, if x is bound by a lambda or let expression), then the assumption for a has the form (x :: σ \ x), so x simply translates as itself.

(x :: σ \ x^) および (x ::i σ \ x^)で、識別子のx^はxを変換したものです。
xがオーバーロードされた識別子でない場合(つまり、xはラムダやlet式によって束縛させている場合)、単にそれ自体として変換し、Xので、そのための前提は、フォーム（X::σ\ X）を持っています。


Figure 8 shows the assumptions available when applying the inference rules to the expression

図6に式

	λp. λq. eq (fst p) (fstq) ∧ eq (snd p) (sndq)

in Figure 6.

に推論規則を適用すると、図8は、仮定が利用可能を示しています。

There are three (::i) bindings, corresponding to the three instance declarations, and two (::) bindings for the two bound variables, and two (::) bindings corresponding to assumed instances of equality.
(We shall see later how assumed instances are introduced by the PRED rule.)

2バインド変数のため:)バインディング、2つ（：平等の想定のインスタンスに対応する:)バインディング3インスタンス宣言に対応する3つ（:: I）バインディング、および2つは（あります。
（私たちは、どのように想定インスタンスをPREDルールによって導入されたかを後で参照しなければなりません。）

## A.4 Instances

Given a set of assumptions A, we define an instance relation between type-schemes,

仮定Aのセットを考えると、私たちは型のスキーム間のインスタンスの関係を定義し、

	σ >-Α σ'.

This can be read as "σ is more general than σ' under assumptions A".
This is the same as the relationship defined by Damas and Milner, but extended to apply to predicated types.

これは、「σは仮定Aの下の「σ'よりも一般的である」と読むことができます。
これはダマとミルナーによって定義された関係と同じであり、前提タイプに適用するために拡張さ購入。

Only certain sets of assumptions are valid.
The definition of validity depends on the >-A relation, so there is a (well-founded) mutual recursion between the definition of valid assumptions and the definition of >-A.
We give the definition of >-A in this section, and the definition of valid assumptions in the next.

仮定の唯一の特定のセットが有効です。
妥当性の定義は>-Aの関係に依存するため、有効な前提条件の定義と>-Aの定義との（十分な根拠）相互再帰があります。
私たちは、このセクションの>-Aの定義、および次の有効な仮定の定義を与えます。

The instance relation

インスタンスの関係

	σ >-Α σ'

where σ = ∀α1 ... αn.ρ and σ' = ∀β1 ... βm . ρ', is defined as follows:
ここでσ = ∀α1 ... αn.ρとσ' = ∀β1 ... βm . ρ'は次のように定義されています。

	σ >-A σ' iff
	  (1) βi is not free in σ and
	  (2) ∃τ1, ..., τn. [τ1/α1, ..., τn/αn]ρ >-Α ρ'

This part is similar to the definition in Damas/Milner.
The bound variables of σ are specialised and the resulting predicated types are compared.

この部分はダマス/ミルナーにおける定義と同様です。
σのバインド変数は、特殊化され、結果の述語の種類が比較されます。

Define ρ >- A ρ' iff the type part of ρ equals the type part of ρ' (the same condition as Damas/Milner), and for every predicate (x :: τ) in ρ, either

いずれか、ρで（X::τ）ρ（ダマ/ミルナーと同じ条件）ρ 'のタイプの一部オフρのタイプの一部に等しい」、およびすべての述語のために - ρ>が定義

- there is a predicate of the form (x :: τ) in ρ' (i.e. the predicate appears in both types); or

- the predicate can be eliminated under assumptions A.

- フォームの述語が（X::τ）ρで '（すなわち述語は両方のタイプに表示される）があります。または

- 述語は仮定Aの下で除去することができます

A predicate (x :: τ) can be eliminated under A iff either

述語(x::τ)は、以下のいずれかで除去することができます

- (x :: τ \ x^) is in A; or

- (x ::i σ' \ x^) is in A and σ' > A τ.

- (x :: τ \ x^)がAにある、または

- (x::i σ' \ x^)がAにあり、かつ、σ' > A τ。

For example, if A0 is the set of assumptions in Figure 8, then

例えば、A0は、図8の仮定の集合は、その後、あります


	(∀α. (eq :: Eq α). [a] -> α -> Bool)
		>-A0 ([Int] -> Int –> Bool)

holds. On the other hand,

保持しています。 一方、

	(∀α. (eq :: Eq α). [α] –> α –> Bool)
	      >-A0 ([Float] –> Float –> Bool)

does not hold, since Ao contains no binding asserting that eq has an instance at type Float.

AOは何がその式をアサートすると、Float型でインスタンスを持って結合が含まれないため、保持していません。

Two type-schemes are unifiable if they overlap, that is, if there exists a type that is an instance of both under some set of assumptions.
We say that σ and σ' are unifiable if there exists a type τ and valid set of assumptions A such that

それらが重複する場合の仮定のいくつかのセットの下で両方のインスタンスがあるタイプが存在する場合、2つのタイプの方式は、すなわち、単一化可能です。
私たちは、そのような仮定の型τおよび有効なセットが存在する場合σとσ 'が単一化可能であると言います。

	σ >-Α τ Λ σ' >-Α τ

We write σ # σ' if σ and σ' are not unifiable.

私たちは「σ場合、σ'σ＃のσを書く単一化可能ではありません。

	     Α, (x :: τ \ xτ) |- e :: ρ \ e^
	PRED ---------------------------------- (x ::ο σ) ∈ Α
	     Α |- e :: (x :: τ).ρ \ (λxτ. e^)

	     Α |- e :: (x :: τ). ρ \ e^
	     Α |- x :: τ \ τ' e'^
	REL  ------------------------------------ (x ::ο σ) ∈ Α
	     A |- e :: ρ \ (e^ e'^)

	     Αx, (r ::ο σ) |- e :: τ \ e^
	OVER ---------------------------------
	     A |- (over x :: σ in e) :: τ \ e^

	     Α, (x ::i σ' \ xσ') |- e' :: σ' \ e'^
	     Α, (x ::i σ' \ xσ') |- e  :: τ  \ e^
	INST ----------------------------------------------------------- (x ::ο σ) ∈ Α
	     A |- (inst x :: σ' = e' in e) :: τ \ (let xσ' = e'^ in e^)

Figure 10: Typing and translation rules, part 2

	let eq(Eq Int) = eqInt in
	let eq(Eq Char) = eqChar in
	let eq(∀α.∀β.(eq::Eq α).(eq::Eq β). Eq(α, β)) 
	    =  λeq(Eq α).λeq(Εq β).λp.λq.
		    eq(Εq α) (fst р) (fst q) /\ eq(Eq β) (snd р) (snd q) in
	eq(∀σ.∀β.(eq::Eq a).(eq::Eq β). Eq (α,β)) eq(Eq Int) eq(Eq Char) (1, 'a') (2, 'b')

Figure 11: Translation of equality, formalised

	A1 : (eq ::o ∀α.Eq α)
	     (eqInt :: Eq Int \ eqInt)
	     (eqChar :: Eq Int \ eqChar)

	e1 : inst eq :: Eq Int = eqľnt in
	     inst eq :: Eq Char = eqChar in
	     eq

Figure 12: A problematic expression

## A.5 Valid assumptions

All sets of assumptions used within proofs must be valid.
The valid sets of assumptions are inductively defined as follows:

証明内で使用される仮定のすべてのセットは有効でなければなりません。
次のような仮定の有効なセットは、誘導定義されています。

- Empty. The empty assumption set, {}, is valid.

- 空。空の仮定の集合は、{}、有効です。

- Normal identifier. If A is a valid assumption set,
	x is an identifier that does not appear in A, and
	σ is a type scheme, then

		Α, (x :: σ \ x)

	is a valid assumption set.

- 通常の識別子。 Aは有効な仮定のセットがある場合には、
	xは Aに表示されていない識別子であり、
	σは型スキームであり、その後、

		Α, (x :: σ \ x)

	は有効な仮定のセットです。

- Overloaded identifier.
	If A is a valid assumption set, x is an identifier that does not appear in A,
	σ is a type scheme, and τ , ..., τm are types and
	σ1, ..., σn are types schemes such that

	– σ >-A 4 σ'i, for i from 1 to n, and
	— σ >-A τi, for i from 1 to m, and
	— σi # σj, for distinct i, j from 1 to n

	then

		Α, (r ::ο σ),
		(x ::i σ1 \ xσ1), ..., (x ::i σn \ xσn) ,
		(x ::  τ1 \ xτ1), ..., (x ::  τm \ xτm) 

	is a valid assumption set.

- オーバーロード識別子。

	Aは有効な仮定のセットがある場合、xはAに表示されていない識別子は、あります
	σは型スキームであり、τは、...、種類τmをされ、
	σ1は、...、その種類のスキームは、ΣNあります

	- σ>-A4Σ'I、iについて1からnまでの、および
	- σ>-Aτiと、iについて1からmまで、および
	- 個別のiに対するΣI＃1ΣJ、1からnまでのJ

	その後

		Α, (r ::ο σ),
		(x ::i σ1 \ xσ1), ..., (x ::i σn \ xσn) ,
		(x ::  τ1 \ xτ1), ..., (x ::  τm \ xτm) 

	有効な仮定のセットです。

For example, the assumptions in Figure 8 are a valid set.
However, this set would be invalid if augmented with the binding

例えば、図8の仮定が有効なセットです。
結合で増強ただし、このセットは無効になります

	(eq ::i ∀γ. Eq(Char, γ) \ eq(∀γ. Eq(Char, γ)) )

as this instance overlaps with One already in the set.

このインスタンスがセットにすでに一つと重複するとして。

## A.6 Inference rules
## A.6 推論規則

We now give inference rules that characterise welltypings of the form

現在の形式のウェル型付けを特徴付ける推論規則を与えます

	Α |- e :: σ \ e^

The rules break into two groups, shown in Figures 9 and 10.
The first group is based directly on the Damas/Milner rules (Figure 9).
There are two small differences: translations have been added to each rule in a straightforward way, and there are two TAUT rules instead of one (one rule for (::) bindings and One for (::i) bindings).

ルールは、図9及び図10に示す2つのグループに分割します。
最初のグループは、ダマ/ミルナールール（図9）に直接基づいています。
二つの小さな違いがあります：変換は簡単な方法で、各ルールに追加されたが、その代わりに1（のための（一つのルールの2タウトルールがある：:)バインディングとするための一つの（::ⅰ）バインディングが）。

For example, let A0 be the set of assumptions shown in Figure 8, together with assumptions about the types of integer and character constants.
Then the above rules are sufficient to derive that

たとえば、A0は整数と文字定数の型についての仮定と一緒に、図8に示した仮定の集合とします。
そして、上記の規則はそれを導き出すのに十分です

	A0 |- (eq 1   2  ) :: Bool \ (eq(Eq Int) 1 2)
	A0 |- (eq 'а' 'b') :: Bool \ (eq(Eq Char) 'а' 'b')

That is, these rules alone are sufficient to resolve simple overloading.

つまり、単独でこれらのルールは単純な過負荷を解決するのに十分です。

More complicated uses of overloading require the remaining four rules, shown in Figure 10.
The first two deal with the introduction and elimination of predicates, and the second two deal with the over and inst constructs.

オーバーロードのより複雑な用途は、図10に示した残りの4つのルールが必要です。
導入と述語の排除、及びオーバーとその構築物で第2の契約で最初の二つの契約。

As we have seen, expressions with types that contain classes (that is, expressions with predicated types) are translated to lambda abstractions that require a dictionary to be passed at run-time.
This idea is encapsulated in the PRED ("predicate") and REL ("release") rules.
The PRED and REL rules introduce and eliminate predicates analogously to the way that the GEN and SPEC rules introduce and eliminate bound type variables.
In particular, the PRED rule adds a predicate to a type (and has a lambda expression as its translation) and the REL rule removes a predicate from a type (and has an application as its translation).

私たちはクラスを含むタイプと、式を見てきたように（つまり、述語タイプの表現）は、実行時に渡される辞書を必要とラムダ抽象に変換されます。
このアイデアは、PRED（「述語」）とREL（"リリース"）のルールにカプセル化されます。
PREDとRELのルールが紹介し、GENとSPEC規則が導入し、バインドされた型変数を排除する方法と同様に述語を排除します。
特に、PREDルールは型に述語を追加します（とその変換としてラムダ式を持っている）とRELルールがタイプから述語を削除（およびその変換などのアプリケーションを持っています）。

The OVER rule types over expressions adding the appropriate (::o) binding to the environment, and the INST rule types inst expressions adding the appropriate (::i) binding to the environment.
The validity condition on sets of assumptions ensures that Overloaded identifiers are only instanced at valid types.

適切な（:: O）（:: i）を環境に結合し、INSTのルールタイプは、適切なを追加する式をINST環境への結合を追加し、式の上にオーバールールタイプ。
仮定のセットの有効条件は、オーバーロード識別子が唯一の有効な型でインスタンス化されることを保証します。

Notice that none of the translations contain over or inst expressions, therefore, they contain no overloading.
It is easy to verify that the translations are themselves well-typed in the Hindley/Milner system.

変換のいずれもが、そのため、彼らは何のオーバーロードを含まない、上やインスト式含まれていません注意してください。
これは、変換自体はヒンドリー/ミルナーシステムにおいてよく型付けされていることを検証するのは容易です。

For example, the program in Figure 6 is translated by these rules into the program in Figure 11.
The reader can easily verify that this corresponds to the translation from Figure 3 to Figure 4.
We have thus shown how to formalise the typing and transformation ideas that were presented informally in the body of the paper.

例えば、図6のプログラムは、図11のプログラムにこれらの規則で変換されています。
読者は簡単に、これは図4に、図3からの変換に対応していることを確認することができます。
そこで我々は、紙の本体で非公式に発表された入力と変換アイデアを正式にする方法が示されています。

## A.7 Principal typings

Given A and e, we call σ a principal type scheme for e under A iff

Aとeが与えられたとき、我々はA中のeのためのσの主要な型スキームと呼び

- A |- e :: σ \ e^ ; and

- for every σ', if A |- e :: σ' \ e'^ then σ >-A σ'

- 全てのσ'で, A |- e :: σ' \ e'^ の場合 σ >-A σ'

A key result in the Hindley/Milner system is that every expression e that has a well-typing has a principal type scheme.

ヒンドリー/ミルナーシステム内の重要な結果はよくタイピングを持っているすべての式eが主要なタイプスキームを持っているということです。

We conjecture that for every valid set of assumptions A and every expression e containing no over or inst expressions, if e has a well-typing under A then e has a principal type scheme under A.

eはその後、電子の下でよくタイピングを持っている場合我々は、仮定Aのすべての有効なセットと、すべての式eのために何の上やインスト式含有していないと推測A.下の主要なタイプのスキームを持っています

For example, let A0 be the set of assumptions in Figure 8.
Then the typing

たとえば、A0は図8の仮定の集合とします。
そして、タイピング

	A0 |- eq :: ∀α. Eq a \ eq(Eq alpha)

is principal.
Examples of non-principal typings are

の主要です。
非主要でないタイピングの例としては、

	A0 |- eq :: Eq Int  \ eq(Eq Int)
	A0 |- eq :: Eq Char \ eq(Ep Char)

Each of these is an instance of the principal typing under assumptions A0.

これらのそれぞれは、仮定のA0下の主要型付けのインスタンスです。

The existence of principal types is problematic for expressions that contain over and inst expressions.
For example, let A1 and e1 be the assumption set and expression in Figure 12.
Then it is possible to derive the typings

overとinst式を含む式のために、主要なタイプの存在は、問題を含みます。
たとえば、A1とe1が図12における仮定集合と式とします。
それは型付けを導出することが可能です

	A1 |- e1 :: Eq Int  \ eqInt
	A1 |- e1 :: Eq Char \ eqChar

But there is no principal type!
One possible resolution of this is to require that over and inst declarations have global scope.
It remains an Open question whether there is some less drastic restriction that still ensures the existence of principal types.

しかし、ここに主要型はありません！
これの一つの可能な解像度がオーバーとinstの宣言はグローバルスコープを持っていることを要求することです。
それはまだ主要タイプが存在することを保証するいくつかのより少なく劇的な制約があるかどうか未解決の問題のまま。

## References

[BEJ88] D. Bjørner, A. Ershov, and N.D. Jones, editors, Partial Evaluation and Mired Computation, North-Holland, 1988 (to appear).

[CW85] L. Cardelli and P. Wegner, On understanding types, data abstraction, and polymorphism. Computing Surveys 17, 4, December 1985.

[Car88] L. Cardelli, Structural subtyping and the
notion of power type. In Proceedings of the 15th Annual Symposium on Principles of Programming Languages, San Diego, California, January 1988.

[DM82] L. Damas and R. Milner, Principal type schemes for functional programs. In Proceedings of the 9th Annual Symposium on Principles of Programming Languages, Albuquerque, N.M., January 1982.

[FGJM85] K. Futasagi, J.A. Goguen, J.-P. Jouannaud, and J. Meseguer, Principles of OBJ2. In Proceedings of the 12th Annual Symposium om Principles of Programming Languages, January 1985.

[GR83] A. Goldberg and D. Robson, Smalltalk80: The Language and Its Implementation. Addison-Wesley, 1983.

[Hin69] R. Hindley, The principal type scheme of an object in combinatory logic. Trans. Am. Math. Soc. 146, pp. 29—60, December 1969.

[HMM86] R. Harper, D. MacQueen, and R. Milner, Standard ML. Report ECS-LFCS-86-2, Edinburgh University, Computer Science Dept., 1986.

[HMT88] R. Harper, R. Milner, and M. Tofte, The definition of Standard ML, version 2. Report ECS-LFCS-88-62, Edinburgh University, Computer Science Dept., 1988.

[Kae88] S. Kaes, Parametric polymorphism. In Proceedings of the 2'nd Furopean Symposium on Programming, Nancy, France, March 1988, LNCS 300, Springer-Verlag, 1988.

[Mil78] R. Milner, A theory of type polymorphism in programming. J. Comput. Syst.
Sci. 17, pp. 348–375, 1978.

[Mil84] R. Milner, A proposal for Standard ML. In Proceedings of the Symposium on Lisp and Functional Programming, Austin, Texas, August 1984.

[Mil87] R. Milner, Changes to the Standard ML core language. Report FCS-LFCS-87-33, Edinburgh University, Computer Science Dept., 1987.

[MP85] J. C. Mitchell and G. D. Plotkin, Abstract types have existential type. In Proceedings of the 12th Annual Symposium on Principles of Programming Languages, January 1985.

[Rey85] J. C. Reynolds, Three approaches to type structure. In Mathematical Foundations of Software Development, LNCS 185, Springer-Verlag, 1985.

|Str67] C. Strachey, Fundamental concepts in programming languages. Lecture notes for International Summer School in Computer Programming, Copenhagen, Allgust 1967.

[Turs85] D. A. Turner, Miranda: A non-strict functional language with polymorphic types. In Proceedings of the 2'nd International Conference on Functional Programming Languages and Computer Architecture, Nancy, France, September 1985. LNCS 201, Springer-Verlag, 1985.

[Wan87] M. Wand, Complete type inference for simple objects. In Proceedings of the Symposium on Logic in Computer Science, Ithaca, NY, June 1987. IEEE Computer Society Press, 1987.
