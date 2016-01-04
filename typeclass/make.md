How to make ad-hoc polymorphism less ad hoc

Philip Wadler and Stephen Blott

University of Glasgow

October 1988

Abstract

This paper presents type classes, a new approach
to ad-hoc polymorphism. Type classes permit over-
loading of arithmetic operators such as multiplica-
tion, and generalise the "eqtype variables" of Stan-
dard ML. Type classes extend the Hindley/Milner
polymorphic type system, and  provide a new ap-
proach to issues that arise in object-oriented pro-
gramming. bounded types. quantication, and ab-
stract data types. This paper provides an informal
introduction to type classes, and defines them for-
mally by means of type inference rules.


1 Introduction

Strachey chose the adjectives ad-hoc and parametric
to distinguish two varieties of polymorphism [Str67].

Ad-hoc polymorphism occurs when a function is
defined over several different types, acting in a dif-
ferent way for each type. A typical example is
overloaded multiplication: the same symbol may be
used to denote multiplication of integers (as in 3*3)
and multiplication of foating point values (as in
3.14*3.14).

Parametric polymorphism o ccurs when a function
is defined over a range of types, acting in the same
way for each type. A typical example is the length
function, which acts in the same way on a list of

integers and a list of floating point numbers.

One widely accepted approach to parametric
polymorphism is the Hindley/Milner type system
[Hin69, Mil78, DM82], which is used in Standard
ML [HMM86, Mil87]. Miranda1[Tur85], and other
languages. On the other hand, there is no widely
accepted approach to ad-hoc polymorphism, and so
its name is doubly appropriate.

This paper presents type classes, which extend the
Hindley/Milner type system to include certain kinds
of overloading, and thus bring together the two sorts
of polymorphism that Strachey separated.

The type system presented here is a generalisa-
tion of the Hindley/Milner type system. As in that
system, type declarations can be inferred, so explicit
type declarations for functions are not required. Dur-
ing the inference process, it is possible to translate a
program using type classes to an equivalent program
that does not use overloading. The translated pro-
grams are typable in the (ungeneralised) Hindley/
Milner type system.

The body of this paper gives an informal introduc-
tion to type classes and the translation rules, while
an appendix gives formal rules for typing and trans-
lation, in the form of inference rules (as in [DM82]).
The translation rules provide a semantics for type

classes. They also provide one possible implementa-
tion technique: if desired, the new system could be
added to an existing language with Hindley/Milner
types simply by writing a pre-processor.

Two places where the issues of ad-hoc polymor-
phism arise are the definition of operators for arith-
metic and equality. Below we examine the ap-
proaches to these three problems adopted by Stan-
dard ML and Miranda; not only do the approaches
differ between￼the two languages, they also differ
within a single language. But as we shall see, type
classes provide a uniform mechanism that can ad-
dress these problems.

-------

\* Authors' address: Department of Computing Science,
University of Glasgow, Glasgow G12 8QQ, Scotland. Elec-
tronic mail: wadler, blott@cs.glasgow.ac.uk.

Published in: 16'th ACM Symposium on Principles of Pro-
gramming Languages, Austin, Texas, January 1989.

Permission to copy without fee all or part of this material is
granted provided that the copies are not made or distributed
for direct commercial advantage, the ACM copyright notice
and the title of the publication and its date appear, and no-
tice is given that copying is by permission of the Association
for Computing Machinery. To copy otherwise, or to republish,
requires a fee and/or specific permission.

----

1 Miranda is a trademark of Research Software Limited.

----















￼
This work grew out of the e􏰔orts of the Haskell committee to design a lazy functional programming language􏰅 􏰍 One of the goals of the Haskell commit􏰀 tee was to adopt 􏰎o􏰔 the shelf 􏰏 solutions to problems
wherever
alise that
no standard solution was available􏰙 Type
were develop ed as an attempt to 􏰑nd a better so􏰀 lution to these problems􏰘 the solution was judged successful enough to be included in the Haskell de􏰀 sign􏰍 However􏰌 type classes should be judged inde􏰀 p endently of Haskell􏰘 they could just as well be in􏰀 corp orated into another language􏰌 such as Standard ML􏰍
arithmetic and equality in Standard ML and Mi􏰀 randa􏰍
Arithmetic􏰍 In the simplest approach to overload􏰀 ing􏰌 basic op erations such as addition and multiplica􏰀 tion are overloaded􏰌 but functions de􏰑ned in terms of them are not􏰍 For example􏰌 although one can write 􏰆􏰥􏰆 and 􏰆􏰍􏰁􏰇􏰥􏰆􏰍􏰁􏰇􏰌 one cannot de􏰑ne
square x 􏰟 x􏰥x
and then write terms such as
square 􏰆 square 􏰆􏰍􏰁􏰇
possible􏰍 We were a little surprised to re􏰀
arithmetic and equality were areas
where classes
Type classes app ear to be
that arise in ob ject􏰀oriented
quanti􏰑cation of types􏰌 and abstract data types 􏰒CW􏰃􏰈􏰌 MP􏰃􏰈􏰌 Rey􏰃􏰈􏰓􏰍 Some of the connections are
outlined below􏰌 but more work is required stand these relations fully􏰍
to under􏰀
b een dis􏰀 􏰒Kae􏰃􏰃􏰓􏰍 Our work improves on Kaes􏰚 in several ways􏰌 notably
A type system very similar to ours has
A more general
equation to stand
loaded versions of square􏰌 with types
and Float 􏰀􏰦 Float􏰍 But consider the function􏰉
covered indep endently by Stefan Kaes
ab ove over􏰀 Int 􏰀􏰦 Int
by the introduction
of type classes to group re􏰀 by providing a better transla􏰀
This paper is
gives an informal introduction to type classes􏰌 while the appendix gives a more formal description􏰍 Sec􏰀 tion 􏰅 motivates the new system by describing limi􏰀 tations of ad􏰀hoc polymorphism as it is used in Stan􏰀
lated op erators􏰌 tion metho d􏰍
and
divided into two
parts􏰉 the body
􏰟 􏰕square
x􏰌
square y􏰌 square z􏰖
dard ML
classes by means of a simple example􏰍 Section 􏰇
and concludes􏰍
Appendix A presents translation􏰍
inference rules
for
􏰅
typing and
closely related to issues programming􏰌 b ounded
and Miranda􏰍 Section 􏰆 introduces type
illustrates
translated
classes􏰍 Section 􏰈 presents a second example􏰌 the def􏰀 inition of an overloaded equality function􏰍 Section 􏰊 describ es sub classes􏰍 Section 􏰋 discusses related work
how the example of Section 􏰆 may be into an equivalent program without type
Limitations of ad-hoc polymorphism
This section motivates our treatment of ad􏰀hoc p oly􏰀 morphism􏰌 by examining problems that arise with
􏰅
committee includes􏰉 Arvind􏰌 Brian Boutel􏰌 John Hughes􏰌 Thomas Johnsson􏰌 Dick Kieburtz􏰌 Simon Peyton Jones􏰌 Rishiyur Nikhil􏰌 Mike Reeve􏰌 Philip Wadler􏰌 David Wise􏰌 and Jonathan
Young􏰍
mits
or a
write 􏰆􏰥􏰇 􏰟􏰟
􏰚a􏰚 􏰟􏰟 􏰚b􏰚 to denote equality over characters􏰍 But
This is the approach taken in
dentally􏰌 it is interesting to note that although Stan􏰀 dard ML includes overloading of arithmetic op era􏰀 tors􏰌 its formal de􏰑nition is delib erately ambiguous ab out how this overloading is resolved 􏰒HMT􏰃􏰃􏰌 page 􏰋􏰁􏰓􏰌 and di􏰔erent versions of Standard ML resolve overloading in di􏰔erent ways􏰍􏰖
squares 􏰕x􏰌 y􏰌 z􏰖
Since
either
ble overloaded versions
there may be exp onential growth in the translations􏰌 and this is one reason why such solu􏰀 tions are not widely used􏰍
In Miranda􏰌 this problem is side􏰀stepp ed by not overloading arithmetic op erations􏰍 Miranda provides only the 􏰗oating p oint type 􏰕named 􏰎num􏰏􏰖􏰌 and there is no way to use the type system to indicate that an op eration is restricted to integers􏰍
Equality􏰍 The history of the equality op eration is checkered􏰉 it has been treated as overloaded􏰌 fully polymorphic􏰌 and partly polymorphic􏰍
The 􏰑rst approach to equality is to make it over􏰀 loaded􏰌 just like multiplication􏰍 In particular􏰌 equal􏰀 ity may be overloaded on every monotype that ad􏰀
i􏰍e􏰍􏰌 does not contain an abstract type type􏰍 In such a language􏰌 one may 􏰁􏰅 to denote equality over integers􏰌 or
each of x􏰌 y􏰌 and type Int or type
z might􏰌 indep endently􏰌 have Float􏰌 there are eight p ossi􏰀
equality􏰌 function
Standard
ML􏰍 􏰕Inci􏰀
approach is to allow the for the de􏰑nition of two
of this function􏰍
In general􏰌 numb er of
￼Jon
de􏰑ne a function member by the equations 􏰟 False
The Haskell
Fairbairn􏰌 Jo e Fasel􏰌 Paul Hudak􏰌
one cannot member 􏰒􏰓 y
􏰅
member
􏰕x􏰉xs􏰖 y
􏰟 􏰕x 􏰟􏰟 y􏰖 􏰎􏰐 member xs y
and then write terms such as
member 􏰒􏰁􏰌􏰅􏰌􏰆􏰓 􏰅 member 􏰏Haskell􏰏 􏰚k􏰚
􏰕We abbreviate a list of characters
as 􏰏abc􏰏􏰍􏰖 This is the approach taken in the 􏰑rst version of Standard ML 􏰒Mil􏰃􏰇􏰓􏰍
A second approach is to make equality fully p oly􏰀 morphic􏰍 In this case􏰌 its type is
􏰕􏰟􏰟􏰖 􏰉􏰉 a 􏰀􏰦 a 􏰀􏰦 Bool
where a is a type variable ranging over every type􏰍 The type of the member function is now
member 􏰉􏰉 􏰒a􏰓 􏰀􏰦
􏰕We write 􏰒a􏰓 for the
that applying equality to functions
do es not generate a type error􏰍 This is the approach taken in Miranda􏰉 if equality is applied on a func􏰀 tion type􏰌 the result is a run􏰀time error􏰘 if equality is
dictionary of appropriate metho ds􏰍 This is exactly the approach used in ob ject􏰀oriented programming 􏰒GR􏰃􏰆􏰓􏰍
In the case of polymorphic equality􏰌 this means
a 􏰀􏰦 Bool
type 􏰎list of a􏰏􏰍􏰖
applied on an
representation for equality􏰍 This last may be consid􏰀 ered a bug􏰌 as it violates the principle of abstraction􏰍
A third approach is to make equality polymorphic in a limited way􏰍 In this case􏰌 its type is
􏰕􏰟􏰟􏰖 􏰉􏰉
abstract type􏰌 the result is to test the
a􏰕􏰟􏰟􏰖 􏰀􏰦 a􏰕􏰟􏰟􏰖 􏰀􏰦 Bool
a􏰕􏰟􏰟􏰖 is a type variable ranging only over
where
types that admit equality􏰍 The type function is now
member 􏰉􏰉 􏰒a􏰕􏰟􏰟􏰖 􏰓 􏰀􏰦 a􏰕􏰟􏰟􏰖 􏰀􏰦
of the
Bool
member
Applying equality􏰌 or member􏰌 on a function abstract type is now a type error􏰍 This is
proach currently taken in Standard ML􏰌 where a􏰕􏰟􏰟􏰖 is written 􏰚􏰚a􏰌 and called an 􏰎eqtype variable􏰏􏰍
Polymorphic equality places certain demands up on
the implementor of the run􏰀time
stance􏰌 in Standard ML reference
for equality di􏰔erently from other
b e possible at run􏰀time to distinguish references other p ointers􏰍
Ob ject􏰀oriented programming􏰍 It would be nice if polymorphic equality could be extended to include user􏰀de􏰑ned equality op erations over abstract types􏰍 To implement this􏰌 we would need to require that every ob ject carry with it a p ointer to a method􏰌 a pro cedure for p erforming the equality test􏰍 If we are to have more than one op eration with this prop erty􏰌 then each object should carry with it a pointer to a
We may now
square x 􏰟
de􏰑ne
x 􏰥 x
􏰒􏰚a􏰚􏰌􏰚b􏰚􏰌􏰚c􏰚􏰓
will they p er􏰀
This means or abstract types
system􏰍 For in􏰀 types are tested types􏰌 so it must from
type constructors􏰌 and a
type or the ap􏰀
􏰆
that both arguments of the equality function contain a p ointer to the same dictionary 􏰕since
are b oth of the same type􏰖􏰍 This suggests that
haps dictionaries should be passed around indep en􏰀 dently of ob jects􏰘 now polymorphic equality would be passed one dictionary and two ob jects 􏰕minus dic􏰀 tionaries􏰖􏰍 This is the intuition behind type classes and the translation metho d describ ed here􏰍
􏰆 An introductory example
We will now introduce type classes by means of an example􏰍
Say that we wish to overload 􏰕􏰡􏰖􏰌 􏰕􏰥􏰖􏰌 and negate 􏰕unary minus􏰖 on types Int and Float􏰍 To do so􏰌 we introduce a new type class􏰌 called Num􏰌 as shown in the class declaration in Figure 􏰁􏰍 This declaration may be read as stating 􏰎a type a belongs to class Num if there are functions named 􏰕􏰡􏰖􏰌 􏰕􏰥􏰖􏰌 and negate􏰌 of the appropriate types􏰌 de􏰑ned on it􏰍􏰏
We may now declare instances of this class􏰌 as shown by the two instance declarations in Figure 􏰁􏰍 The assertion Num Int may be read 􏰎there are func􏰀 tions named 􏰕􏰡􏰖􏰌 􏰕􏰥􏰖􏰌 and negate􏰌 of the appropri􏰀 ate types􏰌 de􏰑ned on Int􏰏􏰍 The instance declaration justi􏰑es this assertion by giving appropriate bindings for the three functions􏰍 The type inference algorithm must verify that these bindings do have the appropri􏰀 ate type􏰌 i􏰍e􏰍􏰌 that addInt has type Int􏰀􏰦Int􏰀􏰦Int􏰌 and similarly for mulInt and negInt􏰍 􏰕We assume that addInt􏰌 mulInt􏰌 and negInt are de􏰑ned in the standard prelude􏰍􏰖 The instance Num Float is de􏰀 clared similarly􏰍
A word on notational conventions􏰉 Type class names and type constructor names begin with a capi􏰀 tal letter􏰌 and type variable names begin with a small letter􏰍 Here􏰌 Num is a type class􏰌 Int and Float are
is a
There exists an
of square from
appendix􏰖􏰍 It derives the type􏰉
square 􏰉􏰉 Num a 􏰟􏰦 a 􏰀􏰦 a
algorithm that
this de􏰑nition 􏰕it is
type
type
variable􏰍
can infer the outlined in the
￼￼￼class Num a 􏰕􏰡􏰖􏰌 􏰕􏰥􏰖
negate
instance Num 􏰕􏰡􏰖 􏰟 􏰕􏰥􏰖 􏰟 negate 􏰟
where
instance Num Float where
􏰕􏰡􏰖 􏰟 􏰕􏰥􏰖 􏰟 negate 􏰟
square squarex
squares
squares 􏰕x􏰌
addFloat
mulFloat
negFloat
􏰉􏰉 􏰉􏰉
 Int
 addInt
 mulInt
 negInt
y􏰌 z􏰖
􏰕a􏰌b􏰌c􏰖 􏰀􏰦 y􏰌 square z􏰖
a 􏰀􏰦 a 􏰀􏰦 a a 􏰀􏰦 a
where
􏰉􏰉 Num a 􏰟􏰦 a 􏰀􏰦 a 􏰟 x􏰥x
􏰟 􏰕square x􏰌 square
􏰕a􏰌b􏰌c􏰖
􏰉􏰉 Num a􏰌 Num b􏰌 Num c 􏰟􏰦
￼Figure 􏰁􏰉 De􏰑nition of arithmetic op erations
￼￼￼data NumD a 􏰟 NumDict 􏰕a 􏰀􏰦 a 􏰀􏰦 a􏰖 􏰕a 􏰀􏰦 a 􏰀􏰦 a􏰖 􏰕a 􏰀􏰦 a􏰖
add􏰕NumDictamn􏰖 􏰟 a mul􏰕NumDictamn􏰖 􏰟 m neg􏰕NumDictamn􏰖 􏰟 n
numDInt 􏰉􏰉 numDInt 􏰟
numDFloat 􏰉􏰉 numDFloat 􏰟
square􏰚
square􏰚 numDa
NumD Int
NumDict addInt mulInt
negInt
NumD Float
NumDict addFloat mulFloat negFloat
􏰉􏰉 NumD a 􏰀􏰦 a 􏰀􏰦 a x 􏰟 mul numDa x x
a􏰌 NumD b􏰌 NumD c􏰖 􏰀􏰦 􏰕a􏰌b􏰌c􏰖 􏰀􏰦
􏰟 􏰕square􏰚 numDa x􏰌 square􏰚 numDb y􏰌 square􏰚 numDc z􏰖
squares􏰚 􏰉􏰉 􏰕NumD
squares􏰚 􏰕numDa􏰌 numDb􏰌 numDc􏰖 􏰕x􏰌 y􏰌 z􏰖
􏰕a􏰌b􏰌c􏰖
￼Figure 􏰅􏰉 Translation of arithmetic op erations
􏰇
This is read􏰌 􏰎square has type a 􏰀􏰦 a􏰌 for every a
􏰕􏰥􏰖􏰌 and negate are de􏰑ned write terms such as
square 􏰆 square 􏰆􏰍􏰁􏰇
and an appropriate type will be derived for each 􏰕Int for the 􏰑rst expression􏰌 Float for the second􏰖􏰍 On
the other hand􏰌 writing square
error at compile time􏰌 because Char has not been asserted 􏰕via an instance declaration􏰖 to be a numeric type􏰍
Each term now replaced
x􏰡y
x􏰥y negate x
of the form x􏰡y􏰌 x􏰥y􏰌 and negate x is
such that a belongs to class Num 􏰕i􏰍e􏰍􏰌 such that
by a corresp onding
term􏰌 as
follows􏰉
function squares men􏰀 type given in Figure 􏰁 may be read􏰌 􏰎squares 􏰕a􏰌b􏰌c􏰖 for every a􏰌 b􏰌 c such that a􏰌 b􏰌 and c belong to class Num􏰏􏰍 􏰕We write 􏰕a􏰌b􏰌c􏰖 for the type that is the cartesian
mul numDFloat 􏰆􏰍􏰁􏰇 􏰆􏰍􏰁􏰇
Finally􏰌 if we de􏰑ne the tioned previously􏰌 then the
easy for the
compiler to these into
will has and
b e inferred􏰍 This type the type 􏰕a􏰌b􏰌c􏰖 􏰀􏰦
pro duct of a􏰌 b􏰌 and c􏰍􏰖 So not eight􏰍 Terms such as
squares 􏰕􏰁􏰌 􏰅􏰌 􏰆􏰍􏰁􏰇􏰖
has one
dictionary that is passed at run􏰀 here is the de􏰑nition of square
􏰉􏰉 Num a 􏰟􏰦 a 􏰀􏰦 a 􏰟 x􏰥x
􏰉􏰉 NumD a 􏰀􏰦 a 􏰀􏰦 a
type􏰌
One
is possible at compile􏰀time to translate
gram containing class and instance declarations to an equivalent program that does not􏰍 The equiva􏰀 lent program will have a valid Hindley􏰐Milner type􏰍
The translation metho d will be illustrated by means of an example􏰍 Figure 􏰅 shows the transla􏰀 tion of the declarations in Figure 􏰁􏰍
For each class declaration we introduce a new type􏰌 corresp onding to an appropriate 􏰎metho d dic􏰀 tionary􏰏 for that class􏰌 and functions to access the metho ds in the dictionary􏰍 In this case􏰌 corresp ond􏰀 ing to the class Num we introduce the type NumD as shown in Figure 􏰅􏰍 The data declaration de􏰑nes NumD to be a type constructor for a new type􏰍 Values of this type are created using the value constructor NumDict􏰌 and have three comp onents of the types shown􏰍 The functions add􏰌 mul􏰌 and neg take a value of type NumD and return its 􏰑rst􏰌 second􏰌 and third comp onent􏰌 resp ectively􏰍
Each instance of the class Num is translated into the declaration of a value of type NumD􏰍 Thus􏰌 corre􏰀 sp onding to the instance Num Int we declare a data structure of type NumD Int􏰌 and similarly for Float􏰍
are
􏰇
legal􏰌 and derive an
Translation
appropriate
type􏰍
feature of this form of
overloading is that it any pro􏰀
of square
must be translated to
􏰕􏰡􏰖􏰌 on a􏰖􏰍􏰏 We can now
􏰚x􏰚 will yield a
type
􏰆􏰥􏰆 􏰀􏰀􏰦
squares
􏰈
where numD appropriate
example􏰌 we have the
􏰀􏰀􏰦 add numD x y 􏰀􏰀􏰦 mul numD x y 􏰀􏰀􏰦 neg numD x
an appropriate dictionary􏰍 How is the following translations􏰉
is
dictionary determined􏰛 By its type􏰍 For
mul numDInt 􏰆􏰍􏰁􏰇 􏰥 􏰆􏰍􏰁􏰇
􏰆 􏰆
􏰀􏰀􏰦
As an optimisation􏰌 it is
p erform beta reductions to transform
mulInt 􏰆 􏰆 and mulFloat 􏰆􏰍􏰁􏰇 􏰆􏰍􏰁􏰇􏰌 resp ectively􏰍
If the type of a function contains a class􏰌 then this
is translated into a time􏰍 For example􏰌 with its type
square squarex
This translates to
square􏰚 square􏰚 numD
Each application
pass in the appropriate extra parameter􏰉
square 􏰆
􏰀􏰀􏰦 square􏰚 numDInt 􏰆
square 􏰆􏰍􏰅
􏰀􏰀􏰦 square􏰚 numDFloat 􏰆
x 􏰟 mul
numD x x
Finally􏰌 the translation of squares Figure 􏰅􏰍 Just as there is one type􏰌
in
eight􏰌 there is only one translation􏰌 rather than eight􏰍 Exp onential growth is avoided􏰍
􏰈 A further example􏰉
equality
This section shows how to de􏰑ne equality using class and instance declarations􏰍 Type classes serve as a straightforward generalisation of the 􏰎eqtype vari􏰀 ables􏰏 used in Standard ML􏰍 Unlike Standard ML􏰌 this mechanism allows the user to extend equality over abstract types in a straightforward way􏰍 And􏰌 unlike Standard ML􏰌 this mechanism can be trans􏰀 lated out at compile time􏰌 so it places no special de􏰀 mands on the implementor of the run􏰀time system􏰍
is also
rather than
shown
￼￼￼class  Eq
  􏰕􏰟􏰟􏰖 􏰉􏰉
instance
  􏰕􏰟􏰟􏰖  􏰟
instance
􏰕􏰟􏰟􏰖 􏰟 eqChar
member
member 􏰒􏰓 y member 􏰕x􏰉xs􏰖 y
instance Eq a􏰌 Eq 􏰕u􏰌v􏰖 􏰟􏰟 􏰕x􏰌y􏰖
instance Eq a 􏰒􏰓 􏰟􏰟 􏰒􏰓
􏰟􏰦
Eq
􏰒􏰓 􏰟􏰟 y􏰉ys x􏰉xs 􏰟􏰟 􏰒􏰓 x􏰉xs 􏰟􏰟 y􏰉ys
a where
a 􏰀􏰦 a 􏰀􏰦 bool
Eq Int  where
 eqInt
Eq Char where
b
􏰉􏰉 Eq a 􏰟􏰦 􏰒a􏰓 􏰀􏰦 a 􏰀􏰦 Bool 􏰟 False
􏰟 􏰕x 􏰟􏰟 y􏰖 􏰎􏰐 member xs y
􏰟􏰦 Eq 􏰕a􏰌b􏰖 where
􏰟 􏰕u 􏰟􏰟 x􏰖 􏰧 􏰕v 􏰟􏰟 y􏰖
􏰒a􏰓 where
􏰟 True
􏰟 False
􏰟 False
􏰟 􏰕x􏰟􏰟y􏰖􏰧􏰕xs􏰟􏰟ys􏰖
data Set a
instance Eq
MkSet xs 􏰟􏰟 MkSet ys 􏰟 and 􏰕map 􏰕member xs􏰖 ys􏰖
􏰧 and 􏰕map 􏰕member ys􏰖 xs􏰖
􏰟 MkSet 􏰒a􏰓
a 􏰟􏰦 Eq 􏰕Set a􏰖 where
￼The de􏰑nition is summarised in Figure 􏰆􏰍 We be􏰀 gin by declaring a class􏰌 Eq􏰌 containing a single op􏰀 erator􏰌 􏰕􏰟􏰟􏰖􏰌 and instances Eq Int and Eq Char of this class􏰍
We then de􏰑ne the member function in the usual
a and b such that a is in class Eq and b is in class Eq􏰌 the pair 􏰕a􏰌b􏰖 is also in class Eq􏰍􏰏 In other words􏰌 􏰎if equality is de􏰑ned on a and equality is de􏰑ned on b􏰌 then equality is de􏰑ned on 􏰕a􏰌b􏰖􏰍􏰏 The instance de􏰑nes equality on pairs in terms of equality on the two comp onents􏰌 in the usual way􏰍
Similarly􏰌 it is possible to de􏰑ne equality over lists􏰍 The 􏰑rst line of this instance reads􏰌 􏰎if equality is de􏰑ned on a􏰌 then equality is de􏰑ned on type 􏰜list of a􏰚􏰍􏰏 We may now write terms such as
way􏰌 as shown in Figure not be given explicitly􏰌 inferred type is􏰉
􏰆􏰍 as
The type it can be
of member need
The
This is read 􏰎member has type 􏰒a􏰓 􏰀􏰦 a 􏰀􏰦
for every type a such that a is in class Eq 􏰕i􏰍e􏰍􏰌 such that equality is de􏰑ned on a􏰖􏰏 􏰕This
member 􏰉􏰉 Eq a 􏰟􏰦 􏰒a􏰓 􏰀􏰦 a 􏰀􏰦 Bool
is exactly equivalent to the
Standard ML type
which all evaluate to False􏰍
The 􏰑nal data declaration de􏰑nes a new type con􏰀
􏰚􏰚a list􏰀􏰦􏰚􏰚a􏰀􏰦bool􏰌 variable􏰏􏰍􏰖 We may now
member 􏰒􏰁􏰌􏰅􏰌􏰆􏰓 􏰅
where write
􏰚􏰚a terms
is an 􏰎eqtype
member
􏰏Haskell􏰏 􏰚k􏰚
new value constructor MkSet􏰍 If Set but hides MkSet􏰌 then out􏰀 the representation of Set will not is the mechanism used in Haskell
which b oth evaluate to True􏰍
Next􏰌 we give an instance de􏰑ning equality over
pairs􏰍 The 􏰑rst line of this instance reads􏰌 􏰎for every
such
as
structor Set and a
a mo dule exp orts
side of the mo dule
b e accessible􏰘 this
to de􏰑ne abstract data types􏰍 The 􏰑nal instance de􏰀 􏰑nes equality over sets􏰍 The 􏰑rst line of this instance reads􏰌 􏰎if equality is de􏰑ned on a􏰌 then equality is
Figure 􏰆􏰉 De􏰑nition of equality
inferred􏰍
Bool􏰌
􏰏hello􏰏 􏰟􏰟 􏰏goodbye􏰏 􏰒􏰒􏰁􏰌􏰅􏰌􏰆􏰓􏰌􏰒􏰇􏰌􏰈􏰌􏰊􏰓􏰓 􏰟􏰟 􏰒􏰓 member 􏰒􏰏Haskell􏰏􏰌 􏰏Alonzo􏰏􏰓
􏰊
􏰏Moses􏰏
￼￼￼data EqD a
eq 􏰕EqDict e􏰖
eqDInt
eqDInt
eqDChar
eqDChar
member􏰚
member􏰚 eqDa
member􏰚 eqDa
􏰟 EqDict 􏰕a 􏰀􏰦 a 􏰀􏰦 Bool􏰖
􏰟 e
􏰉􏰉 EqD Int
􏰟 EqDict eqInt
􏰉􏰉 EqD Int
eqDPair
eqDPair 􏰕eqDa􏰌eqDb􏰖
􏰟 EqDict
eqChar
􏰒􏰓 􏰕x􏰉xs􏰖
􏰉􏰉 EqDa􏰀􏰦􏰒a􏰓􏰀􏰦a􏰀􏰦Bool
􏰟 False
􏰟 eqeqDaxy􏰎􏰐member􏰚eqDaxsy
􏰉􏰉 􏰕EqD a􏰌 EqD b􏰖 􏰀􏰦 EqD 􏰕a􏰌b􏰖 􏰟 EqDict 􏰕eqPair 􏰕eqDa􏰌eqDb􏰖􏰖
􏰉􏰉 􏰕EqD a􏰌 EqD b􏰖 􏰀􏰦 􏰕a􏰌b􏰖 􏰀􏰦 􏰕a􏰌b􏰖 􏰀􏰦 Bool 􏰟 eq eqDa x u 􏰧 eq eqDb y v
􏰉􏰉 EqDa􏰀􏰦EqD􏰒a􏰓
􏰟 EqDict 􏰕eqList eqDa􏰖
􏰉􏰉 EqDa􏰀􏰦􏰒a􏰓􏰀􏰦􏰒a􏰓􏰀􏰦Bool
􏰟 True
􏰟 False
􏰟 False
􏰟 eq eqDa x y 􏰧 eq 􏰕eqDList eqDa􏰖 xs ys
eqPair
eqPair 􏰕eqDa􏰌eqDb􏰖 􏰕x􏰌y􏰖 􏰕u􏰌v􏰖
eqDList
eqDList eqDa
eqList
eqList eqDa 􏰒􏰓 􏰒􏰓
eqList eqDa 􏰒􏰓 􏰕y􏰉ys􏰖 eqList eqDa 􏰕x􏰉xs􏰖 􏰒􏰓 eqList eqDa 􏰕x􏰉xs􏰖 􏰕y􏰉ys􏰖
y
y
￼are rep􏰀 taken to the 􏰑rst is a memb er of the second􏰌 and vice􏰀versa􏰍 􏰕The de􏰑nition uses standard functions map􏰌 which applies a function to every element of a list􏰌 and and􏰌 which returns the conjunction of a list of b o oleans􏰍􏰖 Because set equal􏰀 ity is de􏰑ned in terms of member􏰌 and member uses overloaded equality􏰌 it is valid to apply equality to sets of integers􏰌 sets of lists of integers􏰌 and even sets
of sets of integers􏰍
This last example shows how the type class mech􏰀
anism allows overloaded functions to be de􏰑ned over abstract data types in a natural way􏰍 In particular􏰌 this provides an improvement over the treatment of
de􏰑ned on type 􏰜set of a􏰚􏰍􏰏 In this case􏰌 sets
in Figure 􏰆􏰍 The 􏰑rst part of the translation intro􏰀 duces nothing new􏰌 and is similar to the translation in Section 􏰇􏰍
We begin by de􏰑ning a dicitionary EqD corresp ond􏰀 ing to the class Eq􏰍 In this case􏰌 the class contains only one op eration􏰌 􏰕􏰟􏰟􏰖􏰌 so the dictionary has only one entry􏰍 The selector function eq takes a dictio􏰀 nary of type EqD a and returns the one entry􏰌 of type a􏰀􏰦a􏰀􏰦Bool􏰍 Corresp onding to the instances Eq Int and Eq Char we de􏰑ne two dictionaries of types EqD Int and EqD Char􏰌 containing the appro􏰀 priate equality functions􏰌 and the function member is translated to member􏰚 in a straightforward way􏰍 Here are three terms and their translations􏰉
resented in terms of lists􏰌 and be equal if every memb er of
two sets are
equality provided in Standard ML or
􏰈􏰍􏰁 Translation of equality
Miranda􏰍
numDInt 􏰆
􏰒􏰁􏰌􏰅􏰌􏰆􏰓 􏰅
􏰇􏰖 􏰁􏰅
We now consider how the translation mechanism ap􏰀 plies to the equality example􏰍
Figure 􏰇 shows the translation of the declarations
member 􏰏Haskell􏰏 􏰚k􏰚
􏰀􏰀􏰦 member􏰚 eqDChar 􏰏Haskell􏰏 􏰚k􏰚
The translation of the instance declaration for
Figure 􏰇􏰉 Translation of equality
􏰋
􏰆􏰥􏰇 􏰟􏰟
􏰀􏰀􏰦 eq eqDInt 􏰕mul
member 􏰒􏰁􏰌􏰅􏰌􏰆􏰓 􏰅
􏰀􏰀􏰦 member􏰚 eqDInt
􏰁􏰅
equality over lists is a little trickier􏰍 Recall that instance declaration begins
instance Eq a 􏰟􏰦 Eq 􏰒a􏰓 where 􏰍􏰍􏰍
This states
the
numerical and equality op erations􏰌 then these each app ear in the type separately􏰉
memsq 􏰉􏰉 Eq a􏰌 Num a 􏰟􏰦 􏰒a􏰓􏰀􏰦a􏰀􏰦Bool memsq xs x 􏰟 member xs 􏰕square x􏰖
As a practical matter􏰌 this seems a bit o dd􏰞we would exp ect every data type that has 􏰕􏰡􏰖􏰌 􏰕􏰥􏰖􏰌 and negate de􏰑ned on it to have 􏰕􏰟􏰟􏰖 de􏰑ned as well􏰘 but not the converse􏰍 Thus it seems sensible to make Num a subclass of Eq􏰍
We can do this as follows􏰉
equality this􏰌 the eterised by type
de􏰑ned over
a dictionary for type a􏰌 and so has the
􏰀􏰀􏰦 eq
􏰕eqDList eqDChar􏰖 􏰏hello􏰏
􏰏goodbye􏰏
that equality is de􏰑ned over
􏰒􏰒􏰁􏰌􏰅􏰌􏰆􏰓􏰌􏰒􏰇􏰌􏰈􏰌􏰊􏰓􏰓 􏰟􏰟
􏰀􏰀􏰦 eq 􏰕eqDList 􏰕eqDList 􏰒􏰒􏰁􏰌􏰅􏰌􏰆􏰓􏰌􏰒􏰇􏰌􏰈􏰌􏰊􏰓􏰓
􏰒􏰓
type
type a􏰍 Corresp onding
􏰒a􏰓 if to instance dictionary for type 􏰒a􏰓 is param􏰀
is
eqDList 􏰉􏰉 EqD a 􏰀􏰦 EqD 􏰒a􏰓
The remainder of the
􏰇􏰌 as is the translation for equality
are three terms and their translations􏰉
􏰏hello􏰏 􏰟􏰟 􏰏goodbye􏰏
class Eq a
􏰕􏰡􏰖 􏰉􏰉
  􏰕􏰥􏰖    􏰉􏰉
  negate 􏰉􏰉
􏰟􏰦 Num a where a 􏰀􏰦 a 􏰀􏰦 a
a 􏰀􏰦 a 􏰀􏰦 a
a 􏰀􏰦 a
translation is
shown in
over
eqDInt􏰖􏰖
member 􏰒􏰏Haskell􏰏􏰌 􏰏Alonzo􏰏􏰓
􏰀􏰀􏰦 member􏰚 􏰕eqDList eqDChar􏰖
􏰒􏰏Haskell􏰏􏰌 􏰏Alonzo􏰏􏰓 􏰏Moses􏰏
As an optimisation􏰌 it is easy for the compiler to p er􏰀 form beta reductions to transform terms of the form eq 􏰕eqDList eqD􏰖 into eqList eqD􏰌 where eqD is any dictionary for equality􏰍 This optimisation may be applied to the 􏰑rst two examples ab ove􏰌 and also
to
argument is known in advance􏰍 op erations such as member and itly pass an equality op eration
This
it also belongs to class Eq􏰍 In other words􏰌 Num is a
sub class of Eq􏰌 or􏰌 equivalently􏰌 Eq is a sup erclass Num􏰍 The instance declarations remain the same as before􏰞but the instance declaration Num Int is only valid if there is also an instance declaration Eq Int active within the same scop e􏰍
From this it follows that whenever a type contains Num a it must also contain Eq a􏰘 therefore as a con􏰀
the de􏰑nition of eqList itself in Figure 􏰇􏰍
It is worthwhile to compare the e􏰝ciency of this translation technique with polymorphic equality as found in Standard ML or Miranda􏰍 The individual op erations􏰌 such as eqInt are slightly more e􏰝cient than polymorphic equality􏰌 because the type of the
class Top a where fun􏰁 􏰉􏰉 a 􏰀􏰦 a
class Top a 􏰟􏰦 Left a where fun􏰅 􏰉􏰉 a 􏰀􏰦 a
class Top a 􏰟􏰦 Right a where
that ence is costs􏰍
p olymorphic needed to
asses the
􏰊 Sub classes
trade􏰀o􏰔
b etween
these
􏰀􏰦 a
􏰒􏰓
On the eqList
other hand􏰌 must explic􏰀 around􏰌 an overhead equality avoids􏰍 Further exp eri􏰀
fun􏰆 􏰉􏰉 a
class Left where
fun􏰇 􏰉􏰉 a
􏰀􏰦 a
a􏰌 Right a 􏰟􏰦 Bottom a
In the preceeding􏰌 Num and Eq were considered as completely separate classes􏰍 If we want to use b oth
Figure pairs􏰍 Here
􏰏Moses􏰏
􏰃
asserts that a may belong to class Num only if
venient abbreviation we p ermit Eq a to from a type whenever Num a is present􏰍
b e omitted Thus􏰌 for
mentioned􏰌
the type of memsq we
memsq 􏰉􏰉
The quali􏰑er because it is
Eq a no longer needs implied by Num a􏰍
to be
or
In general􏰌 each class may have any numb er of sub sup erclasses􏰍 Here is a contrived example􏰉
could now write Num a 􏰟􏰦 􏰒a􏰓􏰀􏰦a􏰀􏰦Bool
The
grammed as follows􏰉
Top 􏰐􏰎 􏰐􏰎
types can
b e dia􏰀
relationships among these
of
declaration􏰌 sp ecifying must satisfy􏰉
prop erties that
the class each instance
class Eq a where
Left Right 􏰎 􏰐
􏰎􏰐 Bottom
class Coerce a b where coerce 􏰉􏰉 a 􏰀􏰦 b
instance Coerce Int Float where coerce 􏰟 convertIntToFloat
In this case􏰌 the assertion Coerce a b might be taken as equivalent to the assertion that a is a sub􏰀 type of b􏰍 This suggests a relation between this work and work on b ounded quanti􏰑cation and on subtypes 􏰕see 􏰒CW􏰃􏰈􏰌 Rey􏰃􏰈􏰓 for excellent surveys of work in this area􏰌 and 􏰒Wan􏰃􏰋􏰌 Car􏰃􏰃􏰓 for more recent work􏰖􏰍
variable
b e fruitful􏰍
Type classes also may
of
a
how they are to be implemented􏰍
type class corresp onds to an abstract
many implementations􏰌 one for each instance dec􏰀 laration􏰍 Again􏰌 exploration of the relationship be􏰀 tween type classes and current work on abstract data types 􏰒CW􏰃􏰈􏰌 MP􏰃􏰈􏰌 Rey􏰃􏰈􏰓 app ears to be called for􏰍
We have already referred to the work of Kaes􏰍 One advance of our work over his is the conceptual and notational bene􏰑t of grouping overloaded functions into classes􏰍 In addition􏰌 our system is more gen􏰀 eral􏰘 Kaes cannot handle overloadings involving more than one type variable􏰌 such as the coerce example ab ove􏰍 Finally􏰌 our translation rules are an improve􏰀 ment over his􏰍 Kaes outlines two sets of translation rules 􏰕which he calls 􏰎semantics􏰏􏰖􏰌 one static and one dynamic􏰍 His dynamic semantics is more limited in p ower than the language describ ed here􏰘 his static semantics app ears similar in p ower􏰌 but􏰌 unlike the translation described here􏰌 can greatly increase the size of a program􏰍
One drawback of our translation metho d is that it introduces new parameters to be passed at run􏰀 time􏰌 corresponding to method dictionaries􏰍 It may be possible to eliminate some of these costs by us􏰀 ing partial evaluation 􏰒BEJ􏰃􏰃􏰓 to generate versions of functions specialised for certain dictionaries􏰘 this would reduce run time at the cost of increasing co de size􏰍 Further work is needed to assess the trade􏰀o􏰔s
Although
lems for the
oriented languages􏰌 they p ose no problems for the translation scheme outlined here􏰍 The translation simply assures that the appropriate dictionaries are passed at run􏰀time􏰘 no sp ecial hashing schemes are required􏰌 as in some ob ject􏰀oriented systems􏰍
􏰋 Conclusion It is natural to think of
multiple sup erclasses
usual means of implementing ob ject􏰀
􏰕􏰟􏰟􏰖 􏰉􏰉
􏰨 􏰕􏰟􏰟􏰖 is an equivalence
class Num a where
adding assertions to
a 􏰀􏰦 a 􏰀􏰦 Bool
zero􏰌 one 􏰉􏰉
􏰕􏰡􏰖􏰌 􏰕􏰥􏰖 􏰉􏰉
negate 􏰉􏰉
􏰨 􏰕zero􏰌 one􏰌 􏰕􏰡􏰖􏰌 􏰕􏰥􏰖􏰌 􏰨 form a ring
abstract data type􏰍 collection of functions
Each type and their
class sp eci􏰑es types􏰌 but not In a way􏰌 each
a
a 􏰀􏰦 a 􏰀􏰦 a a 􏰀􏰦 a
data type with
It is valid for any pro of to rely on these prop erties􏰌 long as one proves that they hold for each instance declaration􏰍 Here the assertions have simply been written as comments􏰘 a more sophisticated system could p erhaps verify or use such assertions􏰍 This sug􏰀 gests a relation between classes and ob ject􏰀oriented programming of a di􏰔erent sort􏰌 since class declara􏰀 tions now begin to resemble ob ject declarations in OBJ 􏰒FGJM􏰃􏰈􏰓􏰍
It is possible to have overloaded constants􏰌 such as zero and one in the ab ove example􏰍 However􏰌 unre􏰀 stricted overloading of constants leads to situations where the overloading cannot be resolved without providing extra type information􏰍 For instance􏰌 the expression one 􏰥 one is meaningless unless it is used in a context that sp eci􏰑es whether its result is an Int or a Float􏰍 For this reason􏰌 we have been careful in this paper to use constants that are not overloaded􏰉
􏰆 has type Int􏰌 and 􏰆􏰍􏰁􏰇 has general treatment of constants ercion between subtypes􏰍
type Float􏰍 A more seems to require co􏰀
It is reasonable to allow a class to apply to more than one type variable􏰍 For instance􏰌 we might have
p ose
some prob􏰀
relation
b e thought of as a kind
negate􏰖
so
􏰂
classes may be thought of as a kind of quanti􏰑er􏰌 limiting the types that a type may instantiate to􏰍 But unlike other ap􏰀 proaches to b ounded quanti􏰑cation􏰌 type classes do not introduce any implicit co ercions 􏰕such as from subtype Int to sup ertype Float􏰌 or from a record with 􏰑elds x􏰌 y􏰌 and z to a record with 􏰑elds x and y􏰖􏰍 Further exploration of the relationship between type classes and these other approaches is likely to
Type b ounded
b etween our approach 􏰕with or without partial eval􏰀 uation􏰖 and other techniques􏰍
It is clear from the ab ove that many issues remain to be explored􏰌 and many tradeo􏰔s remain to be as􏰀 sessed􏰍 We lo ok forward to the practical exp erience with type classes that Haskell will provide􏰍
types in other expressions will be inferred by the rules given here􏰍
As an example􏰌 a p ortion of the de􏰑nition of equal􏰀 ity given in Figure 􏰆 is shown in Figure 􏰊􏰍 In this 􏰑gure􏰌 and in the rest of this appendix􏰌 we use Eq 􏰣 as an abbreviation for the type 􏰣 􏰙 􏰣 􏰙 Bool 􏰍
As a second example􏰌 a p ortion of the de􏰑nition
Acknowledgements􏰍 The imp ortant overloading might be re􏰗ected in the type
tion was suggested 􏰕in a rather di􏰔erent
Jo e Fasel􏰍 For discussion and comments􏰌 we are also grateful to􏰉 Luca Cardelli􏰌 Bob Harp er􏰌 Paul Hudak􏰌 John Hughes􏰌 Stefan Kaes􏰌 John Launchbury􏰌 John Mitchell􏰌 Kevin Mitchell􏰌 Nick Rothwell􏰌 Mads Tofte􏰌
David Watt􏰌 the memb ers of the
Haskell committee􏰌
we have a 􏰎dictio􏰀 and indep endent of
The Damas􏰐Milner system distinguishes between types 􏰕written 􏰣 􏰖 and type schemes 􏰕written 􏰪 􏰖􏰍 Our system adds a third syntactic group􏰌 predicated types􏰍 The syntax of these is given in Figure 􏰈􏰍
In the full language􏰌 we wrote types such as member 􏰉􏰉 Eq a 􏰟􏰦 􏰒a􏰓 􏰀􏰦 a 􏰀􏰦 Bool
In the simpli􏰑ed language􏰌 we write this in the form
member 􏰉􏰉 􏰃􏰔􏰉 􏰕eq 􏰉􏰉 Eq 􏰔􏰖􏰉 􏰒􏰔􏰓 􏰙 􏰔 􏰙 Bool
The restriction Eq a can be read 􏰎equality is de􏰑ned on type a􏰏 and the corresp onding restriction 􏰕eq 􏰉􏰉 Eq 􏰔􏰖 can be read 􏰎eq must have an instance of type Eq 􏰔􏰏􏰍
In general􏰌 we refer to 􏰕x 􏰉􏰉 􏰣 􏰖􏰉 􏰫 as a predicated type and 􏰕x 􏰉􏰉 􏰣 􏰖 as a predicate􏰍
We will give rules for deriving typings of the form A 􏰜 e 􏰉􏰉 􏰪 n e
This can be read as􏰌 􏰎under the set of assumptions A􏰌 the expression e has well􏰀typing 􏰪 with transla􏰀 tion e􏰏􏰍 Each typing also includes a translation􏰌 so the rules derive typingntranslation pairs􏰍 It is p ossi􏰀 ble to present the typing rules without reference to the translation􏰌 simply by deleting the 􏰜ne􏰚 p ortion from all rules􏰍 It is not􏰌 however􏰌 possible to present the translation rules indep endently􏰌 since typing con􏰀 trols the translation􏰍 For example􏰌 the introduction and elimination of predicates in types controls the introduction and elimination of lamb da abstractions in translations􏰍
and
A
the memb ers of IFIP 􏰅􏰍􏰃􏰍
op erators together into overloading􏰍
Typing and translation rules
A􏰍􏰅 Types
This appendix presents the formal typing and trans􏰀 lation rules􏰌 one set of rules p erforming b oth typing
and translation􏰍 The rules are an extension given by Damas and Milner 􏰒DM􏰃􏰅􏰓􏰍
A􏰍􏰁 Language
of those
To present the typing and translation rules
loading􏰌 it is helpful to use a slightly simpler language that captures the essential issues􏰍 We will use a lan􏰀 guage with the usual constructs 􏰕identi􏰑ers􏰌 appli􏰀 cations􏰌 lamb da abstractions􏰌 and let expressions􏰖􏰌
plus two new constructs􏰌
that corresp ond to class
resp ectively􏰍 The syntax of expressions and given in Figure 􏰈􏰍
An over expression
over x 􏰉􏰉 􏰪 in e
inst x 􏰉􏰉 􏰪
􏰠
over and inst expressions􏰌 and instance declarations􏰌
declares
scop e of this declaration􏰌 there may be one or more
x to be an overloaded identi􏰑er􏰍
Within the
idea that of a func􏰀 form􏰖 by
of arithmetic op erators
given in Figure 􏰁 is shown in
for over􏰀
types
is
Figure 􏰋􏰍 ation for
In this 􏰑gure the type
we use
Num 􏰣
as an abbrevi􏰀
􏰕􏰣 􏰙 􏰣 􏰙 􏰣􏰘
In translating to the formal language􏰌
group ed the three
nary􏰏􏰍 This is straightforward􏰌 the central issue􏰉 how to resolve
􏰣 􏰙 􏰣 􏰙 􏰣􏰘
􏰣 􏰙 􏰣􏰖
￼￼corresp onding
inst expressions
￼􏰠
the type 􏰪 is an
􏰟 e􏰠 in e􏰁 instance of the
where
notion
and let expressions􏰌 the b ound variables in over and inst expressions may not be redeclared in a smaller scop e􏰍 Also unlike lamb da and let expressions􏰌 over and inst expressions must contain explicit types􏰘 the
to be made precise later􏰖􏰍
type 􏰪 Unlike lamb da
􏰕a
􏰁􏰠
￼￼￼Identi􏰑ers x
Expressions
e 􏰉􏰉􏰟
j e􏰠e􏰁
j 􏰬x􏰉 e
j letx􏰟e􏰠 ine􏰁
j overx􏰉􏰉􏰪ine
j instx􏰉􏰉􏰪􏰟e􏰠ine􏰁
Type
Type
Types
Predicated Types Type􏰀schemes
Variables 􏰔 Constructors 􏰭
􏰣 􏰉􏰉􏰟 􏰫 􏰉􏰉􏰟 􏰪 􏰉􏰉􏰟
􏰠
􏰕􏰣 􏰙 􏰣 􏰖 j 􏰔 j 􏰭􏰕􏰣􏰁 􏰉 􏰉 􏰉 􏰣n􏰖
􏰕x 􏰉􏰉 􏰣 􏰖􏰉 􏰫 j 􏰣 􏰃􏰔􏰉 􏰪 j 􏰫
x
￼Figure 􏰈􏰉 Syntax of expressions and types
￼￼￼over eq 􏰉􏰉 􏰃􏰔􏰉 Eq 􏰔 in
inst eq inst eq inst eq
􏰉􏰉 Eq Int 􏰟 eqInt in
􏰉􏰉 Eq Char 􏰟 eqChar in
􏰉􏰉 􏰃􏰔􏰉􏰃􏰑 􏰉􏰕eq 􏰉􏰉 Eq 􏰔􏰖􏰉􏰕eq 􏰉􏰉 Eq 􏰑 􏰖􏰉Eq 􏰕􏰔􏰘 􏰑 􏰖
􏰟 􏰬p􏰉􏰬q􏰉 eq 􏰕fst p􏰖 􏰕fst q􏰖 􏰮 eq 􏰕snd p􏰖 􏰕snd q􏰖 in eq 􏰕􏰁􏰘 􏰜a􏰚􏰖 􏰕􏰅􏰘 􏰜b􏰚􏰖
￼Figure 􏰊􏰉 De􏰑nition of equality􏰌 formalised
￼￼￼numD 􏰉􏰉 numD 􏰉􏰉 numD 􏰉􏰉
􏰃􏰔􏰉 Num 􏰔 in
Num Int 􏰟 􏰕addInt 􏰘 mulInt 􏰘 negInt 􏰖
Num Float 􏰟 􏰕addFloat 􏰘 mulFloat 􏰘 negFloat 􏰖 in
over
inst
inst
let 􏰕􏰡􏰖
let 􏰕􏰄􏰖
let negate
let square 􏰟 􏰬x􏰉 x􏰄x in square 􏰆
in
􏰟 fst numD in 􏰟 snd numD in 􏰟 thd numD in
￼Figure 􏰋􏰉 De􏰑nition of arithmetic op erations􏰌 formalised
􏰁􏰁
￼￼￼􏰕eq 􏰉􏰉o 􏰃􏰔􏰉Eq 􏰔􏰖􏰘
􏰕eq 􏰉􏰉i Eq Int n eq􏰕Eq Int􏰖 􏰖􏰘
􏰕eq 􏰉􏰉i Eq Char n eq􏰕Eq Char􏰖 􏰖􏰘
􏰕eq 􏰉􏰉i 􏰃􏰔􏰉􏰃􏰑 􏰉􏰕eq 􏰉􏰉 Eq 􏰔􏰖􏰉􏰕eq 􏰉􏰉 Eq 􏰑 􏰖􏰉Eq 􏰕􏰔􏰘 􏰑 􏰖 n eq 􏰕􏰃􏰔􏰉􏰃􏰑 􏰉􏰕eq􏰉􏰉Eq 􏰔􏰖􏰉􏰕eq 􏰉􏰉Eq 􏰑 􏰖􏰉Eq 􏰕􏰔􏰘􏰑 􏰖􏰖 􏰖􏰘 􏰕eq 􏰉􏰉Eq􏰔neq􏰕Eq􏰔􏰖􏰖􏰘
􏰕eq 􏰉􏰉 Eq 􏰑 n eq􏰕Eq 􏰑􏰖 􏰖􏰘
􏰕p 􏰉􏰉 􏰕􏰔􏰘 􏰑􏰖 n p􏰖􏰘
􏰕q 􏰉􏰉􏰕􏰔􏰘􏰑􏰖nq􏰖
￼Figure 􏰃􏰉 Some assumptions
￼￼￼￼￼TAUT A􏰘 􏰕x 􏰉􏰉 􏰪 n x􏰖 􏰜 x 􏰉􏰉 􏰪 n x TAUT A􏰘 􏰕x 􏰉􏰉i 􏰪 n x􏰖 􏰜 x 􏰉􏰉 􏰪 n x
￼￼￼SPEC
GEN
COMB
ABS
LET
A 􏰜 e 􏰉􏰉 􏰃􏰔􏰉 􏰪 n e
A 􏰜 e 􏰉􏰉 􏰒􏰔 n 􏰣 􏰓􏰪 n e
A 􏰜 e 􏰉􏰉 􏰪 n e
􏰔 not free in A
￼￼￼￼￼A 􏰜
A 􏰜 A 􏰜
A 􏰜
e 􏰉􏰉 􏰃􏰔􏰉 􏰪 n e
￼e 􏰉􏰉 􏰕􏰣
􏰠
􏰙 􏰣 􏰖 n e
􏰠􏰠􏰠 e 􏰉􏰉 􏰣 n e
􏰠􏰠 􏰕e e 􏰖 􏰉􏰉 􏰣 n 􏰕e e 􏰖
￼￼￼￼￼Ax􏰘 􏰕x 􏰉􏰉 􏰣
􏰠
n x􏰖 􏰜 e 􏰉􏰉 􏰣 n e
￼￼A􏰜 􏰕􏰬x􏰉 e􏰖 􏰉􏰉 􏰕􏰣
A 􏰜 e 􏰉􏰉 􏰪 n e
􏰠
􏰙 􏰣 􏰖 n 􏰕􏰬x􏰉 e􏰖
￼􏰠􏰠 Ax􏰘 􏰕x 􏰉􏰉 􏰪 n x􏰖 􏰜 e 􏰉􏰉 􏰣 n e
􏰠􏰠 A 􏰜 􏰕let x 􏰟 e in e 􏰖 􏰉􏰉 􏰣 n 􏰕let x 􏰟 e in e 􏰖
￼￼￼￼￼Figure 􏰂􏰉 Typing and translation rules􏰌 part 􏰁
􏰁􏰅
A􏰍􏰆 Assumptions
Typing is done in the context of a set of assump􏰀 tions􏰌 A􏰍 The assumptions bind typing and transla􏰀
The
instance
relation
􏰪 􏰬A 􏰪
tion information to
sion􏰍 This includes
let expression􏰌 and
we write them as sequences􏰌 assumptions are sets􏰌 and therefore the order is irrelevant􏰍
􏰯 􏰕x 􏰉􏰉o 􏰪 􏰖 is used
for overloaded identi􏰑ers􏰘
part is similar to the de􏰑nition in Damas􏰐
the free identi􏰑ers in an expres􏰀 identi􏰑ers b ound in lamb da and overloaded identi􏰑ers􏰍 Although
􏰃􏰔􏰁􏰉􏰉􏰉􏰔n􏰉􏰫 and
􏰠􏰠
There are three forms of binding in an assumption list􏰉
􏰪 􏰬A 􏰪􏰠 i􏰔
􏰕􏰁􏰖 􏰑i is not free in 􏰪 and
􏰕􏰅􏰖 􏰂􏰣􏰁􏰘 􏰉 􏰉 􏰉 􏰘 􏰣n􏰉 􏰒􏰣􏰁􏰟􏰔􏰁􏰘 􏰉 􏰉 􏰉 􏰘 􏰣n􏰟􏰔n􏰓􏰫 􏰬A 􏰫
􏰠
􏰯 􏰕x 􏰉􏰉i 􏰪 n x􏰪􏰖 is used for declared instances of overloaded identi􏰑ers􏰘 and
where
de􏰑ned as follows􏰉
􏰪 􏰟
􏰃􏰑􏰁􏰉􏰉􏰉􏰑m􏰉􏰫 􏰌 is
􏰪 􏰟
This
Milner􏰍 The b ound variables of 􏰪 are sp ecialised and the resulting predicated types are compared􏰍
􏰠
De􏰑ne 􏰫 􏰬A 􏰫 i􏰔 the type part of 􏰫 equals the type
􏰠
part of 􏰫 􏰕the same condition as Damas􏰐Milner􏰖􏰌
and for every predicate 􏰕x 􏰉􏰉 􏰣 􏰖 in 􏰫􏰌 either
􏰯 there is a predicate of the form 􏰕x 􏰉􏰉 􏰣􏰖 in 􏰫􏰠 􏰕i􏰍e􏰍 the predicate app ears in b oth types􏰖􏰘 or
􏰠
￼􏰯 􏰕x 􏰉􏰉 􏰪 n x􏰖 is used for variables􏰌 and assumed identi􏰑ers􏰍
lambda and
instances of overloaded
let
bound
￼￼￼In 􏰕x 􏰉􏰉 􏰪 n x􏰖 and 􏰕x 􏰉􏰉i 􏰪 n x􏰖􏰌 the identi􏰑er x is the translation of x􏰍 If x is not an overloaded identi􏰑er 􏰕that is􏰌 if x is b ound by a lamb da or let expression􏰖􏰌 then the assumption for x has the form 􏰕x 􏰉􏰉 􏰪 n x􏰖􏰌 so x simply translates as itself􏰍
Figure 􏰃 shows the assumptions available when ap􏰀 plying the inference rules to the expression
􏰬p􏰉 􏰬q􏰉 eq 􏰕fst p􏰖 􏰕fstq􏰖 􏰮 eq 􏰕snd p􏰖 􏰕snd q􏰖
in Figure 􏰊􏰍 There are three 􏰕􏰉􏰉i 􏰖 bindings􏰌 corre􏰀 sp onding to the three instance declarations􏰌 and two
􏰕􏰉􏰉􏰖 bindings for the two b ound variables􏰌 and two
􏰕􏰉􏰉􏰖 bindings corresp onding to assumed instances of equality􏰍 􏰕We shall see later how assumed instances
are introduced by the PRED rule􏰍􏰖
􏰯 the predicate can tions A􏰍
b e
eliminated
under assump􏰀
A􏰍􏰇 Instances
Given a set of assumptions A􏰌 we
de􏰑ne an instance
holds􏰍 On the other hand􏰌
􏰕􏰃􏰔􏰉 􏰕eq 􏰉􏰉 Eq 􏰔􏰖􏰉 􏰒􏰔􏰓 􏰙 􏰔 􏰙 Bool􏰖 􏰬A􏰠 􏰕􏰒Float 􏰓 􏰙 Float 􏰙 Bool 􏰖
do es not hold􏰌 since A􏰠 contains no binding asserting
􏰠
􏰪 􏰬A 􏰣 􏰮 􏰪 􏰬A 􏰣
􏰠􏰠
if 􏰪 and 􏰪 are not
relation
b etween
type􏰀schemes􏰌
􏰠 􏰪 􏰬A 􏰪 􏰉
This can be read as 􏰎􏰪 is more general than 􏰪􏰠 under assumptions A􏰏􏰍 This is the same as the relationship de􏰑ned by Damas and Milner􏰌 but extended to apply to predicated types􏰍
Only certain sets of assumptions are valid􏰍 The
eq has an instance at type Float 􏰍 type􏰀schemes are uni􏰑able if
de􏰑nition of validity dep ends on the 􏰬A
there is a 􏰕well􏰀founded􏰖 mutual recursion between the de􏰑nition of valid assumptions and the de􏰑nition of 􏰬A 􏰍 We give the de􏰑nition of 􏰬A in this section􏰌 and the de􏰑nition of valid assumptions in the next􏰍
that is􏰌 if there exists a type that is
b oth under some set of assumptions􏰍 and 􏰪 􏰠 are uni􏰑able if there exists a type set of assumptions A such that
relation􏰌 so
􏰁􏰆
A either
􏰕x 􏰉􏰉 􏰣 􏰖
can
b e
eliminated
under A i􏰔
predicate
￼􏰯 􏰕x 􏰉􏰉 􏰣 n x􏰖 is in A􏰘 or
￼􏰠􏰠
􏰯 􏰕x 􏰉􏰉i 􏰪 n x􏰖 is in A and 􏰪 􏰬A 􏰣 􏰍
For Figure
example􏰌 if A􏰠 􏰃􏰌 then
is the
set of assumptions in
that Two
they overlap􏰌 an instance of We say that 􏰪
􏰣 and valid
uni􏰑able􏰍
We
write
􏰪 􏰢􏰪
􏰕􏰃􏰔􏰉 􏰕eq
􏰬A􏰠 􏰕􏰒Int 􏰓 􏰙 Int 􏰙 Bool 􏰖
􏰉􏰉 Eq 􏰔􏰖􏰉 􏰒􏰔􏰓 􏰙 􏰔 􏰙 Bool􏰖
￼￼￼￼PRED
REL
OVER
INST
A􏰘 􏰕x 􏰉􏰉 􏰣 n x􏰣 􏰖 􏰜 e 􏰉􏰉 􏰫 n e
A 􏰜 e 􏰉􏰉 􏰕x 􏰉􏰉 􏰣 􏰖􏰉 􏰫 n 􏰕􏰬x􏰣 􏰉 e􏰖 A 􏰜 e 􏰉􏰉 􏰕x 􏰉􏰉 􏰣 􏰖􏰉 􏰫 n e
􏰠 A 􏰜 x 􏰉􏰉 􏰣 n e
􏰠 A 􏰜 e 􏰉􏰉 􏰫 n 􏰕e e 􏰖
Ax􏰘 􏰕x 􏰉􏰉o 􏰪􏰖 􏰜 e 􏰉􏰉 􏰣 n e
A 􏰜 􏰕over x 􏰉􏰉 􏰪 in e􏰖 􏰉􏰉 􏰣 n e A􏰘 􏰕x 􏰉􏰉i 􏰪􏰠 n x􏰪􏰠􏰖 􏰜 e􏰠 􏰉􏰉 􏰪􏰠 n e􏰠
􏰠
􏰕x 􏰉􏰉o 􏰪􏰖 􏰅 A
􏰕x 􏰉􏰉o 􏰪􏰖 􏰅 A
􏰕x 􏰉􏰉o 􏰪􏰖 􏰅 A
￼￼￼￼￼￼￼￼￼￼￼￼n x􏰪􏰠􏰖 􏰜 e 􏰉􏰉 􏰣 n e 􏰠􏰠􏰠
A􏰘 􏰕x 􏰉􏰉i 􏰪
A 􏰜 􏰕inst x 􏰉􏰉 􏰪 􏰟 e in e􏰖 􏰉􏰉 􏰣 n 􏰕let x􏰪􏰠 􏰟 e
in e􏰖
￼￼￼￼Figure 􏰁􏰠􏰉 Typing and translation rules􏰌 part 􏰅
￼￼￼let eq􏰕EqInt􏰖 􏰟 eqInt in
let eq 􏰕Eq Char 􏰖 􏰟 eqChar in
let eq 􏰕􏰃􏰔􏰉􏰃􏰑􏰉􏰕eq􏰉􏰉Eq 􏰔􏰖􏰉􏰕eq 􏰉􏰉Eq 􏰑􏰖􏰉Eq 􏰕􏰔􏰘􏰑􏰖􏰖 􏰟 􏰬eq 􏰕Eq 􏰔􏰖􏰉􏰬eq􏰕Eq 􏰑􏰖􏰉􏰬p􏰉􏰬q􏰉
eq 􏰕Eq 􏰔􏰖 􏰕fst p􏰖 􏰕fst q􏰖 􏰮 eq 􏰕Eq 􏰑􏰖 􏰕snd p􏰖 􏰕snd eq 􏰕􏰃􏰔􏰉􏰃􏰑􏰉􏰕eq􏰉􏰉Eq 􏰔􏰖􏰉􏰕eq 􏰉􏰉Eq 􏰑 􏰖􏰉Eq 􏰕􏰔􏰘􏰑􏰖􏰖 eq 􏰕Eq Int 􏰖 eq 􏰕Eq Char 􏰖
q􏰖 in 􏰕􏰁􏰘 􏰜a􏰚􏰖
􏰕􏰅􏰘 􏰜b􏰚􏰖
￼Figure 􏰁􏰁􏰉 Translation of equality􏰌 formalised
￼￼￼A􏰁 􏰉 􏰕eq 􏰉􏰉o 􏰃􏰔􏰉Eq 􏰔􏰖
􏰕eqInt 􏰉􏰉 Eq Int n eqInt􏰖 􏰕eqChar 􏰉􏰉 Eq Int n eqChar 􏰖
e􏰁 􏰉 inst eq 􏰉􏰉 Eq Int 􏰟 eqInt in inst eq 􏰉􏰉 Eq Char 􏰟 eqChar in eq
￼Figure 􏰁􏰅􏰉 A problematic expression
􏰁􏰇
A􏰍􏰈 Valid assumptions
For example􏰌 let A􏰠 be the set of assumptions shown in Figure 􏰃􏰌 together with assumptions ab out the types of integer and character constants􏰍 Then the ab ove rules are su􏰝cient to derive that
is a
􏰕􏰉􏰉o 􏰖 rule
assumptions used within pro ofs must be
All sets of
valid􏰍 The valid sets of assumptions are inductively
de􏰑ned as
follows􏰉
􏰯 Empty􏰍 The empty assumption set􏰌 fg􏰌 is valid􏰍
􏰯 Normal identi􏰑er􏰍 If A is a valid assumption set􏰌
x is an identi􏰑er that does not app ear in A􏰌 and
􏰪 is a type scheme􏰌 then
A􏰘 􏰕x 􏰉􏰉 􏰪 n x􏰖 is a valid assumption set􏰍
􏰯 Overloaded identi􏰑er􏰍 If A is a valid assumption
A􏰠 􏰜 􏰕eq 􏰁 􏰅􏰖 􏰉􏰉 Bool n 􏰕eq􏰕Eq Int􏰖 􏰁 􏰅􏰖
set􏰌 􏰪 is 􏰪􏰁 􏰘 􏰉
􏰤 􏰤 􏰤
then
More complicated uses of overloading require the remaining four rules􏰌 shown in Figure 􏰁􏰠􏰍 The 􏰑rst two deal with the introduction and elimination of predicates􏰌 and the second two deal with the over and inst constructs􏰍
As we have seen􏰌 expressions with types that con􏰀 tain classes 􏰕that is􏰌 expressions with predicated types􏰖 are translated to lamb da abstractions that require a dictionary to be passed at run􏰀time􏰍 This idea is encapsulated in the PRED 􏰕􏰎predicate􏰏􏰖 and
REL 􏰕􏰎release􏰏􏰖 rules􏰍 The PRED introduce and eliminate predicates
x is an identi􏰑er that does not app ear in A􏰌
a type 􏰉 􏰉 􏰘 􏰪n
scheme􏰌 and 􏰣􏰁􏰘 􏰉 􏰉 􏰉 􏰘 􏰣m
are types schemes such that
􏰪 􏰬A 􏰪i􏰌 for i from 􏰁 to n􏰌 and 􏰪 􏰬A 􏰣i􏰌 for i from 􏰁 to m􏰌 and
and REL rules analogously to rules introduce
􏰪i􏰢􏰪j 􏰌 for distinct
i􏰘 j from 􏰁 to n
way that the GEN and SPEC eliminate b ound type variables􏰍
A􏰘 􏰕x 􏰉􏰉o 􏰪 􏰖􏰘
􏰕x 􏰉􏰉i 􏰪􏰁 n x􏰪􏰁 􏰖􏰘 􏰉 􏰉 􏰉 􏰘 􏰕x 􏰉􏰉i 􏰪n n x􏰪n 􏰖􏰘 􏰕x 􏰉􏰉 􏰣􏰁 n x􏰣􏰁 􏰖􏰘 􏰉 􏰉 􏰉 􏰘 􏰕x 􏰉􏰉 􏰣m n x􏰣m 􏰖
valid assumption set􏰍
binding to the environment􏰌 types inst expressions adding binding to the environment􏰍
For example􏰌 the assumptions in Figure 􏰃 are a valid set􏰍 However􏰌 this set would be invalid if aug􏰀 mented with the binding
􏰕eq 􏰉􏰉i 􏰃􏰗􏰉Eq 􏰕Char􏰘 􏰗􏰖 n eq􏰕􏰃􏰗􏰉Eq 􏰕Char􏰘􏰗􏰖􏰖 􏰖
as this instance overlaps with one already in the set􏰍
􏰕􏰉􏰉i 􏰖
A􏰍􏰊 Inference rules
We now give typings of the form
rules that
A 􏰜 e 􏰉􏰉 􏰪 n e
characterise
well􏰀
inference
The
and
Damas􏰐Milner rules 􏰕Figure 􏰂􏰖􏰍 There are two small di􏰔erences􏰉 translations have been added to each rule in a straightforward way􏰌 and there are two TAUT
rules break
􏰁􏰠􏰍 The 􏰑rst group is based directly on the
into two groups􏰌 shown in Figures 􏰂
rules instead of one 􏰕one rule for 􏰕􏰉􏰉􏰖 one for 􏰕􏰉􏰉i􏰖 bindings􏰖􏰍
bindings
and
Given A and e􏰌 we call 􏰪 a principal type scheme for e under A i􏰔
􏰯 A 􏰜 e 􏰉􏰉 􏰪 n e􏰘 and
are types and
A􏰠 􏰜 􏰕eq 􏰜a􏰚 􏰜b􏰚􏰖 􏰉􏰉 Bool
n 􏰕eq 􏰕Eq Char 􏰖
􏰜a􏰚 􏰜b􏰚􏰖
to resolve
That is􏰌 these rules alone are su􏰝cient simple overloading􏰍
the
and
the PRED rule adds a predicate to a type 􏰕and has a lamb da expression as its translation􏰖 and the REL rule removes a predicate from a type 􏰕and has an ap􏰀 plication as its translation􏰖􏰍
The OVER rule types over expressions adding
the appropriate
and the INST
the appropriate
The validity condition on sets of assumptions ensures that overloaded identi􏰑ers are only instanced at valid types􏰍
Notice that none of the translations contain
or inst expressions􏰌 therefore􏰌 they contain no loading􏰍 It is easy to verify that the translations are themselves well􏰀typed in the Hindley􏰐Milner system􏰍
For example􏰌 the program in Figure 􏰊 is translated by these rules into the program in Figure 􏰁􏰁􏰍 The reader can easily verify that this corresp onds to the translation from Figure 􏰆 to Figure 􏰇􏰍 We have thus
shown how to formalise the typing and tion ideas that were presented informally of the paper􏰍
transforma􏰀 in the body
In particular􏰌
over over􏰀
￼A􏰍􏰋 Principal
typings
￼􏰁􏰈
􏰠􏰠􏰠􏰠 􏰯 for every 􏰪 􏰌 if A 􏰜 e 􏰉􏰉 􏰪 n e then 􏰪 􏰬A 􏰪
A key result in the Hindley􏰐Milner system is that every expression e that has a well􏰀typing has a prin􏰀 cipal type scheme􏰍
We conjecture that for every valid set of assump􏰀 tions A and every expression e containing no over or inst expressions􏰌 if e has a well􏰀typing under A then e has a principal type scheme under A􏰍
A􏰠 􏰜 eq 􏰉􏰉 􏰃􏰔􏰉Eq 􏰔 n eq 􏰕Eq alpha􏰖
is principal􏰍 Examples of non􏰀principal typings are
􏰒DM􏰃􏰅􏰓
􏰒FGJM􏰃􏰈􏰓
􏰒GR􏰃􏰆􏰓
􏰒Hin􏰊􏰂􏰓
􏰒HMM􏰃􏰊􏰓
􏰒HMT􏰃􏰃􏰓
􏰒Kae􏰃􏰃􏰓
􏰒Mil􏰋􏰃􏰓
􏰒Mil􏰃􏰇􏰓
􏰒Mil􏰃􏰋􏰓
􏰒MP􏰃􏰈􏰓
L􏰍 Damas and R􏰍 Milner􏰌 Principal type schemes for functional programs􏰍 In Pro􏰀 ceedings of the 􏰂􏰚th Annual Symposium on Principles of Programming Languages􏰌 Albuquerque􏰌 N􏰍M􏰍􏰌 January 􏰁􏰂􏰃􏰅􏰍
K􏰍 Futasagi􏰌 J􏰍A􏰍 Goguen􏰌 J􏰍􏰀P􏰍 Jouan􏰀 naud􏰌 and J􏰍 Meseguer􏰌 Principles of OBJ􏰅􏰍 In Proceedings of the 􏰁􏰅􏰚th An􏰀 nual Symposium on Principles of Pro􏰀
￼For Figure 􏰃􏰍
gramming Languages􏰌
A􏰍 Goldb erg and D􏰍
􏰃􏰠􏰉 The Language and Its Implementa􏰀 tion􏰍 Addison􏰀Wesley􏰌 􏰁􏰂􏰃􏰆􏰍
Each of
these is an instance of the principal
example􏰌 let A􏰠 be the set of assumptions in Then the typing
A􏰠 􏰜 eq 􏰉􏰉 Eq Int n eq􏰕Eq Int􏰖
A􏰠 􏰜 eq 􏰉􏰉 Eq Char n eq􏰕Eq Char􏰖
scheme Trans􏰍
typing The existence of principal types is problematic for
under
expressions that For example􏰌 let and expression in derive the typings
assumptions
A􏰠 􏰍
contain over and inst expressions􏰍
A􏰁 and Figure
A􏰁 􏰜 e􏰁 􏰉􏰉 Eq Int n eqInt
A􏰁 􏰜 e􏰁 􏰉􏰉 Eq Char n eqChar
still ensures the existence of principal
References
types􏰍
􏰒BEJ􏰃􏰃􏰓 D􏰍 Bj􏰣rner􏰌 A􏰍 Ershov􏰌 and N􏰍D􏰍 Jones􏰌 editors􏰌 Partial Evaluation and Mixed Computation􏰌 North􏰀Holland􏰌 􏰁􏰂􏰃􏰃 􏰕to
app ear􏰖􏰍
􏰒CW􏰃􏰈􏰓 L􏰍 Cardelli and
standing types􏰌 data abstraction􏰌 and
polymorphism􏰍 Computing Surveys 􏰁􏰋􏰌 􏰇􏰌 Decemb er 􏰁􏰂􏰃􏰈􏰍
􏰒Car􏰃􏰃􏰓 L􏰍 Cardelli􏰌 Structural subtyping and the notion of p ower type􏰍 In Proceedings of the 􏰁􏰈􏰚th Annual Symposium on Prin􏰀 ciples of Programming Languages􏰌 San
Diego􏰌 California􏰌 January 􏰁􏰂􏰃􏰃􏰍
In Proceedings of the Symposium
and Functional Programming􏰌 Austin􏰌 Texas􏰌 August 􏰁􏰂􏰃􏰇􏰍
R􏰍 Milner􏰌 Changes to the Standard ML core language􏰍 Rep ort ECS􏰀LFCS􏰀􏰃􏰋􏰀􏰆􏰆􏰌 Edinburgh University􏰌 Computer Science Dept􏰍􏰌 􏰁􏰂􏰃􏰋􏰍
J􏰍 C􏰍 Mitchell and G􏰍 D􏰍 Plotkin􏰌 Ab􏰀 stract types have existential type􏰍 In Pro􏰀 ceedings of the 􏰁􏰅􏰚th Annual Symposium on Principles of Programming Languages􏰌 January 􏰁􏰂􏰃􏰈􏰍
e􏰁 be the assumption
􏰁􏰅􏰍 Then it is
set possible to
resolu􏰀 It remains an op en question
But there
tion of this is to require
tions have global scop e􏰍
whether there is some less drastic restriction that
is no principal type􏰙 One possible
that over and inst declara􏰀
P􏰍 Wegner􏰌 On under􏰀
􏰁􏰊
R􏰍 Hindley􏰌 The principal type
of an ob ject in combinatory logic􏰍
Am􏰍 Math􏰍 Soc􏰍 􏰁􏰇􏰊􏰌 pp􏰍 􏰅􏰂􏰤􏰊􏰠􏰌 Decem􏰀 ber 􏰁􏰂􏰊􏰂􏰍
R􏰍 Harper􏰌 D􏰍 MacQueen􏰌 and R􏰍 Milner􏰌 Standard ML􏰍 Rep ort ECS􏰀LFCS􏰀􏰃􏰊􏰀􏰅􏰌 Edinburgh University􏰌 Computer Science Dept􏰍􏰌 􏰁􏰂􏰃􏰊􏰍
R􏰍 Harp er􏰌 R􏰍 Milner􏰌 and M􏰍 Tofte􏰌 The de􏰑nition of Standard ML􏰌 version 􏰅􏰍 Re􏰀 p ort ECS􏰀LFCS􏰀􏰃􏰃􏰀􏰊􏰅􏰌 Edinburgh Uni􏰀 versity􏰌 Computer Science Dept􏰍􏰌 􏰁􏰂􏰃􏰃􏰍
S􏰍 Kaes􏰌 Parametric polymorphism􏰍 In Proceedings of the 􏰅􏰚nd European Sym􏰀 posium on Programming􏰌 Nancy􏰌 France􏰌 March 􏰁􏰂􏰃􏰃􏰍 LNCS 􏰆􏰠􏰠􏰌 Springer􏰀Verlag􏰌 􏰁􏰂􏰃􏰃􏰍
R􏰍 Milner􏰌 A theory of
phism in programming􏰍 J􏰍 Comput􏰍 Syst􏰍 Sci􏰍 􏰁􏰋􏰌 pp􏰍 􏰆􏰇􏰃􏰤􏰆􏰋􏰈􏰌 􏰁􏰂􏰋􏰃􏰍
R􏰍 Milner􏰌 A prop osal for Standard ML􏰍
January 􏰁􏰂􏰃􏰈􏰍
Robson􏰌 Smal ltalk􏰀
type polymor􏰀
on Lisp
􏰒Rey􏰃􏰈􏰓 J􏰍 C􏰍 Reynolds􏰌 Three approaches to type structure􏰍 In Mathematical Foun􏰀 dations of Software Development􏰌 LNCS
􏰁􏰃􏰈􏰌 Springer􏰀Verlag􏰌 􏰁􏰂􏰃􏰈􏰍
􏰒Str􏰊􏰋􏰓 C􏰍 Strachey􏰌 Fundamental concepts in programming languages􏰍 Lecture notes for International Summer Scho ol in Com􏰀 puter Programming􏰌 Cop enhagen􏰌 Au􏰀
gust 􏰁􏰂􏰊􏰋􏰍
􏰒Tur􏰃􏰈􏰓 D􏰍 A􏰍 Turner􏰌 Miranda􏰉 A non􏰀strict functional language with polymorphic types􏰍 In Proceedings of the 􏰅􏰚nd Inter􏰀 national Conference on Functional Pro􏰀 gramming Languages and Computer Ar􏰀 chitecture􏰌 Nancy􏰌 France􏰌 Septemb er
􏰁􏰂􏰃􏰈􏰍 LNCS 􏰅􏰠􏰁􏰌 Springer􏰀Verlag􏰌 􏰁􏰂􏰃􏰈􏰍
􏰒Wan􏰃􏰋􏰓 M􏰍 Wand􏰌 Complete type inference for simple ob jects􏰍 In Proceedings of the Sym􏰀
on Logic in Computer Science􏰌 NY􏰌 June 􏰁􏰂􏰃􏰋􏰍 IEEE Computer
posium
Ithaca􏰌
So ciety Press􏰌 􏰁􏰂􏰃􏰋􏰍
􏰁􏰋
