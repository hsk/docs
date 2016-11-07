test(A) :- A \= a,!,(test(a);!).
test(a) :- !,fail.
:- (test(b),writeln(ok); writeln(fail)),halt.

