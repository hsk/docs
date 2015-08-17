eval(int(A),R) :- !,R is A.
eval(add(A,B),R) :- !,eval(A,A2), eval(B,B2), R is A2 + B2.
eval(sub(A,B),R) :- !,eval(A,A2), eval(B,B2), R is A2 - B2.
eval(mul(A,B),R) :- !,eval(A,A2), eval(B,B2), R is A2 * B2.
eval(div(A,B),R) :- !,eval(A,A2), eval(B,B2), R is A2 / B2.

:- initialization(main).
main :-

  eval(add(mul(int(10),int(20)),int(20)), X),
  write('10 * 20 + 20 = '), write(X), nl,
  halt.
