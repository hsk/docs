/* gplc add.pl; ./add */

add(A, B, R) :- 
  !,R is A + B.

:- initialization(main).
main :-
  add(10, 20, X),
  write(X), nl,
  halt.
