/* gplc add2.pl; ./add */

add(A, B, R) :-
  !,R is A + B.
:- dynamic({}/1).
:- initialization(main).
main :-
  add(10, 20, X),
  write('10 + 20 = '), write(X), nl,
  Z is 10 * 20,
  write('10 * 20 = '), write(Z), nl,
  halt.
