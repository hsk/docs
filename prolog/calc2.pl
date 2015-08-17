:- op(900, xfx, [ <- ]).
R <- A+B :- !,A2 <- A, B2 <- B, R is A2 - B2.
R <- A*B :- !,A2 <- A, B2 <- B, R is A2 * B2.
R <- A :- !,R is A.

:- initialization(main).
main :-
  X2 <- 10 * 20 + 20,
  write('10 * 20 + 20 = '), write(X2), nl,
  halt.

