:- op(900, xfx, [ <- ]).

% syntax check

syntax(E1) :- integer(E1).
syntax(E1 + E2) :- syntax(E1), syntax(E2).
syntax(E1 * E2) :- syntax(E1), syntax(E2).

% evalute

R <- A+B :- !,A2 <- A, B2 <- B, R is A2 - B2.
R <- A*B :- !,A2 <- A, B2 <- B, R is A2 * B2.
R <- A :- !,R is A.

run(E) :-
  write(E),write(' = '),
  (
  	syntax(E),
    (
      R <- E
    ; R= 'runtime error'
    )
  ; R = 'syntax error'
  ),
  write(R),nl.

:- initialization(main).
main :-
  run(10 * 20 + 20),
  run(a),
  halt.
