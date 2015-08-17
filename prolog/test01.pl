% :- dynamic(threshold/1).

threshold(5).


:- initialization(main).

main :-
  threshold(X),
  write(X), nl,
  halt.
