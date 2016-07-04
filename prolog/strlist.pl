/* gplc strlist.pl ; ./strlist */
:- initialization(main).
main :-
  X = "10 * 20 + 20",
  write(X), nl,
  atom_codes(C,X),
  write(C),
  halt.

