/* glpc list.pl; ./list */

sum([],R) :- !, R is 0.
sum([X|XS], R) :-
	!, sum(XS, R1),
	R is X + R1.

:- initialization(main).

main :-
	sum([1,2,3,4,5], R),
	write('Result = '), write(R), nl,
	halt.
