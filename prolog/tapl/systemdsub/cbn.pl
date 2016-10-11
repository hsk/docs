:- initialization(main).

eval1(Env, E, R)          :- writeln(Env;E), eval(Env, E, R).

eval(Env, X, R)           :- atom(X),member(X=dummy(Env_,E_), Env), eval1(Env_,E_,R).
eval(Env, X, V)           :- atom(X),member(X=V, Env).
eval(Env, λ(X, Body), R) :- R=closure(X, Env, Body).
eval(Env, app(E1, E2), R) :- eval1(Env, E1, closure(X, Env_, Body)), eval1([X=dummy(Env, E2) | Env_], Body, R).
eval(  _, N, N)           :- integer(N).
eval(Env, add(E1, E2), N) :- eval1(Env, E1, N1), eval1(Env, E2, N2), N is N1 + N2.
eval(Env, mul(E1, E2), N) :- eval1(Env, E1, N1), eval1(Env, E2, N2), N is N1 * N2.
eval(Env, print(E), N)    :- eval1(Env, E, N), writeln(N).

main :-
  eval1([], print(app(λ(x,mul(x,x)),print(add(5,7)))), R), writeln(R),
  halt.
