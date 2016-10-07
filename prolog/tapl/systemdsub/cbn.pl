:- initialization(main).

eval1(Env, E, R) :- writeln(Env;E), eval(Env, E, R).

eval(Env, var(X), R) :- member(X=vDummy(Env_,E_), Env), eval1(Env_,E_,R).
eval(Env, var(X), V) :- member(X=V, Env).
eval(Env, λ(X, Body), vClosure(X, Env, Body)).
eval(Env, app(E1, E2), R) :-
    eval1(Env, E1, vClosure(Arg, Env_, Body)),
    % eval1(Env, E2,E2_),
    E2_=vDummy(Env, E2),
    eval1([Arg=E2_| Env_], Body, R).
eval(  _, int(N), vInt(N)).
eval(Env, add(E1, E2), vInt(N)) :-
    eval1(Env, E1, vInt(N1)),
    eval1(Env, E2, vInt(N2)),
    N is N1 + N2.
eval(Env, mul(E1, E2), vInt(N)) :-
    eval1(Env, E1, vInt(N1)),
    eval1(Env, E2, vInt(N2)),
    N is N1 * N2.
eval(Env, print(E), vInt(N)) :-
    eval1(Env, E, vInt(N)), writeln(N).

main :-
  eval1([], print(app(λ(x,add(var(x),var(x))),print(add(int(5),int(7))))), R), writeln(R),
  halt.
