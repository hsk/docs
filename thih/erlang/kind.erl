% 3 Kinds
-module(kind).
-export([star/0,kfun/2]).
star() -> star.
kfun(A,B) -> {kfun, A, B}.
