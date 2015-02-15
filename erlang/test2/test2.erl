-module(test2).
-export([main/0]).

double(X) -> 2 * X.

fact(0) -> 1;
fact(N) -> N * fact(N-1).

mult(X, Y) -> X * Y.

convert(M, inch) -> M / 2.54;
convert(N, centimeter) -> N * 2.54.
cnvlen({centimeter, X}) -> {inch, X / 2.54 };
cnvlen({inch, Y}) -> {centimeter, Y * 2.54 }.

eval({int,N}) -> N;
eval({add,X,Y}) -> eval(X) + eval(Y);
eval({mul,X,Y}) -> eval(X) * eval(Y).

listlen([]) -> 0;
listlen([X | XS]) -> 1 + listlen(XS).

new(A) ->
  #{ key => A }.

main()->
        io:format("double  ~w\n", [double(2)]),
        io:format("mult    ~w\n", [mult(10, 20)]),
        io:format("fact    ~w\n", [fact(10)]),
        io:format("convert ~w ~w\n", [convert(10,inch),convert(10,centimeter)]),
        io:format("conv_len ~w ~w\n", [cnvlen({inch,1}),cnvlen({centimeter,1})]),
	E = {add, {int, 1}, {mul, {int, 2},{int,3}}},
        io:format("1+2*3 ~w \n", [eval(E)]),
        io:format("listlen ~w \n", [listlen([1,2,3,4])]),
        io:format("abc ~s \n", [[97,98,99]]),
        ENV = new("a"),
        io:format("")
.

