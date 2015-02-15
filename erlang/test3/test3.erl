-module(test3).
-export([main/0]).

say_something(What, 0) ->
    done;
say_something(What, Times) ->
    io:format("~p~n", [What]),
    say_something(What, Times - 1).

start() ->
    spawn(test3, say_something, [hello, 3]),
    spawn(test3, say_something, [goodbye, 3]).

main()->
        io:format("start\n"),
        start(),
        io:format("")
.

