-module(fizzbuzz).
-export([fizzbuzz/1]).

fizzbuzz(N) when N > 100 -> ok;
fizzbuzz(N) ->
    if
        N rem 15 == 0 -> io:write('FizzBuzz');
        N rem 3  == 0 -> io:write('Fizz');
        N rem 5  == 0 -> io:write('Buzz');
        true -> io:write(N)
    end,
    io:nl(),
    fizzbuzz(N + 1).

