-module(test1).
-export([main/0]).

main()->
        io:format("Hello world!\n").


-module(reloading).

-export([loop/0]).

loop() ->
    receive
        upgrade ->
            code:purge(?MODULE),
            compile:file(?MODULE),
            code:load_file(?MODULE),
            ?MODULE:loop();
        hello ->
            io:format("This is a test~n"),
            loop();
        _ ->
            loop()
    end.

