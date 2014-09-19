-module(t11_1_lit_tests).
-include_lib("eunit/include/eunit.hrl").

enumId1_test() ->
  "v1" = id:enumId(1).

enumId33_test() ->
  "v33" = id:enumId(33).
