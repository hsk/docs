-module(t01_pre_tests).
-include_lib("eunit/include/eunit.hrl").

union_test() ->
  [1, 2, 3] = pre:union([1, 2],[2, 3]).

intersect_test() ->
  [2] = pre:intersect([1, 2], [2, 3]).

union_and_intersect_test() ->
  A = [5, 4, 3, 2, 1],
  B = [4, 5, 6, 7],
  [3, 2, 1, 4, 5, 6, 7] = pre:union(A, B),
  [5, 4] = pre:intersect(A, B).

nub_test() ->
  A = [1, 1, 2, 2, 3, 4, 5, 1],
  [5, 4, 3, 2, 1] = pre:nub(A).

isEmpty1_test() ->
  A = [],
  true = pre:isEmpty(A).

isEmpty2_test() ->
  A = [1],
  false = pre:isEmpty(A).

fold_left1_test() ->
  A = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10],
  B = pre:fold_left1(
    fun(A1, B1) ->
      A1 + B1
    end,
    A
  ),
  B = 55.

deleteFirst_test() ->
  A = [1, 2, 3, 4, 3, 4, 5],
  B = pre:deleteFirst(3, A),
  B = [1, 2, 4, 3, 4, 5].

diff_test() ->
  A = [1, 2, 3, 4],
  B = [3, 4, 5, 6],
  R = pre:diff(A, B),
  [1, 2] = R.


diff2_test() ->
  A = [1, 2, 3, 4, 3, 4],
  B = [3, 4, 5, 6, 4],
  R = pre:diff(A, B),
  R = [1, 2, 3].

split3_test() ->
  A1 = [{1, 10, 100}, {2, 20, 200}, {3, 30, 300}],
  {A, B, C} = pre:split3(A1),
  A = [1, 2, 3],
  B = [10, 20, 30],
  C = [100, 200, 300].
