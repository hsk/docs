-module(pre).
-export([
  union/2,
  intersect/2,
  nub/1,
  isEmpty/1,
  fold_left/3,
  fold_left1/2,
  deleteFirst/2,
  diff/2,
  split3/1
]).

% 2 Preliminaries
% 2 予備知識

% 和集合
union(Xs,Ys) ->
  lists:append(
    lists:filter(fun(X) -> not lists:member(X,Ys) end, Xs),
    Ys
  ).

% 積集合
intersect(Xs, Ys) ->
  lists:filter(fun(X) -> lists:member(X, Ys) end, Xs).

% リストをセットにする。要素が１つずつにまとめる
nub(Xs) -> lists:foldl(fun(X,Ys) ->
  	Mem = lists:member(X, Ys),
    if
      Mem -> Ys;
      true -> [X | Ys]
    end
  end,
  [],
  Xs
).

% 空チェック
isEmpty([]) -> true;
isEmpty(_) -> false.

fold_left(_,A,[]) -> A;
fold_left(F,A,[X|Xs]) -> fold_left(F,F(A,X),Xs).

% reduceじゃないのかな
fold_left1(_, []) -> throw("empty list");
fold_left1(_, [X]) -> X;
fold_left1(F, [X|Xs]) -> fold_left(F,X,Xs).

% リスト内の最初の1個目のxを削除する
deleteFirst(_,[]) -> [];
deleteFirst(X,[Y|Ys]) when X == Y -> Ys;
deleteFirst(X,[Y|Ys]) -> [Y|deleteFirst(X,Ys)].

% 最初のリストから2番目のリストの要素を消す
diff(Xs,[]) -> Xs;
diff(Xs,[Y|Ys]) -> diff(deleteFirst(Y, Xs), Ys).


% 3つの多値を持っているリストを３つのリストに分割する
split3(Xs) ->
  loop({[], [], []}, Xs).

loop({Ws, Xs, Ys},[]) -> {lists:reverse(Ws), lists:reverse(Xs), lists:reverse(Ys)};
loop({Ws, Xs, Ys},[{W,X,Y}|Zs]) -> loop({[W | Ws], [X | Xs], [Y | Ys]},Zs).
