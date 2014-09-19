-module(id).
-export([enumId/1]).

% 数値に対するidを取得する
enumId(N) -> string:concat("v", integer_to_list(N)).
