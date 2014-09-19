% 4 Types
-module(type).
-export([
  tyvar/2,
  tyvar_append/2,
  tycon/2,
  tvar/1,
  tcon/1,
  tap/2,
  tgen/1,
  tUnit/0,
  tChar/0,
  tInt/0,
  tInteger/0,
  tFloat/0,
  tDouble/0,
  tList/0,
  tArrow/0,
  tTuple2/0,
  fn/2,
  list/1,
  tString/0,
  pair/2,
  tyvarKind/1,
  tyconKind/1,
  typeKind/1
]).

%   import Kind._

% 型変数

tyvar(Id,Kind) -> {tyvar,Id,Kind}.

% +->
tyvar_append(Tyvar,T) -> [{Tyvar,T}].

% 型コンストラクタ
tycon(Id,Kind) -> {tycon, Id, Kind}.

% 型 type

tvar(Tyvar) -> {tvar,Tyvar}.
tcon(Tycon) -> {tcon,Tycon}.
tap(Type1,Type2) -> {tap,Type1,Type2}.
tgen(Int) -> {tgen, Int}.

tUnit() -> tcon(tycon("()", kind:star())).
tChar() -> tcon(tycon("Char", kind:star())).
tInt() -> tcon(tycon("Int", kind:star())).
tInteger() -> tcon(tycon("Integer", kind:star())).
tFloat() -> tcon(tycon("Float", kind:star())).
tDouble() -> tcon(tycon("Double", kind:star())).

tList() ->
  tcon(tycon("List()", kind:kfun(kind:star(), kind:star()))).
tArrow() ->
  tcon(tycon("(=>)",
    kind:kfun(kind:star(), kind:kfun(kind:star(), kind:star())))).
tTuple2() ->
  tcon(tycon("(,)",
    kind:kfun(kind:star(), kind:kfun(kind:star(), kind:star())))).

fn(Type1,Type2) -> tap(tap(tArrow(), Type1), Type2).

list(Type) -> tap(tList(), Type).

tString() -> list(tChar()).

pair(Type1, Type2) -> tap(tap(tTuple2(), Type1), Type2).

tyvarKind({tyvar,_,Kind}) -> Kind.

tyconKind({tycon,_,Kind}) -> Kind.

typeKind({tcon, Tycon}) -> tyconKind(Tycon);
typeKind({tvar, Tyvar}) -> tyvarKind(Tyvar);
typeKind({tap, Type, _}) ->
  case typeKind(Type) of
    {kfun, _, K} -> K;
    _ -> throw("inconsistent type")
  end;
typeKind({tgen, _}) -> throw("generic type variables have no kind").
