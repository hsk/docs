% 7 Type Classes, Predicates and Qualified Types
-module(pred).
-export([
  isin/2,
  qual/2,
  predApply/2,
  predsApply/2,
  qualTypeApply/2,
  predTv/1,
  predsTv/1,
  qualTypeTv/1,
  lift/3,
  mguPred/2,
  matchPred/2,
  classEnv/2,
  ':=>'/2,
  arrow/2,
  super/2,
  insts/2,
  defined/2,
  modify/3,
  initialEnv/0,
  '<:>'/2,
  addEnv/2,
  addClass/2,
  addCoreClasses/0,
  addNumClasses/0,
  addPreludeClasses/0,
  overlap/2,
  addInst/2,
  exampleInsts/0,
  bySuper/2,
  byInst/2,
  entail/3,
  inHnf/1,
  toHnfs/2,
  toHnf/2,
  simplify/2,
  reduce/2,
  scEntail/3
]).

%   import Kind._
%   import Type._
%   import Subst._
% 
% 7.1 Basic definitions

%   type Pred = IsIn
isin(Id,Type) -> {isin, Id, Type}.

qual(Preds, T) -> {qual, Preds, T}.

predApply(Subst, {isin, Id, T}) ->
	isin(Id, subst:typeApply(Subst, T)).

predsApply(Subst,Preds) ->
  subst:listApply(fun predApply/2, Subst, Preds).

qualTypeApply(Subst, {qual, Ps, T}) ->
  pred:qual(predsApply(Subst, Ps), subst:typeApply(Subst, T)).

predTv({isin, _, T}) -> subst:typeTv(T).

predsTv(Preds) -> subst:listTv(fun predTv/1, Preds).

qualTypeTv({qual,Ps, T}) ->
  pre:union(predsTv(Ps),subst:typeTv(T)).

lift(M,{isin, I, T}, {isin, I_, T_}) when I == I_ -> M(T,T_);
lift(_,_,_) -> throw("classes differ").

mguPred(Pred, Pred2) ->
  lift(fun unify:mgu/2, Pred, Pred2).

matchPred(Pred, Pred2) ->
  lift(fun unify:match/2, Pred, Pred2).

%   type Inst = Qual[Pred]
%   type Class_ = (List[Id.Id], List[Inst])

% 7.2 Class Environments

%   case class ClassEnv(
%     classes: Id.Id => Class_,
%     defaults: List[Type_])

classEnv(Id2Class,Types) -> {classEnv, Id2Class, Types}.

% :=>
':=>'(Preds, T) -> qual(Preds, T).
arrow(Preds, T) -> qual(Preds, T).

super({classEnv,Classes,_}, Id) -> element(1, Classes(Id)).

insts({classEnv,Classes,_}, Id) -> element(2, Classes(Id)).

defined({classEnv,Classes,_},Id) ->
  try Classes(Id) of
    _ -> true
  catch
    _ -> false
  end.

modify({classEnv, Classes, Defaults}, Id, Class)->
  {classEnv,
    fun (J) ->
      if
        (Id == J) -> Class;
        true      -> Classes(J)
      end
    end,
    Defaults
  }.

initialEnv() ->
  {classEnv,
    fun(_)-> throw("Not found")end,
    [type:tInteger(), type:tDouble()]
  }.
  

%  type EnvTransformer = ClassEnv => ClassEnv

% <:>
'<:>'(F,G) -> addEnv(F,G).

addEnv(F,G) ->
  fun (ClassEnv) -> 
    G(F(ClassEnv))
  end.

addClass(Id, Ids) ->
  fun (ClassEnv) ->
    case defined(ClassEnv, Id) of
    	true -> throw("class already defined");
      false ->
        case lists:any(
          fun(I) -> not defined(ClassEnv, I) end,
          Ids
        ) of
          true -> throw("superclass not defined");
          false -> modify(ClassEnv, Id, {Ids, []})
        end
    end
  end.

addCoreClasses() ->
  pre:fold_left1(fun addEnv/2, [
    addClass("Eq", []),
    addClass("Ord",["Eq"]),
    addClass("Show",[]),
    addClass("Read",[]),
    addClass("Bounded",[]),
    addClass("Enum",[]),
    addClass("Functor",[]),
    addClass("Monad",[])
  ]).

addNumClasses() ->
  pre:fold_left1(fun addEnv/2, [
    addClass("Num",["Eq", "Show"]),
    addClass("Real",["Num", "Ord"]),
    addClass("Fractional",["Num"]),
    addClass("Integral",["Real", "Enum"]),
    addClass("RealFrac",["Real", "Fractional"]),
    addClass("Floating",["Fractional"]),
    addClass("RealFloat",["RealFrac", "Floating"])
  ]).

addPreludeClasses() ->
  addEnv(addCoreClasses(), addNumClasses()).

overlap(Pred1, Pred2) ->
  try mguPred(Pred1, Pred2) of
    _ -> true
  catch
    _ -> false
  end.


addInst(Preds, Pred) ->
  fun (ClassEnv) ->
    {isin, I, _ } = Pred,
    case not defined(ClassEnv, I) of
      true -> throw("no class for instance");
      false ->
        Its = insts(ClassEnv, I),
        Qs = lists:map(fun({qual, _, Q}) -> Q end, Its),
        case lists:any(fun (Q) -> overlap(Pred, Q) end, Qs) of
          true  -> throw("overlapping instance");
          false ->
            C = {super(ClassEnv, I), [pred:qual(Preds, Pred) | Its] },
            modify(ClassEnv, I, C)
        end
    end
  end.

exampleInsts() ->
  pre:fold_left1(
    fun '<:>'/2,
    [
      addPreludeClasses(),
      addInst([],{isin, "Ord", type:tUnit()}),
      addInst([],{isin, "Ord", type:tChar()}),
      addInst([],{isin, "Ord", type:tInt()}),
      addInst(
        [
          {isin, "Ord", type:tvar(type:tyvar("a", kind:star()))},
          {isin, "Ord", type:tvar(type:tyvar("b", kind:star()))}
        ],
        {isin, "Ord",
          type:pair(
          	type:tvar(type:tyvar("a", kind:star())),
            type:tvar(type:tyvar("b", kind:star())))})
    ]
  ).

% 7.3 Entailment

bySuper(ClassEnv, Pred) ->
  {isin, I, T} = Pred,
  PSS = lists:map(
    fun(I2) ->
      bySuper(ClassEnv, pred:isin(I2, T))
    end,
    super(ClassEnv, I)
  ),
  [Pred | lists:concat(PSS)].

byInst(ClassEnv, Pred) ->
  {isin, I, _} = Pred,
  TryInst = fun({qual, PS, H}) ->
    try matchPred(H, Pred) of
      U ->
        {ok, lists:map(fun(P) -> predApply(U, P) end, PS)}
    catch
      _ -> ng
    end
  end,
  msum(lists:map(TryInst, insts(ClassEnv, I))).

msum([]     ) -> ng;
msum([ng|XS]) -> msum(XS);
msum([X | _]) -> X.

entail(ClassEnv, Preds, Pred) ->
  Preds2 = lists:map(fun(P)->bySuper(ClassEnv, P)end, Preds),
  lists:any(fun(L) -> lists:member(Pred, L) end, Preds2) or
    case byInst(ClassEnv, Pred) of
      {ok, QS} ->
        lists:forall(fun(P) -> entail(ClassEnv, Preds, P) end, QS);
      _ -> false
    end.

% 7.4 Context Reduction

inHnf(Pred) ->
  {isin, _, T} = Pred,
  hnf(T).

hnf({tvar,_}  ) -> true;
hnf({tcon,_}  ) -> false;
hnf({tap,T, _}) -> hnf(T);
hnf({tgen,_}  ) -> throw("context reduction on generic variable").


toHnfs(ClassEnv, Preds) ->
  lists:concat(lists:map(fun(Pred) -> toHnf(ClassEnv, Pred)end, Preds)).

toHnf(ClassEnv, Pred) ->
  case inHnf(Pred) of
    true -> [Pred];
    _    ->
      case byInst(ClassEnv, Pred) of
        {ok, PS} -> toHnfs(ClassEnv, PS);
        _        -> throw("context reduction")
      end
  end.

simplify(ClassEnv, Preds) ->
  loop(ClassEnv,[], Preds).

loop(_, RPreds, []) -> RPreds;
loop(ClassEnv, RPreds, [P | Ps]) ->
  case entail(ClassEnv, lists:append(RPreds, Ps), P) of
    true -> loop(ClassEnv, RPreds, Ps);
    _    -> loop(ClassEnv, [P | RPreds], Ps)
  end.

reduce(ClassEnv, Preds) ->
  simplify(ClassEnv, toHnfs(ClassEnv, Preds)).

scEntail(ClassEnv, Preds, Pred) ->
  Ls = lists:map(
    fun(P)->bySuper(ClassEnv, P)end,
    Preds
  ),
  lists:any(
    fun (P1)-> lists:member(Pred, P1) end,
    Ls
  ).
