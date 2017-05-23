/*
Featherweight Java on Prolog

Featherweight Javaは形式化されたJavaのシンプルなサブセットです。
Featherweight Java on PrologはProlog上のDSLとしてFeatherweight Javaを実装したものです。
Featherweight Java on Prologの構文はProlog上で実装しやすい形に書き換えてあります:

構文

CL ::=                             クラス宣言：
　class C <: C = { [F:C], K, [M] }

K ::=                              コンストラクタ宣言：
　def(this,[F:C] = { super([F]), [this.>F=F] })

M ::=                              メソッド宣言：
　def(M, [F:C]:C = {T})

T ::=                              項：
　X                                変数
　T.>F                             フィールドアクセス
　T.>(M,[T])                       メソッド呼び出し
　new(C,[T])                       オブジェクト生成
　cast(C,T)                        キャスト

V ::=                              値：
　new(C,[V])                       オブジェクト生成
*/

:- initialization(main).
:- op(999, fx, class).
:- op(920, xfx, [ ==>, *==> ]).
:- op(910, xfx, [ ⊢ ]).
:- op(909, yfx, [ => ]).
:- op(501, xfx, <:).
:- op(11, yfx, .>).
:- op(1200, xfx, [ -- ]).
term_expansion(A -- B, B :- A).


/*
クラスの宣言(CL)にはクラス名(C)を継承したクラス名があります。
また、フィールド(F)が複数、コンストラクタ(K)が1つ、メソッド(M)が複数あります。
コンストラクタ(K)はメソッドの引数[F:C]がフィールド名とクラス名とのペアが複数で、
スーパークラスのコンストラクタを呼びそのあと、追加分のフィールド設定する必要があります。
メソッド(M)は複数の値を受け取り、式を１つだけかけます。
項(T)は変数や、フィールドアクセス、メソッド呼び出し、
オブジェクト生成、キャストがあるだけです。フィールドの更新はできません。
値(V)はオブジェクト生成時のコンストラクタが値となります。
オリジナルのFetherweight Javaの図では上にバーを振って繰り返しを表現していますが、
テキストに書く場合は[]で括ることにしました。
また、Prologで処理できるように演算子定義をおこない、若干Scalaに近い文法に修正しました。
文法要素は大文字にしました。
*/

% 部分型付け規則 C <: D

% 型のリストが両方とも空なら部分型です
[] <: [].

% リスト内の型の全部が部分型なら部分型です
C <: D, Cs <: Ds
--%----------------
[C|Cs] <: [D|Ds].

% C は C の部分型という式です。
C <: C.

% 全ての型はObjectの部分型です。
_ <: 'Object'.


% CがDの部分型かつDがEの部分型ならCはEの部分型です。
class C <: D ={_}, D <: E
--%----------------------
C <: E.


% フィールドの探索 fields(C, [f:C])

% Objectのフィールドはありません。
fields('Object',[]).

% Cのフィールドはクラステーブルの中身のクラスの中のスーパークラスのフィールドとクラスのフィールドを合わせたものです。

class C <: D = {FCs, _, _},
fields(D, GDs), append(GDs, FCs, FCs_) 
--%-----------------------------------
fields(C, FCs_).

% メソッドの型の探索 mtype(M,C,[C] -> C)

% 補助述語
split([], [], []).
split([X:Y|Ls],[X|Xs],[Y|Ys]) :- split(Ls,Xs,Ys).

% メソッドの型の探索はmtype(メソッド名,クラス名, 型)

% クラステーブルからクラスを取り出し、
% Ms内のメソッドを取得し、
% 引数の型からメソッドのリターンの型になります。

class C <: _ = {_, _, Ms},
member(def(M, Args:B={_}), Ms),
split(Args,_,Bs)
--%-------------------------------------
mtype(M,C,Bs -> B). 

% MがMsの中になかった場合は、スーパークラスを検索する
class C <: D = {_}, mtype(M,D,T)
--%-------------------------------------
mtype(M,C,T).


% メソッドの本体の探索 mbody(M,C,([X],T))

% 型の代わりにメソッドの本体の引数と式のペアを返す
class C <: _ = {_, _, Ms},
member(def(M, Args:_ = {T}), Ms),
split(Args,Xs,_)
--%-------------------------------------
mbody(M, C, (Xs,T)).

class C <: D = {_}, mbody(M,D,T)
--%-------------------------------------
mbody(M,C,T).


% メソッドの正当なオーバーライド override(M, D, [C]->C0)
% ただしく、オーバーライドされているかどうかはoverrideで判定できます。

\+ mtype(M, D, _)
--%-------------------------------------
override(M, D, _ -> _).

mtype(M,D,Ds->D0), Cs = Ds, C0 = D0
--%-------------------------------------
override(M, D, Cs -> C0).

% 項の型付け Γ ⊢ T : C

atom(X),
log('T-Var':X),
member(X : C, Γ),
log('T-Var End'),!
--%------------------------------------- (T-Var)
Γ ⊢ X : C.

log('T-Field'),
Γ ⊢ T0 : C0, fields(C0, FCs),
member(Fi:Ci,FCs),!
--%------------------------------------- (T-Field)
Γ ⊢ T0.>Fi : Ci.

log('T-Invk'),
Γ ⊢ T0 : C0,
mtype(M, C0, Ds -> C),
Γ ⊢ Ts : Cs, Cs <: Ds,!
--%------------------------------------- (T-Invk)
Γ ⊢ T0.>(M, Ts) : C.

log('T-New'),
fields(C, FDs),
Γ ⊢ Ts : Cs, split(FDs, _, Ds), log("check subtyping"=Cs <: Ds), Cs <: Ds,
log('T-New':C),!
--%------------------------------------- (T-New)
Γ ⊢ new(C, Ts) : C.

log('T-UCast'),
Γ ⊢ T0 : D, D <: C,!
--%------------------------------------- (T-UCast)
Γ ⊢ cast(C, T0) : C.

log('T-DCast'),
Γ ⊢ T0 : D, C <: D, C \= D,!
--%------------------------------------- (T-DCast)
Γ ⊢ cast(C, T0) : C.

log('T-SCast'),
Γ ⊢ T0 : D, \+ C <: D, \+ C <: D,
format('愚かさの警告 ~w と ~w はキャストできません\n',[C,D]),!
--%------------------------------------- (T-SCast)
Γ ⊢ cast(C, T0) : C.

!
--%------------------------------------- (T-List0)
_ ⊢ [] : [].

log('T-List1'=[T|Ts]),
Γ ⊢ T : C, Γ ⊢ Ts : Cs,
log('T-List1 End':[C|Cs]),!
--%------------------------------------- (T-List1)
Γ ⊢ [T|Ts] : [C|Cs].

% メソッドの型付け okas(C, M)

[this:C|Args] ⊢ T0:E0, E0 <: C0,
class C <: D = {_},
split(Args,_,Cs),override(M,D,Cs->C0)
--%-------------------------------------
okas(C, def(M, Args:C0 = {T0})).

% クラスの型付け ok(C)

thisffs(this.>F = F).


K= def(this,Args = { super(Gs), ThisFFs }),
maplist(thisffs,ThisFFs),
fields(D,GDs),split(GDs,Gs,_),
append(GDs,FCs,Args),maplist(okas(C), Ms),!
--%-------------------------------------
ok(class C <: D = {FCs, K, Ms}).

ok1(Class) :- ok(Class),!; Class=(class C <: _ = _), format('error class ~W\n',[C,[]]),fail.

okall :-
	findall(class C,class C,Cs),!,maplist(ok1,Cs),!.

% 置き換え

subst(S->D,S->D) :- atom(S).
subst([]->[],T->T).
subst([S|Ss]->[D|Ds],T->T_) :- subst(S->D,T->T1), subst(Ss->Ds,T1->T_).
subst(_->_,X->X) :- atom(X).
subst(_->_,[]->[]).
subst(S->D,[X|Xs]->[X_|Xs_]) :- subst(S->D,X->X_), subst(S->D,Xs->Xs_).
subst(S->D,(T.>(M,Ts))->(T_.>(M,Ts_))) :- subst(S->D,T->T_),subst(S->D,Ts->Ts_).
subst(S->D,(T.>F)->(T_.>F)) :- subst(S->D,T->T_).
subst(S->D,new(C,Ts)->new(C,Ts_)) :- subst(S->D,Ts->Ts_).
subst(S->D,cast(C,T)->cast(C,T_)) :- subst(S->D,T->T_).
isVal(new(C,Vs)) :- atom(C),isVals(Vs).
isVals(Vs) :- maplist(isVal, Vs).

% 値と項を分離する
splitVals([],[],[]).
splitVals([V|Ts],[V|Vs],Ts_) :- isVal(V), splitVals(Ts,Vs,Ts_).
splitVals(Ts,[],Ts).

% 評価 T ==> T_

isVals(Vs),atom(Fi),fields(C,FCs),
nth0(I,FCs,(Fi:_)),nth0(I,Vs,Vi),!
--%------------------------------------- (E-ProjNew)
new(C, Vs).>Fi ==> Vi.

isVals(Vs),atom(M),mbody(M,C,(Xs, T0)),
subst([this|Xs]->[new(C,Vs)|Us], T0 -> T0_),!
--%------------------------------------- (E-InvkNew)
new(C,Vs).>(M,Us) ==> T0_.

isVals(Vs), C <: D,!
--%------------------------------------- (E-CastNew)
cast(D, new(C, Vs)) ==> new(C, Vs).

atom(F), T0 ==> T0_,!
--%------------------------------------- (E-Field)
T0.>F ==> T0_.>F.

atom(M), T0 ==> T0_,!
--%------------------------------------- (E-Invk-Recv)
T0.>(M,Ts) ==> T0_.>(M,Ts).

atom(M), splitVals(Args,Vs,[Ti|Ts]),
Ti ==> Ti_,
append(Vs,[Ti_|Ts],Args_),!
--%------------------------------------- (E-Invk-Arg)
V0.>(M,Args) ==> V0.>(M,Args_).

atom(C), splitVals(Args,Vs,[Ti|Ts]),
Ti ==> Ti_,
append(Vs,[Ti_|Ts],Args_),!
--%------------------------------------- (E-New-Arg)
new(C,Args) ==> new(C,Args_).

atom(C), T0 ==> T0_,!
--%------------------------------------- (E-Cast)
cast(C, T0) ==> cast(C, T0_).

E1 *==> E3 :- E1 ==> E2,!, E2 *==> E3.
E1 *==> E1.

% クラス定義

class 'B' <: 'Object' = {[],def(this,[]={super([]),[]}),[]}.
class 'C' <: 'Object' = {[],def(this,[]={super([]),[]}),[]}.
class 'A' <: 'B' = {[],def(this,[]={super([]),[]}),[]}.
class 'Pair' <: 'B' = {
	[fst:'B',snd:'B'],
	def(this,[fst:'B',snd:'B']={
		super([]),[this.>fst=fst,this.>snd=snd]
	}),
	[
		def(getFst,[]:'B' = {
			this.>fst
		}),
		def(setFst,[fst:'B']:'Pair' = {
			new('Pair',[fst,this.>snd])
		}),
		def(setSnd,[snd:'B']:'Pair' = {
			new('Pair',[this.>fst,snd])
		})
	]
}.
class 'Triple' <: 'Pair' = {
	[thr:'B'],
	def(this,[fst:'B',snd:'B',thr:'B']={
		super([fst,snd]),[this.>thr=thr]
	}),
	[
		def(setFst,[fst:'B']:'Pair' = {
			new('Triple',[fst,this.>snd,this.>thr])
		})
	]
}.

log(_).
%log(A) :- writeln(A).

test(E) :-
  
  ([] ⊢ E : T,!, format('~W : ~W = ',[E,[],T,[]]);format('~W : type error\n',[E,[]]),fail),
  (E *==> V, writeln(V),isVal(V); writeln('runtime error'));!.

main :- (okall,writeln('all classes valid.'); writeln('invalid classess.'),halt),
  test(new('Pair',[new('A',[]),new('B',[])]).>fst),
  test(new('Pair',[new('Pair',[new('A',[]),new('B',[])]).>snd,new('B',[])]).>fst),
  test(new('Pair',[new('A',[]),new('B',[])]).>(getFst,[])),
  test(new('Pair',[new('A',[]),new('B',[])]).>(setFst,[new('B',[])])),
  test(new('Triple',[new('A',[]),new('B',[]),new('B',[])]).>(getFst,[])),
  test(new('Triple',[new('A',[]),new('B',[]),new('B',[])]).>(setFst,[new('B',[])])),
  test(new('Triple',[new('A',[]),new('B',[]),new('B',[])]).>(setSnd,[new('A',[])])),
  test(cast('Object',new('Object',[]))),
  test(cast('Pair',new('Object',[])).>fst),
  test(cast('C',new('C',[]))),
  test(cast('A',new('B',[]))),
  test(cast('B',new('A',[]))),
  test(cast('C',new('B',[]))), % warning
  test(new('Pair',[new('Pair',[new('A',[]),new('B',[])]),new('B',[])]).>fst.>fst),
  test(cast('Pair',new('Pair',[new('Pair',[new('A',[]),new('B',[])]),new('B',[])]).>fst).>fst),

  halt.
