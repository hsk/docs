%%%
%%%  Sequent Calculus Prover for SICStus Prolog
%%%  by Naoyuki Tamura (tamura@kobe-u.ac.jp)
%%%     Dept. Computer & Systems Eng., Kobe Univ., Japan
%%%  TeX form output by Eiji Sugiyama (eiji@grad306c.scitec.kobe-u.ac.jp)
%%%
%%%  You can freely distribute or modify this program.
%%%

:- dynamic(
%	seq_system/2, threshold/1, output_form/1, logging/2,
%	output_count/1,
	output_result/2,
	proved/3, not_proved/2, axiom/1,

%	assert/1,
	dif/2,
  	freeze/2,
  	open_null_stream/1,
  	prolog_flag/3,
  	tab/2
).


seq_system(cl,first).
threshold(5).
output_form(pretty).
logging(no, _).
output_count(1).

assert(A) :- asserta(A).

:- op(1200, xfx, [ -->, <--> ]).
:- op( 900, xfy, [ -> ]).
:- op( 850, xfy, [ /\, \/ ]).
:- op( 800, fy,  [ ~ ]).
:- op( 750, xfy, [ @, # ]).

prover :-
	writex('Sequent Calculus Prover ver 1.0 for SICStus Prolog'), nlx,
	writex('        by Naoyuki Tamura (tamura@kobe-u.ac.jp)'), nlx,
%	seq_init,
	statistics(runtime, [T0,_]),
	seq_prover,
	statistics(runtime, [T1,_]),
	T is T1-T0,
	writex('Exit from Sequent Calculus Prover...'), nlx,
	writex('Total CPU time = '), writex(T), writex(' msec.'), nlx,
	log_end.

seq_init :-
	set_system(cl,first),
	set_threshold(5),
	set_axioms([]),
	set_output_form(pretty),
	log_end,
	reset_output_result.

seq_prover :-
	current_input(STR),
	seq_prover(STR).

seq_prover(STR) :-
	repeat,
	  seq_system(Sys, Opt),
	  writex(Sys), writex('('), writex(Opt), writex(')'),
	  writex('> '),
	  readx(STR, X),
	  seq_command(X),
	seq_quit(X),
	close(STR).

seq_quit(X) :-
	nonvar(X),
	memberq(X, [quit, end, bye, halt, end_of_file]).

seq_command(X) :-
	seq_execute(X),
	!,
	writex(yes), nlx.
seq_command(_) :-
	writex(no), nlx.

seq_execute(X) :- var(X), !, fail.
seq_execute(X) :- seq_quit(X), !.
seq_execute(help) :- !, seq_help.
seq_execute(init) :- !, seq_init.
seq_execute([File]) :- !, seq_consult(File).
seq_execute(cl(Opt)) :- !, set_system(cl,Opt).
%seq_execute(il(Opt)) :- !, set_system(il,Opt).
seq_execute(threshold(N)) :- var(N), !,
	threshold(N),
	writex(threshold(N)), nlx.
seq_execute(threshold(N)) :- !,
	set_threshold(N).
seq_execute(axioms(As)) :- var(As), !,
	(bagof(A, axiom(([]-->[A])), As); As=[]), !,
	writex(axioms(As)), nlx.
seq_execute(axioms(As)) :- !,
	set_axioms(As).
seq_execute(output(Form)) :- var(Form), !,
	output_form(Form),
	writex(output(Form)), nlx.
seq_execute(output(Form)) :-
	set_output_form(Form).
seq_execute(log(File)) :- var(File), !,
	logging(File, _),
	writex(log(File)), nlx.
seq_execute(log(no)) :- !,
	log_end.
seq_execute(log(File)) :- !,
	log_start(File).
seq_execute(N) :- integer(N), !,
	get_output_result(N, N1, P),
	print_output(N1, P).
seq_execute((Xs <--> Ys)) :- !,
	seq_execute((Xs --> Ys)),
	seq_execute((Ys --> Xs)).
seq_execute((Xs --> Ys)) :-
	convert_to_seq((Xs --> Ys), Seq),
	sequentq(Seq),
	!,
	seq_prove(Seq).
seq_execute(X) :-
	writex('Error '),
	writex(X),
	writex(' is not a command nor a sequent.'), nlx.

seq_help :-
	writex('A1,...,Am -->  B1,...,Bn : Try to prove the sequent.'), nlx,
	writex('A1,...,Am <--> B1,...,Bn : Try to prove the sequent '),
	    writex('in both directions.'), nlx,
	writex('help           : Help.'), nlx,
	writex('init           : Initialize the prover.'), nlx,
	writex('[File]         : Read from the file.'), nlx,
	writex('Sys(Opt)       : Select the system. '),
	    writex('Sys={cl}, Opt={first|prop}'), nlx,
	writex('threshold(N)   : Set/retrieve the threshold. N>=0'), nlx,
	writex('axioms(Axioms) : Set/retrieve the axioms. '), 
	    writex('Axioms=[A1,...,Am]'), nlx,
	writex('output(Form)   : Set/retrieve the output form. '), 
	    writex('Form={pretty|tex|standard|term}'), nlx,
	writex('log(L)         : Start/stop/retrieve the output logging. '), 
	    writex('L={yes|File|no}'), nlx,
	writex('N              : Display the N-th output.'), nlx,
	writex('quit           : Quit.'), nlx.

seq_consult(File) :-
	open(File, read, STR),
	seq_prover(STR).

seq_prove(Seq) :-
	statistics(runtime,[_,_]),
	prove(Seq, Proof),
	statistics(runtime,[_,Time]),
	writex('Succeed in proving '), 
	write_seq(Seq),
	writex(' ('), writex(Time), writex(' msec.)'), nlx,
	output_count(N),
	assert_output_result(Proof),
	!,
	print_output(N, Proof).
seq_prove(Seq) :-
	statistics(runtime,[_,Time]),
	writex('Fail to prove '),
	write_seq(Seq),
	writex(' ('), writex(Time), writex(' msec.)'), nlx.

print_output(N, P) :-
	output_form(Form), writex(Form), writex(':'),
	writex(N), writex(' ='), nlx,
	print_proof(P).

print_proof(P) :-
	output_form(Form),
	print_proof_form(Form, P).

print_proof_form(pretty, P) :- !,
	pretty_print(P).
print_proof_form(tex, P) :- !,
	tex_print(P).
print_proof_form(standard, P) :- !,
	standard_print(P).
print_proof_form(term, P) :- !,
	writex(P), nlx.

set_system(Sys, Opt) :-
	memberq(Sys, [cl]),
	memberq(Opt, [prop,first]),
	retractall(seq_system(_,_)),
	assert(seq_system(Sys,Opt)).

set_threshold(N) :-
	integer(N), N >= 0,
	retractall(threshold(_)),
	assert(threshold(N)).

set_axioms(Axioms) :-
	retractall(axiom(_)),
	assert_axioms(Axioms).

assert_axioms([]).
assert_axioms([A|As]) :-
	formulaq(A),
	!,
	assert(axiom(([]-->[A]))),
	assert_axioms(As).
assert_axioms([A|As]) :-
	writex('Error '), writex(A),
	writex(' is not a formula.'), nlx,
	assert_axioms(As).

set_output_form(Form) :-
	memberq(Form, [pretty, tex, standard, term]),
	retractall(output_form(_)),
	assert(output_form(Form)).

reset_output_result :-
	retractall(output_count(_)),
	retractall(output_result(_,_)),
	assert(output_count(1)).

assert_output_result(P) :-
	output_count(N),
	assert(output_result(N,P)),
	retractall(output_count(_)),
	N1 is N+1,
	assert(output_count(N1)).

get_output_result(N, N, P) :- N>0, !,
	output_result(N, P).
get_output_result(N, N1, P) :-
	output_count(M),
	N1 is M+N,
	output_result(N1, P).

convert_to_seq((Xs --> Ys), (Xs1 --> Ys1)) :-
	convert_to_list(Xs, Xs1), convert_to_list(Ys, Ys1).

convert_to_list(X, _) :- var(X), !, fail.
convert_to_list([], []) :- !.
convert_to_list([X|Xs], [X|Xs]) :- !.
convert_to_list((X,Xs), [X|Zs]) :- !, convert_to_list(Xs, Zs).
convert_to_list(X, [X]).

sequentq((X-->Y)) :- formula_listq(X), formula_listq(Y).

formula_listq([]).
formula_listq([A|As]) :- formulaq(A), formula_listq(As).

formulaq(A) :- var(A), !, fail.
formulaq((_-->_)) :- !, fail.
formulaq([]) :- !, fail.
formulaq([_|_]) :- !, fail.
formulaq(~A) :- !, formulaq(A).
formulaq(A/\B) :- !, formulaq(A), formulaq(B).
formulaq(A\/B) :- !, formulaq(A), formulaq(B).
formulaq(A->B) :- !, formulaq(A), formulaq(B).
formulaq(_X@A) :- !, formulaq(A).
formulaq(_X#A) :- !, formulaq(A).
formulaq(_).

%%
%%  Sequent Calculus Prover
%%
%%  seq_system( {cl}, {prop|first} ).
%%    seq_system(cl,first).
%%  Max number of cut and contraction rules: cut, r(all), l(exists)
%%    threshold(5).
%%  Axioms
%%    axiom(Ax1). axiom(Ax2). ....

prove(S, P) :-
	retractall(proved(_,_,_)),
	retractall(not_proved(_,_)),
	threshold(N),
	writex('Trying to prove with threshold ='),
	for2(I, 0, N),
	writex(' '), writex(I),
	flush_out,
	prove(S, P, I),
	nlx,
	!.
prove(_S, _P) :-
	nlx,
	fail.

prove(S, _P, N) :-
	ground(S),
	clause(not_proved(S,M), _),
	N =< M,
	!,
	fail.
prove(S, P, N) :-
	ground(S),
	clause(proved(S,P,M), _),
	M =< N,
	!.
prove(S, P, _N) :-
	clause(axiom(S), _),
	P = [axiom,[[]],S],
	!.
prove(S, P, N) :-
	check_sequent(S),
	select_rule(Rule, S, Ss, Pos),
	P = [Rule,Pos,S|Ps],
	set_sequents(Ss, Ps),
	rule_constraint(Rule, Ps, M),
	N1 is N-M, N1 >= 0,
       	prove_all(Ss, Ps, N1),
%	!,
	assert(proved(S,P,N)).
prove(S, _P, N) :-
	ground(S),
	assert(not_proved(S,N)),
	!,
	fail.

prove_all([], [], _N).
prove_all([S|Ss], [P|Ps], N) :-
	prove(S, P, N),
	prove_all(Ss, Ps, N).

check_sequent((X-->Y)) :-
	length(X, N1), length(Y, N2),
	N1 + N2 < 20.
%check_sequent(_) :- seq_system(cl,_), !.
%check_sequent((_X-->Y)) :- (Y=[]; Y=[_]), !.

select_rule(Rule, S, Ss, Pos) :-
	rule(RuleSys, inv, Rule, S, Ss, Pos),
	check_rule(RuleSys),
	!.
select_rule(Rule, S, Ss, Pos) :-
	rule(RuleSys, no, Rule, S, Ss, Pos),
	check_rule(RuleSys).

check_rule([RSys,ROp]) :-
	seq_system(Sys, Op),
	check_rule1(Sys, RSys),
	check_rule2(Op, ROp),
	!.

check_rule1(Sys, Sys).
check_rule1(cl, _).

check_rule2(Op, Op).
check_rule2(first, _).

set_sequents([], []).
set_sequents([S|Ss], [[_,_,S|_]|Ps]) :- set_sequents(Ss, Ps).

rule_constraint(cut, [P1,_P2], 1) :- !,
	axiom(S1),
	P1 = [axiom,_,S1].
rule_constraint(l(all), [P1], 1) :- !,
	P1 = [NextRule,_,_|_],
	dif(NextRule, l(all)).
rule_constraint(r(exists), [P1], 1) :- !,
	P1 = [NextRule,_,_|_],
	dif(NextRule, r(exists)).
rule_constraint(_Rule, _, 0).

% Logical axiom
% cannot be invertible owing to unification
rule([cl,prop], no, ax, S, [], [[]]) :-
	match(S,  ([_X1,[A],_X2]-->[_Y1,[A],_Y2])).
% Rules for the propositional constants
rule([cl,prop], inv, l(top), S, [S1], [l(N),[]]) :-
	match(S,  ([X1,[top],X2]-->[Y])),
	match(S1, ([X1,X2]-->[Y])),
	length(X1, N).
rule([cl,prop], inv, r(top), S, [], [r(N)]) :-
	match(S,  ([_X]-->[Y1,[top],_Y2])),
	length(Y1, N).
rule([cl,prop], inv, l(bot), S, [], [r(N)]) :-
	match(S,  ([X1,[bot],_X2]-->[_Y])),
	length(X1, N).
rule([cl,prop], inv, r(bot), S, [S1], [r(N),[]]) :-
	match(S,  ([X]-->[Y1,[bot],Y2])),
	match(S1, ([X]-->[Y1,Y2])),
	length(Y1, N).
% Rules for the propositional connectives
rule([cl,prop], inv, l(~), S, [S1], [l(N),r(0)]) :-
	match(S,  ([X1,[(~A)],X2]-->[Y])),
	match(S1, ([X1,X2]-->[[A],Y])),
	length(X1, N).
rule([cl,prop], inv, r(~), S, [S1], [r(N),l(0)]) :-
	match(S,  ([X]-->[Y1,[~A],Y2])),
	match(S1, ([[A],X]-->[Y1,Y2])),
	length(Y1, N).
rule([cl,prop], inv,  l(/\), S, [S1], [l(N),[l(N),l(N1)]]) :-
	match(S,  ([X1,[A/\B],X2]-->[Y])),
	match(S1, ([X1,[A,B],X2]-->[Y])),
	length(X1, N), N1 is N+1.
rule([cl,prop], inv,  r(\/), S, [S1], [r(N),[r(N),r(N1)]]) :-
	match(S,  ([X]-->[Y1,[A\/B],Y2])),
	match(S1, ([X]-->[Y1,[A,B],Y2])),
	length(Y1, N), N1 is N+1.
rule([cl,prop], inv, r(->), S, [S1], [r(N),[l(0),r(N)]]) :-
	match(S,  ([X]-->[Y1,[A->B],Y2])),
	match(S1, ([[A],X]-->[Y1,[B],Y2])),
	length(Y1, N).
rule([cl,prop], inv, r(/\), S, [S1, S2], [r(N),r(N),r(N)]) :-
	match(S,  ([X]-->[Y1,[A/\B],Y2])),
	match(S1, ([X]-->[Y1,[A],Y2])),
	match(S2, ([X]-->[Y1,[B],Y2])),
	length(Y1, N).
rule([cl,prop], inv, l(\/), S, [S1, S2], [l(N),l(N),l(N)]) :-
	match(S,  ([X1,[A\/B],X2]-->[Y])),
	match(S1, ([X1,[A],X2]-->[Y])),
	match(S2, ([X1,[B],X2]-->[Y])),
	length(X1, N).
rule([cl,prop], inv,  l(->), S, [S1, S2], [l(N),r(0),l(N)]) :-
	match(S,  ([X1,[A->B],X2]-->[Y])),
	match(S1, ([X1,X2]-->[[A],Y])),
	match(S2, ([X1,[B],X2]-->[Y])),
	length(X1, N).
% Rules for the quantifiers
rule([cl,first], no,  l(all), S, [S1], [l(N),[l(N),l(N1)]]) :-
	match(S,  ([X1,[V@A],X2]-->[Y])),
	substitute(V, _V1, A, A1),
	match(S1, ([X1,[A1,V@A],X2]-->[Y])),
	length(X1, N), N1 is N+1.
rule([cl,first], no, r(all), S, [S1], [r(N),r(N)]) :-
	match(S,  ([X]-->[Y1,[V@A],Y2])),
	substitute(V, V1, A, A1),
	match(S1, ([X]-->[Y1,[A1],Y2])),
	eigen_variable(V1, S),
	length(Y1, N).
rule([cl,first], no, l(exists), S, [S1], [l(N),l(N)]) :-
	match(S,  ([X1,[V#A],X2]-->[Y])),
	substitute(V, V1, A, A1),
	match(S1, ([X1,[A1],X2]-->[Y])),
	eigen_variable(V1, S),
	length(X1, N).
rule([cl,first], no,  r(exists), S, [S1], [r(N),[r(N),r(N1)]]) :-
	match(S,  ([X]-->[Y1,[V#A],Y2])),
	substitute(V, _V1, A, A1),
	match(S1, ([X]-->[Y1,[A1,V#A],Y2])),
	length(Y1, N), N1 is N+1.
% Cut rule
rule([cl,prop], no,  cut, S, [S1, S2], [[],r(0),l(0)]) :-
	match(S,  ([X]-->[Y])),
	match(S1, ([]-->[[C]])),
	match(S2, ([[C],X]-->[Y])).

match((X-->Y), (P-->Q)) :- append_all(P, X), append_all(Q, Y).

eigen_variable(V, X) :-
	freeze(V, fail),
	free_variables(X, Us),
	dif_list(Us, V).

free_variables(X, Vs) :- free_vars(X, [], [], Vs).

free_vars(X, BVs, Vs0, Vs) :- var(X), !, free_vars1(X, BVs, Vs0, Vs).
free_vars(X, _BVs, Vs, Vs) :- ground(X), !.
free_vars([X|Y], BVs, Vs0, Vs) :- !,
	free_vars(X, BVs, Vs0, Vs1), free_vars(Y, BVs, Vs1, Vs).
free_vars(all(X,A), BVs, Vs0, Vs) :- !,
	free_vars(A, [X|BVs], Vs0, Vs).
free_vars(exists(X,A), BVs, Vs0, Vs) :- !,
	free_vars(A, [X|BVs], Vs0, Vs).
free_vars(X, BVs, Vs0, Vs) :- X =.. Y, free_vars(Y, BVs, Vs0, Vs).

free_vars1(X, BVs, Vs, Vs) :- (memq(X, BVs); memq(X, Vs)), !.
free_vars1(X, _BVs, Vs, [X|Vs]).

dif_list([], _).
dif_list([U|Us], V) :- dif(U, V), dif_list(Us, V).

%%
%%  Standard form printer
%%
standard_print(P) :-
	standard_print(P, 0).

standard_print([Rule,_,S|Ps], Tab) :-
	tabx(Tab),
	write_rule_name(Rule), writex(': '), 
	write_seq(S), nlx,
	Tab1 is Tab+2,
	standard_print_list(Ps, Tab1).

standard_print_list([], _).
standard_print_list([P|Ps], Tab) :-
	standard_print(P, Tab), standard_print_list(Ps, Tab).

%%
%%  Pretty form printer
%%
pretty_print(P0) :-
	name_variables(P0, P),
	place_proof(P, [], Q, _, _),
	print_placed_proof(Q),
	fail.
pretty_print(_).

name_variables(X, Z) :-
	name_variables(X, 0, _N, [], _Vs, Z).

name_variables(X, N, N, Vs, Vs, X) :- ground(X), !.
name_variables(X, N, N, Vs, Vs, Z) :- var(X), assoc(X, Z, Vs), !.
name_variables(X, N0, N, Vs0, [[X|Z]|Vs0], Z) :- var(X), !,
	V is N0 mod 3, VN is N0//3,
	name_var(V, VN, Z),
	N is N0+1.
name_variables([X|Xs], N0, N, Vs0, Vs, [Z|Zs]) :- !,
	name_variables(X, N0, N1, Vs0, Vs1, Z),
	name_variables(Xs, N1, N, Vs1, Vs, Zs).
name_variables(X, N0, N, Vs0, Vs, Z) :-
	X =.. Xs,
	name_variables(Xs, N0, N, Vs0, Vs, Zs),
	Z =.. Zs.

name_var(0, VN, X) :- !, name_var1("X", VN, X).
name_var(1, VN, X) :- !, name_var1("Y", VN, X).
name_var(2, VN, X) :- !, name_var1("Z", VN, X).

name_var1([V], 0, X) :- !, name(X, [V]).
name_var1([V], VN, X) :-
	name(VN, Ns), 
	name(X, [V|Ns]).

%%
%%  place_proof(+Proof, +Margins, 
%%              -Proof_with_pos, -LowerLeftPos, -NewMargins)
%% 
place_proof([Rule,_,S|Ps1], Ms0, Q, LowerPos, Ms) :-
	get_margins([M0,M1|Ms1], Ms0),
	place_uppers(Ps1, Ms1, Ps2, UpperPos0, Ms2),
	get_margins([M2|_], Ms2),
	UpperWidth is M2-UpperPos0,
	get_rule_name_width(Rule, W1),
	get_seq_width(S, LowerWidth),
	LowerOff is max(0,max(UpperPos0,max(M1,M0))-UpperPos0),
	RulePos is UpperPos0+LowerOff,
	RuleWidth is max(UpperWidth,LowerWidth),
	LowerPos is RulePos+(RuleWidth-LowerWidth)//2,
	UpperPos is max(UpperPos0,RulePos),
	UpperOff is UpperPos-UpperPos0,
	move_proof_list(Ps2, UpperOff, Ps3),
	move_margins(Ms2, UpperOff, Ms3),
	new_rule_width(Rule, RuleWidth, W1, RuleWidth1, W2),
	Q = [RulePos+RuleWidth1,Rule,LowerPos+LowerWidth,S|Ps3],
	M11 is RulePos+RuleWidth1+W2,
	M01 is LowerPos+LowerWidth,
	Ms = [M01,M11|Ms3].

new_rule_width([_Rule,_], _, W1, 0, W1) :- !.
new_rule_width(_Rule, RW, W1, RW, W2) :- W2 is W1+1.

place_uppers(Ps, Ms0, Qs, T, Ms) :-
	place_proof_list(Ps, Ms0, Qs, T, Ms).

place_proof_list([], Ms0, [], T, Ms0) :- !,
	get_margins([T|_], Ms0).
place_proof_list([P1], Ms0, [Q1], T, Ms) :- !,
	place_proof(P1, Ms0, Q1, T, Ms).
place_proof_list([P1|Ps], Ms0, [Q1|Qs], T, Ms) :-
	place_proof(P1, Ms0, Q1, T, Ms1),
	move_margins(Ms1, 2, Ms2),
	place_proof_list(Ps, Ms2, Qs, _T1, Ms).

move_proof_list([], _, []).
move_proof_list([P0|Ps0], Off, [P|Ps]) :-
	move_proof(P0, Off, P),
	move_proof_list(Ps0, Off, Ps).

move_proof([M0+W0,Rule,M1+W1,S|Ps0], Off, [N0+W0,Rule,N1+W1,S|Ps]) :-
	N0 is M0+Off, N1 is M1+Off,
	move_proof_list(Ps0, Off, Ps).

get_margins(Zs, Xs) :- var(Zs), !, Zs=Xs.
get_margins([], _).
get_margins([Z|Zs], []) :- !, Z=0, get_margins(Zs, []).
get_margins([Z|Zs], [Z|Xs]) :- get_margins(Zs, Xs).

move_margins([], _, []).
move_margins([M0|Ms0], Off, [M|Ms]) :-
	M is M0+Off, move_margins(Ms0, Off, Ms).

%%
%%  print_placed_proof(+Placed_Proof)
%%
print_placed_proof(P) :-
	print_placed_proofs([P]).

print_placed_proofs([]).
print_placed_proofs(Ps) :- Ps=[_|_],
	gather_uppers(Ps, Qs),
	print_placed_proofs(Qs),
	print_rules(Ps),
	nlx,
	print_lowers(Ps),
	nlx.

gather_uppers([], []).
gather_uppers([P|Ps], Qs) :-
	gather_uppers(Ps, Qs1),
	P = [_,_Rule,_,_S|P0],
	append2(P0, Qs1, Qs).

print_rules([]).
print_rules([P|Ps]) :-
	P = [M0+W0,Rule,_M1+_W1,_S|_],
	goto_pos(M0),
	print_rule_line(W0),
	write_rule_name(Rule),
	print_rules(Ps).

print_rule_line(0) :- !.
print_rule_line(N) :- N>0, n_writex(N, '-'), writex(' ').

print_lowers([]).
print_lowers([P|Ps]) :-
	P = [_M0+_W0,_Rule,M1+_W1,S|_],
	goto_pos(M1),
	write_seq(S),
	print_lowers(Ps).

get_width(X, W) :-
	open_null(NULL, OldSTR, OldLog),
	nlx,
	writex(X),
	line_position(W),
	close_null(NULL, OldSTR, OldLog).

get_rule_name_width(R, W) :-
	open_null(NULL, OldSTR, OldLog),
	nlx,
	write_rule_name(R),
	line_position(W),
	close_null(NULL, OldSTR, OldLog).

get_seq_width(S, W) :-
	open_null(NULL, OldSTR, OldLog),
	nlx,
	write_seq(S),
	line_position(W),
	close_null(NULL, OldSTR, OldLog).

open_null(NULL, OldSTR, OldLog) :-
	current_output(OldSTR),
	open_null_stream(NULL),
	set_output(NULL),
	log_suspend(OldLog).

close_null(NULL, OldSTR, OldLog) :-
	set_output(OldSTR),
	close(NULL),
	log_resume(OldLog).

write_rule_name(axiom) :- !, writex('Axiom').
write_rule_name(ax) :- !, writex('Ax').
write_rule_name(cut) :- !, writex('Cut').
write_rule_name(r(all)) :- !, writex('R@').
write_rule_name(r(exists)) :- !, writex('R#').
write_rule_name(r(R)) :- !, writex('R'), writex(R).
%write_rule_name(r(R,N)) :- !, writex('R'), writex(R), writex(N).
write_rule_name(l(all)) :- !, writex('L@').
write_rule_name(l(exists)) :- !, writex('L#').
write_rule_name(l(R)) :- !, writex('L'), writex(R).
%write_rule_name(l(R,N)) :- !, writex('L'), writex(R), writex(N).

write_seq((Xs-->Ys)) :-
	write_list(Xs), writex(' --> '), write_list(Ys).
	
write_list([]).
write_list([X]) :- !, writex(X).
write_list([X|Xs]) :- writex(X), writex(','), write_list(Xs).

%%
%%  Tex form printer
%%
tex_print(P) :-
	tex_name_variables(P, Q),
	tex_print(0, Q).

tex_name_variables(X, Z) :-
	tex_name_variables(X, 0, _N, [], _Vs, Z).

tex_name_variables(X, N, N, Vs, Vs, X) :- ground(X), !.
tex_name_variables(X, N, N, Vs, Vs, Z) :- var(X), assoc(X, Z, Vs), !.
tex_name_variables(X, N0, N, Vs0, [[X|Z]|Vs0], Z) :- var(X), !,
	V is N0 mod 3, VN is N0//3,
	tex_name_var(V, VN, Z),
	N is N0+1.
tex_name_variables([X|Xs], N0, N, Vs0, Vs, [Z|Zs]) :- !,
	tex_name_variables(X, N0, N1, Vs0, Vs1, Z),
	tex_name_variables(Xs, N1, N, Vs1, Vs, Zs).
tex_name_variables(X, N0, N, Vs0, Vs, Z) :-
	X =.. Xs,
	tex_name_variables(Xs, N0, N, Vs0, Vs, Zs),
	Z =.. Zs.

tex_name_var(0, VN, X) :- !, tex_name_var1("x", VN, X).
tex_name_var(1, VN, X) :- !, tex_name_var1("y", VN, X).
tex_name_var(2, VN, X) :- !, tex_name_var1("z", VN, X).

tex_name_var1([V], 0, X) :- !, name(X, [V]).
tex_name_var1([V], VN, X) :-
	[C1,C2]="_{",
	name(VN, Ns), append2(Ns, "}", Ns1),
	name(X, [V,C1,C2|Ns1]).

tex_print(Tab, [[Rule,LN],_,S]) :- !,
	tabx(Tab), writex('\\deduce{'),
	tex_print_sequent(S), writex('}{'),
	tex_print_rule_name([Rule,LN]),	writex('}'), nlx.
tex_print(Tab, [Rule,_,S]) :- (Rule = ax; Rule = axiom), !,
	tabx(Tab), tex_print_sequent(S).
tex_print(Tab, [Rule,_,S|Ss]) :-
	tabx(Tab), writex('\\infer['),
	tex_print_rule_name(Rule), writex(']{'),
	tex_print_sequent(S),
	writex('}{'),
	tex_print_list(Ss, Tab),
	writex('}'), nlx.

tex_print_list([], _Tab).
tex_print_list([S1], Tab) :- !,
	nlx,
	Tab1 is Tab+2,
	tex_print(Tab1, S1),
	tabx(Tab).
tex_print_list([S1|Ss], Tab) :- !,
	nlx,
	Tab1 is Tab+2,
	tex_print(Tab1, S1),
	tabx(Tab1), writex('&'),
	tex_print_list(Ss, Tab).

tex_print_sequent((Xs-->Ys)) :-
	tex_print_formulas(Xs),
	writex(' \\lra '),
	tex_print_formulas(Ys).

tex_print_formulas([]).
tex_print_formulas([A]) :- !,
	tex_print_formula(A).
tex_print_formulas([A|As]) :-
	tex_print_formula(A),
	writex(', '),
	tex_print_formulas(As).

tex_print_formula(A) :-
	tex_print_formula(999, A).

tex_print_formula(Prec0, A) :- tex_quantifier_name(A, Q, X, B), !,
	Prec = 999,
	(Prec0 < Prec -> writex('('); true),
	writex(Q), writex(' '), writex(X), writex('.'),
	tex_print_formula(B),
	(Prec0 < Prec -> writex(')'); true),
	!.
tex_print_formula(Prec0, A) :- is_op(A, Prec, Type, Op, As), !,
	(Prec0 < Prec -> writex('('); true),
	tex_print_op_formula(Prec, Type, Op, As),
	(Prec0 < Prec -> writex(')'); true),
	!.
tex_print_formula(_Prec0, A) :- tex_unit_name(A, U), !,
	writex(U).
tex_print_formula(_Prec0, A) :-
	rename_functor(A, A1),
	writex(A1).

rename_functor(X, Z) :-
	X =.. [F0|As],
	capitalize_atom(F0, F),
	Z =.. [F|As].

tex_quantifier_name(X@A, '\\forall', X, A).
tex_quantifier_name(X#A, '\\exists', X, A).

tex_unit_name(_, _) :- fail.
tex_unit_name(bot, '\\bot').
tex_unit_name(top, '\\top').

%tex_print_op_formula(Prec, fy, Op, [A1]) :- Op = ~, !,
%	tex_print_op(Op), writex('{'),
%	tex_print_formula(Prec, A1), writex('}').
tex_print_op_formula(Prec, fx, Op, [A1]) :- !,
	Prec1 is Prec - 1,
	tex_print_op(Op), writex(' '),
	tex_print_formula(Prec1, A1).
tex_print_op_formula(Prec, fy, Op, [A1]) :- !,
	tex_print_op(Op), writex(' '),
	tex_print_formula(Prec, A1).
tex_print_op_formula(Prec, xf, Op, [A1]) :- !,
	Prec1 is Prec - 1,
	tex_print_formula(Prec1, A1),
	writex(' '), tex_print_op(Op).
tex_print_op_formula(Prec, yf, Op, [A1]) :- !,
	tex_print_formula(Prec, A1),
	writex(' '), tex_print_op(Op).
tex_print_op_formula(Prec, xfx, Op, [A1,A2]) :- !,
	Prec1 is Prec - 1,
	tex_print_formula(Prec1, A1),
	writex(' '), tex_print_op(Op), writex(' '),
	tex_print_formula(Prec1, A2).
tex_print_op_formula(Prec, xfy, Op, [A1,A2]) :- !,
	Prec1 is Prec - 1,
	tex_print_formula(Prec1, A1),
	writex(' '), tex_print_op(Op), writex(' '),
	tex_print_formula(Prec, A2).
tex_print_op_formula(Prec, yfx, Op, [A1,A2]) :- !,
	Prec1 is Prec - 1,
	tex_print_formula(Prec, A1),
	writex(' '), tex_print_op(Op), writex(' '),
	tex_print_formula(Prec1, A2).

is_op(A, Prec, Type, Op, As) :-
	functor(A, Op, N), current_op(Prec, Type, Op),
	is_op0(A, Type, N, As),
	!.
is_op0(A, fx, 1, [A1]) :- arg(1, A, A1).
is_op0(A, fy, 1, [A1]) :- arg(1, A, A1).
is_op0(A, xf, 1, [A1]) :- arg(1, A, A1).
is_op0(A, yf, 1, [A1]) :- arg(1, A, A1).
is_op0(A, xfx, 2, [A1,A2]) :- arg(1, A, A1), arg(2, A, A2).
is_op0(A, xfy, 2, [A1,A2]) :- arg(1, A, A1), arg(2, A, A2).
is_op0(A, yfx, 2, [A1,A2]) :- arg(1, A, A1), arg(2, A, A2).

tex_print_op(Op) :-
	tex_op_name(Op, OpName), !,
	writex(OpName).

tex_op_name(~,  '\\lnot').
tex_op_name(/\, '\\land').
tex_op_name(\/, '\\lor').
tex_op_name(->, '\\imp').

tex_print_rule_name([Rule,LN]) :- !,
	tex_rule_name(Rule, RuleName),
	!,
	writex(RuleName),
	writex('('), writex(LN), writex(')').
tex_print_rule_name(Rule) :-
	tex_rule_name(Rule, RuleName),
	!,
	writex(RuleName).

tex_rule_name(ax, '({\\rm Ax})').
tex_rule_name(axiom, '({\\rm Axiom})').
tex_rule_name(l(top), '({\\rm L}\\top)').
tex_rule_name(r(top), '({\\rm R}\\top)').
tex_rule_name(l(bot), '({\\rm L}\\bot)').
tex_rule_name(r(bot), '({\\rm R}\\bot)').
tex_rule_name(l(~), '({\\rm L}\\lnot)').
tex_rule_name(r(~), '({\\rm R}\\lnot)').
tex_rule_name(l(/\), '({\\rm L}\\land)').
tex_rule_name(r(/\), '({\\rm R}\\land)').
tex_rule_name(l(\/), '({\\rm L}\\lor)').
tex_rule_name(r(\/), '({\\rm R}\\lor)').
tex_rule_name(l(->), '({\\rm L}\\imp)').
tex_rule_name(r(->), '({\\rm R}\\imp)').
tex_rule_name(l(all), '({\\rm L}\\forall)').
tex_rule_name(r(all), '({\\rm R}\\forall)').
tex_rule_name(l(exists), '({\\rm L}\\exists)').
tex_rule_name(r(exists), '({\\rm R}\\exists)').
tex_rule_name(cut, '({\\rm Cut})').

%%
%%  Utilities
%%
append_all([], []).
append_all([P], P).
append_all([P|Ps], X) :- Ps=[_|_], append2(P, X1, X), append_all(Ps, X1).

merge(X1, X2, X) :- split(X, X1, X2).

split([], [], []).
split([X|Xs], [X|Ys], Zs) :- split(Xs, Ys, Zs).
split([X|Xs], Ys, [X|Zs]) :- split(Xs, Ys, Zs).

append2([], Z, Z).
append2([W|X], Y, [W|Z]) :- append2(X, Y, Z).

member2(X, [X|_Xs]).
member2(X, [_Y|Xs]) :- member2(X, Xs).

memberq(X, [X|_Xs]) :- !.
memberq(X, [_Y|Xs]) :- memberq(X, Xs).

memq(X, [Y|_Xs]) :- X==Y, !.
memq(X, [_Y|Xs]) :- memq(X, Xs).

get_nth(0, [X|_Xs], X) :- !.
get_nth(N, [_|Xs], X) :- N>0, N1 is N-1, get_nth(N1, Xs, X).

for2(I, M, N) :- M =< N, I=M.
for2(I, M, N) :- M =< N, M1 is M+1, for2(I, M1, N).

assoc(X, Z, [[X0|Z]|_As]) :- X==X0, !.
assoc(X, Z, [_|As]) :- assoc(X, Z, As).

substitute(X0, X, A0, A) :- var(A0), X0==A0, !, A=X.
substitute(_X0, _X, A0, A) :- var(A0), !, A=A0.
substitute(_X0, _X, A0, A) :- atomic(A0), !, A=A0.
substitute(X0, X, [A0|B0], [A|B]) :- !,
	substitute(X0, X, A0, A),
	substitute(X0, X, B0, B).
substitute(X0, X, A0, A) :-
	A0 =.. B0,
	substitute(X0, X, B0, B),
	A =.. B.

capitalize_atom(X, Z) :- atom(X),
	name(X, [C0|Cs]), C0 >= "a", C0 =< "z", !,
	C is ((C0 - "a") + "A"),
	name(Z, [C|Cs]).
capitalize_atom(X, X).

n_writex(0, _) :- !.
n_writex(N, X) :- N>0, writex(X), N1 is N-1, n_writex(N1, X).

flush_out :-
	current_output(S),
	flush_output(S).

goto_pos(T) :-
	current_output(S),
	line_position(S, T0),
	tabx(T-T0).

line_position(W) :-
	current_output(S),
	line_position(S, W).

%%
%%  Logging
%%
log_start(yes) :- !,
	log_start('seqprover.log').
log_start(File) :-
	logging(no, _),
	!,
	open(File, write, STR),
	retractall(logging(_,_)),
	assert(logging(File, STR)).

log_end :-
	logging(no, _),
	!.
log_end :-
	logging(_File, STR),
	close(STR),
	fail.
log_end :-
	retractall(logging(_,_)),
	assert(logging(no, _)).

log_suspend(logging(File,STR)) :-
	logging(File, STR),
	retractall(logging(_,_)),
	assert(logging(no, _)).

log_resume(logging(File,STR)) :-
	retractall(logging(_,_)),
	assert(logging(File, STR)).

readx(STR, X) :-
	read(STR, X),
	echo_read(STR, X).

echo_read(user_input, _X) :-
	logging(no, _),
	!.
echo_read(user_input, X) :-
	logging(_File, STR),
	!,
	write(STR, X),
	write(STR, '.'),
	nl(STR).
echo_read(_, X) :-
	writex(X),
	writex('.'),
	nlx.

writex(X) :-
	logging(no, _),
	!,
	write(X).
writex(X) :-
	logging(_, STR),
	write(X),
	write(STR, X).

nlx :-
	logging(no, _),
	!,
	nl.
nlx :-
	logging(_, STR),
	nl,
	nl(STR).

tabx(X) :-
	logging(no, _),
	!,
	tab(X).
tabx(X) :-
	logging(_, STR),
	tab(X),
	tab(STR, X).

%%
%%  CGI
%%
cgi :-
	nl, write('# Start'), nl,
	prolog_flag(syntax_errors, _, quiet),
	read(Output),
	read(Sequent),
	set_output_form(Output),
	cgi_prove(Sequent),
	!,
	flush_out,
	halt.
cgi :-
	nl, write('# Syntax error'), nl,
	flush_out,
	halt.

cgi_prove((X <--> Y)) :- !,
	cgi_prove((X --> Y)),
	nl,
	cgi_prove((Y --> X)).
cgi_prove(S0) :-
	(
	    convert_to_seq(S0, S),
	    sequentq(S)
	;
	    formulaq(S0),
	    S = ([] --> [S0])
	),
	!,
	statistics(runtime, _),
	(prove(S, P) ->
	    statistics(runtime, [_,T]),
	    nl,
	    print_proof(P),
	    nl, write('# Proved in '),
	    write(T), write(' msec.'), nl
	;
	    nl, write('# Fail to prove'), nl
	).

%%
%%  Initialization
%%
%% :- initialization(seq_init).

:- initialization(cgi).
