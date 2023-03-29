:- module(tools, [(+)//1, (*)//1, parse_head//2, nat//1, num//1, atom//1, quoted//1, quoted//2, in/2, fnth/5,
                  decimal//0, report//1, report//2, select_nth/4, maybe//2, maybe/2, fmaybe/3, fjust/3, flip/4,
                  spawn/1, setup_stream/2, registered/2, thread/2]).

:- use_module(library(listutils), [zip/3]).
:- use_module(library(dcg_core), [(//)//2, list//1]).
:- use_module(library(dcg_codes), [esc//2, ctype//1, fmt//2]).
:- use_module(library(snobol), [notany//1, break//1]).

:- dynamic thread/2.
:- meta_predicate spawn(0), registered(+,0).
spawn(G) :- thread_create(G, _, [detached(true)]).
setup_stream(Props, S) :- maplist(set_stream(S), Props).
registered(N, G) :- thread_self(Id), setup_call_cleanup(register(N, Id), G, unregister(N, Id)).
register(N, Id) :- debug(mpd(tools, s(0)), 'Registering thread ~w as ~w', [Id, N]), assert(thread(N, Id)).
unregister(N, Id) :- debug(mpd(tools, s(0)), 'Unregistering thread ~w as ~w', [Id, N]), retract(thread(N, Id)).

parse_head(Head, Tail) --> break(` `) // list(H), list(Tail), {atom_codes(Head, H)}.

report(Name-Value) --> report(Name, Value).
report(Name, Value) --> fmt('~w: ~w\n', [Name, Value]).

in(Xs, X) :- member(X, Xs).
select_nth(N, X, L1, L2) :- nth0(N, L1, X, L2).
fnth(0, X, Y, [X|L], [Y|L]).
fnth(N, X, Y, [Z|L1], [Z|L2]) :- succ(M, N), fnth(M, X, Y, L1, L2).
flip(P, X, Y, Z) :- call(P, Y, X, Z).

:- meta_predicate +(//,?,?), +(//,?,?), quoted(3,?,?,?), quoted(//,?,?).
+(P) --> call_dcg(P), *(P).
*(P) --> []; +(P).

:- meta_predicate fmaybe(2,?,?), fjust(2,?,?), maybe(3,?,?,?), maybe(1,?).
maybe(P, X) --> maybe_(X, P).
maybe(P, X) :- maybe_(X, P).
maybe_(nothing, _) --> [].
maybe_(just(Y), P) --> call(P, Y).
maybe_(nothing, _).
maybe_(just(X), P) :- call(P, X).

fmaybe(_, nothing, nothing).
fmaybe(P, just(X1), just(X2)) :- call(P, X1, X2).
fjust(P, just(X1), just(X2)) :- call(P, X1, X2).

digit  --> ctype(digit).
nat(N) --> {ground(N)}, !, num(N) // +digit.
nat(N) --> +digit // num(N).
decimal --> +digit, ([]; ".", +digit).

num(N,S1,S2) :- ground(N), !, format(codes(S1,S2),'~w',[N]).
num(N,S1,S2) :- list(C,S1,S2), number_codes(N,C).
atom(A,S1,S2) :- ground(A), !, format(codes(S1,S2),'~w',[A]).
atom(A,S1,S2) :- list(C,S1,S2), atom_codes(A,C).

quoted(P, X) --> quoted(call(P, X)).
quoted(P) --> "\"", esc(esc_qq, Codes), "\"", {phrase(P, Codes)}.
esc_qq([0'"|Cs],Cs) --> "\\\"".
esc_qq([0'\\|Cs],Cs) --> "\\\\".
esc_qq([C|Cs],Cs) --> [C] // notany(`\\"`).
