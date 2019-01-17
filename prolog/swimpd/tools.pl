:- module(tools, [(+)//1, (*)//1, parse_head//2, nat//1, num//1, atom//1, pphrase/1, quoted//1, quoted//2,
                  decimal//0, report//1, report//2, select_nth/4, maybe//2, maybe/2, fmaybe/3, fjust/3, flip/4]).

:- use_module(library(dcg_core), [(//)//2, list//1]).
:- use_module(library(dcg_codes), [esc//2, ctype//1, fmt//2]).
:- use_module(library(snobol), [notany//1]).
:- use_module(library(dcg/basics), [string_without//2]).

parse_head(Head, Tail) --> string_without(` `, H), tail(Tail), {atom_codes(Head, H)}.
tail([]) --> [].
tail(Tail) --> " ", list(Tail).

report(Name-Value) --> report(Name, Value).
report(Name, Value) --> fmt('~w: ~w\n', [Name, Value]).

select_nth(N, X, L1, L2) :- nth0(N, L1, X, L2).
flip(P, X, Y, Z) :- call(P, Y, X, Z).

:- meta_predicate +(//,?,?), +(//,?,?), pphrase(//), quoted(3,?,?,?), quoted(//,?,?).
pphrase(P) :- phrase(P, C), format('~s', [C]).
+(P) --> call_dcg(P), *(P).
*(P) --> []; +(P).

:- meta_predicate fmaybe(2,?,?), fjust(2,?,?), maybe(3,?,?,?), maybe(1,?).
maybe(_, nothing) --> [].
maybe(P, just(Y)) --> call(P,Y).
maybe(_, nothing).
maybe(P, just(X)) :- call(P, X).

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
