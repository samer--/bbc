:- module(bbc_tools, [pairf/3, sort_by/3, enum/2, log_failure/1, log_and_succeed/1]).
:- use_module(library(listutils),[zip/3]).

:- meta_predicate log_failure(0), log_and_succeed(0), sort_by(2,+,-), pairf(2,?,?).

sort_by(P, X1, X2) :- maplist(pairf(P), X1, KX1), keysort(KX1, KX2), zip(_, X2, KX2).
pairf(P, X, Y-X) :- call(P, X, Y).

log_failure(G) :- G -> true; debug(mpd(bbc, s(s(s(0)))), 'failed: ~p', [G]), fail.
log_and_succeed(G) :-
   (  catch(G, Ex, debug(mpd(bbc,0), 'Exception on ~q: ~p', [G, Ex])) -> true
   ;  debug(mpd(bbc,s(s(s(0)))), 'Failed on ~q', [G])
   ).

enum(Xs, IXs) :- foldl(enum, Xs, IXs, 0, _).
enum(X, I-X, I, J) :- J is I + 1.
