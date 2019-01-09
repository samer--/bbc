:- module(bbc_tools, [in/2, enum/2, on_accept/2, log_failure/1, log_and_succeed/1]).
:- meta_predicate log_failure(0), log_and_succeed(0), on_accept(0, 0).

on_accept(Query, Goal) :- call_cleanup((Query, Success=true), (Success=true, Goal)).

log_failure(G) :- G -> true; debug(bbc, 'failed: ~p', [G]), fail.
log_and_succeed(G) :-
   (  catch(G, Ex, debug(bbc, 'Exception on ~q: ~p', [G, Ex])) -> true
   ;  debug(bbc, 'Failed on ~q', [G])
   ).

in(List, Item) :- member(Item, List).
enum(Xs, IXs) :- foldl(enum, Xs, IXs, 0, _).
enum(X, I-X, I, J) :- J is I + 1.
