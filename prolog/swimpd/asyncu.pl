:- module(asyncu, [spawn/1, setup_stream/2, registered/2, thread/2]).

:- dynamic thread/2.
:- meta_predicate spawn(0), registered(+,0).
spawn(G) :- thread_create(G, _, [detached(true)]).
setup_stream(Props, S) :- maplist(set_stream(S), Props).
registered(N, G) :- thread_self(Id), setup_call_cleanup(assert(thread(N, Id)), G, retract(thread(N, Id))).
