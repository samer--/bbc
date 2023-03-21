:- module(mpd_state, [excl/1, attach/1, init_state/2, upd_states/2, set_states/2, state/2, states/2,
                      set_vstate/2, rm_vstate/1, vstate/2,  version_queue/2, add_queue/2]).
:- use_module(library(persistency)).

:- persistent state(term, term).
:- dynamic vstate/2, version_queue/2.
:- meta_predicate excl(0), upd_states(+,2).

excl(G)          :- with_mutex(swimpd, G).
attach(DBFile)   :- db_attach(DBFile, []).
init_state(K, V) :- state(K, _) -> true; assert_state(K, V).
upd_states(K, P) :- states(K, S1), call(P, S1, S2), set_states(K, S2).

states(K1-K2, V1-V2) :- !, states(K1,V1), states(K2,V2).
states(K,V)          :- state(K,V).

set_states(K1-K2, V1-V2) :- set_states(K1,V1), set_states(K2,V2).
set_states(K,V)          :- debug(mpd(state, 0), "state ~w set to ~w", [K,V]), retractall_state(K, _), assert_state(K, V).

% volatile state
set_vstate(Key, Val) :- retractall(vstate(Key, _)), assert(vstate(Key, Val)).
rm_vstate(Key)       :- retractall(vstate(Key, _)).

add_queue(Version, Songs)  :- assert(version_queue(Version, Songs)).
