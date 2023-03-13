:- module(mpd_state, [init_state/2, rm_state/1, upd_state/2, set_state/2, state/2, queue/2, set_queue/2]).
:- use_module(library(persistency)).


% state = pair(pair(integer, pair(list(song), maybe(play_state))), dict).
% play_state ---> ps(natural, maybe(pair(pause_state, au_state))).
% au_state   ---> au(duration, elapsed, bitrate, format).
% pause_state ---> play; pause.

:- persistent state(term, term).
:- dynamic queue/2.
:- meta_predicate upd_state(+,2).

:- db_attach('current_state.db', []).

set_state(Key, Val)  :- retractall_state(Key, _), assert_state(Key, Val).
rm_state(Key)        :- retractall_state(Key, _).
init_state(Key, Val) :- state(Key, _) -> true; assert_state(Key, Val).
upd_state(K, P)      :- with_mutex(swimpd, (state(K, S1), call(P, S1, S2), set_state(K, S2))).
set_queue(V, Songs)  :- assert(queue(V, Songs)).
