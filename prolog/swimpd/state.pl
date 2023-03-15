:- module(mpd_state, [init_state/2, upd_state/2, set_state/2, state/2,
                      set_vstate/2, rm_vstate/1, vstate/2,  version_queue/2, add_queue/2]).
:- use_module(library(persistency)).


% state = pair(pair(integer, pair(list(song), maybe(play_state))), dict).
% play_state ---> ps(natural, maybe(pair(pause_state, au_state))).
% au_state   ---> au(duration, elapsed, bitrate, format).
% pause_state ---> play; pause.

:- persistent state(term, term).
:- dynamic vstate/2, version_queue/2.
:- meta_predicate upd_state(+,2).

:- db_attach('current_state.db', []).

set_state(Key, Val)  :- retractall_state(Key, _), assert_state(Key, Val).
init_state(Key, Val) :- state(Key, _) -> true; assert_state(Key, Val).
upd_state(K, P)      :- with_mutex(swimpd, (state(K, S1), call(P, S1, S2), set_state(K, S2))).

% volatile state
set_vstate(Key, Val) :- retractall(vstate(Key, _)), assert(vstate(Key, Val)).
rm_vstate(Key)       :- retractall(vstate(Key, _)).

add_queue(Version, Songs)  :- assert(version_queue(Version, Songs)).
