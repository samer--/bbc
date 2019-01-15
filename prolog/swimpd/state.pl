:- module(mpd_state, [upd_state/2, set_state/2, state/2]).

% state = pair(pair(integer, pair(list(song), maybe(play_state))), dict).
% play_state ---> ps(natural, maybe(pair(pause_state, au_state))).
% au_state   ---> au(duration, elapsed, bitrate, format).
% pause_state ---> play; pause.

:- dynamic state/2.
:- meta_predicate upd_state(+,2).
set_state(Key, Val) :- retractall(state(Key, _)), assert(state(Key, Val)).
upd_state(K, P) :- with_mutex(swimpd, (state(K, S1), call(P, S1, S2), set_state(K, S2))).
