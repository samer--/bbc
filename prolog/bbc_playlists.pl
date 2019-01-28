#! /usr/bin/env swipl
:- module(bbc_playlists, [main/1, maintain_service/2, start_service_maintenance/2]).

:- use_module(bbc_db).
:- use_module(library(fileutils), [with_output_to_file/2]).
:- use_module(bbc_tools, [log_failure/1, log_and_succeed/1]).

save_service_playlist(Now, Dir, Service, Expiry) :-
   InOneWeek is Now + 7*24*3600,
   debug(bbc, 'Gathering playlist for ~w...', [Service]),
   findall(E, distinct(PID, service_entry_pid(S, E, PID)), Entries),
   sort_by(entry_sortkey, Entries, SortedEntries),
   findall(XU-E, (member(E, SortedEntries), log_failure(entry_xurl(best("hls"), E, XU))), Items),
   foldl(min_expiry, Items, InOneWeek, Expiry),
   format(string(FN), '~s/~s.m3u', [Dir, Service]),
   debug(bbc, 'Saving playlist for ~w to "~s"...', [Service, FN]),
   with_output_to_file(FN, (writeln('#EXTM3U'), maplist(write_playlist_item, Items))).

entry_sortkey(E, k(Parents, Date)) :- prop(E, broadcast(Date)), entry_parents(E, Parents).

min_expiry((inf-_)-_, E, E) :- !.
min_expiry((X-_)-_,  E1, E2) :- E2 is min(E1, X).

write_playlist_item((_-URL)-E) :-
   maplist(prop(E), [title(Title), duration(Dur), broadcast(B)]),
   interval_times(B, BStart, _),
   format_time(string(BDate), '%x', BStart),
   format('#EXTINF:~d, ~s [~s]\n~w\n', [Dur, Title, BDate, URL]).

start_service_maintenance(Dir, Service) :-
   thread_create(maintain_service(Dir, Service), _, [detached(true)]).

maintain_service(Dir, Service) :-
   get_time(Now),
   log_and_succeed(time_service_schedule(Now, Service, _)),
   save_service_playlist(Now, Dir, Service, Expiry),
   sleep_until(Expiry),
   maintain_service(Dir, Service).

sleep_until(T) :- get_time(T0), DT is T-T0, sleep(DT).
main([Dir|Services]) :- debug(bbc), maplist(start_service_maintenance(Dir), Services).
:- current_prolog_flag(argv, Args), main(Args).
% vim: set filetype=prolog
