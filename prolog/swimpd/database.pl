:- module(database, [is_programme/1, pid_id/2, id_pid/2, lsinfo//1, addid//2, update_db/1, db_stats/1]).

/* <module> BBC database interface for MPD server

   @todo
      Artist = ? channel? BBC?
      memoise, DB version?
      Shelf for keeping programmes
      confirm URL is ok and try another if not.
 */

:- use_module(library(memo)).
:- use_module(library(dcg_core),  [maybe/3]).
:- use_module(asyncu, [spawn/1]).
:- use_module(state,  [state/2]).
:- use_module(tools,  [report//1, report//2, maybe/2, maybe//2]).
:- use_module(bbc(bbc_tools), [sort_by/3, log_and_succeed/1]).
:- use_module(bbc(bbc_db), [service/3, fetch_new_schedule/1, service_schedule/2, service_live_url/2, service_pid_entry/3,
                            old_pid_entry/2, entry_prop/2, entry_maybe_parent/3, entry_xurl/3, interval_times/3, browse/1,
                            service_entry/2, schedule_updated/2, entry_parents/2, version_prop/2, prog_xurl/3, pid_version/2]).

:- volatile_memo pid_id(+atom, -integer).
pid_id(_, Id) :- flag(songid, Id, Id+1).
id_pid(Id,PID) :- once(browse(pid_id(PID, Id))).
is_programme(PID) :- \+service(PID, _, _).

% --- adding to playlist by path  ---
addid([Dir], nothing) --> {directory(Dir, Entries)}, foldl(add(Dir), Entries).
addid(['Live Radio'], nothing) --> {live_services(Services)}, foldl(add_live, Services).

addid(['PID', PID], just(Id)) --> {pid_id(PID, Id)}, add_pid(PID).
addid(['Live Radio', LongName], just(Id)) --> {live_service(S, LongName), pid_id(S, Id)}, add_live(S-LongName).
addid(['In Progress', PID], just(Id)) --> {old_pid_entry(PID, E), pid_id(PID, Id)}, add('In Progress', E).
addid([LongName, PID], just(Id)) -->
   {longname_service(LongName, S), service_pid_entry(S, PID, E), pid_id(PID, Id)},
   add(LongName, E).

add_live(S-SLN) --> {live_service_tags(S-SLN, Tags), live_url(S, URL)}, [song(S, =(URL), Tags)].
add(LongName, E) --> {entry_tags(LongName, E, PID, Tags, []), entry_xurl(redir(dash), E, _-URL)}, [song(PID, =(URL), Tags)].

add_pid(PID) --> [song(PID, database:version_url(V), [file-PID|Tags])], {pid_version(PID, V), version_tags(V, Tags)}.
version_tags(V, [duration-D, 'Title'-T, 'Comment'-S]) :- maplist(version_prop(V), [duration(D), title(T), summary(S)]).
version_url(V, URL) :- version_prop(V, vpid(VPID)), prog_xurl(_, vpid(VPID), _-URL).

% --- query db contents ---
lsinfo([]) -->
   foldl(report(directory), ['In Progress', 'Live Radio']),
   {all_services(Services)}, foldl(service_dir, Services).
lsinfo(['Live Radio']) --> {live_services(Services)}, foldl(live_radio, Services).
lsinfo([Dir]) --> {directory(Dir, Items)}, foldl(programme(Dir), Items).

directory('In Progress', Items) :-
   findall(E, (state(position(PID), _), once(old_pid_entry(PID, E))), Items).
directory(ServiceName, SortedItems) :-
	longname_service(ServiceName, S),
   findall(E, service_entry(S, E), Items),
   sort_by(entry_sortkey, Items, SortedItems).
entry_sortkey(E, k(SortedParents, Date)) :- entry_prop(E, broadcast(Date)), entry_parents(E, SortedParents).

live_radio(S-ServiceName) -->
   {live_service_tags(S-ServiceName, Tags), pid_id(S, Id)},
   foldl(report, Tags), report('Id'-Id).

service_dir(S-Name) --> report(directory-Name), maybe(service_updated(S)).
service_updated(S) --> {service_schedule(S, Sch), schedule_updated(Sch, Updated)}, report('Last-Modified'-Updated).

programme(Dir, E) -->
	{insist(entry_tags(Dir, E, PID, Tags, [])), pid_id(PID, Id)},
	foldl(report, Tags), report('Id'-Id).

live_service_tags(_-SLN, [file-File, 'Title'-SLN]) :- path_file(['Live Radio', SLN], File).
entry_tags(Dir, E, PID) -->
	[file-File], {entry_prop(E, pid(PID)), path_file([Dir, PID], File)},
   tag(title_and_maybe_album(Dir, PID), E), foldl(maybe, [tag(broadcast, E), tag(availability, E)]),
	['Comment'-Syn, duration-Dur], {maplist(entry_prop(E), [synopsis(Syn), duration(Dur)])}.

tag(broadcast, E)    --> {entry_prop(E, broadcast(B)), interval_times(B,T,_), ts_string(T,Broadcast)}, ['Date'-Broadcast].
tag(availability, E) --> {entry_prop(E, availability(A)), interval_times(A,_,T), ts_string(T,Until)}, ['AvailableUntil'-Until].
tag(title_and_maybe_album(Dir, PID), E) -->
   {entry_prop(E, title(FullTitle)), entry_maybe_parent('Brand', E, Parent)},
   {maybe(cut_parent, Parent,  FullTitle, Title), maybe_add_progress(Dir, PID, Title, Title2)},
   ['Title'-Title2], maybe(parent_as_album, Parent).

maybe_add_progress('In Progress', PID, Tit, Tit2) :- !,
   state(position(PID), Pos), seconds_hms(Pos, H, M, S),
   format(string(Tit2), '~s [~d:~|~`0t~d~2+:~|~`0t~d~2+]', [Tit, H, M, S]).
maybe_add_progress(_, _, Tit, Tit).
seconds_hms(T, H, M, S) :- T1 is round(T), divmod(T1, 60, MM, S), divmod(MM, 60, H, M).

parent_as_album(_-Name) --> ['Album'-Name].
cut_parent(_-Name) --> maybe((str_cut(Name), str_cut(": "))).
str_cut(Pre, String, Suff) :- string_concat(Pre, Suff, String).
ts_string(T, S) :- format_time(string(S), '%c', T).
path_file(Path, File) :- atomic_list_concat(Path, '/', File).

db_stats([artists-1, albums-M, songs-N, db_playtime-Dur]) :-
   findall(B-D, brand_dur(B, D), Items), length(Items, N),
   aggregate_all(count, distinct(PP,  member(just(PP-_)-_, Items)), M),
   aggregate_all(sum(D), member(_-D, Items), Dur).
brand_dur(B, D) :- service_entry(_, E), entry_maybe_parent('Brand', E, B), entry_prop(E, duration(D)).

update_db([]) :- forall(service(S, _, _), fetch_new_schedule(S)).
update_db([ServiceName]) :- service(S, _, ServiceName), fetch_new_schedule(S).
all_services(Services) :- findall(S-SLN, service(S, _, SLN), Services).
live_services(Services) :- findall(S-SLN, live_service(S, SLN), Services).
longname_service(LongName, S) :- service(S, _, LongName), (service_schedule(S, _) -> true; fetch_new_schedule(S)).

live_url(S, URL) :- service_live_url(S, URL).
live_url(resonance, 'http://stream.resonance.fm:8000/resonance').
live_service(S, LongName) :- service(S, _, LongName).
live_service(resonance, 'Resonance FM').
