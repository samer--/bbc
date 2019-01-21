:- module(database, [is_programme/1, pid_id/2, id_pid/2, lsinfo//1, addid//2, update_db/1]).

/* <module> BBC database interface for MPD server

   @todo
      Artist = ? channel? BBC?
      fix parent handling: series and brand
      memoise, DB version?
      Shelf for keeping programmes
      In progress for programmes part listened to
      Allow adding arbitrary PID
      Try putting PID/ID in multiple places in file tree
      memoise db stats: number of programmes, number of brands
 */

:- use_module(library(xpath)).
:- use_module(library(memo)).
:- use_module(library(data/pair)).
:- use_module(library(dcg_core), [maybe/3]).
:- use_module(asyncu, [spawn/1]).
:- use_module(tools,  [report//1, report//2, maybe/2, maybe//2]).
:- use_module(bbc(bbc_tools), [log_and_succeed/1]).
:- use_module(bbc(bbc_db), [service/3, time_service_schedule/3, service_schedule/2, service_live_url/2, service_entry/2,
                            prop/2, entry_maybe_parent/2, entry_xurl/3, interval_times/3, service_parent_children/3]).

:- volatile_memo pid_id(+atom, -integer).
pid_id(_, Id) :- flag(songid, Id, Id+1).
id_pid(Id,PID) :- once(browse(pid_id(PID, Id))).
is_programme(PID) :- \+service(PID, _, _).

% --- adding to playlist by path  ---
addid([LongName], nothing) -->
   {longname_service(LongName, S), findall(E, service_entry(S, E), Entries)},
   ffst(foldl(add(LongName), Entries)).

addid([LongName, PID], just(Id)) -->
   {longname_service(LongName, S), service_entry(S, E), prop(E, pid(PID)), pid_id(PID, Id)},
   ffst(add(LongName, E)).

addid(['Live Radio'], nothing) --> {all_services(Services)}, ffst(foldl(add_live, Services)).
addid(['Live Radio', LongName], just(Id)) --> {service(S, _, LongName), pid_id(S, Id)}, ffst(add_live(S-LongName)).

add_live(S-SLN) --> {live_service_tags(S-SLN, Tags), service_live_url(S, URL)}, add(song(S, URL, Tags)).
add(ServiceName, E) --> {entry_tags(ServiceName, E, PID, Tags, []), entry_xurl(redir(dash), E, _-URL)}, add(song(PID, URL, Tags)).
add(S, Songs1, Songs2) :- append(Songs1, [S], Songs2).

% --- query db contents ---
lsinfo([]) -->
   report(directory, 'Live Radio'),
   {all_services(Services)}, foldl(service_dir, Services).
lsinfo(['Live Radio']) --> {all_services(Services)}, foldl(live_radio, Services).
lsinfo([ServiceName]) -->
	{ longname_service(ServiceName, S),
     findall(Children, service_parent_children(S, _, Children), Families),
     findall(E, (member(Es, Families), member(E, Es)), Items)
	},
	foldl(programme(ServiceName), Items).

live_radio(S-ServiceName) -->
   {live_service_tags(S-ServiceName, Tags), pid_id(S, Id)},
   foldl(report, Tags), report('Id'-Id).

service_dir(S-Name) -->
   report(directory-Name),
   (  {service_schedule(S, Sched)}
   -> {xpath(Sched, /self(@updated), Updated)},
      report('Last-Modified'-Updated)
   ;  []
   ).

programme(ServiceName, E) -->
	{ insist(entry_tags(ServiceName, E, PID, Tags, [])), pid_id(PID, Id)},
	foldl(report, Tags), report('Id'-Id).

live_service_tags(_-SLN, [file-File, 'Title'-SLN]) :- path_file(['Live Radio', SLN], File).
entry_tags(ServiceName, E, PID) -->
	[file-File], {prop(E, pid(PID)), path_file([ServiceName, PID], File)},
   tag(title_and_maybe_album, E), foldl(maybe, [tag(broadcast, E), tag(availability, E)]),
	['Comment'-Syn, duration-Dur], {maplist(prop(E), [synopsis(Syn), duration(Dur)])}.

tag(broadcast, E)    --> {prop(E, broadcast(B)), interval_times(B,T,_), ts_string(T,Broadcast)}, ['Date'-Broadcast].
tag(availability, E) --> {prop(E, availability(A)), interval_times(A,_,T), ts_string(T,Until)}, ['AvailableUntil'-Until].
tag(title_and_maybe_album, E) -->
   {prop(E, title(FullTitle)), entry_maybe_parent(E, Parent)},
   {maybe(cut_parent, Parent,  FullTitle, Title)}, ['Title'-Title],
   maybe(parent_as_album, Parent).

parent_as_album(_-Name) --> ['Album'-Name].
cut_parent(_-Name) --> maybe((str_cut(Name), str_cut(": "))).
str_cut(Pre, String, Suff) :- string_concat(Pre, Suff, String).
ts_string(T, S) :- format_time(string(S), '%c', T). % x for date only
path_file(Path, File) :- atomic_list_concat(Path, '/', File).

update_db([]) :- forall(service(S, _, _), update_service(S)).
update_db([ServiceName]) :- service(S, _, ServiceName), update_service(S).
all_services(Services) :- findall(S-SLN, service(S, _, SLN), Services).
longname_service(LongName, S) :- service(S, _, LongName), (service_schedule(S, _) -> true; update_service(S)).
update_service(S) :- get_time(Now), log_and_succeed(time_service_schedule(Now, S, _)).