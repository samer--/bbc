:- module(database, [is_programme/1, pid_id/2, id_pid/2, lsinfo//1, addid//2, db_update/1, db_image/3,
                     db_stats/1, db_count//1, db_find//2, db_find/3, db_list//3]).

:- use_module(library(memo)).
:- use_module(library(dcg_core),  [maybe/3]).
:- use_module(library(listutils), [enumerate/2]).
:- use_module(state,  [state/2, set_state/2]).
:- use_module(tools,  [report//1, report//2, maybe/2, maybe//2, spawn/1]).
:- use_module(bbc(bbc_tools), [sort_by/3, log_and_succeed/1]).
:- use_module(bbc(bbc_db), [service/1, service/2, fetch_new_schedule/1, service_live_url/2, pid_entry/3,
                            service_entry/2, entry_prop/2, entry_maybe_parent/3, entry_xurl/3, interval_times/3,
                            service_updated/2, entry_parents/2, version_prop/2, prog_xurl/3, pid_version/2]).

:- volatile_memo pid_id(+atom, -integer).
pid_id(_, Id) :- flag(songid, Id, Id+1).
id_pid(Id,PID) :- once(browse(pid_id(PID, Id))).
is_programme(PID) :- \+service(PID).

% --- update and stats ---
db_update([]) :- forall(service(S), fetch_new_schedule(S)), set_with(db_stats, db_stats).
db_update([ServiceName]) :- service(S, ServiceName), fetch_new_schedule(S), set_with(db_stats, db_stats).
db_stats(Stats) :- state(db_stats, Stats) -> true; set_with(db_stats, db_stats), db_stats(Stats). % FIXME
set_with(K, P) :- call(P, _, V), set_state(K, V).

db_stats(_, [artists-NumServices, albums-M, songs-N, db_playtime-Dur]) :-
   findall(B-D, brand_dur(B, D), Items), length(Items, N),
   aggregate_all(count, service(_), NumServices),
   aggregate_all(count, distinct(PP,  member(just(PP-_)-_, Items)), M),
   aggregate_all(sum(D), member(_-D, Items), DurFloat), round(DurFloat, Dur).
brand_dur(B, D) :- service_entry(_, E), entry_maybe_parent('Brand', E, B), entry_prop(E, duration(D)).

% --- adding to playlist by path  ---
addid([Dir], nothing) --> {directory(Dir, Entries)}, foldl(add(Dir), Entries).
addid(['Live Radio'], nothing) --> {live_services(Services)}, foldl(add_live, Services).

addid(['PID', PID], just(Id)) --> {pid_id(PID, Id)}, add_pid(PID).
addid(['Live Radio', LongName], just(Id)) --> {live_service(S, LongName), pid_id(S, Id)}, add_live(S-LongName).
addid(['In Progress', PID], just(Id)) --> {pid_entry(any, PID, E), pid_id(PID, Id)}, add('In Progress', E).
addid([ServiceName, PID], just(Id)) -->
   {service(S, ServiceName), ensure_service_schedule(S),  pid_entry(latest(S), PID, E), pid_id(PID, Id)},
   add(ServiceName, E).

add_live(S-SLN) --> {live_service_tags(S-SLN, Tags), live_url(S, URL)}, [song(S, =(URL), Tags)].
add(Dir, E) --> {entry_tags(Dir, E, PID, Tags, [])}, [song(PID, database:entry_url(E), Tags)].
entry_url(E, URL) :- entry_xurl(_, E, _-URL).

add_pid(PID) --> [song(PID, database:version_url(V), [file-PID|Tags])], {pid_version(PID, V), version_tags(V, Tags)}.
version_tags(V, [duration-D, 'Title'-T, 'Comment'-S1]) :- maplist(version_prop(V), [duration(D), title(T), summary(S)]), to_one_line(S, S1).
version_url(V, URL) :- version_prop(V, vpid(VPID)), prog_xurl(_, vpid(VPID), _-URL).

% --- query db contents ---
lsinfo([]) -->
   foldl(report(directory), ['In Progress', 'Live Radio']),
   {findall(S-SLN, service(S, SLN), Services)}, foldl(service_dir, Services).
lsinfo(['Live Radio']) --> {live_services(Services)}, foldl(live_radio, Services).
lsinfo([Dir]) --> {directory(Dir, Items)}, foldl(programme(Dir), Items).

directory('In Progress', Items) :-
   findall(E, (state(position(PID), _), once(pid_entry(any, PID, E))), Items).
directory(ServiceName, SortedItems) :-
	service(S, ServiceName),
   ensure_service_schedule(S),
   findall(E, service_entry(S, E), Items),
   sort_by(entry_sortkey, Items, SortedItems).
entry_sortkey(E, k(SortedParents, Date)) :- entry_date(E, Date), entry_parents(E, SortedParents).

live_radio(S-ServiceName) -->
   {live_service_tags(S-ServiceName, Tags), pid_id(S, Id)},
   foldl(report, Tags), report('Id'-Id).

service_dir(S-Name) --> report(directory-Name), maybe(service_updated(S)).
service_updated(S) --> {service_updated(S, Updated)}, report('Last-Modified'-Updated).

programme(Dir, E) -->
	{insist(entry_tags(Dir, E, PID, Tags, [])), pid_id(PID, Id)},
	foldl(report, Tags), report('Id'-Id).

% --- list by tag ---
service_tag(artist, 'Artist').
service_tag(albumartist, 'AlbumArtist').

db_find(_, Filters, Paths) :- find(Filters, Tracks), maplist(track_path, Tracks, Paths).
db_find(_, Filters) --> {find(Filters, Tracks)}, foldl(found_track, Tracks).
db_count(Filters) -->
   { find(Filters, Tracks),
     length(Tracks, NumTracks),
     aggregate_all(sum(D), (member(_-E, Tracks), entry_prop(E, duration(D))), TotalDur),
     round(TotalDur, IntDur)
   },
   foldl(report, [songs-NumTracks, playtime-IntDur]).

entries_tracks(Es, Tracks) :- call(enumerate * sort_by(entry_date), Es, Tracks).
track_path(_-E, [SN, PID]) :- maplist(entry_prop(E), [pid(PID), service(SN)]).
found_track(TrackNo-E) -->
	{ entry_prop(E, service(ServiceName)),
     insist(entry_tags(ServiceName, E, _PID, Tags, [])) },
	foldl(report, Tags), report('Track'-TrackNo).

find(Filters, [Track]) :-
   select(track-TrackAtom, Filters, FiltersRem), atom_number(TrackAtom, TrackNo),
   find(FiltersRem, Tracks), nth1(TrackNo, Tracks, Track).
find(Filters, Tracks) :-
   sort(Filters, [album-Album]),
   findall(E, (service_entry(_, E), entry_prop(E, parent(_, _, Album, _))), Es),
   entries_tracks(Es, Tracks).
find(Filters, Tracks) :-
   sort(Filters, [album-Album, ArtistTag-Artist]),
   service_tag(ArtistTag, _), service(S, Artist),
   findall(E, (service_entry(S, E), entry_prop(E, parent(_, _, Album, _))), Es),
   entries_tracks(Es, Tracks).
find(Filters, Tracks) :-
   sort(Filters, [ArtistTag-Artist]),
   service_tag(ArtistTag, _), service(S, Artist),
   findall(E, service_entry(S, E), Es),
   entries_tracks(Es, Tracks).

db_list(genre, [], _) --> [].
db_list(album, [], [GroupBy]) -->
   { service_tag(GroupBy, ServiceTag), !,
     findall(SN-Brands, artist_albums(SN, Brands), ServiceNameBrands) },
   foldl(report_service_name_brands(ServiceTag), ServiceNameBrands).
db_list(album, [], GroupBy) -->
   { album_reporter(GroupBy, Reporter),
     setof(B, S^service_brand(S, B), Brands) },
   foldl(Reporter, Brands).
db_list(album, [ArtistTag-Artist], GroupBy) -->
   { service_tag(ArtistTag, _),
     service(S, Artist), ensure_service_schedule(S),
     artist_albums(Artist, Albums),
     album_reporter(GroupBy, Reporter) },
   foldl(Reporter, Albums).
db_list(albumartist, Filters, []) -->
   { sort(Filters, [album-Album, artist-Artist]),
     service(S, Artist), once(service_brand(S, Album)) },
   report('AlbumArtist'-Artist).
db_list(Tag, [], []) -->
   { service_tag(Tag, ServiceTag),
     findall(ServiceName, service(_, ServiceName), ServiceNames) },
   foldl(report(ServiceTag), ServiceNames).

album_reporter([], report('Album')).
album_reporter([date], report_album_with('Date'-ThisYear)) :-
   get_time(Now), format_time(atom(ThisYear),'%Y',Now).

service_brand(S, B) :- service_entry(S, E), entry_prop(E, parent(_, 'Brand', B, _)).
artist_albums(Ar, Als) :- service(S, Ar), setof(B, service_brand(S, B), Als).
report_service_name_brands(ServiceTag, SN-Brands) --> foldl(report_album_with(ServiceTag-SN), Brands).
report_album_with(TagVal, Album) --> report('Album'-Album), report(TagVal).

db_image(Kind, [Dir, PID], URL) :- service(S, Dir), pid_entry(latest(S), PID, E), entry_image(Kind, E, URL).
entry_image(episode, E, URL) :- entry_prop(E, image(URL)).
entry_image(series, E, URL) :- entry_prop(E, parent(_, _, _, Props)), member(image(URL), Props).

% --- common db access for add and lsinfo ---
ensure_service_schedule(S) :- service_updated(S, _) -> true; fetch_new_schedule(S).

live_services(Services) :- findall(S-SLN, live_service(S, SLN), Services).
live_service(S, ServiceName) :- service(S, ServiceName).
live_service(resonance, 'Resonance FM').
live_url(S, URL) :- service_live_url(S, URL).
live_url(resonance, 'http://stream.resonance.fm:8000/resonance').
live_service_tags(_-SLN, [file-File, 'Title'-SLN]) :- path_file(['Live Radio', SLN], File).

entry_date(E, Date) :- entry_prop(E, broadcast(Date)).
entry_tags(Dir, E, PID) -->
   { maplist(entry_prop(E), [pid(PID), synopsis(Syn), duration(Dur)]),
     path_file([Dir, PID], File), to_one_line(Syn, Syn1) },
	[file-File, 'Comment'-Syn1, duration-Dur],
   tag(title_and_maybe_album(Dir, PID), E),
   foldl(maybe, [tag(service, E), tag(broadcast, E), tag(availability, E)]).

tag(service, E)      --> {entry_prop(E, service(Artist))}, ['Artist'-Artist].
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
to_one_line(X, Y) :- split_string(X, "\r\n", "\r\n ", Ys), atomics_to_string(Ys, "; ", Y).
