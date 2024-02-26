%% Types used in this module:
%  pid   ~ a BBC programme identifier (for a programme or a service)
%  id    ~ a local numeric identifier, valid only during one running instance of SWIPD
%  mpd_filter         ~ a spec for finding items in the database.
%  path == list(atom) ~ a path to an item or directory in the database.

:- module(database, [is_programme/1, pid_id/2, pid_tracks/2, id_pid/2, lsinfo//1, addid//2, db_update/1,
                     db_image/3, db_stats/1, db_count//1, db_find//2, db_find/3, db_list//3]).

:- use_module(library(memo)).
:- use_module(library(dcg_core),  [maybe/3]).
:- use_module(library(listutils), [enumerate/2]).
:- use_module(state,  [state/2, vstate/2, set_vstate/2]).
:- use_module(tools,  [report//1, report//2, maybe/2, maybe//2]).
:- use_module(bbc(bbc_tools), [sort_by/3, log_and_succeed/1]).
:- use_module(bbc(bbc_db), [service/1, service/2, fetch_new_schedule/1, service_live_url/2, pid_entry/3, pid_tracks/2,
                            service_entry/2, entry_prop/2, entry_maybe_parent/3, entry_xurl/3, interval_times/3,
                            service_updated/2, entry_parents/2, version_prop/2, prog_xurl/3, pid_version/2]).

:- volatile_memo pid_id(+atom, -integer).
pid_id(_, Id) :- flag(songid, Id, Id+1).
id_pid(Id,PID) :- once(browse(pid_id(PID, Id))).
is_programme(PID) :- \+service(PID). % it's a programme if it's not a radio station

%% db_update(+Path:path) is det.
%  Update database below given path (empty for root, or a service name), by fetches from BBC.
db_update([]) :- forall(service(S), fetch_new_schedule(S)), set_db_stats.
db_update([ServiceName]) :- service(S, ServiceName), fetch_new_schedule(S), set_db_stats.

%% db_stats(-Stats:list(pair(atom, number)) is det.
%  Retrieve database stats from vstate/2, generating them and adding them to vstate/2 if necessary.
db_stats(Stats) :- vstate(db_stats, Stats) -> true; set_db_stats, db_stats(Stats). % FIXME

set_db_stats :-
   findall(B-D, brand_dur(B, D), Items), length(Items, N),
   aggregate_all(count, service(_), NumServices),
   aggregate_all(count, distinct(PP,  member(just(PP-_)-_, Items)), M),
   aggregate_all(sum(D), member(_-D, Items), DurFloat), round(DurFloat, Dur),
   set_vstate(db_stats, [artists-NumServices, albums-M, songs-N, db_playtime-Dur]).
brand_dur(B, D) :- service_entry(_, E), entry_maybe_parent('Brand', E, B), entry_prop(E, duration(D)).

%% addid(+Path:path, -Id:maybe(id))// is det.
%  Add to playlist by path. Adding a single item (as opposed to a whole directory), returns the id
%  of the added item in the second argument as just(Id).
addid([Dir], nothing) --> {directory(Dir, Entries)}, foldl(add(Dir), Entries).
addid(['Live Radio'], nothing) --> {live_services(Services)}, foldl(add_live, Services).

addid(['PID', PID], just(Id)) --> {pid_id(PID, Id)}, add_pid(PID).
addid(['Live Radio', LongName], just(Id)) --> {live_service(S, LongName), pid_id(S, Id)}, add_live(S-LongName).
addid(['In Progress', PID], just(Id)) --> {once(pid_entry(_, PID, E)), pid_id(PID, Id)}, add('In Progress', E).
addid(['youtube', YT_ID], just(Id)) --> {pid_id(YT_ID, Id)}, add_youtube(YT_ID).
addid([ServiceName, PID], just(Id)) -->
   {service(S, ServiceName), ensure_service_schedule(S),  pid_entry(S, PID, E), pid_id(PID, Id)},
   add(ServiceName, E).

add_live(S-SLN) --> {live_service_tags(S-SLN, Tags), live_url(S, URL)}, [song(S, =(URL), Tags)].
add(Dir, E) --> {entry_tags(Dir, E, PID, Tags, [])}, [song(PID, database:entry_url(E), Tags)].
entry_url(E, URL) :- entry_xurl(_, E, _-URL).

add_pid(PID) --> [song(PID, database:version_url(V), [file-PID|Tags])], {pid_version(PID, V), version_tags(V, Tags)}.
version_tags(V, [duration-D, 'Title'-T, 'Comment'-S1]) :- maplist(version_prop(V), [duration(D), title(T), summary(S)]), to_one_line(S, S1).
version_url(V, URL) :- version_prop(V, vpid(VPID)), prog_xurl(_, vpid(VPID), _-URL).

add_youtube(YT_ID) -->
   [song(YT_ID, AudioURL, [file-File|Tags])],
   {path_file(['youtube', YT_ID], File), youtube_info(YT_ID, '251', AudioURL, Tags)}.

youtube_info(YT_ID, Format, =(URL), [duration-D, 'Title'-Title]) :-
   parse_url(PageURL, [protocol(https), host('www.youtube.com'), path('/watch'), search([v=YT_ID])]),
   shell_lines('yt-dlp', ['--format', Format, '--print', 'title', '--print', 'duration', '--print', 'urls', PageURL],
               [Title, Duration, URL |_]),
   number_string(D, Duration).

shell_lines(Cmd, Args, Lines) :-
   setup_call_cleanup(
      process:process_create(path(Cmd), Args, [stdout(pipe(Out))]),
      read_lines(Out, Lines),
      close(Out)).

read_lines(Out, Lines) :-
   read_line_to_string(Out, Line1),
   read_lines(Line1, Out, Lines).

read_lines(end_of_file, _, []) :- !.
read_lines(Line, Out, [Line|Lines]) :- read_lines(Out, Lines).

% --- query db contents ---

%% lsinfo(+Path:path)// is det.
%  Generates codes for directory listing of named entities. If Paths is empty, then outputs root directory entries,
%  ie for 'In Progress', 'Live Radio', and all services (radio stations). Otherwise, Things must contain a single
%  directory name selected from one of the root entries.
lsinfo([]) -->
   foldl(report(directory), ['In Progress', 'Live Radio']),
   {findall(S-SLN, service(S, SLN), Services)}, foldl(service_dir, Services).
lsinfo(['Live Radio']) --> {live_services(Services)}, foldl(live_radio, Services).
lsinfo([Dir]) --> {directory(Dir, Items)}, foldl(programme(Dir), Items).

directory('In Progress', Items) :-
   findall(E, (state(position(PID), _), once(pid_entry(_, PID, E))), Items).
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

%% db_find(_, +Filters:list(mpd_filter), -Paths:list(path)) is det.
%% db_find(_, +Filters:list(mpd_filter))// is det.
%  Find and return or report as codes list of items matching given filters.
db_find(_, Filters, Paths) :- find(Filters, Tracks), maplist(track_path, Tracks, Paths).
db_find(_, Filters) --> {find(Filters, Tracks)}, foldl(found_track, Tracks).

%% db_count(+Filters:list(mpd_filter))// is det.
%  Counts items in database matching given MPD filters, which must match one of several
%  specific forms. See find/2 for details.
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

%% db_list(+T:tag, +Filters:list(mpd_filter), +GroupBy:list(grouping_tag))// is det.
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

%% db_image(+Kind:oneof([series, episode]), +Path:path, -URL:atom) is det.
db_image(Kind, [Dir, PID], URL) :- service(S, Dir), pid_entry(S, PID, E), entry_image(Kind, E, URL).
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
