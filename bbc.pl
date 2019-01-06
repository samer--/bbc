:- module(bbc, [entry/1, service_entry/2, save_service_playlist/3,  maintain_service_playlist/2,
                service/1, service/3, service_schedule/2]).

:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_sgml_plugin)).
:- use_module(library(lambda)).
:- use_module(library(fileutils)).
:- use_module(library(insist)).
:- use_module(library(memo)).

% see https://docs.google.com/document/pub?id=111sRKv1WO78E9Mf2Km91JNCzfbmfU0QApsZyvnRYFmU

xpath_attr_time(E, Path, A, ts(Time)) :- xpath(E, Path, E1), xpath(E1, /self(@A), T), parse_time(T, iso_8601, Time).
xpath_interval(As, E, Path, T1-T2) :- maplist(xpath_attr_time(E, Path), As, [T1, T2]).
in_interval(ts(A)-ts(B), X) :- A =< X, X =< B.
interval_times(ts(S)-ts(E), S, E).

with_url(URL, Stream, Goal) :- setup_call_cleanup(http_open(URL, Stream, []), Goal, close(Stream)).
get_as(json, URL, Dict) :- with_url(URL, In, json_read_dict(In, Dict)).
get_as(xml, URL, DOM)   :- with_url(URL, In, load_xml(In, DOM, [space(remove)])).
get_as(pls, URL, Codes) :- with_url(URL, In, read_file_to_codes(In, Codes, [])).
uget(Head, Result) :-
   call(Head, Fmt, Pattern-Args),
   format(string(URL), Pattern, Args),
   get_as(Fmt, URL, Result).

on_accept(Query, Goal) :- call_cleanup((Query, Success=true), (Success=true, Goal)).
play_on_accept(E, Query) :- on_accept(Query, play_entry(_, E)).
log_failure(G) :- G -> true; debug(bbc, 'failed: ~p', [G]), fail.
atom_contains(A,Sub) :- sub_atom(A, _, _, _, Sub).
log_and_succeed(G) :-
   (  catch(G, Ex, debug(bbc, 'Exception on ~q: ~p', [G, Ex])) -> true
   ;  debug(bbc, 'Failed on ~q', [G])
   ).

player(gst123).
player('gst-play-1.0').

service(S) :- service(S, _, _).
service(bbc_radio_two,   'R2', 'BBC Radio 2').
service(bbc_radio_three, 'R3', 'BBC Radio 3').
service(bbc_radio_fourfm,'R4', 'BBC Radio 4 FM').
service(bbc_radio_four_extra, 'R4X', 'BBC Radio 4 Extra').
service(bbc_6music, '6Music', 'BBC 6 Music').
service(bbc_world_service, 'World', 'BBC World Service').

mediaset_format(F) :- member(F, [json, xml, pls]).
mediaset_type(aod, MS) :- member(MS, ['pc', 'audio-syndication', 'audio-syndication-dash', 'apple-ipad-hls', 'iptv-all']).
mediaset_type(live_only, MS) :- member(MS, ['apple-icy-mp3a', 'http-icy-aac-lc-a']).

service_availability(S, xml, 'http://www.bbc.co.uk/radio/aod/availability/~s.xml'-[S]) :- service(S).
playlist(PID, json, 'http://www.bbc.co.uk/programmes/~s/playlist.json'-[PID]).
u_mediaset(Fmt, MediaSet, VPID, Fmt, URLForm-[MediaSet, Fmt, VPID]) :-
   URLForm = 'http://open.live.bbc.co.uk/mediaselector/6/select/version/2.0/mediaset/~s/format/~s/vpid/~s',
   mediaset_type(_, MediaSet), mediaset_format(Fmt).

:- volatile_memo time_service_schedule(+number, +atom, -list(compound)).
time_service_schedule(_, S, Schedule) :- insist(uget(service_availability(S), [Schedule])).

service_schedule(S, Schedule) :- aggregate(max(T,Sch), browse(time_service_schedule(T, S, Sch)), max(_, Schedule)).
schedule_timespan(S, X) :- xpath_interval([start_date, end_date], S, /self, X).

:- volatile_memo pid_playlist(+atom, -dict).
pid_playlist(PID, Playlist) :- uget(playlist(PID), Playlist).

mediaset(Fmt, MS, VPID, Result) :- uget(u_mediaset(Fmt, MS, VPID), Result).

entry(E) :- service_entry(_, E).
service_entry(S, E) :-
   service_schedule(S, Schedule),
   xpath(Schedule, /schedule/entry, E).

title_contains(Sub, E) :-
   xpath(E, title(text), T),
   maplist(downcase_atom, [Sub, T], [SubLower, TLower]),
   atom_contains(TLower, SubLower).

play_entry(Fmt, E) :-
   maplist(xpath(E), [pid(text), title(text)], [PID, Title]),
   xpath(E, links/link(@transferformat=Fmt, text), URL),
   format(user_error, 'Playing ~w as ~w: ~w...\n', [PID, Fmt, Title]),
   play_url(URL).

play_url(URL) :-
	player(Player),
   absolute_file_name(path(Player), _, [solutions(all), access(read)]),
   format(string(C), '~w "~s"', [Player, URL]),
   shell(C).

prop(E, key(X)) :- xpath(E, key(text), X).
prop(E, vpid(X)) :- xpath(E, /self(@pid), X).
prop(E, pid(X)) :- xpath(E, pid(text), X).
prop(E, title(X)) :- xpath(E, title(text), X).
prop(E, service(X)) :- xpath(E, service(text), X).
prop(E, synopsis(X)) :- xpath(E, synopsis(text), X).
prop(E, duration(X)) :- xpath(E, broadcast(@duration), Y), atom_number(Y,X).
prop(E, availability(X)) :- xpath_interval([start, end], E, availability, X).
prop(E, broadcast(X)) :- xpath_interval([start, end], E, broadcast, X).
prop(E, link(F,URL)) :- xpath(E, links/link(@transferformat=F,text), URL).
prop(E, parent(PID, Type, Name)) :-
   xpath(E, parents/parent, P),
   maplist(xpath(P), [/self(@pid), /self(@type), /self(text)], [PID, Type, Name]).

media_connection(M, C) :- member(C, M.connection).
connection_expiry(C, Expiry) :- parse_time(C.authExpires, Expiry).

entry_media(MST, E, M) :-
   prop(E, vpid(VPID)),
   mediaset_type(_, MST),
   catch(mediaset(json, MST, VPID, MS), _, fail),
   member(M, MS.media).

entry_xurl(redir(Fmt), E, inf-HREF) :- prop(E, link(Fmt, HREF)).
entry_xurl(best_hls, E, XURL) :-
   aggregate(max(B, XUs), setof(XU, entry_bitrate_hls_xurl(E, B, XU), XUs), max(_, XURLs)),
   insist(XURLs = [XURL]).

entry_bitrate_hls_xurl(E, BR, Expiry-HREF) :-
   entry_media('iptv-all', E, M), number_string(BR, M.bitrate),
   media_connection(M, C), C >:< _{transferFormat:"hls", protocol:"http", href:HREF},
   atom_contains(C.supplier, 'akamai'), connection_expiry(C, Expiry).

entry_maybe_parent(E, just(PPID)) :- prop(E, parent(PPID, _, _)), !.
entry_maybe_parent(_, nothing).

service_entry_pid_parent(Service, E, PID, Parent) :-
   service_entry(Service, E),
   entry_maybe_parent(E, Parent),
   prop(E, pid(PID)).

service_parent_child(Service, MParent, E1) :-
   setof(E, service_entry_pid_parent(Service, E, _PID, MParent), [E1|_]).

service_parent_children(Service, MParent, Children) :-
   setof(E, service_parent_child(Service, MParent, E), Children).

save_service_playlist(Dir, Service, Expiry) :-
   debug(bbc, 'Gathering playlist for ~w...', [Service]),
   findall(Children, service_parent_children(Service, _, Children), Families),
   findall(XU-E, (member(Es, Families), member(E, Es), log_failure(entry_xurl(best_hls, E, XU))), Items),
   aggregate(min(X), E^U^member((X-U)-E, Items), Expiry),
   format(string(FN), '~s/~s.m3u', [Dir, Service]),
   debug(bbc, 'Saving playlist for ~w to <~s>...', [Service, FN]),
   with_output_to_file(FN, (writeln('#EXTM3U'), maplist(write_playlist_item, Items))).

write_playlist_item((_-URL)-E) :-
   maplist(prop(E), [title(Title), duration(Dur), broadcast(B)]),
   interval_times(B, BStart, _),
   format_time(string(BDate), '%x', BStart),
   format('#EXTINF:~d, ~s [~s]\n~w\n', [Dur, Title, BDate, URL]).

user:portray(ts(Timestamp)) :- format_time(user_output, '<%FT%T%z>', Timestamp).
user:portray(element(entry, As, Es)) :-
   maplist(xpath(element(entry, As, Es)), [/entry(@pid), title(text)], [PID, Title]),
   format('<~w|~s>', [PID, Title]).

uninstall_service_playlist_alarm(Dir, Service) :-
   forall(current_alarm(_, maintain_service_playlist(Dir, Service), Id, _),
          uninstall_alarm(Id)).

maintain_service_playlist(Dir, Service) :-
   get_time(Now),
   log_and_succeed(time_service_schedule(Now, Service, _)),
   save_service_playlist(Dir, Service, Expiry),
   alarm_at(Expiry, maintain_service_playlist(Dir, Service), _, [remove(true)]).
% vim: set filetype=prolog
