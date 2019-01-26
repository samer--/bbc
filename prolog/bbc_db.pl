:- module(bbc_db, [service/1, service/3, time_service_schedule/3, service_schedule/2, service_live_url/2,
                   service_entry/2, entry/1, prop/2, entry_xurl/3, play_entry/2, play_entry/3, interval_times/3,
                   old_service_entry/2, entry_maybe_parent/3, service_parent_child/3, service_parent_children/3]).

:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_sgml_plugin)).
:- use_module(library(lambda)).
:- use_module(library(fileutils)).
:- use_module(library(insist)).
:- use_module(library(memo)).
:- use_module(bbc_tools, [log_failure/1, log_and_succeed/1, on_accept/2]).

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

atom_contains(A,Sub) :- once(sub_atom(A, _, _, _, Sub)).
player(gst123).
player('gst-play-1.0').

service(S) :- service(S, _, _).
service(bbc_radio_one,   'R1', 'BBC Radio 1').
service(bbc_radio_two,   'R2', 'BBC Radio 2').
service(bbc_radio_three, 'R3', 'BBC Radio 3').
service(bbc_radio_fourfm,'R4', 'BBC Radio 4 FM').
service(bbc_radio_four_extra, 'R4X', 'BBC Radio 4 Extra').
service(bbc_6music, '6Music', 'BBC 6 Music').
service(bbc_world_service, 'World', 'BBC World Service').

service_live_url(S, URL) :-
   service(S, _, _),
   format(string(URL), 'http://a.files.bbci.co.uk/media/live/manifesto/audio/simulcast/dash/uk/dash_full/ak/~s.mpd', [S]).

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

pid_version(PID, V) :- uget(playlist(PID), PL), member(V, PL.allAvailableVersions).
version_prop(V, vpid(VPID)) :- member(I, V.smpConfig.items), atom_string(VPID, I.vpid).
version_prop(V, duration(D)) :- member(I, V.smpConfig.items), D = I.duration.
version_prop(V, types(V.types)).

mediaset(Fmt, MS, VPID, Result) :- uget(u_mediaset(Fmt, MS, VPID), Result).

entry(E) :- service_entry(_, E).
service_entry(S, E) :-
   service_schedule(S, Schedule),
   xpath(Schedule, /schedule/entry, E).
old_service_entry(S, E) :-
   order_by([desc(T)], snapshot_time_service(T, S)),
   time_service_schedule(T, S, Schedule),
   xpath(Schedule, /schedule/entry, E).
snapshot_time_service(T, S) :- browse(bbc_db:time_service_schedule(T, S, _)).

title_contains(Sub, E) :-
   xpath(E, title(text), T),
   maplist(downcase_atom, [Sub, T], [SubLower, TLower]),
   atom_contains(TLower, SubLower).

play_entry(Fmt, E) :- player(Player), play_entry(Player, Fmt, E).
play_entry(Player, Fmt, E) :-
   maplist(prop(E), [pid(PID), title(Title), link(Fmt, URL)]),
   format(user_error, 'Playing ~w as ~w: ~w...\n', [PID, Fmt, Title]),
   play_url(Player, URL).

play_url(Player, URL) :-
   absolute_file_name(path(Player), _, [solutions(all), access(read)]),
   format(string(C), '~w "~s"', [Player, URL]),
   shell(C).

prop(E, key(X)) :- xpath(E, key(text), X).
prop(E, vpid(X)) :- xpath(E, /self(@pid), X).
prop(E, vpid(X)) :- prop(E, pid(PID)), pid_version(PID, V), version_prop(V, vpid(X)).
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
entry_xurl(best(Fmt), E, XURL) :-
   aggregate(max(B, XUs), setof(XU, entry_fmt_bitrate_xurl(E, Fmt, B, XU), XUs), max(_, XURLs)),
   member(XURL, XURLs).

entry_fmt_bitrate_xurl(E, Fmt, BR, Expiry-HREF) :-
   entry_media('iptv-all', E, M), number_string(BR, M.bitrate),
   media_connection(M, C), C >:< _{transferFormat:Fmt, protocol:"http", href:HREF},
   connection_expiry(C, Expiry).

entry_maybe_parent(T, E, just(PPID-Name)) :- prop(E, parent(PPID, T, Name)), !.
entry_maybe_parent(_, _, nothing).

service_entry_pid_parent(Service, E, PID, Parent) :-
   service_entry(Service, E),
   entry_maybe_parent(E, _, Parent),
   prop(E, pid(PID)).

service_parent_child(Service, MParent, E1) :-
   setof(E, service_entry_pid_parent(Service, E, _PID, MParent), [E1|_]).

service_parent_children(Service, MParent, Children) :-
   setof(E, service_parent_child(Service, MParent, E), Children).

user:portray(ts(Timestamp)) :- format_time(user_output, '<%FT%T%z>', Timestamp).
user:portray(element(entry, As, Es)) :-
   maplist(xpath(element(entry, As, Es)), [pid(text), title(text)], [PID, Title]),
   format('<~w|~s>', [PID, Title]).
