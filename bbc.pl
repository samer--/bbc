:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_sgml_plugin)).
:- use_module(library(lambda)).
:- use_module(library(fileutils)).
:- use_module(library(insist)).
:- use_module(library(memo)).

/*
   see https://docs.google.com/document/pub?id=111sRKv1WO78E9Mf2Km91JNCzfbmfU0QApsZyvnRYFmU
   schedule/entry/key
   /schedule/entry/pid
*/

xbrowse(element(_,As,_), @A, V) :- member(A=V, As).
xbrowse(element(_,_,Es), Path, Val) :- member(E, Es), xbrowse_sub(E, Path, Val).
xbrowse_sub(element(Tag,As,Es), Tag:Path, Val) :- xbrowse(element(Tag,As,Es), Path, Val).
xbrowse_sub(Text, text, Text) :- Text \= element(_,_,_).

xpath_attr_time(E, Path, A, ts(Time)) :- xpath(E, Path, E1), xpath(E1, /self(@A), T), parse_time(T, iso_8601, Time).
xpath_interval(E, Path, T1-T2) :- maplist(xpath_attr_time(E, Path), [start, end], [T1, T2]).

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
log_failure(G, Desc) :- G -> true; debug(bbc, 'failed: ~p', [Desc]), fail.
atom_contains(A,Sub) :- sub_atom(A, _, _, _, Sub).

player(gst123).
player('gst-play-1.0').

service(bbc_radio_two).
service(bbc_radio_three).
service(bbc_radio_fourfm).
service(bbc_radio_four_extra).
service(bbc_6music).
service(bbc_world_service).

mediaset_format(F) :- member(F, [json, xml, pls]).
mediaset_type(aod, MS) :- member(MS, ['pc', 'audio-syndication', 'audio-syndication-dash', 'apple-ipad-hls', 'iptv-all']).
mediaset_type(live_only, MS) :- member(MS, ['apple-icy-mp3a', 'http-icy-aac-lc-a']).

service_availability(S, xml, 'http://www.bbc.co.uk/radio/aod/availability/~s.xml'-[S]) :- service(S).
playlist(PID, json, 'http://www.bbc.co.uk/programmes/~s/playlist.json'-[PID]).
u_mediaset(Fmt, MediaSet, VPID, Fmt, URLForm-[MediaSet, Fmt, VPID]) :-
   URLForm = 'http://open.live.bbc.co.uk/mediaselector/6/select/version/2.0/mediaset/~s/format/~s/vpid/~s',
   mediaset_type(_, MediaSet), mediaset_format(Fmt).

:- volatile_memo service_schedule(+atom, -list(compound)).
service_schedule(S, Schedule) :- insist(uget(service_availability(S), [Schedule])).

:- volatile_memo pid_playlist(+atom, -dict).
pid_playlist(PID, Playlist) :- uget(playlist(PID), Playlist).

% :- volatile_memo mediaset(+atom, +atom, +atom, -compount).
mediaset(Fmt, MS, VPID, Result) :- uget(u_mediaset(Fmt, MS, VPID), Result).

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

prop(E, vpid(X)) :- xpath(E, /self(@pid), X).
prop(E, pid(X)) :- xpath(E, pid(text), X).
prop(E, title(X)) :- xpath(E, title(text), X).
prop(E, service(X)) :- xpath(E, service(text), X).
prop(E, synopsis(X)) :- xpath(E, synopsis(text), X).
prop(E, duration(X)) :- xpath(E, broadcast(@duration(number)), X).
prop(E, availability(X)) :- xpath_interval(E, availability, X).
prop(E, link(F,URL)) :- xpath(E, links/link(@transferformat=F,text), URL).
prop(E, parent(PID, Type, Name)) :-
   xpath(E, parents/parent, P),
   maplist(xpath(P), [/self(@pid), /self(@type), /self(text)], [PID, Type, Name]).

pl_vpid(PL, PL.defaultAvailableVersion.pid).
pl_vpid(PL, VPID) :- member(V, PL.allAvailableVersions), VPID = V.pid.
in_interval(ts(A)-ts(B), X) :- A =< X, X =< B.

media_connection(M, C) :- member(C, M.connection).

mediaset_expiry(MS, Expiry) :-
   member(M, MS.media), media_connection(M, C),
   parse_time(C.authExpires, Expiry).

entry_media_expiry(MST, E, M, ts(MinExp)) :-
   prop(E, vpid(VPID)),
   mediaset_type(_, MST),
   catch(mediaset(json, MST, VPID, MS), _, fail),
   aggregate(min(Exp), mediaset_expiry(MS, Exp), MinExp),
   member(M, MS.media).

service_entry_url(Strategy, Now, Service, E, URL) :-
   service_entry(Service, E),
   log_failure(prop(E, availability(Interval)), no_availability(E)),
   log_failure(in_interval(Interval, Now), not_currently_available(E)),
   log_failure(entry_url(Strategy, E, URL), no_entry_url(Strategy, E)).

entry_url(redir(Fmt), E, HREF) :- prop(E, link(Fmt, HREF)).
entry_url(best_hls, E, URL) :-
   aggregate(max(B, Us), setof(U, entry_bitrate_hls_url(E, B, U), Us), max(_, URLs)),
   insist(URLs = [URL]).

entry_bitrate_hls_url(E, BR, HREF) :-
   entry_media_expiry('iptv-all', E, M, _), number_string(BR, M.bitrate),
   media_connection(M, C), C >:< _{transferFormat:"hls", protocol:"http", href:HREF},
   atom_contains(C.supplier, 'akamai').

write_playlist(EntryURL) :-
   writeln(user_error, 'Writing playlist...'),
   writeln('#EXTM3U'),
   forall(call(EntryURL, E, URL),
          (prop(E, title(Title)), prop(E, duration(Dur)),
           format('#EXTINF:~d, ~s\n', [Dur, Title]), writeln(URL))).

save_service_playlist(Dir, Service) :-
   service(Service), get_time(Now),
   format(string(FN), '~s/~s.m3u', [Dir, Service]),
   with_output_to_file(FN, write_playlist(service_entry_url(best_hls, Now, Service))).

user:portray(ts(Timestamp)) :- format_time(user_output, '<%FT%T%z>', Timestamp).
user:portray(element(entry, As, Es)) :-
   maplist(xpath(element(entry, As, Es)), [/entry(@pid), title(text)], [PID, Title]),
   format('<~w|~s>', [PID, Title]).

% vim: set filetype=prolog
