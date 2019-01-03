:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(insist)).
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_sgml_plugin)).
:- use_module(library(memo)).
:- use_module(library(lambda)).

/*
   see https://docs.google.com/document/pub?id=111sRKv1WO78E9Mf2Km91JNCzfbmfU0QApsZyvnRYFmU
   schedule/entry/key
   /schedule/entry/pid
     http://bbc.co.uk/programmes/:pid
     http://bbc.co.uk/programmes/:pid/microsite
     http://bbc.co.uk/programmes/:pid.rdf - returns an XML document in the RDF specification about the programme.
*/

atom_contains(A,Sub) :- sub_atom(A, _, _, _, Sub).
with_url(URL, Stream, Goal) :- setup_call_cleanup(http_open(URL, Stream, []), Goal, close(Stream)).
get_as(json, URL, Dict) :- with_url(URL, In, json_read_dict(In, Dict)).
get_as(xml, URL, DOM)   :- with_url(URL, In, load_xml(In, DOM, [space(remove)])).
get_as(pls, URL, Codes) :- with_url(URL, In, read_file_to_codes(In, Codes, [])).
uget(Head, Result) :-
   call(Head, Fmt, Pattern-Args),
   format(atom(URL), Pattern, Args),
   get_as(Fmt, URL, Result).

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

:- volatile_memo mediaset(+atom, +atom, +atom, -compount).
mediaset(Fmt, MS, VPID, Result) :- uget(u_mediaset(Fmt, MS, VPID), Result).

service_entry(S, E) :-
   service_schedule(S, Schedule),
   xpath(Schedule, /schedule/entry, E).

title_contains(Sub, E) :-
   xpath(E, title(text), T), 
   maplist(downcase_atom, [Sub, T], [SubLower, TLower]),
   atom_contains(TLower, SubLower).

user:portray(element(entry, As, Es)) :-
   maplist(xpath(element(entry, As, Es)), [/entry(@pid), title(text)], [PID, Title]),
   format('<~w|~s>', [PID, Title]).

play_entry(Fmt, E) :-
   maplist(xpath(E), [pid(text), title(text)], [PID, Title]),
   xpath(E, links/link(@transferformat=Fmt, text), URL),
   format('Playing ~w: ~w...\n', [PID, Title]),
   play_url(URL).

play_connection(Conn) :- play_url(Conn.href).

play_url(URL) :-
	player(Player),
   format(string(C), '~w "~s"', [Player, URL]),
   shell(C).

prop(E, vpid(VPID)) :- xpath(E, /entry(@pid), VPID).
prop(E, pid(PID)) :- xpath(E, pid(text), PID).
prop(E, title(T)) :- xpath(E, title(text), T).

pl_vpid(PL, PL.defaultAvailableVersion.pid).
pl_vpid(PL, VPID) :- member(V, PL.allAvailableVersions), VPID = V.pid.

entry_connection(MST, E, C) :-
   prop(E, vpid(VPID)), 
   catch(mediaset(json, MST, VPID, MS), _, fail),
   member(M, MS.media), M.bitrate = "320", 
   member(C, M.connection). 

service_entry_connection(MST, Service, E, C) :-
   service_entry(Service, E), 
   entry_connection(MST, E, C),
   C.transferFormat="hls", C.protocol="http", 
   atom_contains(C.supplier, 'akamai').

write_service_playlist(MST, Service) :-
   writeln('#EXTM3U'),
   forall(service_entry_connection(MST, Service, E, C),
          (prop(E, title(Title)), format('#EXTINF:-1, ~s\n', [Title]), writeln(C.href))).

save_service_playlist(Dir, Service) :-
   service(Service),
   format(string(FN), '~s/~s.m3u', [Dir, Service]),
   setup_call_cleanup(open(FN, write, S, []), 
                      with_output_to(S, write_service_playlist('iptv-all', Service)), 
                      close(S)).
% vim: set filetype=prolog
