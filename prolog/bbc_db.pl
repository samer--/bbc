:- module(bbc_db, [service/1, service/3, fetch_new_schedule/1, service_schedule/2, service_live_url/2,
                   service_entry/2, schedule_updated/2, pid_entry/3, entry_prop/2, entry_xurl/3, prog_xurl/3,
                   interval_times/3, entry_maybe_parent/3, entry_parents/2, pid_version/2, version_prop/2]).

:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(zlib)). % enable gzip encoded HTTP data
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_sgml_plugin)).
:- use_module(library(data/pair), [fst/2]).
:- use_module(library(callutils)).
:- use_module(library(insist)).
:- use_module(bbc_tools, [log_failure/1, log_and_succeed/1, sort_by/3, pairf/3]).

% see https://docs.google.com/document/pub?id=111sRKv1WO78E9Mf2Km91JNCzfbmfU0QApsZyvnRYFmU
:- dynamic snapshot_time_service/2, time_service_schedule/3.

interval_times(ts(S)-ts(E), S, E).
in_interval(ts(A)-ts(B), X) :- A =< X, X =< B.
xpath_interval(As, E, Path, T1-T2) :- maplist(xpath_attr_time(E, Path), As, [T1, T2]).
xpath_attr_time(E, Path, A, ts(Time)) :- xpath(E, Path, E1), xpath(E1, /self(@A), T), parse_time(T, iso_8601, Time).

with_url(URL, Stream, Goal) :- setup_call_cleanup(http_open(URL, Stream, []), Goal, close(Stream)).
get_as(json, URL, Dict) :- with_url(URL, In, json_read_dict(In, Dict)).
get_as(xml, URL, DOM)   :- with_url(URL, In, load_xml(In, DOM, [space(remove)])).
get_as(html, URL, DOM)  :- with_url(URL, In, load_html(In, DOM, [space(strict), cdata(string)])).
get_as(pls, URL, Codes) :- with_url(URL, In, read_file_to_codes(In, Codes, [])).
uget(Head, Result) :-
   call(Head, Fmt, Pattern-Args),
   format(string(URL), Pattern, Args),
   get_as(Fmt, URL, Result).

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

service_pid(bbc_radio_one,        p00fzl86).
service_pid(bbc_radio_two,        p00fzl8v).
service_pid(bbc_radio_three,      p00fzl8t).
service_pid(bbc_radio_fourfm,     p00fzl7j).
service_pid(bbc_radio_four_extra, p00fzl7l).
service_pid(bbc_6music,           p00fzl65).
service_pid(bbc_world_service,    p00fzl9p).

service_schedule(Suffix, S, html, 'http://www.bbc.co.uk/schedules/~s~s'-[PID, Suffix]) :- service_pid(S, PID).

service_availability(S, xml, 'http://www.bbc.co.uk/radio/aod/availability/~s.xml'-[S]) :- service(S).
playlist(PID, json, 'http://www.bbc.co.uk/programmes/~s/playlist.json'-[PID]).
u_mediaset(Fmt, MediaSet, VPID, Fmt, URLForm-[MediaSet, Fmt, VPID]) :-
   URLForm = 'http://open.live.bbc.co.uk/mediaselector/6/select/version/2.0/mediaset/~s/format/~s/vpid/~s',
   mediaset_type(_, MediaSet), mediaset_format(Fmt).

fetch_new_schedule(S) :-
   get_time(Now),
   catch(fetch_new_schedule_xml(S, Schedule), error(existence_error(_, _), _),
         fetch_new_schedule_json(S, Schedule)),
   assert(time_service_schedule(Now, S, Schedule)),
   assert(snapshot_time_service(Now, S)).

time_to_year_week(T, YearWeek) :- format_time(atom(YearWeek), '%G/w%V', T).
fetch_new_schedule_json(S, sched(Updated, ETree)) :-
   service(S), get_time(Now),
   format_time(atom(Updated), '%FT%T%z', Now),
   LastWeek is Now - 7*24*3600,
   maplist(time_etree(S), [Now, LastWeek], [ET1, ET2]),
   assoc_merge(ET1, ET2, ETree).

assoc_add(K-V, A1, A2) :- put_assoc(K, A1, V, A2).
assoc_merge(A1, A2, A3) :-
   assoc_to_list(A1, A1List),
   foldl(assoc_add, A1List, A2, A3).

time_etree(S, Time, ETree) :-
   format_time(atom(YW1), '/%G/w%V', Time),
   uget(service_schedule(YW1, S), [DOM]),
   xpath(DOM, body/div(@id='orb-modules')//script(@type='application/ld+json',content), [X]),
   atom_json_dict(X, T, []),
   get_dict('@graph', T, Graph),
   findall(E, graph_entry(Graph, E), Es),
   call(ord_list_to_assoc * sort(1, @<) * maplist(pairf(entry_pid)), Es, ETree).

graph_entry(Graph, entry(Props)) :- member(E, Graph), findall(Prop, jprop(E, Prop), Props).
jprop(E, pid(X)) :- string_to_atom(E.identifier, X).
jprop(E, service(X)) :- string_to_atom(E.publication.publishedOn.name, Long), service(X, _, Long).
jprop(E, title(X)) :- X=E.name.
jprop(E, synopsis(X)) :- X=E.description.
jprop(E, duration(X)) :- jprop(E, broadcast(ts(T1)-ts(T2))), X is (T2-T1).
jprop(E, broadcast(ts(T1)-ts(T2))) :- P=E.publication, maplist(parse_time, [P.startDate, P.endDate], [T1,T2]).
jprop(E, parent(PID, 'Brand', Name)) :-
   get_dict(partOfSeries, E, P),
   string_to_atom(P.identifier, PID),
   Name=P.name.

fetch_new_schedule_xml(S, sched(Updated, ETree)) :-
   insist(uget(service_availability(S), [DOM])),
   xpath(DOM, /self(@updated), Updated),
   findall(E, dom_entry(DOM, E), Es),
   call(ord_list_to_assoc * sort(1, @<) * maplist(pairf(entry_pid)), Es, ETree).

entry_pid(E, PID) :- entry_prop(E, pid(PID)).
dom_entry(DOM, entry(Props)) :- xpath(DOM, /schedule/entry, E), findall(Prop, prop(E, Prop), Props).

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

schedule(latest(S), Schedule) :- service_schedule(S, Schedule).
schedule(any,       Schedule) :- ordered_service_schedule(_, Schedule).
schedule_updated(sched(Updated, _), Updated).
service_schedule(S, Schedule) :- service(S, _, _), once(ordered_service_schedule(S, Schedule)).
ordered_service_schedule(S, Sch) :- order_by([desc(T)], snapshot_time_service(T, S)), time_service_schedule(T, S, Sch).

pid_version(PID, C-I) :- uget(playlist(PID), PL), member(V, PL.allAvailableVersions), C=V.smpConfig, member(I, C.items).
version_prop(_-I, vpid(VPID)) :- atom_string(VPID, I.vpid).
version_prop(_-I, duration(I.duration)).
version_prop(C-_, title(C.title)).
version_prop(C-_, summary(C.summary)).

pid_entry(Mode, PID, E)  :- schedule(Mode, sched(_, ETree)), get_assoc(PID, ETree, E).
service_entry(S, E) :- service_schedule(S, sched(_, ETree)), gen_assoc(_, ETree, E).

title_contains(Sub, E) :-
   xpath(E, title(text), T),
   maplist(downcase_atom, [Sub, T], [SubLower, TLower]),
   once(sub_atom(TLower, _, _, _, SubLower)).

play_url(Player, URL) :-
   absolute_file_name(path(Player), _, [solutions(all), access(read)]),
   format(string(C), '~w "~s"', [Player, URL]),
   shell(C).

entry_prop(entry(Props), Prop) :- member(Prop, Props).

entry_vpid(E, X) :- entry_prop(E, vpid(X)).
entry_vpid(E, X) :- entry_prop(E, pid(PID)), pid_version(PID, V), version_prop(V, vpid(X)).

entry_xurl(Method, E, XURL) :- prog_xurl(Method, entry(E), XURL).
prog_xurl(redir(Fmt), entry(E), inf-HREF) :- \+entry_prop(E, vpid('')), entry_prop(E, link(Fmt, HREF)).
prog_xurl(best(Fmt), Prog, XURL) :-
   aggregate(max(B, XUs), setof(XU, prog_fmt_bitrate_xurl(Prog, Fmt, B, XU), XUs), max(_, XURLs)),
   member(XURL, XURLs).

prog_fmt_bitrate_xurl(entry(E), Fmt, BR, XURL) :-
   entry_vpid(E, VPID), prog_fmt_bitrate_xurl(vpid(VPID), Fmt, BR, XURL).
prog_fmt_bitrate_xurl(vpid(VPID), Fmt, BR, Expiry-HREF) :-
   vpid_media('iptv-all', VPID, M), number_string(BR, M.get(bitrate)),
   member(C, M.connection), _{transferFormat:Fmt, protocol:"http", href:HREF} :< C,
   parse_time(C.authExpires, Expiry).

vpid_media(MST, VPID, M) :-
   mediaset_type(_, MST),
   catch(mediaset(json, MST, VPID, MS), _, fail),
   member(M, MS.media).
mediaset(Fmt, MS, VPID, Result) :- uget(u_mediaset(Fmt, MS, VPID), Result).

entry_parents(E, SortedParents) :-
   findall(T-N, entry_prop(E, parent(_, T, N)), Parents),
   sort_by(parent_type_priority * fst, Parents, SortedParents).
parent_type_priority('Brand', 1).
parent_type_priority('Series', 2).

entry_maybe_parent(T, E, just(PPID-Name)) :- entry_prop(E, parent(PPID, T, Name)), !.
entry_maybe_parent(_, _, nothing).

user:portray(ts(Timestamp)) :- format_time(user_output, '<%FT%T%z>', Timestamp).
user:portray(entry(Props)) :-
   maplist(entry_prop(entry(Props)), [pid(PID), title(Title)]),
   format('<~w|~s>', [PID, Title]).
