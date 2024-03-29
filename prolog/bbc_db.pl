:- module(bbc_db, [service/1, service/2, service_updated/2, fetch_new_schedule/1, service_live_url/2,
                   service_entry/2, pid_entry/3, pid_tracks/2, entry_prop/2, entry_xurl/3, prog_xurl/3,
                   interval_times/3, entry_maybe_parent/3, entry_parents/2, pid_version/2, version_prop/2]).

:- use_module(library(sgml)).
:- use_module(library(xpath)).
:- use_module(library(zlib), [zopen/3]). % enable gzip encoded HTTP data
:- use_module(library(http/http_open)).
:- use_module(library(http/json)).
:- use_module(library(http/http_sgml_plugin)).
:- use_module(library(data/pair), [fst/2]).
:- use_module(library(callutils)).
:- use_module(library(insist)).
:- use_module(library(memo)).
:- use_module(bbc_tools, [log_failure/1, log_and_succeed/1, sort_by/3, pairf/3]).

:- if(\+clause(http:encoding_filter(gzip, _, _), _)).
% older versions of SWI don't have this clause
:- multifile http:encoding_filter/3.
http:encoding_filter(gzip, In0, In) :- zopen(In0, In, [ close_parent(true) ]).
:- endif.

% see https://docs.google.com/document/pub?id=111sRKv1WO78E9Mf2Km91JNCzfbmfU0QApsZyvnRYFmU
:- dynamic snapshot_time_service/2, time_service_schedule/3.

interval_times(ts(S)-ts(E), S, E).

% --- URL and HTTP tools ----
with_url(URL, Stream, Goal) :- setup_call_cleanup(http_open(URL, Stream, []), Goal, close(Stream)).
get_as(json, URL, Dict)  :- with_url(URL, In, json_read_dict(In, Dict)).
get_as(xml,  URL, DOM)   :- with_url(URL, In, load_xml(In, DOM, [space(remove)])).
get_as(html, URL, DOM)   :- with_url(URL, In, load_html(In, DOM, [space(preserve), cdata(string)])).
get_as(pls,  URL, Codes) :- with_url(URL, In, read_file_to_codes(In, Codes, [])).
uget(Head, Result) :-
   call(Head, Fmt, Pattern-Args),
   format(string(URL), Pattern, Args),
   debug(mpd(bbc, s(s(0))), "Getting as ~w: ~w", [Fmt, URL]),
   get_as(Fmt, URL, Result).

% --- service database
service(S) :- service(S, _, _).
service(S, ServiceName) :- service(S, _, ServiceName).

service(p00fzl86, bbc_radio_one,        'BBC Radio 1').
service(p00fzl8v, bbc_radio_two,        'BBC Radio 2').
service(p00fzl8t, bbc_radio_three,      'BBC Radio 3').
service(p00fzl7j, bbc_radio_fourfm,     'BBC Radio 4 FM').
service(p00fzl7l, bbc_radio_four_extra, 'BBC Radio 4 Extra').
service(p00fzl65, bbc_6music,           'BBC 6 Music').
service(p00fzl9p, bbc_world_service,    'BBC World Service Online').

% --- getting the schedules -------------------------------------------------

%% fetch_new_schedule(+S:service) is det.
%  Attempt to get and assert into the local (volatile) database the schedule for the given service.
%  First, attempt to get XML schedule, and if that fails, try to get JSON schedule by scraping 3 weeks
%  worth of schedule web pages. Results are added to time_service_schedule/3 and snapshot_time_service/2.
%  Any schedules downloaded more than 60 days ago are deleted.
fetch_new_schedule(S) :-
   get_time(Now),
   catch(fetch_new_schedule_xml(S, Schedule), error(existence_error(_, _), _),
         fetch_new_schedule_json(3, S, Schedule)),
   assert(time_service_schedule(Now, S, Schedule)),
   assert(snapshot_time_service(Now, S)),
   forall((snapshot_time_service(T, S), T < Now - 60 * 24 * 3600),
          (format_time(string(Downloaded), '%FT%T%z', T), service(S, ServiceName),
           debug(mpd(bbc,s(s(0))), "Dropping schedule for '~w' downloaded ~w...", [ServiceName, Downloaded]),
           retract(snapshot_time_service(T, S)),
           retract(time_service_schedule(T, S, _)))).

% -- new JSON schedule from web page
service_web_sched(Suffix, S, html, 'https://www.bbc.co.uk/schedules/~s~s'-[S, Suffix]) :- service(S).

fetch_new_schedule_json(WeeksBack, S, ETree) :-
   service(S), get_time(Now),
   findall(E, (between(0, WeeksBack, W), call(graph_entry * service_time_graph(S) * weeks_back(Now), W, E)), Es),
   call(ord_list_to_assoc * sort(1, @<) * maplist(pairf(entry_pid)), Es, ETree).

weeks_back(Now, W, T) :- T is Now - W*7*24*3600.
graph_entry(Graph, entry(Props)) :- member(E, Graph), findall(Prop, jprop(E, Prop), Props).
service_time_graph(S, Time, Graph) :-
   format_time(atom(YW1), '/%G/w%V', Time),
   insist(uget(service_web_sched(YW1, S), [DOM])),
   insist(dom_graph(DOM, Graph)).


dom_graph(DOM, Graph) :-
   xpath(DOM, body/div(@id='orb-modules')//script(@type='application/ld+json',content), [X]),
   atom_json_dict(X, T, []), get_dict('@graph', T, Graph).

jprop(E, pid(X))      :- string_to_atom(E.identifier, X).
jprop(E, service(X))  :- string_to_atom(E.publication.publishedOn.name, X).
jprop(E, episode(X))  :- get_dict(episodeNumber, E, X), X \= null.
jprop(E, date(X))     :- get_dict(datePublished, E, X).
jprop(E, image(X))    :- get_dict(image, E, X).
jprop(E, title(X))    :- X=E.name.
jprop(E, synopsis(X)) :- X=E.description.
jprop(E, duration(X)) :- jprop(E, broadcast(ts(T1)-ts(T2))), X is (T2-T1).
jprop(E, broadcast(ts(T1)-ts(T2))) :- P=E.publication, maplist(parse_time, [P.startDate, P.endDate], [T1,T2]).
jprop(E, parent(PID, 'Brand', Name, Props)) :-
   get_dict(partOfSeries, E, P),
   findall(Prop, parent_prop(P, Prop), Props),
   string_to_atom(P.identifier, PID),
   string_to_atom(P.name, Name).

parent_prop(P, synopsis(X)) :- get_dict(description, P, X).
parent_prop(P, image(X))    :- get_dict(image, P, X).

% -- old XML feed
service_xml_feed(S, xml,  'http://www.bbc.co.uk/radio/aod/availability/~s.xml'-[P]) :- service(S, P).

fetch_new_schedule_xml(S, ETree) :-
   insist(uget(service_xml_feed(S), [DOM])),
   findall(E, dom_entry(DOM, E), Es),
   call(ord_list_to_assoc * sort(1, @<) * maplist(pairf(entry_pid)), Es, ETree).

dom_entry(DOM, entry(Props)) :- xpath(DOM, /schedule/entry, E), findall(Prop, prop(E, Prop), Props).
prop(E, key(X))      :- xpath(E, key(text), X).
prop(E, vpid(X))     :- xpath(E, /self(@pid), X).
prop(E, pid(X))      :- xpath(E, pid(text), X).
prop(E, title(X))    :- xpath(E, title(text), X).
prop(E, service(X))  :- xpath(E, service(text), Y), service(_, Y, X).
prop(E, synopsis(X)) :- xpath(E, synopsis(text), X).
prop(E, duration(X)) :- xpath(E, broadcast(@duration), Y), atom_number(Y,X).
prop(E, availability(X)) :- xpath_interval([start, end], E, availability, X).
prop(E, broadcast(X)) :- xpath_interval([start, end], E, broadcast, X).
prop(E, link(F,URL)) :- xpath(E, links/link(@transferformat=F,text), URL).
prop(E, parent(PID, Type, Name, [])) :-
   xpath(E, parents/parent, P),
   maplist(xpath(P), [/self(@pid), /self(@type), /self(text)], [PID, Type, Name]). % FIXME: Name must be atom

xpath_interval(As, E, Path, T1-T2) :- maplist(xpath_attr_time(E, Path), As, [T1, T2]).
xpath_attr_time(E, Path, A, ts(Time)) :- xpath(E, Path, E1), xpath(E1, /self(@A), T), parse_time(T, iso_8601, Time).

% --- schedule access -----------------

%% service_updated(+S: service, -T:atom) is semidet.
%% service_updated(-S: service, -T:atom) is nondet.
%  True when the schedule of services S was updated most recently at time T (formatted time).
service_updated(S, Updated) :-
   service(S), aggregate_all(max(T), snapshot_time_service(T, S), TMax),
   format_time(atom(Updated), '%FT%T%z', TMax).

ordered_service_schedule(S, Sch) :- order_by([desc(T)], snapshot_time_service(T, S)), time_service_schedule(T, S, Sch).

%% pid_entry(+S:service, +P:pid, -E:entry) is semidet.
%% pid_entry(-S:service, +P:pid, -E:entry) is nondet.
%  True when E is the latest known entry for a PID in the schedules for service S.
pid_entry(S, PID, E) :- distinct(S-PID, (ordered_service_schedule(S, Schedule), get_assoc(PID, Schedule, E))).

%% service_entry(+S:service, -E:entry) is semidet.
%% service_entry(-S:service, -E:entry) is nondet.
%  True when E is the latest known entry for a PID in the schedules for service S.
service_entry(S, E) :- service(S), distinct(PID, service_entry_pid(S, E, PID)).
service_entry_pid(S, E, PID) :- ordered_service_schedule(S, ETree), gen_assoc(_, ETree, E), entry_pid(E, PID).

% --- entry attributes ----------------
entry_prop(entry(Props), Prop) :- member(Prop, Props).
entry_pid(E, PID) :- entry_prop(E, pid(PID)).
entry_vpid(E, X) :- entry_prop(E, vpid(X)).
entry_vpid(E, X) :- entry_prop(E, pid(PID)), pid_version(PID, V), version_prop(V, vpid(X)).
entry_xurl(Method, E, XURL) :- prog_xurl(Method, entry(E), XURL).

entry_parents(E, SortedParents) :-
   findall(T-N, entry_prop(E, parent(_, T, N, _)), Parents),
   sort_by(parent_type_priority * fst, Parents, SortedParents).
parent_type_priority('Brand', 1).
parent_type_priority('Series', 2).

entry_maybe_parent(T, E, just(PPID-Name)) :- entry_prop(E, parent(PPID, T, Name, _)), !.
entry_maybe_parent(_, _, nothing).

entry_title_contains(Sub, E) :-
   xpath(E, title(text), T),
   maplist(downcase_atom, [Sub, T], [SubLower, TLower]),
   once(sub_atom(TLower, _, _, _, SubLower)).

% --- versions and media streams -------------------------------

service_live_url(S, URL) :-
   service(S, P, _),
   % A-B = 'media/live/manifesto'-'dash_full/ak',
   A-B = 'ms6/live/3441A116-B12E-4D2F-ACA8-C1984642FA4B'-'pc_hd_abr_v2/aks',
   format(string(URL), 'http://a.files.bbci.co.uk/~s/audio/simulcast/dash/uk/~s/~s.mpd', [A, B, P]).

playlist(PID, json, 'http://www.bbc.co.uk/programmes/~s/playlist.json'-[PID]).
pid_version(PID, C-I) :- uget(playlist(PID), PL), member(V, PL.allAvailableVersions), C=V.smpConfig, member(I, C.items).

version_prop(_-I, vpid(VPID)) :- atom_string(VPID, I.vpid).
version_prop(_-I, duration(I.duration)).
version_prop(C-_, title(C.title)).
version_prop(C-_, summary(C.summary)).

play_url(Player, URL) :-
   absolute_file_name(path(Player), _, [solutions(all), access(read)]),
   format(string(C), '~w "~s"', [Player, URL]),
   shell(C).

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

u_mediaset(Fmt, MediaSet, VPID, Fmt, URLForm-[MediaSet, Fmt, VPID]) :-
   URLForm = 'http://open.live.bbc.co.uk/mediaselector/6/select/version/2.0/mediaset/~s/format/~s/vpid/~s',
   mediaset_type(_, MediaSet), mediaset_format(Fmt).

mediaset_format(F) :- member(F, [json, xml, pls]).
mediaset_type(aod, MS) :- member(MS, ['pc', 'audio-syndication', 'audio-syndication-dash', 'apple-ipad-hls', 'iptv-all']).
mediaset_type(live_only, MS) :- member(MS, ['apple-icy-mp3a', 'http-icy-aac-lc-a']).

% --- programme details and track list --------------------------

player_page(PID, html, 'https://www.bbc.co.uk/sounds/play/~s'-[PID]).

:- volatile_memo pid_tracks(+atom, -any).
pid_tracks(PID, Tracks) :- insist(uget(player_page(PID), [DOM])), insist(dom_tracks(DOM, Tracks)).
dom_tracks(DOM, Tracks) :-
   xpath(DOM, body/div(@id='orb-modules')//script(content), [JSExpr]),
   once(sub_string(JSExpr, I, 1, _, "{")),
   sub_string(JSExpr, I, _, 2, JSONExpr),
   atom_json_dict(JSONExpr, JSONData, []),
   Tracks = JSONData.get(tracklist).get(tracks).


% --- term display -----
user:portray(ts(Timestamp)) :- format_time(user_output, '<%FT%T%z>', Timestamp).
user:portray(entry(Props)) :-
   maplist(entry_prop(entry(Props)), [pid(PID), title(Title)]),
   format('<~w|~s>', [PID, Title]).
