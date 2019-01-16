:- module(swimpd, [in/2, start_mpd/2, mpd_interactor/0, mpd_init/0, start_gst_thread/1]).

:- use_module(library(socket)).
:- use_module(library(listutils)).
:- use_module(library(callutils), [true2/2, bt_call/2]).
:- use_module(library(insist), [insist/1]).
:- use_module(library(xpath)).
:- use_module(library(data/pair)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).
:- use_module(library(dcg_pair)).
:- use_module(library(snobol)).
:- use_module(library(memo)).
:- use_module(bbc(bbc_tools), [in/2, enum/2, log_and_succeed/1]).
:- use_module(bbc(bbc_db)).
:- use_module(state,    [set_state/2, upd_state/2, state/2]).
:- use_module(protocol, [mpd_interactor/0, notify_all/1]).
:- use_module(telnetd,  [telnet_server/3]).
:- use_module(asyncu,   [thread/2, registered/2, spawn/1, setup_stream/2]).
:- use_module(gst,      [gst_audio_info/3, enact_player_change/3, gst/2, start_gst_thread/1, set_volume/1]).
:- use_module(tools,    [quoted//1, quoted//2, select_nth/4, (+)//1, parse_head//2, nat//1, decimal//0,
                         report//1, report//2, num//1, atom//1, maybe//2, maybe/2, fmaybe/3, fjust/3]).


/* <module> MPD server for BBC radio programmes.

   @todo
   rethink top level control flow
   unify command arg parsing
   randomised play sequence
   seek, CLP approach?
   go to next when finished playing and delete stored position (check gstreamer messages?)
   allow different alsa device (audio-sink, device)
   add arbitrary PID (works for TV too!)
   state persistence
   add 'shelf' for keeping programmes
   Clients: fix for MPDroid
   fix failed status after play (gst player has no caps yet?)
   swap client thread roles while idle
   get format once on play; get bitrate notifications instead of polling
 */

:- dynamic queue/2.

%! mpd_init is det.
%  Set state of MPD to an empty queue with version 0, volume set to 50%, and start
%  and db update times to now.
mpd_init :-
   get_time(Now),
   maplist(set_state, [start_time, dbtime, volume, queue], [Now, Now, 50, 0-([]-nothing)]),
   retractall(queue(_,_)), assert(queue(0, [])).

%! start_mpd(Port, Options) is det.
%  Start server as a detached thread with alias =|mpd_server|=.
%  See mpd_server/2 for description of parameters.
start_mpd(Port, Options) :- thread_create(telnet_server(mpd_interactor, Port, Options), _, [detached(true), alias(mpd_server)]).

longname_service(LongName, S) :- service(S, _, LongName), (service_schedule(S, _) -> true; update_service(S)).
update_service(S) :- get_time(Now), log_and_succeed(time_service_schedule(Now, S, _)), set_state(dbtime, Now).

:- volatile_memo pid_id(+atom, -integer).
pid_id(_, Id) :- flag(songid, Id, Id+1).

clear --> \< trans(Q, ([]-nothing)), \> player_if_queue_playing(Q).
player_if_queue_playing(_-PS) --> maybe(player_if_playstate_playing, PS).
player_if_playstate_playing(_) --> [player].

delete_range(nothing, Id) --> \< get(_-just(ps(Pos, _))), delete(Pos, Id).
delete_range(just(M:N), Ids) --> {numlist(M, N, Is), reverse(Is, [_|Js])}, foldl(delete, Js, Ids).
delete_id(Id, Ps) --> delete(Pos, Id) -> {Ps = [Pos|Ps2]}, delete_id(Id, Ps2); {Ps=[]}.
delete(Pos, Id) -->
   \< get(Songs-PS), {nth0(Pos, Songs, song(Id,_,_))},
   (\< {PS = just(ps(Pos, _))} -> step(keep, next) <\> [player]; []),
   \< (select_nth(Pos, _) <\> fmaybe(update_pos(Pos))).

update_pos(Pos, ps(PPos1, Slave), ps(PPos2, Slave)) :-
   (PPos1 < Pos -> PPos1 = PPos2; PPos1 >= Pos, succ(PPos2, PPos1)).

addid([LongName], nothing) -->
   {longname_service(LongName, S), findall(E, service_entry(S, E), Entries)},
   ffst(foldl(add(LongName), _Ids, Entries)).

addid([LongName, PID], just(Id)) -->
   {longname_service(LongName, S), service_entry(S, E), prop(E, pid(PID))},
   ffst(add(LongName, Id, E)).

addid(['Live Radio'], nothing) --> {all_services(Services)}, ffst(foldl(add_live, Services, _)).
addid(['Live Radio', LongName], just(Id)) --> {service(S, _, LongName)}, ffst(add_live(S-LongName, Id)).

add_live(S-SLN, Id) --> {live_service_tags(S-SLN, Tags), pid_id(S, Id)}, add(song(Id, live(S), Tags)).
add(ServiceName, Id, E) --> {entry_tags(ServiceName, E, Id, Tags)}, add(song(Id, E, Tags)).
add(S, Songs1, Songs2) :- append(Songs1, [S], Songs2).

pause(X, ps(C, just(P1 - Au)), ps(C, just(P2 - Au))) :- pausex(X, P1, P2).
pausex(just(1), _, pause).
pausex(just(0), _, play).
pausex(nothing, pause, play).
pausex(nothing, play, pause).

seekcur(rel(DPos)) --> {gst:send(fmt("seekrel ~f", [DPos]))}. % FIXME: No!!
seekcur(abs(PPos)) --> {gst:send(fmt("seek ~f", [PPos]))}. % FIXME: No!!
seek_pos_id(Pos, Id, PPos) --> current(Pos, Id), {gst:send(fmt("seek ~f", [PPos]))}. % FIXME: No!!
current(Pos, Id) --> get(Songs-just(ps(Pos, _))), {nth0(Pos, Songs, song(Id, _, _))}.

get_audio_info(Au1, Au2) :- gst(Id, _), !, gst_audio_info(Id, Au1, Au2).
get_audio_info(Au, Au).

stop(Songs-just(ps(Pos, _)), Songs-just(ps(Pos, nothing))).
step(Op, Dir) --> get(Songs-just(ps(Pos, _))), ({step(Dir, Songs, Pos, Pos1)} -> play(Op, Pos1, _); \> set(nothing)).
step(next, L, Pos, Pos1) :- succ(Pos, Pos1), length(L, N), Pos1 < N.
step(prev, _, Pos, Pos1) :- succ(Pos1, Pos).

play(nothing) --> get(_-just(ps(Pos, _))), !, play(Pos, _).
play(nothing) --> play(0, _).
play(just(Pos)) --> play(Pos, _).

play(Pos, Id) --> play(play, Pos, Id).
play(Op, Pos, Id, Songs-PS1, Songs-PS2) :-
   nth0(Pos, Songs, song(Id, E, _)),
   update_play_state(Op, Pos, E, PS1, PS2).

update_play_state(play, Pos, E, _, just(ps(Pos, just(play-Au)))) :- entry_audio(E, Au).
update_play_state(keep, Pos, E, just(ps(_, Sl1)), just(ps(Pos, Sl2))) :- fmaybe(update_slave(E), Sl1, Sl2).
update_slave(E, P-_, P-Au) :- entry_audio(E, Au).
entry_audio(live(_), au(0.0, 0.0, 320, 48000:f:2)) :- !.
entry_audio(E, au(Dur, 0.0, 320, 48000:f:2)) :- prop(E, duration(Dur)).

enact(volume, [], _, _) :- !.
enact(volume, [mixer], _, V) :- !, set_volume(V).
enact(queue, Changes) --> {member(player, Changes)} -> \> enact_queue_change; true2.
enact_queue_change(Songs1-PS1, Songs2-PS2) :- enact_player_change(Songs1-Songs2, PS1, PS2).

gst:spec_url(live(S), URL) :- !, service_live_url(S, URL).
gst:spec_url(E, URL) :- entry_xurl(redir(dash), E, _-URL).
gst:notify_eos :- updating_play_state(stop).

mpd_protocol:notify(Id, Subsystem) :- thread_send_message(Id, changed(Subsystem)).

% --- command implementations -----
:- meta_predicate upd_and_notify(+, //).
upd_and_notify(K, P) :- upd_state(K, upd_and_enact(K, P, Changes)), sort(Changes, Changed), notify_all(Changed).
upd_and_enact(K, P, Changes, S1, S2) :- call_dcg(P, S1-Changes, S2-[]), enact(K, Changes, S1, S2).

reading_state(K, Action) --> {state(K, S)}, call(Action, S).
reading_queue(Action, _-Q) --> call(Action, Q).
updating_play_state(Action) :- upd_and_notify(queue, (\< fsnd(Action), \> [player])).
updating_queue_state(Action) :-
   upd_and_notify(queue, (fqueue(Action,V,Songs), \> [playlist])),
   assert(queue(V,Songs)).
fqueue(P, V2, Songs, (V1-Q1)-C1, (V2-Q2)-C2) :- call(P, Q1-C1, Q2-C2), succ(V1, V2), Q2 = Songs-_.

:- op(1200, xfx, :->).
:- discontiguous command/1.
term_expansion(command(H,T) :-> Body, [Rule, command(H)]) :- dcg_translate_rule(mpd_protocol:command(H,T) --> Body, Rule).

command(commands, []) :-> {findall(C, command(C), Commands)}, foldl(report(command), [close, idle|Commands]).
command(setvol, Tail) :-> {phrase(quoted(num(V)), Tail), upd_and_notify(volume, (\< set(V), \> [mixer]))}.
command(clear, [])    :-> {updating_queue_state(clear)}.
command(add, Tail)    :-> {phrase(quoted(path(Path)), Tail), updating_queue_state(\< addid(Path, _))}.
command(addid, Tail)  :-> {phrase(quoted(path(Path)), Tail), updating_queue_state(\< addid(Path, just(Id)))}, report('Id'-Id).
command(delete, Tail) :-> {phrase(maybe(quoted(range), R), Tail), updating_queue_state(delete_range(R, _))}.
command(deleteid, Tail) :-> {phrase(quoted(num(Id)), Tail), updating_queue_state(delete_id(Id, _))}.
command(playid, Tail) :-> {phrase(quoted(num(Id)), Tail), updating_play_state(play(_, Id))}.
command(play, Tail)   :-> {phrase(maybe(quoted(num), N), Tail), updating_play_state(play(N))}.
command(stop, [])     :-> {updating_play_state(stop)}.
command(previous, []) :-> {updating_play_state(step(play, prev))}.
command(next, [])     :-> {updating_play_state(step(play, next))}.
command(pause, Tail)  :-> {phrase(maybe(quoted(num), X), Tail), updating_play_state(fsnd(fjust(pause(X))))}.
command(seek, Tail)   :-> {phrase((quoted(num(Pos)), " ", quoted(num(PPos))), Tail), updating_play_state(seek_pos_id(Pos, _, PPos))}.
command(seekid, Tail) :-> {phrase((quoted(num(Id)), " ", quoted(num(PPos))), Tail), updating_play_state(seek_pos_id(_, Id, PPos))}.
command(seekcur, Tail) :-> {phrase(quoted(seek_spec(Spec)), Tail), updating_play_state(seekcur(Spec))}.
command(update, Tail) :-> {phrase(maybe_quoted_path(Path), Tail)}, update(Path).
command(lsinfo, Tail) :-> {phrase(maybe_quoted_path(Path), Tail)}, lsinfo(Path).
command(stats, [])    :->
   {uptime(T), state(dbtime, D), thread_self(Id), thread_statistics(Id, Stats),
    print_term(Stats, [output(user_error)])},
   foldl(report, [artists-1, albums-10, songs-90, uptime-T, db_update-D]).
command(outputs, [])  :-> foldl(report, [outputid-0, outputname-'Default output', outputenabled-1]).
command(status, [])   :-> reading_state(volume, report(volume)), reading_state(queue, report_status).
command(playlistinfo, Tail) :-> {phrase(maybe(quoted(range), R), Tail)}, reading_state(queue, reading_queue(playlistinfo(R))).
command(playlistid, [])  :-> reading_state(queue, reading_queue(playlistinfo(nothing))).
command(plchanges, Tail) :-> {phrase(quoted(num(V)), Tail)}, reading_state(queue, reading_queue(plchanges(V))).
command(currentsong, []) :-> reading_state(queue, reading_queue(currentsong)).
command(listplaylists, _) :-> [].
command(list, _)      :-> [].
command(decoders, []) :-> [].
command(ping, [])     :-> [].

update(Path) --> {flag(update, JOB, JOB+1), spawn(update_and_notify(Path))}, report(updating_db-JOB).
update_and_notify(Path) :- update(Path), notify_all([database]).

update([]) :- forall(service(S), update_service(S)).
update([ServiceName]) :- service(S, _, ServiceName), update_service(S).

all_services(Services) :- findall(S-SLN, service(S, _, SLN), Services).
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

playlistinfo(R, Songs-_) --> {enum(Songs, NS), subrange(R, NS, NS2)}, foldl(report_song_info, NS2).
subrange(just(N:M), L, Sel) :- length(Pre, N), length(PreSel, M), append(PreSel, _, L), append(Pre, Sel, PreSel).
subrange(nothing, L, L).

plchanges(V, Songs-_) --> {queue(V, OldSongs), enum(Songs, NSongs)}, report_changes(OldSongs, NSongs).
report_changes(_, []) --> !.
report_changes([], NSongs) --> foldl(report_song_info, NSongs).
report_changes([Old|Olds], [N-New|News]) -->
   ({Old=New} -> []; report_song_info(N-New)),
   report_changes(Olds, News).

currentsong(Songs-PS) --> maybe(currentsong(Songs), PS).
currentsong(Songs, ps(Pos, _)) --> {nth0(Pos, Songs, Song)}, report_song_info(Pos-Song).
report_song_info(Pos-song(Id, _, Tags)) --> foldl(report, Tags), foldl(report, ['Pos'-Pos, 'Id'-Id]).
hack(duration-Dur, duration-Dur1) :- !, Dur1 is float(Dur).
hack(T,T).

service_dir(S-Name) -->
   report(directory-Name),
   (  {service_schedule(S, Sched)}
   -> {xpath(Sched, /self(@updated), Updated)},
      report('Last-Modified'-Updated)
   ;  []
   ).

programme(ServiceName, E) -->
	{ insist(entry_tags(ServiceName, E, Id, Tags))},
	foldl(report, Tags), report('Id'-Id).

live_service_tags(_-SLN, [file-File, 'Title'-SLN]) :- path_file(['Live Radio', SLN], File).
entry_tags(ServiceName, E, Id, Tags) :- entry_tags(ServiceName, E, Id, Tags, []).
entry_tags(ServiceName, E, Id) -->
	[file-File], {prop(E, pid(PID)), pid_id(PID, Id), path_file([ServiceName, PID], File)},
   tag(title_and_maybe_album, E), foldl(maybe, [tag(broadcast, E), tag(availability, E)]),
	['Comment'-Syn, duration-Dur], {maplist(prop(E), [synopsis(Syn), duration(Dur)])}.

tag(broadcast, E)    --> {prop(E, broadcast(B)), interval_times(B,T,_), ts_string(T,Broadcast)}, ['Broadcast'-Broadcast].
tag(availability, E) --> {prop(E, availability(A)), interval_times(A,_,T), ts_string(T,Until)}, ['AvailableUntil'-Until].
tag(title_and_maybe_album, E) -->
   {prop(E, title(FullTitle)), entry_maybe_parent(E, Parent)},
   {maybe(cut_parent, Parent,  FullTitle, Title)}, ['Title'-Title],
   maybe(parent_as_album, Parent).

parent_as_album(_-Name) --> ['Album'-Name].
cut_parent(_-Name) --> maybe((str_cut(Name), str_cut(": "))).
str_cut(Pre, String, Suff) :- string_concat(Pre, Suff, String).
ts_string(T, S) :- format_time(string(S), '%c', T). % x for data only

report_status((Ver-(Songs-PS))) -->
   {length(Songs, Len)},
   foldl(report, [repeat-0, random-0, single-0, consume-0, playlist-Ver, playlistlength-Len]),
   report_play_state(PS, Songs).

report_play_state(nothing, _) --> report(state-stop).
report_play_state(just(ps(Pos,Slave)), Songs) -->
   report_nth_song(Songs, song, songid, Pos), {succ(Pos, Pos1)},
   opt(report_nth_song(Songs, nextsong, nextsongid, Pos1)),
   report_slave_state(Slave).

report_nth_song(Songs, K1, K2, Pos) -->
   {nth0(Pos, Songs, song(Id, _, _))},
   foldl(report, [K1-Pos, K2-Id]).
report_slave_state(nothing) --> report(state-stop).
report_slave_state(just(State-Au)) -->
   {get_audio_info(Au, au(Dur, Elap, BR, Fmt)), format(string(Time), '~0f:~0f', [Elap, Dur])},
   foldl(report, [state-State, time-Time, elapsed-Elap, duration-Dur, bitrate-BR, audio-Fmt]).

uptime(T) :- get_time(Now), state(start_time, Then), T is Now - Then.

% --- command and reply DCGs -----
range(N:M) --> nat(N), (":", nat(M); {succ(N,M)}).
seek_spec(abs(T)) --> decimal // num(T).
seek_spec(rel(T)) --> (any(`+-`), decimal) // num(T).
seek_dir(M) --> "+", {M=fwd}; "-", {M=bwd}.
maybe_quoted_path(Path) --> {Path=[]}; quoted(path(Path)).
path(Path) --> seqmap_with_sep("/", path_component, Path).
path([]) --> [].
path_component(Dir) --> +(notany(`/`)) // atom(Dir).
path_file(Path, File) :- phrase_string(seqmap_with_sep("/", at, Path), File). % FIXME: generation

