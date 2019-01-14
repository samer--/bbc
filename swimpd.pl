:- module(swimpd, [ in/2, start_mpd/2, mpd_server/2, mpd_interactor/0, mpd_init/0 ]).

:- use_module(library(socket)).
:- use_module(library(listutils)).
:- use_module(library(callutils), [true2/2, bt_call/2]).
:- use_module(library(insist), [insist/1]).
:- use_module(library(xpath)).
:- use_module(library(dcg/basics), [string//1, string_without//2]).
:- use_module(library(data/pair)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).
:- use_module(library(dcg_pair)).
:- use_module(library(snobol)).
:- use_module(library(memo)).
:- use_module(bbc_tools, [in/2, enum/2, log_and_succeed/1]).
:- use_module(bbc_db).

/* <module> MPD server for BBC radio programmes.

   @todo
   rethink top level control flow
   unify command arg parsing
   randomised play sequence
   seek, CLP approach?
   go to next when finished playing and delete stored position (check gstreamer messages?)
   allow different alsa device (audio-sink, device)
 */

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
start_mpd(Port, Options) :- thread_create(mpd_server(Port, Options), _, [detached(true), alias(mpd_server)]).

longname_service(LongName, S) :- service(S, _, LongName), (service_schedule(S, _) -> true; update_service(S)).
update_service(S) :- get_time(Now), log_and_succeed(time_service_schedule(Now, S, _)), set_state(dbtime, Now).

% ------------------------------ State management ------------------------------
% state = pair(pair(integer, pair(list(song), maybe(play_state))), dict).
% play_state ---> ps(natural, maybe(pair(pause_state, au_state))).
% au_state   ---> au(duration, elapsed, bitrate, format).
% pause_state ---> play; pause.
:- dynamic state/2, thread/2, queue/2.
set_state(Key, Val) :- retractall(state(Key, _)), assert(state(Key, Val)).

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

get_audio_info(au(Dur, _Elap, _BR, _Fmt), au(FDur, Elap, BR, Fmt)) :- gst(Id, _), !,
   maplist(send, ["bitrate", "format", "position"]), FDur is float(Dur),
   thread_get_message(Id, bitrate(BR), [timeout(1)]),
   thread_get_message(Id, format(Fmt), [timeout(1)]),
   thread_get_message(Id, position(Elap), [timeout(1)]).
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
enact(volume, [mixer], _, V) :- !, FV is V/100.0, send(fmt('volume ~5f', [FV])).
enact(queue, Changes) --> {member(player, Changes)} -> \> enact_queue_change; true2.
enact_queue_change(Songs1-PS1, Songs2-PS2) :- enact_player_change(Songs1-Songs2, PS1, PS2).
enact_player_change(_, nothing, nothing).
enact_player_change(Songs-_, just(ps(Pos, Slave)), nothing) :- maybe(stop_if_playing(Songs-Pos), Slave), send("close").
enact_player_change(_-Songs, nothing, just(ps(Pos, Slave))) :- maybe(cue_and_maybe_play(Songs, Pos), Slave).
enact_player_change(SongsPair, just(PS1), just(PS2)) :- enact_ps_change(SongsPair, PS1, PS2).

enact_ps_change(Songs1-Songs2, ps(Pos1, Sl1), ps(Pos2, Sl2)) :-
   nth0(Pos1, Songs1, song(Id1, _, _)),
   nth0(Pos2, Songs2, song(Id2, _, _)),
   (  Id1 \= Id2
   -> maybe(stop_if_playing(Songs1-Pos1), Sl1), send("close"),
      maybe(cue_and_maybe_play(Songs2, Pos2), Sl2)
   ;  enact_slave_change((Songs1-Pos1)-(Songs2-Pos2), Sl1, Sl2)
   ).
enact_slave_change(_,          nothing, nothing) :- !.
enact_slave_change(SongsPos-_, just(_), nothing) :- !, save_position(SongsPos), send("stop").
enact_slave_change(_-SongsPos, nothing, just(S-Au)) :- !, send("pause"), restore_position(SongsPos), maybe_play(S, Au).
enact_slave_change(_,          just(S1-_Au1), just(S2-_Au2)) :-
   (  S1-S2 = play-pause -> send("pause")
   ;  S1-S2 = pause-play -> send("resume")
   ;  true
   ).
maybe_play(P, _) :- P=play -> send("resume"); true.
stop_if_playing(SongsPos, _) :- save_position(SongsPos).
cue_and_maybe_play(Songs, Pos, P-Au) :-
   nth0(Pos, Songs, song(_, E, _)), spec_url(E, URL),
   send(fmt('uri ~s', [URL])),
   restore_position(Songs-Pos),
   maybe_play(P, Au).

save_position(Songs-Pos) :-
   send("position"), recv(position(PPos)),
   nth0(Pos, Songs, song(Id, _, _)), set_state(position(Id), PPos).
restore_position(Songs-Pos) :-
   nth0(Pos, Songs, song(Id, _, _)),
   (state(position(Id), PPos) -> send(("seek ", num(PPos))); true).


spec_url(live(S), URL) :- !, service_live_url(S, URL).
spec_url(E, URL) :- entry_xurl(redir(dash), E, _-URL).

with_gst(P, Status) :-
   setup_call_cleanup(start_gst(PID, IO),
                      catch(call(P, PID-IO), Ex, (process_kill(PID), throw(Ex))),
                      process_wait(PID, Status)).

start_gst(PID,In-Out) :-
   process_create('python/gst12.py', [], [stdin(pipe(In)), stdout(pipe(Out)), stderr(std), process(PID)]),
   maplist(setup_stream([close_on_abort(false), buffer(line)]), [In, Out]).

:- dynamic gst/2.
gst_reader_thread(_-(In-Out)) :-
   thread_self(Self),
   setup_call_cleanup(assert(gst(Self,In)), gst_reader(Self, Out), retract(gst(Self,In))).

gst_reader(Self, Out) :- state(volume, V), enact(volume, [mixer], _, V), gst_read_next(Self, Out).
gst_read_next(Self, Out) :- read_line_to_codes(Out, Codes), gst_handle(Codes, Self, Out).
gst_handle(end_of_file, _, _) :- debug(mpd(gst), 'End of stream from gst', []).
gst_handle(Codes, Self, Out) :-
   debug(mpd(gst), '~~> ~s', [Codes]),
   insist(phrase(parse_head(Head, Tail), Codes)),
   (phrase(gst_message(Head, Msg), Tail) -> thread_send_message(Self, Msg); true),
   gst_read_next(Self, Out).

gst_message(bitrate, bitrate(BR)) --> num(BR).
gst_message(position, position(BR)) --> num(BR).
gst_message(duration, duration(D)) --> num(D).
gst_message(format, format(Rate:Fmt:Ch)) --> split_on_colon([nat(Rate), sample_fmt(Fmt), nat(Ch)]).
sample_fmt(f) --> "F", !, arb.
sample_fmt(N) --> [_], nat(N), ([]; any(`LB_`), arb).

gst_volume(V) :- send(fmt('volume ~f',V)).
gst_uri(URI) :- send(fmt('uri ~s',[URI])).
send(P) :- gst(_,In), phrase(P, Codes), format(In, '~s\n', [Codes]).
recv(M) :- gst(Id,_), thread_get_message(Id, M, [timeout(2)]).

start_gst_thread :- spawn(with_gst(gst_reader_thread, _)).
% ------------------------ client interactor --------------------

%! mpd_interactor is det.
% Run MPD client interaction using the current user_input and user_output Prolog streams.
mpd_interactor :- registered(client, wait_for_input).
wait_for_input :- read_command(Cmd), handle(Cmd).

read_command(Cmd) :-
   read_line_to_codes(user_input, Cmd),
   debug(mpd(command), ">> ~s", [Cmd]).

handle(end_of_file) :- !.
handle(`close`) :- !.
handle(Cmd) :- catch(handle1(Cmd), exec(Cmd2), handle(Cmd2)).

handle1(Cmd) :-
   insist(parse_head(Head, Tail, Cmd, [])),
   insist(catch((execute(Head, Tail), Reply=ok), mpd_ack(Ack), Reply=Ack)),
   reply(Reply), wait_for_input.

execute(command_list_ok_begin, []) :- !,  command_list(list_ok).
execute(command_list_begin, []) :- !, command_list(silent).
execute(noidle, []) :- !, read_command(Cmd), throw(exec(Cmd)).
execute(idle, Tail) :- !,
   phrase(idle_filter(Filter), Tail),
   thread_self(Self),
   stream_property(Out, alias(user_output)),
   setup_call_cleanup(thread_create(with_output_to(Out, listener(Self-Filter, [])), Id, []),
                      read_command(Cmd), cleanup_listener(Cmd, Self,Id, NextCmd)),
   throw(exec(NextCmd)).
execute(Cmd, T) :- execute1(0-'', Cmd, T).

execute1(Ref, Cmd, T) :-
   (  catch(reply_phrase(command(Cmd, T)), mpd_err(Err), throw(mpd_ack(ack(Ref, Err)))) -> true
   ;  throw(mpd_ack(ack(Ref, err(99, 'Failed on ~s', [Cmd]))))
   ).

reply_phrase(P) :-
   phrase(P, Codes), format('~s', [Codes]),
   debug(mpd(reply), '<< ~s|', [Codes]).

command_list(Reply) :- accum(Commands, []), execute_list(Reply, Commands, 0).
execute_list(_, [], _).
execute_list(Reply, [Cmd|Cmds], Pos) :-
   parse_head(Head, Tail, Cmd, []),
   execute1(Pos-Head, Head, Tail),
   sub_reply(Reply), succ(Pos, Pos1),
   execute_list(Reply, Cmds, Pos1).

sub_reply(silent).
sub_reply(list_ok) :- fmt_reply('list_OK', []).

reply(ok) :- fmt_reply('OK', []).
reply(ack(Pos-SubCmd, err(Code, Fmt, Args))) :-
   fmt_reply('ACK [~d@~d] {~s} ~@', [Code, Pos, SubCmd, format(Fmt, Args)]).

fmt_reply(Fmt, Args) :-
   format(string(R), Fmt, Args),
   debug(mpd(command), '<< ~s', [R]),
   format('~s\n', [R]).

accum --> {read_command(Cmd)}, accum_cont(Cmd).
accum_cont(`command_list_begin`) --> {throw(protocol_fail)}.
accum_cont(`command_list_ok_begin`) --> {throw(protocol_fail)}.
accum_cont(`command_list_end`) --> !, [].
accum_cont(Cmd) --> [Cmd], accum.

% -- notification system ---
listener(Id-Filter, ToPutBack) :-
   thread_get_message(Id, Msg),
   debug(mpd(notify), 'Listener head got message ~w.', [Msg]),
   message_queue_property(Id, size(N)), length(Msgs, N),
   maplist(thread_get_message(Id), Msgs),
   debug(mpd(notify), 'Listener processing ~w...', [[Msg|Msgs]]),
   listener_msg(Msg, Msgs, Id-Filter, []-ToPutBack).

listener_msg(broken, _, _, _) :- throw(broken).
listener_msg(cmd(end_of_file), _, _, _) :- throw(cmd(end_of_file)).
listener_msg(cmd(`noidle`), Msgs, Id-_, ToReport-ToPutBack) :-
   reply_phrase(foldl(report(changed), ToReport)), reply(ok),
   maplist(thread_send_message(Id), Msgs),
   maplist(notify(Id), ToPutBack),
   throw(cmd(`noidle`)).

listener_msg(changed(S), Msgs, E, ToReport-ToPutBack) :-
   (  E = _-Filter, call(Filter, S)
   -> listener_msgs(Msgs, E, [S|ToReport]-ToPutBack)
   ;  listener_msgs(Msgs, E, ToReport-[S|ToPutBack])
   ).

listener_msgs([Msg|Msgs], E, S) :- listener_msg(Msg, Msgs, E, S).
listener_msgs([], E, []-ToPutBack) :- !, listener(E, ToPutBack).
listener_msgs([], Id-_, ToReport-ToPutBack) :-
   debug(mpd(notify), 'Listener reporting changes in ~w...', [ToReport]),
   reply_phrase(foldl(report(changed), ToReport)), reply(ok),
   listener_tail_wait(Id, ToPutBack).

listener_tail_wait(Id, ToPutBack) :-
   debug(mpd(notify), 'Listener tail waiting...', []),
   thread_get_message(Id, Msg),
   listener_tail_msg(Msg, Id, ToPutBack).

listener_tail_msg(broken, _, _) :- throw(broken).
listener_tail_msg(changed(S), Id, ToPutBack) :- listener_tail_wait(Id, [S|ToPutBack]).
listener_tail_msg(cmd(Cmd), Id, ToPutBack) :- maplist(notify(Id), ToPutBack), throw(cmd(Cmd)).

cleanup_listener(Cmd, Self, Id, NextCommand) :-
   (var(Cmd) -> Msg = broken; Msg = cmd(Cmd)),
   thread_send_message(Self, Msg),
   thread_join(Id, Status),
   debug(mpd(notify), 'Listener exited with ~w', [Status]),
   insist(Status = exception(cmd(NextCommand))).

notify_all(Subsystems) :- forall(thread(client, Id), maplist(notify(Id), Subsystems)).
notify(Id, Subsystem) :- thread_send_message(Id, changed(Subsystem)).

% --- command and reply DCGs -----
parse_head(Head, Tail) --> string_without(` `, H), tail(Tail), {atom_codes(Head, H)}.
tail([]) --> [].
tail(Tail) --> " ", string(Tail).

range(N:M) --> nat(N), (":", nat(M); {succ(N,M)}).

maybe_quoted_path(Path) --> {Path=[]}; quoted(path(Path)).
path(Path) --> seqmap_with_sep("/", path_component, Path).
path([]) --> [].
path_component(Dir) --> +(notany(`/`)) // atom(Dir).
path_file(Path, File) :- phrase_string(seqmap_with_sep("/", at, Path), File). % FIXME: generation

idle_filter(nonvar) --> [].
idle_filter(in(Subsystems)) --> seqmap_with_sep(" ", quoted(atom), Subsystems).

report(Name-Value) --> report(Name, Value).
report(Name, Value) --> fmt('~w: ~w\n', [Name, Value]).

% --- command implementations -----
:- meta_predicate upd_and_notify(+, //).
upd_and_notify(K, P) :- with_mutex(swimpd, with_state(K, upd_and_enact(K, P, Changes))), sort(Changes, Changed), notify_all(Changed).
upd_and_enact(K, P, Changes, S1, S2) :- call_dcg(P, S1-Changes, S2-[]), enact(K, Changes, S1, S2).
with_state(K, P) :- state(K, S1), call(P, S1, S2), set_state(K, S2).

reading_state(K, Action) --> {state(K, S)}, call(Action, S).
reading_queue(Action, _-Q) --> call(Action, Q).
updating_play_state(Action) :- upd_and_notify(queue, (\< fsnd(Action), \> [player])).
updating_queue_state(Action) :-
   upd_and_notify(queue, (fqueue(Action,V,Songs), \> [playlist])),
   assert(queue(V,Songs)).
fqueue(P, V2, Songs, (V1-Q1)-C1, (V2-Q2)-C2) :- call(P, Q1-C1, Q2-C2), succ(V1, V2), Q2 = Songs-_.

:- op(1200, xfx, :->).
:- discontiguous command/1, command/4.
term_expansion(command(H,T) :-> Body, [Rule, command(H)]) :- dcg_translate_rule(command(H,T) --> Body, Rule).

command(commands, []) :-> {findall(C, command(C), Commands)}, foldl(report(command), [close, idle|Commands]).
command(ping, [])     :-> [].
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
command(update, Tail) :-> {phrase(maybe_quoted_path(Path), Tail)}, update(Path).
command(lsinfo, Tail) :-> {phrase(maybe_quoted_path(Path), Tail)}, lsinfo(Path).
command(list, _)      :-> "Music\nSpoken\nNews\n".
command(listplaylists, _) :-> [].
command(stats, [])    :->
   {uptime(T), state(dbtime, D), thread_self(Id), thread_statistics(Id, Stats),
    with_output_to(user_error, print_term(Stats, []))},
   foldl(report, [artists-1, albums-10, songs-90, uptime-T, db_update-D]).
command(outputs, [])  :-> foldl(report, [outputid-0, outputname-'Default output', outputenabled-1]).
command(status, [])   :-> reading_state(volume, report(volume)), reading_state(queue, report_status).
command(playlistinfo, Tail) :-> {phrase(maybe(quoted(range), R), Tail)}, reading_state(queue, reading_queue(playlistinfo(R))).
command(playlistid, [])  :-> reading_state(queue, reading_queue(playlistinfo(nothing))).
command(plchanges, Tail) :-> {phrase(quoted(num(V)), Tail)}, reading_state(queue, reading_queue(plchanges(V))).
command(currentsong, []) :-> reading_state(queue, reading_queue(currentsong)).

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
% ------------------------------ Server framework ------------------------------

%!  mpd_server(+Port, +Options) is det.
%
%   Currently defined options are:
%
%           * allow(pred(ip))
%           Allow access from addresses that satisfy unary predicate over ip, where
%           =|ip ---> ip(integer, integer, integer, integer)|=.
%           Default is =|=(ip(127,0,0,1]))|= (i.e. localhost only).
mpd_server(Port, Options) :-
   option(allow(Allow), Options, =(ip(127,0,0,1))),
   setup_call_cleanup(tcp_socket(Socket),
                      socket_server(Socket, Port, Allow),
                      tcp_close_socket(Socket)).

socket_server(Socket, Port, Allow) :-
   tcp_setopt(Socket, reuseaddr),
   tcp_bind(Socket, Port),
   tcp_listen(Socket, 5),
   server_loop(Socket, Allow).

server_loop(Socket, Allow) :-
   tcp_accept(Socket, Slave, Peer),
   debug(mpd(connection), "new connection from ~w", [Peer]),
   tcp_open_socket(Slave, In, Out),
   maplist(setup_stream([close_on_abort(false)]), [In, Out]),
   catch(spawn(call_cleanup(client_thread(In, Out, Peer, Allow), (close(In), close(Out)))),
         error(permission_error(create, thread, mpdclient), _), fail), !,
   server_loop(Socket, Allow).

client_thread(In, Out, Peer, Allow) :- call(Allow, Peer), !, service_client(In, Out).
client_thread(_, Out, _, _) :- format(Out, 'Access denied.~n', []).

service_client(In, Out) :-
   set_prolog_IO(In, Out, user_error), prompt(_, ''),
   current_prolog_flag(encoding, Enc),
   maplist(setup_stream([encoding(Enc), newline(posix)]), [user_input, user_output]),
   writeln('OK MPD 0.20.0'),
   mpd_interactor.

% -------------- DCG and other tools --------------------

setup_stream(Props, S) :- maplist(set_stream(S), Props).
registered(N, G) :- thread_self(Id), setup_call_cleanup(assert(thread(N, Id)), G, retract(thread(N, Id))).
spawn(G) :- thread_create(G, _, [detached(true)]).
select_nth(N, X, L1, L2) :- nth0(N, L1, X, L2).
fmaybe(_, nothing, nothing).
fmaybe(P, just(X1), just(X2)) :- call(P, X1, X2).
fjust(P, just(X1), just(X2)) :- call(P, X1, X2).
pphrase(P) :- phrase(P, C), format('~s', [C]).

+(P) --> call_dcg(P), *(P).
*(P) --> []; +(P).

maybe(_, nothing) --> [].
maybe(P, just(Y)) --> call(P,Y).
maybe(_, nothing).
maybe(P, just(X)) :- call(P, X).

digit  --> ctype(digit).
nat(N) --> {ground(N)}, !, num(N) // +digit.
nat(N) --> +digit // num(N).

num(N,S1,S2) :- ground(N), !, format(codes(S1,S2),'~w',[N]).
num(N,S1,S2) :- list(C,S1,S2), number_codes(N,C).
atom(A,S1,S2) :- ground(A), !, format(codes(S1,S2),'~w',[A]).
atom(A,S1,S2) :- list(C,S1,S2), atom_codes(A,C).

split_on_colon(Ps) --> seqmap_with_sep(":", broken(":"), Ps).
broken(Cs, P) --> break(Cs) // P.

quoted(P, X) --> quoted(call(P, X)).
quoted(P) --> "\"", esc(esc_qq, Codes), "\"", {phrase(P, Codes)}.
esc_qq([0'"|Cs],Cs) --> "\\\"".
esc_qq([0'\\|Cs],Cs) --> "\\\\".
esc_qq([C|Cs],Cs) --> [C] // notany(`\\"`).
