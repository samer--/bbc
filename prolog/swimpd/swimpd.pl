:- module(swimpd, [mpd_init/0, restore_state/1]).

:- use_module(library(socket)).
:- use_module(library(data/pair)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).
:- use_module(library(dcg_pair)).
:- use_module(library(snobol)).
:- use_module(library(listutils)).
:- use_module(library(callutils), [true2/2, bt_call/2]).
:- use_module(library(insist), [insist/1]).
:- use_module(bbc(bbc_tools), [enum/2]).
:- use_module(state,    [set_state/2, upd_state/2, state/2, queue/2, set_queue/2]).
:- use_module(protocol, [notify_all/1]).
:- use_module(database, [is_programme/1, id_pid/2, pid_id/2, lsinfo//1, addid//2, update_db/1]).
:- use_module(asyncu,   [thread/2, registered/2, spawn/1, setup_stream/2]).
:- use_module(gst,      [gst_audio_info/3, enact_player_change/3, gst/2, set_volume/1]).
:- use_module(tools,    [quoted//1, quoted//2, select_nth/4, (+)//1, parse_head//2, nat//1, decimal//0,
                         flip/4, report//1, report//2, num//1, atom//1, maybe//2, maybe/2, fmaybe/3, fjust/3]).

/* <module> MPD server for BBC radio programmes.

   @todo
   Core
      rethink top level control flow
      unify command arg parsing
      seek, CLP approach?
      swap client thread roles while idle
      check for trail and stack leaks

   Player
      allow different alsa device (audio-sink, device)
      fix failed status after play (gst player has no caps yet?)
      get format once on play; get bitrate notifications instead of polling
      synchronous seek vs ASYNC_DONE msg

   Control
      randomised play sequence
      auto next as well as single (handle stored position correctly too)

   State management:
      multiple sessions
      persistence
      Avoid further DB lookups
 */

%! mpd_init is det.
%  Set state of MPD to an empty queue with version 0, volume set to 50%, and start
%  and db update times to now.
mpd_init :-
   get_time(Now),
   maplist(set_state, [start_time, dbtime, volume, queue], [Now, Now, 50, 0-([]-nothing)]),
   retractall(queue(_,_)), assert(queue(0, [])).

:- meta_predicate restore_state(2).
restore_state(State) :-
   maplist(State, [volume, queue], [Vol, _-(Songs-_)]),
   forall((member(song(Id, _, _), Songs), call(State, position(Id), PPos)), set_state(position(Id), PPos)),
   upd_and_notify(volume, (\< set(Vol), \> [mixer])),
   updating_queue_state(\< \< flip(append, Songs)).

% --- command implementations -----
:- op(1200, xfx, :->).
:- discontiguous command/1.
term_expansion(command(H,T) :-> Body, [Rule, command(H)]) :- dcg_translate_rule(mpd_protocol:command(H,T) --> Body, Rule).

command(commands, []) :-> {findall(C, command(C), Commands)}, foldl(report(command), [close, idle|Commands]).
command(setvol, Tail) :-> {phrase(a(num(V)), Tail), upd_and_notify(volume, (\< set(V), \> [mixer]))}.
command(clear, [])    :-> {updating_queue_state(clear)}.
command(add, Tail)    :-> {phrase(a(path(Path)), Tail), updating_queue_state(\< addid(Path, _))}.
command(addid, Tail)  :-> {phrase(a(path(Path)), Tail), updating_queue_state(\< addid(Path, just(Id)))}, report('Id'-Id).
command(delete, Tail) :-> {phrase(maybe(a(range), R), Tail), updating_queue_state(delete_range(R, _))}.
command(deleteid, Tail) :-> {phrase(a(pid(Id)), Tail), updating_queue_state(delete_id(Id, _))}.
command(playid, Tail) :-> {phrase(a(pid(Id)), Tail), updating_play_state(play(_, Id))}.
command(play, Tail)   :-> {phrase(maybe(a(nat), N), Tail), updating_play_state(play(N))}.
command(stop, [])     :-> {updating_play_state(stop)}.
command(previous, []) :-> {updating_play_state(step(play, prev))}.
command(next, [])     :-> {updating_play_state(step(play, next))}.
command(pause, Tail)  :-> {phrase(maybe(a(nat), X), Tail), updating_play_state(fsnd(fjust(pause(X))))}.
command(seek, Tail)   :-> {phrase((a(nat(Pos)), " ", a(num(PPos))), Tail), updating_play_state(seek_pos_id(Pos, _, PPos))}.
command(seekid, Tail) :-> {phrase((a(pid(Id)), " ", a(num(PPos))), Tail), updating_play_state(seek_pos_id(_, Id, PPos))}.
command(seekcur, Tail) :-> {phrase(a(seek_spec(Spec)), Tail), updating_play_state(seekcur(Spec))}.
command(tagtypes, []) :-> foldl(report(tagtype), ['Album', 'Title', 'Date', 'Comment', 'AvailableUntil']).
command(update, Tail) :-> {phrase(maybe_quoted_path(Path), Tail)}, update_db(Path).
command(lsinfo, Tail) :-> {phrase(maybe_quoted_path(Path), Tail)}, lsinfo(Path).
command(stats, [])    :-> {uptime(T), state(dbtime, D), round(D,DD)}, foldl(report, [artists-1, uptime-T, db_update-DD]).
command(outputs, [])  :-> foldl(report, [outputid-0, outputname-'Default output', outputenabled-1]).
command(status, [])   :-> reading_state(volume, report(volume)), reading_state(queue, report_status).
command(playlistinfo, Tail) :-> {phrase(maybe(a(range), R), Tail)}, reading_state(queue, reading_queue(playlistinfo(R))).
command(playlistid, [])  :-> reading_state(queue, reading_queue(playlistinfo(nothing))).
command(plchanges, Tail) :-> {phrase(a(nat(V)), Tail)}, reading_state(queue, reading_queue(plchanges(V))).
command(currentsong, []) :-> reading_state(queue, reading_queue(currentsong)).
command(listplaylists, _) :-> [].
command(list, _)      :-> [].
command(decoders, []) :-> [].
command(ping, [])     :-> [].

% -- interaction with state --
upd_and_notify(K, P) :- upd_state(K, upd_and_enact(K, P, Changes)), sort(Changes, Changed), notify_all(Changed).
upd_and_enact(K, P, Changes, S1, S2) :- call_dcg(P, S1-Changes, S2-[]), enact(K, Changes, S1, S2).

updating_play_state(Action) :- upd_and_notify(queue, (\< fsnd(Action), \> [player])).
updating_queue_state(Action) :- upd_and_notify(queue, (fqueue(Action,V,Songs), \> [playlist])), set_queue(V, Songs).
fqueue(P, V2, Songs, (V1-Q1)-C1, (V2-Q2)-C2) :- call(P, Q1-C1, Q2-C2), succ(V1, V2), Q2 = Songs-_.

reading_state(K, Action) --> {state(K, S)}, call(Action, S).
reading_queue(Action, _-Q) --> call(Action, Q).
uptime(T) :- get_time(Now), state(start_time, Then), T is integer(Now - Then).

update_db(Path) --> {flag(update, JOB, JOB+1), spawn(update_and_notify(Path))}, report(updating_db-JOB).
update_and_notify(Path) :- update_db(Path), get_time(Now), set_state(dbtime, Now), notify_all([database]).

enact(volume, [], _, _) :- !.
enact(volume, [mixer], _, V) :- !, set_volume(V).
enact(queue, Changes) --> {member(player, Changes)} -> \> (trans(Ss1, Ss2) <\> enact_player_change(Ss1-Ss2)); true2.

% -- playlist management --
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

report_song_info(Pos-song(PID, _, Tags)) --> {pid_id(PID, Id)}, foldl(report, Tags), foldl(report, ['Pos'-Pos, 'Id'-Id]).

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

% -- player management --
pause(X, ps(C, just(P1 - Prog)), ps(C, just(P2 - Prog))) :- pausex(X, P1, P2).
pausex(just(1), _, pause).
pausex(just(0), _, play).
pausex(nothing, pause, play).
pausex(nothing, play, pause).

seekcur(rel(DPos)) --> {gst:send(fmt("seekrel ~f", [DPos]))}. % FIXME: No!!
seekcur(abs(PPos)) --> {gst:send(fmt("seek ~f", [PPos]))}. % FIXME: No!!
seek_pos_id(Pos, Id, PPos) --> current(Pos, Id), {gst:send(fmt("seek ~f", [PPos]))}. % FIXME: No!!
current(Pos, Id) --> get(Songs-just(ps(Pos, _))), {nth0(Pos, Songs, song(Id, _, _))}.

stop(Songs-just(ps(Pos, _)), Songs-just(ps(Pos, nothing))).
step(Op, Dir) --> get(Songs-just(ps(Pos, _))), ({step(Dir, Songs, Pos, Pos1)} -> play(Op, Pos1, _); \> set(nothing)).
step(next, L, Pos, Pos1) :- succ(Pos, Pos1), length(L, N), Pos1 < N.
step(prev, _, Pos, Pos1) :- succ(Pos1, Pos).

play(nothing) --> get(_-just(ps(Pos, _))), !, play(Pos, _).
play(nothing) --> play(0, _).
play(just(Pos)) --> play(Pos, _).

play(Pos, Id) --> play(play, Pos, Id).
play(Op, Pos, Id, Songs-PS1, Songs-PS2) :-
   nth0(Pos, Songs, song(Id, _, Tags)),
   (member(duration-Dur, Tags) -> true; Dur=0.0),
   update_play_state(Op, Pos, Dur, PS1, PS2).

update_play_state(play, Pos, Dur, _, just(ps(Pos, just(play-0.0/Dur)))).
update_play_state(keep, Pos, I, just(ps(_, Sl1)), just(ps(Pos, Sl2))) :- fmaybe(update_slave(I), Sl1, Sl2).
update_slave(Dur, P-_, P-0.0/Dur).

gst:id_wants_bookmark(PID) :- is_programme(PID).
gst:notify_eos :- updating_play_state(stop).

% -- status --
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
   {nth0(Pos, Songs, song(PID, _, _)), pid_id(PID, Id)},
   foldl(report, [K1-Pos, K2-Id]).
report_slave_state(nothing) --> report(state-stop).
report_slave_state(just(State-Prog)) -->
   {  gst(Id, _), gst_audio_info(Id, Prog, au(Dur2, Elap2, BR, Fmt)) -> true
   ;  Elap2/Dur2=Prog, BR=nothing, Fmt=nothing
   },
   {format(string(Time), '~0f:~0f', [Elap2, Dur2])},
   foldl(report, [state-State, time-Time, elapsed-Elap2, duration-Dur2]),
   foldl(maybe_report, [bitrate-BR, audio-Fmt]).
maybe_report(K-V) --> maybe(report(K), V).

% --- command and reply DCGs -----
a(P) --> quoted(P); break(` "`) // P.
a(P, X) --> quoted(P, X); break(` "`) // call(P, X).
pid(Id) --> nat(N), {id_pid(N, Id)}.
range(N:M) --> nat(N), (":", nat(M); {succ(N,M)}).
seek_spec(abs(T)) --> decimal // num(T).
seek_spec(rel(T)) --> (any(`+-`), decimal) // num(T).
maybe_quoted_path(Path) --> {Path=[]}; quoted(path(Path)).
path(Path) --> seqmap_with_sep("/", path_component, Path).
path([]) --> [].
path_component(Dir) --> +(notany(`/`)) // atom(Dir).
