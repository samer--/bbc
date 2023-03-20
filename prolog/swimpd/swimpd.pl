:- module(swimpd, [mpd_init/0, restore_state/1, save_state/1]).

:- use_module(library(http/http_open)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg_codes), [fmt//2, ctype//1]).
:- use_module(library(data/pair), [fsnd/3]).
:- use_module(library(snobol),    [any//1, notany//1, break//1, arb//0, arbno//1]).
:- use_module(library(insist),    [insist/1]).
:- use_module(library(callutils), [true2/2, (*)/4]).
:- use_module(library(fileutils), [with_output_to_file/2]).
:- use_module(bbc(bbc_tools), [enum/2]).
:- use_module(state,    [excl/1, init_state/2, set_states/2, upd_states/2, state/2, states/2,
                         set_vstate/2, vstate/2, version_queue/2, add_queue/2]).
:- use_module(protocol, [notify_all/1, reply_binary/4]).
:- use_module(database, [is_programme/1, id_pid/2, pid_id/2, pid_tracks/2, lsinfo//1, addid//2, db_update/1,
                         db_count//1, db_find//2, db_find/3, db_list//3, db_image/3, db_stats/1]).
:- use_module(gst,      [gst_audio_info/2, enact_player_change/3, set_volume/1]).
:- use_module(tools,    [quoted//1, quoted//2, select_nth/4, (+)//1, nat//1, decimal//0, fnth/5, flip/4,
                         report//1, report//2, num//1, atom//1, maybe//2, maybe/2, fmaybe/3, fjust/3,
                         registered/2, spawn/1, setup_stream/2]).

/* <module> MPD server for BBC radio programmes.

   @todo
   Core
      seek, CLP approach?
      lightweight threads
      more efficient artist-album-track database view
      review process synch and comms, see eg Erlang approach

   Control
      rewind if playing track where current position is at end
      Better seekable timeline for radio streams
      Stop GST player after some time to release audio device

   State management:
      version_queue/2 -> version tree, undo etc. (plchanges?)
      selective restore - everything vs just queue.

   Protocol:
      clearerror (check error in status?) consume, mutliple group

   Extensions:
      Set up as service under Linux or Mac OS launchd (see how MPD does it)
      Actions on timer: update db, add certain programmes to playlist
      Use tracklist for cool stuff:
      - seek to nth/prev/next track
      - query current track info

   Playlists
      More playlist protocol: listing and loading
 */

%! mpd_init is det.
%  Set state of MPD to an empty queue with version 0, volume set to 50%, and start
%  and db update times to now. States:
%  queue  : pair(integer, list(song))
%  player : maybe(play_state).
%  play_state ---> ps(natural, maybe(pair(pause_state, au_state))).
%  au_state   ---> au(duration, elapsed, bitrate, format).
%  pause_state ---> play; pause.
mpd_init :-
   get_time(Now), flag(update, _, 1),
   maplist(init_state, [volume, queue, player, consume, single], [50, 0-[], nothing, 0, 1]),
   maplist(set_vstate, [start_time, dbtime], [Now, Now]),
   retractall(version_queue(_,_)), assert(version_queue(0, [])).

save_state(Filename) :- with_output_to_file(Filename, excl(listing(mpd_state:state))).

restore_state(Filename) :-
   read_file_to_terms(Filename, Terms, []),
   findall(K-V, member(state(K,V), Terms), Pairs),
   call(ord_list_to_assoc * sort, Pairs, Assoc),
   maplist(flip(get_assoc, Assoc), [consume, single, volume, player, queue],
                                   [Consume, Single, Volume, Player, _-Songs]),
   forall(member(position(Id)-PPos, Pairs), set_states(position(Id), PPos)),
   maplist(upd_and_notify_option, [single-Single, consume-Consume]),
   upd_and_notify(volume, (set(Volume) <\> [mixer])),
   updating_queue_state(set(Songs-Player) <\> [player]).

restore_queue_version(V) :- version_queue(V, Songs), updating_queue_state(set_songs(Songs)).

% --- command implementations -----
:- op(1200, xfx, :->).
:- discontiguous command/1.
term_expansion(command(H,A) :-> B, [R, command(H)]) :- dcg_translate_rule((mpd_protocol:command(H,T) --> {phrase(A, T)}, B), R).
term_expansion(command(H,A,Bin) :-> B, [R, command(H)]) :- dcg_translate_rule((mpd_protocol:command(H,T,Bin) --> {phrase(A, T)}, B), R).

command(commands, []) :-> {setof(C, command(C), Commands)}, foldl(report(command), [close, idle|Commands]).
command(save,     a(path([Name]))) :-> {save_state(Name)}.
command(setvol,   a(num(V)))       :-> {upd_and_notify(volume, (set(V) <\> [mixer]))}.
command(single,   a(nat(X)))       :-> {upd_and_notify_option(single-X)}.
command(consume,  a(nat(X)))       :-> {upd_and_notify_option(consume-X)}.
command(add,      a(path(Path)))   :-> {add_at(nothing, Path, _)}.
command(addid,    (a(path(Path)), maybe(a(nat), Pos))) :-> {add_at(Pos, Path, just(Id))}, report('Id'-Id).
% command(clear,    [])                       :-> {updating_queue_state(set_songs([]))}.
command(delete,   maybe(a(range), R))       :-> {updating_queue_state(delete_range(R, _))}.
command(deleteid, a(pid(Id)))               :-> {updating_queue_state(delete_id(Id, _))}.
command(move,     (a(nat(P1)), a(nat(P2)))) :-> {reordering_queue(move(P1, P2))}.
command(moveid,   (a(pid(I1)), a(nat(P2)))) :-> {reordering_queue((id_pos(I1,P1), move(P1, P2)))}.
command(swap,     (a(nat(P1)), a(nat(P2)))) :-> {reordering_queue(swap(P1, P2))}.
command(swapid,   (a(pid(I1)), a(pid(I2)))) :-> {reordering_queue(swap_id(I1, I2))}.
command(shuffle,  maybe(a(range), R))       :-> {reordering_queue(shuffle(R))}.
command(playid,   a(pid(Id)))                  :-> {updating_play_state(play(_, Id))}.
command(play,     maybe(a(nat), N))            :-> {updating_play_state(play(N))}.
command(stop,     [])                          :-> {updating_play_state(stop)}.
command(previous, [])                          :-> {updating_play_state(step_track_or_prog(prev))}.
command(next,     [])                          :-> {updating_play_state(step_track_or_prog(next))}.
command(pause,    maybe(a(nat), X))            :-> {updating_play_state(fsnd(fjust(pause(X))))}.
command(seek,     (a(nat(Pos)), a(num(PPos)))) :-> {updating_play_state(seek_pos_id(Pos, _, PPos))}.
command(seekid,   (a(pid(Id)), a(num(PPos))))  :-> {updating_play_state(seek_pos_id(_, Id, PPos))}.
command(seekcur,  a(seek_spec(Spec)))          :-> {updating_play_state(seekcur(Spec))}.
command(update,   maybe_quoted_path(Path)) :-> update_db(Path).
command(lsinfo,   maybe_quoted_path(Path)) :-> lsinfo(Path).
command(playlistinfo,  maybe(a(range), R)) :-> reading_state(queue, playlistinfo(R)).
command(playlistid,    [])                 :-> reading_state(queue, playlistinfo(nothing)).
command(plchanges,     a(nat(V)))          :-> reading_state(queue, plchanges(V)).
command(currentsong,   [])                 :-> reading_state(queue-player, currentsong).
command(listplaylists, arb) :-> [].
command(tagtypes, []) :-> foldl(report(tagtype), ['Artist', 'Album', 'Title', 'Track', 'Date', 'Comment', 'AvailableUntil']).
command(tagtypes, foldl(a(atom), [_Cmd|_Args])) :-> [].
command(outputs,  []) :-> foldl(report, [outputid-0, outputname-'Default output', outputenabled-1]).
command(status,   []) :-> foldl(report_state, [volume, single, consume]), reading_state(queue-player, report_status).
command(stats,    []) :-> {stats(Stats)}, foldl(report, Stats).
command(decoders, []) :-> [].
command(list,     list_args(Tag, Filters, GroupBy)) :-> db_list(Tag, Filters, GroupBy).
command(find,     find_args(Filters)) :-> db_find(true, Filters).
command(search,   find_args(Filters)) :-> db_find(false, Filters).
command(findadd,  find_args(Filters)) :-> {db_find(true, Filters, Files), add_multi(Files)}.
command(searchadd,find_args(Filters)) :-> {db_find(false, Filters, Files), add_multi(Files)}.
command(count,    find_args(Filters)) :-> db_count(Filters). % group not supported
command(ping,     []) :-> [].
command(make,     []) :-> {make}.

command(albumart,    (a(path(Path)), a(nat(Offset))), swimpd:reply_url_bin(URL, Offset)) :-> {db_image(series, Path, URL)}.
command(readpicture, (a(path(Path)), a(nat(Offset))), swimpd:reply_url_bin(URL, Offset)) :-> {db_image(episode, Path, URL)}.

find_args(Filters) --> foldl(tag_value, Filters).
list_args(album, [artist-Artist], []) --> a("album"), a(atom(Artist)).
list_args(Tag, Filters, GroupBy) --> a(tag(Tag)), foldl(tag_value, Filters), foldl(group_by, GroupBy).

reply_url_bin(URL, Offset) :-
   setup_call_cleanup(
      http_open(URL, Stream, [header(content_type, Type), size(Size), range(bytes(Offset, end))]),
      (Total is Offset + Size, reply_binary(Type, Total, Size, Stream)),
      close(Stream)).

% -- interaction with state --
reading_state(K, Action) --> {excl(states(K, V))}, call(Action, V).
upd_and_notify(K, P) :- excl(upd_states(K, upd_and_enact(K, P, Changes))), sort(Changes, Changed), notify_all(Changed).
upd_and_enact(K, P, Changes, S1, S2) :- call_dcg(P, S1-Changes, S2-[]), enact(K, Changes, S1, S2), !.

upd_and_notify_option(Key-V) :- upd_and_notify(Key, set(V) <\> [options]).
updating_queue_state(Action) :- upd_and_notify(queue-player, fqueue(Action,V,Songs)), add_queue(V, Songs). %
updating_play_state(Action)  :- upd_and_notify(player, fplay_state(Action) <\> [player]).
fplay_state(Action, P1, P2)  :- state(queue, _-Q), call(Action, Q-P1, Q-P2). % NB. Action must not change Q.
fqueue(P, V2, Q2, ((V1-Q1)-P1)-[playlist|C1], ((V2-Q2)-P2)-C2) :- call(P, (Q1-P1)-C1, (Q2-P2)-C2), succ(V1, V2).

reordering_queue(Action) :- updating_queue_state(\< preserving_player(Action)).
preserving_player(P) --> (P // trans(Songs1, Songs2)) <\> fmaybe(update_pos(Songs1, Songs2)).

report_state(K) --> reading_state(K, report(K)).
uptime(T) :- get_time(Now), vstate(start_time, Then), T is integer(Now - Then).
stats([uptime-T, db_update-DD|DBStats]) :- uptime(T), vstate(dbtime, D), round(D,DD), db_stats(DBStats).
update_db(Path) --> {flag(update, JOB, JOB+1), spawn(update_db_and_notify(Path))}, report(updating_db-JOB). % FIXME: put JOB in state
update_db_and_notify(Path) :- db_update(Path), get_time(Now), set_vstate(dbtime, Now), notify_all([database]).

enact(volume, [], _, _) :- !, debug(mpd(alert), "UNEXPECTED ENACT VOLUME CLAUSE", []).
enact(volume, [mixer], _, V) :- !, set_volume(V).
enact(single, _, _, _).
enact(consume, _, _, _).
enact(player, Changes) --> ({member(player, Changes)} -> {state(queue, _-Ss)}, enact_player_change(Ss-Ss); true2).
enact(queue-player, Changes) --> ({member(player, Changes)} -> enact_queue_change; true2).
enact_queue_change --> trans(_-Songs1, _-Songs2) <\> enact_player_change(Songs1-Songs2).

% -- playlist management --
playlistinfo(R, _-Songs) --> {enum(Songs, NS), subrange(R, NS, NS2)}, foldl(report_song_info, NS2).
subrange(just(N:M), L, Sel) :- length(Pre, N), length(PreSel, M), append(PreSel, _, L), append(Pre, Sel, PreSel).
subrange(nothing, L, L).

plchanges(V, _-Songs) --> {version_queue(V, OldSongs), enum(Songs, NSongs)}, report_changes(OldSongs, NSongs).
report_changes(_, []) --> !.
report_changes([], NSongs) --> foldl(report_song_info, NSongs).
report_changes([Old|Olds], [N-New|News]) -->
   ({Old=New} -> []; report_song_info(N-New)),
   report_changes(Olds, News).

currentsong((_-Songs)-PS) --> maybe(currentsong(Songs), PS).
currentsong(Songs, ps(Pos, _)) --> {nth0(Pos, Songs, Song)}, report_song_info(Pos-Song).

report_song_info(Pos-song(PID, _, Tags)) --> {pid_id(PID, Id)}, foldl(report, Tags), foldl(report, ['Pos'-Pos, 'Id'-Id]).

add_multi(Files) :- updating_queue_state(\< \< add_at_end(foldl(addid, Files, _))).
add_at(nothing, File, Id) :- updating_queue_state(\< \< add_at_end(addid(File, Id))).
add_at(just(Pos), File, Id) :- reordering_queue(insert_at(Pos, addid(File, Id))).
add_at_end(P, Songs1, Songs2) :- phrase((list(Songs1), P), Songs2).

copy --> [X] <\> [X].
insert_at(Pos, P, Songs1, Songs2) :-
   rep(Pos, copy, Songs1-Songs2, Suffix-PrefixT),
   phrase(P, PrefixT, Suffix).

set_songs(Songs) --> trans(_-Player, (Songs-nothing)) <\> maybe(player_if_playing, Player).
player_if_playing(_) --> [player].

delete_range(nothing, Id) --> \< get(_-just(ps(Pos, _))), delete(Pos, Id).
delete_range(just(M:N), Ids) --> {numlist(M, N, Is), reverse(Is, [_|Js])}, foldl(delete, Js, Ids).
delete_id(Id, Ps) --> delete(Pos, Id) -> {Ps = [Pos|Ps2]}, delete_id(Id, Ps2); {Ps=[]}.
delete(Pos, Id) -->
   \< get(Songs-PS), {nth0(Pos, Songs, song(Id,_,_))},
   (\< {PS = just(ps(Pos, _))} -> step(keep, next) <\> [player]; []),
   \< (select_nth(Pos, _) <\> fmaybe(update_pos(Pos))).

update_pos(Pos, ps(PPos1, Slave), ps(PPos2, Slave)) :-
   (PPos1 < Pos -> PPos1 = PPos2; PPos1 >= Pos, succ(PPos2, PPos1)).

update_pos(Songs1, Songs2, ps(Pos1, Sl), ps(Pos2, Sl)) :-
   nth0(Pos1, Songs1, song(Id, _, _)),
   nth0(Pos2, Songs2, song(Id, _, _)).

shuffle(nothing) --> random_permutation.
swap_id(Id1, Id2) --> foldl(id_pos, [Id1, Id2], [P1, P2]), swap(P1, P2).
swap(P1, P2) --> fnth(P1, Song1, Song2), fnth(P2, Song2, Song1).
move(P1, P2, Songs1, Songs2) :- select_nth(P1, Song, Songs1, Songs3), select_nth(P2, Song, Songs2, Songs3).
id_pos(Id, Pos, Songs, Songs) :- nth0(Pos, Songs, song(Id, _, _)).

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

% works on a state of type (playlist_version * list(song)) * maybe(player_state)
eos_queue  --> []. % if(state(consume, 1), FIMXE how to do this?

% these work on a state of type list(song) * maybe(player_state) (and not allowed to change song list?)
eos_player --> stop, if(state(single, 0), step(play, next)).
stop(Songs-just(ps(Pos, _)), Songs-just(ps(Pos, nothing))).
step(Op, Dir) --> get(Songs-just(ps(Pos, _))), ({upd_pos(Dir, Songs, Pos, Pos1)} -> play(Op, Pos1, _); \> set(nothing)).
upd_pos(next, L, Pos, Pos1) :- succ(Pos, Pos1), length(L, N), Pos1 < N.
upd_pos(prev, _, Pos, Pos1) :- succ(Pos1, Pos).

% -- next and previous handling --
step_track_or_prog(Dir) -->
   (  current(_, PID), {is_programme(PID), pid_tracks(PID, Tracks)}
   -> {gst_audio_info(_, au(_Dur, Elap, _, _)),
       cursor_at_time(Elap, [], Tracks, cursor(Fore, _, Aft)),
       track_in_direction(Dir, Fore, Aft, Target)
      },
      seekcur(abs(Target.offset.start))
   ;  step(play, Dir)
   ).

cursor_at_time(Time, Fore, [Track | Aft], Cursor) :-
   compare(R1, Time, Track.offset.start),
   compare(R2, Time, Track.offset.end),
   ( build_cursor(R1, R2, Fore, Track, Aft, Cursor) -> true
   ; cursor_at_time(Time, [Track | Fore], Aft, Cursor)
   ).

track_in_direction(prev, [T|_], _, T).
track_in_direction(next, _, [T|_], T).

build_cursor(<, <, Fore, T, Aft, cursor(Fore, nothing, [T | Aft])). % T ...  (start, end)
build_cursor(=, <, Fore, T, Aft, cursor(Fore, nothing, [T | Aft])). % (T=start,   end)
build_cursor(=, =, Fore, T, Aft, cursor(Fore, just(T), Aft)).       % (T=start=end)
build_cursor(>, <, Fore, T, Aft, cursor(Fore, just(T), Aft)).       % (start, T, end)
build_cursor(>, =, Fore, T, Aft, cursor([T | Fore], nothing, Aft)). % (start,  T=end)

% --------------------------------------------------------------
% play//1, play//2, play//3 - state transformers on list(song) * maybe(player_state)
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
gst:notify_eos :- updating_play_state(eos_player), updating_queue_state(eos_queue).

% -- status --
report_status((Ver-Songs)-PS) -->
   {length(Songs, Len)},
   foldl(report, [repeat-0, random-0, playlist-Ver, playlistlength-Len]),
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
   {  gst_audio_info(Prog, au(Dur2, Elap2, BR, Fmt)) -> true
   ;  Elap2/Dur2=Prog, BR=nothing, Fmt=nothing
   },
   {format(string(Time), '~0f:~0f', [Elap2, Dur2])},
   foldl(report, [state-State, time-Time, elapsed-Elap2, duration-Dur2]),
   foldl(maybe_report, [bitrate-BR, audio-Fmt]).
maybe_report(K-V) --> maybe(report(K), V).

% --- command and reply DCGs -----
a(P) --> " ", (quoted(P); break(` "`) // P).
a(P, X) --> " ", (quoted(P, X); break(` "`) // call(P, X)).
pid(Id) --> nat(N), {id_pid(N, Id)}.
range(N:M) --> nat(N), (":", nat(M); {succ(N,M)}).
seek_spec(abs(T)) --> decimal // num(T).
seek_spec(rel(T)) --> (any(`+-`), decimal) // num(T).
maybe_quoted_path(Path) --> {Path=[]}; " ", quoted(path(Path)).
path(Path) --> seqmap_with_sep("/", path_component, Path).
path([]) --> [].
path_component(Dir) --> +(notany(`/`)) // atom(Dir).
tag(Tag) --> +(ctype(graph)) // atom(Word), {downcase_atom(Word, Tag), Tag \= group}. % FIXME: make definite
tag_value(Tag-Value) --> a(tag(Tag)), a(atom(Value)). % FIXME: typed tags?
group_by(G) --> a("group"), a(tag(G)).

