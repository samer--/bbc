:- module(swimpd, [ run/2, start_mpd/2, mpd_server/2, mpd_interactor/0, mpd_init/0 ]).

:- use_module(library(socket)).
:- use_module(library(listutils)).
:- use_module(library(insist)).
:- use_module(library(xpath)).
:- use_module(library(dcg/basics), [string//1, string_without//2]).
:- use_module(library(data/pair)).
:- use_module(library(dcg_core)).
:- use_module(library(dcg_codes)).
:- use_module(library(dcg_pair)).
:- use_module(library(snobol)).
:- use_module(library(memo)).
:- use_module(bbc).

/* <module> MPD server for BBC radio programmes.

   @todo actually play something
   @todo check behaviour of play when no current song
   @todo check if add can change play state
   @todo rethink top level control flow
   @todo unify command arg parsing
   @todo randomised play sequence
 */

%! mpd_init is det.
%  Set state of MPD to an empty queue with version 0 and volume set to 50%.
mpd_init :-
   get_time(Now),
   maplist(set_state, [start_time, dbtime, volume, queue], [Now, Now, 50, 1-([]-nothing)]),
   retractall(queue(_,_)), assert(queue(1, [])).

%! start_mpd(Port, Options) is det.
%  Start server as a detached thread with alias =|mpd_server|=.
%  See mpd_server/2 for description of parameters.
start_mpd(Port, Options) :- thread_create(mpd_server(Port, Options), _, [detached(true), alias(mpd_server)]).

:- initialization(mpd_init, program).

run(Port,Opts) :- maplist(debug, [mpd(connection), mpd(command)]), start_mpd(Port, Opts).
% ------------------------------ State management ------------------------------

longname_service(LongName, S) :- service(S, _, LongName), (service_schedule(S, _) -> true; update_service(S)).
update_service(S) :- get_time(Now), bbc:log_and_succeed(time_service_schedule(Now, S, _)), set_state(dbtime, Now).

% state = pair(pair(integer, pair(list(song), maybe(play_state))), dict).
% play_state ---> ps(natural, au_state).
% au_state   ---> stopped; playing(bool, duration, elapsed, bitrate, format).
:- dynamic state/2, client/1, queue/2.
set_state(Key, Val) :- retractall(state(Key, _)), assert(state(Key, Val)).

:- volatile_memo pid_id(+atom, -integer).
pid_id(_, Id) :- flag(songid, Id, Id+1).

clear --> \< trans(Q, ([]-nothing)), \> player_if_queue_playing(Q).
player_if_queue_playing(_-PS) --> maybe(player_if_playstate_playing, PS).
player_if_playstate_playing(_) --> [player].

deleteid(Id) -->
   \< trans(Songs1-PS1, Songs2-PS2),
   {nth0(Pos, Songs1, song(Id, _, _), Songs2)},
   ( {PS1 = just(ps(Pos, _))} -> {PS2 = nothing}, \> [player]
   ; {PS2 = PS1}
   ).

addid([ServiceLongName], nothing) -->
   {longname_service(ServiceLongName, S), findall(E, service_entry(S, E), Entries)},
   ffst(foldl(add(ServiceLongName), _Ids, Entries)).

addid([ServiceLongName, PID], just(Id)) -->
   {longname_service(ServiceLongName, S), service_entry(S, E), prop(E, pid(PID))},
   ffst(add(ServiceLongName, Id, E)).

add(ServiceLongName, Id, E) --> {entry_tags(ServiceLongName, E, Id, Tags)}, add(song(Id, E, Tags)).
add(S, Songs1, Songs2) :- append(Songs1, [S], Songs2).

pause(X, ps(C, playing(P1, Dur, El, BR, Au)), ps(C, playing(P2, Dur, El, BR, Au))) :- pausex(X, P1, P2).
pausex(just(1), _, pause).
pausex(just(0), _, play).
pausex(nothing, pause, play).
pausex(nothing, play, pause).

stop(Songs-just(ps(Pos, _)), Songs-just(ps(Pos, stopped))).
next     --> get(_-just(ps(Pos, _))), {succ(Pos, NextPos)}, play(NextPos, _).
previous --> get(_-just(ps(Pos, _))), {succ(PrevPos, Pos)}, play(PrevPos, _).
play --> get(_-just(ps(Pos, _))), !, play(Pos, _).
play --> get([_|_]-nothing), play(0, _).
play(Pos, Id, Songs-_, Songs-just(ps(Pos, playing(play, Dur, 0.0, 320, 48000:f:2)))) :-
   nth0(Pos, Songs, song(Id, E, _)),
   prop(E, duration(Dur)).

% ------------------------ client interactor --------------------

% --- core protocol ---
% ! mpd_interactor is det.
% Run MPD client interaction using the current user_input and user_output Prolog streams.
mpd_interactor :-
   thread_self(Self),
   setup_call_cleanup(assert(client(Self)), wait_for_input, retract(client(Self))).

read_command(Cmd) :-
   read_line_to_codes(user_input, Cmd),
   debug(mpd(command), ">> ~s", [Cmd]).

wait_for_input :- read_command(Cmd), handle(Cmd).
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

notify_all(Subsystems) :- forall(client(Id), maplist(notify(Id), Subsystems)).
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

idle_filter(true1) --> [].
idle_filter(in(Subsystems)) --> seqmap_with_sep(" ", quoted(atom), Subsystems).

report(Name-Value) --> report(Name, Value).
report(Name, Value) --> fmt('~w: ~w\n', [Name, Value]).

% --- command implementations -----
:- meta_predicate upd_and_notify(+, //).
upd_and_notify(K, P) :-
   with_mutex(swimpd, (state(K, S1), call_dcg(P, S1-Changes, S2-[]), set_state(K, S2))),
   sort(Changes, Changed), notify_all(Changed).

reading_state(K, Action) --> {state(K, S)}, call(Action, S).
reading_queue(Action, _-Q) --> call(Action, Q).
updating_play_state(Action) :- upd_and_notify(queue, (\< fsnd(Action), \> [player])).
updating_queue_state(Action) :-
   upd_and_notify(queue, (fqueue(Action,V,Songs), \> [playlist])),
   assert(queue(V,Songs)).
fqueue(P, V2, Songs, (V1-Q1)-C1, (V2-Q2)-C2) :- call(P, Q1-C1, Q2-C2), succ(V1, V2), Q2 = Songs-_.

:- op(1200, xfx, :->).
:- discontiguous command/1, command/4.
term_expansion(command(H,T) :-> Body, [(command(H,T) --> Body), command(H)]).

command(commands, []) :-> {findall(C, command(C), Commands)}, foldl(report(command), [close, idle|Commands]).
command(ping, [])     :-> [].
command(setvol, Tail) :-> {phrase(quoted(num(V)), Tail), upd_and_notify(volume, (\< set(V), \> [mixer]))}.
command(clear, [])    :-> {updating_queue_state(clear)}.
command(add, Tail)    :-> {phrase(quoted(path(Path)), Tail), updating_queue_state(\< addid(Path, _))}.
command(addid, Tail)  :-> {phrase(quoted(path(Path)), Tail), updating_queue_state(\< addid(Path, just(Id)))}, report('Id'-Id).
command(deleteid, Tail) :-> {phrase(quoted(num(Id)), Tail), updating_queue_state(deleteid(Id))}.
command(playid, Tail) :-> {phrase(quoted(num(Id)), Tail), updating_play_state(play(_, Id))}.
command(play, [])     :-> {updating_play_state(play)}.
command(stop, [])     :-> {updating_play_state(stop)}.
command(previous, []) :-> {updating_play_state(previous)}.
command(next, [])     :-> {updating_play_state(next)}.
command(pause, Tail)  :-> {phrase(maybe(quoted(num), X), Tail), updating_play_state(fsnd(fjust(pause(X))))}.
command(update, Tail) :-> {phrase(maybe_quoted_path(Path), Tail)}, update(Path).
command(lsinfo, Tail) :-> {phrase(maybe_quoted_path(Path), Tail)}, lsinfo(Path).
command(stats, [])    :-> {uptime(T), state(dbtime, D)}, foldl(report, [artists-0, albums-0, songs-0, uptime-T, db_update-D]).
command(outputs, [])  :-> foldl(report, [outputid-0, outputname-'Default output', outputenabled-1]).
command(status, [])   :-> reading_state(volume, report(volume)), reading_state(queue, report_status).
command(playlistinfo, Tail) :-> {phrase(maybe(quoted(range), R), Tail)}, reading_state(queue, reading_queue(playlistinfo(R))).
command(playlistid, [])  :-> reading_state(queue, reading_queue(playlistinfo(nothing))).
command(plchanges, Tail) :-> {phrase(quoted(num(V)), Tail)}, reading_state(queue, reading_queue(plchanges(V))).
command(currentsong, []) :-> reading_state(queue, reading_queue(currentsong)).

update(Path) --> {flag(update, JOB, JOB+1), spawn(update_and_notify(Path))}, report(updating_db-JOB).
update_and_notify(Path) :- update(Path), notify_all([database]).

update([]) :- forall(service(S), update_service(S)).
update([ServiceLongName]) :- service(S, _, ServiceLongName), update_service(S).

lsinfo([]) -->
   {findall(S, service(S), Services)},
   foldl(service_dir, Services).
lsinfo([ServiceLongName]) -->
	{ longname_service(ServiceLongName, S),
     findall(Children, bbc:service_parent_children(S, _, Children), Families),
     findall(E, (member(Es, Families), member(E, Es)), Items)
	},
	foldl(programme(ServiceLongName), Items).

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

service_dir(S) -->
   {service(S, _, Name)}, report(directory-Name),
   (  {service_schedule(S, Sched)}
   -> {xpath(Sched, /self(@updated), Updated)},
      report('Last-Modified'-Updated)
   ;  []
   ).

programme(ServiceLongName, E) -->
	{ entry_tags(ServiceLongName, E, Id, Tags)},
	foldl(report, Tags), report('Id'-Id).

entry_tags(ServiceLongName, E, Id, [file-File, 'Title'-Title, 'Comment'-Syn, duration-Dur]) :-
	maplist(prop(E), [pid(PID), title(Title), synopsis(Syn), duration(Dur)]),
   pid_id(PID, Id), format(string(File),'~w/~w', [ServiceLongName, PID]).

report_status((Ver-(Songs-PS))) -->
   {length(Songs, Len)},
   foldl(report, [repeat-0, random-0, single-0, consume-0, playlist-Ver, playlistlength-Len]),
   report_play_state(PS, Songs).

report_play_state(nothing, _) --> report(state-stop).
report_play_state(just(ps(Pos,Audio)), Songs) -->
   report_nth_song(Songs, song, songid, Pos), {succ(Pos, Pos1)},
   opt(report_nth_song(Songs, nextsong, nextsongid, Pos1)),
   report_audio_state(Audio).

report_nth_song(Songs, K1, K2, Pos) -->
   {nth0(Pos, Songs, song(Id, _, _))},
   foldl(report, [K1-Pos, K2-Id]).
report_audio_state(stopped) --> report(state-stop).
report_audio_state(playing(State, Dur, Elap, BR, Fmt)) -->
   foldl(report, [state-State, elapsed-Elap, duration-Dur, bitrate-BR, audio-Fmt]).

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
   set_stream(In, close_on_abort(false)),
   set_stream(Out, close_on_abort(false)),
   catch(spawn(call_cleanup(client_thread(In, Out, Peer, Allow), (close(In), close(Out)))),
         error(permission_error(create, thread, mpdclient), _), fail), !,
   server_loop(Socket, Allow).

client_thread(In, Out, Peer, Allow) :- call(Allow, Peer), !, service_client(In, Out).
client_thread(_, Out, _, _) :- format(Out, 'Access denied.~n', []).

set_telnet_stream(Enc, S) :- maplist(set_stream(S), [encoding(Enc), newline(posix)]).
service_client(In, Out) :-
   set_prolog_IO(In, Out, user_error), prompt(_, ''),
   current_prolog_flag(encoding, Enc),
   maplist(set_telnet_stream(Enc), [user_input, user_output]),
   writeln('OK MPD 0.20.0'),
   mpd_interactor.

% -------------- DCG and other tools --------------------

true1(_).
in(List, Item) :- member(Item, List).
enum(Xs, IXs) :- foldl(enum, Xs, IXs, 0, _).
enum(X, I-X, I, J) :- J is I + 1.
spawn(G) :- thread_create(G, _, [detached(true)]).
fjust(P, just(X1), just(X2)) :- call(P, X1, X2).
pphrase(P) :- phrase(P, C), format('~s', [C]).

+(P) --> call_dcg(P), *(P).
*(P) --> []; +(P).

maybe(_, nothing) --> [].
maybe(P, just(Y)) --> call(P,Y).

digit  --> ctype(digit).
nat(N) --> {ground(N)}, !, num(N) // nat_digits.
nat(N) --> nat_digits // num(N).
nat_digits --> +digit.

num(N,S1,S2) :- ground(N), !, format(codes(S1,S2),'~w',[N]).
num(N,S1,S2) :- list(C,S1,S2), number_codes(N,C).
atom(A,S1,S2) :- ground(A), !, format(codes(S1,S2),'~w',[A]).
atom(A,S1,S2) :- list(C,S1,S2), atom_codes(A,C).

quoted(P, X) --> quoted(call(P, X)).
quoted(P) --> "\"", esc(esc_qq, Codes), "\"", {phrase(P, Codes)}.
esc_qq([0'"|Cs],Cs) --> "\\\"".
esc_qq([0'\\|Cs],Cs) --> "\\\\".
esc_qq([C|Cs],Cs) --> [C] // notany(`\\"`).
