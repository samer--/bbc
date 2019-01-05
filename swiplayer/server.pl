:- module(mpd_server, [ start_mpd/2, mpd_server/2, mpd_interactor/0, mpd_init/0 ]).

:- use_module(library(socket)).
:- use_module(library(listutils)).
:- use_module(library(insist)).
:- use_module(library(xpath)).
:- use_module(library(dcg_codes)).
:- use_module(library(dcg_pair)).
:- use_module(library(dcg/basics)).
:- use_module(bbc).

maybe(_, nothing) --> [].
maybe(P, just(X)) --> call(P, X).
enum(Xs, IXs) :- foldl(enum, Xs, IXs, 0, _).
enum(X, I-X, I, J) :- J is I + 1.

% playlist_state ==  pair(list(song), maybe(play_state)).
% play_state    ---> ps(song, maybe(song), au_state).
% au_state      ---> stopped; playing(bool, duration, elapsed, bitrate, format).
:- dynamic state/1.

set_state(S) :- retractall(state(_)), assert(state(S)).
upd_state(P) :- state(S1), call(P, S1, S2), set_state(S2).
mpd_init :- set_state(nothing - s{volume: 50, repeat: 0, random: 0, single: 0, consume: 0, playlist: 0, error: nothing}).
start_mpd(Port, Options) :- thread_create(mpd_server(Port, Options), _, [alias(mpd_server)]).

add(S, nothing, just([S]-nothing)).
add(S, just(Songs1-PS), just(Songs2-PS)) :- append(Songs1, [S], Songs2).
clear(_, nothing).

%!  mpd_server(+Port, +Options) is det.
%
%   Currently defined options are:
%
%           * allow(list(ip))
%           Allow access from addresses in list of terms of type ip, where
%           =|ip ---> ip(integer, integer, integer, integer)|=.
%           Default is [ip(127,0,0,1])] (localhost only).
mpd_server(Port, Options) :-
   option(allow(Peers), Options, [ip(127,0,0,1)]),
   setup_call_cleanup(tcp_socket(Socket),
                      socket_server(Socket, Port, Peers),
                      tcp_close_socket(Socket)).

socket_server(Socket, Port, Peers) :-
   tcp_setopt(Socket, reuseaddr),
   tcp_bind(Socket, Port),
   tcp_listen(Socket, 5),
   server_loop(Socket, Peers).

server_loop(Socket, Peers) :-
   tcp_accept(Socket, Slave, Peer),
   debug(mpd, "new connection from ~w", [Peer]),
   tcp_open_socket(Slave, InStream, OutStream),
   set_stream(InStream, close_on_abort(false)),
   set_stream(OutStream, close_on_abort(false)),
   catch(thread_create(service_client(InStream, OutStream, Peer, Peers), _, []),
         error(permission_error(create, thread, mpdclient), _), fail), !,
   server_loop(Socket, Peers).

set_telnet_stream(Enc, S) :- maplist(set_stream(S), [encoding(Enc), newline(posix)]).
service_client(In, Out, Peer, Peers) :-
   thread_self(Id),
   call_cleanup(maybe_service_client(In, Out, Peer, Peers),
                (close(In), close(Out), thread_detach(Id))).
maybe_service_client(In, Out, Peer, Peers) :-
   (  member(Peer, Peers)
   -> set_prolog_IO(In, Out, user_error), prompt(_, ''),
      current_prolog_flag(encoding, Enc),
      maplist(set_telnet_stream(Enc), [user_input, user_output]),
      writeln(user_output, 'OK MPD 0.20.0'),
      mpd_interactor
   ;  format(Out, 'Access denied.~n', [])
   ).

read_command(Cmd) :-
   read_line_to_codes(user_input, Cmd),
   debug(mpd, ">> ~s", [Cmd]).

mpd_interactor :- read_command(Cmd), handle(Cmd).
handle(end_of_file) :- !.
handle(`close`) :- !.
handle(Cmd) :-
   insist(parse_head(Head, Tail, Cmd, [])),
   insist(catch((execute(Head, Tail), Reply=ok), mpd_ack(Ack), Reply=Ack)),
   reply(Reply), mpd_interactor.

execute(command_list_ok_begin, []) :- !,  command_list(list_ok).
execute(command_list_begin, []) :- !, command_list(silent).
execute(idle, []) :- !, insist(read_command(`noidle`)).
execute(Cmd, T) :- execute(0-'', Cmd, T).

execute(Ref, Cmd, T) :-
   catch(command(Cmd, T, Output, []), mpd_err(Err), throw(mpd_ack(ack(Ref, Err)))),
   debug(mpd, '<< ~s', [Output]), format('~s', [Output]).

command_list(Reply) :- accum(Commands, []), execute_list(Reply, Commands, 0).
execute_list(_, [], _).
execute_list(Reply, [Cmd|Cmds], Pos) :-
   parse_head(Head, Tail, Cmd, []),
   execute(Pos-Head, Head, Tail),
   sub_reply(Reply), succ(Pos, Pos1),
   execute_list(Reply, Cmds, Pos1).

sub_reply(silent).
sub_reply(list_ok) :- writeln('list_OK').

reply(ok) :- writeln('OK'), flush_output, debug(mpd, '<< OK', []).
reply(ack(Pos-SubCmd, err(Code, Fmt, Args))) :-
   format(string(Msg), Fmt, Args),
   format(user_output, 'ACK [~d@~d] {~s} ~s\n', [Code, Pos, SubCmd, Msg]).

accum --> {read_command(Cmd)}, accum_cont(Cmd).
accum_cont(`command_list_end`) --> !, [].
accum_cont(Cmd) --> [Cmd], accum.

parse_head(Head, Tail) --> string_without(` `, H), tail(Tail), {atom_codes(Head, H)}.
tail([]) -->[].
tail(Tail) --> " ", string(Tail).

command(play, [])  --> !.
command(playid, _) --> !.
command(pause, []) --> !.
command(ping, [])  --> !.
command(lsinfo, _) --> !, {findall(S, service(S), Services)}, foldl(service_dir, Services).
command(stats, [])   --> !, maplist(report, [artists-0, albums-0, songs-1000]). % uptime, db_playtime, db_update, playtime
command(outputs, []) --> !, foldl(report, [outputid-0, outputname-'Default output', outputenabled-1]).
command(status, [])  --> !, reading_state(status).
command(playlistinfo, []) --> !, reading_state(if_playable(playlistinfo)).
command(currentsong, [])  --> !, reading_state(if_playable(currentsong)).
command(Cmd, _) --> {throw(mpd_err(err(5, 'unknown command "~s"', [Cmd])))}.

reading_state(Action) --> {state(S)}, call(Action, S).
if_playable(Action, PS-_) --> maybe(Action, PS).

playlistinfo(Songs-_) --> {enum(Songs, NumSongs)}, foldl(report_song_info, NumSongs).
currentsong(Songs-PS) --> maybe(currentsong(Songs), PS).
currentsong(Songs, ps(Pos-_, _, _)) --> {nth0(Pos, Songs, Song)}, report_song_info(Pos-Song).
report_song_info(Pos-s(Id, Tags)) --> foldl(report, Tags), foldl(report, ['Pos'-Pos, 'Id'-Id]).

service_dir(S) -->
   {service(S, _, Name)}, report(directory-Name),
   (  {service_schedule(S, Sched)}
   -> {xpath(Sched, /self(@updated), Updated)},
      report('Last-Modified'-Updated)
   ;  []
   ).

status(PL-GS) -->
   foldl(report_key(GS), [volume, repeat, random, single, consume, playlist]),
   report_playlist_state(PL).

report_playlist_state(nothing) --> foldl(report, [playlistlength-0, state-stop]).
report_playlist_state(just(Songs-PS)) -->
   {length(Songs, Len)},
   report(playlistlength-Len),
   report_play_state(PS).
report_play_state(nothing) --> report(state-stop).
report_play_state(just(ps(Current, Next, Audio))) -->
   report_song(song, songid, Current),
   maybe(report_song(nextsong, nextsongid), Next),
   report_audio_state(Audio).
report_song(K1, K2, Pos-Id) --> foldl(report, [K1-Pos, K2-Id]).
report_audio_state(stopped) --> report(state-stop).
report_audio_state(playing(State, Dur, Elap, BR, Fmt)) -->
   foldl(report, [state-State, elapsed-Elap, duration-Dur, bitrate-BR, audio-Fmt]).

report_key(S, Key) --> report(Key - S.Key).
report(Name-Value) --> fmt('~w: ~w\n', [Name, Value]).
