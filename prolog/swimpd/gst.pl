:- module(gst, [start_gst_thread/0, gst_audio_info/2, gst/2, enact_player_change/3, set_volume/1]).
% TODO: audio and video sink control. Fix protocol.

:- use_module(library(insist), [insist/1]).
:- use_module(library(dcg_core), [seqmap_with_sep//3, (//)//2]).
:- use_module(library(dcg_codes), [fmt//2]).
:- use_module(library(snobol), [break//1, arb//0, any//1]).
:- use_module(state, [state/2, set_state/2]).
:- use_module(protocol, [notify_all/1]).
:- use_module(tools,  [forever/1, parse_head//2, num//1, nat//1, maybe/2, setup_stream/2]).

:- multifile notify_eos/0, id_wants_bookmark/1.

gst_peer :-
   setup_call_cleanup(start_gst(PID, IO),
                      catch(gst_reader_thread(PID-IO), Ex, (process_kill(PID), throw(Ex))),
                      process_wait(PID, _Status)).

start_gst(PID,In-Out) :-
   process_create(python('gst12.py'), [], [stdin(pipe(In)), stdout(pipe(Out)), stderr(std), process(PID)]).

:- dynamic gst/2.
gst_reader_thread(_-(In-Out)) :-
   thread_self(Self),
   maplist(setup_stream([close_on_abort(false), buffer(full)]), [In, Out]),
   setup_call_cleanup(assert(gst(Self,In)), gst_reader(Self, Out), retract(gst(Self,In))).

gst_reader(Self, Out) :- state(volume, V), set_volume(V), gst_read_next(Self, Out).
gst_read_next(Self, Out) :- read_line_to_codes(Out, Codes), gst_handle(Codes, Self, Out).
gst_handle(end_of_file, _, _) :- !, debug(gst, 'End of stream from gst', []).
gst_handle(Codes, Self, Out) :-
   debug(gst, '~~> ~s', [Codes]),
   insist(phrase(parse_head(Head, Tail), Codes)),
   (  phrase(gst_message(Head, Msgs, Globals), Tail)
   -> maplist(thread_send_message(Self), Msgs),
      maplist(set_global, Globals)
   ;  true
   ),
   gst_read_next(Self, Out).
set_global(K-V) :- set_state(K, V). %, notify_all([player]). % Upsets MPD Droid

gst_message(eos, [], []) --> {notify_eos}.
gst_message(position, [position-X], []) --> " ", num(X).
gst_message(bitrate, [], [bitrate-just(BR)]) --> " ", num(BR).
gst_message(duration, [], [duration-D]) --> " ", num(D).
gst_message(format, [], [format-just(Rate:Fmt:Ch)]) --> " ", split_on_colon([nat(Rate), sample_fmt(Fmt), nat(Ch)]).
sample_fmt(f) --> "F", !, arb.
sample_fmt(N) --> [_], nat(N), ([]; any(`LB_`), arb).

set_volume(V) :- FV is (V/100.0)^1.75, send(fmt("volume ~5f", [FV])).
gst_uri(URI) :- send(fmt("uri ~s",[URI])).
send(P) :- gst(_,In), phrase(P, Codes), debug(gst, '<~~ ~s', [Codes]), format(In, "~s\n", [Codes]), flush_output(In).
recv(K, MV) :- gst(Id, _), ( thread_get_message(Id, K-V, [timeout(5)]) -> MV = just(V)
                           ; print_message(warning, recv_timeout(K)), MV = nothing).

start_gst_thread :- thread_create(forever(gst_peer), _, [alias(gst_slave), detached(true)]).

split_on_colon(Ps) --> seqmap_with_sep(`:`, broken(`:`), Ps).
broken(Cs, P) --> break(Cs) // P.

gst_audio_info(_, au(Dur, Elap, BR, Fmt)) :-
   gst(Id,_), flush_queue(Id, position),
   send("position"), recv(position, just(Elap)),
   maplist(state, [bitrate, format, duration], [BR, Fmt, Dur]).
flush_queue(Q, K) :- (thread_get_message(Q, K-_, [timeout(0)]) -> flush_queue(Q, K); true).

enact_player_change(_, nothing, nothing).
enact_player_change(Songs-_, just(ps(Pos, Slave)), nothing) :- maybe(stop_if_playing(Songs-Pos), Slave).
enact_player_change(_-Songs, nothing, just(ps(Pos, Slave))) :- maybe(cue_and_maybe_play(Songs-Pos), Slave).
enact_player_change(SongsPair, just(PS1), just(PS2)) :- enact_ps_change(SongsPair, PS1, PS2).

enact_ps_change(Songs1-Songs2, ps(Pos1, Sl1), ps(Pos2, Sl2)) :-
   nth0(Pos1, Songs1, song(Id1, _, _)),
   nth0(Pos2, Songs2, song(Id2, _, _)),
   (  Id1 \= Id2
   -> maybe(stop_if_playing(Songs1-Pos1), Sl1),
      maybe(cue_and_maybe_play(Songs2-Pos2), Sl2)
   ;  enact_slave_change((Songs1-Pos1)-(Songs2-Pos2), Sl1, Sl2)
   ).

enact_slave_change(_,          nothing, nothing) :- !.
enact_slave_change(SongsPos-_, just(_), nothing) :- !, save_position(SongsPos), send("stop").
enact_slave_change(_-SongsPos, nothing, just(S-Au)) :- !, cue_and_maybe_play(SongsPos, S-Au).
enact_slave_change(_,          just(S1-_), just(S2-_)) :-
   (  S1-S2 = play-pause -> send("pause")
   ;  S1-S2 = pause-play -> send("play")
   ;  true
   ).
stop_if_playing(SongsPos, _) :- save_position(SongsPos), send("stop").
cue_and_maybe_play(Songs-Pos, P-(_/Dur)) :-
   nth0(Pos, Songs, song(_, GetURL, _)), call(GetURL, URL),
   maplist(set_state, [bitrate, format, duration], [nothing, nothing, Dur]),
   send(fmt('uri ~s', [URL])),
   restore_position(Songs-Pos),
   (P=play -> send("play"); true).

save_position(_).
restore_position(_).
% save_position(Songs-Pos) :-
%    nth0(Pos, Songs, song(Id, _, _)),
%    (  id_wants_bookmark(Id)
%    -> send("position"), recv(position, PPos), maybe(set_state(position(Id)), PPos)
%    ;  true
%    ).

% restore_position(Songs-Pos) :-
%    nth0(Pos, Songs, song(Id, _, _)),
%    (state(position(Id), PPos) -> send(("seek ", num(PPos))); true).
