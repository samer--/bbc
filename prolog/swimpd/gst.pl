:- module(gst, [start_gst_thread/1, gst_audio_info/3, gst/2, enact_player_change/3, set_volume/1]).

:- use_module(library(insist), [insist/1]).
:- use_module(library(dcg_core), [seqmap_with_sep//3, (//)//2]).
:- use_module(library(dcg_codes), [fmt//2]).
:- use_module(library(snobol), [break//1, arb//0, any//1]).
:- use_module(state, [state/2, set_state/2]).
:- use_module(asyncu, [spawn/1, setup_stream/2]).
:- use_module(tools,  [parse_head//2, num//1, nat//1, maybe/2]).

:- multifile spec_url/2, notify_eos/0.

with_gst(P, Status) :-
   setup_call_cleanup(start_gst(PID, IO),
                      catch(call(P, PID-IO), Ex, (process_kill(PID), throw(Ex))),
                      process_wait(PID, Status)).

start_gst(PID,In-Out) :-
   process_create(python('gst12.py'), [], [stdin(pipe(In)), stdout(pipe(Out)), stderr(std), process(PID)]),
   maplist(setup_stream([close_on_abort(false), buffer(line)]), [In, Out]).

:- dynamic gst/2.
gst_reader_thread(V, _-(In-Out)) :-
   thread_self(Self),
   setup_call_cleanup(assert(gst(Self,In)), gst_reader(V, Self, Out), retract(gst(Self,In))).

gst_reader(V, Self, Out) :- set_volume(V), gst_read_next(Self, Out).
gst_read_next(Self, Out) :- read_line_to_codes(Out, Codes), gst_handle(Codes, Self, Out).
gst_handle(end_of_file, _, _) :- debug(mpd(gst), 'End of stream from gst', []).
gst_handle(Codes, Self, Out) :-
   debug(mpd(gst), '~~> ~s', [Codes]),
   insist(phrase(parse_head(Head, Tail), Codes)),
   (phrase(gst_message(Head, Msgs), Tail) -> maplist(thread_send_message(Self), Msgs); true),
   gst_read_next(Self, Out).

gst_message(eos, []) --> {notify_eos}.
gst_message(bitrate, [bitrate(BR)]) --> num(BR).
gst_message(position, [position(BR)]) --> num(BR).
gst_message(duration, [duration(D)]) --> num(D).
gst_message(format, [format(Rate:Fmt:Ch)]) --> split_on_colon([nat(Rate), sample_fmt(Fmt), nat(Ch)]).
sample_fmt(f) --> "F", !, arb.
sample_fmt(N) --> [_], nat(N), ([]; any(`LB_`), arb).

set_volume(V) :- FV is (V/100.0)^1.5, send(fmt('volume ~5f', [FV])).
gst_volume(V) :- send(fmt('volume ~f',V)).
gst_uri(URI) :- send(fmt('uri ~s',[URI])).
send(P) :- gst(_,In), phrase(P, Codes), format(In, '~s\n', [Codes]).
recv(M) :- gst(Id,_), thread_get_message(Id, M, [timeout(2)]).

start_gst_thread(V) :- spawn(with_gst(gst_reader_thread(V), _)).

split_on_colon(Ps) --> seqmap_with_sep(`:`, broken(`:`), Ps).
broken(Cs, P) --> break(Cs) // P.

gst_audio_info(Id, au(Dur, Elap1, BR1, Fmt1), au(FDur, Elap, BR, Fmt)) :-
   maplist(send, ["bitrate", "format", "position"]), FDur is float(Dur),
   (thread_get_message(Id, bitrate(BR), [timeout(1)]) -> true; BR=BR1),
   (thread_get_message(Id, format(Fmt), [timeout(1)]) -> true; Fmt=Fmt1),
   (thread_get_message(Id, position(Elap), [timeout(1)]) -> true; Elap=Elap1).

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
