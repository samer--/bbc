:- module(mpd_protocol, [mpd_interactor/0, execute/2, notify_all/1]).

:- use_module(library(insist), [insist/1]).
:- use_module(library(dcg_core), [seqmap_with_sep//3]).
:- use_module(asyncu, [registered/2, thread/2]).
:- use_module(tools,  [in/2, quoted//2, atom//1, report//2, parse_head//2]).

:- multifile command//2.

%! mpd_interactor is det.
% Run MPD client interaction using the current input and output Prolog streams.
mpd_interactor :- writeln('OK MPD 0.20.0'), registered(client, wait_for_input).
wait_for_input :- read_command(Cmd), !, handle(Cmd).

read_command(Cmd) :-
   read_line_to_codes(current_input, Cmd),
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
   once(phrase(idle_filter(Filter), Tail)),
   thread_self(Self),
   current_output(Out),
   setup_call_cleanup(thread_create(with_output_to(Out, listener(Self-Filter, [])), Id, []),
                      read_command(Cmd), cleanup_listener(Cmd, Self,Id, NextCmd)),
   throw(exec(NextCmd)).
execute(Cmd, T) :- execute1(0-'', Cmd, T).

idle_filter(nonvar) --> [].
idle_filter(in(Subsystems)) --> " ", seqmap_with_sep(" ", quoted(atom), Subsystems).

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
   message_queue_property(Id, size(N)), length(Msgs, N),
   maplist(thread_get_message(Id), Msgs),
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
   reply_phrase(foldl(report(changed), ToReport)), reply(ok),
   listener_tail_wait(Id, ToPutBack).

listener_tail_wait(Id, ToPutBack) :-
   thread_get_message(Id, Msg),
   listener_tail_msg(Msg, Id, ToPutBack).

listener_tail_msg(broken, _, _) :- throw(broken).
listener_tail_msg(changed(S), Id, ToPutBack) :- listener_tail_wait(Id, [S|ToPutBack]).
listener_tail_msg(cmd(Cmd), Id, ToPutBack) :- maplist(notify(Id), ToPutBack), throw(cmd(Cmd)).

cleanup_listener(Cmd, Self, Id, NextCommand) :-
   (var(Cmd) -> Msg = broken; Msg = cmd(Cmd)),
   thread_send_message(Self, Msg),
   thread_join(Id, Status),
   insist(Status = exception(cmd(NextCommand))).

notify_all(Subsystems) :- forall(thread(client, Id), maplist(notify(Id), Subsystems)).
notify(Id, Subsystem) :- thread_send_message(Id, changed(Subsystem)).
