:- module(mpd_protocol, [mpd_interactor/0, execute/3, notify_all/1]).

:- use_module(library(insist), [insist/1]).
:- use_module(library(dcg_core), [seqmap_with_sep//3]).
:- use_module(library(dcg_pair)).
:- use_module(tools, [in/2, quoted//2, atom//1, report//2, parse_head//2, registered/2, thread/2]).

:- multifile command//2.

%! mpd_interactor is det.
% Run MPD client interaction using the current input and output Prolog streams.
mpd_interactor :-
   writeln('OK MPD 0.20.0'), thread_self(Self),
   setup_call_cleanup(thread_create(client, Id, [at_exit(thread_signal(Id, throw(kill)))]),
                      transduce(Id), cleanup_client(Id)).

client :- catch(registered(client, normal_wait([])), Ex, print_message(error, Ex)).
cleanup_client(Id) :- catch(thread_send_message(Id, eos), _, true), thread_join(Id, _).
transduce(Q)       :- read_command(Cmd), handle_cmd(Cmd, Q).
handle_cmd(cmd(Head, Tail), Q) :- thread_send_message(Q, cmd(Head, Tail)), transduce(Q).
handle_cmd(eos, _).

read_command(Cmd) :-
   read_line_to_codes(current_input, Codes),
   debug(mpd(command), ">> `~s`", [Codes]),
   (  Codes = end_of_file -> Cmd = eos
   ;  insist(parse_head(Head, Tail, Codes, [])), Cmd = cmd(Head,Tail)
   ).

get_message(M) :- thread_self(Self), thread_get_message(Self, M, [timeout(120)]).
normal_wait(Pending) :- get_message(M), normal_msg(M, Pending).
normal_msg(changed(S), Pending) :- normal_wait([S|Pending]).
normal_msg(cmd(Head, Tail), Pending) :- handle(Head, Tail, Pending).
normal_msg(eos, _).

handle(close,  [], _) :- !.
handle(command_list_ok_begin, [], Pending) :- !, command_list(list_ok, Pending).
handle(command_list_begin, [], Pending)    :- !, command_list(silent, Pending).
handle(noidle, [], Pending) :- !, normal_wait(Pending).
handle(idle, Tail, Pending) :- !,
   once(phrase(idle_filter(Filter), Tail)),
   idle(Pending, Filter).
handle(Head, Tail, Pending) :- do_and_cont(execute(0-'', Head, Tail), Pending).

idle_filter(nonvar) --> [].
idle_filter(in(Subsystems)) --> " ", seqmap_with_sep(" ", quoted(atom), Subsystems).

% --- command lists ---
command_list(SubReply, Pending1) :-
   accum(Commands-Pending2, []-Pending1),
   do_and_cont(execute_list(Commands, 0, SubReply), Pending2).

execute_list([], _, _).
execute_list([Head-Tail|Cmds], Pos, Reply) :-
   execute(Pos-Head, Head, Tail),
   sub_reply(Reply), succ(Pos, Pos1),
   execute_list(Cmds, Pos1, Reply).

accum --> {get_message(Msg)}, accum_msg(Msg).
accum_msg(changed(S)) --> \> [S], accum.
accum_msg(cmd(Head,Tail)) --> accum_cont(Head, Tail).
accum_cont(command_list_begin, _) --> {throw(protocol_fail)}.
accum_cont(command_list_ok_begin, _) --> {throw(protocol_fail)}.
accum_cont(command_list_end, []) --> !, [].
accum_cont(Head, Tail) --> \< [Head-Tail], accum.

sub_reply(silent).
sub_reply(list_ok) :- fmt_reply('list_OK', []).

reply_phrase(P) :-
   phrase(P, Codes), format('~s', [Codes]),
   debug(mpd(reply), '<< ~s|', [Codes]).

reply(ok) :- fmt_reply('OK', []).
reply(ack(Pos-SubCmd, err(Code, Fmt, Args))) :-
   fmt_reply('ACK [~d@~d] {~s} ~@', [Code, Pos, SubCmd, format(Fmt, Args)]).

fmt_reply(Fmt, Args) :-
   debug(mpd(reply), '~@', format(Fmt, Args)),
   format(Fmt, Args), nl.

% -- command execution --
do_and_cont(G, Pending) :-
   insist(catch((G, Reply=ok), mpd_ack(Ack), Reply=Ack)),
   reply(Reply), normal_wait(Pending).

execute(Ref, Cmd, T) :-
   (  catch(reply_phrase(command(Cmd, T)), mpd_err(Err), throw(mpd_ack(ack(Ref, Err)))) -> true
   ;  throw(mpd_ack(ack(Ref, err(99, 'Failed on ~s', [Cmd]))))
   ).

% -- notification system ---
idle(Pending, Filter) :-
   partition(Filter, Pending, ToReport, ToIgnore),
   (  ToReport=[] -> idle_wait(Filter, ToIgnore)
   ;  report_changes(ToReport), normal_wait(ToIgnore)
   ).

idle_wait(Filter, ToIgnore) :- thread_get_message(Msg), idle_msg(Msg, Filter, ToIgnore).
idle_msg(eos, _, _).
idle_msg(cmd(noidle,[]), _, ToIgnore) :- reply(ok), normal_wait(ToIgnore).
idle_msg(changed(S), Filter, ToIgnore) :-
   (  call(Filter, S) -> report_changes([S]), normal_wait(ToIgnore)
   ;  idle_wait(Filter, [S|ToIgnore])
   ).

report_changes(L) :- sort(L, L1), reply_phrase(foldl(report(changed), L1)), reply(ok).
notify_all(Subsystems) :- forall(thread(client, Id), maplist(notify(Id), Subsystems)).
notify(Id, Subsystem) :- thread_send_message(Id, changed(Subsystem)).
