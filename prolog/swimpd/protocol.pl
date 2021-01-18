:- module(mpd_protocol, [mpd_interactor/0, notify_all/1, reply_binary/4, execute_string/1]).

:- use_module(library(insist), [insist/1]).
:- use_module(library(dcg_core), [seqmap_with_sep//3]).
:- use_module(library(dcg_pair)).
:- use_module(tools, [in/2, quoted//2, atom//1, report//1, report//2, parse_head//2, registered/2, thread/2]).

:- multifile command//3, all_tags/1.

%! mpd_interactor is det.
% Run MPD client interaction using the current input and output Prolog streams.
mpd_interactor :-
   output("OK MPD 0.21.26"), thread_self(Self),
   setup_call_cleanup(thread_create(client, Id, [at_exit(thread_signal(Self, throw(kill_transducer)))]),
                      catch(transduce(Id), kill_transducer, true), cleanup_client(Id)).

client :- all_tags(AllTags), registered(client, normal_wait(AllTags-[])).
cleanup_client(Id) :- catch(thread_send_message(Id, kill_client), _, true), thread_join(Id, _).
transduce(Q)       :- read_command(Cmd), thread_send_message(Q, Cmd), transduce(Q).

read_command(cmd(Head, Tail)) :-
   read_line_to_codes(current_input, Codes),
   debug(mpd(command), ">> ~s", [Codes]),
   insist(parse_head(Head, Tail, Codes, [])).

execute_string(Cmd) :-
   string_codes(Cmd, Codes),
   insist(parse_head(H, T, Codes, [])),
   execute(0-'', H, T, Reply),
   reply(Reply).

get_message(M) :- thread_self(Self), thread_get_message(Self, M, [timeout(120)]).
normal_wait(State) :- get_message(M), normal_msg(M, State).
normal_msg(changed(S), Tags-Pending) :- normal_wait(Tags-[S|Pending]).
normal_msg(cmd(Head, Tail), State) :- handle(Head, Tail, State).

handle(close,  [], _) :- !.
handle(command_list_ok_begin, [], State) :- !, command_list(list_ok, State).
handle(command_list_begin, [], State)    :- !, command_list(silent, State).
handle(noidle, [], State) :- !, normal_wait(State).
handle(idle, Tail, State) :- !,
   once(phrase(idle_filter(Filter), Tail)),
   idle(State, Filter).
handle(Head, Tail, State) :- do_and_cont(execute(0-'', Head, Tail), State).

idle_filter(nonvar) --> [].
idle_filter(in(Subsystems)) --> " ", seqmap_with_sep(" ", quoted(atom), Subsystems).

% --- command lists ---
command_list(SubReply, Tags-Pending1) :-
   accum(Commands-Pending2, []-Pending1),
   do_and_cont(execute_list(Commands, 0, SubReply), Tags-Pending2).

execute_list([], _, _, ok) --> [].
execute_list([Head-Tail|Cmds], Pos, SubReply, Reply) -->
   execute(Pos-Head, Head, Tail, Reply1),
   execute_list_tail(Reply1, Cmds, Pos, SubReply, Reply).
execute_list_tail(ack(Ref, Err), _, _, _, ack(Ref, Err)) --> [].
execute_list_tail(ok, Cmds, Pos, SubReply, Reply) -->
   {sub_reply(SubReply), succ(Pos, Pos1)},
   execute_list(Cmds, Pos1, SubReply, Reply).

accum --> {get_message(Msg)}, accum_msg(Msg).
accum_msg(changed(S)) --> \> [S], accum.
accum_msg(cmd(Head,Tail)) --> accum_cont(Head, Tail).
accum_cont(command_list_begin, _) --> {throw(protocol_fail)}.
accum_cont(command_list_ok_begin, _) --> {throw(protocol_fail)}.
accum_cont(command_list_end, []) --> !, [].
accum_cont(Head, Tail) --> \< [Head-Tail], accum.

sub_reply(silent).
sub_reply(list_ok) :- output("list_OK").

reply_phrase(P) :-
   phrase(P, Codes), format("~s", [Codes]),
   debug(mpd(reply), "<< ~s|", [Codes]).

reply(ok) :- output("OK").
reply(ack(Pos-SubCmd, err(Code, Fmt, Args))) :-
   format(string(R), "ACK [~d@~d] {~s} ~@", [Code, Pos, SubCmd, format(Fmt, Args)]),
   output(R).

output(R) :-
   debug(mpd(command), "<< ~s", [R]),
   write(R), nl, flush_output.

reply_binary(Type, Total, Size, Stream) :-
   reply_phrase(foldl(report, [size-Total, type-Type, binary-Size])),
   with_binary_output(current_output, copy_stream_data(Stream, current_output)).
with_binary_output(S, G) :- setup_call_cleanup(set_stream(S, type(binary)), G, set_stream(S, type(text))).

% -- command execution --
:- meta_predicate do_and_cont(3,+).
do_and_cont(G, Tags1-Pending) :- call(G, Reply, Tags1, Tags2), reply(Reply), normal_wait(Tags2-Pending).
execute(_, Head, Tail, ok, T, T) :- reply_phrase(command(Head, Tail, none)), !.
execute(_, Head, Tail, ok, T, T) :- reply_phrase(command(Head, Tail, do(G))), !, call(G).
execute(_, Head, Tail, ok, T1, T2) :- reply_phrase(command(Head, Tail, tags(T1,T2))), !.
execute(Ref, Head, _, ack(Ref, err(99, 'Failed on ~s', [Head]))) --> [].

% -- notification system ---
idle(Tags-Pending, Filter) :-
   partition(Filter, Pending, ToReport, ToIgnore),
   (  ToReport=[] -> idle_wait(Filter, Tags-ToIgnore)
   ;  report_changes(ToReport), normal_wait(Tags-ToIgnore)
   ).

idle_wait(Filter, Tags-ToIgnore) :- thread_get_message(Msg), idle_msg(Msg, Filter, Tags-ToIgnore).
idle_msg(cmd(noidle,[]), _, State) :- reply(ok), normal_wait(State).
idle_msg(changed(S), Filter, Tags-ToIgnore) :-
   (  call(Filter, S) -> report_changes([S]), normal_wait(Tags-ToIgnore)
   ;  idle_wait(Filter, Tags-[S|ToIgnore])
   ).

report_changes(L) :- sort(L, L1), reply_phrase(foldl(report(changed), L1)), reply(ok).
notify_all(Subsystems) :- forall(thread(client, Id), maplist(notify(Id), Subsystems)).
notify(Id, Subsystem) :- thread_send_message(Id, changed(Subsystem)).
