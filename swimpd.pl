user:file_search_path(python, 'python').
user:file_search_path(bbc, 'prolog').

:- set_prolog_flag(optimise_debug, false). % only works on SWI 8 and above

:- use_module(library(apply_macros)).
:- use_module(library(dcg_macros)). % NB. import into user means these apply globally

:- use_module(prolog/swimpd/telnetd,  [telnet_server/3]).
:- use_module(prolog/swimpd/protocol, [mpd_interactor/0, execute_string/1]).
:- use_module(prolog/swimpd/gst,      [start_gst_thread/0]).
:- use_module(prolog/swimpd/commands, [mpd_init/0, restore_state/1, save_state/1]).
:- use_module(prolog/swimpd/state,    [attach/1, sync_state/0]).
:- use_module(prolog/swimpd/tools,    [thread/2]).

swimpd(async) :- thread_create(main, _, [detached(true), alias(mpd_server)]).
swimpd(sync)  :- on_signal(term, _, kill_server), main.

kill_server(Signal) :-
   debug(mpd(swimpd,s(s(0))), 'Killing swimpd server on signal ~w', [Signal]),
   throw(shutdown).

kill_satellites :-
   forall(thread(client, Id), thread_signal(Id, throw(hangup))),
   thread_signal(gst_slave, throw(shutdown)),
   thread_join(gst_slave, _).

local(ip(127,0,0,1)).
local(ip(192,168,1,_)).

main :-
   writeln("<<< Starting SWIMPD - BBC Music Player Daemon >>>"),
   current_prolog_flag(argv, [PortAtom, StateFile | DebugTopics]),
   forall(member(A, DebugTopics), (atom_to_term(A,T,[]), debug(T))),
   atom_number(PortAtom, Port), attach(StateFile), sync_state, mpd_init,
   setup_call_cleanup(start_gst_thread,
                      telnet_server(mpd_interactor, Port, [allow(local)]),
                      kill_satellites).

% vim: ft=prolog
