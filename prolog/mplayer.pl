% MPlayer process handling
% See http://www.mplayerhq.hu/DOCS/tech/slave.txt for MPlayer slave mode commands
:- module(mplayer, [mplayer/2]).

:- use_module(library(typedef)).

:- type pid == ground.
:- type stream == \is_stream.
:- type process ---> proc(pid,stream,stream).

%% mplayer(+File:path, -Status) is det.
%  Runs MPlayer in slave mode on given file, sending commands from user_input through
%  to MPlayer and printing output from MPlayer on user_output.
mplayer(File, Status) :- mplayer(relay_user_io, File, Status).
mplayer(Manager, File, Status) :-
   setup_call_cleanup(start_mplayer(File, proc(PID,In,Out)),
                      manage_mplayer(Manager, PID, In, Out),
                      process_wait(PID, Status)).

%% start_mplayer( +F:filename, -P:processs) is det.
start_mplayer(File, proc(PID,In,Out)) :-
   process_create(path(mplayer), ['-slave','-quiet',File],
                  [stdin(pipe(In)), stdout(pipe(Out)), stderr(null), process(PID)]),
   set_stream(In, close_on_abort(false)),
   set_stream(Out, close_on_abort(false)).

manage_mplayer(Manager, PID, In, Out) :-
   catch(call(Manager, PID, In, Out), Ex, (process_kill(PID), throw(Ex))).

relay_user_io(PID, In, Out) :- prompt(_, ''), handle_events(PID, In, Out).
handle_events(PID, In, Out) :-
   wait_for_input([user_input,Out], [R|_], infinite),
   nth1(K, [user_input,Out], R),
   handle_event_from(K, PID, In, Out).

handle_event_from(1, PID, In, Out) :-
   read_line_to_codes(current_input, Codes),
   (  Codes=end_of_file  -> process_kill(PID)
   ;  format(In, '~s\n', [Codes]), flush_output(In),
      handle_events(PID, In, Out)
   ).

handle_event_from(2, PID, In, Out) :-
   read_line_to_codes(Out, Codes),
   (  Codes=end_of_file -> true
   ;  format(current_output, '~s\n', [Codes]), flush_output,
      handle_events(PID, In, Out)
   ).
