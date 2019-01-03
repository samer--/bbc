% Player process handling

:- use_module(library(fileutils)).
:- use_module(library(typedef)).
% :- use_module(library(process)).

:- type pid == ground.
:- type stream == \is_stream.
:- type process ---> proc(pid,stream,stream).

:- dynamic pipes/2.

file_search_path(music,'~/Music').

music_file(Spec,File) :-
   find_files(under(Spec,,'*.{ogg,mp3,aac}'),File).

%% mplayer( +F:filename, -P:processs) is det.
mplayer(File,proc(PID,In,Out)) :-
   process_create(path(mplayer),['-slave','-quiet',File],
                  [stdin(pipe(In)),stdout(pipe(Out)),stderr(null),process(PID)]).

run_mplayer(PID,In,Out) :-
  set_stream(In,close_on_abort(false)),
  set_stream(Out,close_on_abort(false)),
  catch( handle_events(PID,In,Out), Ex,
         (process_kill(PID),throw(Ex))).

handle_events(PID,In,Out) :-
  wait_for_input([user_input,Out],[R|_],infinite),
  nth1(K,[user_input,Out],R),
  handle_event_from(K,PID,In,Out).

handle_event_from(1,PID,In,Out) :-
   get_single_char(C),
   format('Got >~w<\n',[C]),
   (  C=end_of_file        % end of user input
   -> process_kill(PID)    % tell mplayer to quit
   ;  put_char(In,C),      % otherwise relay characters to mplayer
      flush_output(In),
      handle_events(PID,In,Out)
   ).

handle_event_from(2,PID,In,Out) :-
   get_char(Out,C),
   (  C=end_of_file % mplayer stopped running
   -> writeln('mplayer terminated')          % finished
   ;  put_char(current_output,C),
      handle_events(PID,In,Out)
   ).

with_mplayer(File,Status) :-
   setup_call_cleanup( mplayer(File,proc(PID,In,Out)),
                       run_mplayer(PID,In,Out),
                       process_wait(PID,Status)).

