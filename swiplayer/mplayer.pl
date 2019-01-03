% MPlayer process handling
% See http://www.mplayerhq.hu/DOCS/tech/slave.txt for MPlayer slave mode commands

:- use_module(library(fileutils)).
:- use_module(library(typedef)).

:- type pid == ground.
:- type stream == \is_stream.
:- type process ---> proc(pid,stream,stream).

:- dynamic pipes/2.

file_search_path(music,'~/Music').

%% mplayer(+File:path, -Status) is det.
%  Runs MPlayer in slave mode on given file, sending commands from user_input through
%  to MPlayer and printing output from MPlayer on user_output.
mplayer(File,Status) :-
   setup_call_cleanup( start_mplayer(File,proc(PID,In,Out)),
                       manage_mplayer(PID,In,Out),
                       process_wait(PID,Status)).

%% music_file(+Spec:filespec, -F:path) is nondet.
% Succeeds once for each music file under given path spec.
music_file(Spec,File) :-
   find_files(under(Spec,'*.{ogg,mp3,aac}'),File).

%% start_mplayer( +F:filename, -P:processs) is det.
start_mplayer(File,proc(PID,In,Out)) :-
   process_create(path(mplayer),['-slave','-quiet',File],
                  [stdin(pipe(In)),stdout(pipe(Out)),stderr(null),process(PID)]).

manage_mplayer(PID,In,Out) :-
  set_stream(In,close_on_abort(false)),
  set_stream(Out,close_on_abort(false)),
  catch(handle_events(PID,In,Out), Ex,
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
