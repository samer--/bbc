--------------------------- MODULE SwimpdGst ---------------------------
(***************************************************************************)
(* TLA+ / PlusCal specification of the Prolog MPD server <-> Python      *)
(* GStreamer slave bidirectional protocol.                                  *)
(*                                                                          *)
(* Abstractions:                                                            *)
(*   - The 0.5s Python uri/load sleep is one atomic "Py_Loading" step.    *)
(*   - The 2s Prolog recv_position timeout is a nondeterministic choice.  *)
(*   - Only key commands/events are modeled.                                *)
(*   - The swimpd mutex is held across MPD_WaitPos, modeling the real     *)
(*     bug where excl/1 wraps the entire status+position poll.              *)
(*                                                                          *)
(* TO USE: paste into TLA+ Toolbox, run PlusCal translation, then run TLC.  *)
(* Recommended model bounds:                                                *)
(*   - Set pipe_p2g, pipe_g2p, gst_mailbox max sequence lengths to ~3-5.    *)
(*   - Symmetry on commands/events if TLC complains about infinite sets.  *)
(***************************************************************************)

EXTENDS Integers, Sequences, FiniteSets, TLC

CONSTANTS COMMANDS, EVENTS

(* Set of commands Prolog sends to Python via stdin pipe *)
ASSUME COMMANDS \subseteq {"play", "pause", "stop", "uri", "position"}

(* Set of events Python sends to Prolog via stdout pipe *)
ASSUME EVENTS \subseteq {"state_null", "state_paused", "state_playing",
                          "position_42", "eos"}

-----------------------------------------------------------------------------
(* PlusCal algorithm                                                          *)

(* --algorithm SwimpdGst

variables
  swimpd_mutex = "free";
  pipe_p2g = << >>;
  pipe_g2p = << >>;
  gst_mailbox = << >>;
  prolog_player = "nothing";
  python_pipeline = "null";
  prolog_position = -1;
  pending_pos_req = FALSE;
  python_busy = FALSE;
  gst_next_event = "";

define
  TypeInvariant ==
    /\ swimpd_mutex \in {"free", "MPD", "GST"}
    /\ prolog_player \in {"nothing", "paused", "playing"}
    /\ python_pipeline \in {"null", "paused", "playing", "loading"}
    /\ prolog_position \in Int
    /\ pending_pos_req \in BOOLEAN
    /\ python_busy \in BOOLEAN
    /\ gst_next_event \in {"", "state_null", "state_paused",
                            "state_playing", "position_42", "eos"}
    /\ pipe_p2g \in Seq(COMMANDS)
    /\ pipe_g2p \in Seq(EVENTS)
    /\ gst_mailbox \in Seq(Int)

  MutexSafety ==
    Cardinality({p \in {"MPD", "GST"} : swimpd_mutex = p}) <= 1

  (* Temporal property expected to FAIL because of the state desync bug.     *)
  (* If the system is quiescent, the two sides should agree on player state. *)
  QuiescentConsistent ==
    /\ Len(pipe_p2g) = 0
    /\ Len(pipe_g2p) = 0
    /\ ~python_busy
    /\ pending_pos_req = FALSE
    /\ gst_next_event = ""
    => (  (prolog_player = "nothing" /\ python_pipeline = "null")
       \/ (prolog_player = "paused" /\ python_pipeline = "paused")
       \/ (prolog_player = "playing" /\ python_pipeline = "playing"))
end define;

macro send_to_python(cmd)
begin
  pipe_p2g := Append(pipe_p2g, cmd);
end macro;

macro send_to_prolog(evt)
begin
  pipe_g2p := Append(pipe_g2p, evt);
end macro;

macro acquire_mutex(who)
begin
  await swimpd_mutex = "free";
  swimpd_mutex := who;
end macro;

macro release_mutex
begin
  swimpd_mutex := "free";
end macro;

(* ======================================================================== *)
(* MPD Command Executor (Prolog thread)                                     *)
(* Models one MPD client handler that can issue state-mutating commands.    *)
(* The mutex is acquired before work and released after.                    *)
(* For the position poll, the mutex is intentionally held during the wait   *)
(* to reproduce the real-code deadlock risk.                                *)
(* ======================================================================== *)

fair process MPD = "MPD"
begin
MPD_Loop:
  either
    (* Command: play — update state and tell Python to start playback.      *)
    acquire_mutex("MPD");
    prolog_player := "playing";
    send_to_python("play");
    release_mutex;
    goto MPD_Loop;

  or
    (* Command: stop — clear player state and tell Python to stop.          *)
    acquire_mutex("MPD");
    prolog_player := "nothing";
    send_to_python("stop");
    release_mutex;
    goto MPD_Loop;

  or
    (* Command: uri — cue a new stream.                                     *)
    (* BUG: Prolog immediately claims "playing" before Python finishes load.*)
    acquire_mutex("MPD");
    prolog_player := "playing";
    send_to_python("uri");
    release_mutex;
    goto MPD_Loop;

  or
    (* Command: status + position poll.                                     *)
    (* In the real code, with_mutex(swimpd, Goal) wraps the ENTIRE           *)
    (* gst_audio_info/2 call, so the mutex is held while waiting.            *)
    acquire_mutex("MPD");
    pending_pos_req := TRUE;
    send_to_python("position");
    goto MPD_WaitPos;
  end either;

MPD_WaitPos:
  either
    (* Receive position reply from GST Reader mailbox.                        *)
    await Len(gst_mailbox) > 0;
    prolog_position := Head(gst_mailbox);
    gst_mailbox := Tail(gst_mailbox);
    pending_pos_req := FALSE;
    release_mutex;
    goto MPD_Loop;

  or
    (* Timeout — abstracted as always-possible nondeterministic branch.      *)
    (* In reality this fires after ~2 seconds of blocking.                  *)
    pending_pos_req := FALSE;
    release_mutex;
    goto MPD_Loop;
  end either;
end process;

(* ======================================================================== *)
(* GST Reader (Prolog thread)                                                *)
(* Reads events from Python stdout one at a time. If the event requires     *)
(* updating persistent state (sync_player_state), it must acquire swimpd.   *)
(* If the mutex is held by MPD, GST blocks at GST_Handle with the event      *)
(* already popped from the pipe — modeling the real deadlock window.       *)
(* ======================================================================== *)

fair process GST = "GSTReader"
begin
GST_Read:
  await Len(pipe_g2p) > 0;
  gst_next_event := Head(pipe_g2p);
  pipe_g2p := Tail(pipe_g2p);

GST_Handle:
  if gst_next_event = "position_42" then
    (* No mutex needed — just drops into the mailbox for MPD to collect.    *)
    gst_mailbox := Append(gst_mailbox, 42);
    gst_next_event := "";
    goto GST_Read;

  elsif gst_next_event = "state_null" then
    (* sync_player_state(null, _) — aggressively resets to nothing.         *)
    acquire_mutex("GST");
    prolog_player := "nothing";
    release_mutex;
    gst_next_event := "";
    goto GST_Read;

  elsif gst_next_event = "state_paused" then
    (* sync_player_state(paused, nothing)                                   *)
    acquire_mutex("GST");
    if prolog_player = "nothing" then
      prolog_player := "paused";
    end if;
    release_mutex;
    gst_next_event := "";
    goto GST_Read;

  elsif gst_next_event = "state_playing" then
    (* FALL-THROUGH BUG: real code has no clause for this case.            *)
    (* It logs "desync" and does nothing.                                   *)
    gst_next_event := "";
    goto GST_Read;

  elsif gst_next_event = "eos" then
    (* notify_eos — updates queue/player state, needs mutex.                  *)
    acquire_mutex("GST");
    prolog_player := "nothing";
    release_mutex;
    gst_next_event := "";
    goto GST_Read;

  else
    gst_next_event := "";
    goto GST_Read;
  end if;
end process;

(* ======================================================================== *)
(* Python Main Thread                                                        *)
(* Reads commands from stdin pipe sequentially. The "uri" command enters    *)
(* a loading sub-state (python_busy) during which no new commands are read. *)
(* ======================================================================== *)

fair process PyMain = "PythonMain"
begin
Py_Loop:
  await Len(pipe_p2g) > 0 /\ ~python_busy;
  with cmd = Head(pipe_p2g) do
    pipe_p2g := Tail(pipe_p2g);
    if cmd = "play" then
      python_pipeline := "playing";
      send_to_prolog("state_playing");
    elsif cmd = "pause" then
      python_pipeline := "paused";
      send_to_prolog("state_paused");
    elsif cmd = "stop" then
      python_pipeline := "null";
      send_to_prolog("state_null");
    elsif cmd = "position" then
      send_to_prolog("position_42");
    elsif cmd = "uri" then
      python_busy := TRUE;
      python_pipeline := "loading";
      send_to_prolog("state_null");
    end if;
  end with;
  if python_busy then
    goto Py_Loading;
  else
    goto Py_Loop;
  end if;

Py_Loading:
  (* Abstracts: stop() -> set URI -> sleep 0.5s -> pause() -> sync()     *)
  python_pipeline := "paused";
  send_to_prolog("state_paused");
  python_busy := FALSE;
  goto Py_Loop;
end process;

(* ======================================================================== *)
(* Python Bus Thread (GStreamer bus message daemon)                          *)
(* Asynchronously emits events. We only model "eos" here.                   *)
(* ======================================================================== *)

fair process PyBus = "PythonBus"
begin
PyBus_Loop:
  either
    await python_pipeline \in {"playing", "paused", "loading"};
    send_to_prolog("eos");
    goto PyBus_Loop;
  or
    goto PyBus_Loop;
  end either;
end process;

end algorithm *)

=============================================================================
