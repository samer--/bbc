--------------------------- MODULE SwimpdGstPure ---------------------------
(***************************************************************************)
(* Pure TLA+ specification of the Prolog MPD server <-> Python            *)
(* GStreamer slave bidirectional protocol.                                  *)
(*                                                                          *)
(* This is the TLC-checkable version. It models four processes, two pipes,    *)
(* and a shared mutex. The mutex is modeled explicitly only for the           *)
(* MPD position poll (held across WaitPos) and as a precondition for           *)
(* GST state-updating actions.                                                *)
(***************************************************************************)

EXTENDS Integers, Sequences, FiniteSets, TLC

CONSTANTS COMMANDS, EVENTS

ASSUME COMMANDS \subseteq {"play", "pause", "stop", "uri", "position"}
ASSUME EVENTS \subseteq {"state_null", "state_paused", "state_playing",
                          "position_42", "eos"}

-----------------------------------------------------------------------------

VARIABLES
  swimpd_mutex,      (* "free", "MPD", or "GST" *)
  pipe_p2g,          (* FIFO: Prolog -> Python commands *)
  pipe_g2p,          (* FIFO: Python -> Prolog events *)
  gst_mailbox,       (* Prolog GST Reader internal message queue *)
  pc_mpd,            (* Program counter for MPD process *)
  pc_gst,            (* Program counter for GST Reader process *)
  pc_pymain,         (* Program counter for Python Main process *)
  pc_pybus,          (* Program counter for Python Bus process *)
  prolog_player,     (* Prolog's view: "nothing", "paused", "playing" *)
  python_pipeline,   (* Python's actual GStreamer state *)
  prolog_position,   (* Cached position, -1 = unknown *)
  pending_pos_req,   (* TRUE while MPD awaits a position response *)
  python_busy,       (* TRUE while Python is in the 0.5s load sleep *)
  gst_next_event     (* Event GST has popped from pipe_g2p but not yet handled *)

vars == << swimpd_mutex, pipe_p2g, pipe_g2p, gst_mailbox,
           pc_mpd, pc_gst, pc_pymain, pc_pybus,
           prolog_player, python_pipeline, prolog_position,
           pending_pos_req, python_busy, gst_next_event >>

-----------------------------------------------------------------------------
(* Type invariant *)
TypeInvariant ==
  /\ swimpd_mutex \in {"free", "MPD", "GST"}
  /\ pc_mpd \in {"MPD_Loop", "MPD_WaitPos"}
  /\ pc_gst \in {"GST_Read", "GST_Handle"}
  /\ pc_pymain \in {"Py_Loop", "Py_Loading"}
  /\ pc_pybus \in {"PyBus_Loop"}
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

(* Mutex safety: at most one thread holds the mutex *)
MutexSafety ==
  Cardinality({p \in {"MPD", "GST"} : swimpd_mutex = p}) <= 1

(* Temporal property expected to FAIL because of the state desync bug.     *)
(* If the system is quiescent (no pending commands/events, not loading),    *)
(* the two sides should agree on player state.                              *)
QuiescentConsistent ==
  /\ Len(pipe_p2g) = 0
  /\ Len(pipe_g2p) = 0
  /\ ~python_busy
  /\ pending_pos_req = FALSE
  /\ gst_next_event = ""
  => (  (prolog_player = "nothing" /\ python_pipeline = "null")
     \/ (prolog_player = "paused" /\ python_pipeline = "paused")
     \/ (prolog_player = "playing" /\ python_pipeline = "playing"))

(* State constraint for TLC: bound sequence lengths *)
StateConstraint ==
  /\ Len(pipe_p2g) <= 5
  /\ Len(pipe_g2p) <= 5
  /\ Len(gst_mailbox) <= 5

(* Tighter bound for exhaustive (i.e. expected-pass) checks *)
SoundnessConstraint ==
  /\ Len(pipe_p2g) <= 2
  /\ Len(pipe_g2p) <= 2
  /\ Len(gst_mailbox) <= 1

-----------------------------------------------------------------------------
(*                              MPD Handler                                *)
(*  Simple commands: check mutex is free, do work atomically, leave free.   *)
(*  For position poll: acquire mutex in StatusPoll, hold across WaitPos,    *)
(*  release in RecvPos or Timeout.                                            *)

(* Command: play — update state and tell Python to start playback.        *)
MPD_Play ==
  /\ pc_mpd = "MPD_Loop"
  /\ swimpd_mutex = "free"
  /\ prolog_player' = "playing"
  /\ pipe_p2g' = Append(pipe_p2g, "play")
  /\ pc_mpd' = "MPD_Loop"
  /\ UNCHANGED << swimpd_mutex, pipe_g2p, gst_mailbox, pc_gst, pc_pymain,
                  pc_pybus, python_pipeline, prolog_position,
                  pending_pos_req, python_busy, gst_next_event >>

(* Command: stop — clear player state and tell Python to stop.             *)
MPD_Stop ==
  /\ pc_mpd = "MPD_Loop"
  /\ swimpd_mutex = "free"
  /\ prolog_player' = "nothing"
  /\ pipe_p2g' = Append(pipe_p2g, "stop")
  /\ pc_mpd' = "MPD_Loop"
  /\ UNCHANGED << swimpd_mutex, pipe_g2p, gst_mailbox, pc_gst, pc_pymain,
                  pc_pybus, python_pipeline, prolog_position,
                  pending_pos_req, python_busy, gst_next_event >>

(* Command: uri — cue a new stream.                                        *)
(* BUG: Prolog immediately claims "playing" before Python finishes load.   *)
MPD_Uri ==
  /\ pc_mpd = "MPD_Loop"
  /\ swimpd_mutex = "free"
  /\ prolog_player' = "playing"
  /\ pipe_p2g' = Append(pipe_p2g, "uri")
  /\ pc_mpd' = "MPD_Loop"
  /\ UNCHANGED << swimpd_mutex, pipe_g2p, gst_mailbox, pc_gst, pc_pymain,
                  pc_pybus, python_pipeline, prolog_position,
                  pending_pos_req, python_busy, gst_next_event >>

(* Command: status + position poll.                                         *)
(* Acquire mutex here; hold it across the wait.                             *)
MPD_StatusPoll ==
  /\ pc_mpd = "MPD_Loop"
  /\ swimpd_mutex = "free"
  /\ swimpd_mutex' = "MPD"
  /\ pending_pos_req' = TRUE
  /\ pipe_p2g' = Append(pipe_p2g, "position")
  /\ pc_mpd' = "MPD_WaitPos"
  /\ UNCHANGED << pipe_g2p, gst_mailbox, pc_gst, pc_pymain, pc_pybus,
                  prolog_player, python_pipeline, prolog_position,
                  python_busy, gst_next_event >>

(* MPD receives position reply from mailbox.                                 *)
MPD_RecvPos ==
  /\ pc_mpd = "MPD_WaitPos"
  /\ Len(gst_mailbox) > 0
  /\ prolog_position' = Head(gst_mailbox)
  /\ gst_mailbox' = Tail(gst_mailbox)
  /\ pending_pos_req' = FALSE
  /\ swimpd_mutex' = "free"
  /\ pc_mpd' = "MPD_Loop"
  /\ UNCHANGED << pipe_p2g, pipe_g2p, pc_gst, pc_pymain, pc_pybus,
                  prolog_player, python_pipeline, python_busy,
                  gst_next_event >>

(* MPD times out waiting for position.                                       *)
MPD_Timeout ==
  /\ pc_mpd = "MPD_WaitPos"
  /\ pending_pos_req' = FALSE
  /\ swimpd_mutex' = "free"
  /\ pc_mpd' = "MPD_Loop"
  /\ UNCHANGED << pipe_p2g, pipe_g2p, gst_mailbox, pc_gst, pc_pymain,
                  pc_pybus, prolog_player, python_pipeline, prolog_position,
                  python_busy, gst_next_event >>

MPD == MPD_Play \/ MPD_Stop \/ MPD_Uri \/ MPD_StatusPoll \/ MPD_RecvPos \/ MPD_Timeout

-----------------------------------------------------------------------------
(*                           GST Reader Thread                             *)
(*  Reads events from Python stdout one at a time. State-updating actions    *)
(*  require swimpd_mutex = "free" so they do not interleave with MPD       *)
(*  holding the mutex during a position poll.                                 *)

GST_Read ==
  /\ pc_gst = "GST_Read"
  /\ Len(pipe_g2p) > 0
  /\ gst_next_event' = Head(pipe_g2p)
  /\ pipe_g2p' = Tail(pipe_g2p)
  /\ pc_gst' = "GST_Handle"
  /\ UNCHANGED << swimpd_mutex, pipe_p2g, gst_mailbox, pc_mpd, pc_pymain,
                  pc_pybus, prolog_player, python_pipeline, prolog_position,
                  pending_pos_req, python_busy >>

(* position_42: no mutex needed — just drops into mailbox for MPD to collect.*)
GST_Handle_pos ==
  /\ pc_gst = "GST_Handle"
  /\ gst_next_event = "position_42"
  /\ gst_mailbox' = Append(gst_mailbox, 42)
  /\ gst_next_event' = ""
  /\ pc_gst' = "GST_Read"
  /\ UNCHANGED << swimpd_mutex, pipe_p2g, pipe_g2p, pc_mpd, pc_pymain,
                  pc_pybus, prolog_player, python_pipeline, prolog_position,
                  pending_pos_req, python_busy >>

(* state_null: sync_player_state(null, _) — aggressively resets to nothing. *)
(* Must wait for mutex to be free (not held by MPD during position poll).   *)
GST_Handle_null ==
  /\ pc_gst = "GST_Handle"
  /\ gst_next_event = "state_null"
  /\ swimpd_mutex = "free"
  /\ prolog_player' = "nothing"
  /\ gst_next_event' = ""
  /\ pc_gst' = "GST_Read"
  /\ UNCHANGED << swimpd_mutex, pipe_p2g, pipe_g2p, gst_mailbox, pc_mpd,
                  pc_pymain, pc_pybus, python_pipeline, prolog_position,
                  pending_pos_req, python_busy >>

(* state_paused: sync_player_state(paused, nothing)                        *)
GST_Handle_paused ==
  /\ pc_gst = "GST_Handle"
  /\ gst_next_event = "state_paused"
  /\ swimpd_mutex = "free"
  /\ IF prolog_player = "nothing"
        THEN prolog_player' = "paused"
        ELSE prolog_player' = prolog_player
  /\ gst_next_event' = ""
  /\ pc_gst' = "GST_Read"
  /\ UNCHANGED << swimpd_mutex, pipe_p2g, pipe_g2p, gst_mailbox, pc_mpd,
                  pc_pymain, pc_pybus, python_pipeline, prolog_position,
                  pending_pos_req, python_busy >>

(* state_playing: FALL-THROUGH BUG — real code has no clause for this case. *)
GST_Handle_playing ==
  /\ pc_gst = "GST_Handle"
  /\ gst_next_event = "state_playing"
  /\ gst_next_event' = ""
  /\ pc_gst' = "GST_Read"
  /\ UNCHANGED << swimpd_mutex, pipe_p2g, pipe_g2p, gst_mailbox, pc_mpd,
                  pc_pymain, pc_pybus, prolog_player, python_pipeline,
                  prolog_position, pending_pos_req, python_busy >>

(* eos: notify_eos — updates queue/player state.                            *)
GST_Handle_eos ==
  /\ pc_gst = "GST_Handle"
  /\ gst_next_event = "eos"
  /\ swimpd_mutex = "free"
  /\ prolog_player' = "nothing"
  /\ gst_next_event' = ""
  /\ pc_gst' = "GST_Read"
  /\ UNCHANGED << swimpd_mutex, pipe_p2g, pipe_g2p, gst_mailbox, pc_mpd,
                  pc_pymain, pc_pybus, python_pipeline, prolog_position,
                  pending_pos_req, python_busy >>

GST == GST_Read \/ GST_Handle_pos \/ GST_Handle_null
         \/ GST_Handle_paused \/ GST_Handle_playing \/ GST_Handle_eos

-----------------------------------------------------------------------------
(*                          Python Main Thread                             *)
(*  Reads commands from stdin pipe sequentially. The "uri" command enters   *)
(*  a loading sub-state (python_busy) during which no new commands are read.  *)

Py_Read_play ==
  /\ pc_pymain = "Py_Loop"
  /\ Len(pipe_p2g) > 0
  /\ ~python_busy
  /\ Head(pipe_p2g) = "play"
  /\ python_pipeline' = "playing"
  /\ pipe_p2g' = Tail(pipe_p2g)
  /\ pipe_g2p' = Append(pipe_g2p, "state_playing")
  /\ pc_pymain' = "Py_Loop"
  /\ UNCHANGED << swimpd_mutex, gst_mailbox, pc_mpd, pc_gst, pc_pybus,
                  prolog_player, prolog_position, pending_pos_req,
                  python_busy, gst_next_event >>

Py_Read_pause ==
  /\ pc_pymain = "Py_Loop"
  /\ Len(pipe_p2g) > 0
  /\ ~python_busy
  /\ Head(pipe_p2g) = "pause"
  /\ python_pipeline' = "paused"
  /\ pipe_p2g' = Tail(pipe_p2g)
  /\ pipe_g2p' = Append(pipe_g2p, "state_paused")
  /\ pc_pymain' = "Py_Loop"
  /\ UNCHANGED << swimpd_mutex, gst_mailbox, pc_mpd, pc_gst, pc_pybus,
                  prolog_player, prolog_position, pending_pos_req,
                  python_busy, gst_next_event >>

Py_Read_stop ==
  /\ pc_pymain = "Py_Loop"
  /\ Len(pipe_p2g) > 0
  /\ ~python_busy
  /\ Head(pipe_p2g) = "stop"
  /\ python_pipeline' = "null"
  /\ pipe_p2g' = Tail(pipe_p2g)
  /\ pipe_g2p' = Append(pipe_g2p, "state_null")
  /\ pc_pymain' = "Py_Loop"
  /\ UNCHANGED << swimpd_mutex, gst_mailbox, pc_mpd, pc_gst, pc_pybus,
                  prolog_player, prolog_position, pending_pos_req,
                  python_busy, gst_next_event >>

Py_Read_position ==
  /\ pc_pymain = "Py_Loop"
  /\ Len(pipe_p2g) > 0
  /\ ~python_busy
  /\ Head(pipe_p2g) = "position"
  /\ pipe_p2g' = Tail(pipe_p2g)
  /\ pipe_g2p' = Append(pipe_g2p, "position_42")
  /\ pc_pymain' = "Py_Loop"
  /\ UNCHANGED << swimpd_mutex, gst_mailbox, pc_mpd, pc_gst, pc_pybus,
                  prolog_player, python_pipeline, prolog_position,
                  pending_pos_req, python_busy, gst_next_event >>

Py_Read_uri ==
  /\ pc_pymain = "Py_Loop"
  /\ Len(pipe_p2g) > 0
  /\ ~python_busy
  /\ Head(pipe_p2g) = "uri"
  /\ python_busy' = TRUE
  /\ python_pipeline' = "loading"
  /\ pipe_p2g' = Tail(pipe_p2g)
  /\ pipe_g2p' = Append(pipe_g2p, "state_null")
  /\ pc_pymain' = "Py_Loading"
  /\ UNCHANGED << swimpd_mutex, gst_mailbox, pc_mpd, pc_gst, pc_pybus,
                  prolog_player, prolog_position, pending_pos_req,
                  gst_next_event >>

(* Abstracts: stop -> set URI -> sleep 0.5s -> pause -> sync               *)
Py_Loading ==
  /\ pc_pymain = "Py_Loading"
  /\ python_pipeline' = "paused"
  /\ python_busy' = FALSE
  /\ pipe_g2p' = Append(pipe_g2p, "state_paused")
  /\ pc_pymain' = "Py_Loop"
  /\ UNCHANGED << swimpd_mutex, pipe_p2g, gst_mailbox, pc_mpd, pc_gst,
                  pc_pybus, prolog_player, prolog_position, pending_pos_req,
                  gst_next_event >>

PYMAIN == Py_Read_play \/ Py_Read_pause \/ Py_Read_stop
            \/ Py_Read_position \/ Py_Read_uri \/ Py_Loading

-----------------------------------------------------------------------------
(*                          Python Bus Thread                              *)
(*  Asynchronously emits events. We only model "eos" here.                *)

PyBus_Emit ==
  /\ pc_pybus = "PyBus_Loop"
  /\ python_pipeline \in {"playing", "paused", "loading"}
  /\ pipe_g2p' = Append(pipe_g2p, "eos")
  /\ pc_pybus' = "PyBus_Loop"
  /\ UNCHANGED << swimpd_mutex, pipe_p2g, gst_mailbox, pc_mpd, pc_gst,
                  pc_pymain, prolog_player, python_pipeline, prolog_position,
                  pending_pos_req, python_busy, gst_next_event >>

PyBus_Skip ==
  /\ pc_pybus = "PyBus_Loop"
  /\ pc_pybus' = "PyBus_Loop"
  /\ UNCHANGED << swimpd_mutex, pipe_p2g, pipe_g2p, gst_mailbox, pc_mpd,
                  pc_gst, pc_pymain, prolog_player, python_pipeline,
                  prolog_position, pending_pos_req, python_busy,
                  gst_next_event >>

PYBUS == PyBus_Emit \/ PyBus_Skip

-----------------------------------------------------------------------------
(*                               Specification                               *)

Init ==
  /\ swimpd_mutex = "free"
  /\ pipe_p2g = << >>
  /\ pipe_g2p = << >>
  /\ gst_mailbox = << >>
  /\ pc_mpd = "MPD_Loop"
  /\ pc_gst = "GST_Read"
  /\ pc_pymain = "Py_Loop"
  /\ pc_pybus = "PyBus_Loop"
  /\ prolog_player = "nothing"
  /\ python_pipeline = "null"
  /\ prolog_position = -1
  /\ pending_pos_req = FALSE
  /\ python_busy = FALSE
  /\ gst_next_event = ""

Next == MPD \/ GST \/ PYMAIN \/ PYBUS

Spec == Init /\ [][Next]_vars
           /\ WF_vars(MPD)        (* MPD is fair: doesn't starve            *)
           /\ WF_vars(GST)        (* GST Reader is fair                     *)
           /\ WF_vars(PYMAIN)     (* Python Main is fair                    *)
           /\ WF_vars(PYBUS)      (* Bus thread is fair                     *)

-----------------------------------------------------------------------------
(*                              Properties                                 *)

THEOREM MutexTheorem == Spec => []MutexSafety
THEOREM TypeTheorem    == Spec => []TypeInvariant

(* Temporal: quiescent states should be consistent.                        *)
(* This will fail because of the sync_player_state fall-through bug.       *)
THEOREM ConsistencyTheorem == Spec => []QuiescentConsistent

=============================================================================
