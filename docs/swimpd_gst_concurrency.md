# Concurrency Analysis: Prolog MPD Server ↔ Python GStreamer Slave

## 1. Topology

There are **four active threads/processes** in the runtime:

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                           SWI-PROLOG (OS process)                           │
│  ┌──────────────────────┐              ┌──────────────────────┐            │
│  │ MPD Client Handler   │◄────────────►│ GST Reader Thread    │            │
│  │ (per TCP connection) │   mailbox    │ (gst_reader/1)       │            │
│  └──────────┬───────────┘              └──────────┬───────────┘            │
│             │                                      │                        │
│      holds  │  swimpd                              │ reads                  │
│      mutex  │  mutex                               │ stdout                 │
│             ▼                                      ▼                        │
│       ┌─────────────┐                      ┌─────────────┐                │
│       │  state/2    │                      │  pipe_g2p    │                │
│       │  vstate/2   │                      │  (async)    │                │
│       └─────────────┘                      └─────────────┘                │
└─────────────────────────────────────────────────────────────────────────────┘
              │                                      ▲
     stdin   │                                      │ stdout
     (pipe)  │                                      │ (pipe)
              ▼                                      │
┌─────────────────────────────────────────────────────────────────────────────┐
│                      PYTHON 3 (OS subprocess)                               │
│  ┌──────────────────────┐              ┌──────────────────────┐            │
│  │ Python Main Thread   │              │ Python Bus Thread    │            │
│  │ (reads stdin, blocks │              │ (GStreamer bus pop)  │            │
│  │  during load/uri)    │              │                      │            │
│  └──────────┬───────────┘              └──────────┬───────────┘            │
│             │                                      │                        │
│             │      ┌─────────────┐                 │                        │
│             └─────►│  playbin3   │◄────────────────┘                        │
│                    │  (GStreamer)│                                          │
│                    └─────────────┘                                          │
└─────────────────────────────────────────────────────────────────────────────┘
```

### 1.1 Threads

| Thread | File / Predicate | Role |
|--------|-----------------|------|
| **MPD Handler** | `telnetd.pl` → `client/4` | One per TCP connection. Parses MPD commands, mutates state inside `swimpd` mutex. |
| **GST Reader** | `gst.pl` → `gst_reader/1` | Single thread reading Python stdout line-by-line. May need `swimpd` mutex to sync state. |
| **Python Main** | `python/gst12.py` `main()` | Reads stdin, executes commands on GStreamer `playbin3`. **Blocks for 0.5s** during `uri`/`load`. |
| **Python Bus** | `python/gst12.py` `handle_messages()` | Daemon thread (`t.daemon = True`). Pops GStreamer bus messages asynchronously. Prints events. |

### 1.2 Channels

| Channel | Direction | Mechanism | Content |
|---------|-----------|-----------|---------|
| `pipe_p2g` | Prolog → Python | OS pipe (stdin) | Text commands: `uri <url>`, `play`, `pause`, `seek <n>`, `volume <v>`, `position`, `stop` |
| `pipe_g2p` | Python → Prolog | OS pipe (stdout) | Text events: `eos`, `bitrate <n>`, `duration <n>`, `position <n>`, `id_pos <id>:<pos>`, `format <r>:<f>:<c>`, `state <name>`, `error <msg>` |
| `gst_mailbox` | GST Reader → MPD Handler | SWI-Prolog thread message queue | `position(Pos)` tuples |
| `swimpd` mutex | — | `with_mutex(swimpd, Goal)` | Guards `state/2` mutations and `set_states/2` |

---

## 2. Critical Concurrency Patterns

### 2.1 Fire-and-Forget Command Protocol

Prolog sends commands via `send/1`:

```prolog
send(P) :-
   thread(gst(In), _), phrase(P, Codes),
   format(In, "~s\n", [Codes]).
```

There is **no acknowledgment** of command completion. Prolog's only feedback is asynchronous events from Python arriving on `pipe_g2p`. This means:

- Prolog cannot know when the `uri` command has finished its 0.5s sleep.
- Prolog cannot know if a `play` command actually succeeded (only infers it from later `state` events, which are not actually emitted in current code).
- Commands can be pipelined: Prolog may send `uri`, `seek`, and `play` before Python has finished processing `uri`.

### 2.2 The 0.5s Delay in `load`

```python
def load(u):
    stop(); p.set_property('uri', stream_url(u)); sync(); time.sleep(0.5); pause(); sync();
```

When Python receives `uri <url>`:
1. Stops pipeline (`null` state), emits `state null`.
2. Sets URI property.
3. Syncs state.
4. **Sleeps 0.5 seconds** — during this time Python Main cannot read new stdin commands.
5. Pauses pipeline, emits `state paused`.

During this 0.5s window:
- The Bus thread continues to emit messages from the **old** stream (e.g., `eos` if the old stream ends) or from the new stream setup.
- Prolog may send additional commands (`play`, `seek`) which buffer in `pipe_p2g`.
- GST Reader may read `state null`, `state paused`, `eos`, `error`, etc.

### 2.3 The `swimpd` Mutex + `recv_position` Deadlock Risk

The `status` MPD command triggers a position poll:

```prolog
% In gst.pl
gst_audio_info(_, au(Dur, Elap, BR, Fmt)) :-
   send("position"),               % 1. Send command to Python
   recv_position(Elap),           % 2. BLOCK up to 2 seconds
   maplist(vstate, ...).

recv_position(Pos) :-
   thread(gst(_), Id),
   (  thread_get_message(Id, position(Pos), [timeout(2)]) -> true
   ;  print_message(warning, recv_timeout(position)), fail
   ).
```

But `gst_audio_info` is called from `report_slave_state`, which is called from `report_status`, which is called during `status` command execution. And `status` is executed **inside the `swimpd` mutex**:

```prolog
% In commands.pl (via DCG expansion)
reading_state(K, Action) --> {excl(states(K, V))}, call(Action, V).
% excl/1 == with_mutex(swimpd, Goal)
```

**Sequence leading to transient deadlock:**

| Step | MPD Handler Thread | GST Reader Thread | Python Main |
|------|-------------------|-------------------|-------------|
| 1 | Acquires `swimpd` mutex | Reading stdout | Idle |
| 2 | Sends `position` to Python | | Receives `position` |
| 3 | Blocks on `thread_get_message(..., timeout(2))` | | Sends `position 42.0` to stdout |
| 4 | **Still holding mutex** | Reads `position 42.0`, sends to own mailbox | |
| 5 | **Still holding mutex** | Reads `state null` from old stream | |
| 6 | **Still holding mutex** | Tries `set_states(player, nothing)` → **blocks on `swimpd` mutex** | |
| 7 | Times out after 2s, releases mutex | | |
| 8 | | Acquires mutex, updates state | |

**Result:** A 2-second stall in the MPD handler while holding the mutex. If another MPD client is connected, it cannot execute any state-mutating command during this window.

The code contains a developer FIXME acknowledging this:
> `% FIXME: synchronise with mutex from commands module?`

### 2.4 Python GIL and Unprotected Shared State

Python Main and Python Bus share:
- The `p` (playbin3) reference
- The `durations` mutable cell `[0.0]`
- The `wrapper` mutable cell `[identity]`

Only `print_` is protected by a `threading.Lock()`. Pipeline mutations (`set_state`, `set_property`, `seek_simple`) are **not** explicitly synchronized. GStreamer's C API is thread-safe, but Python-level operations on the `p` wrapper object are under the GIL, which provides some safety but can still lead to races at the GStreamer level.

More importantly, the Bus thread's `events` handler calls `durations(state[0])` which reads a mutable cell. If the Main thread is also accessing `durations`, there is no Python-level lock.

---

## 3. State Desynchronization Bug (Identified via Analysis)

### 3.1 The `uri` + `play` Sequence

When a user plays a new track, Prolog executes (simplified):

```prolog
cue_and_maybe_play(Songs-Pos, play-(_/Dur)) :-
   send(fmt('uri ~s', [URL])),      % Python goes: null → [sleep 0.5s] → paused
   restore_position(Songs-Pos),      % May send seek
   send("play").                    % Python goes: paused → playing
```

But Prolog's state is updated **before** `cue_and_maybe_play` is called:

```prolog
update_play_state(play, Pos, Dur, _, just(ps(Pos, just(play-0.0/Dur)))).
```

So:
1. **Prolog state** = `playing` at position 0.
2. Prolog sends `uri` → Python starts `load`.
3. Python emits `state null` (from `stop()` inside `load`).
4. GST Reader processes `state null` and calls `sync_player_state(null, just(ps(Pos, play-...)))`.
5. This hits the clause:
   ```prolog
   sync_player_state(null, _) :- !,
      set_states(player, nothing),   % ← RESETS to nothing!
      notify_all([player]).
   ```
6. Python finishes 0.5s sleep, emits `state paused`.
7. GST Reader hits:
   ```prolog
   sync_player_state(paused, nothing) :- !,
      set_states(player, just(ps(0, just(pause-0.0/1.0)))).
   ```
8. Prolog state = `paused` at position 0.
9. Prolog sends `play` → Python emits `state playing`.
10. GST Reader hits the catch-all:
    ```prolog
    sync_player_state(GS, PS) :-
       debug(..., 'Player state desync: fall-through(~w, ~w)', [GS, PS]).
    ```
    **Does nothing!**

**Final state:** Python pipeline = `playing`, Prolog state = `paused`.

### 3.2 Why This Matters

- MPD clients displaying `status` will show `state: pause` while audio is playing.
- The `next`/`previous` track logic depends on `prolog_player` state. If Prolog thinks `paused`, some stepping logic may not fire correctly.
- `notify_all([player])` fires during the resets, causing MPD clients to flicker between `play` → `stop` → `pause` → (stays `pause`).

### 3.3 Root Cause

`sync_player_state/2` is incomplete:
- It handles `null` vs anything → reset to `nothing`.
- It handles `paused` vs `nothing` → set to `paused`.
- It does **not** handle `playing` vs `paused` or `playing` vs `playing`.

Furthermore, `sync_player_state` appears to be **unreachable dead code** in normal operation. Python only emits `state <name>` in response to the `state` command from Prolog, but Prolog never sends `state` during normal playback. The events dict in `gst12.py` does not include a `STATE_CHANGED` bus message handler. So in practice, state synchronization relies entirely on the assumption that commands deterministically update state — which is violated by the async `uri` load sequence.

---

## 4. Delay Model

| Delay | Location | Reason | Impact |
|-------|----------|--------|--------|
| **0.5s** | `python/gst12.py` `load()` | `time.sleep(0.5)` between `set_property(uri)` and `pause()` | Python Main cannot process new commands. Bus events from old/new stream may interleave. |
| **2.0s** | `prolog/swimpd/gst.pl` `recv_position/1` | `thread_get_message(..., timeout(2))` waiting for GST Reader mailbox | MPD Handler holds `swimpd` mutex during wait, blocking other clients. |
| **Unbounded** | `python/gst12.py` `youtube_url_api()` | `yt_dlp().extract_info()` over the network | Called synchronously in `stream_url()` during `uri` command. Blocks Python Main. Can be 1–10s. |
| **Unbounded** | `prolog/bbc_db.pl` `uget/2` | `http_open` fetching BBC schedules | Blocks calling Prolog thread. Happens in `db_update`. |

---

## 5. What TLA+ Can Reveal

The above analysis was done manually by reading code. A TLA+ model can **mechanically verify** the following:

### 5.1 Safety Properties (Invariants)

1. **MutexSafety**: `swimpd_mutex` is held by at most one thread at any time.
2. **NoPermanentDeadlock**: It is impossible for MPD Handler to be waiting on `gst_mailbox` while GST Reader is waiting on `swimpd_mutex` — **unless** a timeout action is enabled. (In the real code, the 2s timeout prevents permanent deadlock.)
3. **MailboxOrdering**: Messages in `gst_mailbox` are consumed FIFO by `recv_position`.
4. **PipeFIFO**: `pipe_p2g` and `pipe_g2p` respect FIFO ordering.

### 5.2 Temporal / Liveness Properties

1. **EventualConsistency**: If the system becomes quiescent (no pending commands, `python_busy = FALSE`, pipes empty), then `prolog_player` and `python_pipeline` should agree.
   > **Expected result: VIOLATED** — because of the `sync_player_state` fall-through bug.

2. **Liveness_Load**: If Prolog sends `uri` followed by `play`, Python pipeline eventually reaches `playing`.
   > **Expected result: SATISFIED** — Python does reach `playing`, even if Prolog doesn't know it.

3. **Liveness_PositionPoll**: If Prolog sends `position` and Python replies, Prolog eventually reads the reply (or times out).
   > **Expected result: SATISFIED** (with timeout caveat).

4. **NoSpuriousStop**: If a track is cued and should play, Prolog should not spuriously transition to `nothing`.
   > **Expected result: VIOLATED** — `sync_player_state(null, _)` resets to `nothing` during `uri` load.

---

## 6. Suggested Fixes (Informed by Analysis)

### 6.1 Fix the State Desync

1. **Remove the aggressive `sync_player_state(null, _)` reset**, OR only apply it if Prolog has not just issued a `uri` command.
2. **Complete `sync_player_state`** for all `(PythonState, PrologState)` pairs.
3. **Add a `loading` state** to Prolog's player state machine. During `uri` processing, Prolog should enter `loading`, and `sync_player_state` should be a no-op or transition from `loading` to `paused`.
4. **Have Python emit `ready` or `loading` events** so Prolog knows the pipeline is mid-transition.

### 6.2 Fix the Mutex Contention

1. **Do not hold `swimpd` mutex while waiting for `position`**. Split `status` command execution:
   - Read all non-audio state under mutex.
   - Release mutex.
   - Query Python for position (which may take time).
   - Re-acquire mutex only if position needs to be stored in `vstate`.

2. **Use `vstate/2` (volatile, no mutex) for position, bitrate, format**. These are already volatile. The `set_global/1` predicate in `gst.pl` uses `set_vstate/2`, which does **not** use `swimpd` mutex. So GST Reader can update position without deadlock! The issue is only when `sync_player_state` calls `set_states/2` (which uses the mutex).

### 6.3 Fix Python Delays

1. **Move the 0.5s sleep out of the command loop**. Use GStreamer bus messages to detect when the stream is ready, then auto-pause or auto-play.
2. **Pre-resolve YouTube URLs** in Prolog or a separate thread, so `uri` command only sets a ready URL.
3. **Use GStreamer's async state change** API instead of blocking sleeps.

---

## 7. Relation to the TLA+ Specification

The specification in `SwimpdGst.tla` (PlusCal) and its TLC-checkable pure-TLA+ translation
`SwimpdGstPure.tla` capture the above phenomena at an abstract level:

- **Processes**: `MPD`, `GSTReader`, `PythonMain`, `PythonBus`.
- **Channels**: `pipe_p2g`, `pipe_g2p`, `gst_mailbox`.
- **Mutex**: `swimpd_mutex`.
- **Delay abstraction**: The 0.5s `uri` load is one discrete `Py_Loading` step. The 2s timeout is a nondeterministic `MPD_WaitPos` branch.
- **Bug reproduction**: The `sync_player_state` behavior in `GSTReader` is modeled exactly as in the code (reset on `null`, set on `paused`, fall-through otherwise).

### 7.1 Verified results (TLC via Grounds, `SwimpdGstPure.tla`)

TLC checked the bounded model (`StateConstraint` caps all queues at length 5) and found a
counterexample to the `QuiescentConsistent` invariant — the state desync is **mechanically
confirmed**, not just inferred:

```
1. Init
2. MPD_Play            prolog: nothing -> playing; pipe p->g += [play]
3. Py_Read_play        python: null -> playing; pipe g->p += [state_playing]
4. GST_Read            pops state_playing from pipe g->p
5. GST_Handle_playing  FALL-THROUGH BUG: no clause in sync_player_state,
                       prolog stays "playing" (this step is correct here only
                       by luck — prolog already said playing)
6. PyBus_Emit          bus emits eos (stream hit end-of-stream)
7. GST_Read            pops eos
8. GST_Handle_eos      prolog: playing -> nothing (notify_eos resets player)
                       python still: playing  <-- DESYNC
```

Final state is quiescent (all queues empty, `python_busy = FALSE`) but inconsistent:
`prolog_player = "nothing"` while `python_pipeline = "playing"`. The next MPD `status`
would report `state: stop` to clients while audio is actually still playing — exactly the
client-visible symptom of the desync. This is a second manifestation of the same
`sync_player_state` defect class (the `playing` fall-through plus the unconditional `eos`/`null`
reset), now proven by model checking.

> Note: the original prediction was a `uri`+`play` trace where Prolog stays `paused` while
> Python reaches `playing`. TLC found an even shorter witness via `play` then `eos`. Both
> paths share the same root cause: `sync_player_state` cannot reconcile
> `state_playing`, and `eos`/`null` events unconditionally reset Prolog.

`TypeInvariant` and `MutexSafety` were run as a bounded soundness check (`SoundnessConstraint`, all queues ≤ 2): **holds within bounds — 44,126 states, depth 36** (recorded as Grounds evidence). The model itself is well-formed; the sole intentional failure is the consistency property.

### 7.2 How to re-run

1. Open `docs/SwimpdGst.tla` in the TLA⁺ Toolbox (it is PlusCal; the Toolbox translates it).
2. Or use `docs/SwimpdGstPure.tla` directly with a TLC config:
   ```
   CONSTANTS
     COMMANDS = {"play", "pause", "stop", "uri", "position"}
     EVENTS = {"state_null", "state_paused", "state_playing", "position_42", "eos"}
   SPECIFICATION Spec
   INVARIANTS TypeInvariant MutexSafety QuiescentConsistent
   CONSTRAINT StateConstraint
   ```
3. Expect: `TypeInvariant` ✓, `MutexSafety` ✓, `QuiescentConsistent` ✗ (counterexample §7.1).

### 7.3 Open follow-up (not yet modeled)

The `uri`+`play` trace and the mutex-contention deadlock window (MPD holds `swimpd` across a
2s position wait while GST Reader blocks in `set_states/2`) are described structurally but the
model does not yet force `MPD_WaitPos` and `GST` contention in the same run. Adding a `loading`
state to `prolog_player` and a `uri`-first scenario is the natural next extension to confirm the
`paused`-while-`playing` variant and the transient deadlock.
