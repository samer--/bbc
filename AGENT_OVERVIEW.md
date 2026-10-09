# Project Overview: bbc

**Purpose:** Tools for working with BBC content and metadata, focused on streaming BBC radio without needing a web browser. Designed for headless/hi-fi use.

**Languages:** Prolog (SWI-Prolog), Python 3, Erlang/OTP, shell.

**Current Working Directory:** `/Users/samer/src/samer/bbc`

---

## Architecture at a Glance

The project is a monorepo with three semi-independent implementations of the same core idea:

1. **swimpd** (Prolog + Python) — The primary, working MPD server.
2. **erl_mpd** (Erlang/OTP) — A newer, incomplete Erlang rewrite exploring actor-model concurrency.
3. **swiplayer** (Prolog) — A nascent web UI (not functional yet).

### swimpd — Main MPD Server

`swimpd` implements the [Music Player Daemon (MPD)](https://www.musicpd.org/doc/html/protocol.html) text protocol so standard MPD clients (`mpc`, `ncmpc`, MPDroid, M.A.L.P.) can browse and play BBC Radio programmes.

**Entry point:** `swimpd.pl`
**Run script:** `swimpd` (shell wrapper calling SWI-Prolog)
**Async run:** `swimpd(async).` from SWI-Prolog

**Components:**

| Module | File | Role |
|--------|------|------|
| Main | `swimpd.pl` | Bootstraps server: parses CLI args, starts GStreamer thread, starts telnet server, manages shutdown signals. |
| Telnet server | `prolog/swimpd/telnetd.pl` | Accepts TCP connections, spawns a Prolog thread per client. |
| Protocol | `prolog/swimpd/protocol.pl` | Parses MPD text protocol, handles `command_list_begin` / `idle` / `noidle`, dispatches to command handlers, manages async notifications. |
| Commands | `prolog/swimpd/commands.pl` | Implements every MPD command (`play`, `pause`, `add`, `seek`, `status`, `lsinfo`, `update`, `albumart`, etc.). Heavy use of DCGs and state monads. |
| State | `prolog/swimpd/state.pl` | Persistent state via `library(persistency)` (`state/2` facts) + volatile state (`vstate/2`, `version_queue/2`). Auto-syncs DB every hour. |
| Database | `prolog/swimpd/database.pl` | Wraps BBC programme data: schedules, live radio URLs, programme metadata, images, tracklists. Abstracts BBC PIDs/VPIDs into MPD paths. |
| GStreamer bridge | `prolog/swimpd/gst.pl` | Spawns Python slave process (`python/gst12.py`) via pipes. Sends text commands (`play`, `pause`, `seek`, `uri`, `volume`). Receives async events (`eos`, `bitrate`, `duration`, `position`, `state`, `error`). |
| Tools | `prolog/swimpd/tools.pl` | Shared DCG utilities, thread helpers, parsing helpers. |
| BBC data | `prolog/bbc_db.pl` | Fetches BBC schedules (XML or JSON), parses them, asserts `time_service_schedule/3`. Knows about 7 BBC radio services. |
| BBC tools | `prolog/bbc_tools.pl` | Small utilities: sorting, enumeration, logging. |

**Audio pipeline:**

```
MPD client → TCP → telnetd → protocol → commands → state/db
                                           ↓
                                    gst.pl (Prolog)
                                           ↓ (stdin/stdout pipes)
                                    gst12.py (Python + GStreamer)
                                           ↓
                                    BBC stream / YouTube / file
```

**Python audio slave:** `python/gst12.py`

- Self-contained functional-utility digest at the top.
- Uses GStreamer 1.0 (`playbin3`) via `gi.repository.Gst`.
- Reads one line per command from stdin, prints events to stdout.
- Supports `yt-dlp` for YouTube audio extraction.
- Environment variables: `GST12_YTDLP_FORMAT`, `GST12_YTDLP_CONFIG`, `GST12_YTDLP_USE_CMD`.

**Python stream lookup:** `python/bbc_stream.py`

- Standalone CLI script to resolve a BBC PID/VPID to a stream URL.
- Queries BBC's `mediaselector` API.
- Can select DASH vs HLS, protocol, mediaset, etc.

**Macports note:** Python Gnome introspection (`gi`) may need:
```sh
export GSETTINGS_SCHEMA_DIR=/opt/local/share/glib-2.0/schemas
```
This is baked into the `launchd` service.

### erl_mpd — Erlang Rewrite (WIP)

**Directory:** `erlang/`

An OTP application exploring actor-model concurrency as an alternative to Prolog's multi-threading.

| Module | Role |
|--------|------|
| `mpd_app` | OTP application callback |
| `mpd_sup` | Top-level supervisor |
| `mpd_acceptor` | TCP acceptor |
| `mpd_client` | `gen_statem` per connection (states: `waiting_socket`, `ready`, `idle`, `command_list`) |
| `mpd_client_sup` | Dynamic client supervisor |
| `mpd_protocol` | Binary protocol parser (equivalent to Prolog `protocol.pl`) |
| `mpd_state` | Player & queue state server |
| `mpd_player` | Stub `gen_server` for GStreamer integration (logs only) |

**Build:** `rebar3 compile`

**Status:** Skeleton works; no GStreamer integration yet. The player module is a stub.

### swiplayer — Web UI (very WIP)

**File:** `prolog/web/swiplayer.pl`

- A SWI-Prolog `library(http)` web server.
- Only stub HTTP handlers exist (`root`, `hello`, `search`, `cats`, `channels`).
- Not functional for end users yet.

---

## Data Model

**BBC entities:**
- `service/1,2,3` — BBC radio station (e.g., `bbc_radio_three`).
- `pid_entry/3` — Programme in a service schedule.
- `pid_version/2`, `version_prop/2` — Programme versions and metadata.
- `pid_tracks/2` — Tracklists within a programme (for `previous`/`next` track-seeking).
- `time_service_schedule/3` — Cached schedule data (timestamped).

**MPD abstractions:**
- `song(PID, URLSource, Tags)` — An item in the queue.
- `path` — A list of atoms like `[bbc_radio_three, some_pid]` or `[Live Radio, BBC Radio 3]` or `[youtube, YT_ID]`.
- `state/2` — Persisted facts: `volume`, `queue`, `player`, `consume`, `single`.
- `vstate/2` — Volatile runtime state: `start_time`, `dbtime`, `error`, `bitrate`, `duration`, `format`.

---

## Key Dependencies

**SWI-Prolog packages (must be installed):**
- `memo`
- `fileutils`
- `dcgutils`
- Also uses: `dcg_core`, `dcg_codes`, `dcg_pair`, `snobol`, `insist`, `callutils`, `listutils`, `data/pair` (likely from `dcgutils` or `matthews` packs).

**Python:**
- `gi` (GObject introspection, GStreamer)
- `yt-dlp` (optional, for YouTube)

**Erlang:**
- Standard OTP + `rebar3`

---

## Running / Development

### Start swimpd manually:
```sh
./swimpd 6601 /path/to/state.db
# or with debug topics:
./swimpd 6601 /path/to/state.db 'mpd(_,s(s(_)))'
```

### macOS LaunchAgent:
- `launchd/org.samer.swimpd.plist` — configures auto-start on port 6601.

### systemd:
- `systemd/swimpd.service` — Linux service file.

### Erlang:
```sh
cd erlang
rebar3 compile
rebar3 shell
```

---

## Notable Design Patterns

1. **DCG-heavy command DSL** — `commands.pl` uses DCG meta-predicates to embed MPD command definitions directly as grammar rules. The `(:->)/2` operator is custom syntax sugar.
2. **State monad via DCGs** — Many operations thread state through DCG pairs, e.g., `trans/3`, `get/1`, `set/1`, `<\>`.
3. **Persistent + volatile split** — `state/2` is disk-backed; `vstate/2` is in-memory only.
4. **Versioned queue** — Every queue mutation bumps version; `version_queue/2` keeps history for `plchanges` (MPD delta protocol).
5. **Track-seeking within programmes** — `cursor_at_time/4` and `step_track_or_prog/2` allow `next`/`previous` to jump between tracks *inside* a BBC programme, not just between programmes.
6. **Process-per-client** — Both Prolog (thread) and Erlang (process) versions spawn an actor per TCP connection.

---

## TODOs / Known Issues

See `prolog/TODO` and inline comments:
- Seek implementation is flagged "FIXME: No!!" in several places.
- GStreamer state desync handling is heuristic.
- Erlang version lacks real audio backend.
- swiplayer is non-functional.
- BBC stream URLs are time-limited and need periodic refresh.
- `Artist` tag is not set for programmes (causes issues with Theremin client).

---

## File Map (Quick Reference)

```
.
├── swimpd                    # shell launcher (Prolog version)
├── swimpd.pl                 # Prolog entry point
├── swimpdi                   # interactive wrapper?
├── erl_mpd                   # shell launcher (Erlang version)
│
├── prolog/
│   ├── swimpd/
│   │   ├── telnetd.pl        # TCP server
│   │   ├── protocol.pl       # MPD text protocol engine
│   │   ├── commands.pl       # MPD command implementations
│   │   ├── state.pl          # persistent + volatile state
│   │   ├── database.pl       # BBC data → MPD database
│   │   ├── gst.pl            # GStreamer slave control
│   │   └── tools.pl          # shared utilities
│   ├── bbc_db.pl             # BBC schedule fetcher / parser
│   ├── bbc_tools.pl          # BBC data utilities
│   └── web/swiplayer.pl      # nascent web UI
│
├── python/
│   ├── gst12.py              # GStreamer audio slave
│   └── bbc_stream.py         # BBC stream URL resolver
│
├── erlang/
│   ├── src/
│   │   ├── mpd_app.erl
│   │   ├── mpd_sup.erl
│   │   ├── mpd_acceptor.erl
│   │   ├── mpd_client.erl    # gen_statem per connection
│   │   ├── mpd_client_sup.erl
│   │   ├── mpd_protocol.erl  # binary parser
│   │   ├── mpd_state.erl
│   │   └── mpd_player.erl    # audio stub
│   └── rebar.config
│
├── launchd/
│   └── org.samer.swimpd.plist
├── systemd/
│   └── swimpd.service
├── README.md
└── AGENT_OVERVIEW.md         # this file
```
