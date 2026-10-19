# AGENTS.md — bbc

Notes for coding agents working in this repository. For a full project map
(architecture, modules, protocol, run instructions) see `AGENT_OVERVIEW.md`.
This file is loaded into context by pi (and other agents) each session.

## Hard-won observations (2026-10-19)

### 0. Branch layout — this repo's branches get rewritten often

Branch `erlang_version` was rebased away and **deleted** (locally and on
origin); work now lives on `erlang-poc` (current) and `formalise-with-tla`
(which carries the TLA⁺ work: `docs/SwimpdGst.{tla,md}`,
`docs/SwimpdGstPure.tla`, `AGENT_OVERVIEW.md`, `.grounds/conformance/…`).
`erlang-poc` does **not** contain `docs/` or `AGENT_OVERVIEW.md`.

When resuming work: `git branch --show-current`, `git status -sb`, and check
`git branch -vv` — the branch you finished on may no longer exist, and the
commit you made may have been absorbed into another branch by a rebase.

### 1. Grounds MCP: never diagnose it with curl

The `grounds` MCP server (`http://localhost:3000/mcp`, configured in
`~/.pi/agent/mcp.json`) only exposes its tools through the pi MCP client, which
applies its own auth context. Raw HTTP probes **do not work**:

- `curl /mcp` + `tools/list` → `[]` (empty) with no auth, `401 Unauthorized`
  with the stored header.

That is **not** a sign Grounds is down. To check availability, use codemode:
`searchTools("grounds")` and `describeTool("mcp__grounds__grounds_plan")`
(there are 35 `mcp__grounds__*` tools). If those succeed, the server is fine.

### 2. Grounds workflow (this repo is a "Standard" Grounds repo)

- Order: `grounds_start` → `grounds_plan` (files you'll touch) → `grounds_goal`
  (one criterion per thing, with the evidence it needs) → `grounds_step` for
  each read/edit → reasoner calls (`tla_check`, `imandrax_*`, …) → `grounds_gap`
  for what you could not establish → `grounds_submit`.
- **Before changing any file, `grounds_start`** (continue with
  `continue_session` on the same branch: one piece of work stays one trace).
- Reasoner results (`tla_check` etc.) are recorded as Grounds evidence
  automatically — do not re-report them via `grounds_evidence`.
- Every symbol/file you edit must be named by a goal criterion (or declared
  out of scope via `grounds_gap`), or submit bounces.
- A submit that returns "waiting for a person's decision" accepts no more
  reports until you `grounds_start` again (which reopens it; the first report
  after that restarts the rounds).
- **Known infrastructure quirk**: the judging engine reads trace spec ≤ 1.14
  while sessions record 1.15 — submits bounce with "trace declares trace spec
  1.15" until the Grounds engine is upgraded; the session ends "waiting for a
  person". File `grounds_feedback` rather than re-reporting.

### 3. TLA+ via grounds `tla_check`: the "1 state at depth 1" trap

If an action both primes a variable and lists it in `UNCHANGED << … x … >>`,
the action is contradictory and **never enabled** — TLC reports only the
initial state and finds no counterexamples (looks like the spec is fine).
Rules:

- Each primed variable must be assigned **exactly once** per action. `x' = a`
  and `UNCHANGED << x >>` in the same action is a contradiction even when
  `a` equals the current value.
- A *failing* property stops fast at the first counterexample; a *passing*
  invariant check exhausts the whole state space — add a `CONSTRAINT`
  (named definition, e.g. `Len(pipe) <= 2`; inline expressions are rejected
  by the config parser) or TLC times out at 60s.
- TLC config for the model in `docs/SwimpdGstPure.tla`: `CONSTANTS`,
  `SPECIFICATION Spec`, `INVARIANTS`, `CONSTRAINT` — see
  `docs/swimpd_gst_concurrency.md` §7.2 for the working config.

### 4. Secrets: redact the `access` field

`~/.pi/agent/auth.json` stores API credentials under a key literally named
`access` (OpenRouter OAuth). Redaction filters that match only
`token|key|secret|bearer` **miss it** — a full API key ended up printed in a
transcript and the user had to rotate it. When printing any config, redact
`access` too. Never print the value of the OpenRouter key.
