## Purpose

Short, focused guidance for AI coding assistants working on rummy-ocaml. Use this to discover the project's architecture, developer workflows, noteworthy conventions, and integration points so you can be productive quickly.

## Big-picture architecture (what to know first)
- Core game library: `src/` contains the rummy engine and types compiled as the `rummy_engine` library. Key files:
  - `src/types.ml` — canonical data shapes and JSON (Yojson) (serialize_state / deserialize_state).
  - `src/engine.ml` — core step API (draw/play/discard/endcheck). Functions return `step_result = Ok | End_round | Error` (handle these explicitly; don't assume exceptions).
  - `src/rules.ml` and `src/setup.ml` — rule checks (sets/runs) and initial-state helpers.

- Command-line / simulation:
  - `src/main.ml` — interactive CLI game loop (uses `Engine` API). Good for tracing engine behavior.
  - `bin/sim` (defined in `bin/dune`) — small executable used for scripted runs / simulations.

- Web UI (Bonsai):
  - `ui_bonsai/src/app.ml` — the main Bonsai UI. Compiled to JS via dune/js_of_ocaml into `app.bc.js`.
  - `ui_bonsai/src/firestore.ml` and `ui_bonsai/src/listener.js` — Firestore integration and realtime listener. The UI expects remote state serialized by `Types.serialize_state` and stored under the Firestore document field `state` as a string.

## Build / run / test workflows (concrete)
- Full build (native + JS):
  - Primary build: `dune build @all` (project uses dune 3.x; see `dune-project`).
  - The included helper script `./run.sh` runs a full build, copies the compiled JS from `_build/default/ui_bonsai/src/app.bc.js` into `ui_bonsai/src/app.bc.js` and starts a simple HTTP server on port 8001. On macOS it opens the browser automatically.

- Run the Bonsai UI locally (quick):
  1. `./run.sh` (preferred convenience script).
  2. Alternatively: `dune build @all`, then copy `_build/default/ui_bonsai/src/app.bc.js` → `ui_bonsai/src/app.bc.js`, then serve the repo (the script uses `python3 -m http.server 8001`).

- CLI / sim:
  - `dune exec sim` runs the simulation/CLI executable defined in `bin/dune`.

- Tests:
  - Inline expect tests live under `src/test/` and are configured in `src/test/dune` (ppx_expect). Run them with `dune runtest` from the repo root.

## Important project-specific conventions & patterns
- State persistence: the game state is serialized with `Types.serialize_state` (Yojson) and persisted to Firestore as a single string stored in the document field `fields.state.stringValue`. The JS realtime listener expects that exact layout.
- Firestore document: `src/firebase_config.ml` and `ui_bonsai/src/firestore.ml` reference the same document id (currently `n9HZyYuqk9cUnEvEDcz5`). If you change docs or project, update both OCaml and JS helpers.
- JS listener: `ui_bonsai/src/listener.js` registers a global `startRummyListener(cb)` that the OCaml JS glue calls. The listener receives the serialized state string and calls the provided callback.
- Error handling: Engine APIs return `Error (Illegal_play "msg")` etc; calling code typically pattern-matches on `Ok | End_round | Error`. When modifying engine behavior, keep the step_result contract intact.
- No runtime exceptions for normal control flow: prefer explicit Result/variant handling across engine, play, and discard code paths.

## Integration points & external deps
- Firestore/HTTP
  - OCaml REST calls use `src/http_client.ml` and `src/firebase_client.ml` (Async/Deferred). The browser UI uses `ui_bonsai/src/firestore.ml` (XmlHttpRequest) and `ui_bonsai/src/listener.js` (Firestore JS SDK).
  - Update `src/firebase_config.ml` when switching Firestore projects; update `ui_bonsai/src/listener.js` firebaseConfig constants for the browser side.

- Serialization
  - `Types` contains Yojson converters (`[@@deriving yojson]`). Use these when reading/writing state across boundaries.

## Examples (where to look when changing behavior)
- To change game rules: inspect and modify `src/rules.ml` and then update tests in `src/test/`.
- To change persistence shape: update `src/types.ml` (yojson) and adjust `src/firebase_client.ml` and `ui_bonsai/src/firestore.ml` to keep Firestore read/write in sync.
- To debug a round transition: trace calls in `src/engine.ml` (functions `draw`, `play`, `discard`, `endcheck`) and follow the CLI loop in `src/main.ml` for examples of handling `End_round` vs `Ok`.

## Quick pointers for contributors
- Prefer pattern-matching on `Engine.step_result` instead of catching exceptions.
- When adding features that touch remote state, update both OCaml REST code (`src/firebase_client.ml`) and the JS listener (`ui_bonsai/src/listener.js` / `ui_bonsai/src/firestore.ml`).

## Where to look first (order)
1. `src/types.ml` — understand data shapes and JSON contract.
2. `src/engine.ml` — engine API and step_result semantics.
3. `src/rules.ml` and `src/setup.ml` — rules and initial state.
4. `ui_bonsai/src/app.ml` and `ui_bonsai/src/firestore.ml` — UI wiring and persistence.
5. `run.sh`, `bin/dune`, and `src/test/` — build & test flows.

If any section is unclear or you want more examples (small diffs or a brief dev checklist for a common task), tell me which area and I'll iterate.
