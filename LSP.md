# `rescript lsp` — A Unified Build & Analysis Process

## Overview

The `lsp` subcommand replaces the previous three-process architecture (rewatch + Node.js LSP server + analysis binary) with a single long-running Rust process that owns the build state and speaks LSP protocol over stdio.

```
# Before
rescript watch          # Rust process: file watching, incremental builds
rescript-vscode server  # Node.js process: LSP protocol, orchestration
rescript-editor-analysis # OCaml process: hover, completion, references (one-shot per request)

# Now
rescript lsp            # Single Rust process: LSP protocol + builds + completion
rescript build          # One-shot build (unchanged, for CI)
```

## Architecture

```
┌─────────────────────────────────────────────────────┐
│  rescript lsp                                        │
│  (Single Rust process, tower-lsp)                    │
│                                                      │
│  ┌─────────────┐  ┌──────────────┐  ┌─────────────┐ │
│  │ LSP Protocol │  │ Build Engine │  │  Completion  │ │
│  │ (JSON-RPC)   │  │ (rewatch)    │  │  (analysis   │ │
│  │              │  │              │  │   binary)    │ │
│  │ stdio        │  │ - Parse      │  │              │ │
│  │              │  │ - Deps       │  │ Shells out   │ │
│  │ Handles:     │  │ - Compile    │  │ to rescript- │ │
│  │ - didChange  │  │              │  │ editor-      │ │
│  │ - didSave    │  │ Owns:        │  │ analysis.exe │ │
│  │ - completion │  │ - BuildState │  │ via stdin/   │ │
│  │ - diagnostics│  │ - Dep graph  │  │ stdout JSON  │ │
│  │              │  │ - Modules    │  │              │ │
│  └──────┬───────┘  └──────┬───────┘  └──────┬──────┘ │
│         │                 │                  │        │
│         └─────────────────┴──────────────────┘        │
│                    Shared in-process state             │
│                                                       │
│  Backend {                                            │
│    client: Client,                                    │
│    workspace_folders: RwLock<Vec<String>>,             │
│    projects: Arc<Mutex<ProjectMap>>,                   │
│    queue: Mutex<Option<Queue>>,                        │
│    open_buffers: Mutex<HashMap<Url, String>>,          │
│  }                                                    │
│                                                       │
│  No server-side file watcher needed.                  │
│  The editor watches the filesystem and notifies       │
│  the server via LSP:                                  │
│  - didChange / didSave (open documents)               │
│  - workspace/didChangeWatchedFiles (external changes) │
└─────────────────────────────────────────────────────┘
```

## File Structure

```
rewatch/src/
├── lsp.rs                       # Backend struct, LanguageServer impl, diagnostics publishing, run_stdio()
└── lsp/
    ├── analysis.rs              # Shared context-building and analysis binary spawning
    ├── code_action.rs           # Code actions via analysis binary subprocess
    ├── code_lens.rs             # Code lens via analysis binary subprocess
    ├── completion.rs            # Completion via analysis binary subprocess
    ├── definition.rs            # Go-to-definition via analysis binary subprocess
    ├── dependency_closure.rs    # Dependency/dependent graph traversal
    ├── diagnostic_store.rs      # Diagnostic storage and idle tracking for HTTP endpoint
    ├── document_symbol.rs       # Document symbols via analysis binary subprocess
    ├── file_args.rs             # TypecheckArgs extraction from BuildCommandState
    ├── formatting.rs            # Document formatting via bsc
    ├── hover.rs                 # Hover via analysis binary subprocess
    ├── http.rs                  # HTTP diagnostics endpoint server
    ├── initial_build.rs         # Full TypecheckOnly build on startup
    ├── initialize.rs            # Workspace discovery, file watcher registration
    ├── inlay_hint.rs            # Inlay hints via analysis binary subprocess
    ├── notifications.rs         # Custom rescript/buildFinished notification
    ├── queue.rs                 # Unified debounced queue: types, public API, consumer, merge, flush orchestration
    ├── queue/
    │   ├── file_build.rs        # Per-file incremental build (dependency + dependent closure)
    │   ├── file_typecheck.rs    # Per-file typecheck (parallel, wave-based, staleness-aware)
    │   └── project_build.rs     # Per-project full rebuild + artifact cleanup on file creation/deletion
    ├── references.rs            # Find references via analysis binary subprocess
    ├── rename.rs                # Rename via analysis binary subprocess
    ├── semantic_tokens.rs       # Semantic tokens via analysis binary subprocess
    ├── signature_help.rs        # Signature help via analysis binary subprocess
    ├── type_definition.rs       # Go-to-type-definition via analysis binary subprocess
    └── typecheck.rs             # Single-file typecheck via bsc stdin piping (used by queue and pre-queue fallback)
```

## Invocation

```bash
# Started by editors (VS Code, Neovim, Helix, Zed, etc.)
rescript lsp --stdio

# With verbosity for debugging
rescript -vv lsp --stdio
```

## Initialization Options

The server accepts optional configuration via `initializationOptions` in the `initialize` request:

| Key                 | Type     | Default | Description                                                                                                                                                                                                                                                          |
| ------------------- | -------- | ------- | -------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `queue_debounce_ms` | `number` | `100`   | Debounce timeout in milliseconds for the unified queue. Lower values give faster feedback; higher values batch more events.                                                                                                                                          |
| `diagnostics_http`  | `number` | —       | **(Experimental)** Port for the HTTP diagnostics server. When set, starts an HTTP server on this port that exposes current diagnostics. Use a fixed port per project for predictable URLs. See [HTTP Diagnostics Endpoint](#http-diagnostics-endpoint-experimental). |

Example (Zed `settings.json`):

```json
{
  "lsp": {
    "rescript-lsp": {
      "initialization_options": {
        "queue_debounce_ms": 50
      }
    }
  }
}
```

### Diagnostic Server endpoint

The clue of the diagnostic server endpoint is that it exposes current diagnostics from the IDE.
You could wire this up in a Claude hook, example `.claude/settings.local.json`:

```json
{
  "hooks": {
    "Stop": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "curl -s http://127.0.0.1:12303/diagnostics"
          }
        ]
      }
    ],
    "Edit": [
      {
        "matcher": "\\.res$",
        "hooks": [
          {
            "type": "command",
            "command": "curl -s http://127.0.0.1:12303/diagnostics"
          }
        ]
      }
    ],
    "Write": [
      {
        "matcher": "\\.res$",
        "hooks": [
          {
            "type": "command",
            "command": "curl -s http://127.0.0.1:12303/diagnostics"
          }
        ]
      }
    ]
  }
}
```

(assuming `diagnostics_http` is `12303`).
This workaround exists because IDEs like Zed don't expose LSP diagnostics to LLM agents yet. See [agentclientprotocol/discussions#495](https://github.com/orgs/agentclientprotocol/discussions/495) for the upstream discussion.

## Capabilities

The LSP server advertises these capabilities during `initialize`:

```
textDocumentSync:       Full (with open_close, save notifications, no include_text)
completionProvider:     { triggerCharacters: [".", ">", "@", "~", "\"", "=", "("] }
hoverProvider:          true
definitionProvider:     true
typeDefinitionProvider: true
referencesProvider:     true
codeActionProvider:     true
renameProvider:         { prepareProvider: true }
documentSymbolProvider: true
inlayHintProvider:      true
signatureHelpProvider:  { triggerCharacters: ["(", ","] }
semanticTokensProvider: full
codeLensProvider:       true
documentFormattingProvider: true
```

## Lifecycle

```
Editor starts ──▶ rescript lsp --stdio
                       │
                       ▼
                  initialize request
                       │
                       ▼
                  Store workspace folder paths from params
                  Return capabilities
                       │
                       ▼
                  initialized notification
                       │
                       ├── Discover packages (ProjectContext, read_packages)
                       ├── Register scoped file watchers via client/registerCapability
                       │   (patterns: **/rescript.json, src/**/*.res, src/**/*.resi etc.)
                       ├── Run initial build (TypecheckOnly)
                       │   ├── extend_with_children (scan source folders)
                       │   ├── prepare_build (compiler info, validation, cleanup)
                       │   ├── Downgrade any Built modules → TypeChecked
                       │   └── incremental_build(TypecheckOnly)
                       ├── Publish diagnostics from build results
                       ├── Recheck open buffers (typecheck against buffer content)
                       ├── Start unified queue (queue::Queue::new)
                       └── Send rescript/buildFinished notification
                       │
                       ▼
              ┌─────────────────────────────────┐
              │   Event Loop                    │
              │                                 │
              │  didOpen                     ──┼──▶ Enqueue BufferOpened into unified queue
              │  didChange                   ──┼──▶ Enqueue BufferChanged into unified queue
              │  didSave                     ──┼──▶ Enqueue FileChangedOnDisk or ConfigChanged
              │  didChangeWatchedFiles       ──┼──▶ Enqueue FileChangedOnDisk / FileCreated / FileDeleted
              │  completion / hover / etc.   ──┼──▶ Shell out to analysis binary
              │  shutdown                    ──┼──▶ Ok(())
              └─────────────────────────────────┘
```

## Build Integration

### Initial Build (on `initialized`)

The initial build runs `TypecheckOnly` — it produces `.cmi` and `.cmt` files but no `.cmj` or `.js`. All modules end up at `CompilationStage::TypeChecked`. This establishes the baseline for incremental builds.

Implementation: `lsp/initial_build.rs`

### Unified Queue

All file events flow into a single unified queue (`lsp/queue.rs`). The queue accepts fact-based events describing what happened — the merge logic derives the correct build actions:

- **BufferOpened / BufferChanged** (from `didOpen` / `didChange`) — unsaved buffer content
- **FileChangedOnDisk** (from `didSave` / `didChangeWatchedFiles` Changed) — file changed on disk
- **FileCreated / FileDeleted** (from `didChangeWatchedFiles` Created/Deleted) — structural change requiring full project rebuild
- **ConfigChanged** (from `didSave` / `didChangeWatchedFiles` for `rescript.json`) — config change requiring full project rebuild

A single background consumer task collects events into a `PendingState` with three action-oriented fields:

- `typechecks` — files needing typecheck (unsaved buffer content)
- `compile_files` — files needing incremental build (saved to disk)
- `build_projects` — file paths whose creation/deletion requires a full project rebuild
- `config_changed` — `rescript.json` paths requiring full project re-initialization

The merge function applies promotion rules to consolidate per-file state. When a `FileCreated` or `FileDeleted` event arrives, any pending per-file typecheck or build for that same file is removed since the full rebuild will cover it. A `ConfigChanged` event clears pending typechecks for the same project (since the rebuild will produce fresh type information) but keeps pending compile_files. After 100ms of silence the batch is flushed sequentially:

1. **Full builds first** — re-initialize affected projects from scratch (same flow as the initial build), replacing the old `BuildCommandState`. Clears stale diagnostics for files that no longer exist.
2. **Incremental builds second** — saved files get a full incremental build (compile dependencies + typecheck dependents), holding the projects lock.
3. **Typechecks third** — unsaved edits get a lightweight typecheck via `bsc -bs-read-stdin`, with brief lock for arg extraction only.
4. **Post-build recheck** — if a saved file also had unsaved buffer content (didChange + didSave in the same debounce window), a typecheck pass runs from the buffer so diagnostics match the editor.
5. **`buildFinished` notification** — sent when full builds or incremental builds ran.

Sequential execution within one consumer eliminates all races on `lib/lsp/` artifacts.

#### Event promotion rules

When multiple events arrive for the same file within a debounce window:

| Existing state | New event               | Result                                                                          |
| -------------- | ----------------------- | ------------------------------------------------------------------------------- |
| typechecks     | BufferChanged           | Update content and generation                                                   |
| typechecks     | FileChangedOnDisk       | Promote to compile_files, stash buffer for post-build recheck                   |
| compile_files  | BufferChanged           | Stay in compile_files, update stashed buffer                                    |
| compile_files  | FileChangedOnDisk       | No-op                                                                           |
| any            | FileCreated/FileDeleted | Remove from typechecks/compile_files, add to build_projects                     |
| typechecks     | ConfigChanged           | Clear typechecks for same project (rebuild produces fresh types), add to config |
| compile_files  | ConfigChanged           | Keep compile_files, add to config                                               |

#### Staleness detection

A generation counter (`AtomicU64`) detects stale typecheck results — if a newer `didChange` for the same file arrived while the batch was running, the old result is discarded. Build results are authoritative and always published. Generation entries are pruned after publishing to prevent unbounded growth.

### On `didChange` / `didOpen` (unsaved edits)

Buffer content is stored in `open_buffers` immediately (for completion/hover), then the file is enqueued as a Typecheck event into the unified queue.

**Single-file fast path**: For a single file (the common keystroke case), one `bsc -bs-read-stdin -bs-cmi-only` invocation runs — identical to a direct bsc call. No JS output, no dependent recompilation.

**Multi-file batch**: When multiple files are edited within the debounce window (e.g. LLM agents editing several files at once), the batch runs in three phases:

1. **Parse in parallel** — `bsc -bs-ast -bs-read-stdin` produces `.ast` files with dependency information.
2. **Compute in-batch deps** — Read module names from `.ast` files (via `deps::read_raw_deps`) and intersect with the batch to find ordering constraints.
3. **Typecheck in waves** — Files with no in-batch deps go first (parallel via rayon), then files whose deps are satisfied, etc. Each wave writes `.cmi` to `lib/lsp/` so the next wave sees updated types.

**Pre-queue fallback**: Before the initial build completes (queue not yet created), `didOpen` and `didChange` fall back to a synchronous single-file typecheck via `typecheck::run`.

```
didChange/didOpen notification
    │
    ├── Store content in open_buffers[uri]
    └── Enqueue Typecheck event into unified queue
                    │
          background consumer task
                    │
          ┌─────────▼──────────┐
          │ collect into HashMap │  (latest content per file, with promotion)
          │ reset 100ms timer    │
          └─────────┬──────────┘
                    │ timer expires
                    ▼
          lock projects briefly → extract FileContext per file → release lock
                    │
          ┌─────────▼──────────┐
          │ single file?        │──yes──► typecheck_single_file (one bsc, stdin)
          └─────────┬──────────┘
                    │ no (multi-file batch)
                    ▼
          Phase 1: Parse all files in parallel (bsc -bs-ast -bs-read-stdin)
          Phase 2: Read deps from .ast, find in-batch edges
          Phase 3: Typecheck in waves (dependency order, parallel within wave)
                    │
                    ▼
          Publish diagnostics (skip stale results via generation counter)
```

Implementation: `lsp/queue.rs` (queue, merge, flush orchestration), `lsp/queue/file_typecheck.rs` (batch typecheck logic), `lsp/typecheck.rs` (single-file bsc invocation), `lsp/file_args.rs` (compiler arg extraction)

### On `didSave` / `didChangeWatchedFiles` (saved files)

`didSave` enqueues a `FileChangedOnDisk` event for source files, or a `ConfigChanged` event for `rescript.json`. `didChangeWatchedFiles` classifies by event type: `Changed` → `FileChangedOnDisk` (or `ConfigChanged` for `rescript.json`), `Created` → `FileCreated`, `Deleted` → `FileDeleted` (source files only). The consumer flushes sequentially:

```
didSave / didChangeWatchedFiles notification
    │
    ├── rescript.json → Enqueue ConfigChanged event (via didSave)
    ├── Changed  → Enqueue FileChangedOnDisk event
    ├── Created  → Enqueue FileCreated event (file_path only)
    └── Deleted  → Enqueue FileDeleted event (file_path only)
                    │
          background consumer task
                    │
          ┌─────────▼──────────┐
          │ merge into           │
          │ PendingState         │  (typechecks + compile_files + build_projects + config_changed)
          │ reset 100ms timer    │
          └─────────┬──────────┘
                    │ timer expires
                    ▼
          Step 0: Config change builds (per rescript.json)
          ├── Resolve project root from rescript.json path
          ├── Re-initialize project (re-read packages, re-scan sources)
          ├── Replace old BuildCommandState
          └── Publish diagnostics from new build
                    │
                    ▼
          Step 1: Full builds (per project root)
          ├── Group build_projects paths by project root (resolved at flush time)
          ├── Clean up associated files for deleted .res files (.resi + compiled JS)
          ├── Re-initialize project (re-read packages, re-scan sources)
          ├── Replace old BuildCommandState
          ├── Invalidate uri_cache entries
          ├── Publish diagnostics from new build
          └── Clear diagnostics for files that no longer exist (old/new URI diff)
                    │
                    ▼
          Step 2: Incremental builds (per project)
          ├── group files by project root
          ├── mark_file_parse_dirty per file
          ├── Phase 1: compile_dependencies
          │   ├── parse_and_resolve (parse dirty files, resolve deps)
          │   ├── get_dependency_closure (DFS downward through module.deps)
          │   ├── incremental_build with closure as compile universe
          │   └── (compile loop restores any orphan dirty modules on exit)
          ├── Phase 2: typecheck_dependents
          │   ├── get_dependent_closure (DFS upward through module.dependents)
          │   ├── Mark each dependent as Dirty
          │   ├── incremental_build with dependents as compile universe
          │   └── (compile loop restores any orphan dirty modules on exit)
          └── Publish combined diagnostics from both phases
                    │
                    ▼
          Post-build recheck (if any Build file had stashed buffer content)
                    │
                    ▼
          Send rescript/buildFinished notification
```

`didChangeWatchedFiles` filters for `.res`/`.resi` files and `rescript.json`. For source files: `Changed` events trigger incremental builds via `FileChangedOnDisk`, `Created` and `Deleted` events trigger a full project re-initialization via `FileCreated`/`FileDeleted` events — project root resolution happens at flush time, not at enqueue time. When a `.res` file is deleted, its associated `.resi` and compiled JS files are also cleaned up; deleting a `.resi` alone does not remove the `.res` or JS. For `rescript.json` files: `Changed` events trigger a `ConfigChanged` event that re-initializes the affected project (creation/deletion of `rescript.json` is not handled).

Implementation: `lsp/queue.rs`, `lsp/queue/file_build.rs`, `lsp/queue/project_build.rs`, `lsp/dependency_closure.rs`

### Build Artifacts

The LSP writes build artifacts to `lib/lsp/` (not `lib/bs/`), keeping it independent from `rescript build`. Both produce identical `.js` output to the configured output directory. This separation is the same approach used by Gleam (`build/lsp/` vs `build/dev/`), ensuring the LSP and CLI never interfere with each other's cached state.

## Completion Integration

Completion shells out to `rescript-editor-analysis.exe completion-rewatch`, passing a JSON blob over stdin with all the context the analysis binary needs. This avoids redundant project discovery in the analysis binary.

```
completion request
    │
    ├── Get source from open_buffers (or disk fallback)
    ├── find_module_for_file(file_path)
    ├── If .cmt missing: run did_change typecheck first
    ├── Build JSON blob:
    │   {
    │     "source": "<full source>",
    │     "path": "<absolute path>",
    │     "pos": [line, character],
    │     "rootPath": "<package root>",
    │     "namespace": "<ns or null>",
    │     "suffix": ".js",
    │     "rescriptVersion": [13, 0],
    │     "genericJsxModule": "<name or null>",
    │     "opens": [[...], ...],
    │     "pathsForModule": { ... },
    │     "projectFiles": [...],
    │     "dependenciesFiles": [...]
    │   }
    ├── Spawn rescript-editor-analysis.exe completion-rewatch
    ├── Pipe JSON to stdin
    └── Parse JSON completion items from stdout
```

The `opens` list includes:

1. `["Stdlib", "place holder"]` and `["Pervasives", "JsxModules", "place holder"]` (unless `-nopervasives`)
2. `[namespace_name, "place holder"]` if there's a namespace
3. From `-open Foo.Bar` in compiler_flags: `["Foo", "Bar", "place holder"]`

The `pathsForModule` maps each module name to its `.cmt`/`.res` paths (and `.cmti`/`.resi` for modules with interfaces), using the `lib/lsp` build path.

Implementation: `lsp/completion.rs`

## Diagnostics Publishing

Diagnostics are published via `textDocument/publishDiagnostics`. Build diagnostics are grouped by file URI, converted from 1-based bsc positions to 0-based LSP positions, and published for all touched files (including empty diagnostics to clear stale errors).

Typecheck diagnostics go through a staleness check: if a newer `didChange` arrived for the same file while the typecheck was running, the result is discarded.

Implementation: `lsp.rs` — `group_by_file()` and `to_lsp_diagnostic()`

## HTTP Diagnostics Endpoint (Experimental)

> **Status:** Experimental. This feature may change or be removed. It exists to explore whether external tools (e.g. LLM agents like Claude Code) can leverage the running LSP to verify code changes without triggering a separate build.

When `diagnostics_http` is set to a port number in `initializationOptions`, the LSP spawns a lightweight HTTP server on `127.0.0.1` using that port. This lets you assign a predictable port per project. The port is logged via `window/logMessage` so you can confirm it in the editor's LSP log:

```
rescript-lsp HTTP diagnostics server listening on http://127.0.0.1:54321/diagnostics
```

### Endpoint

`GET /diagnostics` — returns the current diagnostics as JSON. Files with no diagnostics are omitted. An empty `{}` response means the project is clean.

The endpoint **blocks** until the LSP is idle (no pending events, no flush in progress) or until a 30-second timeout expires. This means you can save a file and immediately curl — the response will wait for the build to finish before returning. The `X-Build-Status` response header is `idle` on success or `timeout` if the deadline was reached.

```json
{
  "/absolute/path/to/File.res": [
    {
      "range": {
        "start": { "line": 5, "character": 10 },
        "end": { "line": 5, "character": 20 }
      },
      "severity": "error",
      "message": "This has type: string\nBut it's expected to have type: int"
    }
  ]
}
```

### Usage

```bash
curl -s http://127.0.0.1:54321/diagnostics | jq .
```

### Idle tracking

The store uses a pending-ID model: each queue event increments a monotonic ID. When a flush completes, it only signals "idle" if no newer events arrived during the flush. A `FlushGuard` ensures the signal fires even if the flush panics. This prevents the HTTP endpoint from returning stale results when rapid edits overlap with builds.

Implementation: `lsp/diagnostic_store.rs`, `lsp/http.rs`

## Custom Notifications

### `rescript/buildFinished`

Sent after the initial build completes and after each unified queue flush that included builds. Clients can use this to update UI (e.g., disable loading spinners). Not sent after typecheck-only flushes.

Implementation: `lsp/notifications.rs`

## File Watcher Registration

On `initialized`, the server discovers packages and registers scoped file watchers via `client/registerCapability`. For each package, glob patterns are derived from `rescript.json` source directories:

- `**/rescript.json` — always registered
- `src/**/*.res`, `src/**/*.resi` — for recursive source dirs
- `src/*.res`, `src/*.resi` — for flat source dirs

Patterns use forward slashes and are scoped to declared source directories to avoid matching `node_modules/` or `lib/bs/`.

File watcher events (`workspace/didChangeWatchedFiles`) are classified by event type and fed into the unified queue: `Changed` → `FileChangedOnDisk`, `Created` → `FileCreated`, `Deleted` → `FileDeleted`.

Implementation: `lsp/initialize.rs`

### `rescript.json` changes

Config changes (e.g. suffix, package-spec) trigger a full project re-initialization: the affected project is re-read from disk, packages are re-discovered, and a fresh `TypecheckOnly` build runs. Any pending typechecks scoped to that project are cleared to avoid stale results. The `rescript.json` watcher pattern is already registered alongside source file patterns.

## What Is NOT Implemented Yet

| Feature                                 | Status                                                                                  |
| --------------------------------------- | --------------------------------------------------------------------------------------- |
| `textDocument/didOpen`                  | Implemented                                                                             |
| `textDocument/didClose`                 | Implemented                                                                             |
| `textDocument/didChange`                | Implemented                                                                             |
| `textDocument/didSave`                  | Implemented                                                                             |
| `textDocument/completion`               | Implemented (analysis)                                                                  |
| `textDocument/completion/resolve`       | Implemented (analysis)                                                                  |
| `textDocument/hover`                    | Implemented (analysis)                                                                  |
| `textDocument/formatting`               | Implemented                                                                             |
| `textDocument/definition`               | Implemented (analysis)                                                                  |
| `textDocument/typeDefinition`           | Implemented (analysis)                                                                  |
| `textDocument/references`               | Implemented (analysis)                                                                  |
| `textDocument/rename` / `prepareRename` | Implemented (analysis)                                                                  |
| `textDocument/documentSymbol`           | Implemented (analysis)                                                                  |
| `textDocument/codeAction`               | Implemented (analysis)                                                                  |
| `textDocument/signatureHelp`            | Implemented (analysis)                                                                  |
| `textDocument/semanticTokens`           | Implemented (analysis)                                                                  |
| `textDocument/inlayHint`                | Implemented (analysis)                                                                  |
| `textDocument/codeLens`                 | Implemented (analysis)                                                                  |
| `workspace/didChangeWatchedFiles`       | Implemented (Changed, Created, Deleted)                                                 |
| Monorepo multi-workspace                | Implemented                                                                             |
| `textDocument/createInterface`          | TODO — niche feature, generates `.resi` from `.res` (already exists in analysis binary) |
| `textDocument/openCompiled`             | TODO — niche feature, opens compiled `.js` output                                       |
| File creation/deletion handling         | Implemented (FileCreated/FileDeleted queue events)                                      |
| `rescript.json` change handling         | Implemented (full re-initialization on config change)                                   |
| `workspace/symbol`                      | TODO — project-wide symbol search                                                       |
| `textDocument/documentHighlight`        | TODO — highlight all occurrences of a symbol in current file                            |
| `textDocument/foldingRange`             | TODO — code folding regions                                                             |
| `textDocument/selectionRange`           | TODO — smart expand/shrink selection                                                    |
| `window/workDoneProgress`               | TODO — progress indicator during initial build                                          |

## Relationship to `rescript build`

`rescript build` remains a standalone one-shot command for CI. It writes to `lib/bs/`. The LSP writes to `lib/lsp/`. They do not share state or coordinate.

`.js` output goes to the same configured output directory from both. The output is deterministic — the last writer wins with identical content.

## What the Editor Extension Becomes

The extension becomes a thin LSP client:

```typescript
import { LanguageClient } from "vscode-languageclient/node";

export function activate(context: ExtensionContext) {
  const serverOptions: ServerOptions = {
    command: "rescript",
    args: ["lsp", "--stdio"],
  };

  const clientOptions: ClientOptions = {
    documentSelector: [{ scheme: "file", language: "rescript" }],
  };

  const client = new LanguageClient(
    "rescript",
    "ReScript",
    serverOptions,
    clientOptions,
  );

  client.start();
}
```

## Next Up

### 1. Prompt for Full JS Rebuild

After certain events (initial build, `rescript.json` config change), only a typecheck-only build runs. The user must save a source file to trigger JS emission. We could use `window/showMessageRequest` to ask the user if they want a full JS rebuild — similar to what the old VS Code extension did. This would improve the experience for config changes like suffix or package-spec modifications where the user expects all JS output to be regenerated.

### 2. Cold Start Performance

The initial build blocks the `initialized` handler. Large projects may experience a delay before diagnostics appear. Options:

- Add `window/workDoneProgress` notifications so the editor shows a loading indicator.
- Explore making the initial build non-blocking (serve analysis from partial state, then refresh).

### 3. `createInterface` / `openCompiled` Commands

- **`createInterface`**: Already exists in the analysis binary. Expose as an LSP command or code action.
- **`openCompiled`**: Open the compiled `.js` output for a `.res` file. Useful for debugging.

### 4. File Operation Notifications (LSP 3.16)

Register support for `workspace/didDeleteFiles`, `workspace/didCreateFiles`, and `workspace/didRenameFiles` via `fileOperations` in server capabilities. These are sent by the editor when it performs file operations itself (e.g., delete from file tree, rename via UI), unlike `didChangeWatchedFiles` which relies on the editor's file watcher. Zed, for example, does not fire `didChangeWatchedFiles` for deletions, but may support `didDeleteFiles`. Supporting both provides redundant coverage across editors.

### 5. Additional LSP Features

Lower priority, but would improve the editing experience:

- **`workspace/symbol`** — project-wide symbol search
- **`textDocument/documentHighlight`** — highlight all occurrences of a symbol in the current file
- **`textDocument/foldingRange`** — code folding regions
- **`textDocument/selectionRange`** — smart expand/shrink selection

## Potential Carve-Outs

The following changes on this branch are self-contained and could be sent as separate PRs ahead of merging the full LSP branch:

### Rewatch test infrastructure

`f56353869`..`d78103524` — introduces OpenTelemetry tracing in rewatch, a Vitest-based integration test framework with OTEL span snapshots, sandbox isolation, and fixture-based test projects. Foundation for all LSP tests but independently valuable for testing build/watch/clean/format. See also [#8241](https://github.com/rescript-lang/rescript/pull/8241).

### `-bs-read-stdin` flag for bsc

`05398ce52`, `88e4b8678` — adds a `-bs-read-stdin` flag to bsc that reads source from stdin instead of from the file on disk. The filename argument is still required for error locations and output path derivation. Touches the compiler across several layers:

- `Js_config.read_stdin` flag and `-bs-read-stdin` CLI option
- `Res_io.read_stdin` and `Res_driver.parse_*_from_stdin` for parsing from stdin
- `Res_multi_printer.print_source` for formatting source provided as a string (used by `-format -bs-read-stdin`)
- `Clflags.skip_source_digest` to skip `Digest.file` when the source file doesn't match stdin content
- `Location.stdin_source` to hold stdin content for accurate error reporting code frames

Used by the LSP to pipe unsaved buffer content directly to bsc without writing temporary files.

### Skip JS write when content is unchanged

`compiler/core/lam_compile_main.ml` — render JS output to a buffer, compare with the existing file on disk, and only write when the content differs. Avoids unnecessary file writes that trigger bundler HMR, file watchers, and editor reload events.

### Remove `compiler-args` CLI subcommand

`b0f7dc034` — the `compiler-args` subcommand was unused and removed. Clean standalone deletion.

### Rewatch build profiles (`lib/lsp-ocaml/` isolation)

`d56f92c36` — introduces `BuildProfile` (Standard / TypecheckOnly / TypecheckAndEmit) and a dedicated `lib/lsp-ocaml/` flat artifact directory so LSP and CLI builds don't interfere with each other. Threads the profile through all artifact path resolution in packages, compile, parse, clean, and compiler-info modules.

### Watcher improvements

- `e63278313` — graceful shutdown via stdin EOF (allows parent process to signal shutdown by closing stdin)
- `a4db5edb6` — deduplicate and normalize file watch events (mtime/content-hash dedup, atomic write normalization across macOS/Linux/Windows)
- `9ba38d7d6`, `5b536cf7d` — preserve original line endings in format output on Windows

### OTEL trace fix

`bd3b38beb` — fix compile_file spans not nested under compile_wave in OTEL traces (thread-local spans need explicit parent when using Rayon's par_iter).

## Prior Art

| Language       | CLI build     | LSP                        | Architecture                                |
| -------------- | ------------- | -------------------------- | ------------------------------------------- |
| **Gleam**      | `gleam build` | `gleam lsp` (same binary)  | Single binary, LSP is the watcher           |
| **Rust**       | `cargo build` | `rust-analyzer` (separate) | LSP does its own analysis                   |
| **TypeScript** | `tsc`         | `tsserver` (same package)  | LSP and CLI share code but no runtime state |
| **Zig**        | `zig build`   | `zls` (separate)           | LSP uses compiler as library                |
| **Go**         | `go build`    | `gopls` (same module)      | LSP wraps the compiler toolchain            |
