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
    ├── initialize.rs            # Workspace discovery, file watcher registration
    ├── initial_build.rs         # Full TypecheckOnly build on startup
    ├── queue.rs                 # Unified debounced queue for all file events (didChange, didSave, didChangeWatchedFiles)
    ├── typecheck.rs             # Single-file typecheck via bsc stdin piping (used by queue and pre-queue fallback)
    ├── file_args.rs             # TypecheckArgs extraction from BuildCommandState
    ├── completion.rs            # Completion via analysis binary subprocess
    ├── hover.rs                 # Hover via analysis binary subprocess
    ├── dependency_closure.rs    # Dependency/dependent graph traversal
    └── notifications.rs         # Custom rescript/buildFinished notification
```

## Invocation

```bash
# Started by editors (VS Code, Neovim, Helix, Zed, etc.)
rescript lsp --stdio

# With verbosity for debugging
rescript -vv lsp --stdio
```

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
              │  didOpen                     ──┼──▶ Enqueue typecheck into unified queue
              │  didChange                   ──┼──▶ Enqueue typecheck into unified queue
              │  didSave                     ──┼──▶ Enqueue build into unified queue
              │  didChangeWatchedFiles       ──┼──▶ Enqueue build into unified queue
              │  completion / hover / etc.   ──┼──▶ Shell out to analysis binary
              │  shutdown                    ──┼──▶ Ok(())
              └─────────────────────────────────┘
```

## Build Integration

### Initial Build (on `initialized`)

The initial build runs `TypecheckOnly` — it produces `.cmi` and `.cmt` files but no `.cmj` or `.js`. All modules end up at `CompilationStage::TypeChecked`. This establishes the baseline for incremental builds.

Implementation: `lsp/initial_build.rs`

### Unified Queue

All file events flow into a single unified queue (`lsp/queue.rs`). The queue accepts two kinds of events:

- **Typecheck** (from `didChange` / `didOpen`) — unsaved buffer content, lightweight typecheck only
- **Build** (from `didSave` / `didChangeWatchedFiles`) — saved file, full incremental build from disk

A single background consumer task collects events into a `HashMap<Url, PendingFile>`, applying promotion rules to consolidate per-file state. After 100ms of silence the batch is flushed sequentially:

1. **Builds first** — saved files get a full incremental build (compile dependencies + typecheck dependents), holding the projects lock.
2. **Typechecks second** — unsaved edits get a lightweight typecheck via `bsc -bs-read-stdin`, with brief lock for arg extraction only.
3. **Post-build recheck** — if a saved file also had unsaved buffer content (didChange + didSave in the same debounce window), a typecheck pass runs from the buffer so diagnostics match the editor.
4. **`buildFinished` notification** — sent only when builds ran.

Sequential execution within one consumer eliminates all races on `lib/lsp/` artifacts.

#### Event promotion rules

When multiple events arrive for the same file within a debounce window:

| Existing | New event | Result |
|----------|-----------|--------|
| Typecheck | Typecheck | Keep latest buffer content |
| Typecheck | Build | Promote to Build, stash buffer for post-build recheck |
| Build | Typecheck | Stay Build, update stashed buffer |
| Build | Build | No-op |

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

Implementation: `lsp/queue.rs` (queue, batch logic, build/typecheck flush), `lsp/typecheck.rs` (single-file bsc invocation), `lsp/file_args.rs` (compiler arg extraction)

### On `didSave` / `didChangeWatchedFiles` (saved files)

The `didSave` and `didChangeWatchedFiles` handlers enqueue Build events into the unified queue and return immediately. The consumer groups files by project root and runs a batched build per project:

```
didSave / didChangeWatchedFiles notification
    │
    └── Enqueue Build event(s) into unified queue
                    │
          background consumer task
                    │
          ┌─────────▼──────────┐
          │ collect into HashMap │  (deduplicate per URI, apply promotion)
          │ reset 100ms timer    │
          └─────────┬──────────┘
                    │ timer expires
                    ▼
          group files by project root
                    │
          for each project (sequential):
          ├── mark_file_parse_dirty per file
          ├── Phase 1: compile_dependencies
          │   ├── get_dependency_closure (DFS downward through module.deps)
          │   ├── Temporarily promote non-closure TypeChecked modules → Built
          │   ├── incremental_build(TypecheckAndEmit, only_incremental=true)
          │   └── Restore promoted modules → TypeChecked
          ├── Phase 2: typecheck_dependents
          │   ├── get_dependent_closure (DFS upward through module.dependents)
          │   ├── Mark each dependent as Dirty
          │   ├── Temporarily promote non-dependent TypeChecked modules → Built
          │   ├── incremental_build(TypecheckOnly, only_incremental=true)
          │   └── Restore promoted modules → TypeChecked
          └── Publish combined diagnostics from both phases
                    │
                    ▼
          Post-build recheck (if any Build file had stashed buffer content)
                    │
                    ▼
          Send rescript/buildFinished notification
```

`didChangeWatchedFiles` filters for `Changed` events on `.res`/`.resi` files only. Created/Deleted events are not yet handled — they need module graph updates.

Implementation: `lsp/queue.rs`, `lsp/dependency_closure.rs`

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

File watcher events (`workspace/didChangeWatchedFiles`) for `Changed` events are fed into the unified queue as Build events, just like `didSave`. Created/Deleted events are not yet handled — see below.

Implementation: `lsp/initialize.rs`

### Not yet implemented

- **File creation** (`Created` events): New `.res` files need to be added as modules, requiring re-running source discovery for the affected package.
- **File deletion** (`Deleted` events): Modules should be removed from the build state, and dependents re-typechecked.
- **`rescript.json` changes**: Config changes likely need a full re-initialization (re-read config, re-discover packages).

## What Is NOT Implemented Yet

| Feature | Status |
|---------|--------|
| `textDocument/didOpen` | Implemented |
| `textDocument/didClose` | Implemented |
| `textDocument/didChange` | Implemented |
| `textDocument/didSave` | Implemented |
| `textDocument/completion` | Implemented (analysis) |
| `textDocument/completion/resolve` | Implemented (analysis) |
| `textDocument/hover` | Implemented (analysis) |
| `textDocument/formatting` | Implemented |
| `textDocument/definition` | Implemented (analysis) |
| `textDocument/typeDefinition` | Implemented (analysis) |
| `textDocument/references` | Implemented (analysis) |
| `textDocument/rename` / `prepareRename` | Implemented (analysis) |
| `textDocument/documentSymbol` | Implemented (analysis) |
| `textDocument/codeAction` | Implemented (analysis) |
| `textDocument/signatureHelp` | Implemented (analysis) |
| `textDocument/semanticTokens` | Implemented (analysis) |
| `textDocument/inlayHint` | Implemented (analysis) |
| `textDocument/codeLens` | Implemented (analysis) |
| `workspace/didChangeWatchedFiles` | Implemented (Changed events only) |
| Monorepo multi-workspace | Implemented |
| `textDocument/createInterface` | TODO — niche feature, generates `.resi` from `.res` |
| `textDocument/openCompiled` | TODO — niche feature, opens compiled `.js` output |
| File creation/deletion handling | TODO — needs module graph updates |
| `rescript.json` change handling | TODO — needs full re-initialization |

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
    documentSelector: [
      { scheme: "file", language: "rescript" },
    ],
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

## Open Questions

1. **Cold start performance**: The initial build blocks the `initialized` handler. Large projects may experience a delay before diagnostics appear.

## Prior Art

| Language | CLI build | LSP | Architecture |
|----------|-----------|-----|--------------|
| **Gleam** | `gleam build` | `gleam lsp` (same binary) | Single binary, LSP is the watcher |
| **Rust** | `cargo build` | `rust-analyzer` (separate) | LSP does its own analysis |
| **TypeScript** | `tsc` | `tsserver` (same package) | LSP and CLI share code but no runtime state |
| **Zig** | `zig build` | `zls` (separate) | LSP uses compiler as library |
| **Go** | `go build` | `gopls` (same module) | LSP wraps the compiler toolchain |
