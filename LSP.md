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
│    build_state: Mutex<Option<BuildCommandState>>,      │
│    last_diagnostics_files: Mutex<HashSet<Url>>,        │
│    open_buffers: Mutex<HashMap<Url, String>>,          │
│  }                                                    │
│                                                       │
│  No server-side file watcher needed.                  │
│  The editor watches the filesystem and notifies       │
│  the server via LSP:                                  │
│  - didChange / didSave (open documents)               │
│  - workspace/didChangeWatchedFiles (registered but    │
│    not yet handled — see Open Questions)               │
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
    ├── did_save.rs              # Two-phase incremental build on save
    ├── did_change.rs            # Single-file typecheck via bsc stdin piping
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

The LSP server currently advertises these capabilities during `initialize`:

```
textDocumentSync:   Full (with open_close, save notifications, no include_text)
completionProvider: { triggerCharacters: [".", ">", "@", "~", "\"", "=", "("] }
hoverProvider:      true
```

The following are planned but commented out in the code:

```
definitionProvider, typeDefinitionProvider,
referencesProvider, codeActionProvider, renameProvider (with prepare),
documentSymbolProvider,
inlayHintProvider, signatureHelpProvider
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
                       └── Send rescript/buildFinished notification
                       │
                       ▼
              ┌─────────────────────────────┐
              │   Event Loop                │
              │                             │
              │  didOpen                 ──┼──▶ Single-file typecheck (bsc stdin)
              │                             │    → Publish diagnostics
              │  didChange               ──┼──▶ Single-file typecheck (bsc stdin)
              │                             │    → Publish diagnostics
              │  didSave                 ──┼──▶ Two-phase incremental build
              │                             │    → Publish diagnostics
              │                             │    → Send rescript/buildFinished
              │  completion              ──┼──▶ Shell out to analysis binary
              │  shutdown                ──┼──▶ Ok(())
              └─────────────────────────────┘
```

## Build Integration

### Initial Build (on `initialized`)

The initial build runs `TypecheckOnly` — it produces `.cmi` and `.cmt` files but no `.cmj` or `.js`. All modules end up at `CompilationStage::TypeChecked`. This establishes the baseline for incremental builds.

Implementation: `lsp/initial_build.rs`

### On `didChange` (unsaved edits)

Single-file typecheck using `bsc -bs-read-stdin`. The unsaved buffer content is piped to bsc's stdin. No JS output, no dependent recompilation — just diagnostics for the edited file.

**Future optimization — debounced typechecking**: Currently every `didChange` immediately spawns bsc. During fast typing this creates wasted work since intermediate keystrokes produce incomplete code that will be superseded milliseconds later.

Gleam solves this with a `MessageBuffer` that collects incoming notifications and uses a **100ms debounce timer** — when 100ms passes with no new messages, it flushes the batch and appends a compile request. Only the latest buffer content for each file is compiled. For LSP requests (hover, completion), compilation is triggered immediately before handling the request so responses always reflect the current state.

Our approach could be simpler since we typecheck single files rather than the whole project: store the latest `didChange` content in `open_buffers` immediately (for completion/hover), but delay the bsc typecheck spawn behind a per-file debounce timer (e.g. 100-200ms). If a new `didChange` arrives for the same file before the timer fires, reset the timer. A `tokio::time::sleep` future that gets cancelled/replaced on each new change would work. Requests like hover/completion that need fresh diagnostics could force an immediate flush.

```
didChange notification (full document content)
    │
    ├── Store content in open_buffers[uri]
    ├── find_module_for_file(file_path)
    ├── compiler_args() for the module (TypecheckOnly profile)
    ├── Insert -bs-read-stdin before last arg (source file path)
    ├── Spawn bsc, pipe content to stdin
    ├── Parse stderr → Vec<BscDiagnostic>
    ├── Remap relative paths to absolute
    └── Publish diagnostics
```

Implementation: `lsp/did_change.rs`

### On `didSave` (saved file)

The `didSave` handler sends the file path into a debounced build queue and returns immediately. A background consumer task collects paths, deduplicates them, and after 150ms of silence flushes one batched build per project.

```
didSave notification ──► queue file path ──► return immediately
                              │
                    background consumer task
                              │
                    ┌─────────▼──────────┐
                    │ collect into HashSet │
                    │ reset 150ms timer    │
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
                    ├── Publish combined diagnostics from both phases
                    └── Send rescript/buildFinished notification
```

**TODO**: When multiple projects are affected by a single flush, builds currently run sequentially because they share a `Mutex<ProjectMap>`. Per-project locks would enable parallel builds across projects.

Implementation: `lsp/build_queue.rs`, `lsp/dependency_closure.rs`

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

Diagnostics are published via `textDocument/publishDiagnostics`. The `Backend::publish_diagnostics` method:

1. Groups `BscDiagnostic` items by file URI
2. Converts 1-based bsc positions to 0-based LSP positions
3. Computes stale URIs (had diagnostics before, don't now) and publishes empty diagnostics to clear them
4. Publishes current diagnostics for all affected files
5. Updates the tracking set (`last_diagnostics_files`) for the next cycle

On initial build, no stale clearing is performed (the editor starts with a clean slate).

Implementation: `lsp.rs` — `Backend::publish_diagnostics()` and `to_lsp_diagnostic()`

## Custom Notifications

### `rescript/buildFinished`

Sent after the initial build completes and after each `didSave` incremental build. Clients can use this to update UI (e.g., disable loading spinners). Not sent after `didChange` (which is a lightweight single-file typecheck).

Implementation: `lsp/notifications.rs`

## File Watcher Registration

On `initialized`, the server discovers packages and registers scoped file watchers via `client/registerCapability`. For each package, glob patterns are derived from `rescript.json` source directories:

- `**/rescript.json` — always registered
- `src/**/*.res`, `src/**/*.resi` — for recursive source dirs
- `src/*.res`, `src/*.resi` — for flat source dirs

Patterns use forward slashes and are scoped to declared source directories to avoid matching `node_modules/` or `lib/bs/`.

**Note**: File watcher events (`workspace/didChangeWatchedFiles`) are registered but **not yet handled** — see plan below.

Implementation: `lsp/initialize.rs`

### Plan: `workspace/didChangeWatchedFiles` + debounced `didSave`

External file changes (git checkout, terminal edits, LLM agents, formatters) arrive as `workspace/didChangeWatchedFiles` notifications. These carry a batch of `FileEvent`s, each with a URI and a change type (Created, Changed, Deleted).

These events require the same work as `didSave`: mark modules dirty, compile dependency closures, typecheck dependent closures. The key difference is that multiple files change at once, and they should be handled as a single batch for efficiency.

#### Batched save/rebuild

Generalize `did_save::run` to accept multiple files:

1. Mark all changed files as parse-dirty (call `mark_file_parse_dirty` for each)
2. Compute the **union** of all dependency closures → one `TypecheckAndEmit` build
3. Compute the **union** of all dependent closures → one `TypecheckOnly` build
4. Publish diagnostics for all touched files
5. Recheck open buffers whose on-disk dependencies may have changed

This gives 2 incremental builds total instead of 2N. The same batched function serves both:
- `didSave` — batch of one (or multiple with "Save All")
- `didChangeWatchedFiles` — batch of N from the editor's file watcher

#### Debouncing watched file events

`didChangeWatchedFiles` can fire rapidly during a `git checkout` (one event per file). Debounce with a short timer (e.g. 100-200ms): collect events, and when the timer expires with no new events, flush the batch and run the batched build.

`didSave` could also participate in this debounce window. When a save arrives, start/reset the timer. If more saves or watched-file events arrive within the window, they join the batch. This handles "Save All" and "format-on-save writes multiple files" efficiently.

#### File creation and deletion

`didChangeWatchedFiles` also reports Created and Deleted events. These require updating the build state's module graph:
- **Created**: A new `.res` file needs to be added as a module. This may require re-running source discovery (`build::load_package_sources`) for the affected package to pick up the new file and establish its dependencies.
- **Deleted**: The module should be removed from the build state. Dependents need to be re-typechecked (they will now have "unbound module" errors).
- **`rescript.json` changed**: If the watcher picks up a `rescript.json` change (we register `**/rescript.json`), the project likely needs a full re-initialization (re-read config, re-discover packages).

#### `didChange` debouncing (separate concern)

`didChange` (unsaved buffer edits) works differently — it runs a single-file bsc stdin typecheck without touching the build state graph. Debounce independently with a per-file timer: store content in `open_buffers` immediately, delay the bsc spawn. Cancel/reset the timer on each new change for the same file.

#### Implementation order

1. ~~Generalize `did_save::run` to accept `&[&Path]` instead of `&Path`~~ Done
2. ~~Add debounced build queue (`build_queue.rs`) with 150ms timer~~ Done
3. Implement `didChangeWatchedFiles` handler — send Changed events into the same build queue
4. Handle Created/Deleted events (module graph updates)
5. Add `didChange` per-file debounce timer (independent)

## What Is NOT Implemented Yet

Comparison with the old Node.js LSP (`rescript-vscode/server/src/server.ts`). Features marked with "(analysis)" shell out to `rescript-editor-analysis.exe`.

| Feature | Status | Worth it? |
|---------|--------|-----------|
| `textDocument/didOpen` | Implemented | - |
| `textDocument/didClose` | Implemented | - |
| `textDocument/didChange` | Implemented | - |
| `textDocument/didSave` | Implemented | - |
| `textDocument/completion` | Implemented (analysis) | - |
| `textDocument/hover` | Implemented (analysis) | - |
| `textDocument/formatting` | Implemented | - |
| `textDocument/definition` | Implemented (analysis) | - |
| `textDocument/typeDefinition` | Implemented (analysis) | - |
| `textDocument/references` | Implemented (analysis) | - |
| `textDocument/rename` / `prepareRename` | Implemented (analysis) | - |
| `textDocument/completion/resolve` | Implemented (analysis) | - |
| `textDocument/documentSymbol` | Implemented (analysis) | - |
| `textDocument/codeAction` | Implemented (analysis) | - |
| `textDocument/signatureHelp` | Implemented (analysis) | - |
| `textDocument/semanticTokens` | Implemented (analysis) | - |
| `textDocument/inlayHint` | Implemented (analysis) | - |
| `textDocument/codeLens` | Implemented (analysis) | - |
| `textDocument/createInterface` | TODO (analysis) | Low priority — niche feature, generates `.resi` from `.res` |
| `textDocument/openCompiled` | TODO | Low priority — niche feature, opens compiled `.js` output |
| `diagnosticSyntax` on `didChange` | TODO (analysis) | Low priority — old LSP ran syntax diagnostics on every keystroke via analysis binary. Our `didChange` already runs `bsc` which catches syntax errors. |
| `workspace/didChangeWatchedFiles` | Registered but handler not implemented | Yes — needs to handle external file changes (git checkout, etc.) |
| Monorepo multi-workspace | Implemented | - |

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
