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
                       └── Send rescript/buildFinished notification
                       │
                       ▼
              ┌─────────────────────────────┐
              │   Event Loop                │
              │                             │
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

Two-phase incremental build:

**Phase 1 — Compile dependencies** (`TypecheckAndEmit`): Compile the saved file and its transitive imports to produce JS output. Only the dependency closure is compiled — modules outside it are temporarily promoted to `Built` so they are excluded from the compile universe.

**Phase 2 — Typecheck dependents** (`TypecheckOnly`): Re-typecheck modules that transitively import the saved file to surface errors caused by API changes. No JS is emitted for dependents — they get JS when they are themselves saved.

```
didSave notification
    │
    ├── mark_file_parse_dirty(file_path)
    │
    ├── Phase 1: compile_dependencies
    │   ├── get_dependency_closure (DFS downward through module.deps)
    │   ├── Temporarily promote non-closure TypeChecked modules → Built
    │   ├── incremental_build(TypecheckAndEmit, only_incremental=true)
    │   └── Restore promoted modules → TypeChecked
    │
    ├── Phase 2: typecheck_dependents
    │   ├── get_dependent_closure (DFS upward through module.dependents)
    │   ├── Mark each dependent as Dirty
    │   ├── Temporarily promote non-dependent TypeChecked modules → Built
    │   ├── incremental_build(TypecheckOnly, only_incremental=true)
    │   └── Restore promoted modules → TypeChecked
    │
    ├── Publish combined diagnostics from both phases
    └── Send rescript/buildFinished notification
```

Implementation: `lsp/did_save.rs`, `lsp/dependency_closure.rs`

### Build Artifacts

The LSP writes build artifacts to `lib/lsp/` (not `lib/bs/`), keeping it independent from `rescript build`. Both produce identical `.js` output to the configured output directory.

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

**Note**: File watcher events (`workspace/didChangeWatchedFiles`) are registered but **not yet handled** — the notification handler has not been implemented. External file changes (e.g., `git checkout`) are not yet picked up by the LSP.

Implementation: `lsp/initialize.rs`

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
| `textDocument/typeDefinition` | TODO (analysis) | Yes — core navigation |
| `textDocument/references` | TODO (analysis) | Yes — core navigation |
| `textDocument/rename` / `prepareRename` | TODO (analysis) | Yes — core refactoring |
| `textDocument/completion/resolve` | TODO (analysis) | Yes — populates documentation for selected completion item |
| `textDocument/documentSymbol` | TODO (analysis) | Yes — powers outline view and breadcrumbs |
| `textDocument/codeAction` | TODO (analysis) | Yes — quick fixes from diagnostics and analysis |
| `textDocument/signatureHelp` | TODO (analysis) | Yes — shows function parameter info while typing |
| `textDocument/semanticTokens` | TODO (analysis) | Nice to have — enhanced syntax highlighting |
| `textDocument/inlayHint` | TODO (analysis) | Nice to have — inline type annotations, off by default in old LSP |
| `textDocument/codeLens` | TODO (analysis) | Nice to have — off by default in old LSP |
| `textDocument/createInterface` | TODO (analysis) | Low priority — niche feature, generates `.resi` from `.res` |
| `textDocument/openCompiled` | TODO | Low priority — niche feature, opens compiled `.js` output |
| `diagnosticSyntax` on `didChange` | TODO (analysis) | Low priority — old LSP ran syntax diagnostics on every keystroke via analysis binary. Our `didChange` already runs `bsc` which catches syntax errors. |
| `workspace/didChangeWatchedFiles` | Registered but handler not implemented | Yes — needs to handle external file changes (git checkout, etc.) |
| Monorepo multi-workspace | Single BuildState only | Yes eventually — needed for monorepo setups |

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

1. **Monorepo support**: Only a single `BuildState` is stored. Multiple workspace folders each trigger a build, but only the last one's state is retained.

2. **Remaining analysis features**: Hover, definition, references, rename, formatting, etc. need to be wired up to the analysis binary (same pattern as completion).

3. **Cold start performance**: The initial build blocks the `initialized` handler. Large projects may experience a delay before diagnostics appear.

## Prior Art

| Language | CLI build | LSP | Architecture |
|----------|-----------|-----|--------------|
| **Gleam** | `gleam build` | `gleam lsp` (same binary) | Single binary, LSP is the watcher |
| **Rust** | `cargo build` | `rust-analyzer` (separate) | LSP does its own analysis |
| **TypeScript** | `tsc` | `tsserver` (same package) | LSP and CLI share code but no runtime state |
| **Zig** | `zig build` | `zls` (separate) | LSP uses compiler as library |
| **Go** | `go build` | `gopls` (same module) | LSP wraps the compiler toolchain |
