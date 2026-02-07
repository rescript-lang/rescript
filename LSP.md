# `rescript lsp` — A Unified Build & Analysis Process

## Overview

This document proposes adding an `lsp` subcommand to the `rescript` binary that replaces the current three-process architecture (rewatch + Node.js LSP server + analysis binary) with a single long-running Rust process.

```
# Today
rescript watch          # Rust process: file watching, incremental builds
rescript-vscode server  # Node.js process: LSP protocol, orchestration
rescript-editor-analysis # OCaml process: hover, completion, references (one-shot per request)

# Proposed
rescript lsp            # Single Rust process: LSP protocol + builds + analysis
rescript build          # One-shot build (unchanged, for CI)
```

## Current Architecture and Its Problems

### Three processes, three views of the world

Today, three independent components each parse `rescript.json`, discover packages, and build their own understanding of the project:

```
┌─────────────────┐     .compiler.log     ┌──────────────────┐
│  rescript watch  │ ──────────────────▶   │  Node.js LSP     │
│  (Rust/rewatch)  │                       │  (rescript-vscode)│
│                  │◀── lockfile check ──  │                  │
│  Owns:           │                       │  Owns:           │
│  - BuildState    │     spawn per req     │  - File watchers │
│  - Dep graph     │ ◀─────────────────    │  - Diagnostics   │
│  - Dirty flags   │                       │  - Open files    │
│  - File watcher  │                       │                  │
│  - Lockfile      │                       │                  │
└─────────────────┘                        └────────┬─────────┘
                                                    │ execFileSync
                                                    ▼
                                           ┌──────────────────┐
                                           │  Analysis binary  │
                                           │  (OCaml)          │
                                           │                   │
                                           │  Reads .cmt files │
                                           │  Stateless queries│
                                           └──────────────────┘
```

**Problems with this:**

1. **File-based IPC**: Diagnostics flow through `.compiler.log` — a text file that the LSP must watch and parse. Race conditions are possible during reads/writes.

2. **Redundant project discovery**: All three components independently read `rescript.json` and scan source directories.

3. **Stale state by design**: The LSP reads `.cmt` files that rewatch produced. Between a user edit and the next build completing, analysis results are stale. The `lib/bs/___incremental/` workaround exists solely to paper over this gap.

4. **Lockfile as coordination**: The lockfile exists only because two processes need to avoid stepping on each other. It is a symptom, not a feature.

5. **Subprocess-per-request analysis**: Every hover, completion, or definition spawns a new OCaml process, re-discovers the project, re-reads `.cmt` files, answers, and exits.

## Proposed Architecture

```
┌─────────────────────────────────────────────────────┐
│  rescript lsp                                        │
│  (Single Rust process)                               │
│                                                      │
│  ┌─────────────┐  ┌──────────────┐  ┌─────────────┐ │
│  │ LSP Protocol │  │ Build Engine │  │  Analysis   │ │
│  │ (JSON-RPC)   │  │ (from        │  │  Queries    │ │
│  │              │  │  rewatch)    │  │  (from      │ │
│  │ stdio/pipe   │  │              │  │   analysis  │ │
│  │              │  │ - Parse      │  │   binary)   │ │
│  │ Handles:     │  │ - Deps       │  │             │ │
│  │ - hover      │  │ - Compile    │  │ - Hover     │ │
│  │ - completion │  │              │  │ - Complete  │ │
│  │ - definition │  │ Owns:        │  │ - Refs      │ │
│  │ - diagnostics│  │ - BuildState │  │ - Rename    │ │
│  │ - formatting │  │ - Dep graph  │  │             │ │
│  │ - code action│  │ - Dirty flags│  │ Reads:      │ │
│  │              │  │              │  │ - .cmt files│ │
│  └──────┬───────┘  └──────┬───────┘  └──────┬──────┘ │
│         │                 │                  │        │
│         └─────────────────┴──────────────────┘        │
│                    Shared in-process state             │
│                                                       │
│  No server-side file watcher needed.                  │
│  The editor watches the filesystem and notifies       │
│  the server via LSP:                                  │
│  - didOpen / didChange / didSave (open documents)     │
│  - workspace/didChangeWatchedFiles (all file events,  │
│    including external changes like git checkout)       │
│                                                       │
│  The server reads rescript.json source dirs and        │
│  registers scoped watchers via client/registerCapability│
└─────────────────────────────────────────────────────┘
```

### What changes

| Concern | Today | Proposed |
|---------|-------|----------|
| LSP protocol | Node.js (rescript-vscode) | Rust (in `rescript` binary) |
| Build execution | `rescript watch` (separate process) | In-process, triggered by file events or `didSave` |
| Analysis queries | Subprocess per request (OCaml) | In-process function calls |
| Diagnostics delivery | `.compiler.log` -> file watch -> parse -> publish | Direct: compile result -> LSP `publishDiagnostics` |
| Project discovery | 3x independent | Once, shared `BuildState` |
| File watching | rewatch (`notify`) + LSP (Node.js watchers) | Editor watches, notifies server via LSP protocol |
| Lockfile | Required for coordination | Eliminated |
| `.compiler.log` | Written by rewatch, read by LSP | Eliminated (or optional for `rescript build` only) |
| `lib/bs/___incremental/` | Workaround for stale .cmt files | Eliminated |
| State coherence | Eventually consistent (file-based) | Always consistent (in-memory) |

### What stays the same

- **`rescript build`**: One-shot build for CI. Reads sources, compiles, writes `.js`, exits. No shared state with the LSP, like how `tsc` and `tsserver` coexist independently.
- **`rescript clean`**: Removes build artifacts. Unchanged.
- **`rescript format`**: Formats files. Unchanged.
- **`bsc`**: The underlying compiler. Still invoked for compilation. The build engine calls it, not the user.

## `rescript lsp` — Command Design

### Invocation

```bash
# Started by editors (VS Code, Neovim, Helix, Zed, etc.)
rescript lsp --stdio

# With verbosity for debugging
rescript -vv lsp --stdio
```

The editor extension becomes a thin client — it only needs to spawn `rescript lsp --stdio` and speak standard LSP protocol. No project discovery, no build watcher management, no diagnostics parsing.

### CLI Definition (in `cli.rs`)

```rust
/// Start the language server
Lsp {
    /// Communication channel
    #[bpaf(long("stdio"))]
    stdio: bool,
},
```

### Capabilities

The LSP server advertises these capabilities during `initialize`:

```
textDocumentSync:           Full
hoverProvider:              true
completionProvider:         { triggerCharacters: [".", ">", "@", "~", "\"", "=", "("] }
definitionProvider:         true
typeDefinitionProvider:     true
referencesProvider:         true
documentSymbolProvider:     true
codeActionProvider:         true
renameProvider:             { prepareProvider: true }
documentFormattingProvider: true
semanticTokensProvider:     true
inlayHintProvider:          true
codeLensProvider:           true
signatureHelpProvider:      true
```

### Lifecycle

```
Editor starts ──▶ rescript lsp --stdio
                       │
                       ▼
                  initialize request
                       │
                       ▼
                  Discover project (rescript.json)
                  Initialize BuildState
                  Parse all sources
                  Build dependency graph
                  Initial compilation
                       │
                       ▼
                  initialized notification
                       │
                       ▼
              ┌─────────────────────────────┐
              │   Event Loop                │
              │   (all input from editor)   │
              │                             │
              │  didChangeWatchedFiles    ──┼──▶ File created/deleted/renamed
              │                             │    -> Update BuildState -> Rebuild
              │                             │    -> Push diagnostics
              │  didOpen/didChange        ──┼──▶ Update content -> Syntax check
              │  didSave                  ──┼──▶ Incremental compile -> Push diagnostics
              │  hover/completion/defn    ──┼──▶ Query analysis (in-process)
              │  formatting              ──┼──▶ Format in-process
              │  shutdown                ──┼──▶ Clean up, exit
              └─────────────────────────────┘
```

## Monorepo Behavior

Rewatch already has monorepo awareness through `ProjectContext`, which walks up from the given folder to detect the project scope:

- **From workspace root**: `MonorepoRoot` — builds root + all local packages listed in `dependencies`/`dev-dependencies`
- **From a sub-package**: `MonorepoPackage` — builds only that package, resolves deps from the workspace root's `node_modules`

`rescript lsp` reuses this exact logic. The LSP receives `workspaceFolders` in the `initialize` request — this is whatever the user opened in their editor.

### User opens the workspace root

The LSP creates a single `BuildState` covering the root and all local packages. All files across all packages get diagnostics, analysis, etc. This is the simple case.

### User opens a single package

The LSP creates a `BuildState` scoped to that package (the `MonorepoPackage` path in `ProjectContext`). It builds only that package and its dependencies.

If the user then opens a file from a sibling package (via "go to definition" or manually), the LSP has two options:

1. **Treat it as read-only** — provide analysis (hover, definition) using the sibling's existing `.cmt` files from `lib/bs/` (produced by a prior `rescript build`), but do not compile it or provide diagnostics. This is the simpler approach.
2. **Expand scope lazily** — detect the sibling's `rescript.json`, create a second `BuildState`, compile it. This is what `tsserver` does with multiple `tsconfig.json` projects.

For Stage 1, option 1 is sufficient. The user can always open the workspace root for full coverage.

### One LSP instance per editor window

The editor spawns one `rescript lsp` process per window. If the user opens two VS Code windows on different packages in the same monorepo, each gets its own LSP instance with its own `BuildState` and its own `lib/lsp/` artifacts. They do not coordinate — same as two independent `rescript build` runs.

## Build Integration

### On file change (from `didSave` or `didChangeWatchedFiles`)

```
Editor notifies server of file change
    │
    ▼
Mark module parse_dirty in BuildState
    │
    ▼
Re-parse AST (bsc -bs-ast)
    │
    ▼
Update dependency graph if deps changed
    │
    ▼
Expand compile universe (dirty module + dependents)
    │
    ▼
Compile in dependency order
    │
    ▼
For each compiled module:
  - Update BuildState (compile_state, warnings, timestamps)
  - Publish diagnostics immediately via LSP
  - .cmt files available for analysis queries
    │
    ▼
Emit telemetry span (OTEL) for the build
```

### On `didChange` (unsaved edits)

For responsive feedback without waiting for a save:

1. Run syntax-level diagnostics immediately (fast, no compilation needed)
2. Call `compiler_args()` directly in Rust for the affected module — no `build.ninja` parsing, no `rescript compiler-args` subprocess
3. Invoke `bsc` on the unsaved content for a single-file typecheck against the current BuildState
4. Push diagnostics via LSP
5. Full compilation (with `.js` output) happens on `didSave` or `didChangeWatchedFiles`

This replaces the `lib/bs/___incremental/` workaround entirely. Today that workaround exists because the Node.js LSP does not have access to compiler args or build state — it has to extract args from `build.ninja` or shell out to `rescript compiler-args`, write temp files to `lib/bs/___incremental/`, invoke `bsc`, parse stderr, and clean up. With `rescript lsp`, `compiler_args()` is a direct function call in the same process.

### JS output

The LSP produces `.js` files as part of compilation, just like `rescript watch` does today. Vite/webpack pick up the `.js` changes through their own file watchers. This is the same as today — nothing changes for bundler integration.

In the future, a Vite plugin could connect to the LSP for push-based notifications ("module X recompiled, output at Y.js") instead of polling the filesystem, but this is not required for the initial implementation.

## Analysis Integration

### Current: subprocess per request with redundant discovery

```
LSP receives hover request
  -> spawn rescript-editor-analysis hover src/Foo.res 10 5 /tmp/current.res
  -> analysis binary reads rescript.json, discovers project, builds pathsForModule
  -> reads .cmt files
  -> prints JSON to stdout
  -> LSP parses JSON, sends response
  -> process exits (all discovered state discarded)
```

Every request pays the cost of project discovery. The analysis binary is stateless but not in a good way — it re-does work that the build system already did.

### Proposed: analysis binary as pure fire-and-forget

**Phase 1** (pragmatic): Shell out to the analysis binary, but make it truly stateless:
- The LSP knows the project structure from `BuildState` (module map, `.cmt` paths, source dirs)
- Pass the analysis binary only what it needs: source path, position, `.cmt` path, project root
- The analysis binary skips all project discovery (`Packages.getPackage()`, `FindFiles`, config parsing)
- It becomes a pure function: (file + position + cmt path) -> JSON result
- Results are still JSON over stdout — simple, debuggable, low-risk

This means stripping the state-discovery code out of the analysis binary, not porting it. The analysis binary does not need to know about `rescript.json`, dependency resolution, or source folder scanning. The LSP already has all that.

Phase 1 is sufficient for the initial `rescript lsp` implementation. Making the analysis binary truly stateless — no project discovery, no config parsing — is already a significant win over today's architecture. Further optimization (in-process calls, `.cmt` caching) can be explored later if subprocess latency becomes a bottleneck.

### What we do NOT port

The `rescript-vscode` TypeScript codebase contains significant complexity that is not worth porting:
- `build.ninja` parsing for compiler args — call `compiler_args()` directly in Rust
- All bsb/ReScript-version-specific branching — single binary, single version
- `.compiler.log` text parsing — diagnostics come directly from build results
- `incrementalCompilation.ts` orchestration — replaced by direct `bsc` invocation with known args
- Lockfile detection and monorepo heuristics — the LSP knows its own project structure

## What the Editor Extension Becomes

### Today (rescript-vscode, ~3000 lines of TypeScript)

The extension handles:
- Project discovery (find `rescript.json`, resolve paths)
- Build watcher lifecycle (spawn/kill `rescript watch`, detect lockfiles)
- `.compiler.log` parsing (135+ lines of text parsing)
- Diagnostics aggregation (merge compiler + syntax diagnostics)
- Incremental compilation orchestration (`lib/bs/___incremental/`)
- Analysis binary invocation (subprocess management)
- Monorepo/workspace detection (lockfile heuristics)
- Version-specific behavior (ReScript < 12 vs >= 12)
- Compilation status notifications

### Proposed (~200 lines of TypeScript)

The extension becomes a standard LSP client:

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

All the complexity moves into the `rescript` binary where it belongs. The extension is editor glue, nothing more. This is how Gleam, Rust (rust-analyzer), Zig, and most modern language toolchains work.

**Benefit**: Neovim, Helix, Zed, Emacs, and any other LSP-capable editor get first-class ReScript support by simply pointing at `rescript lsp --stdio`. No per-editor plugin needs to reimplement build watcher management or diagnostics parsing.

## What Gets Eliminated

| Component | Lines (approx) | Why it is no longer needed |
|-----------|----------------|---------------------------|
| `watcher.rs` | ~400 | Editor is the file watcher, notifies server via LSP protocol |
| `lock.rs` | ~60 | No separate process to coordinate with |
| `.compiler.log` parsing (LSP) | ~135 | Diagnostics delivered directly via LSP protocol |
| `incrementalCompilation.ts` | ~500 | LSP owns BuildState, no workaround needed |
| Build watcher management (LSP) | ~200 | No child process to manage |
| Monorepo lockfile detection | ~50 | LSP knows its own project structure |
| `lib/bs/___incremental/` | entire mechanism | Single-file checks done against live BuildState |
| Watch-mode bash tests | ~1000 | Replaced by structured telemetry-based tests |
| Version-specific branching (LSP) | ~100 | Single binary, single version |

## Relationship to `rescript build`

`rescript build` remains a standalone one-shot command, similar to `tsc` or `gleam build`:

```
rescript build
    │
    ▼
Read rescript.json, discover packages
    │
    ▼
Scan sources, determine what is stale (file timestamps vs artifacts)
    │
    ▼
Compile everything needed
    │
    ▼
Write .js output
    │
    ▼
Print diagnostics to stderr
    │
    ▼
Exit (code 0 or 1)
```

No lockfile. No shared state with the LSP.

### Coexistence with the LSP

`rescript build` and `rescript lsp` can run with different flags (`--warn-error`, `--filter`) and produce different compilation results. To avoid one stepping on the other's artifacts, they use separate output directories:

- `rescript build` writes to `lib/bs/` — owns it exclusively
- `rescript lsp` writes to `lib/lsp/` — owns it exclusively
- The analysis binary reads `.cmt` files from `lib/lsp/` when called by the LSP

No shared mutable state on disk. No artifact watching. No rebuild-after-interference. Each process is fully independent.

`.js` output still goes to the configured output directory (e.g., `src/Foo.mjs` or `src/Foo.js`). Both produce identical `.js` since the output is deterministic — the last writer wins, and the content is the same.

The `.cmt`/`.cmi` files are small, so the additional disk usage from `lib/lsp/` is negligible.

## Testing

### Approach: extend the PR #8241 infrastructure

PR #8241 introduced a testing pattern for rewatch: isolated sandbox, OTEL span collection, vitest snapshots. The same infrastructure extends naturally to LSP testing by adding one new helper: an LSP client that speaks JSON-RPC over stdio.

### What already exists (from PR #8241)

| Helper | File | Purpose |
|--------|------|---------|
| `createSandbox()` | `tests/rewatch_tests/helpers/sandbox.mjs` | Copies fixture project to temp dir, runs yarn install |
| `createOtelReceiver()` | `tests/rewatch_tests/helpers/otel-receiver.mjs` | HTTP server that collects OTEL spans in memory |
| `createRescriptCli()` | `tests/rewatch_tests/helpers/process.mjs` | Spawns rescript binary with env vars for local bsc/runtime |
| `buildSpanTree()` | `tests/rewatch_tests/helpers/test-context.mjs` | Builds hierarchical tree from flat spans |
| `treeToSummary()` | `tests/rewatch_tests/helpers/test-context.mjs` | Filters and formats span tree for deterministic snapshots |
| `normalizePaths()` | `tests/rewatch_tests/helpers/test-context.mjs` | Strips absolute paths for portable snapshots |

### What to add: `createLspClient()`

A new helper that spawns `rescript lsp --stdio` and speaks JSON-RPC over stdin/stdout:

```javascript
// tests/rewatch_tests/helpers/lsp-client.mjs
export function createLspClient(cwd, otelEndpoint) {
  const proc = spawn(rescript_exe, ["lsp", "--stdio"], {
    cwd,
    env: {
      ...process.env,
      RESCRIPT_BSC_EXE: bsc_exe,
      RESCRIPT_RUNTIME: runtimePath,
      OTEL_EXPORTER_OTLP_ENDPOINT: otelEndpoint,
    },
  });

  // JSON-RPC message framing (Content-Length headers)
  // Collect notifications (diagnostics, status) in arrays
  // Route responses to pending request promises

  return {
    // LSP lifecycle
    initialize(workspaceFolder),
    shutdown(),

    // Document sync
    didOpen(path, content),
    didChange(path, content),
    didSave(path),
    didChangeWatchedFiles(changes),

    // Queries (return parsed JSON response)
    hover(path, line, col),
    completion(path, line, col),
    definition(path, line, col),
    references(path, line, col),
    formatting(path),

    // Collected notifications
    waitForDiagnostics(path, timeoutMs),
    getDiagnostics(path),
    clearDiagnostics(),
  };
}
```

### Test shape: two snapshot streams

Each test produces two snapshots from the same scenario:

1. **OTEL spans** — what the server did internally (built which modules, in what order)
2. **LSP messages** — what the server told the client (diagnostics, responses)

```javascript
// tests/rewatch_tests/tests/lsp.test.mjs

describe("lsp", () => {
  it("pushes diagnostics on initial build", () =>
    runLspTest(async ({ lsp }) => {
      await lsp.initialize();
      const diags = await lsp.waitForDiagnostics("src/App.res");
      expect(diags).toMatchSnapshot("diagnostics");
    }));

  it("incremental rebuild on save", () =>
    runLspTest(async ({ lsp, writeFile }) => {
      await lsp.initialize();
      await lsp.waitForDiagnostics("src/App.res");

      await writeFile("src/App.res", 'let x: string = 123\n');
      await lsp.didSave("src/App.res");

      const diags = await lsp.waitForDiagnostics("src/App.res");
      expect(diags).toMatchSnapshot("diagnostics after type error");
    }));

  it("hover returns type info", () =>
    runLspTest(async ({ lsp }) => {
      await lsp.initialize();
      await lsp.waitForDiagnostics("src/App.res"); // wait for build

      const result = await lsp.hover("src/App.res", 0, 4);
      expect(result).toMatchSnapshot("hover");
    }));

  it("syntax error on didChange", () =>
    runLspTest(async ({ lsp }) => {
      await lsp.initialize();
      await lsp.waitForDiagnostics("src/App.res");

      await lsp.didChange("src/App.res", "let x = \n");
      const diags = await lsp.waitForDiagnostics("src/App.res");
      expect(diags).toMatchSnapshot("syntax diagnostics");
    }));
});
```

### `runLspTest` orchestrator

Mirrors `runRewatchTest` from PR #8241 but boots an LSP client instead of a CLI:

```javascript
export async function runLspTest(scenario) {
  const otelReceiver = await createOtelReceiver();
  const sandbox = await createSandbox();

  const lsp = createLspClient(sandbox, otelReceiver.endpoint);
  const ctx = {
    lsp,
    sandbox,
    writeFile(path, content) { /* write to sandbox */ },
    deleteFile(path) { /* delete from sandbox */ },
  };

  try {
    await scenario(ctx);
  } finally {
    await lsp.shutdown();
  }

  // OTEL span snapshot (same as runRewatchTest)
  const spans = otelReceiver.getSpans();
  const tree = buildSpanTree(spans);
  let summary = treeToSummary(tree);
  summary = normalizePaths(summary, sandbox);
  expect(summary).toMatchSnapshot("otel spans");

  await otelReceiver.stop();
}
```

Every test automatically gets both snapshots: the OTEL span tree (build internals) and whatever LSP messages/responses the test explicitly asserts on. If a build regresses — compiles too many modules, changes compilation order, drops a span — the OTEL snapshot catches it. If diagnostics change format or content, the LSP snapshot catches it.

### What this replaces

| Today | Proposed |
|-------|----------|
| `sleep 2` | `await lsp.waitForDiagnostics()` |
| Parse stdout for "Finished compilation" | OTEL span with `incremental_build` completes |
| `assert_file_exists lib/bs/src/New.js` | Diagnostic snapshot shows successful compilation |
| `exit_watcher` (remove lockfile) | `await lsp.shutdown()` |
| `sed -i` path normalization | `normalizePaths()` from PR #8241 |
| Platform-specific bash scripts | JavaScript, runs on all platforms |

### No Rust tests needed initially

The LSP server is tested end-to-end through the vitest/OTEL infrastructure. Unit tests in Rust can be added later for specific protocol edge cases, but the vitest approach validates the full stack: LSP protocol handling, build engine integration, analysis binary invocation, and diagnostic delivery.

## `rescript watch` — Unchanged

`rescript watch` continues to work exactly as it does today. It is not affected by the introduction of `rescript lsp`.

- Uses `notify` for server-side file watching
- Owns `lib/bs/` for build artifacts
- Writes `.compiler.log` for diagnostics
- Uses the lockfile (`lib/rescript.lock`) for mutual exclusion
- Supports `--after-build`, `--filter`, `--warn-error`

Users who run `rescript watch` in a terminal (without an LSP-capable editor, or alongside one) can continue to do so. The existing `rescript-vscode` extension continues to work with `rescript watch` as before — spawning it as a child process, watching `.compiler.log`, parsing diagnostics.

### Coexistence with `rescript lsp`

When both `rescript watch` and `rescript lsp` are running on the same project:

- `rescript watch` writes to `lib/bs/` (as today)
- `rescript lsp` writes to `lib/lsp/`
- They do not share state or coordinate — no lockfile interaction between them
- `.js` output goes to the same configured directory from both; the output is deterministic so the last writer wins with identical content
- The existing `rescript-vscode` extension would not run `rescript watch` when using `rescript lsp`, so in practice they only coexist if the user explicitly starts `rescript watch` in a terminal

### Future deprecation

`rescript watch` is not deprecated as part of this proposal. It can be revisited once `rescript lsp` has proven itself in production. If and when it is deprecated, it would be in a future major version with advance notice.

## Migration Path

### Stage 1: Add `rescript lsp` as a new subcommand

- Add `Lsp` variant to `cli.rs`
- Implement basic LSP protocol handling in Rust (initialize, shutdown, didOpen, didChange, didSave)
- Register watched file patterns via `client/registerCapability` (`**/rescript.json` initially, source-dir-scoped `*.res`/`*.resi` after build init)
- Wire `didSave` and `didChangeWatchedFiles` to the existing build engine (rewatch's `build::build()`)
- LSP writes artifacts to `lib/lsp/`, not `lib/bs/`
- Push diagnostics directly from build results
- Analysis queries still delegate to the analysis binary (subprocess), pointing it at `lib/lsp/` for `.cmt` files
- No server-side file watcher needed — the editor handles all filesystem monitoring
- `rescript watch` continues to work unchanged, using `lib/bs/` as always

**User impact**: None. `rescript lsp` is opt-in. Editors can be configured to use it. `rescript watch` is unaffected.

### Stage 2: Editor extensions adopt `rescript lsp`

- rescript-vscode detects when `rescript lsp` is available and uses it instead of spawning `rescript watch`
- Falls back to the old architecture (`rescript watch` + `.compiler.log`) for older ReScript versions
- Extension sheds build watcher management, `.compiler.log` parsing, incremental compilation workarounds
- `rescript watch` still works for users who prefer terminal-based workflows or use editors without LSP support

**User impact**: Faster diagnostics, fewer race conditions, simpler extension. No breaking changes.

### Stage 3 (future, separate decision): Deprecate `rescript watch`

This stage is not part of the current proposal. It would only be considered after `rescript lsp` is proven and stable. When the time comes:

- `rescript watch` would print a deprecation notice
- Watch-mode bash tests would be replaced with structured LSP/telemetry tests
- The lockfile would be deprecated
- Removal would happen in a major version

## Open Questions

1. **Non-editor users**: Some developers run `rescript watch` in a terminal alongside their editor. With `rescript watch` eventually removed, they would rely on the editor's LSP for compilation feedback. Is `rescript build --watch` (a simpler terminal-only mode that just prints diagnostics) worth keeping as a convenience?

2. **Vite/bundler integration**: Should the LSP expose a non-LSP API (e.g., a simple socket or named pipe) for build tools to subscribe to compilation events? Or is filesystem watching of `.js` output sufficient?

3. **Cold start performance**: The initial build when `rescript lsp` starts may take several seconds for large projects. Should the LSP respond to queries (with potentially stale data) before the first build completes, or block until ready?

4. **Monorepo: sibling package files**: When the user opens a file from a sibling package outside the current `BuildState` scope, should the LSP treat it as read-only or lazily expand scope? (See Monorepo Behavior section.)

## Prior Art

| Language | CLI build | LSP | Watch mode | Architecture |
|----------|-----------|-----|------------|--------------|
| **Gleam** | `gleam build` | `gleam lsp` (same binary) | No separate watch — LSP rebuilds on save | Single binary, LSP is the watcher |
| **Rust** | `cargo build` | `rust-analyzer` (separate) | No watch mode | LSP does its own analysis, delegates builds to cargo |
| **TypeScript** | `tsc` | `tsserver` (same package) | `tsc --watch` exists but LSP does not use it | LSP and CLI share code but no runtime state |
| **Zig** | `zig build` | `zls` (separate) | No watch mode | LSP uses compiler as library |
| **Go** | `go build` | `gopls` (same module) | No watch mode | LSP wraps the compiler toolchain |

The Gleam model is the closest match: a single binary where `gleam lsp` is a subcommand that owns builds and analysis. ReScript is well-positioned to follow this pattern since the build system (rewatch) is already in Rust and is already part of the `rescript` binary.

## Future Direction: `rescript check` for LLM / Script Integration

Once `rescript lsp` is the primary long-running process, an interesting possibility opens up: letting external tools (LLM coding agents, CI scripts, custom tooling) ask the running LSP to build and return diagnostics — without spawning a full `rescript build`.

The use case: an LLM agent (Claude Code, Cursor, etc.) edits a `.res` file and wants to verify the change compiles. Today it has to run `rescript build`, which does a full project initialization. If an LSP is already running with a warm `BuildState`, an incremental check would be near-instant.

### Possible approach: `rescript check`

A `rescript check` subcommand that connects to a running `rescript lsp` instance, requests a build, and prints structured diagnostics:

```bash
# Quick verification after editing files
rescript check
# Exit code 0 = clean, 1 = errors
# Stdout: JSON diagnostics

# In a skill.md for LLM agents
rescript check --json
```

### The hard part

Finding the running LSP. The process was started by the editor over stdio — there's no socket or pidfile to discover. Options:

- **LSP writes a socket file** on startup (e.g., `lib/lsp/server.sock`). `rescript check` connects to it. Adds a small responsibility to the LSP but is reliable.
- **Fall back to `rescript build`** if no LSP is running. `rescript check` tries the socket, and if it's not there, does a one-shot build. The agent gets correct results either way, just slower without the LSP.

This is not part of the initial proposal but becomes natural once `rescript lsp` owns build state. The LSP already knows how to do incremental builds — `rescript check` just exposes that capability to non-editor clients.

## Future Direction: Vite Plugin via LSP Notifications

Same socket idea, different consumer. A Vite plugin could connect to the running `rescript lsp` and receive push notifications when modules are compiled — no file watching at all.

Today a Vite plugin for ReScript would have to watch the filesystem for `.js` changes, guess when writes are complete, and hope it doesn't pick up a partial file. With an LSP socket, the plugin receives structured events: which module compiled, where the `.js` landed, whether it succeeded, what the errors are. No filesystem polling, no race conditions with partial writes, no guessing. HMR triggers at exactly the right moment.

The plugin would behave differently depending on the Vite mode:

- **`vite dev`**: Connect to the running LSP socket, receive notifications, trigger HMR. The developer already has their editor open with `rescript lsp` running — the Vite plugin just piggybacks on it.
- **`vite build`**: Run `rescript build` as a one-shot command (no LSP needed). This is a production build — deterministic, no long-running process, same as calling `tsc` before `vite build` in a TypeScript project.

This maps cleanly: dev mode is interactive and benefits from the warm LSP, production builds are hermetic and use the one-shot command.

## Implementation Guide

### Code isolation principle

The LSP module (`lsp.rs` + `lsp/`) should be the *only* new code that knows about LSP. Existing build code (`build/`, `cli.rs`, `main.rs`) should gain at most one or two new parameters (like output directory), never LSP-specific types or imports. If a change to existing code is needed, it should make the code more general (e.g., "return diagnostics as data" instead of "print to stderr"), not more LSP-specific.

This keeps changes to the existing codebase minimal and reviewable:
- `watcher.rs`, `lock.rs` — untouched. The LSP doesn't use them.
- `build/` modules — may gain a parameter (e.g., build output path on `BuildState`), but never import from `lsp/`.
- `logs.rs` — diagnostics should be extractable as structured data via a general-purpose change, not an LSP-aware one.
- `compiler_args()` — already pure, no changes needed.

The goal is a large "here is an LSP" PR where almost all new code lives under `lsp/`, and changes to existing files are small, obvious, and easy to review independently.

### Why we register file watchers

The LSP protocol provides two channels for the server to learn about file changes:

1. **`textDocument/didOpen`, `didChange`, `didSave`** — the editor sends these for files that are *open in editor tabs*. If a user has `src/App.res` open and types, the server gets `didChange`. When they save, the server gets `didSave`. But a file that is not open in any tab produces none of these notifications.

2. **`workspace/didChangeWatchedFiles`** — the editor watches the filesystem for patterns the server registered (via `client/registerCapability`) and sends notifications for *any* matching file that changes on disk, whether it is open in the editor or not.

Channel 2 is essential because many file mutations happen outside the editor's open buffers:
- **`git checkout` / `git rebase`** — switches branches, rewrites files on disk
- **LLM coding agents** (Claude Code, Cursor, etc.) — write `.res` files directly via filesystem APIs
- **Terminal commands** — `mv`, `cp`, `touch`, bulk renames
- **Other tools** — formatters, code generators, `sed` scripts

Without channel 2, the server would only see changes to files the user happens to have open. A `git checkout` that modifies 50 files would be invisible to the LSP — the `BuildState` would be stale, diagnostics wrong, and the user would have to manually reopen files to trigger updates.

On `initialized`, the server reads `rescript.json` from each workspace folder, extracts the declared source directories, and registers scoped watchers. For a project with `"sources": {"dir": "src", "subdirs": true}`, the registered patterns are:

- `**/rescript.json` — config changes (always registered)
- `src/**/*.res` — source files (recursive because `"subdirs": true`)
- `src/**/*.resi` — interface files

We deliberately avoid blanket `**/*.res` globs because they match files in `node_modules/` and `lib/bs/`, putting unnecessary load on the editor's file watcher and triggering spurious events for copied build artifacts. This mirrors how `watcher.rs` watches specific `source_folders` from the config rather than the entire workspace.

### Dependencies to add

Rewatch already uses `tokio` with multi-thread runtime. Add `tower-lsp` which builds on tokio:

```toml
# Cargo.toml additions
tower-lsp = "0.20"
```

### Key files to modify

| File | Change |
|------|--------|
| `rewatch/src/cli.rs` | Add `Lsp { stdio: bool }` variant |
| `rewatch/src/main.rs` | Add `Command::Lsp` dispatch |
| `rewatch/src/build/packages.rs:71` | Parameterize `get_build_path()` to support `lib/lsp` |
| `rewatch/src/build/packages.rs:83` | Same for `get_ocaml_build_path()` |

### New files to create

| File | Purpose |
|------|---------|
| `rewatch/src/lsp.rs` | LSP server setup, `tower-lsp` Backend impl |
| `rewatch/src/lsp/dispatch.rs` | Route LSP requests to analysis binary or build engine |

### Existing files to study

| File | What it gives you |
|------|-------------------|
| `rewatch/src/build.rs` | `build()` and `initialize_build()` — the functions the LSP calls on `didSave` |
| `rewatch/src/build/build_types.rs` | `BuildState`, `Module`, `CompileState` — the state the LSP owns |
| `rewatch/src/build/compile.rs` | `compiler_args()` — builds the `bsc` invocation for `didChange` typechecks |
| `rewatch/src/build/logs.rs` | Current diagnostic output — the LSP replaces this with `publishDiagnostics` |
| `rewatch/src/build/packages.rs` | `Package`, `get_build_path()` — need to parameterize for `lib/lsp` |
| `rewatch/src/project_context.rs` | `ProjectContext` — monorepo detection, reuse as-is |
| `rewatch/src/watcher.rs` | Reference for how file events map to dirty flags — same logic, different trigger source |
| `analysis/src/BuildSystem.ml:43` | `getLibBs` — the one OCaml function to change (or bypass via CLI arg) |
| `analysis/bin/main.ml` | Full command dispatch — every analysis command the LSP needs to call |

### Analysis binary commands (exact invocations)

For each LSP feature, the Rust server shells out to `rescript-editor-analysis`:

| LSP request | Analysis command | Arguments |
|-------------|-----------------|-----------|
| `textDocument/hover` | `hover` | `path line col currentFile supportsMarkdownLinks` |
| `textDocument/completion` | `completion` | `path line col currentFile` |
| `completionItem/resolve` | `completionResolve` | `path modulePath` |
| `textDocument/definition` | `definition` | `path line col` |
| `textDocument/typeDefinition` | `typeDefinition` | `path line col` |
| `textDocument/references` | `references` | `path line col` |
| `textDocument/rename` | `rename` | `path line col newName` |
| `textDocument/prepareRename` | `prepareRename` | `path line col` |
| `textDocument/codeAction` | `codeAction` | `path startLine startCol endLine endCol currentFile` |
| `textDocument/formatting` | `format` | `path` |
| `textDocument/documentSymbol` | `documentSymbol` | `path` |
| `textDocument/semanticTokens` | `semanticTokens` | `currentFile` |
| `textDocument/inlayHint` | `inlayHint` | `path lineStart lineEnd maxLength` |
| `textDocument/codeLens` | `codeLens` | `path` |
| `textDocument/signatureHelp` | `signatureHelp` | `path line col currentFile allowForConstructorPayloads` |
| `textDocument/diagnostic` (syntax) | `diagnosticSyntax` | `path` |

All commands output JSON to stdout. Positions are zero-indexed.

For commands that take `currentFile` (hover, completion, signatureHelp, codeAction), the LSP writes the unsaved editor content to a temp file and passes that path. This is how the current TypeScript LSP does it too.

## End-to-End Scenarios

### Scenario 1: Initialize and build

The first thing to get working. No analysis, just "start LSP, build project, push diagnostics."

```
Editor                          rescript lsp
  │                                 │
  │──── initialize ────────────────▶│
  │                                 │  ProjectContext::new(workspace_folder)
  │                                 │  initialize_build(path)
  │                                 │  → ParseState, CompileState per module
  │◀─── initialize result ─────────│
  │                                 │
  │──── initialized ───────────────▶│
  │                                 │  Register watched files: **/rescript.json
  │                                 │  (source-dir watchers added after build init)
  │                                 │
  │◀─── publishDiagnostics ────────│  (for any errors/warnings from initial build)
  │                                 │
```

**What to verify**: Open a project with a type error. The LSP should push a diagnostic for that file without any user interaction.

**Files involved**: `cli.rs`, `main.rs`, `lsp.rs` (new), `build.rs`

### Scenario 2: Save a file, incremental rebuild

The core feedback loop. User saves, LSP rebuilds only what's dirty.

```
Editor                          rescript lsp
  │                                 │
  │──── didSave (src/App.res) ─────▶│
  │                                 │  Mark App module parse_dirty
  │                                 │  Re-parse AST (bsc -bs-ast)
  │                                 │  Update deps if changed
  │                                 │  Compile dirty modules + dependents
  │                                 │
  │◀─── publishDiagnostics ────────│  (clear old errors or push new ones)
  │                                 │
```

**What to verify**: Save a file with an error → diagnostic appears. Fix the error and save → diagnostic clears. Change a type in module A → dependent module B gets a diagnostic.

**Files involved**: `lsp.rs`, `build.rs` (incremental path), `build/compile.rs`, `build/deps.rs`

### Scenario 3: Hover

First analysis feature. Shells out to the analysis binary.

```
Editor                          rescript lsp                    analysis binary
  │                                 │                                │
  │──── hover(App.res:10:5) ───────▶│                                │
  │                                 │  Write unsaved content to tmp  │
  │                                 │──── hover path 10 5 tmp ──────▶│
  │                                 │                                │  Read .cmt from lib/lsp
  │                                 │◀──── JSON result ──────────────│
  │◀─── hover response ────────────│                                │
  │                                 │                                │
```

**What to verify**: Hover over a let binding → see its type. Hover over a function call → see the function signature.

**Files involved**: `lsp.rs` (request handler), `lsp/dispatch.rs` (new, shells out to analysis binary)

### Scenario 4: Completion

Same pattern as hover but with trigger characters.

```
Editor                          rescript lsp                    analysis binary
  │                                 │                                │
  │──── completion(App.res:5:3) ───▶│                                │
  │                                 │  Write unsaved content to tmp  │
  │                                 │──── completion path 5 3 tmp ──▶│
  │                                 │◀──── JSON result ──────────────│
  │◀─── completion response ───────│                                │
  │                                 │                                │
```

**What to verify**: Type `Array.` → get completions for Array module. Type a partial identifier → get matching completions.

**Files involved**: Same as hover.

### Scenario 5: `didChange` with syntax diagnostics

Fast feedback without a full build. On every keystroke (debounced).

```
Editor                          rescript lsp                    analysis binary
  │                                 │                                │
  │──── didChange(App.res) ────────▶│                                │
  │                                 │  Write content to tmp          │
  │                                 │──── diagnosticSyntax tmp ─────▶│
  │                                 │◀──── JSON diagnostics ─────────│
  │◀─── publishDiagnostics ────────│  (syntax errors only)          │
  │                                 │                                │
```

**What to verify**: Type `let x =` (incomplete) → syntax error appears immediately. Complete to `let x = 1` → syntax error clears. This should be fast (<50ms).

**Files involved**: `lsp.rs` (didChange handler with debounce), `lsp/dispatch.rs`

### Scenario 6: `didChange` with incremental typecheck

Richer feedback on unsaved changes. Uses `compiler_args()` to invoke `bsc` on the unsaved content.

```
Editor                          rescript lsp
  │                                 │
  │──── didChange(App.res) ────────▶│
  │                                 │  compiler_args() for App module
  │                                 │  Write unsaved content to tmp
  │                                 │  Invoke bsc with args + tmp file
  │                                 │  Parse bsc stderr for diagnostics
  │◀─── publishDiagnostics ────────│  (type errors from unsaved content)
  │                                 │
```

**What to verify**: Change a function's return type without saving → type error appears in callers. This is the `___incremental` replacement.

**Files involved**: `lsp.rs`, `build/compile.rs` (`compiler_args()`), diagnostic parsing

### Scenario 7: File created/deleted externally

External changes (git checkout, terminal `touch`, etc.) come through `didChangeWatchedFiles`.

```
Editor                          rescript lsp
  │                                 │
  │──── didChangeWatchedFiles ─────▶│  (NewModule.res created)
  │                                 │
  │                                 │  Full rebuild (new module in graph)
  │                                 │  Re-scan source dirs
  │                                 │  Update BuildState
  │                                 │
  │◀─── publishDiagnostics ────────│
  │                                 │
```

**What to verify**: `git checkout` a branch with different files → diagnostics update. Create a new `.res` file in the source directory → it becomes available for imports.

**Files involved**: `lsp.rs`, `build.rs` (full rebuild path)

### Suggested implementation order

1. **Scenario 1** — Initialize + build. This proves the wiring works.
2. **Scenario 2** — didSave + incremental rebuild. This is the core value.
3. **Scenario 5** — didChange + syntax diagnostics. Fast feedback loop.
4. **Scenario 3** — Hover. First analysis feature, proves the subprocess call works.
5. **Scenario 4** — Completion. Same pattern as hover.
6. **Scenario 7** — didChangeWatchedFiles. External file changes.
7. **Scenario 6** — didChange + incremental typecheck. The `___incremental` replacement.

Scenarios 1-2 give you a working build server. Add scenario 5 and you have real-time syntax feedback. Add scenario 3-4 and you have a usable LSP. The rest is polish.
