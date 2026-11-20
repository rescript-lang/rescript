# ReScript language server

This directory contains the OCaml language server for ReScript. It calls the
in-tree [analysis library](../analysis/README.md) directly for editor features
and runs inside the OCaml `rescript` CLI. There is no separate
`rescript-language-server` executable.

The implementation is experimental. This guide covers the implementation,
design rationale, editor setup, feature status, and diagnostic ownership.
Planned work and historical behavior are distinguished from current behavior.

## Why OCaml

Editor features already live in the OCaml analysis library and depend on
compiler types: typed trees, locations, package metadata, and diagnostics.
Calling those APIs directly avoids subprocess orchestration and JSON
serialization through `rescript-editor-analysis`. Changes to the compiler,
analysis, and server can be developed together in the same language and tested
with Dune and expect tests.

### Why ship with the compiler

The `.cmt` and `.cmti` artifacts contain marshalled `Typedtree`, `Types`, and
`Env` values. These are not a stable cross-version protocol. Changing their
OCaml representations can make older artifacts unsafe to read. For example,
[issue #8475](https://github.com/rescript-lang/rescript/issues/8475) describes
crashes from incompatible typedtree data; record-rest changes exposed the same
class of problem.

The project's ReScript dependency supplies the compiler, analysis, and server
from one release. Editors use one project-local entrypoint instead of selecting
a separately versioned server, and compiler/LSP changes can ship atomically.

Bundling does not eliminate stale artifacts. Changes to marshalled types still
require appropriate CMT magic-number changes, and projects must be rebuilt with
the matching compiler. A server supporting multiple compiler versions would
need version-specific decoders or a stable, explicitly versioned analysis
format. Graceful reporting of incompatible artifacts remains an important
error-handling boundary.

## Running the server

Configure the editor to launch the project's ReScript installation:

```sh
node_modules/.bin/rescript lsp --stdio
```

`--stdio` is required. `rescript lsp` without it fails with usage help; the
subcommand does not accept `--quiet` or `--verbose`. Only stdio transport is
implemented, not Node IPC or sockets. Standard output carries protocol messages.

Run a build or build watcher separately to produce and refresh typed metadata:

```sh
node_modules/.bin/rescript build
# Or keep a watcher running:
node_modules/.bin/rescript watch
```

The server does not start or manage builds. Unsaved buffers receive syntax
diagnostics, but type-based features depend on compiled artifacts from a build.
Incremental LSP document synchronization means applying text edits to the
in-memory buffer; it does not mean incremental typechecking of that buffer.

Use the workspace's build root when configuring the editor, rather than an
arbitrary nested source package. The client must handle dynamic registration of
`workspace/didChangeWatchedFiles` to report compiler-log changes.

## Architecture

```text
Editor client
  <-> JSON-RPC over stdio
rescript lsp --stdio (rewatch-ocaml/rescript_ocaml.ml)
  -> Eio_main.run supplies filesystem and stdio capabilities
  -> Server handles transport and protocol lifecycle
  -> Rescript_language_server dispatches requests and notifications
  -> Analysis reads compiler metadata and computes editor results

External build -> lib/rescript artifacts and compiler logs
Editor file watcher -> workspace/didChangeWatchedFiles -> context/diagnostics refresh
```

The server links the analysis code rather than spawning
`rescript-editor-analysis` for requests. Analysis consumes `.cmt` and `.cmti`
files containing marshalled compiler-internal data, not a stable cross-version
schema. Shipping the server with the compiler keeps their representations
aligned, but does not make stale artifacts compatible: rebuild a project after
updating its compiler.

The server library lives in `lsp/src` and has an opam package named
`rescript-language-server`. It depends on the OCaml `lsp` library, Eio,
`ppx_deriving_yojson`, and the in-tree analysis library; tests use `ppx_expect`.

### Code map

| Module | Responsibility |
| --- | --- |
| [CLI entrypoint](../rewatch-ocaml/rescript_ocaml.ml) | Starts the server inside the `rescript` executable using Eio. |
| [server.ml](src/server.ml) | JSON-RPC framing, serialized input dispatch, initialization/shutdown lifecycle, and tracking responses to server requests. |
| [rescript_language_server.ml](src/rescript_language_server.ml) | Advertised capabilities, request handlers, document events, package discovery, and configuration/diagnostic refresh. |
| [state.ml](src/state.ml) | Per-session documents, settings, filesystem capability, initialization parameters, encoding, analysis state, and package configuration. |
| [document.ml](src/document.ml), [document_store.ml](src/document_store.ml) | Versioned in-memory `.res`/`.resi` documents and incremental text edits using `Lsp.Text_document`. |
| [helpers.ml](src/helpers.ml), [compiler_config.ml](src/compiler_config.ml) | Package selection, typed-artifact loading, and compiled JavaScript/interface path resolution. |
| [compiler_log.ml](src/compiler_log.ml), [source_dirs.ml](src/source_dirs.ml) | Compiler-log parsing, build-completion detection, and `.sourcedirs.json` build-root extraction. |
| [diagnostics.ml](src/diagnostics.ml) | Separate diagnostic sources, snapshot updates, URI/range conversion, and publication. |
| [code_actions.ml](src/code_actions.ml), [custom_requests.ml](src/custom_requests.ml), [execute_commands.ml](src/execute_commands.ml) | Analysis actions, interface creation, compiled-file navigation, and debugging commands. |
| [configuration.ml](src/configuration.ml) | Settings decoding and defaults. |
| [formatter.ml](src/formatter.ml) | Runs the project's `rescript format` command on the buffer and computes text edits; its process helper uses Unix on Windows and Eio on Linux/macOS. |
| [fs.ml](src/fs.ml) | Filesystem helpers over the Eio filesystem capability. |

### Project discovery and recovery

Initialization discovers the workspace package and dependencies and builds the
analysis lookup tables and compiler configuration map. Generated metadata under
`lib/rescript` is needed for most type-based requests.

The server dynamically registers compiler-log watchers. When a `.compiler.log`
is created or changed, it rediscovers the project into fresh analysis tables.
Successful discovery replaces the context while preserving open buffers,
settings, encoding, and diagnostics. If discovery still fails, the old context
remains usable. This lets a session opened before the first build recover
without restarting the server.

For diagnostic refresh, the server reads the compiler logs identified by
`lib/rescript/.sourcedirs.json`, not just the log mentioned in the watcher
event. This keeps diagnostics across discovered build roots in sync.

## Diagnostics

Diagnostics have three owners with different lifetimes:

| Source | Lifetime |
| --- | --- |
| `compiler` | Saved build errors and warnings, including diagnostics for closed files. |
| `compiler_syntax` | Compiler syntax errors for closed documents only. |
| `syntax` | Syntax errors computed from open in-memory buffers. |

On open/change, the server parses the current buffer for syntax errors. On
close, it clears that buffer's syntax diagnostics but retains build diagnostics;
a saved syntax error can then come from the compiler log instead.

Only completed compiler logs form a new snapshot. A log still being written
must not clear the last completed results. Even a completed, syntax-only log
does not replace previous non-syntax diagnostics: parsing may have stopped the
compiler before it could check the rest of the workspace. A subsequent snapshot
with non-syntax diagnostics replaces them; a completed empty snapshot clears
them. Type diagnostics may therefore remain stale while syntax errors prevent
a complete check.

For clients supporting pull diagnostics, open documents use
`textDocument/diagnostic`, while closed documents receive pushed compiler
diagnostics. Clients without pull support receive pushed diagnostics for both.
Avoid publishing the same diagnostic through both channels: clients can retain
them separately and display duplicates. Supported clients are asked to refresh
pull diagnostics after compiler-log updates.

### Diagnostic scenarios and design history

Three editor scenarios motivated the ownership model:

1. Only A is open, while B and C have compiler type errors caused by an edit to
   A. Saving a syntax error in A previously replaced the compiler snapshot with
   that syntax error and incorrectly cleared B/C. The server now preserves
   their non-syntax diagnostics until a later usable snapshot replaces them.
2. Closing A while B and C still have type errors must not clear those errors,
   even if every document is closed. They belong to the workspace build, not
   to the lifetime of an editor buffer.
3. Closing a buffer with an unsaved syntax error should clear that in-memory
   diagnostic. If the syntax error was saved and reported by the compiler, it
   should instead remain visible through compiler syntax diagnostics.

The original proposal separated editor syntax and compiler diagnostics into
two maps. The current implementation further separates compiler syntax so
open documents can use their buffers without losing saved syntax errors for
closed files. Completed-log checks prevent save-time flicker, and separate
push/pull ownership prevents duplicate display.

```text
didChange:
  update syntax diagnostics from the current buffer
  retain compiler diagnostics

compiler log changed:
  incomplete log -> keep the last completed snapshot
  completed syntax-only log -> refresh closed-file syntax; retain type errors
  completed non-syntax/empty log -> replace/clear compiler diagnostics

didClose:
  clear buffer syntax diagnostics
  retain build diagnostics and allow compiler syntax for the closed document
```

## Features and settings

Implemented requests include hover, completion and completion resolution,
signature help, definitions and type definitions, references, prepare rename
and rename, document symbols, code actions, code lenses, inlay hints, full
semantic tokens, document formatting, and document diagnostics.

Custom requests provide compiled JavaScript navigation, interface creation, and
implementation/interface switching. Execute commands also expose navigation
and state/CMT dumps. Dumps are written to temporary files and opened with
`window/showDocument`, not returned as JSON content for the client to display.
File-opening commands require client support for that request.

Settings are fetched through `workspace/configuration` using the `rescript`
section after `workspace/didChangeConfiguration`. `initializationOptions` is
not a settings source. The following JSON shows the defaults for that section:

```json
{
  "hover": { "supportMarkdownLinks": false },
  "codeLens": false,
  "inlayHints": { "enable": false, "maxLength": 25 }
}
```

Set `inlayHints.maxLength` to `null` for unlimited length. Signature help is
enabled independently of these settings. Client configuration layouts may
require middleware to return the `rescript` section in this shape. Markdown
links in hover are optional because editor support varies.

### Protocol coverage

The following groups summarize the implemented protocol surface. They do not
imply complete feature parity or exhaustive test coverage.

| Area | Implemented requests and notifications |
| --- | --- |
| Lifecycle | `initialize`, `initialized`, `shutdown`, `exit` |
| Documents | `textDocument/didOpen`, `textDocument/didChange` (incremental edits), `textDocument/didClose` |
| Inspection | `textDocument/hover`, `textDocument/signatureHelp`, `textDocument/documentSymbol` |
| Completion | `textDocument/completion`, `completionItem/resolve` |
| Navigation | `textDocument/definition`, `textDocument/typeDefinition`, `textDocument/references` |
| Editing | `textDocument/prepareRename`, `textDocument/rename`, `textDocument/codeAction`, `textDocument/formatting` |
| Annotations | `textDocument/codeLens`, `workspace/codeLens/refresh`, `textDocument/inlayHint`, `textDocument/semanticTokens/full` |
| Diagnostics | `textDocument/publishDiagnostics`, `textDocument/diagnostic`, `workspace/diagnostic/refresh` when supported |
| Workspace | `workspace/didChangeWatchedFiles`, `client/registerCapability`, `workspace/didChangeConfiguration`, `workspace/configuration`, `workspace/executeCommand` |

Code actions combine analysis refactorings, diagnostic quick fixes, and
compiled-JavaScript/interface navigation. Circular-dependency errors have no
precise source location, so the server uses a document-wide range when it has
the open buffer and a fallback range otherwise. Warning messages include their
warning number. Monorepo compiler-log discovery uses `cmt_scan` build roots in
`.sourcedirs.json`, metadata introduced in ReScript 12.1.

### Custom requests and commands

| Name | Behavior |
| --- | --- |
| `textDocument/openCompiled` | Resolves the compiled JavaScript file for a source document. |
| `textDocument/createInterface` | Creates an interface from compiled metadata; fails if it already exists. |
| `textDocument/switchImplementationInterface` | Resolves the corresponding implementation/interface. |
| `rescript/openCompiled` | Execute-command navigation to compiled JavaScript. |
| `rescript/createInterface` | Execute-command interface creation and opening. |
| `rescript/switchImplementationInterface` | Execute-command navigation between implementation and interface. |
| `rescript/dumpServerState` | Writes a temporary `.json` dump of session state and opens it. |
| `rescript/dumpCmt` | Writes a temporary `.txt` dump of typed metadata and opens it. |

State dumps include document versions/lengths, diagnostics, configuration,
initialization parameters, analysis state, and compiler configuration. The
file-opening command flow is:

```text
CodeAction.command -> workspace/executeCommand -> window/showDocument
```

Clients without `window/showDocument` support cannot use this opening flow.
The server also sends `rescript/compilationFinished` on watched-file updates
for compatibility with the existing VS Code analysis integration. This is a
watcher-triggered notification, not server-managed build progress.

### Current limitations

- No server-managed builds or unsaved-buffer incremental typechecking.
- One workspace context per session; workspace-folder changes are not handled.
- No semantic-token deltas, range formatting, or workspace-symbol search.
- Document synchronization negotiates UTF-8 or UTF-16, with UTF-16 as the LSP
  default. Analysis locations and some range helpers still use byte offsets;
  conversion is not complete for non-ASCII text under UTF-16. Negotiating an
  encoding does not by itself make all analysis positions use it.
- Configuration-file changes are not independently watched. Restart the server
  if a configuration change is not picked up through a later build-log event.

Other unimplemented requests include `textDocument/declaration`,
`textDocument/implementation`, `textDocument/documentHighlight`,
`codeAction/resolve`, `codeLens/resolve`, `inlayHint/resolve`,
`textDocument/semanticTokens/range`, `textDocument/onTypeFormatting`,
`textDocument/foldingRange`, `textDocument/selectionRange`,
`textDocument/documentLink`, `documentLink/resolve`,
`textDocument/documentColor`, and `textDocument/linkedEditingRange`.
`textDocument/didSave` does not trigger work; compiler-log notifications drive
build refresh. Node IPC and sockets are not supported.

Unimplemented custom features include `rescript/startBuild`,
`rescript/compilationStatus`, `rescript/dumpParseTree`, and
`rescript/dumpTypedTree`. Build integration would need decisions about watcher
ownership, shutdown, cancellation, and progress reporting. Direct compiler
integration and unsaved-buffer typechecking remain future work; the existing
analysis artifact lookup is not a complete implementation of either feature.

## Development and tests

From the repository root, build the toolchain/runtime and run the E2E suite:

```sh
opam exec -- make test-lsp
```

After those artifacts are built, run only the E2E tests or the server's inline
tests:

```sh
opam exec -- dune runtest tests/lsp_tests
opam exec -- dune runtest lsp
```

The E2E tests are OCaml `ppx_expect` snapshots. Their shared client launches the
actual CLI over stdio using Unix process APIs for Windows/Linux/macOS support.
Tests create temporary projects, build them, edit buffers, send requests and
notifications, and normalize workspace paths in snapshots. Coverage includes
editor features, diagnostic ownership, build recovery, formatting, and command
capability handling. The server also has inline tests for its local helpers.

`Test.setup_project` writes `rescript.json` and documents and can run a build;
`Test.with_project` provides temporary workspace setup and cleanup. Tests use
explicit zero-based cursor positions, not analysis-suite cursor markers.
The harness supplies `RESCRIPT_RUNTIME` because it bypasses the npm launcher.
Cases cover JSX, multi-document requests, unsaved buffers, configuration,
client capabilities, stale warnings, duplicate publication, and save-time
flicker. Representative cases come from `tests/analysis_tests`, which remains
an independent suite with additional coverage.

See [tests/lsp_tests/README.md](../tests/lsp_tests/README.md) for fixture helpers,
snapshot review/promotion commands, and known limitations captured by tests.

