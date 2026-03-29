# Analysis Binary

OCaml binary (`rescript-editor-analysis.exe`) that provides type-aware language intelligence: hover, completion, references, definition, rename, code actions, etc.

## How it's invoked

The Rust LSP server (`rewatch/src/lsp/`) shells out to this binary for each request. The protocol:

```
rescript-editor-analysis.exe rewatch <subcommand>
```

The LSP sends a JSON context blob via **stdin** containing source text, cursor position, module paths, and dependency information. The binary writes its result to **stdout**.

See `rewatch/src/lsp/analysis.rs` for the Rust side (context construction and spawning).

## Key files

- `bin/main.ml` — Entry point. The `rewatch` subcommand dispatch is at line ~135.
- `src/CommandsRewatch.ml` — Handlers for each subcommand (reads JSON from stdin, calls into the analysis engine).
- `src/Commands.ml` — Older CLI-style handlers (used by the legacy LSP architecture).

## Subcommands

The `rewatch` subcommands (dispatched in `bin/main.ml`) map 1:1 to LSP requests:

`completion`, `completionResolve`, `hover`, `definition`, `typeDefinition`, `references`, `documentSymbol`, `prepareRename`, `rename`, `signatureHelp`, `codeLens`, `inlayHint`, `semanticTokens`, `codeAction`, `llmIndex`, `workspaceSymbol`

## Building

The analysis binary is built as part of the compiler's dune build:

```bash
make          # builds everything including the analysis binary
```

The binary ends up next to `bsc.exe` in `packages/@rescript/<platform>/bin/`.
