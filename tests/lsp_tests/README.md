# LSP Snapshot Tests

These OCaml `ppx_expect` tests run the language server over stdio. They adapt
representative cases from `tests/analysis_tests`; the original analysis suite
remains in place and has additional coverage.

Each feature has its own module, with one named `let%expect_test` per scenario.
Source snippets contain no cursor markers. Positions are explicit, zero-based
LSP positions, recalculated after removing marker lines and uncommenting code.

| Analysis marker | Test module | LSP request |
| --- | --- | --- |
| `def` | `definition.ml` | `textDocument/definition` |
| `com` | `completion.ml`, `completion_jsx.ml` | `textDocument/completion` |
| `cre` | `completion_resolve.ml` | `completionItem/resolve` |
| `doc` | `document_symbol.ml` | `textDocument/documentSymbol` |
| `sem`, `hig` | `semantic_tokens.ml` | `textDocument/semanticTokens/full` |
| `hov` | `hover.ml`, `hover_jsx.ml` | `textDocument/hover` |
| `she` | `signature_help.ml` | `textDocument/signatureHelp` |
| `int` | `create_interface.ml` | `textDocument/createInterface` (custom) |
| `ref` | `references.ml` | `textDocument/references` |
| `pre` | `prepare_rename.ml` | `textDocument/prepareRename` |
| `ren` | `rename.ml` | `textDocument/rename` |
| `typ` | `type_definition.ml` | `textDocument/typeDefinition` |
| `xfm` | `code_action.ml` | `textDocument/codeAction` |
| `hin` | `inlay_hint.ml` | `textDocument/inlayHint` |
| `dia` | `document_syntax.ml` | `textDocument/diagnostic` |
| `cle` | `code_lens.ml` | `textDocument/codeLens` |

`Test.with_project` creates an isolated temporary project, optionally builds it,
opens `Test.res`, and cleans up after the server exits. Additional `.res` and
`.resi` files can be supplied through `~files`. Use `Test.change_document` to
exercise unsaved buffers without changing the compiled source, and
`Test.configure` to answer the server's configuration request.

Use `~jsx:true` to build with a temporary copy of the local React bindings.
Use `~capabilities` to exercise push-only clients as well as the default client
with pull diagnostics. `diagnostics.ml` tests both actual recompilation and
explicit compiler-log phases delivered through `workspace/didChangeWatchedFiles`.
The client captures every diagnostic publication and uses a request round trip
as a barrier, so these tests do not rely on sleeps or an external file watcher.

`Test.snapshot` serializes typed LSP responses, replacing the temporary root
with `/workspace` while preserving relative paths, ranges and response order.
The interface tests also snapshot the generated file contents.

Some snapshots characterize current limitations rather than desired behavior:
file rename destinations are double encoded; module rename omits JSX tag uses;
component references repeat the declaration range; and a missing constructor
argument produces no signature help. These cases have TODO comments in their
tests. Path normalization preserves the malformed URI shape instead of hiding it.

Build the local toolchain and runtime with `opam exec -- make lib`, then run:

```sh
opam exec -- dune runtest tests/lsp_tests
```

Review snapshot differences before accepting them for a specific module:

```sh
opam exec -- dune promotion diff tests/lsp_tests/definition.ml
opam exec -- dune promote tests/lsp_tests/definition.ml
opam exec -- dune runtest tests/lsp_tests
```
