# Editor analysis

The analysis executable powers editor features such as completion, hover,
references, semantic tokens, and code actions. It reads compiler-produced
`.cmt` and `.cmti` files, so an analysis binary and the project artifacts it
inspects must be built with compatible compiler representations.

## Code map

- [`bin/main.ml`](bin/main.ml) starts the `rescript-editor-analysis` command.
- [`src/commands.ml`](src/commands.ml) dispatches commands, including the
  source-annotated test command.
- `src/completion_*.ml` implements the completion frontend, context-specific
  completion logic, and result conversion.
- [`src/hover.ml`](src/hover.ml), [`src/references.ml`](src/references.ml),
  [`src/semantic_tokens.ml`](src/semantic_tokens.ml), and
  [`src/code_actions.ml`](src/code_actions.ml) own the corresponding features.
- [`src/cmt.ml`](src/cmt.ml), [`src/process_cmt.ml`](src/process_cmt.ml), and
  [`src/process_extra.ml`](src/process_extra.ml) are the main typed-artifact
  boundary. Shared typed-tree definitions and traversal utilities live under
  `compiler/ml`.
- [`reactive/README.md`](reactive/README.md) documents the reactive analysis
  library. [`reanalyze/README.md`](reanalyze/README.md) covers Reanalyze, which
  is a separate analysis pipeline in this directory.

Run the binary from the repository root:

```sh
dune exec -- rescript-editor-analysis --help
```

## Tests

Build the compiler and runtime, then run the repository target:

```sh
make lib
make test-analysis
```

The target runs the suites under `tests/analysis_tests/`, including the main
snapshot suite and focused projects for generic JSX, incremental type checking,
namespaced references, and source-directory dependencies.

The main suite uses directives embedded in ReScript comments. For example:

```rescript
let value = 5
// value.
//       ^com
```

`^com` asks the test command to compute completion at that position. See the
directive match in `analysis/src/commands.ml` for the current set. Tests compile
a temporary source file and compare command output with checked-in snapshots.
After an intentional change, inspect every updated snapshot rather than
accepting the directory wholesale.

To inspect one test while developing:

```sh
dune exec -- rescript-editor-analysis test tests/analysis_tests/tests/src/CompletePrioritize1.res
```

Use paths from the repository root, and ensure the test project has first been
built with the local compiler.

## Changing typed compiler representations

Editor analysis consumes typedtree nodes and compiler type representations
directly. When adding or changing a parsetree or typedtree node:

1. search this directory for matches on the surrounding constructors, rather
   than relying only on exhaustiveness warnings;
2. check completion, hover/type printing, references, document symbols,
   semantic tokens, code actions, and interface generation as applicable;
3. update both positive results and recovery behavior for incomplete source;
4. run `make test-analysis` in addition to compiler tests.

Keep compiler representation contracts in the owning compiler `.mli` files.
Put analysis-specific assumptions beside the analysis code that relies on them,
and use this guide only for navigation and cross-cutting workflow.
