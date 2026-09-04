# Lambda optimization and JavaScript generation

This directory contains the compiler backend after typedtree translation. It
owns the Lambda optimization passes, the JavaScript IR, and JavaScript output.
Lambda itself is defined in [`../ml/lambda.mli`](../ml/lambda.mli).

## Pipeline and code map

Typedtree translation in `compiler/ml/translcore.ml` and
`compiler/ml/translmod.ml` produces the `Lambda` representation defined in
`compiler/ml/lambda.mli`.

`lam_pass_*.ml` and the other `lam_*.ml` modules
: Analyze and transform Lambda. [`lam_compile_main.ml`](lam_compile_main.ml)
  coordinates the backend pass sequence; read it before inserting or
  reordering a pass.

  The sequence is hand-unrolled rather than iterated to a fixed point. Only
  `simplify_alias` reads the statistics, and a fresh `collect_info` runs
  immediately before each of its three rounds. `simplify_lets` and `sroa`
  compute what they need themselves. Each `-debug-ir` dump is named after the
  pass whose output it holds.

  | # | pass | statistics |
  |---|---|---|
  | 1 | `collapse_var_aliases` | |
  | 2 | `deep_flatten` | |
  | 3 | `simplify_exits` | |
  | 4 | `simplify_alias` | reads a snapshot taken just before |
  | 5 | `deep_flatten` | |
  | 6 | `simplify_alias` | reads a snapshot taken just before |
  | 7 | `deep_flatten` | |
  | 8 | `simplify_exits` | |
  | 9 | `simplify_alias` | reads a snapshot taken just before |
  | 10 | `simplify_lets` | own occurrence count |
  | 11 | `sroa` | own field-use classification |
  | 12 | `simplify_exits` | |
  | 13 | `guard_raises` | |

  A snapshot is fresh when its pass starts, but `simplify_alias` also mutates
  the table as it rewrites, so entries can describe an earlier version of the
  term by the time the pass finishes.

[`lam_compile.ml`](lam_compile.ml)
: Lowers Lambda to JavaScript IR. Primitive-specific and FFI lowering is split
  into `lam_compile_primitive.ml`, `lam_compile_external_call.ml`, and related
  modules.

[`j.ml`](j.ml)
: Defines JavaScript expressions, statements, and blocks.

`js_pass_*.ml` and other `js_*.ml` modules
: Analyze and transform JavaScript IR. `js_dump*.ml` renders the final program,
  while [`js_implementation.ml`](js_implementation.ml) coordinates compilation
  from a source file.

## Changing a representation

`Lambda.t` is private: every term is built through the constructors in
[`../ml/lambda.mli`](../ml/lambda.mli), seven of which normalize as they
build: `apply`, `prim`, `switch`, `stringswitch`, `if_`, `seq` and `not_`.
A constructor may replace a node with an equivalent one, but may not move code
between branches - that is what a pass is for. When adding or changing a
constructor, search every producer, traversal, optimizer, printer, serializer,
and consumer.

Check persistence boundaries as part of the change. `Lambda.t` can be stored in
`.cmj` data through `js_cmj_format`; a constructor or payload change therefore
changes cached compiler data even when generated JavaScript is unchanged.

Keep representation contracts in the owning `.mli`, pass-order or analysis
invariants beside the pass implementation, and this guide limited to
navigation. Remove completed design notes rather than retaining them as an
alternative description of the current backend.

## Testing and inspection

Run the full compiler tests for backend changes:

```sh
make test
```

End-to-end fixtures under `tests/tests/` check generated `.mjs` output. Add
focused OUnit coverage for an isolated analysis or transformation. Use the
compiler flags below to compare intermediate forms for a small source file:

```sh
./cli/bsc.js -dtypedtree example.res
./cli/bsc.js -drawlambda example.res
```

For backend debugging, use [`lam_print.ml`](lam_print.ml) at the relevant
pass boundary and remove temporary output before committing.
