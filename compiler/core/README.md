# Lambda, Lam, and JavaScript generation

This directory contains the compiler backend after typedtree translation. It
owns ReScript's Lam representation, Lam optimization passes, JavaScript IR,
and JavaScript output.

## Pipeline and code map

Typedtree translation in `compiler/ml/translcore.ml` and
`compiler/ml/translmod.ml` produces the `Lambda` representation defined in
`compiler/ml/lambda.mli`.

[`lam_convert.ml`](lam_convert.ml)
: Converts `Lambda.lambda` to the ReScript-specific [`Lam.t`](lam.mli),
  normalizes aliases, and collects potential module dependencies.

`lam_pass_*.ml` and the other `lam_*.ml` modules
: Analyze and transform Lam. [`lam_compile_main.ml`](lam_compile_main.ml)
  coordinates the backend pass sequence; read it before inserting or
  reordering a pass.

[`lam_compile.ml`](lam_compile.ml)
: Lowers Lam to JavaScript IR. Primitive-specific and FFI lowering is split
  into `lam_compile_primitive.ml`, `lam_compile_external_call.ml`, and related
  modules.

[`j.ml`](j.ml)
: Defines JavaScript expressions, statements, and blocks.

`js_pass_*.ml` and other `js_*.ml` modules
: Analyze and transform JavaScript IR. `js_dump*.ml` renders the final program,
  while [`js_implementation.ml`](js_implementation.ml) coordinates compilation
  from a source file.

## Changing a representation

`Lambda` and `Lam` have similarly named constructors but are distinct IRs.
When adding or changing one, search every producer, traversal, optimizer,
printer, serializer, and consumer of that specific type. Do not assume a match
on the other representation covers it.

Check persistence boundaries as part of the change. `Lam.t` can be stored in
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

For Lam-specific debugging, use [`lam_print.ml`](lam_print.ml) at the relevant
pass boundary and remove temporary output before committing.
