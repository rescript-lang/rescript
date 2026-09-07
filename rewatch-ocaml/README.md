# Experimental OCaml rewatch

This directory contains the separately named OCaml port of the ReScript build
system. It does not replace the Rust `rescript` executable.

## Build

From the repository root, with OCaml, dune, and yojson installed:

```sh
opam exec -- dune build rewatch-ocaml/rescript_ocaml.exe
```

The executable is written to:

```text
_build/default/rewatch-ocaml/rescript_ocaml.exe
```

It invokes `bsc` as an external process. When running outside this repository's
normal Makefile environment, point it at the compiler and runtime explicitly:

```sh
export RESCRIPT_BSC_EXE="$PWD/_build/default/compiler/bsc/rescript_compiler_main.exe"
export RESCRIPT_RUNTIME="$PWD/packages/@rescript/runtime"
_build/default/rewatch-ocaml/rescript_ocaml.exe build path/to/project
```

Supported commands are `build` (the default), `watch`, `clean`, `format`, and
`compiler-args`. Run the executable with `--help` for the current option summary.

## Test

```sh
opam exec -- dune runtest rewatch-ocaml
sh rewatch-ocaml/tests/run.sh \
  "$PWD/_build/default/rewatch-ocaml/rescript_ocaml.exe"
```

The canonical integration suite can use the port through its existing override:

```sh
export REWATCH_EXECUTABLE="$PWD/_build/default/rewatch-ocaml/rescript_ocaml.exe"
eval "$(cd rewatch/tests && node ./get_bin_paths.js)"
export RESCRIPT_BSC_EXE RESCRIPT_RUNTIME
bash rewatch/tests/compile/01-basic-compile.sh
```

See `PROGRESS.md` for verified coverage, measurements, review results, and
remaining compatibility or platform gaps.

## Platform status

Windows support is required for completion, even though runtime verification is
not available in the current Linux development environment. Subprocesses use
the cross-platform `spawn` library, which uses `CreateProcess` on Windows; the
polling watcher and lock lifecycle still require a Windows cross-build and
runtime verification. `PROGRESS.md` tracks the remaining portability blockers.
Shared path construction uses OCaml's `Filename` APIs so Windows separators and
drive roots are not hard-coded assumptions.
