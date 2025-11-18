# Reactive Graph Reachability

This directory hosts two minimal OCaml implementations for computing the set of
nodes that are unreachable from all marked nodes in a graph assembled from
multiple per-file fragments:

1. `traditional_analysis/` contains a plain OCaml executable plus unit tests that parse
   `.graph` files, build the global graph, and print unreachable nodes.
2. `reactive_analysis/` reuses the same parser/graph logic but runs the pipeline through
   the Skip reactive runtime (vendored under `reactive_graph/vendor/skip-ocaml`)
   so that file
   parsing and reachability recompute incrementally.

The implementations share `graph_lib/`, which knows how to parse graph files and
compute reachability.

## Input format

Each `.graph` file can contain `node` and `edge` directives:

```
node <id> [marked]
edge <src> <dst>
```

* Node identifiers are integers and must be unique globally.
* Adding the literal `marked` after a node ID makes it one of the starting
  points for reachability.
* Edge sources must be defined in the same file; edge destinations must exist in
  some file.

Sample files live under `data/`.

## Traditional build & tests

```
cd /Users/cristianocalcagno/GitHub/rescript
dune runtest --root reactive_graph        # runs unit tests
dune exec --root reactive_graph reactive_graph/traditional_analysis/bin/traditional_main.exe -- reactive_graph/data
```

The executable prints either `All nodes are reachable...` or the sorted list of
unreachable nodes.

## Reactive build & usage

`reactive_graph/vendor/skip-ocaml/` vendors the Skip runtime sources copied from
[`skip-ocaml`](https://github.com/skiplang/skip-ocaml) at commit
`0a8ddbf29970869d9396d7e860e3b48bb3df8695`. The snapshot includes only the
pieces required to build `libskip_reactive.a` (runtime C/C++ glue plus the LLVM
module `external/skip.ll`). If you want libbacktrace support, build
`external/libbacktrace.a` for your platform and drop it into the vendor tree
before compiling. Otherwise, build the static library once:

```
cd /Users/cristianocalcagno/GitHub/rescript/reactive_graph/vendor/skip-ocaml
make build/libskip_reactive.a
```

(Re-run the command whenever you clean the vendor build directory.)

1. Build the reactive binary from the ReScript repo:

   ```
   cd /Users/cristianocalcagno/GitHub/rescript
   dune build --root reactive_graph reactive_analysis/bin/reactive_main.exe
   ```

2. Run it by pointing to the directory with `.graph` files. The executable
   creates `graph.rheap` by default; pass a custom heap path as the first
   argument if you need to override it (macOS users should delete heaps between
   runs, but the CLI now removes the default automatically):

```
./_build/default/reactive_analysis/bin/reactive_main.exe reactive_graph/data
```

   The first run parses all files and caches the intermediate representations.
   Subsequent runs only re-parse and recompute for the files that change.
   (Any heap filename works; `graph.rheap` is just a short example.)
   After each run the CLI now prints the reactive heap usage using the
   `Reactive.heap_usage()` API, so you can see how many bytes of the `.rheap`
   file are currently occupied.

### Makefile shortcuts

From `reactive_graph/` you can use the tiny Makefile:

```
make traditional-run   # build + run the non-reactive analysis
make reactive-run      # build runtime + reactive binary + run
make clean
```

`make reactive-run` builds the vendored runtime automatically (via
`make -C vendor/skip-ocaml ...`) before invoking the executable.

### macOS note

Because ASLR prevents reusing heaps across fresh processes, macOS users must
delete the `.rheap` file before every new invocation of the reactive binary.
The runtime now exits immediately with an explanatory error if it detects an
existing heap file on macOS.

## Directory layout

```
graph_lib/       Shared parser and reachability logic
traditional_analysis/  Non-reactive CLI + tests
reactive_analysis/     Reactive CLI + vendored Skip runtime bindings
data/            Example input fragments
```

Both executables accept a directory path; they enumerate all `*.graph` files
inside that directory and report unreachable nodes in ascending order.
