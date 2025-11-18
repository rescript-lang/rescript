# Vendored Skip Runtime (Source Snapshot)

This directory contains a trimmed copy of
[`skip-ocaml`](https://github.com/SkipLabs/skip-ocaml) that is just large enough
to build `libskip_reactive.a`, the static runtime needed by
`reactive_graph/reactive`.

Included components:

- `Makefile`
- Required C/C++ runtime sources under `src/c` and `external/runtime`
- The OCaml interface in `src/ocaml/reactive.{ml,mli}`
- License file

The upstream project can optionally link against `libbacktrace`, but that binary
is platform-specific and **not** vendored here. If you need stack traces backed
by libbacktrace, build it for your platform and place the resulting
`external/libbacktrace.a` next to `external/skip.ll` before running `make`.

Removed components:

- Tests, docs, scripts, and release artifacts that are irrelevant to building
  the static runtime

## Building the runtime

From the root of this directory:

```sh
make build/libskip_reactive.a
```

This compiles the static library in `build/libskip_reactive.a`. The top-level
reactive example references this path directly, so you must run this command at
least once (and again whenever you clean the vendor build) before building the
reactive executable.

## License

MIT (see `LICENSE`).
