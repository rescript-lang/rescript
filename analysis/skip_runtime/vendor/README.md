Vendored Skip runtime sources
=============================

The files in this directory are copied from
[`skip-ocaml`](https://github.com/skiplang/skip-ocaml) commit
`0a8ddbf29970869d9396d7e860e3b48bb3df8695`. Only the portions needed to build
`libskip_reactive.a` are included:

- `src/c`: platform glue for Skip’s reactive runtime
- `external/runtime`: the portable runtime implementation (with the 32-bit
  specific file excluded at build time)
- `external/skip.ll`: the LLVM IR module required by the runtime
- `external/libbacktrace.a`: prebuilt dependency used by the runtime when
  available

`scripts/build_skip_runtime.sh` compiles these sources locally so the ReScript
tree no longer depends on a sibling `skip-ocaml` checkout.

