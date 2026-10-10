# OCaml rewatch

This directory contains the OCaml implementation of ReScript's build system and
CLI (`rescript build`, `watch`, `clean`, `format`, and `compiler-args`). It
ships as `rescript` in the `@rescript/<platform>` packages. The Rust
implementation in [`../rewatch`](../rewatch) ships next to it as
`rescript-rust`, and both are tested with the shared integration suite in
[`../rewatch/tests`](../rewatch/tests).

The port follows Rust rewatch's architecture and algorithms. Every parse and
compile runs `bsc` as a separate process.

## Building and running

From the repository root, with the OPAM dependencies of `rescript.opam`
installed:

```sh
make                                     # builds bsc and packages/@rescript/<platform>/bin/rescript.exe
dune build rewatch-ocaml/rescript_ocaml.exe   # just this executable
```

The packaged `rescript.exe` finds `bsc.exe` next to itself, and the npm
launcher (`cli/rescript.js`) supplies the runtime path. To run the dune
executable directly, point it at a compiler and runtime:

```sh
export RESCRIPT_BSC_EXE="$PWD/packages/@rescript/linux-arm64/bin/bsc.exe"
export RESCRIPT_RUNTIME="$PWD/packages/@rescript/runtime"
_build/default/rewatch-ocaml/rescript_ocaml.exe build path/to/project
```

Linux release builds use dune's `static` profile, so the published executable
does not depend on the runner's libc. The version lives in
`rewatch_version.ml` and is kept in sync with the compiler and Rust rewatch by
`yarn constraints --fix`.

## Layout

| Area | Modules |
|---|---|
| CLI and entry point | `cli`, `rescript_ocaml`, `output` |
| Configuration | `config_types`, `config_decode` (strict JSON decoding), `config` |
| Projects and packages | `project_context` (workspace roots and dependency lookup), `package_resolution`, `package_traversal`, `package_graph`, `package_plan`, `package_diagnostics` |
| Sources | `source`, `source_filter` (`--filter`), `source_dirs` |
| Build orchestration | `build` (one build or watch rebuild), `build_preparation`, `build_session` (state kept across watch rebuilds), `build_attempt` (state of one attempt), `build_state`, `build_report` |
| Parsing and compiling | `package_parse`, `package_compilation`, `compiler_args`, `compiler_process`, `compiler_scheduler`, `compiler_info` (fingerprints that decide when a package is cleaned), `compile_assets`, `module_graph`, `graph` |
| Artifacts | `build_artifacts` (output paths, publication, stale cleanup), `build_freshness`, `clean`, `file_util` |
| Processes | `process` (worker pool), `process_child` (one child and its pipes), `after_build` |
| Platform | `platform.mli` implemented by `platform_unix.ml` or `platform_windows.ml` (selected by dune), `windows_job_stubs.c`, `termination_signals`, `build_lock` |
| Watching | `watcher` (rebuild loop), `watch_scope` (what to watch), `watch_snapshot` (filesystem snapshots), `native_watcher` (libuv notifications) |
| Other commands | `format`, `compiler_args_command` |

Native filesystem notifications only wake the watcher. The snapshot taken
afterwards decides what changed, so merged, reordered, or dropped events can't
cause missed rebuilds.

## Differences from Rust rewatch

These are intentional:

- `rescript.json` maps that the build system interprets reject duplicate keys
  instead of using the last one.
- A `namespace-entry` without a namespace is an error.
- Configuration errors use the compiler-style form `path: message`, and JSON
  syntax errors use `path:line:column: message`. Messages name the field and
  the accepted values instead of the parser's internal types.
- Dependencies are rebuilt when the root's package-specs change. Rust keeps
  their artifacts until `rescript clean`
  ([#8540](https://github.com/rescript-lang/rescript/pull/8540)).
- `rescript clean` also removes out-of-source JavaScript outputs.
- The CLI uses Cmdliner:
  - help is rendered as a man page;
  - `--no-timing` takes no value;
  - help takes precedence over version, and `--version` also works after a
    subcommand.
- `--filter` supports the common subset of Rust's regular expression syntax and
  rejects other constructs, such as `(?i)`, with an error. Non-ASCII names are
  matched byte-wise.
- A failing `--after-build` command fails the build. On Unix, hooks inherit
  redirected stdin but not an interactive terminal, because they run in their
  own process group.
- `format -v` and `compiler-args -v` write their logs to stderr, so stdout
  remains formatted source or JSON. Rust writes them to stdout.
- OpenTelemetry export is not implemented.

## Platforms

- **Unix:** children are started with `spawn` in their own process groups.
  SIGINT, SIGTERM, SIGHUP, and SIGQUIT terminate those groups before
  `rescript` exits.
- **Windows:** children are started through `windows_job_stubs.c`, which
  assigns each child to a Job Object before it runs. Cancellation then also
  ends its descendants.
- **Pipes:** each child's stdout and stderr are drained by reader threads,
  because Windows can't `select` on anonymous pipes.
- **Watching:** uses libuv through `luv`. `.github/opam-repository` holds a
  patched luv package for static arm64 musl builds.

The test suite compiles `platform_windows.ml` on every platform, so the Windows
backend is always type-checked. Its process and Job Object paths only run on
Windows CI.

## Testing

```sh
dune runtest tests/rewatch_ounit_tests                          # unit tests
sh rewatch-ocaml/tests/run.sh "$PWD/packages/@rescript/<platform>/bin/rescript.exe"   # focused end-to-end tests
make test-rewatch                                               # shared suite in rewatch/tests
```

The scripts in `tests/check_*.sh` compare the OCaml and Rust executables on
configuration acceptance, command validation, and verbose and interactive
output. `tests/check_canonical_test_coverage.sh` ensures that every shared test
script is run by `rewatch/tests/suite.sh`. Benchmarks and their equivalence
checks are described in [`bench/README.md`](bench/README.md).

## Packaging

Each platform package contains `bin/rescript.exe` (this implementation) and
`bin/rescript-rust.exe`, together with `THIRD_PARTY_NOTICES_REWATCH.md` and
`RE_LICENSE.md` for the bundled OCaml libraries. After a build,
`node scripts/updateArtifactList.js` must leave `packages/artifacts.json`
unchanged.
