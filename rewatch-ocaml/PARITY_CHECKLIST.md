# Rewatch parity checklist

This checklist complements the shared integration suite. A passing suite proves
the scenarios it exercises; it does not by itself prove that every Rust guard,
diagnostic, or interactive output path has an OCaml equivalent.

## Validation inventory gate

Before the port can replace Rust rewatch, inventory every user-reachable
validation and sanity check in the pinned Rust implementation. Search at least
these owners and record each check below (splitting rows as needed):

- `cli.rs` and `project_context.rs`: argument shape, command context, project
  discovery, missing folders, and configuration-file selection.
- `config.rs`: JSON shape, deprecated/unsupported fields, feature maps,
  package outputs, source directories, dependencies, warnings, JSX, GenType,
  and post-build configuration.
- `helpers.rs`, `lock.rs`, and `watcher.rs`: compiler/runtime discovery, path
  and executable checks, lock ownership, watcher lifecycle, and event inputs.
- `build.rs` and `build/*.rs`: package resolution, dependency permissions,
  duplicate modules, cycles, namespaces, compiler subprocess failures, output
  ownership, and cleanup safety.
- `format.rs`: input modes, extensions, formatter lookup/failure, and check
  status.

For every Rust check, the final inventory must name its Rust source location,
OCaml source location, and focused or canonical test. A missing check is an open
gap. A deliberate difference needs a rationale and regression test in
`PROGRESS.md`; similar wording alone is not proof of equivalent behavior.

| Validation area | Current evidence | Status |
| --- | --- | --- |
| Missing/non-project folder and config discovery | Missing-folder wording is matched in the focused runner; project-context tests distinguish listed parent dependencies from unrelated packages under a `package.json` workspace; the full source-location inventory remains pending | Partial |
| Configuration schema and aliases | Unit tests plus canonical config, feature, experimental, warning, suffix, and GenType cases; source `type` and legacy GenType shim normalization/map semantics are matched | Partial |
| Package/dependency graph | Canonical compile/feature cases and graph unit tests | Partial |
| Compiler/runtime/executable discovery | Packaged-layout test removes `RESCRIPT_BSC_EXE` and uses sibling `bsc.exe`; focused runtime test removes `RESCRIPT_RUNTIME` and resolves `@rescript/runtime`; platform path tests cover Windows verbatim drive and UNC paths | Matched for discovery and environment precedence; native Windows execution remains pending |
| Locks and watcher lifecycle | Canonical lock/watch cases and focused stale-lock tests | Partial |
| Output ownership and cleanup | Canonical clean/suffix cases and focused artifact tests | Partial |
| CLI and format input validation | Dedicated Cmdliner tests mirror Rust CLI cases; compiler-args tests cover dev/regular dependency selection and missing-package behavior; canonical format/compiler-args cases | Partial |

No row becomes complete until the Rust source inventory has been performed,
not merely because the current tests pass.

### Configuration inventory

Symbols below are stable source locations; line numbers are intentionally
omitted because the Rust and OCaml files are still changing.

| Behavior | Rust location | OCaml location | Evidence | Status |
| --- | --- | --- | --- | --- |
| File read, JSON root, required `name`, legacy filename, optional JSON `null`, and duplicate keys | `config.rs`: `Config::new`, `Config::new_from_json_string`, `Config::set_path`; Serde struct/`Option` fields | `config.ml`: `load`, `load_root`, `optional_member`, `reject_duplicate_fields` | Unit tests cover missing/directory paths without raw exceptions; focused missing-project test; differential audits covered 41 `null` positions and 15 duplicate-key cases | Partial; parse-error wording inventory remains |
| Source forms, `dir`, `subdirs`, `type`, feature inheritance | `config.rs`: `Source`, `PackageSource`; `build/packages.rs`: `get_source_dirs` | `config.ml`: `sources_of_json`, `parse_sources`, `source_is_dev` | A retained 36-case Rust/OCaml differential gate covers accepted and rejected outer, qualified, nested, nullable, unknown, and duplicate shapes and compares arguments for accepted cases; unit and canonical tests cover flattening and inheritance | Matched for the complete schema and flattening inventory |
| Package module, suffix/location defaults and duplicate outputs | `config.rs`: `PackageSpec`, `validate_package_specs_value` | `config.ml`: `parse_package_spec`, duplicate-output check in `load` | 28 differential schema/argument cases plus unit and canonical suffix tests | Matched for the complete schema and output-conflict inventory |
| Dependency forms, aliases, feature maps and cycles | `config.rs`: `Dependency`, `resolve_active_features`; package traversal | `config.ml`: `dependency_name`, `dependency_alias`; `build.ml` feature resolution | The differential gate adds 42 dependency, feature-map, alias, and `allowed-dependents` shapes; Rust/OCaml unit and canonical tests cover feature resolution, cycles, permissions, and traversal | Schema and feature algorithms matched; remaining package-resolution diagnostics stay in the broader source inventory |
| Compiler, warning, and PPX flags | `config.rs`: `flatten_flags`, `flatten_ppx_flags`, `get_warning_args`; `build/parse.rs`: `filter_ppx_flags`; `build/compile.rs`: `compiler_args` | `config.ml`: flag decoders; `build.ml`: `filter_ppx_flags`, phase-ordered `compiler_flags` | Exact unit argument-order/filter tests, focused filtered-PPX build, and canonical compiler-argument/PPX builds | Matched, with documented empty-argument and empty-PPX safety fixes |
| JSX and source maps | `config.rs`: `JsxSpecs`, `SourceMapConfig`, argument getters | `config.ml`: JSX and `sourceMap` branches in `load` | 53 differential schema/argument cases cover all fields, modes, nulls, JSON kinds, unknowns, and the reference decoder's incidental typed-vs-map duplicate-key distinction; unit and canonical build tests remain | Matched for schema and argument projection; diagnostic wording remains separate |
| GenType schema and argument projection | `config.rs`: `GenTypeConfig`, `GenTypeShims`, `get_gentype_args` | `config.ml`: `gentype_args` | Unit tests cover defaults, suffix, normalization, duplicate shims; canonical GenType tests | Partial |
| Post-build command | `config.rs`: `JsPostBuild`; `build/compile.rs` execution | `config.ml`: `js_post_build`; `build.ml`: post-build execution | 10 differential schema cases plus canonical execution tests | Matched for schema and Unix execution; native Windows command execution remains pending |
| Deprecated, unsupported, and unknown fields | `config.rs`: all five Serde aliases, `get_unknown_fields`, `get_unsupported_fields` | `config.ml`: alias diagnostics, `unknown_fields`, unsupported-field diagnostics | Unit/focused tests cover `bs-dependencies`, `bs-dev-dependencies`, `bsc-flags`, `cjs`, and `es6`, Rust's nested warning boundary, and ignored unsupported payloads | Matched for the complete alias and field-classification inventory |

## Rust unit-test coverage gate

[`tests/check_rust_test_coverage.sh`](tests/check_rust_test_coverage.sh)
discovers every `#[test]` and `#[tokio::test]` below `rewatch/src` and compares
that inventory with [`tests/rust_test_coverage.tsv`](tests/rust_test_coverage.tsv).
Each Rust test must map to focused OCaml coverage, the shared canonical suite,
an intentional architectural difference, an explicit project omission, or a
known gap. New Rust tests and stale mapping rows fail the ordinary check.

Run the stricter final gate with:

```bash
rewatch-ocaml/tests/check_rust_test_coverage.sh --require-complete
```

That mode also fails while any scenario is `unreviewed` or `gap`. The inventory
contains 136 Rust tests, all reviewed, with no remaining entries in either
category. They map to focused OCaml tests, the shared suite, accepted
architectural differences, or the explicit telemetry omission. A mapping is
evidence only after its cited OCaml/shared test has been inspected; grouping by
similar wording alone is not proof of equivalent behavior. Passing this unit
inventory does not replace the broader validation-source and interactive-output
gates in this document.

## Canonical integration-test coverage gate

CI runs the shared [`rewatch/tests/suite.sh`](../rewatch/tests/suite.sh) against
the packaged OCaml executable. Therefore its scenarios are exercised by the
port rather than copied into a second suite that could drift. The
[`tests/check_canonical_test_coverage.sh`](tests/check_canonical_test_coverage.sh)
guard inventories every test script below `rewatch/tests`, and fails if
`suite.sh` omits one, references a stale path, or references a test more than
once. It currently finds 48 canonical integration tests, all referenced
exactly once.

This establishes shared integration-test inclusion, not exhaustive parity on
its own. Rust source paths without a test remain the responsibility of the
validation inventory above, and output modes not exercised by the shell suite
remain the responsibility of the output gate below.

## Output parity gate

Output is tested in two modes because Rust deliberately changes behavior based
on whether stdout and stderr are terminals.

| Mode | Required comparison | Current status |
| --- | --- | --- |
| Redirected/plain output | Success summaries, warnings, errors, ordering, exit status, and absence of terminal control sequences; Cmdliner help may use its native man-page headings and layout | Canonical snapshots cover important cases; inventory pending |
| Interactive build | TTY detection, parsing/compilation progress, spinner lifecycle, timing, colors, symbols/emojis, quiet/verbose behavior, and cleanup on interruption | Partial; final status, warning state, timing, and emoji match, while phase progress and verbosity remain open |
| Interactive watch | Initial-build and rebuild progress, clear-screen behavior, persistent warnings, recovery errors, symbols/emojis, and orderly shutdown | Partial; final status, clear-screen, warning persistence, and lifecycle are covered, while phase presentation remains open |
| Accessibility/terminal fallback | Stable meaningful text when color or richer glyphs are unavailable | Open |

Interactive checks should run both implementations under a pseudo-terminal and
capture normalized frames/events rather than snapshotting spinner timing byte
for byte. Plain-output snapshots remain exact where paths and ANSI sequences
can be normalized deterministically.
