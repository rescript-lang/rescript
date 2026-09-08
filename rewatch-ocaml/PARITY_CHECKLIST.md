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
| Missing/non-project folder and config discovery | Missing-folder wording is matched in the focused runner; configuration-context cases pass, but the full source-location inventory remains pending | Partial |
| Configuration schema and aliases | Unit tests plus canonical config, feature, experimental, warning, suffix, and GenType cases; source `type` and legacy GenType shim normalization/map semantics are matched | Partial |
| Package/dependency graph | Canonical compile/feature cases and graph unit tests | Partial |
| Compiler/runtime/executable discovery | Focused subprocess tests; platform implementations are type-checked | Partial |
| Locks and watcher lifecycle | Canonical lock/watch cases and focused stale-lock tests | Partial |
| Output ownership and cleanup | Canonical clean/suffix cases and focused artifact tests | Partial |
| CLI and format input validation | Dedicated Cmdliner tests mirror Rust CLI cases; canonical format/compiler-args cases | Partial |

No row becomes complete until the Rust source inventory has been performed,
not merely because the current tests pass.

### Configuration inventory

Symbols below are stable source locations; line numbers are intentionally
omitted because the Rust and OCaml files are still changing.

| Behavior | Rust location | OCaml location | Evidence | Status |
| --- | --- | --- | --- | --- |
| File read, JSON root, required `name`, and legacy filename | `config.rs`: `Config::new`, `Config::new_from_json_string`, `Config::set_path` | `config.ml`: `load`, `load_root` | Unit tests plus focused missing-project/config tests | Partial; exact parse-error inventory remains |
| Source forms, `dir`, `subdirs`, `type`, feature inheritance | `config.rs`: `Source`, `PackageSource`; `build/packages.rs`: `get_source_dirs` | `config.ml`: `sources_of_json`, `parse_sources` | Unit tests cover non-dev strings and parent type propagation; canonical source/feature tests | Partial; all invalid shapes still need cataloguing |
| Package module, suffix/location defaults and duplicate outputs | `config.rs`: `PackageSpec`, `validate_package_specs_value` | `config.ml`: `parse_package_spec`, duplicate-output check in `load` | Unit tests and canonical suffix tests | Matched for inventoried checks |
| Dependency forms, aliases, feature maps and cycles | `config.rs`: `Dependency`, `resolve_active_features`; package traversal | `config.ml`: `dependency_name`, `dependency_alias`; `build.ml` feature resolution | Rust/OCaml unit tests and canonical feature/dependency tests | Partial; diagnostic/source inventory remains |
| Compiler, warning, and PPX flags | `config.rs`: `flatten_flags`, `flatten_ppx_flags`, `get_warning_args` | `config.ml`: `compiler_flags`, warning/PPX parsing; `build.ml`: `compiler_flags` | Canonical compiler-argument tests | Partial |
| JSX and source maps | `config.rs`: `JsxSpecs`, `SourceMapConfig`, argument getters | `config.ml`: JSX and `sourceMap` branches in `load` | Unit tests plus canonical JSX/source-map builds | Partial; invalid JSX catalog remains |
| GenType schema and argument projection | `config.rs`: `GenTypeConfig`, `GenTypeShims`, `get_gentype_args` | `config.ml`: `gentype_args` | Unit tests cover defaults, suffix, normalization, duplicate shims; canonical GenType tests | Partial |
| Post-build command | `config.rs`: `JsPostBuild`; `build/compile.rs` execution | `config.ml`: `js_post_build`; `build.ml`: post-build execution | Canonical post-build tests | Partial; invalid-shape cases remain |
| Deprecated, unsupported, and unknown fields | `config.rs`: Serde aliases, `get_unknown_fields`, `get_unsupported_fields` | `config.ml`: alias diagnostics, `unknown_fields`, unsupported-field diagnostics | Unit tests; `config_tests.ml` covers Rust's exact nested-decoder warning boundary | Partial; complete alias list audit remains |

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

That mode also fails while any scenario is `unreviewed` or `gap`. The initial
inventory contains 136 Rust tests: 71 have been reviewed and 65 remain
unreviewed. A mapping is evidence only after its cited OCaml/shared test has
been inspected; grouping by similarly named functions is not sufficient.

## Output parity gate

Output is tested in two modes because Rust deliberately changes behavior based
on whether stdout and stderr are terminals.

| Mode | Required comparison | Current status |
| --- | --- | --- |
| Redirected/plain output | Success summaries, warnings, errors, ordering, exit status, and absence of terminal control sequences; Cmdliner help may use its native man-page headings and layout | Canonical snapshots cover important cases; inventory pending |
| Interactive build | TTY detection, parsing/compilation progress, spinner lifecycle, timing, colors, symbols/emojis, quiet/verbose behavior, and cleanup on interruption | Open; OCaml currently prints plain summaries |
| Interactive watch | Initial-build and rebuild progress, clear-screen behavior, persistent warnings, recovery errors, symbols/emojis, and orderly shutdown | Partial; clear-screen and lifecycle are covered, presentation parity is open |
| Accessibility/terminal fallback | Stable meaningful text when color or richer glyphs are unavailable | Open |

Interactive checks should run both implementations under a pseudo-terminal and
capture normalized frames/events rather than snapshotting spinner timing byte
for byte. Plain-output snapshots remain exact where paths and ANSI sequences
can be normalized deterministically.
