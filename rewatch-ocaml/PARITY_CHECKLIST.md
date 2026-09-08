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
| Configuration schema and aliases | A 297-case differential gate covers every typed configuration family and exact arguments for shared accepted behavior; four documented Rust/OCaml divergences are explicit expectations rather than omitted cases | Matched for typed schema and argument projection; parse-error and diagnostic wording inventory remains |
| Package/dependency graph | Canonical compile/feature cases and graph unit tests | Partial |
| Compiler/runtime/executable discovery | Packaged-layout test removes `RESCRIPT_BSC_EXE` and uses sibling `bsc.exe`; focused runtime test removes `RESCRIPT_RUNTIME` and resolves `@rescript/runtime`; platform path tests cover Windows verbatim drive and UNC paths | Matched for discovery and environment precedence; native Windows execution remains pending |
| Locks and watcher lifecycle | Canonical lock/watch cases and focused stale-lock tests | Partial |
| Output ownership and cleanup | Canonical clean/suffix cases and focused artifact tests | Partial |
| CLI and format input validation | Dedicated Cmdliner tests mirror Rust CLI cases, including required/surplus `compiler-args` paths; compiler-args tests cover extension, dependency selection, and missing-package behavior; focused format failures cover stdin labels and check summaries; canonical format/compiler-args cases cover success | CLI shape and format input validation matched; project-scope and remaining filesystem diagnostics stay in the source inventory |

No row becomes complete until the Rust source inventory has been performed,
not merely because the current tests pass.

### Configuration inventory

Symbols below are stable source locations; line numbers are intentionally
omitted because the Rust and OCaml files are still changing.

| Behavior | Rust location | OCaml location | Evidence | Status |
| --- | --- | --- | --- | --- |
| File read, JSON root, required `name`, internal `path`, legacy filename, optional JSON `null`, and duplicate keys | `config.rs`: `Config::new`, `Config::new_from_json_string`, `Config::set_path`; Serde struct/`Option` fields | `config.ml`: `load`, `load_root`, `optional_member`, `reject_duplicate_fields` | Unit tests cover missing/directory paths without raw exceptions and the user-deserializable internal `path` field; focused missing-project test; differential audits cover root/name/path shapes, 41 `null` positions, and representative duplicate keys | Partial; parse-error wording inventory remains |
| Source forms, `dir`, `subdirs`, `type`, feature inheritance | `config.rs`: `Source`, `PackageSource`; `build/packages.rs`: `get_source_dirs` | `config.ml`: `sources_of_json`, `parse_sources`, `source_is_dev` | A retained 36-case Rust/OCaml differential gate covers accepted and rejected outer, qualified, nested, nullable, unknown, and duplicate shapes and compares arguments for accepted cases; unit and canonical tests cover flattening and inheritance | Matched for the complete schema and flattening inventory |
| Package module, suffix/location defaults and duplicate outputs | `config.rs`: `PackageSpec`, `validate_package_specs_value` | `config.ml`: `parse_package_spec`, duplicate-output check in `load` | 28 differential schema/argument cases plus unit and canonical suffix tests | Matched for the complete schema and output-conflict inventory |
| Dependency forms, aliases, feature maps and cycles | `config.rs`: `Dependency`, `resolve_active_features`; package traversal | `config.ml`: `dependency_name`, `dependency_alias`; `build.ml` feature resolution | The differential gate adds 42 dependency, feature-map, alias, and `allowed-dependents` shapes; Rust/OCaml unit and canonical tests cover feature resolution, cycles, permissions, and traversal | Schema and feature algorithms matched; remaining package-resolution diagnostics stay in the broader source inventory |
| Compiler, warning, and PPX flags | `config.rs`: `Warnings`, `flatten_flags`, `flatten_ppx_flags`, `get_warning_args`; `build/parse.rs`: `filter_ppx_flags`; `build/compile.rs`: `compiler_args` | `config.ml`: flag and warning decoders; `build.ml`: `filter_ppx_flags`, phase-ordered `compiler_flags` | 37 differential cases cover valid and invalid shapes plus exact shared argument projection; explicit divergence cases retain whitespace normalization and the empty-PPX panic fix; exact unit argument-order/filter tests and canonical builds cover execution | Matched, with documented safety fixes |
| Namespace and namespace entry | `config.rs`: `NamespaceConfig`, `get_namespace`, `get_namespace_entry`; namespace argument helpers | `config.ml`: namespace branches in `load`; `build.ml`: `namespace_args` | 12 differential cases cover boolean/string normalization, scoped names, entries, nulls, and invalid kinds; canonical namespace builds cover artifacts | Matched, with documented rejection of an entry when namespace is disabled |
| JSX and source maps | `config.rs`: `JsxSpecs`, `SourceMapConfig`, argument getters | `config.ml`: JSX and `sourceMap` branches in `load` | 53 differential schema/argument cases cover all fields, modes, nulls, JSON kinds, unknowns, and the reference decoder's incidental typed-vs-map duplicate-key distinction; unit and canonical build tests remain | Matched for schema and argument projection; diagnostic wording remains separate |
| GenType schema and argument projection | `config.rs`: `GenTypeConfig`, `GenTypeShims`, `get_gentype_args` | `config.ml`: `gentype_args` | 53 differential cases cover every field, enum, JSON kind, nullable option, duplicate typed field, shim representation/map behavior, sorting, package fallback, sources, and dependencies; unit and canonical tests cover execution | Matched for the complete schema and argument projection inventory |
| Post-build command | `config.rs`: `JsPostBuild`; `build/compile.rs` execution | `config.ml`: `js_post_build`; `build.ml`: post-build execution | 10 differential schema cases plus canonical execution tests | Matched for schema and Unix execution; native Windows command execution remains pending |
| Deprecated, unsupported, and unknown fields | `config.rs`: all five Serde aliases, `get_unknown_fields`, `get_unsupported_fields` | `config.ml`: alias diagnostics, `unknown_fields`, unsupported-field diagnostics | Unit/focused tests cover `bs-dependencies`, `bs-dev-dependencies`, `bsc-flags`, `cjs`, and `es6`, Rust's nested warning boundary, and ignored unsupported payloads | Matched for the complete alias and field-classification inventory |

### CLI, project context, and format inventory

| Behavior | Rust location | OCaml location | Evidence | Status |
| --- | --- | --- | --- | --- |
| Implicit `build`, global flag placement, `--`, known commands, help, and version | `cli.rs`: `parse_with_default_from`, `should_default_to_build`, `build_default_args`; Clap command declaration | `cli.ml`: `normalize_argv`; Cmdliner command group | `cli_tests.ml` mirrors implicit/explicit routing, leading/trailing globals, short and long help/version, and `--` | Matched; Cmdliner help layout is an accepted presentation difference |
| Build/watch/clean option ownership and values | `cli.rs`: `BuildArgs`, `WatchArgs`, `Command`; feature and regex value parsers | `cli.ml`: `build_term`, `clean_term`, feature and filter converters | `cli_tests.ml` covers command-only rejection, boolean `--no-timing`, production mode, feature trimming/emptiness, invalid regex, and watch clear-screen | Matched for the declared option schema |
| Format input mode | `cli.rs`: `Command::Format`, `FileExtension`, Clap `format_input_mode` | `cli.ml`: `format_term` | `cli_tests.ml` covers `.res`/`.resi`, invalid extensions, stdin/file conflicts, and stdin/check conflicts in either argument order | Matched |
| `compiler-args` positional input | `cli.rs`: `Command::CompilerArgs`; `build/compile.rs`: `get_compiler_args` | `cli.ml`: `compiler_args_term`; `build.ml`: `compiler_args` | `cli_tests.ml` covers missing and surplus paths; `compiler_args_tests.ml` and focused integration cover extensions, dev/regular dependencies, context, and output | Matched for input validation, with the documented missing-regular-dependency panic fix |
| Missing folder, missing config, and parent-config discovery failures | `lock.rs`: `get_lock`; `project_context.rs`: `ProjectContext::new`; `build/packages.rs`: `read_config` | `build.ml`: `project_root`, lock acquisition, `workspace_lock_root`; `config.ml`: `load_root` | Focused runner exactly checks a nonexistent folder; configuration tests cover missing/directory paths | Partial; existing folders without a config and malformed parent configs still need explicit cross-implementation cases |
| Monorepo root/package classification | `project_context.rs`: `read_local_packages`, `is_config_listed_in_workspace`, `monorepo_or_single_project` | `build.ml`: `workspace_lock_root`, package traversal | `project_context_tests.ml` covers listed regular/dev packages and an unlisted package beneath a workspace; canonical monorepo builds cover symlinked traversal | Matched for classification; diagnostic inventory remains with package resolution |
| Formatter compiler execution and errors | `format.rs`: `format_stdin`, `format_files` | `format.ml`: `formatted`, `format_stdin`, `format_files` | Focused integration runs successful and invalid stdin formatting; `format_tests.ml` protects stable stdin/file error labels | Matched for subprocess failure classification; platform execution remains part of the Windows gate |
| Format check result | `format.rs`: `format_files` | `format.ml`: `format_files`, `format_check_summary` | Focused integration checks path, singular summary, error, and failure status; unit tests cover singular/plural messages | Matched |
| Implicit format project scope | `format.rs`: `get_files_in_scope`; `ProjectContext::get_scoped_local_packages`; `packages::make` | `format.ml`: `files_in_scope`, `local_dependency`, `package_sources` | Four canonical format tests cover the current fixture, a single file, stdin, and formatting from a workspace package; focused integration proves that implicit format requires a config in the current directory rather than searching parents | Partial; source-level equivalence for transitive/local dependency scope remains |

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
