# Rewatch parity checklist

This checklist complements the shared integration suite. A passing suite proves
the scenarios it exercises; it does not by itself prove that every Rust guard,
diagnostic, or interactive output path has an OCaml equivalent.

## Architecture mapping gate

The final port must provide a clear mapping from each material Rust
responsibility, state value, algorithm, and lifecycle transition to its OCaml
owner. In particular, package discovery, build and compile-asset state,
dependency extraction and invalidation, parsing, compilation, cleanup, and the
watcher lifecycle must be traceable across the two implementations. The OCaml
implementation should perform equivalent work in the corresponding phase and
consume already-computed state where Rust does, rather than repeatedly using
the filesystem as an implicit database.

This is not a requirement to reproduce Rust file sizes, function boundaries,
or control-flow syntax mechanically. Idiomatic OCaml boundaries are preferred.
A material deviation needs a concrete correctness, portability,
maintainability, or simple-efficiency reason, and must be documented with its
behavioral evidence and Windows implications. Deliberate Rust bug fixes remain
permitted under the same rule.

Rust's existing OpenTelemetry spans may be used to identify phase ownership,
duration, and overlap while constructing this mapping. OTEL export remains an
intentional non-goal for the OCaml executable, and instrumented timings are
diagnostic rather than benchmark results. Filesystem-call parity is measured
separately with the retained syscall-audit tooling.

| Rust owner | Responsibility | OCaml owner | Mapping status |
| --- | --- | --- | --- |
| `build.rs` | Command build lifecycle and phase orchestration | `build.ml` | Present, but still mixed with phase implementations |
| `build/build_types.rs` | Packages, modules, dirty flags, dependency edges, and compiled-asset timestamps | `build_state.ml`, `source.ml`, and package records in `build.ml` | Explicit module state now owns resolved/reverse edges, dirty flags, and compiled-asset timestamps; package state and remaining freshness consumers are still split |
| `build/packages.rs` | Package resolution and source/module discovery | `build.ml`, `config.ml`, `source.ml`, `project_context.ml` | Behavior is substantially present and canonical resolved identities now flow through graph/build traversal; ownership and source inventories still need consolidation |
| `build/read_compile_state.rs` | One compile-asset inventory for cleanup and freshness | `compile_assets.ml` and `build_state.ml` | The shared inventory supplies CMI/CMT presence and timestamps to module/dependency freshness; AST/output freshness still has live filesystem consumers |
| `build/clean.rs` | Stale and explicit artifact cleanup | `build_artifacts.ml`, with command traversal in `build.ml` | Present; stale cleanup now accepts the shared compile-asset inventory |
| `build/deps.rs` | Dependency extraction, edges, and invalidation | `graph.ml` and dependency code in `build.ml` | Behavior present; extraction/invalidation still needs a cohesive owner |
| `build/parse.rs` | Parser jobs and parse-state transitions | Parsing code in `build.ml` | Behavior present; module split and explicit state transitions remain |
| `build/compile.rs` | Compiler arguments, dirty propagation, scheduling, publication | `build_state.ml`, `process.ml`, and compilation code in `build.ml` | Rust-shaped fixed pre-scheduling dirty state and byte-identical CMI-change propagation are present; publication/freshness ownership remains to be consolidated |
| `watcher.rs` | Watch handles, batching, rebuild lifecycle, and recovery | `native_watcher.ml` and watch code in `build.ml` | Native handles and behavior present; lifecycle ownership remains split |
| `lock.rs` | Build/watch ownership and stale-process handling | Lock code in `build.ml`, process operations behind `platform.mli` | Behavior present; final module-quality review remains |
| `telemetry.rs` | Optional OTLP export | No OCaml owner | Intentional project-level omission; Rust traces remain diagnostic tooling |

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
| Missing/non-project folder and config discovery | A differential command gate covers missing, config-less, and malformed build folders plus no-project compiler inputs; project-context tests distinguish listed parent dependencies from unrelated packages under a `package.json` workspace; the full package-resolution source inventory remains pending | Partial |
| Configuration schema and aliases | A 297-case differential gate covers every typed configuration family and exact arguments for shared accepted behavior; four documented Rust/OCaml divergences are explicit expectations rather than omitted cases | Matched for typed schema and argument projection; parse-error and diagnostic wording inventory remains |
| Package/dependency graph | Canonical compile/feature cases and graph unit tests; the differential command gate covers missing, config-less, malformed, metadata-name-mismatched, and malformed-package-metadata cases | Partial; OCaml retains Rust's package-name warning and strict metadata parsing but consistently uses the ReScript dependency name where Rust currently panics after mixing identities; remaining source guards and diagnostics are inventoried below |
| Compiler/runtime/executable discovery | Packaged-layout test removes `RESCRIPT_BSC_EXE` and uses sibling `bsc.exe`; focused runtime test removes `RESCRIPT_RUNTIME` and resolves `@rescript/runtime`; differential command gate covers a missing explicit compiler; platform path tests cover Windows verbatim drive and UNC paths | Matched for discovery and environment precedence; OCaml reports a stale explicit compiler as a normal contextual error while Rust currently panics; native Windows execution remains pending |
| Locks and watcher lifecycle | Canonical lock/watch cases, focused atomic/stale-lock tests, and differential malformed build/watch lock cases that preserve unknown ownership | Matched for acquisition, active-owner refusal/waiting, valid stale-owner takeover, malformed-owner refusal, workspace scope, and owned cleanup; native Windows process probing and watcher execution remain pending |
| Output ownership and cleanup | Canonical clean/suffix/removal cases, focused stale-artifact tests, and `clean_tests.ml` coverage that distinguishes configured outputs from neighboring unowned files in both the root and an installed dependency and removes abandoned watch sidecars | Matched for explicit clean ownership, stale compiler/output cleanup, and interrupted staging cleanup; native platform filesystem behavior remains pending |
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
| `compiler-args` positional and filesystem input | `cli.rs`: `Command::CompilerArgs`; `build.rs`: `get_compiler_args`; `helpers.rs`: `read_file`; `build/compile.rs`: dependency arguments | `cli.ml`: `compiler_args_term`; `build.ml`: `compiler_args` | `cli_tests.ml` covers missing and surplus paths; the differential command gate covers valid, non-ReScript, missing, and no-project sources; `compiler_args_tests.ml` covers dev/regular dependencies and context | Matched where Rust validates, with documented extension validation and three non-panicking OCaml fixes |
| Missing folder, missing config, and parent-config discovery failures | `lock.rs`: `get_lock`; `project_context.rs`: `ProjectContext::new`; `build/packages.rs`: `read_config` | `build.ml`: `project_root`, lock acquisition, `workspace_lock_root`; `config.ml`: `load_root` | The differential command gate covers nonexistent, config-less, malformed, directory-config, and malformed-parent project paths; the focused runner exactly checks missing-folder wording; configuration tests cover direct file-read failures | Matched for project/config discovery outcomes; exact diagnostic wording remains in the output inventory |
| Monorepo root/package classification | `project_context.rs`: `read_local_packages`, `is_config_listed_in_workspace`, `monorepo_or_single_project` | `build.ml`: `workspace_lock_root`, package traversal | `project_context_tests.ml` covers listed regular/dev packages and an unlisted package beneath a workspace; canonical monorepo builds cover symlinked traversal | Matched for classification; diagnostic inventory remains with package resolution |
| Dependency package resolution | `build/packages.rs`: `read_dependency`, `read_dependencies` | `build.ml`: `require_dependency_directory`, `prepare_global_graph`, `clean_internal` | The differential command gate covers missing paths, existing packages without config, and malformed dependency config for build and clean, plus watch startup; it requires exit 2 and verifies OCaml lock cleanup | Matched for failure outcomes; diagnostic text remains in the output inventory |
| Package metadata name | `build/packages.rs`: `read_package_name`, `make_package` | `package_metadata.ml`: `package_name`; `build.ml`: `validate_package_metadata` | Differential cases compare the root mismatch warning exactly, reject malformed `package.json`, and retain the dependency mismatch Rust panic as an explicit port fix; unit tests cover last-key and non-string name behavior | Matched validation and warning behavior; OCaml deliberately keeps the ReScript name as its consistent graph identity |
| Source module/interface identity | `build/packages.rs`: `parse_packages` implementation/interface branches | `source.ml`: `discover`, `duplicate_error`, `interface_mismatch_error` | Canonical duplicate-module and orphan-interface snapshots; the differential command gate covers a basename-case mismatch; `source_tests.ml` also covers duplicate implementations and cross-directory mismatches | Matched: implementation and interface paths must agree exactly before `.res`/`.resi`; module-name collisions remain deterministic errors and orphan interfaces are skipped with a diagnostic |
| Development source locality | `build/packages.rs`: `get_source_files`, `extend_with_children` (`package.is_local_dep && !prod`) | `build.ml`: `source_discovery_prod` at clean, graph preparation, and fallback package discovery | The differential command gate builds through an installed dependency containing a deliberately invalid dev-only source; focused unit coverage retains all local/production combinations; canonical dev-dependency and production builds exercise local packages | Matched: installed dependencies never contribute `type: "dev"` source folders, while local packages contribute them outside `--prod` |
| Missing source folders | `build/packages.rs`: `get_source_files` | `source.ml`: `scan_dir`; `build.ml`: `report_missing_source_folder` | Exact differential command case covers an active missing folder in an installed dependency; canonical watch recovery covers a missing local folder that is later created | Matched: missing active source folders are diagnosed with folder/package/root context but remain non-fatal; excluded dev/feature folders are not scanned |
| Formatter compiler execution and errors | `format.rs`: `format_stdin`, `format_files` | `format.ml`: `formatted`, `format_stdin`, `format_files` | Focused integration runs successful and invalid stdin formatting; `format_tests.ml` protects stable stdin/file error labels | Matched for subprocess failure classification; platform execution remains part of the Windows gate |
| Format check result | `format.rs`: `format_files` | `format.ml`: `format_files`, `format_check_summary` | Focused integration checks path, singular summary, error, and failure status; unit tests cover singular/plural messages | Matched |
| Implicit format project scope | `format.rs`: `get_files_in_scope`; `ProjectContext::get_scoped_local_packages`; `packages::make` | `format.ml`: `files_in_scope`, `local_dependency`, `package_sources`; `build.ml`: `is_local_dependency` | Four canonical format tests cover the current fixture, a single file, stdin, and formatting from a workspace package; focused integration proves that implicit format requires a config in the current directory rather than searching parents; `format_tests.ml` proves installed `node_modules` dependencies are excluded | Matched: the current package is always included, direct symlink-local regular/dev dependencies are included only at a monorepo root, a listed child formats only itself, transitive and installed dependencies are excluded, and all feature-gated source directories are considered |

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
| Interactive build | TTY detection, parsing/compilation progress, spinner lifecycle, timing, colors, symbols/emojis, quiet/verbose behavior, and cleanup on interruption | Partial; a retained Linux PTY gate exactly compares normalized cleanup/parse/compile completion lines, step counts, timing, phase emojis, and final status; warning state is covered separately, while live spinner updates and verbosity remain open |
| Interactive watch | Initial-build and rebuild progress, clear-screen behavior, persistent warnings, recovery errors, symbols/emojis, and orderly shutdown | Partial; initial three-step completion presentation has an exact PTY comparison, rebuilds emit two-step completion presentation, and final status, clear-screen, warning persistence, and lifecycle are covered; an exact rebuild PTY comparison and live spinner updates remain open |
| Accessibility/terminal fallback | Stable meaningful text when color or richer glyphs are unavailable | Open |

Interactive checks should run both implementations under a pseudo-terminal and
capture normalized frames/events rather than snapshotting spinner timing byte
for byte. Plain-output snapshots remain exact where paths and ANSI sequences
can be normalized deterministically.
