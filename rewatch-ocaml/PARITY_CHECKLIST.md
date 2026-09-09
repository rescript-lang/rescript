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
| `project_context.rs` | Workspace classification, package lookup, locality, and path presentation | `project_context.ml` | Explicit owner now selects the ReScript-level workspace root, resolves ancestor/hoisted/sibling package paths, classifies canonical local dependencies, and presents project-relative paths; graph traversal retains the command-wide resolved-package cache |
| `build/build_types.rs` | Packages, modules, dirty flags, dependency edges, and compiled-asset timestamps | `build_state.ml`, `source.ml`, and package records in `build.ml` | Explicit module state owns resolved/reverse edges, dirty flags, and compiled-asset timestamps; package discovery retains source mtimes, while broader package state remains split |
| `build/packages.rs` | Package resolution and source/module discovery | `project_context.ml`, `build.ml`, `config.ml`, and `source.ml` | `project_context.ml` owns path resolution/locality while graph traversal in `build.ml` reserves all direct dependency names before recursion and owns compilation modules; source discovery retains mtimes, the full cleanup leaf inventory, and GenType directories; Unix traversal deduplicates directories by metadata identity while Windows uses canonical paths |
| `build/read_compile_state.rs` | One compile-asset inventory for cleanup and freshness | `compile_assets.ml` and `build_state.ml` | The shared inventory supplies CMI/CMT timestamps and published-AST source locations/mtimes; source/AST freshness consumes those snapshots and, like Rust, metadata is read only for AST/IAST/CMI/CMT state |
| `build/compiler_info.rs` | Compiler/config fingerprints and package invalidation | `compiler_info.ml` | Compiler, runtime, package config, source-map arguments, effective root package-output specs, and artifact build owner invalidate affected packages; previous output specs drive precise stale-output removal, including installed source dependencies, while independently built package outputs remain owned by that package rather than an unrelated consumer |
| `build/logs.rs` | Compiler-log lifecycle and ANSI-free persisted diagnostics | `compiler_log.ml` | Explicit owner initializes, appends, finalizes, strips terminal control sequences, and publishes each package log; warning selection remains with compilation as in Rust's `build/compile.rs` |
| `build/clean.rs` | Stale and explicit artifact cleanup | `build_artifacts.ml`, with command traversal in `build.ml` | Present; stale cleanup consumes shared compile-asset/source inventories and directly calculated working paths; consumer clean preserves independently owned dependency artifacts instead of mutating a published package, while consumer-built source dependencies remain recursive; a whole-tree `lib/bs` scan is retained only as a lazy malformed/legacy-artifact fallback |
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
| Missing/non-project folder and config discovery | A differential command gate covers missing, config-less, and malformed build folders plus no-project compiler inputs; project-context tests distinguish listed parent dependencies from unrelated packages under a `package.json` workspace | Matched for `ProjectContext::new`, parent-config selection, and root/config discovery; command tests retain the selected configuration path while parser- and OS-native detail is allowed to differ |
| Configuration schema and aliases | A 297-case differential gate covers every typed configuration family and exact arguments for shared accepted behavior; four documented Rust/OCaml divergences are explicit expectations rather than omitted cases | Matched for typed schema, argument projection, and contextual failure categories; Serde and Yojson parse locations/wording remain library-native |
| Package/dependency graph | Canonical compile/feature cases and graph unit tests; the differential command gate covers missing, config-less, malformed, duplicate-path, metadata-name-mismatched, malformed-package-metadata, and omitted dependency-source cases across the commands that construct the graph; parse/compile/cleanup guards are inventoried below | Matched for graph construction, scheduling, reachable cleanup states, and stable package/path diagnostic context; implicit format now validates and scans the complete graph before selecting its local files, and exact warning checks retain first-path duplicate selection, omitted-`sources`, and missing-source-folder behavior |
| Compiler/runtime/executable discovery | Packaged-layout test removes `RESCRIPT_BSC_EXE` and uses sibling `bsc.exe`; focused runtime test removes `RESCRIPT_RUNTIME` and resolves `@rescript/runtime`; differential command cases cover missing explicit compiler/runtime paths and an unavailable runtime package; platform path tests cover Windows verbatim drive and UNC paths | Matched for discovery and environment precedence; OCaml reports a stale explicit compiler normally instead of Rust's panic and rejects a stale explicit runtime before spawning compiler work instead of Rust's later `Pervasives` failures; native Windows execution remains pending |
| Locks and watcher lifecycle | Canonical lock/watch cases, focused atomic/stale-lock tests, a slow-child lock-removal integration case, scheduler cancellation unit coverage, and differential malformed build/watch lock cases that preserve unknown ownership | Matched for acquisition, active-owner refusal/waiting, valid stale-owner takeover, malformed-owner refusal, workspace scope, owned cleanup, and lock-driven cancellation of active child process trees; native Windows process probing and watcher execution remain pending |
| Output ownership and cleanup | Canonical clean/suffix/removal cases, focused stale-artifact tests, and `clean_tests.ml` coverage that distinguishes configured outputs from neighboring unowned files in both the root and an installed dependency and removes abandoned watch sidecars; the fresh-build gate compares the complete Rust/OCaml file-name set and hashes every byte-stable artifact class | Matched for explicit clean ownership, fresh outputs/control files, stale compiler/output cleanup, and interrupted staging cleanup; native platform filesystem behavior remains pending |
| CLI and format input validation | Dedicated Cmdliner tests mirror Rust CLI cases, including required/surplus `compiler-args` paths; compiler-args tests cover extension, dependency selection, and missing-package behavior; focused format failures cover stdin labels, check summaries, and contextual write errors; differential cases exactly compare explicit missing-file, directory, and unsupported-extension failures, retain the stale-compiler Rust panic fix, semantically compare missing/malformed/directory-valued implicit project configs and invalid stdin, and exercise permission-denied write-back where supported; canonical format/compiler-args cases cover success | Matched or documented across CLI shape, formatter/compiler discovery, explicit-file failures, implicit project discovery, stdin, check status, and write-back; OS errors, temporary names, and malformed JSON retain platform/library-native detail after common command/path context |

No row becomes complete until the Rust source inventory has been performed,
not merely because the current tests pass.

### Configuration inventory

Symbols below are stable source locations; line numbers are intentionally
omitted because the Rust and OCaml files are still changing.

| Behavior | Rust location | OCaml location | Evidence | Status |
| --- | --- | --- | --- | --- |
| File read, JSON root, required `name`, internal `path`, legacy filename, optional JSON `null`, and duplicate keys | `config.rs`: `Config::new`, `Config::new_from_json_string`, `Config::set_path`; Serde struct/`Option` fields | `config.ml`: `load`, `load_root`, `optional_member`, `reject_duplicate_fields` | Unit tests cover missing/directory paths without raw exceptions and the user-deserializable internal `path` field; focused missing-project test; differential audits cover root/name/path shapes, 41 `null` positions, and representative duplicate keys; command cases retain missing, malformed, parent, and directory-valued config paths | Matched; Serde/Yojson parse locations and OS error detail intentionally remain library/platform-native after shared path context |
| Source forms, `dir`, `subdirs`, `type`, feature inheritance | `config.rs`: `Source`, `PackageSource`; `build/packages.rs`: `get_source_dirs`, `make_package` | `config.ml`: `sources_of_json`, `parse_sources`, `source_is_dev`, `sources_defined`; `build.ml`: `report_missing_sources` | A retained 36-case Rust/OCaml differential gate covers accepted and rejected outer, qualified, nested, nullable, unknown, and duplicate shapes and compares arguments for accepted cases; unit and canonical tests cover flattening and inheritance; the command gate exactly compares the omitted-`sources` dependency warning for build, clean, and format | Matched for the complete schema, field-presence warning, and flattening inventory; an explicit empty list remains distinct from an omitted field |
| Package module, suffix/location defaults and duplicate outputs | `config.rs`: `PackageSpec`, `validate_package_specs_value` | `config.ml`: `parse_package_spec`, duplicate-output check in `load` | 28 differential schema/argument cases plus unit and canonical suffix tests | Matched for the complete schema and output-conflict inventory |
| Dependency forms, aliases, feature maps and cycles | `config.rs`: `Dependency`, `resolve_active_features`, `compute_active_features`; package traversal | `config.ml`: `dependency_name`, `dependency_alias`; `source.ml`: `resolve_active_features`; `build.ml` and `format.ml`: consumer-request aggregation | The differential gate adds 42 dependency, feature-map, alias, and `allowed-dependents` shapes; Rust/OCaml unit and canonical tests cover feature resolution, cycles, permissions, and traversal; command cases distinguish an unrestricted irrelevant cycle from a specifically requested cycle during build and format | Schema and feature algorithms matched: all-features selection does not traverse implication edges, while restricted requests are unioned and transitively validated; resolution failures are covered by the package-discovery row below |
| Compiler, warning, and PPX flags | `config.rs`: `Warnings`, `flatten_flags`, `flatten_ppx_flags`, `get_warning_args`; `build/parse.rs`: `filter_ppx_flags`; `build/compile.rs`: `compiler_args` | `config.ml`: flag and warning decoders; `build.ml`: `filter_ppx_flags`, phase-ordered `compiler_flags` | 37 differential cases cover valid and invalid shapes plus exact shared argument projection; explicit divergence cases retain whitespace normalization and the empty-PPX panic fix; exact unit argument-order/filter tests and canonical builds cover execution | Matched, with documented safety fixes |
| Namespace and namespace entry | `config.rs`: `NamespaceConfig`, `get_namespace`, `get_namespace_entry`; namespace argument helpers | `config.ml`: namespace branches in `load`; `build.ml`: `namespace_args` | 12 differential cases cover boolean/string normalization, scoped names, entries, nulls, and invalid kinds; canonical namespace builds cover artifacts | Matched, with documented rejection of an entry when namespace is disabled |
| JSX and source maps | `config.rs`: `JsxSpecs`, `SourceMapConfig`, argument getters | `config.ml`: JSX and `sourceMap` branches in `load` | 53 differential schema/argument cases cover all fields, modes, nulls, JSON kinds, unknowns, and the reference decoder's incidental typed-vs-map duplicate-key distinction; all rejected cases retain the owning `jsx` or `sourceMap` context; unit and canonical build tests cover execution | Matched for schema, argument projection, and contextual rejection; Serde/Yojson wording remains implementation-native |
| GenType schema and argument projection | `config.rs`: `GenTypeConfig`, `GenTypeShims`, `get_gentype_args`; `build/packages.rs`: `collect_gentype_source_dirs` | `config.ml`: `gentype_args`; `source.ml`: `discovery.gentype_dirs` | 53 differential cases cover every field, enum, JSON kind, nullable option, duplicate typed field, shim representation/map behavior, sorting, package fallback, sources, and dependencies; unit tests cover recursive directory discovery and feature/dev-source selection; canonical tests cover execution | Matched for the complete schema and argument projection inventory; source directories are now package-discovery state as in Rust |
| Post-build command | `config.rs`: `JsPostBuild`; `build/compile.rs` execution | `config.ml`: `js_post_build`; `build.ml`: post-build execution | 10 differential schema cases plus canonical execution tests | Matched for schema and Unix execution; native Windows command execution remains pending |
| Deprecated, unsupported, and unknown fields | `config.rs`: all five Serde aliases, `get_unknown_fields`, `get_unsupported_fields` | `config.ml`: alias diagnostics, `unknown_fields`, unsupported-field diagnostics | Unit/focused tests cover `bs-dependencies`, `bs-dev-dependencies`, `bsc-flags`, `cjs`, and `es6`, Rust's nested warning boundary, and ignored unsupported payloads | Matched for the complete alias and field-classification inventory |

### CLI, project context, and format inventory

| Behavior | Rust location | OCaml location | Evidence | Status |
| --- | --- | --- | --- | --- |
| Implicit `build`, global flag placement, `--`, known commands, help, and version | `cli.rs`: `parse_with_default_from`, `should_default_to_build`, `build_default_args`; Clap command declaration | `cli.ml`: `normalize_argv`; Cmdliner command group | `cli_tests.ml` mirrors implicit/explicit routing, leading/trailing globals, short and long help/version, and `--` | Matched; Cmdliner help layout is an accepted presentation difference |
| Build/watch/clean option ownership and values | `cli.rs`: `BuildArgs`, `WatchArgs`, `Command`; feature and regex value parsers | `cli.ml`: `build_term`, `clean_term`, feature and filter converters | `cli_tests.ml` covers command-only rejection, boolean `--no-timing`, production mode, feature trimming/emptiness, invalid regex, watch clear-screen, and retained clean verbosity | Matched for the declared option schema |
| Source filter matching | `build/packages.rs`: `matches_filter`, `read_folders` | `source.ml`: `discover_with_inventory`; `cli.ml`: filter validation | Differential builds prove that a directory-only regex excludes a nested source while a basename regex includes it; focused source tests retain both boundaries | Basename and positive-match semantics are matched. Known syntax gap: Rust uses the `regex` crate whereas OCaml uses `Str`; basic shared expressions work, but alternation, grouping, shorthand classes, repetition syntax, and some validation outcomes can differ. `re` was evaluated but deliberately not added because its Perl frontend is closer rather than exact (including incomplete Rust-compatible Unicode properties). |
| Watch filter consistency | `build/packages.rs`: positive `matches_filter`; `watcher.rs`: event `matches_filter` | `source.ml`: positive discovery filter; `build.ml`: complete snapshot wakeup followed by filtered graph preparation | A differential watch case waits for initial output and after-build markers, edits an included then excluded source for Rust, and proves its included output remains stale after the later build; the OCaml side requires the included edit to complete a hook and change the output | Deliberate Rust bug fix: OCaml retains the CLI's positive regex meaning across initial discovery and every rebuild instead of negating it only for watch events |
| Format input mode and explicit files | `cli.rs`: `Command::Format`, `FileExtension`, Clap `format_input_mode`; `format.rs`: `format_files` | `cli.ml`: `format_term`; `format.ml`: `format_files_with_bsc` | `cli_tests.ml` covers `.res`/`.resi`, invalid stdin extensions, stdin/file conflicts, and stdin/check conflicts in either argument order; differential cases exactly compare missing-file, directory, and unsupported-extension diagnostics | Matched, including errors delegated to the formatter for explicit operands |
| `compiler-args` positional and filesystem input | `cli.rs`: `Command::CompilerArgs`; `build.rs`: `get_compiler_args`; `helpers.rs`: `read_file`; `build/compile.rs`: dependency arguments | `cli.ml`: `compiler_args_term`; `build.ml`: `compiler_args` | `cli_tests.ml` covers missing and surplus paths; the differential command gate covers valid, non-ReScript, missing, and no-project sources; `compiler_args_tests.ml` covers dev/regular dependencies and context | Matched where Rust validates, with documented extension validation and three non-panicking OCaml fixes |
| After-build hook execution | `main.rs`: successful-build hook dispatch; `cmd.rs`: `run` | `build.ml`: `run_after_build` and post-success dispatch | Canonical/focused integration covers a successful hook; three differential command cases cover an empty command, a missing program, and a program exiting 7 with captured stderr | Deliberate safety fixes: OCaml reports empty and unlaunchable hooks normally rather than panicking, and makes a nonzero hook fail the command instead of discarding its status; all commands are still split on whitespace and launched outside the build lock like Rust |
| Per-output JS post-build hook | `build/compile.rs`: `execute_post_build_command` and `compile_file` | `build.ml`: `run_post_build`; `platform.mli` command construction | Focused integration checks the generated-file argument for a successful hook; the differential command gate makes the shell command exit 7 and requires both diagnostics to identify the generated JavaScript path | Matched for invocation timing, working directory, output argument, failure status, and path-bearing diagnostic on Unix; native `cmd.exe` quoting remains part of the Windows gate |
| Missing folder, missing config, and parent-config discovery failures | `lock.rs`: `get_lock`; `project_context.rs`: `ProjectContext::new`; `build/packages.rs`: `read_config` | `build.ml`: `project_root`, lock acquisition, `workspace_lock_root`; `config.ml`: `load_root` | The differential command gate covers nonexistent, config-less, malformed, directory-config, and malformed-parent project paths; the focused runner exactly checks missing-folder wording; configuration tests cover direct file-read failures | Matched for project/config discovery outcomes and selected-path context; JSON-parser and OS-error tails remain implementation-native |
| Monorepo root/package classification | `project_context.rs`: `read_local_packages`, `is_config_listed_in_workspace`, `monorepo_or_single_project` | `build.ml`: `workspace_lock_root`, package traversal | `project_context_tests.ml` covers listed regular/dev packages and an unlisted package beneath a workspace; canonical monorepo builds cover symlinked traversal | Matched for classification; package-resolution failures are covered separately below |
| Dependency package resolution | `build/packages.rs`: `read_dependency`, `read_dependencies` | `build.ml`: `require_dependency_directory`, `prepare_global_graph`, `clean_internal` | The differential command gate covers missing paths, existing packages without config, and malformed dependency config for build and clean, plus watch startup; it requires exit 2, retains the shared dependency/workspace diagnostic prefix, and verifies OCaml lock cleanup | Matched for failure outcomes and stable diagnostic context; canonicalization, OS, and JSON-parser tails remain implementation-native |
| Dependency permission enforcement | `build/packages.rs`: `get_unallowed_dependents`, `validate_packages_dependencies` | `build.ml`: `dependent_is_allowed`, active-edge validation in `prepare_global_graph` | Focused OUnit coverage rejects a traversed disallowed dependency; one differential case makes an installed package declare a dormant dev edge to another root dependency, while another gives a root two actively denied regular dependencies and inspects both implementations' complete output | Matched for every traversed regular/local-dev edge; deliberate Rust bug fixes: dormant installed dev edges are ignored, every denied edge is reported instead of only the first per dependency class, details remain on stderr, and guidance names `allowed-dependents` in `rescript.json` rather than obsolete identifiers |
| Duplicate dependency path selection | `build/packages.rs`: `read_dependencies` registered-dependency branch | `build.ml`: `prepare_global_graph`, `resolved_packages` | The differential command gate places one dependency both at the root and below another package, requires both builds to succeed, and requires the duplicate warning from each implementation | Matched: the first path selected for a requested dependency name is retained throughout the graph; later paths warn and reuse it rather than inserting duplicate modules |
| Package metadata name | `build/packages.rs`: `read_package_name`, `make_package` | `package_metadata.ml`: `package_name`; `build.ml`: `validate_package_metadata` | Differential cases compare the root mismatch warning exactly, reject malformed `package.json`, and retain the dependency mismatch Rust panic as an explicit port fix; unit tests cover last-key and non-string name behavior | Matched validation and warning behavior; OCaml deliberately keeps the ReScript name as its consistent graph identity |
| Source module/interface identity | `build/packages.rs`: `parse_packages` implementation/interface branches | `source.ml`: `discover`, `duplicate_error`, `interface_mismatch_error` | Canonical duplicate-module and orphan-interface snapshots; the differential command gate covers a basename-case mismatch; `source_tests.ml` also covers duplicate implementations and cross-directory mismatches | Matched: implementation and interface paths must agree exactly before `.res`/`.resi`; module-name collisions remain deterministic errors and orphan interfaces are skipped with a diagnostic |
| Namespace map membership | `helpers.rs`: `is_non_exotic_module_name`; `build/packages.rs`: namespace `depending_modules`/`deps` construction | `source.ml`: `is_non_exotic_module_name`; `build.ml`: `namespace_job` | A differential namespace build compares the complete generated `.mlmap` with ordinary and punctuated source names; `source_tests.ml` covers ordinary, punctuated, and empty predicate inputs | Matched: only ASCII module identifiers beginning with an uppercase letter enter the namespace map; exotic source files may still compile but are not exported through the namespace |
| Development source locality | `build/packages.rs`: `get_source_files`, `extend_with_children` (`package.is_local_dep && !prod`) | `build.ml`: `source_discovery_prod` at clean, graph preparation, and fallback package discovery | The differential command gate builds through an installed dependency containing a deliberately invalid dev-only source; focused unit coverage retains all local/production combinations; canonical dev-dependency and production builds exercise local packages | Matched: installed dependencies never contribute `type: "dev"` source folders, while local packages contribute them outside `--prod` |
| Missing or non-directory source folders | `build/packages.rs`: `get_source_files` | `source.ml`: `scan_dir`; `build.ml`: `report_missing_source_folder`; `format.ml`: complete graph scan | Exact differential command cases cover an active missing folder in an installed dependency during build and implicit format plus a configured regular file; canonical watch recovery covers a missing local folder that is later created | Matched: unavailable active source directories are diagnosed with folder/package/root context but remain non-fatal; excluded dev/feature folders are not scanned |
| Parse execution and graph invariants | `build/parse.rs`: `generate_asts`, `generate_ast`; `build/deps.rs`: `get_dep_modules` | `build.ml`: `parse_job`, `ast_dependencies`, global parse scheduling and publication | Canonical syntax-error, warning-persistence, rename, and deletion cases plus the exact compiler-work gate cover normal failures and state transitions; an isolated differential syntax-error build exactly compares normalized stdout/stderr and phase classification; differential compiler wrappers remove discovered sources before later parse work and a successfully generated AST before dependency extraction; package/module/namespace lookups are graph-construction invariants in both implementations | Matched for compiler outcomes and internal state; OCaml reports both disappearing-source and missing-AST races normally while Rust panics |
| Compile execution and dependency scheduling | `build/compile.rs`: scheduler, `dependency_cycle`, `compiler_args`, `compile_file`, dirty propagation | `build.ml`: global graph resolution, scheduled compilation, `publish_compiled`; `build_state.ml`; `graph.ml`: shortest-cycle selection | Canonical cycle, missing-module, interface, warning, namespace, feature, and incremental-watch cases; exact clean/unchanged/edit work manifests; fresh-tree artifact equivalence; focused `-bs-no-bin-annot` build; graph coverage requires the shortest of two disjoint cycles | Matched for user-reachable compiler and scheduler outcomes, including optional CMT/CMTI debug artifacts; equal shortest cycles and rotations are deliberately lexical rather than hash-order-dependent; package/module/interface unwraps are invariants established by the graph and scheduled job shape |
| Previous-state and stale-output cleanup | `build/read_compile_state.rs`; `build/clean.rs`: `cleanup_previous_build`, `cleanup_after_build` | `compile_assets.ml`; `build_artifacts.ml`: `cleanup_stale`; `build.ml` finalization | Canonical rename/deletion/clean/suffix/feature cases and focused malformed-AST fallback, stale output, deleted-JS repair, interrupted staging, and compiler-info invalidation tests | Matched for reachable artifact states; malformed or unreadable AST state is ignored/recovered rather than unwrapped, and internal AST/module/package associations remain construction invariants |
| Editor cache-bust marker | `build.rs`: `write_build_ninja`; full rebuild call sites in `build` and `watcher.rs` | `build.ml`: `write_build_ninja`, success/failure finalization | Isolated complete control-file manifests differed only by this marker before the fix; focused success and compiler-failure builds require it, and the differential config-recovery watch case requires it after rebuilding | Matched for normal builds and structural watch recovery; the snapshot watcher conservatively rewrites it after every post-initial rebuild because it does not expose Rust's event-kind classification |
| Compiler artifact publication I/O failures | `build/compile.rs`: post-compile `fs::copy(...).expect(...)` calls | `build_artifacts.ml`: `copy_existing_file`; `rescript_ocaml.ml`: top-level I/O errors | Fresh-tree manifests prove normal publication equivalence; a differential compiler wrapper deletes the source after successful compilation; focused tests cover deleted-output repair and watch staging | Deliberate safety fix: OCaml emits a path-bearing normal error, while Rust panics its worker and leaves the scheduler waiting indefinitely; the bounded differential gate retains both outcomes |
| Watch reconfiguration failures | `watcher.rs`: full-rebuild `initialize_build(...).expect(...)` | `build.ml`: `watch` `run_build` error boundary | A differential lifecycle case waits for initial finalization, writes invalid JSON, observes Rust exit 101, verifies the OCaml watcher remains alive, restores a valid config with a new suffix, and waits for the rebuilt output | Deliberate safety fix: invalid intermediate config is recoverable in OCaml watch mode instead of panicking and terminating the watcher |
| Formatter compiler execution and errors | `format.rs`: `format_stdin`, `format_files` | `format.ml`: `formatted`, `format_stdin`, `format_files_with_bsc`, `format_files` | Focused integration runs successful and invalid stdin formatting; `format_tests.ml` protects stable stdin/file error labels and requires two independent formatter subprocesses to overlap | Matched for bounded parallel execution, stop-on-error scheduling, and subprocess failure classification; platform execution remains part of the Windows gate |
| Format check result | `format.rs`: `format_files` | `format.ml`: `format_files`, `format_check_summary` | Focused integration checks path, singular summary, error, and failure status; unit tests cover singular/plural messages | Matched |
| Implicit format project scope and graph validation | `format.rs`: `get_files_in_scope`; `ProjectContext::get_scoped_local_packages`; `packages::make` | `format.ml`: `files_in_scope`, `discover_package_graph`, `local_dependency`, `package_sources`; `build.ml`: package resolution and locality helpers | Four canonical format tests cover the current fixture, a single file, stdin, and formatting from a workspace package; focused integration proves that implicit format requires a config in the current directory rather than searching parents; `format_tests.ml` proves installed `node_modules` dependencies are excluded from formatting; the differential command gate covers missing, config-less, malformed, duplicate-path, omitted-source, requested feature-cycle, installed missing-source, and local dependency feature-selection cases | Matched: one complete applicable package scan supplies both graph diagnostics and the files to format; the current package uses all features, dependencies use the union of consumer feature requests, direct symlink-local regular/dev dependencies are included only at a monorepo root, a listed child formats only itself, and transitive or installed dependencies are validated but not formatted |

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
| Redirected/plain output | Success summaries, warnings, errors, ordering, exit status, and absence of terminal control sequences; Cmdliner help may use its native man-page headings and layout | Matched for ordinary output: differential cases exactly compare normalized clean-build, compile-error, parse-error, and successful-warning stdout/stderr plus default and quiet `clean`; format and compiler-args have focused/canonical output checks, and redirected watch lifecycle is covered by the canonical suite. Semantic `-v`/`-vv` events remain deliberately deferred with the interactive presentation pass |
| Interactive build | TTY detection, parsing/compilation progress, spinner lifecycle, timing, colors, symbols/emojis, quiet/verbose behavior, and cleanup on interruption | Partial; a retained Linux PTY gate exactly compares normalized cleanup/parse/compile completion lines, step counts, timing, phase emojis, and final status; warning state is covered separately, while live spinner updates and verbosity remain open |
| Interactive watch | Initial-build and rebuild progress, clear-screen behavior, persistent warnings, recovery errors, symbols/emojis, and orderly shutdown | Partial; a retained PTY gate exactly compares Rust/OCaml initial three-step and incremental two-step phase lines, counts, symbols, and final status after normalizing timing; clear-screen, warning persistence, recovery, and lifecycle are covered; live spinner updates remain open |
| Accessibility/terminal fallback | Stable meaningful text when color or richer glyphs are unavailable | Redirected output uses stable text without driver ANSI/glyph decoration and is covered by canonical snapshots; neither implementation probes terminal glyph support, so no richer fallback contract exists to port |

The remaining non-spinner output gap is diagnostic verbosity. A focused
single-package comparison shows that Rust `-v` prints project-context, package
discovery, per-module parse, and per-module interface/implementation compile
events, while OCaml currently prints only its project-root context. Rust `-vv`
also prints the compiled/dirty scheduler-universe count trace. These events need
semantic comparison without imposing Rayon completion order on OCaml. Live
spinner frames and verbosity are deliberately grouped into one final
output-parity pass after native macOS and Windows implementation and
verification, so platform-specific terminal findings can inform that pass.

Interactive checks should run both implementations under a pseudo-terminal and
capture normalized frames/events rather than snapshotting spinner timing byte
for byte. Plain-output snapshots remain exact where paths and ANSI sequences
can be normalized deterministically.
