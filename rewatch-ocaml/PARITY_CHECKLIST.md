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
