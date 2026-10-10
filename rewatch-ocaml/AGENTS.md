# OCaml rewatch agent instructions

Read these alongside the [root instructions](../AGENTS.md) and the
[OCaml rewatch guide](README.md). Paths and commands are relative to the
repository root.

## Compatibility with Rust rewatch

- Both implementations ship and run the same `rewatch/tests` suite. A change in
  observable behavior (output, exit status, generated files, configuration
  handling) must either be made in [`rewatch`](../rewatch) too, or be added to
  the intentional differences in the README and covered by a test.
- Don't edit the shared tests or their snapshots to make this implementation
  pass. Fix the implementation, or document the difference.
- When master changes Rust rewatch, check whether the same change is needed
  here.

## Platforms

- Every change must work on Windows as well as Unix. Put OS-specific code
  behind `platform.mli` and implement it in both `platform_unix.ml` and
  `platform_windows.ml`. Don't add Unix-only calls (signals, process groups,
  `select` on pipes, symlink assumptions) to shared modules.
- The Windows backend is type-checked on every platform through
  `tests/rewatch_ounit_tests`, but its process code only runs on Windows CI.

## Processes and state

- Every child process, pipe, thread, lock, and temporary file needs an owner
  that releases it on success, failure, and interruption. Use the existing
  owners in `process_child.ml`, `build_lock.ml`, and `build_attempt.ml`.
- Signal handlers only record the request. Work stops at the next poll point.
- Keep state that lives across watch rebuilds in `build_session.ml` and state
  for one attempt in `build_attempt.ml`.

## Testing

- Unit tests: `dune runtest tests/rewatch_ounit_tests`.
- Focused end-to-end tests:
  `sh rewatch-ocaml/tests/run.sh "$PWD/packages/@rescript/<platform>/bin/rescript.exe"`.
- Shared suite: `make test-rewatch`. Run it for any change to build behavior.
- Add a test for every bug fix. Prefer the shared suite when Rust behaves the
  same. Poll for observable conditions instead of sleeping.
