# Build system agent instructions

Read these alongside the [root instructions](../AGENTS.md) and the
[build system guide](README.md). Paths and commands are relative to the
repository root.

## Observable behavior

- A change in observable behavior (output, exit status, generated files,
  configuration handling) needs a test, and a changelog fragment when users
  can notice it.
- Only update the snapshots in `tests/build_tests/integration` and `tests/build_tests/isolated` for an
  intended output change, never just to make a failing test pass.

## Platforms

- Every change must work on Windows as well as Unix. Put OS-specific code
  behind `platform.mli` and implement it in both `platform_unix.ml` and
  `platform_windows.ml`. Don't add Unix-only calls (signals, process groups,
  `select` on pipes, symlink assumptions) to shared modules.
- The Windows backend is type-checked on every platform through
  `tests/build_tests/unit`, but its process code only runs on Windows CI.

## Processes and state

- Every child process, pipe, thread, lock, and temporary file needs an owner
  that releases it on success, failure, and interruption. Use the existing
  owners in `process_child.ml`, `build_lock.ml`, and `build_attempt.ml`.
- Signal handlers only record the request. Work stops at the next poll point.
- Keep state that lives across watch rebuilds in `build_session.ml` and state
  for one attempt in `build_attempt.ml`.

## Testing

- All build system tests: `make test-build`. Run it for any change to build
  behavior. Single suites: `make -C tests/build_tests unit`, `isolated`,
  `snapshots`, `verbose`, `interactive` or `integration`.
- Add a test for every bug fix. Prefer the integration suite in
  `tests/build_tests/integration`. Poll for observable conditions instead of sleeping.
