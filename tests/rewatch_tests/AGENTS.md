# Rewatch Tests

This directory contains integration tests for the `rewatch` build system using vitest and OpenTelemetry-based span collection.

## Overview

These tests verify rewatch's behavior by:
1. Creating isolated sandbox copies of a fixture monorepo
2. Running rewatch commands with OTEL tracing enabled
3. Collecting and asserting on the emitted spans using snapshot testing
4. Cleaning up sandboxes after each test

## Running Tests

```bash
# Install dependencies (from project root)
yarn install

# Build the compiler and runtime (required before running tests)
make lib

# Run all rewatch tests
cd tests/rewatch_tests
yarn test

# Run tests in watch mode
yarn test:watch

# Update snapshots
yarn test:update

# Run a single test file
yarn test tests/build.test.mjs

# Run a specific test by name
yarn test -t "builds all packages from root"

# Debug with OTEL output
DEBUG_OTEL=1 yarn test
```

## Testing with a Published Package (pkg.pr.new)

By default, tests use the locally built compiler and rewatch binaries. You can also test against a published `pkg.pr.new` package by installing it into the fixture:

```bash
# Install a specific PR build into the fixture
cd tests/rewatch_tests/fixture
yarn add "rescript@https://pkg.pr.new/rescript-lang/rescript@<commit-sha>"

# Run tests — bins.mjs auto-detects the installed package
cd ..
yarn test

# Clean up when done
cd fixture
yarn remove rescript
```

`helpers/bins.mjs` checks if `fixture/node_modules/@rescript/<platform>/bin/` exists. If it does, all binaries (`rescript.exe`, `bsc.exe`) and the runtime are resolved from the installed package. Otherwise it falls back to the local dev build.

Environment variables always take precedence over auto-detection:
- `REWATCH_EXECUTABLE` — path to the `rescript` (rewatch) binary
- `RESCRIPT_BSC_EXE` — path to the `bsc` compiler binary
- `RESCRIPT_RUNTIME` — path to the `@rescript/runtime` directory

In CI, the `test-integration-rewatch` job installs the PR's package into the fixture via `yarn add` before running the tests. No env vars are needed.

## Directory Structure

```
tests/rewatch_tests/
├── AGENTS.md              # This file
├── package.json           # Test dependencies (vitest, protobufjs)
├── vitest.config.mjs      # Vitest configuration
├── globalSetup.mjs        # Logs resolved binary paths once before all tests
│
├── fixture/               # Monorepo fixture (copied per test)
│   ├── package.json       # Workspace root
│   ├── rescript.json      # Root ReScript config
│   ├── .yarnrc.yml        # Yarn config
│   ├── .yarn/releases/    # Bundled Yarn binary
│   ├── src/Root.res       # Root source file
│   └── packages/
│       ├── app/           # App package (depends on library)
│       │   ├── package.json
│       │   ├── rescript.json
│       │   └── src/App.res
│       └── library/       # Library package (no dependencies)
│           ├── package.json
│           ├── rescript.json
│           └── src/Library.res
│
├── helpers/               # Test infrastructure
│   ├── bins.mjs           # Binary path resolution (rescript, bsc.exe)
│   ├── sandbox.mjs        # Sandbox creation/cleanup
│   ├── process.mjs        # CLI wrapper for running rewatch commands
│   ├── otel-receiver.mjs  # OTLP HTTP receiver for collecting spans
│   └── test-context.mjs   # Main test harness
│
└── tests/                 # Test files
    ├── build.test.mjs
    ├── clean.test.mjs
    ├── format.test.mjs
    ├── compiler-args.test.mjs
    ├── watch.test.mjs
    └── __snapshots__/     # Auto-generated vitest snapshots
```

## How Tests Work

### Test Flow

Each test uses `runRewatchTest()` which:

1. **Setup Phase**
   - Starts an OTEL HTTP receiver on a random port
   - Creates a sandbox by copying `fixture/` to a temp directory
   - Runs `yarn install` in the sandbox to set up workspace symlinks
   - Creates a CLI helper configured with the OTEL endpoint

2. **Test Phase**
   - Your test code runs commands via `ctx.cli.build()`, `ctx.cli.clean()`, etc.
   - Each command sets `OTEL_EXPORTER_OTLP_ENDPOINT` to send traces to the receiver
   - Tests can also manipulate files via `ctx.writeFile()` and `ctx.deleteFile()`

3. **Assertion Phase**
   - Waits 500ms for spans to be exported (batch exporter delay)
   - Builds a span tree from collected traces
   - Generates a summary of relevant spans with their attributes
   - Compares against the snapshot

4. **Cleanup Phase**
   - Removes the sandbox directory
   - Stops the OTEL receiver

### Writing Tests

```javascript
import { describe, it } from "vitest";
import { runRewatchTest } from "../helpers/test-context.mjs";

describe("my-feature", () => {
  it("does something", () =>
    runRewatchTest(async ({ cli, sandbox, writeFile, deleteFile, fileExists }) => {
      // Run a build
      await cli.build();

      // Modify a file
      await writeFile("src/Root.res", 'let x = 1\n');

      // Run another command
      await cli.clean();
    }));
});
```

### Available CLI Methods

- `cli.build(args?)` - Run `rescript build`
- `cli.clean(args?)` - Run `rescript clean`
- `cli.format(args?)` - Run `rescript format`
- `cli.formatStdin(ext, input)` - Run `rescript format --stdin <ext>` with input
- `cli.compilerArgs(filePath)` - Run `rescript compiler-args <file>`
- `cli.spawnWatch(args?)` - Spawn `rescript watch` (returns handle with `stop()` and `waitForOutput()`)

### Span Summary

The snapshot captures spans matching these names:
- `rewatch.build`
- `rewatch.clean`
- `rewatch.watch`
- `rewatch.format`
- `rewatch.compiler_args`

Each span includes relevant attributes (working_dir, check, is_stdin, etc.) and paths are normalized to be sandbox-relative.

## Known Limitations

### Span Collection for Commands Using `std::process::exit()`

Some commands (build, compiler-args) call `std::process::exit()` which doesn't run destructors. This means the telemetry guard's `Drop` implementation doesn't get called, and spans may not be flushed. Commands that return normally (clean, format) work correctly.

To fully capture spans for all commands, the rewatch code would need to be modified to avoid `std::process::exit()` and return from main properly.

## Adding New Tests

1. Create a new test file in `tests/` following the naming convention `*.test.mjs`
2. Use `runRewatchTest()` to get an isolated sandbox and CLI helper
3. Run your scenario and make assertions
4. Run `yarn test:update` to generate the initial snapshot
5. Review the snapshot to ensure it captures the expected behavior

## Adding More Telemetry

To add new spans or attributes in rewatch:

1. Add the `tracing` crate instrumentation in the relevant Rust code:
   ```rust
   use tracing::instrument;

   #[instrument(name = "rewatch.my_operation", skip_all, fields(some_attr = %value))]
   fn my_operation() {
       // ...
   }
   ```

2. Update `SUMMARY_SPAN_NAMES` and `SUMMARY_ATTRS` in `helpers/test-context.mjs` to include the new span

3. Run tests to capture the new spans in snapshots

## Important Rules

- **Never add arbitrary sleeps/timeouts to fix flaky tests.** If a test is flaky, find the root cause (e.g., missing synchronization, race condition in the code under test) and fix it properly. Adding `setTimeout` delays masks the real problem and makes tests unreliable.

## Debugging

- Set `DEBUG_OTEL=1` to print span summaries during test execution
- Check `result.stdout` and `result.stderr` for command output
- For watch tests, use `watch.waitForOutput(pattern, timeout)` to wait for specific output
- If tests timeout, increase the timeout in `vitest.config.mjs`

## Prerequisites

- Node.js 20+
- The rewatch binary must be built: `cargo build --manifest-path rewatch/Cargo.toml`
- The ReScript compiler and runtime must be built: `make lib`
