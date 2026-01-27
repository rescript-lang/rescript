# Daemon Integration Tests

Tests for the rewatch daemon's persistent behavior. Each test starts a daemon, runs operations, and uses snapshot testing to verify the OpenTelemetry span tree structure.

## Architecture

```
Test Flow:
  1. Copy fixture/ to a unique temp directory (sandbox)
  2. Start OTLP receiver to collect spans
  3. Start daemon process pointing at sandbox
  4. Run operations (Build, Clean, Format, Watch via gRPC)
  5. Stop daemon (flushes spans)
  6. Convert spans to tree summary
  7. Assert summary matches snapshot
  8. Remove sandbox
```

Each test runs in its own sandbox (temp directory copy of `fixture/`), so daemons never share sockets and tests can run in parallel.

## Running Tests

```bash
# From this directory
npx vitest run

# Single file
npx vitest run build.test.mjs

# Single test by name
npx vitest run -t "builds all packages from root"

# Watch mode (re-runs on file change)
npx vitest

# With verbose output
npx vitest run --reporter=verbose

# Update snapshots after intentional changes
npx vitest run --update

# From project root
make test-daemon
```

## File Structure

```
tests/daemon_tests/
├── README.md                   # This file
├── vitest.config.mjs           # Test config (30s timeout)
├── package.json                # Workspace package with vitest + grpc deps
├── fixture/                    # Monorepo copied per test (never modified in place)
│   ├── package.json            # workspaces: ["packages/*"]
│   ├── rescript.json           # Root config, depends on app + lib
│   ├── src/Root.res
│   └── packages/
│       ├── app/                # Depends on lib
│       └── lib/                # No dependencies
├── helpers/
│   ├── sandbox.mjs             # createSandbox(), removeSandbox()
│   ├── daemon.mjs              # startDaemon(), stopDaemon()
│   ├── grpc-client.mjs         # createClient(), createDebugClient(), collectStream()
│   ├── test-context.mjs     # runDaemonTest() - main test harness
│   ├── otel-receiver.mjs       # OTLP HTTP receiver for collecting spans
│   ├── assertions.mjs          # countSourceFiles(), countModules(), getAllPackages()
│   ├── process.mjs             # createRescriptCli() with build(), clean(), format(), spawnWatch()
│   └── wait.mjs                # waitForProcessExit(), collectProcessOutput()
└── tests/
    ├── build.test.mjs              # Build scenarios
    ├── lifecycle.test.mjs          # GetClients, daemon exit on disconnect
    ├── watch.test.mjs              # Watch mode + signal handling
    ├── scope-expansion.test.mjs    # Scoped builds and lazy package loading
    └── *.test.mjs.snap             # Snapshot files (auto-generated)
```

## Key Helpers

### `helpers/test-context.mjs`

The main test harness. Use `runDaemonTest()` for all daemon tests:

```javascript
import { runDaemonTest } from "../helpers/test-context.mjs";

it("my test", () =>
  runDaemonTest(async ({ sandbox, build, clean, watch, writeFile, deleteFile }) => {
    await build(sandbox);
    // Spans are automatically collected and snapshot-tested
  }));
```

The context provides:
- `sandbox` — Path to the test sandbox
- `build(workingDir)` — Execute a build via gRPC
- `clean(workingDir)` — Execute a clean via gRPC
- `watch(workingDir)` — Start watch mode, returns handle with `waitForBuild()`, `waitForSettle()`, `stop()`
- `writeFile(path, content)` — Write a file in the sandbox
- `deleteFile(path)` — Delete a file in the sandbox
- `fileExists(path)` — Check if a file exists

### `helpers/sandbox.mjs`

- `createSandbox()` — copies `fixture/` to a unique temp dir, returns path
- `removeSandbox(dir)` — removes the temp dir

### `helpers/daemon.mjs`

- `startDaemon(projectRoot, options)` — spawns `rescript daemon`, waits for Ping, returns `{ process, socketPath, root }`
- `stopDaemon(daemon)` — sends Shutdown RPC, then SIGKILL fallback

### `helpers/grpc-client.mjs`

- `createClient(socketPath)` — raw gRPC client for the daemon service
- `createDebugClient(socketPath)` — connects as debug client, returns `{ events, stream, client, close() }`
- `collectStream(stream)` — collects all events from a streaming RPC until it ends

### `helpers/process.mjs`

- `createRescriptCli(cwd)` — provides `build()`, `clean()`, `format()`, `formatStdin()`, `spawnWatch()` methods
- `spawnWatch()` returns the raw `ChildProcess` so tests can send signals (SIGINT, SIGTERM)

### `helpers/assertions.mjs`

- `countSourceFiles()` — count .res files in the fixture
- `countModules()` — count modules (source files + namespace modules)
- `getAllPackages()` — get all package names from fixture config

## Adding a New Test

1. Create a new `*.test.mjs` file (or add to an existing one)
2. Use the `runDaemonTest()` pattern:

```javascript
import { describe, it } from "vitest";
import { runDaemonTest } from "../helpers/test-context.mjs";

describe("my-feature", () => {
  it("does something", () =>
    runDaemonTest(async ({ sandbox, build }) => {
      await build(sandbox);
      // Spans are automatically collected and compared to snapshot
    }));
});
```

3. Run the test once to generate the snapshot:

```bash
npx vitest run -t "does something" --update
```

4. Review the generated `.snap` file to ensure it captures the expected behavior.

## Snapshot Format

Snapshots contain a tree summary of OpenTelemetry spans. Each line is a span name with optional attributes:

```
rpc.build[working_dir=.]
  work_queue.handle_build
    build.incremental_build[module_count=5]
      build.load_package_sources[package=@daemon-test/app]
      build.load_package_sources[package=@daemon-test/library]
      build.load_package_sources[package=daemon-test]
      build.parse[dirty_modules=5]
      build.compile
```

Only selected span names and attributes are included (configured in `SUMMARY_SPAN_NAMES` and `SUMMARY_ATTRS`). Paths are normalized to sandbox-relative paths for cross-platform stability.

## Modifying the Fixture

The `fixture/` directory is a minimal monorepo with 3 packages (root, app, lib). To add test scenarios:

- **New package**: Add to `fixture/packages/<name>/` with `package.json`, `rescript.json`, and `src/`
- **New dependency**: Update the relevant `rescript.json` dependencies array and `package.json` dependencies
- **New source file**: Add `.res` files to the appropriate `src/` directory
- **Remember**: The fixture is copied fresh per test, so changes here affect all tests

If a test needs a fixture variation (e.g., a broken file for error testing), modify files in the sandbox within the test itself using `writeFile()` or `deleteFile()`.

### Updating the lockfile

After adding or removing packages in the fixture, you must update `yarn.lock`:

```bash
cd tests/daemon_tests/fixture
node .yarn/releases/yarn-4.12.0.cjs install
```

If the lockfile is out of date, tests will fail with `YN0028: The lockfile would have been modified by this install, which is explicitly forbidden.`

## Debugging Daemon Tests

The daemon uses OpenTelemetry for observability. There are several ways to inspect traces depending on your needs.

### Quick Inspection: DEBUG_OTEL

Set `DEBUG_OTEL=1` to print the span summary to the console during tests:

```bash
# Run a specific test with span output
DEBUG_OTEL=1 npx vitest run -t "builds all packages from root"
```

This prints the same tree summary that gets compared against the snapshot, useful for debugging test failures.

### Standalone OTLP Collector

For development outside of tests (e.g., debugging the daemon directly), use the standalone OTLP file collector:

```bash
# Start the collector (listens on port 4318 by default)
node tests/daemon_tests/helpers/otel-receiver.mjs --output traces.json

# In another terminal, run builds with OTEL enabled
cd your-project
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 rescript build

# ... do some builds ...

# Press Ctrl+C in the collector terminal to write spans to traces.json
```

Options:
- `--output, -o <file>` - Output file path (writes on exit)
- `--port, -p <port>` - Port to listen on (default: 4318)
- `--help, -h` - Show help message

### Jaeger UI (Deep Analysis)

For interactive trace visualization, use Jaeger:

```bash
# Start Jaeger locally (exposes OTLP HTTP on port 4318)
docker run -d -p 16686:16686 -p 4317:4317 -p 4318:4318 jaegertracing/all-in-one

# Run tests with otel export
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 npx vitest run -t "your test name"

# Open http://localhost:16686 to view traces in Jaeger UI
```

Traces show the full operation hierarchy:

```
rpc.build
└─ work_queue.handle_build
    ├─ build.load_package_sources [package=@daemon-test/library]
    ├─ build.parse [dirty_count=5]
    └─ build.compile [compiled_count=5]
```

### When to Use Which

| Situation                                 | Approach              |
| ----------------------------------------- | --------------------- |
| Quick check of spans during a test        | DEBUG_OTEL            |
| Debugging test assertions                 | DEBUG_OTEL            |
| No Docker/infrastructure available        | DEBUG_OTEL or standalone collector |
| Debugging daemon outside of tests         | Standalone collector  |
| Why is this build slow?                   | Jaeger UI             |
| Analyzing timing across multiple builds   | Jaeger UI             |
| Interactive trace exploration             | Jaeger UI             |

## Troubleshooting

### Stale daemon processes

```bash
pkill -f "rescript daemon" 2>/dev/null
pkill -f "rescript.exe daemon" 2>/dev/null
```

### Socket file left behind

Socket files now live in the system temp directory (`/tmp` on Unix) with hash-based names.
Each sandbox gets a unique socket path, so collisions should not happen. If testing manually:

```bash
rm -f lib/bs/rescript.pid lib/bs/rescript.sock.path
rm -f /tmp/rescript-*.sock
```

### Timeout failures

- The daemon has 5s to start (poll for socket + Ping). If this fails consistently, check that `rescript_exe` resolves correctly via `#cli/bins`.
- Test timeout is 30s (configured in `vitest.config.mjs`). Increase if builds are slow.

### Snapshot failures

When snapshots fail, the diff shows what changed. Common causes:
- New spans added to the daemon (update `SUMMARY_SPAN_NAMES` in test-context.mjs)
- Attribute values changed (check if the change is expected)
- Build order changed (parallel spans are sorted alphabetically, but dependency order matters)

To update snapshots after intentional changes:

```bash
npx vitest run --update
```

### Compiler not found

The daemon needs `RESCRIPT_BSC_EXE` and `RESCRIPT_RUNTIME` environment variables (or the platform binary package installed). These are inherited from the parent process environment via `#cli/bins`.

### Proto changes

The proto file is loaded dynamically at runtime (`@grpc/proto-loader`). No codegen step needed. Just update `rewatch/proto/rescript.proto` and the tests will pick up changes automatically.

### Running a build in the fixture manually

To troubleshoot fixture issues, you can run a build directly in the `fixture/` directory using the local rewatch binary. You need to set the compiler and runtime paths:

```bash
cd tests/daemon_tests/fixture
yarn install

export RESCRIPT_BSC_EXE=$(realpath ../../../_build/default/compiler/bsc/rescript_compiler_main.exe)
export RESCRIPT_RUNTIME=$(realpath ../../../packages/@rescript/runtime)

../../../rewatch/target/debug/rescript build
```

### Watch tests hang

Watch tests use `watch()` which internally spawns a watch process. The test framework automatically cleans up watch handles. If a test hangs, check that the watch process didn't die unexpectedly.

## Design Constraints

- **Snapshot-based assertions**: Tests use OpenTelemetry spans for assertions, not gRPC debug events. This provides a consistent, hierarchical view of daemon operations.
- **Daemon exits on disconnect**: The daemon shuts down immediately when all clients disconnect. Each test manages its own daemon lifecycle.
- **Sandbox isolation**: Each test gets a fresh temp directory copy. Daemons never interfere because socket paths are unique per sandbox.
- **Dynamic proto loading**: The proto schema is in flux. No generated stubs to maintain.
- **Deterministic output**: Parallel spans are sorted alphabetically, and paths are normalized to ensure consistent snapshots across platforms.
