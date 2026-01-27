# Rewatch Daemon Architecture

Daemon-based architecture for rewatch. All build operations flow through a persistent daemon that owns build state.

**Writing style**: Sacrifice grammar for concision. Bullet points over prose. No fluff.

## Goal

Eliminate lockfile limitation via gRPC daemon that owns build state. Commands (`build`, `clean`, `watch`, `format`) are thin clients.

**Success**: `rescript watch` in one terminal + `rescript build`/`clean`/`format` in another - no lockfile conflicts.

## Current Status

✅ **All tests passing** (`make test-rewatch`)

### Completed Features

- Build, clean, watch, format commands work through daemon
- Scoped builds: run from child package → only builds that package + deps
- Scoped format: run from child package → only formats that package
- Scoped clean: run from child package → cleans that package + external deps (not sibling local packages)
- Debug TUI for monitoring daemon events
- OpenTelemetry tracing for detailed debugging (via `OTEL_EXPORTER_OTLP_ENDPOINT`)
- `compiler-args` command (daemon-aware with standalone fallback)
- Lazy package loading: sources loaded on-demand based on scope
- Build state reuse across requests
- Watch mode: mtime-based deduplication filters phantom FSEvents and handles atomic writes
- Watch mode: config-aware file watching (daemon tells watch client what to watch via `WatchPaths` event)
- Config change detection: watch client detects `rescript.json` changes and triggers full rebuild via `NotifyConfigChange` RPC
- `--filter` flag: per-build module regex passed via proto, applied at compile time only (source graph stays complete for reuse)
- Signal handling: daemon handles SIGTERM/SIGINT gracefully, ignores SIGHUP (survives terminal close)
- Stale daemon detection: client checks PID file validity before connecting, cleans up stale files automatically

### Key Scoping Behavior

| Command  | From Root          | From Child Package                                   |
| -------- | ------------------ | ---------------------------------------------------- |
| `build`  | All local packages | Package + its dependencies                           |
| `format` | All local packages | Only that package                                    |
| `clean`  | All packages       | Package + external deps (not sibling local packages) |

## Architecture Overview

```
┌──────────────────────────────────────────────────────────────────┐
│                    Rewatch Daemon                                │
│                  (gRPC Server on Unix Socket)                    │
│                                                                  │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  BuildState (Mutex, locked only by work loop)            │   │
│  │  - modules, packages, dependencies                       │   │
│  │  - compiler_info                                         │   │
│  │  - project_context                                       │   │
│  └──────────────────────────────────────────────────────────┘   │
│                          ▲                                       │
│                          │ lock                                  │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  Work Queue (mpsc channel → sequential work loop)        │   │
│  │  - Serializes all state-mutating operations              │   │
│  │  - Build, Clean, WatchInitialBuild, FileChangeBuild,     │   │
│  │    ConfigChange, FormatCollectFiles                       │   │
│  │  - Precondition: ensure_state_ready() before each item   │   │
│  │  - Completion via oneshot channels                       │   │
│  └──────────────────────────────────────────────────────────┘   │
│                          ▲                                       │
│                          │ submit WorkItem                       │
│  ┌──────────────────────────────────────────────────────────┐   │
│  │  RPC Handlers (service.rs)                               │   │
│  │  - Register client, subscribe to events                  │   │
│  │  - Submit WorkItem, stream events until oneshot fires    │   │
│  └──────────────────────────────────────────────────────────┘   │
│                                                                  │
│  ┌──────────────────┐  ┌────────────────────────────────────┐   │
│  │  Client Manager      │  │  File Change Processor         │   │
│  │  - broadcast channel │  │  - 50ms debounce               │   │
│  │  - client_id filter  │  │  - Submits FileChangeBuild     │   │
│  └──────────────────────┘  └────────────────────────────────┘   │
└──────────────────────────────────────────────────────────────────┘
        ▲                ▲                    ▲
        │                │                    │
┌───────┴───────┐ ┌──────┴──────┐ ┌──────────┴──────────┐
│  Watch Client │ │ Build Client│ │     Debug Client    │
│               │ │             │ │                     │
│ - File watch  │ │ - Single    │ │ - Ratatui TUI       │
│   (notify-rs) │ │   request   │ │ - Two-column layout │
│ - Send events │ │ - Wait/exit │ │ - Live event stream │
│ - Stream out  │ │             │ │ - Connected clients │
└───────────────┘ └─────────────┘ └─────────────────────┘
        ▲                ▲
        │                │
┌───────┴───────┐ ┌──────┴──────┐
│ Format Client │ │ Clean Client│
│               │ │             │
│ - stdin/files │ │ - Single    │
│ - check mode  │ │   request   │
│ - Stream out  │ │ - Wait/exit │
└───────────────┘ └─────────────┘
```

## Key Design Decisions

| Decision               | Choice                                 | Rationale                                                      |
| ---------------------- | -------------------------------------- | -------------------------------------------------------------- |
| Communication protocol | gRPC with tonic                        | Type-safe, streaming support, structured progress messages     |
| Daemon lifecycle       | Auto-start on first command            | Transparent to users                                           |
| Daemon scope           | One per monorepo root                  | Daemon knows full project context                              |
| State ownership        | Daemon owns BuildState                 | Single source of truth                                         |
| Concurrency            | Serialized work queue (mpsc + oneshot) | No races between build/clean/file-change, mutex never poisoned |
| File watching          | Client responsibility                  | Watch client runs notify-rs, sends events to daemon            |
| Progress reporting     | Unified `DaemonEvent` stream           | Single event type for all daemon-to-client communication       |
| Reporter pattern       | Always required                        | No fallback stdout printing, all output flows through RPC      |
| Socket location        | `<tmpdir>/rescript-<hash>.sock`        | Short path avoids Unix socket length limit (104 bytes macOS)   |
| Event architecture     | Single broadcast channel               | All events go to broadcast, server filters by client_id        |
| Client isolation       | client_id on all events                | Concurrent builds don't interfere, each sees only its own      |
| Per-build CLI flags    | Passed to build, not stored            | Flags like `--warn-error` apply per-build without persisting   |
| Scoping via path       | `get_scope_package_from_working_dir()` | Working dir determines scope, passed as parameter to commands  |
| Connection resilience  | HTTP/2 keepalive + heartbeat + retry   | Daemon detects dead clients, watch client auto-reconnects      |

## How It Works

### Scoping Mechanism

`ProjectContext` is always created at monorepo root level. Scoping is determined by comparing `working_dir` to root:

- `helpers::get_scope_package_from_working_dir(working_dir, root)` returns `Option<String>`
- If `working_dir == root` → `None` (build all packages)
- If `working_dir` is a child package → `Some(package_name)` (scoped build)

### Event System

Unified `DaemonEvent` proto with client_id filtering:

- `proto/rescript.proto` - message definitions
- `daemon/clients.rs` - `ClientManager` with broadcast channel
- Every event has `client_id` for isolation
- Server filters per client; debug client receives ALL events

### Daemon Service

`daemon/service.rs` - `DaemonService` struct, `impl RescriptDaemon`

RPC handlers are thin: register client, subscribe to broadcast, submit `WorkItem` to work queue, stream events until oneshot completion fires.

- `build()` - submits `WorkItem::Build`, streams events, yields final `BuildFinished`
- `clean()` - submits `WorkItem::Clean`, streams events, yields final `BuildFinished`
- `watch()` - submits `WorkItem::WatchInitialBuild`, then streams indefinitely
- `format()` - stdin: direct `spawn_blocking`; files: submits `WorkItem::FormatCollectFiles` then formats outside queue
- `notify_config_change()` - submits `WorkItem::ConfigChange` + triggers rebuild via file change channel
- `get_compiler_args()` - read-only, locks mutex directly (bypasses queue)
- `GetClients`, `Ping`, `Shutdown`, `Debug` - no state mutation, no queue

### Build State

- Single `BuildState` owned by daemon via `std::sync::Mutex`
- **Only the work loop locks the mutex** — RPC handlers never lock it directly (except read-only `get_compiler_args`)
- **Daemon startup** (`DaemonService::new`): Creates `ProjectContext` and `CompilerInfo`. These are available immediately.
- **Lazy package/source loading**: Package discovery and source file loading are deferred until the first work item that needs them (via `ensure_state_ready()` precondition in the work loop)
- Persists for daemon lifetime
- Clean invalidates in-memory state (`clear_sources_and_modules`) after removing disk artifacts

### Work Queue (`work_queue.rs`)

All state-mutating operations go through a single `mpsc` channel consumed by one work loop. This guarantees no two operations run concurrently on `BuildState`.

**Types:**

- `WorkItem` enum: `Build`, `Clean`, `WatchInitialBuild`, `FileChangeBuild`, `ConfigChange`, `FormatCollectFiles`
- `WorkResult` enum: `Success { duration_seconds, module_count }` | `Failure { duration_seconds, error }`
- Completion signaled via `oneshot::Sender<WorkResult>`

**Precondition pattern:**

Every work item that touches `BuildState` goes through a two-phase execution:

1. **Precondition** — `ensure_state_ready(build_state, scope, reporter)`: calls `initialize_packages()` then `ensure_packages_loaded(scope)`
2. **Operation** — the actual work (build, clean, format file collection, etc.)

**Read-only operations bypass the queue:** `get_compiler_args`, `ping`, `flush`, `get_clients`.

### Watch Client

The watch client (`client/watch.rs`) uses a **daemon-driven** watching strategy:

1. Watch client sends `WatchRequest` to daemon
2. Daemon does initial build, emits `WatchPaths` event with source dirs + config paths
3. Watch client receives `WatchPaths`, sets up notify-rs watchers
4. On `.res`/`.resi` change → `NotifyFileChange` RPC
5. On `rescript.json` change → `NotifyConfigChange` RPC
6. After rebuild, daemon emits updated `WatchPaths` (source dirs may have changed)

**Signal handling:** Watch client uses `tokio::signal::unix` to handle SIGTERM/SIGINT. On signal, sends `DisconnectRequest` to daemon before exiting cleanly.

**Reconnection:** On gRPC stream errors, the watch client automatically reconnects:

- `run()` wraps each session in a retry loop with exponential backoff (1s → 2s → 4s → ... → 30s cap)
- `run_watch_session()` returns `SessionExit::Clean` (signal, no retry) or `SessionExit::StreamError` (retry)
- On reconnect, `connect_or_start()` handles starting a new daemon if the old one died
- File watchers are recreated fresh via `WatchPaths` event each session — no state carryover needed
- Logs the last event received before the error for diagnostics

**Heartbeat:** Daemon sends `Heartbeat` event every 30s on the watch stream. This ensures broken connections are detected even during idle periods (no build activity). The heartbeat write failing on the server side triggers tonic to drop the stream, firing `scopeguard` cleanup.

**CompileType enum:** `Incremental`, `SourceCreated`, `SourceDeleted`, `SourceRenamed`, `ConfigChange`

### Daemon Lifecycle

- `daemon.rs` - `pub async fn start()` - entry point
- `shutdown_monitor()` - monitors both signals and client count:
  - SIGTERM/SIGINT → graceful shutdown
  - SIGHUP → ignored (daemon survives terminal close)
  - All clients disconnected → shutdown
- HTTP/2 keepalive: `http2_keepalive_interval(10s)` + `http2_keepalive_timeout(5s)` on tonic server — detects dead clients within ~15s even if no application traffic
- Writes PID to `<root>/lib/bs/rescript.daemon.pid`
- Writes socket path to `<root>/lib/bs/rescript.sock.path`
- Cleans up socket, PID file, and socket path file on exit

### Stale Daemon Detection

Client connection (`client/connection.rs`) handles ghost daemons:

- If socket doesn't exist but PID file exists → check if process is alive via `sysinfo`
- If process is dead → clean up stale files (PID, socket path file, socket)
- If socket exists → try to ping daemon, clean up if unresponsive
- Avoids race conditions by only checking PID when socket is missing

### File Structure

```
rewatch/
├── proto/
│   └── rescript.proto      # gRPC service and unified DaemonEvent definition
├── src/
│   ├── main.rs             # CLI routing to client module
│   ├── cli.rs              # Command definitions
│   ├── daemon.rs           # Daemon module + entry point, socket/PID management, shutdown monitor
│   ├── daemon/
│   │   ├── clients.rs      # ClientManager with single broadcast channel
│   │   ├── reporters.rs    # BroadcastReporter (with client_id), progress_to_event
│   │   ├── state.rs        # DaemonState with Mutex<BuildState>, work_tx, CompileType enum
│   │   ├── service.rs      # DaemonService RPC implementations, setup_client()
│   │   ├── work_queue.rs   # WorkItem/WorkResult enums, run_work_loop(), handler functions
│   │   ├── file_change_handler.rs  # 50ms debounce, submits WorkItem::FileChangeBuild
│   │   └── telemetry.rs    # OpenTelemetry initialization (opt-in via OTEL_EXPORTER_OTLP_ENDPOINT)
│   ├── client.rs           # Client module exports
│   ├── client/
│   │   ├── connection.rs   # find_project_root, connect, start_daemon_if_needed
│   │   ├── output.rs       # render_event() for daemon event rendering
│   │   ├── build.rs        # Build command client
│   │   ├── clean.rs        # Clean command client
│   │   ├── watch.rs        # Watch command client (file watcher + daemon)
│   │   ├── format.rs       # Format command client (stdin, files, check modes)
│   │   ├── compiler_args.rs # Compiler args client (daemon-aware with fallback)
│   │   └── debug.rs        # Debug command client (Ratatui TUI)
│   ├── project_context.rs  # ProjectContext - always created at root level
│   ├── helpers.rs          # get_scope_package_from_working_dir()
│   ├── build.rs            # Build module, BuildProgress enum, BuildReporter trait
│   └── build/
│       ├── build_types.rs  # BuildState with ensure_packages_loaded(), empty()
│       ├── clean.rs        # clean() with scoping
│       ├── packages.rs     # make(), ensure_sources_loaded(), get_watch_paths()
│       └── ...             # Other build modules
```

## Development Methodology

### Proto Breaking Changes OK

Experiment phase - breaking changes to `proto/rescript.proto` acceptable.

### Always Use Reporter Pattern

All build functions take `&R: BuildReporter`. Progress flows through RPC, client controls rendering.

### Use `tracing::` for Debug Output

In `build/` modules, use the `tracing` crate for debug output:

```rust
// Debug output flows to otel when OTEL_EXPORTER_OTLP_ENDPOINT is set
tracing::debug!(name = %name, "Processing");
tracing::info!(count = dirty_count, "Parsing files");
```

This provides structured, queryable output via OpenTelemetry instead of unstructured console logs.

### Client-Side Rendering

All formatting in `client/output.rs`. Daemon sends data, client renders.

### Run Clippy Often

```bash
cargo clippy --manifest-path rewatch/Cargo.toml --all-targets --all-features
```

### Keep Document Current

Update after each session. Reflect current state, not history.

**Every code change to daemon/client architecture should have a matching update to this document.**

## Remaining Work

### Must Have

1. **Protocol versioning**:
   - Add version field to proto
   - Client refuses to use daemon with incompatible version

2. **Build timeout mechanism**:
   - Prevent hung `bsc` processes from blocking forever
   - Configurable timeout with sensible default (5 min)

### Nice to Have

1. Build cancellation on client disconnect
2. Persistent build state for faster daemon restart

## Known Weaknesses (Summary)

- **Broadcast channel overflow**: Fixed 100-event buffer, slow clients may miss events
- **No build timeout**: Hung `bsc` blocks work queue forever
- **std::sync::Mutex in async**: Works because locking only in `spawn_blocking`, but footgun
- **No protocol versioning**: Client/daemon must be same version
- **Unix-only**: No Windows support (needs named pipes or TCP)
- **Reconnection is watch-only**: Build/clean/format clients don't retry on stream errors (they're short-lived, so restart is acceptable)

## Testing

```bash
# Run Jaeger locally for development (exposes OTLP HTTP on port 4318)
docker run -d -p 16686:16686 -p 4317:4317 -p 4318:4318 jaegertracing/all-in-one

# Set OTEL_EXPORTER_OTLP_ENDPOINT for daemon (uses HTTP, not gRPC)
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 rescript build

# Open http://localhost:16686 to view traces
```

**Graceful degradation:**

If no otel collector is configured, tracing falls back to local logging only. The daemon works the same either way — otel is opt-in for debugging.

**Replacing Debug TUI:**

The debug TUI (`rescript debug`) currently shows a live event stream. With otel:

- Keep the TUI for quick local debugging (no infrastructure needed)
- Use Jaeger/Grafana for detailed trace analysis
- TUI could optionally show trace IDs for each build, linking to Jaeger

**Integration with future clients:**

| Client      | Trace Context                                             |
| ----------- | --------------------------------------------------------- |
| CLI         | Starts new trace per command                              |
| LSP adapter | Propagates from editor (if supported) or starts new trace |
| Vite plugin | Propagates from Vite's request context                    |
| MCP adapter | Propagates from AI agent's context                        |
| DevTools UI | Shows trace ID, links to Jaeger                           |

This makes it possible to answer questions like:

- "The build was slow — was it parsing, compilation, or bsc itself?"
- "The editor hover took 2 seconds — where did time go?"
- "Which module is causing the most recompilation?"

**Unifying Test Infrastructure:**

The current test infrastructure in `tests/daemon_tests/` does manual trace collection:

# Daemon integration tests (vitest + gRPC)

make test-daemon

# Single test file

cd tests/daemon_tests && npx vitest run tests/build.test.mjs

# Single test by name

cd tests/daemon_tests && npx vitest run -t "builds all packages from root"

# Rust quality

cargo clippy --manifest-path rewatch/Cargo.toml --all-targets --all-features
cargo fmt --check --manifest-path rewatch/Cargo.toml

````

### Manual Testing

```bash
# Terminal 1: watch mode
rescript watch

# Terminal 2: concurrent build
rescript build

# Terminal 3: debug TUI
rescript debug

# Clean + rebuild
rescript clean && rescript build

# Format

# Scoped operations (from child package)
cd packages/my-package
rescript build               # only builds my-package + deps
rescript format              # only formats my-package
rescript clean               # cleans my-package + node_modules deps

# Format
rescript format
rescript format --check
echo "let x = 1" | rescript format --stdin .res

# Check for running daemon
cat lib/bs/rescript.daemon.pid    # shows PID if daemon running
cat lib/bs/rescript.sock.path     # shows socket path in /tmp
ls $(cat lib/bs/rescript.sock.path)  # socket exists if daemon alive
pgrep -f "rescript.exe daemon"        # list daemon processes

# Kill stale daemon
kill $(cat lib/bs/rescript.daemon.pid)
rm -f lib/bs/rescript.daemon.pid lib/bs/rescript.sock.path
rm -f $(cat lib/bs/rescript.sock.path 2>/dev/null)  # remove socket from /tmp
````

### Automated

```bash
# Bash integration tests (suite.sh)
make test-rewatch

# Daemon integration tests (vitest + gRPC)
make test-daemon

# Rust quality
cargo clippy --manifest-path rewatch/Cargo.toml --all-targets --all-features
cargo fmt --check --manifest-path rewatch/Cargo.toml
```

### Daemon Integration Tests

Located in `tests/daemon_tests/`. Uses vitest with a gRPC debug client to observe daemon events.

- Each test copies the fixture monorepo to a temp sandbox for isolation
- Starts a daemon, connects a debug client, runs commands, asserts on the event stream
- RPC streams complete when operations finish, ensuring all events are delivered before assertions
- Two test styles:
  - **gRPC-based**: uses `setupTestContext()` to get a debug client + direct RPC calls (build, clean, compiler-args, scope-expansion)
  - **CLI-based**: uses `createProcessHelper()` to spawn the `rescript` binary (format, concurrent-clients, watch-file-changes, build-errors)

```bash
# Run all daemon tests
make test-daemon

# Run single file
cd tests/daemon_tests && npx vitest run tests/build.test.mjs

# Run single test by name
cd tests/daemon_tests && npx vitest run -t "builds all packages from root"
```

### Testing Strategy

#### Fresh Daemon Per Command

The daemon shuts down immediately when the last client disconnects, so each command gets a fresh daemon.

#### Daemon Cleanup in Tests

`suite.sh` kills lingering daemons before tests:

```bash
pkill -f "rescript daemon" 2>/dev/null || true
pkill -f "rescript.exe daemon" 2>/dev/null || true
```

#### Test Files

- `lock.sh`: Removed (daemon handles concurrency)
- `watch.sh`: Kill via PID, not lockfile
- Others: Work unchanged (daemon exits when client disconnects)

#### Snapshots

Must match: message content, errors, warnings.
Can change: whitespace, ANSI artifacts.

#### Output Split

- Daemon: structured `DaemonEvent` data
- Client: emojis, colors, formatting

#### Debug Failures

```bash
# Check socket cleanup
ls -la lib/bs/rescript.sock

# Run isolated
cd rewatch/testrepo
../target/debug/rescript build

# Debug output via otel
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 rescript build
# then view traces in Jaeger at http://localhost:16686

# Kill stale daemon
pkill -f "rescript daemon" || true
rm -f lib/bs/rescript.daemon.pid lib/bs/rescript.sock.path
rm -f /tmp/rescript-*.sock  # clean up sockets from temp dir
```

## Build State Reuse and Expansion

Build state is lazily loaded and reused across requests. State can be **expanded** by subsequent clients:

1. Client A runs `rescript build` from `packages/foo/` → daemon starts, loads only `foo`
2. Client B runs `rescript build` from root → expands state to all packages
3. Both builds complete, daemon now has full state cached

This works because `ensure_packages_loaded()` is additive - it only loads packages not already loaded.

## Design Rules

1. One daemon per project root
2. Unix sockets only (Windows later)
3. Daemon sends data, clients format
4. All state-mutating operations go through the work queue — never lock `BuildState` from RPC handlers
5. `compiler-args`: uses daemon if running, doesn't start one (read-only, bypasses queue)
6. Client isolation via `client_id` filtering
7. Single reporter: `BroadcastReporter` with `client_id`
8. Scoping passed as parameter via `get_scope_package_from_working_dir()`
9. File watching is client responsibility, but daemon directs which paths to watch via `WatchPaths` events
10. Clean invalidates in-memory state (not just disk artifacts)
