# Rewatch agent instructions

Read these alongside the [root instructions](../AGENTS.md) and the
[Rewatch guide](README.md). All paths and commands below are relative to the
repository root unless stated otherwise.

## Build System Flow

1. **Initialization** (`build::initialize_build`)
   - Parse `rescript.json` configuration
   - Discover packages and dependencies
   - Set up compiler information
   - Create initial `BuildState`

2. **AST Generation** (`build::parse`)
   - Generate AST files using `bsc -bs-ast`
   - Handle PPX transformations
   - Process JSX

3. **Dependency Analysis** (`build::deps`)
   - Analyze module dependencies from AST files
   - Build dependency graph
   - Detect circular dependencies

4. **Compilation** (`build::compile`)
   - Generate `bsc` compiler arguments
   - Compile modules in dependency order
   - Handle warnings and errors
   - Generate JavaScript output

5. **Incremental Updates** (`watcher.rs`)
   - Watch for file changes
   - Determine dirty modules
   - Recompile only affected modules

### Development Guidelines

#### Adding New Features

1. **CLI Arguments**: Add to `cli.rs` in `BuildArgs` and `WatchArgs`
2. **Configuration**: Extend `config.rs` for new `rescript.json` fields
3. **Build Logic**: Modify appropriate `build/*.rs` modules
4. **Thread Parameters**: Pass new parameters through the build system chain
5. **Add Tests**: Include unit tests for new functionality

#### Common Patterns

- **Parameter Threading**: New CLI flags need to be passed through:
  - `main.rs` → `build::build()` → `initialize_build()` → `BuildState`
  - `main.rs` → `watcher::start()` → `async_watch()` → `initialize_build()`

- **Configuration Precedence**: Command-line flags override `rescript.json` config
- **Error Handling**: Use `anyhow::Result` for error propagation
- **Logging**: Use `log::debug!` for development debugging

#### Testing

```bash
# Run rewatch tests (from project root)
cargo test --manifest-path rewatch/Cargo.toml

# Test specific functionality
cargo test --manifest-path rewatch/Cargo.toml config::tests::test_get_warning_args

# Run clippy for code quality
cargo clippy --manifest-path rewatch/Cargo.toml --all-targets --all-features

# Check formatting
cargo fmt --check --manifest-path rewatch/Cargo.toml

# Build rewatch
cargo build --manifest-path rewatch/Cargo.toml --release

# Or use the Makefile shortcuts
make rewatch          # Build rewatch
make test-rewatch     # Run integration tests
```

**Note**: The rewatch project is located in the `rewatch/` directory with its own `Cargo.toml` file. All cargo commands should be run from the project root using the `--manifest-path rewatch/Cargo.toml` flag, as shown in the CI workflow.

**Integration Tests**: The `make test-rewatch` command runs bash-based integration tests located in `rewatch/tests/suite.sh`. These tests use the `rewatch/testrepo/` directory as a test workspace with various package configurations to verify rewatch's behavior across different scenarios.

**Running Individual Integration Tests**: You can run individual test scripts directly by setting up the environment manually:

```bash
cd rewatch/tests
export REWATCH_EXECUTABLE="$(realpath ../target/debug/rescript)"
eval $(node ./get_bin_paths.js)
export RESCRIPT_BSC_EXE
export RESCRIPT_RUNTIME
source ./utils.sh
bash ./watch/06-watch-missing-source-folder.sh
```

This is useful for iterating on a specific test without running the full suite.

#### Debugging

- **Build State**: Use `log::debug!` to inspect `BuildState` contents
- **Compiler Args**: Check generated `bsc` arguments in `compile.rs`
- **Dependencies**: Inspect module dependency graph in `deps.rs`
- **File Watching**: Monitor file change events in `watcher.rs`

#### OpenTelemetry Tracing

Rewatch supports OpenTelemetry (OTEL) tracing for build and watch commands. To visualize traces locally, run a Jaeger all-in-one container:

```bash
docker run -d --name jaeger \
  -p 4317:4317 -p 4318:4318 -p 16686:16686 \
  jaegertracing/all-in-one
```

Then run rewatch with the OTLP endpoint set:

```bash
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4318 cargo run --manifest-path rewatch/Cargo.toml -- build
```

Open http://localhost:16686 to view traces in the Jaeger UI.

Note: Use `tracing::debug!` (not `log::debug!`) for events you want to appear in OTEL traces — they use separate logging systems.

##### Honored environment variables

Rewatch follows the OTEL spec for configuration — no rewatch-specific knobs exist.

| Variable | Purpose |
|---|---|
| `OTEL_EXPORTER_OTLP_ENDPOINT` | Base endpoint of the collector (e.g. `http://localhost:4318`). `/v1/traces` is appended for the trace exporter. Setting this (or `OTEL_EXPORTER_OTLP_TRACES_ENDPOINT`) is what enables telemetry — if neither is set, tracing is a no-op. |
| `OTEL_EXPORTER_OTLP_TRACES_ENDPOINT` | Full trace endpoint used verbatim. Overrides the general endpoint for traces. |
| `OTEL_EXPORTER_OTLP_HEADERS` | Extra headers on exporter requests (e.g. `authorization=Bearer xyz`). |
| `OTEL_SERVICE_NAME` | Service name reported on spans. Defaults to `rewatch`. |
| `OTEL_RESOURCE_ATTRIBUTES` | Comma-separated `key=value` pairs added as resource attributes (e.g. `deployment.environment=ci,host.name=$HOSTNAME`). |
| `RUST_LOG` | Controls which span/event levels are captured (e.g. `RUST_LOG=info`, `RUST_LOG=rewatch=debug`). Defaults to `debug` when telemetry is enabled. |

#### Running Rewatch Directly

When running the rewatch binary directly (via `cargo run` or the compiled binary) during development, you need to set environment variables to point to the local compiler and runtime. Otherwise, rewatch will try to use the installed versions:

```bash
# Set the compiler executable path
export RESCRIPT_BSC_EXE=$(realpath _build/default/compiler/bsc/rescript_compiler_main.exe)

# Set the runtime path
export RESCRIPT_RUNTIME=$(realpath packages/@rescript/runtime)

# Now you can run rewatch directly
cargo run --manifest-path rewatch/Cargo.toml -- build
```

Note that the dev binary is `./rewatch/target/debug/rescript`, not `rewatch`. The binary name is `rescript` because that's the package name in `Cargo.toml`.

This is useful when testing rewatch changes against local compiler modifications without running a full `make` build cycle.

Use `-v` for info-level logging or `-vv` for debug-level logging (e.g., to see which folders are being watched in watch mode):

```bash
cargo run --manifest-path rewatch/Cargo.toml -- -vv watch <folder>
```

#### Performance Considerations

- **Incremental Builds**: Only recompile dirty modules
- **Parallel Compilation**: Use `rayon` for parallel processing
- **Memory Usage**: Be mindful of `BuildState` size in large projects
- **File I/O**: Minimize file system operations

#### Performance vs Code Quality Trade-offs

When clippy suggests refactoring that could impact performance, consider the trade-offs:

- **Parameter Structs vs Many Arguments**: While clippy prefers parameter structs for functions with many arguments, sometimes the added complexity isn't worth it. Use `#[allow(clippy::too_many_arguments)]` for functions that legitimately need many parameters and where a struct would add unnecessary complexity.

- **Cloning vs Borrowing**: Sometimes cloning is necessary due to Rust's borrow checker rules. If the clone is:
  - Small and one-time (e.g., `Vec<String>` with few elements)
  - Necessary for correct ownership semantics
  - Not in a hot path

  Then accept the clone rather than over-engineering the solution.

- **When to Optimize**: Profile before optimizing. Most "performance concerns" in build systems are negligible compared to actual compilation time.

- **Avoid Unnecessary Type Conversions**: When threading parameters through multiple function calls, use consistent types (e.g., `String` throughout) rather than converting between `String` and `&str` at each boundary. This eliminates unnecessary allocations and conversions.

### Common Tasks

#### Adding New CLI Flags

1. Add to `BuildArgs` and `WatchArgs` in `cli.rs`
2. Update `From<BuildArgs> for WatchArgs` implementation
3. Pass through `main.rs` to build functions
4. Thread through build system to where it's needed
5. Add unit tests for the new functionality

#### Modifying Compiler Arguments

1. Update `compiler_args()` in `build/compile.rs`
2. Consider both parsing and compilation phases
3. Handle precedence between CLI flags and config
4. Test with various `rescript.json` configurations

#### Working with Dependencies

1. Use `packages.rs` for package discovery
2. Update `deps.rs` for dependency analysis
3. Handle both local and external dependencies
4. Consider dev dependencies vs regular dependencies

#### File Watching

1. Modify `watcher.rs` for file change handling
2. Update `AsyncWatchArgs` for new parameters
3. Handle different file types (`.res`, `.resi`, etc.)
4. Consider performance impact of watching many files

## CI Gotchas

- **`sleep` is fragile** — Prefer polling (e.g., `wait_for_file`) over fixed sleeps. CI runners are slower than local machines.
- **`exit_watcher` is async** — It only signals the watcher to stop (removes the lock file), it doesn't wait for the process to exit. Avoid triggering config-change events before exiting, as the watcher may start a concurrent rebuild.
- **`sed -i` differs across platforms** — macOS requires `sed -i '' ...`, Linux does not. Use the `replace` / `normalize_paths` helpers from `rewatch/tests/utils.sh` instead of raw `sed`.
