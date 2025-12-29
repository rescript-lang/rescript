# Reanalyze

Dead code analysis and other experimental analyses for ReScript.

## Analyses

- **Dead Code Elimination (DCE)** - Detect unused values, types, and modules
- **Exception Analysis** - Track potential exceptions through call chains
- **Termination Analysis** - Experimental analysis for detecting non-terminating functions

## Usage

```bash
# Run DCE analysis on current project (reads rescript.json)
rescript-editor-analysis reanalyze -config

# Run DCE analysis on specific CMT directory
rescript-editor-analysis reanalyze -dce-cmt path/to/lib/bs

# Run all analyses
rescript-editor-analysis reanalyze -all
```

## Performance Options

### Reactive Mode (Experimental)

Cache processed file data and skip unchanged files on subsequent runs:

```bash
rescript-editor-analysis reanalyze -config -reactive
```

This provides significant speedup for repeated analysis (e.g., in a watch mode or service):

| Mode | CMT Processing | Total | Speedup |
|------|----------------|-------|---------|
| Standard | 0.78s | 1.01s | 1x |
| Reactive (warm) | 0.01s | 0.20s | 5x |

### Benchmarking

Run analysis multiple times to measure cache effectiveness:

```bash
rescript-editor-analysis reanalyze -config -reactive -timing -runs 3
```

## CLI Flags

| Flag | Description |
|------|-------------|
| `-config` | Read analysis mode from rescript.json |
| `-dce` | Run dead code analysis |
| `-exception` | Run exception analysis |
| `-termination` | Run termination analysis |
| `-all` | Run all analyses |
| `-reactive` | Cache processed file_data, skip unchanged files |
| `-runs n` | Run analysis n times (for benchmarking) |
| `-churn n` | Remove/re-add n random files between runs (incremental correctness/perf testing) |
| `-timing` | Report timing of analysis phases |
| `-mermaid` | Output Mermaid diagram of reactive pipeline (to stderr) |
| `-debug` | Print debug information |
| `-json` | Output in JSON format |
| `-ci` | Internal flag for CI mode |

## Architecture

See [ARCHITECTURE.md](ARCHITECTURE.md) for details on the analysis pipeline.

The DCE analysis is structured as a pure pipeline:

1. **MAP** - Process each `.cmt` file independently → per-file data
2. **MERGE** - Combine all per-file data → project-wide view
3. **SOLVE** - Compute dead/live status → issues
4. **REPORT** - Output issues

This design enables order-independence and incremental updates.

## Reactive Analysis

The reactive mode (`-reactive`) caches processed per-file results and efficiently skips unchanged files on subsequent runs:

1. **First run**: All files are processed and results cached
2. **Subsequent runs**: Only changed files are re-processed
3. **Unchanged files**: Return cached `file_data` immediately (no I/O or unmarshalling)

This is the foundation for the **reanalyze-server** — a persistent analysis service that keeps reactive state warm across requests.

## Reanalyze Server

A long-lived server process that keeps reactive analysis state warm across multiple requests. This enables fast incremental analysis for editor integration.

### Transparent Server Delegation

When a server is running on the default socket (`/tmp/rescript-reanalyze.sock`), the regular `reanalyze` command **automatically delegates** to it. This means:

1. **Start the server once** (in the background)
2. **Use the editor normally** — all `reanalyze` calls go through the server
3. **Enjoy fast incremental analysis** — typically 10x faster after the first run

This works transparently with the VS Code extension's "Start Code Analyzer" command.

### Quick Start

```bash
# In your project directory, start the server:
rescript-editor-analysis reanalyze-server --cwd . -- -config -ci -json

# Now any reanalyze call will automatically use the server:
rescript-editor-analysis reanalyze -config -ci -json  # → delegates to server
```

### Starting the Server

```bash
rescript-editor-analysis reanalyze-server [--socket <path>] [--cwd <dir>] [--once] -- <reanalyze args...>
```

Options:
- `--socket <path>` — Unix domain socket path (default: `/tmp/rescript-reanalyze.sock`)
- `--cwd <dir>` — Working directory for analysis (useful when launched from a different directory)
- `--once` — Handle one request then exit (for testing)
- `-- <args>` — Reanalyze arguments the server will accept (e.g., `-config -ci -json`)

Examples:

```bash
# Start server with default socket (recommended)
rescript-editor-analysis reanalyze-server \
  --cwd /path/to/my-project \
  -- -config -ci -json

# With custom socket path
rescript-editor-analysis reanalyze-server \
  --socket /tmp/my-custom.sock \
  --cwd /path/to/my-project \
  -- -config -ci -json
```

### Explicit Requests

You can also send requests explicitly (useful for testing or custom socket paths):

```bash
rescript-editor-analysis reanalyze-server-request \
  --cwd /path/to/my-project \
  -- -config -ci -json
```

### Behavior

- **Transparent delegation**: Regular `reanalyze` calls automatically use the server if running
- **Default socket**: `/tmp/rescript-reanalyze.sock` (used by both server and client)
- **Reactive mode forced**: The server always runs with `-reactive` enabled internally
- **Strict argv matching**: Requests must use the same arguments the server was started with
- **Same output**: stdout/stderr/exit-code match what a direct CLI invocation would produce
- **Incremental updates**: When source files change and the project is rebuilt, subsequent requests reflect the updated analysis

### Typical Workflow

1. **Start server** (once, in background)
2. **Edit source files**
3. **Rebuild project** (`yarn build` / `rescript build`)
4. **Use editor** — analysis requests automatically go through the server
5. **Stop server** when done (or leave running)

## Development

### Testing

```bash
# Run reanalyze tests
make test-reanalyze

# Run with shuffled file order (order-independence test)
make test-reanalyze-order-independence
```

The order-independence test uses the test-only CLI flag `-test-shuffle`, which randomizes the per-file processing order to ensure results don’t depend on traversal order.

### Benchmarking

The benchmark project generates ~5000 files to measure analysis performance:

```bash
cd tests/analysis_tests/tests-reanalyze/deadcode-benchmark

# Generate files, build, and run benchmark
make benchmark

# Compare CMT cache effectiveness (cold vs warm)
make time-cache

# Benchmark reactive mode (shows speedup on repeated runs)
make time-reactive
```

#### Reactive Benchmark

The `make time-reactive` target runs:

1. **Standard mode** (baseline) - Full analysis every time
2. **Reactive mode** with 3 runs - First run is cold (processes all files), subsequent runs are warm (skip unchanged files)

Example output:

```
=== Reactive mode benchmark ===

Standard (baseline):
  CMT processing: 0.78s
  Total: 1.01s

Reactive mode (3 runs):
  === Run 1/3 ===
  CMT processing: 0.78s
  Total: 1.02s
  === Run 2/3 ===
  CMT processing: 0.01s  <-- 74x faster
  Total: 0.20s           <-- 5x faster
  === Run 3/3 ===
  CMT processing: 0.01s
  Total: 0.20s
```

