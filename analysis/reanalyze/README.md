# Reanalyze

Dead code analysis and other experimental analyses for ReScript.

## Analyses

- **Dead Code Elimination (DCE)** - Detect unused values, types, and modules
- **Exception Analysis** - Track potential exceptions through call chains
- **Termination Analysis** - Experimental analysis for detecting non-terminating functions

## Usage

```bash
# Run DCE analysis on current project (reads rescript.json)
rescript-tools reanalyze -config

# Run DCE analysis on specific CMT directory
rescript-tools reanalyze -dce-cmt path/to/lib/bs

# Run all analyses
rescript-tools reanalyze -all
```

## Performance Options

### Reactive Mode (Experimental)

Cache processed file data and skip unchanged files on subsequent runs:

```bash
rescript-tools reanalyze -config -reactive
```

This provides significant speedup for repeated analysis (e.g., in a watch mode or service):

| Mode | CMT Processing | Total | Speedup |
|------|----------------|-------|---------|
| Standard | 0.78s | 1.01s | 1x |
| Reactive (warm) | 0.01s | 0.20s | 5x |

### Benchmarking

Run analysis multiple times to measure cache effectiveness:

```bash
rescript-tools reanalyze -config -reactive -timing -runs 3
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
| `-transitive` | Force transitive reporting (overrides rescript.json) |
| `-no-transitive` | Disable transitive reporting (overrides rescript.json) |
| `-debug` | Print debug information |
| `-json` | Output in JSON format |
| `-ci` | Internal flag for CI mode |

## Architecture

See [ARCHITECTURE.md](ARCHITECTURE.md) for details on the analysis pipeline.

### Regenerating the checked-in reactive pipeline diagram

`analysis/reanalyze/diagrams/reactive-pipeline-full.mmd` is generated from the live reactive graph printer (`Reactive.to_mermaid()`), and **we check in the non-transitive (`-no-transitive`) variant** because that is where cross-file `hasRefBelow` suppression is relevant (and where reactive invalidation bugs are easiest to spot).

To regenerate it:

```bash
# Run from any ReScript project (so -config works), then capture stderr:
rescript-tools reanalyze -config -reactive -no-transitive -mermaid \
  >/dev/null 2> analysis/reanalyze/diagrams/reactive-pipeline-full.mmd
```

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

When a server is running on the default socket (`<projectRoot>/.rescript-reanalyze.sock`), the regular `reanalyze` command **automatically delegates** to it. This means:

1. **Start the server once** (in the background)
2. **Use the editor normally** — all `reanalyze` calls go through the server
3. **Enjoy fast incremental analysis** — typically 10x faster after the first run

This works transparently with the VS Code extension's "Start Code Analyzer" command.

### Quick Start

```bash
# From anywhere inside your project, start the server:
rescript-tools reanalyze-server

# Now any reanalyze call will automatically use the server:
rescript-tools reanalyze -json  # → delegates to server
```

### Starting the Server

```bash
rescript-tools reanalyze-server [--socket <path>]
```

Options:
- `--socket <path>` — Unix domain socket path (default: `<projectRoot>/.rescript-reanalyze.sock`)

Examples:

```bash
# Start server with default socket (recommended)
rescript-tools reanalyze-server \

# With custom socket path
rescript-tools reanalyze-server \
  --socket /tmp/my-custom.sock \
```

### Behavior

- **Transparent delegation**: Regular `reanalyze` calls automatically use the server if running
- **Default socket**: `<projectRoot>/.rescript-reanalyze.sock` (used by both server and client)
- **Socket location invariant**: socket is always in the project root; `reanalyze` may be called from anywhere inside the project
- **Reactive mode forced**: The server always runs with `-reactive` enabled internally
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

