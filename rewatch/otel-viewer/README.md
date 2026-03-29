# otel-viewer

A local OTLP trace viewer for rewatch development. Receives OpenTelemetry traces, stores them in SQLite, and provides a web UI for browsing and exporting span data.

This tool was vibe-coded with Claude and replaces the previous Jaeger-based workflow.

## Why not Jaeger?

We originally used the [Jaeger all-in-one](https://www.jaegertracing.io/) Docker image to view OTLP traces. It works, but has several drawbacks for our use case:

- **Requires Docker** — an extra dependency that not every contributor has running, and the image is heavy for what we need.
- **Generic UI** — Jaeger is designed for production distributed tracing across many services. Its UI is cluttered with service selectors, operation dropdowns, and tag filters that are irrelevant when you're just debugging a single rewatch build or LSP session.
- **No LLM export** — we frequently want to copy a subtree of spans (with attributes and events) as JSON to paste into an LLM conversation for debugging. Jaeger has no way to do this.
- **No deep links to traces** — Jaeger generates opaque URLs that don't map cleanly to trace IDs. Our viewer gives each trace a permalink at `/traces/{trace_id}`.

otel-viewer is a single Python file with a SQLite database. It starts instantly, needs no Docker, and has a focused UI tailored to rewatch development.

## Setup

Requires [uv](https://docs.astral.sh/uv/), a fast Python package and project manager written in Rust. It replaces `pip`, `virtualenv`, and `pyenv` in a single tool — it handles installing Python itself, creating virtual environments, and installing dependencies. If you've never worked with Python before, `uv` is the only thing you need to install.

On macOS:

```bash
brew install uv
```

Then install the project dependencies:

```bash
cd rewatch/otel-viewer
uv sync
```

## Usage

Start the viewer:

```bash
uv run python server.py
```

This starts a server on port 4707 that acts as both an OTLP collector and a web UI.

Run rewatch with tracing pointed at the viewer:

```bash
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4707 cargo run --manifest-path rewatch/Cargo.toml -- build
```

Open http://localhost:4707 in your browser to browse traces.

## Features

- **Trace list**: Shows all captured traces with timestamp, root span name, duration, and span count.
- **Lazy-loaded span tree**: Click a trace to see its root spans. Expand spans to load children on demand.
- **Export for LLM**: Select spans via checkboxes, click "Copy for LLM" to copy the full subtree (including all descendants) as JSON to your clipboard.

## API

The following endpoints are available for programmatic access:

- `GET /api/traces` — list all traces
- `GET /api/traces/{trace_id}/roots` — root spans of a trace
- `GET /api/spans/{span_id}/children` — direct children of a span
- `GET /api/spans/{span_id}` — full detail for a single span (attributes, events)
- `POST /api/export` — export selected spans with full subtrees; body: `{"span_ids": ["id1", "id2"]}`

## How traces work

Only **finished traces** are shown in the UI. A trace is considered finished when its root span (a span with no parent) has been received. This means:

- Traces from completed `build`, `clean`, or `format` commands appear immediately.
- LSP traces only appear after the LSP server shuts down cleanly and flushes its telemetry. If the LSP process is killed without a graceful shutdown (e.g. `kill -9`), the root `rewatch.lsp` span is never sent and the trace won't be listed.
- During development, use `--watch` mode (`uv run python server.py --watch`) and restart the LSP server cleanly (via editor restart or SIGTERM) to see its trace.

Spans from in-progress or interrupted traces are still stored in the database but hidden from the trace list.

## Viewing test traces

The rewatch integration tests (`tests/rewatch_tests/`) can forward their OTEL traces to this viewer. Start the viewer, then run tests with `OTEL_VIEWER_ENDPOINT` set:

```bash
# Terminal 1
cd rewatch/otel-viewer
uv run python server.py

# Terminal 2
cd tests/rewatch_tests
OTEL_VIEWER_ENDPOINT=http://localhost:4707 yarn test tests/build.test.mjs
```

Test traces appear in the viewer alongside manually triggered builds. The forwarding is fire-and-forget — if the viewer isn't running, tests pass normally.

## Data

Traces are stored in `traces.db` (SQLite) in the `otel-viewer` directory. Delete this file to start fresh.
