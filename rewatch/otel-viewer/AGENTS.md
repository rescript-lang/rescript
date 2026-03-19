# OTEL Viewer - Development Notes

A local OTLP trace viewer for rewatch development. See [README.md](README.md) for setup and usage.

## Related resources

- **Skill for investigating traces**: [otel-investigate](../../.claude/skills/otel-investigate/SKILL.md) — the `/investigate` skill fetches span data from this viewer's API to diagnose LSP issues.
- **LSP development notes**: [rewatch/src/lsp/AGENTS.md](../src/lsp/AGENTS.md) — tracing patterns, `spawn_blocking` gotchas, and debugging techniques.
- **Rewatch tracing setup**: described in the root [AGENTS.md](../../AGENTS.md) under "OpenTelemetry Tracing".

## Architecture

Single Python file (`server.py`) using FastAPI. Acts as both an OTLP collector (receives protobuf via HTTP) and a web UI. Traces are stored in `traces.db` (SQLite, managed by `db.py`).

## API endpoints

Standard endpoints:

| Endpoint | Description |
|----------|-------------|
| `GET /health` | Health check — returns `{"status": "ok"}` |
| `GET /api/traces` | List all finished traces |
| `GET /api/traces/{trace_id}/roots` | Root spans of a trace |
| `GET /api/spans/{span_id}/children` | Direct children of a span |
| `GET /api/spans/{span_id}` | Full span detail (attributes, events) |
| `POST /api/export` | Export span subtrees as JSON (`{"span_ids": [...]}`) |

Investigation endpoints (used by the `otel-investigate` skill):

| Endpoint | Description |
|----------|-------------|
| `GET /api/spans/{span_id}/context` | Report message + session timeline for `lsp.llm_report` spans |
| `GET /api/spans/{span_id}/flush` | Structured flush summary with errors inlined |

## Sending traces here

```bash
# Check if otel-viewer is already running (it typically is)
curl -sf http://localhost:4707/health

# If not running, start it
cd rewatch/otel-viewer
uv run python server.py

# Run rewatch with tracing
OTEL_EXPORTER_OTLP_ENDPOINT=http://localhost:4707 cargo run --manifest-path rewatch/Cargo.toml -- build

# Or forward test traces
cd tests/rewatch_tests
OTEL_VIEWER_ENDPOINT=http://localhost:4707 yarn test tests/build.test.mjs
```

## Data

`traces.db` is gitignored. Delete it to start fresh.
