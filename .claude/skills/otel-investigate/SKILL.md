---
name: investigate
description: Investigate a rewatch OTEL trace report from a span ID.
user-invocable: true
---

# OTEL Investigation Skill

The user invokes this as `/investigate-otel <span_id>` where `<span_id>` is a 16-character hex string identifying an `lsp.llm_report` span in the local otel-viewer.

## OTEL Viewer API (http://localhost:4707)

Only these endpoints exist — do NOT invent others:

- `GET /api/spans/{span_id}/context` — **start here** for `lsp.llm_report` spans. Returns the report's `message` attribute AND the session timeline (filtered `did_save`/`did_change`/`flush` siblings) in one call.
- `GET /api/spans/{span_id}/flush` — structured flush summary with errors inlined. Use for flush spans found in the context.
- `GET /api/spans/{span_id}` — full span detail (attributes, events). Use for drill-down.
- `GET /api/spans/{span_id}/children` — direct children of a span. Use for drill-down.

## Procedure

1. **Fetch context** — this is the entry point, gives you the report message and session timeline:

   ```
   curl -s http://localhost:4707/api/spans/{SPAN_ID}/context | jq .
   ```

   The `target_span.attributes.message` is the user's problem description. `session_spans` is the chronological editing session.

2. **Drill into flushes** — for `lsp.flush` spans in `session_spans` (especially with `has_error: true`):

   ```
   curl -s http://localhost:4707/api/spans/{FLUSH_SPAN_ID}/flush | jq .
   ```

3. **Drill deeper** if needed via `/api/spans/{id}` or `/api/spans/{id}/children`.

4. **Read rewatch source** based on what the traces reveal:
   - `rewatch/src/lsp/` — LSP logic
   - `rewatch/src/build/` — build system
   - `rewatch/src/watcher.rs` — file watching

5. **Summarize**: what the report describes, session timeline, where things went wrong, root cause hypothesis, suggested fix.

## Notes

- Attributes like `code.filepath`, `code.namespace`, `code.lineno`, `thread.name` are instrumentation noise — ignore them.
- The `/context` endpoint already filters to session-relevant spans with gap detection.
- The `/flush` endpoint shows build stages with error texts inlined; collapsed subtrees have `span_id` for drill-down.
