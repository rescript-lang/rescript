# EmbedLang Performance TODO

This document tracks incremental improvements to EmbedLang performance in Rewatch. Each step should land with tests passing and without changing user-facing semantics.

Suggested order of changes (from EmbedLang design):

1) Single index read + tag→generator map
- Read `*.embeds.json` once per module and reuse the parsed structure for both planning and processing.
- Build a per-package `tag -> generator` map once and reuse for O(1) lookups.

2) Global scheduling + batch parse
- Replace per‑module pools/loops with a single global work queue across all modules.
- Accumulate generated files and parse them via the standard parallel AST generation (avoid per‑file `generate_ast`).

3) Batch‑first (one‑shot) protocol
- Group requests per `generator.id` and send in batches to reduce process overhead while keeping one‑shot execution.

4) Daemon mode scaffolding
- Add optional persistent per‑generator processes with a simple stdio protocol and deterministic queues.

5) Watch maps + cleanup reductions
- Maintain in‑memory maps for `extraSource -> tags -> modules` to avoid disk scans.
- Reduce O(outDir) scans in cleanup by grouping or per‑module manifests.

