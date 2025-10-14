# Embed Lang (Rewatch) — Design Spec

This document proposes “embed lang”, a Rewatch feature that lets users call external code generators from embedded code snippets in ReScript source files, generate ReScript modules, and link them seamlessly into the original source.

## Implementation Status (WIP)
- Phase progress
  - Phase 2 (Rewatch: Parse step): DONE — `-embeds <csv>` threaded via parser args from `rescript.json` tags.
  - Phase 3 (Generator invocation): MOSTLY DONE — per‑embed process invocation + generated file write + headers, caching (hash + extraSources mtime), per‑embed timeout, and a concurrency cap implemented; remaining work: richer progress UX (per‑embed/per‑module events) and polish.
  - Phase 4 (Inline rewrite via PPX): PRESENT — embeds are rewritten directly during the main compile using a deterministic naming scheme; no separate rewrite pass or map artifacts.
  - Phase 5 (Rewatch integration): DONE — integrates generation + compile, registers generated modules and parses their ASTs.
- Phase 7 (Watch/cleanup): DONE — extraSources changes now invalidate affected modules in watch mode; stale generated files are cleaned up per-module.
- Phase 8 (Diagnostics): PARTIAL — structured generator diagnostics mapping with code frames; map‑mismatch errors are obsolete in the single‑pass design.
- Schema tooling — ADDED: run `rescript schema embeds --output-dir ./schemas --openapi` to generate JSON Schema for the generator input/output and an OpenAPI (components-only) document. Fields are camelCase and unknown fields are denied for generator-facing types.
  - Committed copies live at `docs/schemas/`:
    - `docs/schemas/embedlang.input.schema.json`
    - `docs/schemas/embedlang.output.schema.json`
    - `docs/schemas/embedlang.openapi.json`
  - Or regenerate via `make schemas`.
- Test coverage
  - Compiler‑only flow: `rewatch/tests/embeds-compiler.sh` validates index + PPX rewrite (no separate rewrite pass).
  - Rewatch E2E: `rewatch/tests/embeds.sh` builds a fixture repo and snapshots index, rewritten source, and generated module.
- Known gaps (to implement next)
  - Progress reporting polish: concise per‑embed and per‑module events (discovered, start, cache hit/miss, done/failed) and build summaries; integrate with progress bar and `--verbose`.
  - Concurrency cap and scheduling for generator processes (e.g. limit to num_cpus/2) with stable deterministic ordering per module.

## Summary
- Users write an embed expression in `.res` files using a tag and either:
  - a string literal (backtick or normal quoted), for example:
    - `let query = ::sql.one(`/* @name GetUser */ select * from users where id = :id`)`
    - or `let query = ::sql.one("/* @name GetUser */ select * from users where id = :id")`
  - a config record literal, for example:
    - `let query = ::sql.one({id: "GetUser", query: "select * from users where id = :id"})`
  - Equivalent extension form: `%embed.sql.one("...")` (printed as `::sql.one(...)`). Note: plain `%sql.one("...")` is not treated as an embed and remains available for other PPXs.
- The compiler detects these embeds during parsing and records them. Rewrites happen inline during the normal compile using a PPX that deterministically computes the target generated module name — no second pass or resolution map.
- Rewatch invokes user-configured generators based on the recorded embeds, receives ReScript code, and writes generated files with a conventional name (e.g. `SomeFile__embed_sql_one_GetUser.res`, optional `.resi`).
- The embed PPX performs the AST rewrite to `GeneratedModule.default` directly in the compile pipeline, based solely on the tag and a deterministic filename scheme.
- Errors from generators are mapped back to original source locations by Rewatch. Caching avoids unnecessary generator runs.

## Goals
- Support user-defined generators that “claim” one or more embed tags.
- Provide a stable file/module naming convention for generated modules.
- Seamlessly link generated modules in place of the embed expression without changing user code on disk or requiring a second compiler pass.
- Map generator diagnostics to user source locations so they appear in editors.
- Add caching and invalidation driven by the embed content and additional watched sources (e.g. schema files).
- Integrate cleanly with Rewatch’s parse/compile/watch pipeline.

## Non‑Goals (Initial Version)
- Reserving new keywords. The `::` prefix is not a keyword and does not conflict with existing syntax.
- Supporting multi-file generation from a single embed (future extension).
- Providing a long-lived generator “server mode” (future optimization).

## Syntax & Semantics
- Embed expression grammar:
  - `::<tag>(<string-literal>)`
  - `::<tag>.<subtag>(<string-literal>)`
  - `::<tag>({<config-object>})` where the config is a record literal with JSON‑serializable values
  - Equivalent extension form: `%embed.<tag>(<string-literal>)` and `%embed.<tag>.<subtag>(<string-literal>)`
  - The `::` form parses to an extension node with the attribute name automatically prefixed with `embed.`; i.e. `::sql.one(...)` parses as `%embed.sql.one(...)` in the parsetree. The printer also emits `::sql.one(...)` when encountering `%embed.<tag>(...)`.
  - The `<string-literal>` can be a backtick string or a normal quoted string, but must be a single literal (no concatenation, pipelines, or computed expressions). Interpolation is not allowed.
  - The `<config-object>` must be a single record literal whose fields and nested values are JSON‑serializable (string, number, boolean, null, arrays, objects); no computed expressions. It must include `id: string` for naming; all fields are forwarded to the generator as `data`.
  - Examples: `::sql.one(`...`)`, `::graphql.query(\"...\")`, `::sql.one({id: \"GetUser\", query: \"select * from users where id = :id\"})`
- The embed expression evaluates to the value exported by the generated module’s entry binding, which is always `default`.
- The embedded string may contain metadata comments (e.g. `/* @name GetUser */`) consumed by the generator. The compiler does not interpret these beyond discovery.

Syntax support notes:
- Tags may contain dots in their names (e.g. `sql.one`); the parser accepts dotted extension names in both expression and module positions.
- The printer recognizes `%embed.<tag>(...)` and prints it as `::<tag>(...)`.
- Only expression and module‑expression contexts are supported in v1 (see “Rewrite semantics”). Embeds cannot appear in pattern, type, or other unsupported positions.

Rewrite semantics:
- Value expression context:
  - `%tag(...): expr` or `::tag(...): expr` → `GeneratedModule.default`
- Module expression context:
  - `module X = %tag(...)` or `module X = ::tag(...)` → `module X = GeneratedModule`
  - `include %tag(...)` or `include ::tag(...)` → `include GeneratedModule`

## File & Module Naming
- Generated filename: `<SourceModule>__embed_<tagNormalized>_<suffix>.res`
  - `tagNormalized` = tag with non‑alphanumeric chars replaced by `_` (e.g. `sql.one` → `sql_one`).
  - `suffix` is deterministic and not supplied by the generator:
    - For simple string embeds (`::<tag>("...")`): `_N` where `N` is the 1‑based occurrence index for this tag within the source file in appearance order (e.g. `_1`, `_2`).
    - For config embeds (`::<tag>({...})`): the sanitized `id` field value from the config object (must be a string) with non‑alphanumeric characters replaced by `_`.
  - Module name is derived from filename as usual (`SomeFile__embed_sql_one_GetUser`).
  
The compiler rewrites the embed expression to `SomeFile__embed_sql_one_<suffix>.default` via PPX.

## Configuration (rescript.json)
Add a new top‑level `embeds` key to configure generators and behavior:

```
{
  // ...existing config
  "embeds": {
    "generators": [
      {
        "id": "sqlgen",
        "cmd": "node",
        "args": ["scripts/sql-gen.mjs"],
        "cwd": "./",
        "env": { "DATABASE_URL": "env:DATABASE_URL" },
        "tags": ["sql.one"],
        "extraSources": [
          "db/schema.sql",
          "db/migrations/**/*.sql"
        ],
        "timeoutMs": 10000
      }
    ],
    "outDir": "src/__generated__"            // optional; default is <pkgRoot>/src/__generated__,
                                               // falls back to <pkgRoot>/__generated__ if no src/
  }
}
```

Notes:
- `env` values with `env:` prefix are resolved from the current environment at runtime.
- `extraSources` are per‑generator lists of additional files to hash and watch. In v1, generators do not return per‑embed dependency paths.
- CLI flags may override `outDir` and timeouts (standard precedence rules apply).
- Naming prefix is fixed by convention; there is no `modulePrefix` configuration.
- Future: Generators should be able to ship a base config that projects can extend; only project‑specific values need to be set by users.
- Multi‑package repos: defaults apply per package root; generated files live under each package’s own outDir.

## Generator Protocol
Generators are simple processes invoked per embed. Communication is over stdin/stdout using JSON.

Invocation:
- Working directory: `cwd` from config (or project root if absent).
- Environment: inherited + configured overrides.
- Input: a single JSON object on stdin; Output: a single JSON object on stdout.

Input JSON (v1):
```
{
  "version": 1,
  "tag": "sql.one",
  "data": "/* @name GetUser */ select * from users where id = :id",   // string embeds
  // or, for config embeds
  // "data": {"id": "GetUser", "query": "select * from users where id = :id", ...},
  "source": {
    "path": "src/SomeFile.res",
    "module": "SomeFile"
  },
  "occurrenceIndex": 1,                // 1-based within this file for this tag
  "config": {
    "extraSources": ["db/schema.sql"], // from rescript.json (resolved to absolute paths)
    "options": {}                       // reserved for future project-level options
  }
}
```

Successful Output JSON:
```
{
  "status": "ok",
  "code": "let query = \"select * from users where id = $1\"\n type params = {...}\n let default = ...\n"
}
```

Error Output JSON (diagnostics map to the embed string):
```
{
  "status": "error",
  "errors": [
    {
      "message": "Unknown column idd",
      "severity": "error",        // "error" | "warning" | "info"
      "start": {"line": 1, "column": 35},
      "end":   {"line": 1, "column": 38},
      "code": "SQL001"
    }
  ]
}
```

Protocol considerations:
- Rewatch enforces a per‑embed timeout (configurable). Timeout or non‑zero exit → treated as a generator error.
- Generators do not implement caching; Rewatch is the source of truth for cache decisions.
- All paths in generator output are normalized to absolute paths by Rewatch and validated to be inside the project root unless explicitly allowed.
- Generators cannot influence file naming: the filename is determined by the tag + (occurrenceIndex or config.id). Rewatch and the PPX must compute the same target.
- Generators cannot control the entry binding; the compiler always expects `default`.
 - For config embeds, the full config object is forwarded as `data` and must be JSON‑serializable (no functions, symbols, or non‑JSON values).

## Build & Watch Flow (High‑Level)
1. Compiler Embed Index (pass 1)
   - During parsing, the compiler records all embed occurrences (tag, argument data (string or config), precise ranges, occurrence index, and context: expression vs module expression vs include) and writes a per‑module artifact next to the `.ast` file, e.g. `SomeFile.embeds.json`.
   - Index emission is controlled by a new `-embeds <csv>` flag. The timing mirrors the approach in PR #6823: emit immediately after parsing (before type‑checking and heavy transforms), alongside the binary AST output, so that Rewatch never needs to re‑parse sources.
   - This artifact is the single source of truth for Rewatch to know which embeds exist, without Rewatch re‑parsing sources. For `::tag(...)`, the recorded `tag` is the base name without the `embed.` prefix (e.g. `sql.one`).
2. Caching Check
   - For each embed in the index, compute an embed hash `H = hash(specVersion + generator.id + tag + dataAsJson)`.
   - For per‑generator `extraSources`, use mtime‑based invalidation by default (content hashes optional if needed).
   - If a generated module exists with matching header metadata (see “Generated File Format”), skip generation.
3. Generation
   - If cache miss or invalid, invoke the generator and capture output.
   - On `status=ok`, write/overwrite the generated `.res` file to `outDir` (default `src/__generated__`) with the conventional name.
   - On `status=error`, collect diagnostics mapped to the original source positions (see “Diagnostics & Mapping”).
4. Rewrite During Compile (Single‑Pass)
   - The embed PPX runs as part of the main compile and rewrites embeds directly in the AST to reference the computed generated module:
     - Expression contexts: `%tag(...)` or `::tag(...)` → `GeneratedModule.default`.
     - Module contexts: `module X = %tag(...)` or `module X = ::tag(...)` → `module X = GeneratedModule`.
     - Include contexts: `include %tag(...)` or `include ::tag(...)` → `include GeneratedModule`.
   - The PPX computes the same deterministic target module name as Rewatch using the tag and either the occurrence index (string case) or the `id` in the config object.
5. Dependency Graph
   - Add edges: `OriginalFile -> GeneratedModule` and `GeneratedModule -> extraSources`.
   - Include generated files in the parse/compile lists alongside user sources.
6. Watch Mode
   - Watch original `.res` files, generated files (discouraged to edit manually), and all `extraSources`.
   - On changes, invalidate relevant embeds via mtime checks, re‑generate if needed, and rebuild affected modules.

## Compiler Flags & Entry Points
- `-embeds <csv>`
  - Example: `-embeds sql.one,sql.many,sql.execute`
  - When present during parsing, the compiler collects only these extension names and emits `SomeFile.embeds.json` next to the `.ast`.
  - The flag can also accept `all` to collect all extension names if desired in the future.
  
There is no separate `-rewrite-embeds` entry point in the single‑pass design; rewriting is handled by the embed PPX during normal compilation.

## Artifact Filenames
- Per module (next to `.ast`):
  - Index: `SomeFile.embeds.json`
  - (removed) Resolution map: no longer produced in the single‑pass design

## Artifact Schemas (initial)
- `SomeFile.embeds.json` (embed index; written during parse with `-embeds`):
```
{
  "version": 1,
  "module": "SomeFile",
  "sourcePath": "src/SomeFile.res",         // project‑relative (normalized to /)
  "embeds": [
    {
      "tag": "sql.one",
      "context": "expr",                    // "expr" | "module" | "include"
      "occurrenceIndex": 1,                  // 1‑based within this file for this tag
      "range": {"start": {"line": 5, "column": 12}, "end": {"line": 5, "column": 78}},
      "data": "/* @name GetUser */ select * from users where id = :id",
      // or {"id":"GetUser","query":"...", ...}
      "literalHash": "<hex>"                 // hash(tag + dataAsJson)
    }
  ]
}
```

## Cross‑Platform Paths
- All paths written to artifacts (`*.embeds.json`) use `/` as the separator and are project‑relative where possible.
- Rewatch normalizes paths when computing hashes and comparing cache keys to avoid Windows vs POSIX discrepancies.

Resolution map lookup: not applicable in the single‑pass design.

## Generated File Format
- Generated file begins with a header comment Rewatch can read quickly without parsing full code:
  - `/* rewatch-embed: v1; tag=sql.one; src=src/SomeFile.res; idx=1; suffix=GetUser; entry=default; hash=<hex>; gen=sqlgen */`
  - Additionally, include a first-line single-line marker for very fast cache checks (compatible with prior art): `// @sourceHash <hex>`
- Rewatch reads only the first line to validate hash equality for cache hits.
- The remainder is arbitrary ReScript code provided by the generator. Best practices:
  - Always export a stable `default` binding.
  - Keep top-level names deterministic for reproducibility.

## Loop Prevention (No Nested Embeds)
- Generated files are ignored by the compiler’s embed indexer (exclude `outDir` and/or detect header marker).
- This prevents infinite embed expansion chains and cyclic generation.

## Diagnostics & Mapping
- Generator diagnostics are returned relative to the embedded string (line/column within the literal). Rewatch computes absolute source positions using the ranges from the compiler’s embed index and prints a concise code frame.
- The compiler handles PPX rewrites directly on the AST; diagnostics from the compiler refer to the original source files.
- Error presentation: Rewatch includes a code frame in logs with the embedded code, highlights the error span, and shows surrounding context for quick inspection.
 

## Invalidation & Caching
- Cache key includes:
  - `tag`, `data` (string or config) content as canonical JSON, generator `id`, generator command string/version, embed spec version. The embed `data` is content‑hashed; per‑generator `extraSources` use mtime by default.
- Quick check reads only the generated file’s header to confirm hash equality; if mismatch, regenerate.
- Rewatch may persist a small cache index to memoize `extraSources` mtimes for performance.

## Edge Cases & Errors
- Unknown tag: error with code `EMBED_NO_GENERATOR` listing known tags.
- Missing/invalid string literal: error `EMBED_SYNTAX` with a short hint.
- Generator timeout/crash or structured errors: log `EMBED_GENERATOR_FAILED` with mapped code frames.
- Naming collision: error (`EMBED_NAMING_CONFLICT`) with both locations.
- Illegal id chars: sanitized to `_`; collapse repeats.
- `.resi` generation: not supported in v1; the generated module is compiled without an interface.
- Nested embeds: disallowed. Generated files are ignored by the compiler’s embed indexer and never expanded.

## Naming & Collision Policy
- File/module naming is fully deterministic and not controlled by generators.
- For string embeds: suffix `_N` where `N` is 1‑based per‑tag occurrence within the file.
- For config embeds: suffix from `id` after sanitization to `[A-Za-z0-9_]+`.
- Rewatch enforces uniqueness per source file and tag for a given build; collisions raise `EMBED_NAMING_CONFLICT` with both locations.
- Cross‑file collisions are avoided by including the source module name in the generated filename (e.g., `SomeFile__embed_sql_one_<suffix>.res`).

## Cleanup & Lifecycle
- Per build (and on watch updates), compute the expected set of generated files for each source file based on current embeds.
- Remove stale generated files that were previously produced for a source but are no longer referenced (e.g., embed removed or renamed) and clear their cache entries.
- When a source file is deleted, delete all its associated generated files.
- Generated files reside in `outDir` (default `src/__generated__`); cleanup routines operate in that directory accordingly.

## Security Considerations
- Generators run arbitrary commands configured by the user’s project. Rewatch does not fetch or execute remote code.
- Paths returned by generators are normalized and must resolve within the workspace unless explicitly allowed by a `allowOutsideProjectRoot` flag (off by default).
- Consider adding an opt‑in sandbox profile in the future.

## Performance Considerations
- Minimize full content hashing by memoizing `extraSources` hashes per path.
- Cap concurrent generator processes to `N = max(1, num_cpus / 2)` with a small queue.
- Rely on the compiler’s embed index artifact; Rewatch does not scan sources.
 - Rewrite occurs inline via PPX during normal compilation and is a small traversal relative to type checking and codegen.

## Testing Plan
- Compiler unit: embed indexer collects tags for both backtick and normal string literals; ignores generated outDir; occurrence indices stability. Validate PPX rewrite behavior for string vs config embeds.
- Rewatch unit: naming sanitization; mtime vs content hash behavior for extraSources.
- Integration (rewatch/tests):
  - Happy path: create a small generator that returns code; ensure generated file(s) are created and linked; build succeeds.
  - Cache hit/miss: modify embed input (`data`) and `extraSources`; ensure regeneration occurs only when needed. Covered by `rewatch/tests/embeds-cache.sh` (asserts generator run count and invalidation on `extraSources`).
  - Errors: generator returns diagnostics; verify mapping to original file positions and code‑fenced logs.
  - Watch: change extra source; verify incremental rebuild of affected modules and cleanup of unused files.

## Future Extensions
- Long‑lived generator server with handshake to claim tags and avoid per‑embed process cost.
- Multiple files per embed (e.g. helper modules), richer emission APIs.
- Richer mapping: embed‑specific source maps and IDE hovers with generator metadata.
- Support structured config schemas per tag (validated and surfaced to generators).

## Open Questions
1. Embed index format
   - JSON vs compact binary; stability/versioning. (Timing is specified: emit index right after parse.)
2. Naming collisions across files
   - If two files produce the same `<suffix>`, we’re safe because the filename also includes the source module; confirm no package‑level namespace issues.
3. Diagnostics severity mapping
   - Exact mapping to ReScript’s error/warning display conventions?

---

## Step‑By‑Step Implementation Plan

Phase 0 — Wiring and Flags
- Define CLI flag `-embeds <csv|all>` in `bsc` (parser phase only).
  
- Remove the standalone `-rewrite-embeds` entry; rewriting happens via the embed PPX.
- Plumb `-embeds` through `compiler/bsc/rescript_compiler_main.ml` and ensure it is orthogonal to existing flags (no impact on `-bs-no-builtin-ppx`).
Tests (E2E‑first):
- Smoke: `bsc -help` lists `-embeds`; no `-rewrite-embeds` entry.
- Minimal unit (optional): flag wiring helpers, if any, remain backward compatible.

Phase 1 — Compiler: Embed Indexing (after parse)
- Add a lightweight AST walker to collect embeds:
  - Expression: `Pexp_extension (name, payload)` where `name` matches configured tags.
  - Module expr: `Pmod_extension ...` and `Pstr_include` forms for include contexts.
  - Accept either a single string literal (backtick or quoted) or a single record literal with JSON‑serializable fields. Otherwise, record an `EMBED_SYNTAX` error location.
- Emit `SomeFile.embeds.json` next to `.ast` when `-embeds` is present:
  - Fields: version, module, sourcePath (project‑relative), embeds[] with tag, context, occurrenceIndex (1‑based per‑tag), range, data (string or object), literalHash.
  - Use `/` path separators for portability.
- Exclude generated outDir from indexing (by path prefix and by reading the generated header marker if present) to prevent nested embeds.
- Implementation points:
  - Hook immediately after parse and before any heavy transforms (mirroring PR #6823 pattern used for early artifacts).
  - Ensure binary AST emission remains unchanged.
Tests (E2E‑first):
- Golden: `bsc -bs-ast -embeds sql.one -o build/src/Foo src/Foo.res` produces `build/src/Foo.ast` and `build/src/Foo.embeds.json` matching expected JSON (dotted tags, string and config arguments, expr/module/include contexts, correct occurrenceIndex, ranges present).
- Golden: non‑literal payload case fixture → indexer reports `EMBED_SYNTAX` in a companion diagnostics artifact or stderr (choose one) with correct location.
- Golden: files under outDir are ignored (no index emitted). 
- Minimal unit (optional): pure helpers like literal hashing and tag normalization.

Phase 2 — Rewatch: Parse Step and Tag Discovery
- Compute the set of tags to index from `rescript.json` `embeds.generators[].tags`.
- During AST generation (`build/parse.rs`), add `-embeds <csv>` to the `bsc -bs-ast` invocation for modules in packages that configure embeds.
- Confirm index files are written and co‑located with `.ast` files; add error handling if missing when embeds are configured.
Tests (Integration):
- Rust unit: `parse.rs` threads `-embeds <csv>` when configured; absent otherwise.
- Rewatch testrepo: configured tags → `*.embeds.json` co‑located with `.ast`; unset config → none created.

Phase 3 — Rewatch: Generator Invocation & Caching
- Read `SomeFile.embeds.json` and group embeds by generator (tag → generator.id).
- For each embed:
  - Compute cache key `H = hash(specVersion + generator.id + tag + dataAsJson)`.
  - Check existing generated file header for a quick hash match; also check per‑generator `extraSources` mtimes.
  - On miss or invalidation, spawn the generator process with the JSON protocol over stdin/stdout; enforce `timeoutMs`.
  - Validate response: ensure `entry` is `default`, normalize paths, collect diagnostics.
  - Write generated `*.res` (and header) to `outDir` using naming scheme `<SourceModule>__embed_<tagNormalized>_<suffix>.res` computed from occurrence index or config `id`.
  - Enforce name uniqueness per source+tag; on collision, raise `EMBED_NAMING_CONFLICT` with both locations.
- Concurrency: cap concurrent processes to `max(1, num_cpus/2)` (implemented).
- Maintain a cache index for `extraSources` mtimes to avoid repeated stat calls.
 - Progress reporting: for each module and embed, emit concise progress events —
   - discovery (N embeds found), per‑embed start, cache hit/miss, done/failed (with error class),
   - and a per‑module summary (generated X, reused Y, failed Z). Integrate with the existing progress bar and `--verbose`.
Tests (Integration):
- Stub generator returns `status=ok`: generated files written with header; second run is a cache hit.
- Modify embed string → cache miss; touch `extraSources` → cache miss; unrelated change → cache hit.
- Diagnostics mapping: generator error (line/column) → logs show mapped source span + code frame; non‑zero exit/timeout → `EMBED_GENERATOR_FAILED`.
- Minimal unit: naming sanitization and collision detection.

Phase 4 — Compiler: Embed PPX Rewrite
- Implement a PPX that:
  - Counts per‑tag occurrences in a module in appearance order.
  - Detects argument kind (string vs record literal) and computes the target module name deterministically.
  - Rewrites expression contexts to `GeneratedModule.default`, and module/include contexts to the module itself.
  - Rejects non‑literal or non‑JSON‑serializable config values with `EMBED_SYNTAX`.
- Ensure counting rules match the indexer to keep filenames in sync with Rewatch.
Tests (E2E‑first):
- Print parsetree/source with `-dsource` and assert rewritten form shows `GeneratedModule.default`.
- Idempotency: PPX rewrite does not re‑enter on generated modules.

Phase 5 — Rewatch: Pipeline Integration
- After AST generation and generation, compile modules normally; the PPX handles rewriting during compilation.
- Extend dependency graph:
  - `OriginalFile → GeneratedModule(s)` and `GeneratedModule → extraSources`.
  - Treat generated files as regular sources for ordering; do not index embeds within them.
- Progress reporting: show per‑module summaries (modules with embeds, total embeds processed, generated/reused/failed).
Tests (Integration):
- End‑to‑end: `bsc -bs-ast -embeds ...` → generate files → normal compile produces JS; imports from generated module resolved.
- Type errors in generated code surface normally; removing an embed or generated file triggers correct rebuild and cleanup.
- Multi‑package: generated files live under each package’s outDir; no cross‑package collisions.

Phase 6 — Watch Mode & Cleanup
- After AST generation and generation, compile modules normally; the PPX handles rewriting during compilation.
- Watch original `.res`, generated `outDir`, and `extraSources`.
- On changes, invalidate affected embeds, regenerate only for impacted modules, and rebuild dependents.
- Cleanup: compute expected generated files per source; remove stale files and clear cache entries when embeds are removed or sources deleted.
Tests (Integration, watch):
- Change `extraSources` → only affected module regenerates; JS updates; others untouched.
- Delete an embed → stale generated files removed; dependent modules rebuild.
- Manual edits to generated files are overwritten by the next build.

Phase 7 — Errors & Diagnostics
 - Map generator diagnostics (literal‑relative positions) to absolute source spans via the index ranges; print rich code frames.
 - Error codes: `EMBED_NO_GENERATOR`, `EMBED_SYNTAX`, `EMBED_GENERATOR_FAILED`, `EMBED_NAMING_CONFLICT`.
 - Align severity with compiler conventions; ensure non‑zero exit on errors to integrate with CI.
Tests (Integration):
 - Each error class reproduced in testrepo with stable messages and exit codes.
 - Optional unit: code frame formatting helper includes correct context lines.

- E2E‑first: integration tests live under `rewatch/tests/` and are invoked from `suite-ci.sh`.
- Embeds tests use a standalone fixture repo at `rewatch/tests/fixtures/embeds/` and a driver script `rewatch/tests/embeds.sh` that:
  - Produces `.ast` + `*.embeds.json` via `bsc -bs-ast -embeds ...`
  - Compiles sources normally and snapshots the rewritten source printed from the AST.
  - Fails if the snapshot changes and is not staged, consistent with other tests.
- Compiler unit tests (minimal OUnit only where warranted):
  - Pure helpers: naming sanitization, tag normalization, literal hashing.
  - Optional: JSON schema validation for generator protocol.
- Harness commands used in tests:
  - `bsc -bs-ast -embeds <tags> -o <outprefix> <file.res>` → writes `<outprefix>.ast` and `*.embeds.json`.
  - `bsc -only-parse -dsource <out.ast>` or `-dparsetree` → snapshot rewritten AST as source or parsetree.
  - Normal `bsc` compile entry → typecheck and generate JS for full end‑to‑end checks.
 - CI: wire into `make test-rewatch` and keep snapshots stable.

Phase 8 — Documentation & Examples
- Document `embeds` config in `rescript.json`, CLI flags, and generator protocol.
- Provide a minimal example project demonstrating SQL and GraphQL embed flows.
- Call out limitations: no nested embeds, no `.resi` in v1, single literal only.

Acceptance Checklist
- Index files emitted correctly on `-embeds` and are stable across runs.
- Generated files and headers are deterministic; naming policy enforced.
- Embed PPX rewrite is deterministic and only rewrites targeted nodes.
- End‑to‑end build (including watch) works across multi‑package repos.
- Tests cover syntax, compiler passes, Rewatch integration, and watch behavior.
