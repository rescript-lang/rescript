# Embed Lang (Rewatch) — Design Spec

This document proposes “embed lang”, a Rewatch feature that lets users call external code generators from embedded code snippets in ReScript source files, generate ReScript modules, and link them seamlessly into the original source.

## Summary
- Users write an embed expression in `.res` files using a tag and a string literal (backtick or normal quoted), for example:
  - `let query = %sql.one(`/* @name GetUser */ select * from users where id = :id`)
`
  - or `let query = %sql.one("/* @name GetUser */ select * from users where id = :id")`
- The compiler detects these embeds during parsing and records them. Rewrites happen in a dedicated, AST‑only second phase driven by Rewatch (see “Two‑Phase Rewrite”).
- Rewatch invokes user-configured generators based on the recorded embeds, receives ReScript code, and writes generated files with a conventional name (e.g. `SomeFile__embed_sql_one_GetUser.res`, optional `.resi`).
- A dedicated `-rewrite-embeds` compiler entrypoint performs the AST rewrite to `GeneratedModule.default`, using a small resolution map produced by Rewatch.
- Errors from generators are mapped back to original source locations by Rewatch. Caching avoids unnecessary generator runs.

## Goals
- Support user-defined generators that “claim” one or more embed tags.
- Provide a stable file/module naming convention for generated modules.
- Seamlessly link generated modules in place of the embed expression without changing user code on disk.
- Map generator diagnostics to user source locations so they appear in editors.
- Add caching and invalidation driven by the embed content and additional watched sources (e.g. schema files).
- Integrate cleanly with Rewatch’s parse/compile/watch pipeline.

## Non‑Goals (Initial Version)
- Changing the ReScript parser or reserving new keywords.
- Supporting multi-file generation from a single embed (future extension).
- Providing a long-lived generator “server mode” (future optimization).

## Syntax & Semantics
- Embed expression grammar:
  - `%<tag>(<string-literal>)`
  - `%<tag>.<subtag>(<string-literal>)`
  - The `<string-literal>` can be a backtick string or a normal quoted string, but must be a single literal (no concatenation, pipelines, or computed expressions). Interpolation is not allowed.
  - Examples: `%sql.one(`...`)`, `%graphql.query("...")`
- The embed expression evaluates to the value exported by the generated module’s entry binding, which is always `default`.
- The embedded string may contain metadata comments (e.g. `/* @name GetUser */`) consumed by the generator. The compiler does not interpret these beyond discovery.

Syntax support notes:
- Tags may contain dots in their names (e.g. `sql.one`); the parser accepts dotted extension names in both expression and module positions.
- Only expression and module‑expression contexts are supported in v1 (see “Rewrite semantics”). Embeds cannot appear in pattern, type, or other unsupported positions.

Rewrite semantics:
- Value expression context:
  - `%tag(...): expr` → `GeneratedModule.default`
- Module expression context:
  - `module X = %tag(...)` → `module X = GeneratedModule`
  - `include %tag(...)` → `include GeneratedModule`

## File & Module Naming
- Generated filename: `<SourceModule>__embed_<tagNormalized>_<suffix>.res`
  - `tagNormalized` = tag with non‑alphanumeric chars replaced by `_` (e.g. `sql.one` → `sql_one`).
  - `suffix` = provided by generator output (preferred), else a stable fallback derived from either an explicit `@name` found by the generator or the 1‑based index of this tag occurrence in the source file (e.g. `_1`, `_2`).
  - Module name is derived from filename as usual (`SomeFile__embed_sql_one_GetUser`).
  
The compiler rewrites the embed expression to `SomeFile__embed_sql_one_<suffix>.default` (see Compiler Integration).

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
  "embedString": "/* @name GetUser */ select * from users where id = :id",
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
  "code": "let query = \"select * from users where id = $1\"\n type params = {...}\n let default = ...\n",
  "suffix": "GetUser"            // optional; must be sanitized by Rewatch
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
- Rewatch sanitizes `suffix` to `[A-Za-z0-9_]+`; collisions are handled as errors per file (see Suffix & Collision Policy).
- Generators cannot control the entry binding; the compiler always expects `default`.

## Build & Watch Flow (High‑Level)
1. Compiler Embed Index (pass 1)
   - During parsing, the compiler records all embed occurrences (tag, literal content, precise ranges, occurrence index, and context: expression vs module expression vs include) and writes a per‑module artifact next to the `.ast` file, e.g. `SomeFile.embeds.json`.
   - Index emission is controlled by a new `-embeds <csv>` flag. The timing mirrors the approach in PR #6823: emit immediately after parsing (before type‑checking and heavy transforms), alongside the binary AST output, so that Rewatch never needs to re‑parse sources.
   - This artifact is the single source of truth for Rewatch to know which embeds exist, without Rewatch re‑parsing sources.
2. Caching Check
   - For each embed in the index, compute an embed hash `H = hash(specVersion + generator.id + tag + embedString)`.
   - For per‑generator `extraSources`, use mtime‑based invalidation by default (content hashes optional if needed).
   - If a generated module exists with matching header metadata (see “Generated File Format”), skip generation.
3. Generation
   - If cache miss or invalid, invoke the generator and capture output.
   - On `status=ok`, write/overwrite the generated `.res` file to `outDir` (default `src/__generated__`) with the conventional name.
   - On `status=error`, collect diagnostics mapped to the original source positions (see “Diagnostics & Mapping”).
4. Rewrite Stage (AST‑Only, Two‑Phase)
   - For each source module, Rewatch writes a resolution map artifact (e.g. `SomeFile.embeds.map.json`) that lists, for each embed occurrence, the target generated module name (e.g., `SomeFile__embed_sql_one_GetUser`). Entry is always `default` for expression contexts.
   - Rewatch invokes a dedicated compiler entrypoint that only:
     - Reads the input `.ast` file (`-ast <in.ast>`) and the explicit resolution map path (`-map <SomeFile.embeds.map.json>`).
     - Runs a small, isolated AST mapper that performs only the embed rewrites:
       - Expression contexts: `%tag(...)` → `GeneratedModule.default`
       - Module contexts: `module X = %tag(...)` → `module X = GeneratedModule`
       - Include contexts: `include %tag(...)` → `include GeneratedModule`
     - Writes the rewritten AST to `-o <out.ast>` (or in‑place if `-o` is omitted).
   - Files without embeds skip this stage entirely.
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
- `-rewrite-embeds -ast <in.ast> -map <SomeFile.embeds.map.json> [-o <out.ast>]`
  - Runs a minimal AST‑only rewriter that applies the resolution map, replacing only recognized embed nodes.
  - `-map` is explicit (no implicit discovery). This is idiomatic in ReScript’s tooling: callers (Rewatch) compute and pass exact paths to avoid ambiguity across multi‑package workspaces.
  - If `-o` is omitted, rewriting may happen in place.
  - No type checking or further transforms occur in this mode.

## Artifact Filenames
- Per module (next to `.ast`):
  - Index: `SomeFile.embeds.json`
  - Resolution map: `SomeFile.embeds.map.json`

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
      "embedString": "/* @name GetUser */ select * from users where id = :id",
      "literalHash": "<hex>"                 // hash(tag + embedString)
    }
  ]
}
```

## Cross‑Platform Paths
- All paths written to artifacts (`*.embeds.json`, `*.embeds.map.json`) use `/` as the separator and are project‑relative where possible.
- Rewatch normalizes paths when computing hashes and comparing cache keys to avoid Windows vs POSIX discrepancies.

Resolution map lookup:
- Rewatch computes the exact resolution map path (next to the corresponding `.ast`) and passes it explicitly via `-map`. The compiler does not search for the map implicitly; this avoids ambiguity and keeps the interface explicit and reproducible.

- `SomeFile.embeds.map.json` (resolution map; written by Rewatch after generation):
```
{
  "version": 1,
  "module": "SomeFile",
  "entries": [
    {
      "tag": "sql.one",
      "occurrenceIndex": 1,
      "literalHash": "<hex>",                 // must match index; used to validate mapping
      "targetModule": "SomeFile__embed_sql_one_GetUser"
    }
  ]
}
```

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
- Generator diagnostics are returned relative to the embedded string (line/column within the literal). Rewatch computes the absolute source positions using the ranges from the compiler’s embed index.
- The compiler handles PPX rewrites directly on the AST; diagnostics from the compiler refer to the original source files.
- Error presentation: Rewatch includes a code fence in logs with the embedded code, highlights the error span, and shows surrounding context for quick inspection (similar to compiler formatting).

## Invalidation & Caching
- Cache key includes:
  - `tag`, `embedString` content, generator `id`, generator command string/version, embed spec version. Embed string is content‑hashed; per‑generator `extraSources` use mtime by default.
- Quick check reads only the generated file’s header to confirm hash equality; if mismatch, regenerate.
- Rewatch may persist a small cache index to memoize `extraSources` mtimes for performance.

## Edge Cases & Errors
- Unknown tag: error with code `EMBED_NO_GENERATOR` listing known tags.
- Missing/invalid string literal: error `EMBED_SYNTAX` with a short hint.
- Generator timeout/crash: error `EMBED_GENERATOR_FAILED` with stderr summary.
- Suffix collision: error (`EMBED_SUFFIX_COLLISION`) with both locations.
- Resolution map mismatch: error (`EMBED_MAP_MISMATCH`) when `literalHash` in the map does not match the current embed string; triggers regeneration.
- Illegal suffix chars: sanitized to `_`; collapse repeats.
- `.resi` generation: not supported in v1; the generated module is compiled without an interface.
- Nested embeds: disallowed. Generated files are ignored by the compiler’s embed indexer and never expanded.

## Suffix & Collision Policy
- Generators may supply a custom `suffix`. After sanitization, Rewatch enforces uniqueness per source file and tag for a given build.
- If two embeds in the same source file and tag resolve to the same `suffix`, Rewatch reports `EMBED_SUFFIX_COLLISION` with both locations. Default policy is to error (no overwrite) for determinism.
- If `suffix` is omitted, Rewatch uses a stable numeric fallback: `_1`, `_2`, ... in appearance order for that tag in the file.
- Cross-file collisions are avoided by including the source module name in the generated filename (e.g., `SomeFile__embed_sql_one_<suffix>.res`).

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
 - Rewrite stage is an AST‑only pass that reads `.ast` + `*.embeds.map.json` and performs a single traversal. Overhead is small vs type checking and codegen.

## Testing Plan
- Compiler unit: embed indexer collects tags for both backtick and normal string literals; ignores generated outDir; occurrence indices stability.
- Rewatch unit: suffix sanitization; resolution map writer/reader; mtime vs content hash behavior for extraSources.
- Integration (rewatch/tests):
  - Happy path: create a small generator that returns code; ensure generated file(s) are created and linked; build succeeds.
  - Cache hit/miss: modify embed string and extra sources; ensure regeneration occurs only when needed.
  - Errors: generator returns diagnostics; verify mapping to original file positions and code‑fenced logs.
  - Watch: change extra source; verify incremental rebuild of affected modules and cleanup of unused files.

## Future Extensions
- Long‑lived generator server with handshake to claim tags and avoid per‑embed process cost.
- Multiple files per embed (e.g. helper modules), richer emission APIs.
- Richer mapping: embed‑specific source maps and IDE hovers with generator metadata.
- Inline rewrite during initial parse when a valid resolution map is already available (skip separate rewrite stage); only if validation remains trivial and robust.

## Open Questions
1. Embed index and resolution map formats
   - JSON vs compact binary; stability/versioning. (Timing is specified: emit index right after parse, rewrite as a distinct pass.)
2. Naming collisions across files
   - If two files produce the same `<suffix>`, we’re safe because the filename also includes the source module; confirm no package‑level namespace issues.
3. Diagnostics severity mapping
   - Exact mapping to ReScript’s error/warning display conventions?

---

If this plan looks good, next steps would be:
- Confirm grammar (string literal only; no interpolation) and config shape.
- Compiler: add embed indexing during parse and emit `*.embeds.json` artifacts next to `*.ast`.
- Rewatch: read embed index, implement generator invocation + caching + mtime watching, write generated files and `*.embeds.map.json` resolution maps.
- Compiler: add the dedicated `-rewrite-embeds` pass that reads `-ast` and `-map` and rewrites embeds into references to generated modules.
- Thread dependency info through Rewatch’s `BuildState`; wire cleanup of stale generated files.
- Add integration tests (happy path, caching, errors with code fences, watch, cleanup).

## Step‑By‑Step Implementation Plan

Phase 0 — Wiring and Flags
- Define CLI flag `-embeds <csv|all>` in `bsc` (parser phase only).
- Define CLI entry `-rewrite-embeds -ast <in.ast> -map <map.json> [-o <out.ast>]`.
- Plumb flags through `compiler/bsc/rescript_compiler_main.ml` and ensure they are mutually orthogonal to existing flags (no impact on `-bs-no-builtin-ppx`).

Phase 1 — Compiler: Embed Indexing (after parse)
- Add a lightweight AST walker to collect embeds:
  - Expression: `Pexp_extension (name, payload)` where `name` matches configured tags.
  - Module expr: `Pmod_extension ...` and `Pstr_include` forms for include contexts.
  - Only accept a single string literal argument (backtick or quoted). Otherwise, record an `EMBED_SYNTAX` error location.
- Emit `SomeFile.embeds.json` next to `.ast` when `-embeds` is present:
  - Fields: version, module, sourcePath (project‑relative), embeds[] with tag, context, occurrenceIndex (1‑based per‑tag), range, embedString, literalHash.
  - Use `/` path separators for portability.
- Exclude generated outDir from indexing (by path prefix and by reading the generated header marker if present) to prevent nested embeds.
- Implementation points:
  - Hook immediately after parse and before any heavy transforms (mirroring PR #6823 pattern used for early artifacts).
  - Ensure binary AST emission remains unchanged.

Phase 2 — Rewatch: Parse Step and Tag Discovery
- Compute the set of tags to index from `rescript.json` `embeds.generators[].tags`.
- During AST generation (`build/parse.rs`), add `-embeds <csv>` to the `bsc -bs-ast` invocation for modules in packages that configure embeds.
- Confirm index files are written and co‑located with `.ast` files; add error handling if missing when embeds are configured.

Phase 3 — Rewatch: Generator Invocation & Caching
- Read `SomeFile.embeds.json` and group embeds by generator (tag → generator.id).
- For each embed:
  - Compute cache key `H = hash(specVersion + generator.id + tag + embedString)`.
  - Check existing generated file header for a quick hash match; also check per‑generator `extraSources` mtimes.
  - On miss or invalidation, spawn the generator process with the JSON protocol over stdin/stdout; enforce `timeoutMs`.
  - Validate response: sanitize `suffix`, ensure `entry` is `default`, normalize paths, collect diagnostics.
  - Write generated `*.res` (and header) to `outDir` using naming scheme `<SourceModule>__embed_<tagNormalized>_<suffix>.res`.
  - Enforce suffix uniqueness per source+tag; on collision, raise `EMBED_SUFFIX_COLLISION` with both locations.
- Concurrency: cap concurrent processes to `max(1, num_cpus/2)`.
- Maintain a cache index for `extraSources` mtimes to avoid repeated stat calls.

Phase 4 — Rewatch: Resolution Map Writer
- For each source module with embeds, write `SomeFile.embeds.map.json` next to `.ast`:
  - Fields: version, module, entries[] with tag, occurrenceIndex, literalHash, targetModule.
  - Always target `default` for expression contexts; module/include target the module itself.
- Ensure `literalHash` in map matches the current index; if mismatch during rewrite, surface `EMBED_MAP_MISMATCH`.

Phase 5 — Compiler: AST‑Only Rewrite Pass
- Implement a minimal rewriter that:
  - Reads `-ast` (binary AST) and `-map` (JSON), builds a lookup by (tag, occurrenceIndex) and validates `literalHash`.
  - Traverses AST and replaces only recognized nodes:
    - `%tag("...")` (expr) → `GeneratedModule.default`.
    - `module X = %tag("...")` → `module X = GeneratedModule`.
    - `include %tag("...")` → `include GeneratedModule`.
  - Writes AST to `-o` (or in‑place if omitted).
- Do not perform JSX or builtin PPX here; keep this pass surgical and idempotent.

Phase 6 — Rewatch: Pipeline Integration
- After AST generation and generation/map writing, invoke `bsc -rewrite-embeds` per module that has an index.
- Feed the (possibly rewritten) `.ast` into the normal compile path (typecheck, lambda, JS) unchanged.
- Extend dependency graph:
  - `OriginalFile → GeneratedModule(s)` and `GeneratedModule → extraSources`.
  - Treat generated files as regular sources for ordering; do not index embeds within them.

Phase 7 — Watch Mode & Cleanup
- Watch original `.res`, generated `outDir`, and `extraSources`.
- On changes, invalidate affected embeds, re‑run generation and rewrite only for impacted modules, and rebuild dependents.
- Cleanup: compute expected generated files per source; remove stale files and clear cache entries when embeds are removed or sources deleted.

Phase 8 — Errors & Diagnostics
- Map generator diagnostics (literal‑relative positions) to absolute source spans via the index ranges; print rich code frames.
- Error codes: `EMBED_NO_GENERATOR`, `EMBED_SYNTAX`, `EMBED_GENERATOR_FAILED`, `EMBED_SUFFIX_COLLISION`, `EMBED_MAP_MISMATCH`.
- Align severity with compiler conventions; ensure non‑zero exit on errors to integrate with CI.

Phase 9 — Testing
- Compiler unit tests (ounit):
  - Indexer: dotted tags, both string literal kinds, expr/module/include contexts, occurrenceIndex stability, outDir exclusion.
  - Rewriter: given AST+map, verify node replacements and validation errors.
- Rewatch unit/integration:
  - Happy path: generator returns code; files created; map written; build succeeds.
  - Caching: modify embed string and extra sources; verify regeneration as expected.
  - Errors: timeouts, non‑zero exit, diagnostics mapping to original source.
  - Watch: change extra source; only affected modules rebuild; stale files cleaned.
- Wire into `make test`, `make test-rewatch`, and add a small sample generator used only in tests.

Phase 10 — Documentation & Examples
- Document `embeds` config in `rescript.json`, CLI flags, and generator protocol.
- Provide a minimal example project demonstrating SQL and GraphQL embed flows.
- Call out limitations: no nested embeds, no `.resi` in v1, single literal only.

Acceptance Checklist
- Index files emitted correctly on `-embeds` and are stable across runs.
- Generated files and headers are deterministic; suffix policy enforced.
- `-rewrite-embeds` pass is idempotent and only rewrites targeted nodes.
- End‑to‑end build (including watch) works across multi‑package repos.
- Tests cover syntax, compiler passes, Rewatch integration, and watch behavior.
