# LLM Index — Architecture Document

## Problem

LLM agents (Claude Code, Cursor, etc.) working on ReScript projects need access to type information — function signatures, module contents, type definitions — to write correct code. They cannot call LSP requests directly from the editor, so they need an alternative way to query this information.

The current solution is a Claude Code skill that:

1. Hooks into `js-post-build` to run a Python script after each file compiles
2. The script calls `rescript-tools doc` (subprocess) to extract type info from `.cmi`/`.cmt` files
3. Writes the results into a SQLite database (`rescript.db`)
4. LLMs query the database via `sqlite3 rescript.db "SELECT ..."`

This works but has significant friction:

- Requires Python (`uv`) as a runtime dependency
- Requires `js-post-build` hook configuration in every `rescript.json`
- Concurrent `js-post-build` invocations cause write contention (Python's `sqlite3.connect(timeout=30)` is the workaround)
- Spawns a `rescript-tools doc` subprocess per file, per compile
- The sync/update/discovery logic duplicates knowledge the compiler already has (package resolution, source directories, module graph)

## Goal

Move the index generation into the `rescript lsp` server so that the database stays in sync automatically, with zero user configuration. The skill simplifies to just the query layer.

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│  rescript lsp                                            │
│                                                          │
│  ┌──────────────┐  ┌──────────────┐  ┌───────────────┐  │
│  │ Build Engine  │  │ LSP Protocol │  │ LLM Index     │  │
│  │ (rewatch)     │  │ (tower-lsp)  │  │ Writer        │  │
│  │               │  │              │  │               │  │
│  │ Knows:        │  │              │  │ After build:  │  │
│  │ - All modules │  │              │  │ 1. Identify   │  │
│  │ - Dep graph   │  │              │  │    changed    │  │
│  │ - cmi/cmt     │  │              │  │    modules    │  │
│  │   paths       │  │              │  │ 2. Call        │  │
│  │               │  │              │  │    analysis   │  │
│  │               │  │              │  │    binary     │  │
│  │               │  │              │  │    (batch)    │  │
│  │               │  │              │  │ 3. Write      │  │
│  │               │  │              │  │    SQLite     │  │
│  └──────┬────────┘  └──────────────┘  └───────┬───────┘  │
│         │                                     │          │
│         └──────── triggers ───────────────────┘          │
└─────────────────────────────────────────────────────────┘
                        │
                        ▼
                   rescript.db  ◄──── sqlite3 queries from LLM agents
```

### Components

**Analysis binary — new `docIndex` subcommand**

A new subcommand for `rescript-editor-analysis` that processes multiple modules in a single invocation and outputs JSON tailored for direct SQLite insertion. This follows the same pattern as existing analysis subcommands: the Rust side sends a JSON blob on stdin, the OCaml binary processes it and writes JSON to stdout.

Input (JSON via stdin):

```json
{
  "files": [
    { "cmt": "/path/to/lib/lsp/src/App.cmt", "cmi": "/path/to/lib/ocaml/App.cmi" },
    { "cmti": "/path/to/lib/lsp/src/Types.cmti", "cmi": "/path/to/lib/ocaml/Types.cmi" }
  ],
  "runtimePath": "/path/to/node_modules/@rescript/runtime"
}
```

Output (JSON on stdout) — structured to match the database schema directly, not the generic `rescript-tools doc` format. The Rust side should be able to iterate over this and insert rows without reshaping:

```json
[
  {
    "moduleName": "App",
    "qualifiedName": "App",
    "sourceFilePath": "src/App.res",
    "types": [
      { "name": "state", "kind": "record", "signature": "type state = {count: int}", "detail": "{\"items\":[{\"name\":\"count\",\"signature\":\"int\"}]}" }
    ],
    "values": [
      { "name": "make", "signature": "(~title: string) => React.element", "paramCount": 1, "returnType": "React.element" }
    ],
    "aliases": [],
    "nestedModules": [
      {
        "moduleName": "Inner",
        "qualifiedName": "App.Inner",
        "types": [],
        "values": [],
        "aliases": [],
        "nestedModules": []
      }
    ]
  }
]
```

Key design choices for the output format:

- `detail` is pre-serialized as a JSON string (not a nested object) — the Rust side stores it as-is in SQLite without re-serializing
- `paramCount` and `returnType` are computed by the OCaml side (it has the typed tree, it can do this accurately rather than regex-counting `=>`)
- `sourceFilePath` is relative to the package root — the Rust side has the package path and can make it absolute for the database
- Nested modules are inline — the Rust side handles `parent_module_id` assignment during insertion

The Rust side provides the `.cmt`/`.cmti` paths (it knows these from `BuildCommandState`) and the `.cmi` paths (for hash-based invalidation). The analysis binary reads the `.cmt`/`.cmti` to extract type information.

**LLM Index Writer (Rust, in rewatch)**

A new module in `rewatch/src/` responsible for:

- Building the stdin JSON from `BuildCommandState` (it already knows all module paths, package paths, etc.)
- Spawning the analysis binary once with `["rewatch", "docIndex"]` and parsing the stdout JSON
- Owning the SQLite connection (single writer, no contention)
- Inserting rows directly from the output format — no reshaping needed
- Tracking `.cmi` hashes to skip unchanged modules (hash computed on the Rust side before calling the analysis binary, so unchanged modules are never sent)

The writer does not need to resolve packages or discover source files — the `BuildCommandState` already has this information.

**Database file location**

One `rescript.db` per workspace root, not per project root. This maps to the `ProjectMap.states: HashMap<PathBuf, BuildCommandState>` structure in the LSP — a single database can contain modules from multiple project roots (monorepo case, or multiple folders open in the editor).

For the `rescript lsp` case, the database lives alongside the workspace. For the CLI case (`rescript db sync`), it lives at the project root.

### Trigger Points

**Initial sync (on LSP startup)**

After `initial_build()` completes and the `BuildCommandState` is populated:

1. Spawn a background task (non-blocking — the LSP should be responsive immediately)
2. Enumerate all `.cmi` files across all project roots in `ProjectMap.states`
3. Call the analysis binary batch subcommand
4. Write everything to `rescript.db`
5. This includes dependencies (`@rescript/react`, `@rescript/webapi`, etc.)

**Incremental update (after queue flush)**

After the queue consumer finishes a flush cycle (builds + typechecks):

1. Identify which modules were recompiled (the build engine already tracks this)
2. For changed modules, call the analysis binary to extract updated docs
3. Upsert into `rescript.db`

Dependencies don't change during normal editing, so incremental updates only cover project modules.

**CLI sync (`rescript sync`) — start here**

A standalone subcommand that builds the project and writes `rescript.db`. This is the first thing to implement because it exercises the analysis binary subcommand + SQLite writer end-to-end without any async/LSP complexity.

Usage:

```bash
rescript sync              # build + index, writes rescript.db in project root
rescript sync --folder ./packages/app  # monorepo: specify project root
```

What it does:

1. Run `build::build()` (same as `rescript build`) to get a `BuildState` with all modules compiled
2. Enumerate all modules from `BuildState.modules` + dependency packages + runtime
3. For each module, compute `.cmi` hash and collect `.cmt`/`.cmti` paths
4. Call the analysis binary once: `rescript-editor-analysis.exe rewatch docIndex` with the file list on stdin
5. Create/open `rescript.db`, apply schema DDL
6. Insert all rows from the analysis output
7. Mark auto-opened modules (`Stdlib`, `Pervasives`, and `-open` flags from compiler config)

Implementation touches:

- `rewatch/src/cli.rs` — add `Sync` variant to `Command` enum with a `FolderArg`
- `rewatch/src/main.rs` — add `cli::Command::Sync { folder } => run_sync(&folder)` match arm
- `rewatch/src/llm_index.rs` (new) — the SQLite writer module: schema DDL, insert logic, hash tracking
- `analysis/bin/main.ml` — add `| ["llmIndex"] -> CommandsRewatch.llmIndex ()` to the rewatch dispatch
- `analysis/src/LlmIndex.ml` (new) — `llmIndex` handler that reads file list from stdin, processes each `.cmt`/`.cmti`, outputs the schema-tailored JSON

### Trying the `llmIndex` subcommand

After building the project (`make lib`), you can test the analysis binary's `llmIndex` subcommand directly:

```bash
# First, build a ReScript project so .cmt files exist
cd /path/to/your/rescript-project
rescript build

# Craft the stdin JSON and pipe it to the analysis binary
cat <<'EOF' | rescript-editor-analysis.exe rewatch llmIndex
{
  "rootPath": "/path/to/your/rescript-project",
  "namespace": null,
  "suffix": ".mjs",
  "rescriptVersion": [13, 0],
  "genericJsxModule": null,
  "opens": [],
  "pathsForModule": {
    "MyModule": {
      "impl": {
        "cmt": "/path/to/your/rescript-project/lib/bs/src/MyModule.cmt",
        "res": "/path/to/your/rescript-project/lib/bs/src/MyModule.res"
      }
    }
  },
  "projectFiles": ["MyModule"],
  "dependenciesFiles": [],
  "files": [
    { "moduleName": "MyModule", "cmt": "/path/to/your/rescript-project/lib/bs/src/MyModule.cmt", "cmti": "" }
  ]
}
EOF
```

```bash
cat <<'EOF' | /Users/nojaf/Projects/rescript/packages/@rescript/darwin-arm64/bin/rescript-editor-analysis.exe rewatch llmIndex
{
  "rootPath": "/Users/nojaf/Projects/relocation",
  "namespace": null,
  "suffix": ".res.mjs",
  "rescriptVersion": [13, 0],
  "genericJsxModule": null,
  "opens": [],
  "pathsForModule": {
    "App": {
      "impl": {
        "cmt": "/Users/nojaf/Projects/relocation/lib/bs/src/App.cmt",
        "res": "/Users/nojaf/Projects/relocation/lib/bs/src/App.res"
      }
    }
  },
  "projectFiles": ["App"],
  "dependenciesFiles": [],
  "files": [
    { "moduleName": "App", "cmt": "/Users/nojaf/Projects/relocation/lib/bs/src/App.cmt", "cmti": "" }
  ]
}
EOF
```

The output is a JSON array of module objects with `records`, `variants`, `typeAliases`, `values`, `moduleAliases`, and `nestedModules`.

### Database Schema

Same schema as the current skill, proven to work well for LLM queries:

```sql
packages (id, name, path, rescript_json, config_hash)
modules  (id, package_id, parent_module_id, name, qualified_name,
          source_file_path, compiled_file_path, file_hash, is_auto_opened)
types    (id, module_id, name, kind, signature, detail)
"values" (id, module_id, name, return_type, param_count, signature, detail)
aliases  (id, source_module_id, alias_name, alias_kind, target_qualified_name, docstrings)
```

Key indexes: `qualified_name`, `compiled_file_path`, `is_auto_opened`, `alias_name`.

Hash-based invalidation: `modules.file_hash` stores the SHA-256 of the `.cmi` file. On incremental update, skip modules whose hash hasn't changed.

### What the Skill Becomes

The skill reduces to:

- `SKILL.md` with the schema documentation and query patterns
- LLMs query directly: `sqlite3 rescript.db "SELECT ..."`

No Python, no `uv`, no `js-post-build` hook, no sync/update scripts.

## Key Files

### Rust side (rewatch)

| File | What's there | Relevance |
|------|-------------|-----------|
| `rewatch/src/lsp.rs` | `Backend` struct, `LanguageServer` impl, `ProjectMap` (maps project roots → `BuildCommandState`) | Top-level LSP orchestration. `ProjectMap.states` is the source of truth for all modules/packages. `initial_build()` (line 762) and queue startup (line 343) are the trigger points. |
| `rewatch/src/lsp/analysis.rs` | `AnalysisContext`, `build_context_json()`, `spawn()` | Pattern to follow: builds JSON context from `BuildCommandState`, sends via stdin to analysis binary, parses stdout. The new `docIndex` subcommand follows this same pattern. |
| `rewatch/src/lsp/queue.rs` | Unified debounced queue, `flush_inner()` (line 522) | After flush completes (builds + typechecks), this is where incremental index updates would be triggered. The `buildFinished` notification (line 681) marks the natural hook point. |
| `rewatch/src/lsp/queue/file_build.rs` | Per-file incremental build | Knows which modules were recompiled — needed to identify what to re-index. |
| `rewatch/src/build/build_types.rs` | `BuildCommandState` (line 666), `BuildState` (line 647), `Module` enum (line 572), `SourceFileModule` (line 464) | Core types. `BuildState.modules: HashMap<String, Module>` contains all modules with their paths, deps, and compilation stage. |
| `rewatch/src/cli.rs` | CLI entry point, `Command` enum (line 388) | Where to add a `rescript db sync` subcommand. |
| `rewatch/src/lsp/initial_build.rs` | Full `TypecheckOnly` build on startup | Runs before the queue starts. After this completes, the initial index sync would begin as a background task. |

### OCaml side (analysis binary)

| File | What's there | Relevance |
|------|-------------|-----------|
| `analysis/bin/main.ml` | CLI dispatch, `rewatch` subcommand routing (line 135) | Where to add the `"docIndex"` match arm: `\| ["docIndex"] -> CommandsRewatch.docIndex ()` |
| `analysis/src/CommandsRewatch.ml` | `withRewatchContext` (line 145), all rewatch subcommand handlers | Pattern to follow: reads JSON from stdin via `withRewatchContext`, calls into analysis logic, prints JSON to stdout. The new `docIndex` handler goes here. |
| `analysis/src/DocumentSymbol.ml` | `command ~path ~source` — extracts symbols from a single file | Existing per-file symbol extraction. The `docIndex` implementation may reuse some of this logic but needs a different output shape. |
| `tools/src/tools.ml` | `extractDocs` (line 421) — the function behind `rescript-tools doc` | This is what the current Python skill calls. Produces the generic doc JSON. The new `docIndex` subcommand replaces this with a schema-tailored output. |
| `tools/bin/main.ml` | `rescript-tools` CLI, `"doc"` command (line 60) | Reference for how `extractDocs` is invoked today. Not modified by this work. |

### Current skill (reference implementation to replace)

| File | What's there | Relevance |
|------|-------------|-----------|
| `../relocation/.claude/skills/rescript/scripts/rescript-db.py` | Python sync/update/query CLI | The logic being replaced. Useful as reference for: schema DDL, `parse_module_documentation()` (the JSON→rows mapping), hash-based invalidation, auto-opened module detection. |
| `../relocation/.claude/skills/rescript/SKILL.md` | Skill documentation, schema docs, query patterns | The query patterns and schema documentation survive as-is. The sync/update sections go away. |

## Decisions Made

- **Analysis binary input format**: JSON via stdin, consistent with how all other analysis subcommands work (see `analysis.rs` — `build_context_json` + `spawn()`).
- **Output format**: Tailored for SQLite insertion, not the generic `rescript-tools doc` shape. The OCaml side computes `paramCount`/`returnType` accurately from the typed tree. `detail` is pre-serialized as a JSON string.

## Open Questions

- **Database path configuration**: Should the LSP accept an initialization option for the database path, or always use a fixed location relative to the workspace/project root?
- **Dependency indexing frequency**: Dependencies only change on `bun install` / package updates. Should we track a hash of `node_modules` state to know when to re-index deps, or just re-index them on every full sync?
- **WAL mode and readers**: SQLite WAL mode allows concurrent reads while the LSP writes. Do we need any additional coordination, or is WAL sufficient?
- **Multi-root workspaces**: When multiple project roots exist in `ProjectMap.states`, should the database include a `project_root` column to disambiguate, or is the `packages` table sufficient?
- **Auto-opened modules**: The current skill detects these from compiler flags (`-open`) and hardcodes `Stdlib`/`Pervasives` for `@rescript/runtime`. Should the analysis binary report `is_auto_opened` per module, or should the Rust side keep this logic?
