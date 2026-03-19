---
name: rescript-db
description: Use when working with ReScript (.res, .resi) files and you need accurate type signatures, function definitions, or module contents. Query indexed documentation from @rescript/runtime, @rescript/react, @rescript/webapi, or project packages. Prevents hallucination by looking up real API definitions instead of guessing.
---

# ReScript Database Skill

This skill provides access to indexed ReScript type information. Use it to look up accurate function signatures, type definitions, and module contents instead of guessing.

## When to Use

- Looking up function signatures from ReScript dependencies
- Finding what's available in a ReScript module
- Checking type definitions (records, variants, abstract types)
- Any time you're unsure about ReScript API details

## Prerequisites

The project must have:

1. A `rescript.json` file at the project root
2. The `rescript` compiler installed (provides the `rescript sync` command)

## How the Database is Created

The `rescript sync` command (built into the ReScript compiler) handles everything:

1. Compiles the project
2. Indexes all modules, types, and values from all packages and dependencies
3. Creates `rescript.db` in the project root

## Before Querying: Check Database Status

Before running queries, check if the database exists:

```bash
ls -la rescript.db 2>/dev/null && stat -f "%Sm" rescript.db
```

If `rescript.db` doesn't exist, run a sync first:

```bash
npx rescript sync
```

Use sync for:

- Initial database creation
- After updating dependencies (`bun install` / `npm install`)
- When the database is missing or corrupted

## Query Database

Run from the repo root using `sqlite3`:

```bash
sqlite3 rescript.db "SELECT ..."
```

For JSON output:

```bash
sqlite3 -json rescript.db "SELECT ..."
```

## Database Schema

### Tables Overview

```
packages -> modules -> types   -> fields
                    -> types   -> constructors
                    -> values
                    -> aliases
```

### packages

| Column      | Type    | Description                                                |
| ----------- | ------- | ---------------------------------------------------------- |
| id          | INTEGER | Primary key                                                |
| name        | TEXT    | Package name (e.g., `@rescript/react`, `@rescript/webapi`) |
| path        | TEXT    | Filesystem path                                            |
| config_hash | TEXT    | For change detection                                       |

### modules

| Column             | Type    | Description                                                  |
| ------------------ | ------- | ------------------------------------------------------------ |
| id                 | INTEGER | Primary key                                                  |
| package_id         | INTEGER | FK to packages                                               |
| parent_module_id   | INTEGER | FK to parent module (for nested modules)                     |
| name               | TEXT    | Simple module name                                           |
| qualified_name     | TEXT    | Full path (e.g., `React`, `React.Children`, `DOMAPI-WebAPI`) |
| source_file_path   | TEXT    | Path to .res/.resi file                                      |
| compiled_file_path | TEXT    | Path to .cmi file                                            |
| cmt_hash           | TEXT    | Hash of the .cmt file (for invalidation)                     |
| has_interface      | INTEGER | 1 if the module has a .resi interface file                   |
| is_auto_opened     | INTEGER | 1 if globally available (Stdlib, Pervasives)                 |

**Note on qualified names**: Namespaced modules use hyphen format internally: `DOMAPI-WebAPI`, `FetchAPI-WebAPI`. In ReScript code you write `WebAPI.DOMAPI`. Nested modules use dots: `React.Children`.

### types

| Column    | Type    | Description                      |
| --------- | ------- | -------------------------------- |
| id        | INTEGER | Primary key                      |
| module_id | INTEGER | FK to modules                    |
| name      | TEXT    | Type name (e.g., `t`, `element`) |
| kind      | TEXT    | `record`, `variant`, or `alias`  |
| signature | TEXT    | Full type signature              |

### fields

| Column    | Type    | Description                |
| --------- | ------- | -------------------------- |
| id        | INTEGER | Primary key                |
| type_id   | INTEGER | FK to types (record types) |
| name      | TEXT    | Field name                 |
| signature | TEXT    | Field type signature       |
| optional  | INTEGER | 1 if the field is optional |

### constructors

| Column    | Type    | Description                   |
| --------- | ------- | ----------------------------- |
| id        | INTEGER | Primary key                   |
| type_id   | INTEGER | FK to types (variant types)   |
| name      | TEXT    | Constructor name              |
| signature | TEXT    | Constructor payload signature |

### values

| Column      | Type    | Description                                    |
| ----------- | ------- | ---------------------------------------------- |
| id          | INTEGER | Primary key                                    |
| module_id   | INTEGER | FK to modules                                  |
| name        | TEXT    | Function/value name                            |
| signature   | TEXT    | Full signature (e.g., `(string, int) => bool`) |
| param_count | INTEGER | Number of parameters                           |
| return_type | TEXT    | Return type                                    |

### aliases

| Column                | Type    | Description                  |
| --------------------- | ------- | ---------------------------- |
| source_module_id      | INTEGER | Module containing the alias  |
| alias_name            | TEXT    | The alias name               |
| alias_kind            | TEXT    | `type`, `value`, or `module` |
| target_qualified_name | TEXT    | What it points to            |

### usages

Tracks which project modules reference which dependency symbols. Updated incrementally by the LSP after each build.

| Column           | Type    | Description                                                   |
| ---------------- | ------- | ------------------------------------------------------------- |
| id               | INTEGER | Primary key                                                   |
| source_module_id | INTEGER | FK to modules — the project module containing the usage       |
| target_module_id | INTEGER | FK to modules — the dependency module being referenced         |
| target_path      | TEXT    | Dot-joined path within the module (e.g., `Storage.getItem`)   |
| tip              | TEXT    | Symbol kind: `Value`, `Type`, `Field(name)`, `Constructor(name)`, `Module` |
| source_line      | INTEGER | 0-based line number of the usage in the source file           |
| source_col       | INTEGER | 0-based column of the usage in the source file                |

## Common Query Patterns

### Find a function by name

```sql
SELECT v.name, v.signature, v.param_count, m.qualified_name as module
FROM "values" v
JOIN modules m ON v.module_id = m.id
WHERE v.name = 'useState'
LIMIT 10
```

### Explore a module's contents

```sql
-- Get all values in a module
SELECT name, signature, param_count
FROM "values"
WHERE module_id = (SELECT id FROM modules WHERE qualified_name = 'React')
ORDER BY name

-- Get all types in a module
SELECT name, kind, signature
FROM types
WHERE module_id = (SELECT id FROM modules WHERE qualified_name = 'React')
ORDER BY name
```

### Find record fields

```sql
SELECT f.name, f.signature, f.optional
FROM fields f
JOIN types t ON f.type_id = t.id
JOIN modules m ON t.module_id = m.id
WHERE m.qualified_name = 'FetchAPI-WebAPI'
  AND t.name = 'requestInit'
```

### Find variant constructors

```sql
SELECT c.name, c.signature
FROM constructors c
JOIN types t ON c.type_id = t.id
JOIN modules m ON t.module_id = m.id
WHERE m.qualified_name = 'Stdlib_JSON'
  AND t.name = 't'
```

### Search for modules by pattern

```sql
SELECT m.qualified_name, p.name as package
FROM modules m
JOIN packages p ON m.package_id = p.id
WHERE m.qualified_name LIKE '%Fetch%'
LIMIT 20
```

### Find functions by return type

```sql
SELECT v.name, v.signature, m.qualified_name as module
FROM "values" v
JOIN modules m ON v.module_id = m.id
WHERE v.return_type LIKE '%promise%'
LIMIT 20
```

### Which dependency modules does the project use?

```sql
SELECT tm.qualified_name as dependency, COUNT(*) as usage_count
FROM usages u
JOIN modules tm ON u.target_module_id = tm.id
GROUP BY tm.qualified_name
ORDER BY usage_count DESC
```

### Which project files reference a specific dependency?

```sql
SELECT DISTINCT sm.qualified_name, sm.source_file_path
FROM usages u
JOIN modules sm ON u.source_module_id = sm.id
JOIN modules tm ON u.target_module_id = tm.id
WHERE tm.qualified_name = 'DOMAPI-WebAPI'
```

### What specific symbols from a dependency are used?

```sql
SELECT tm.qualified_name, u.target_path, u.tip, COUNT(*) as times_used
FROM usages u
JOIN modules tm ON u.target_module_id = tm.id
WHERE tm.qualified_name LIKE '%-WebAPI'
GROUP BY tm.qualified_name, u.target_path, u.tip
ORDER BY times_used DESC
LIMIT 20
```

### Check globally available symbols

```sql
SELECT m.qualified_name,
       (SELECT COUNT(*) FROM types WHERE module_id = m.id) as type_count,
       (SELECT COUNT(*) FROM "values" WHERE module_id = m.id) as value_count
FROM modules m
WHERE m.is_auto_opened = 1
```

### List all packages

```sql
SELECT name FROM packages ORDER BY name
```

## Query Strategy

1. **Start broad, then narrow**: First find the right module with a pattern search, then query that specific module for details.

2. **Two-phase lookup**:
   - Phase 1: `SELECT qualified_name FROM modules WHERE qualified_name LIKE '%Search%' LIMIT 10`
   - Phase 2: `SELECT * FROM "values" WHERE module_id = (SELECT id FROM modules WHERE qualified_name = 'ExactModule')`

3. **Quote "values"**: The table name `values` is a SQL keyword — always quote it as `"values"`.

## Common Packages

| Package             | Description                                                                                 |
| ------------------- | ------------------------------------------------------------------------------------------- |
| `@rescript/runtime` | Core types (int, string, array, option, etc.)                                               |
| `@rescript/core`    | Standard library (Array, String, Option, Result) - modules have `Stdlib_` prefix internally |
| `@rescript/react`   | React bindings                                                                              |
| `@rescript/webapi`  | Browser APIs - modules use `-WebAPI` suffix (e.g., `DOMAPI-WebAPI`)                         |

## Important: Verify Before Writing Code

**Never assume you know ReScript APIs.** The bindings often differ from JavaScript:

- Different function names
- Different parameter order
- Labeled arguments where JS uses positional
- Types that don't exist in JS

Always query the database to get the actual signature before writing code.

## Important: Check Module Accessibility

When providing usage examples, always check if the module requires an `open` statement:

```sql
SELECT qualified_name, is_auto_opened FROM modules WHERE qualified_name = 'Global-WebAPI'
```

- `is_auto_opened = 1`: Module is globally available (e.g., `Stdlib`, `Pervasives`)
- `is_auto_opened = 0`: Module requires either:
  - `open ParentModule` (e.g., `open WebAPI` to access `Global.fetch`)
  - Fully qualified access (e.g., `WebAPI.Global.fetch`)
