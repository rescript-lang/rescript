---
name: rescript-lookup
description: Use when you need to look up type information, find definitions, or explore symbols in a ReScript project via the LSP's HTTP endpoints. Provides hover info, go-to-definition, find-references, document symbols, and workspace-wide symbol search — all via curl.
---

# ReScript Lookup

This skill provides access to LSP intelligence via HTTP. Use it to look up types, find definitions, discover references, and search symbols across the project.

## When to Use

- You need to know the type of a specific symbol at a position in a file
- You want to find where a function or type is defined
- You want to find all usages of a symbol
- You want to see the outline of a file (all top-level declarations)
- You want to search for a symbol by name across the whole project

## Prerequisites

The editor must be running with the ReScript LSP server and `diagnostics_http` configured. The project must have been built at least once (`.cmt` files must exist).

To check if the server is ready:

```bash
curl -s http://127.0.0.1:{{PORT}}/diagnostics -o /dev/null -w '%{http_code}'
```

## Endpoints

All endpoints use **0-based** line and column numbers (matching LSP convention).

### Hover — Get Type Information

```bash
curl -s 'http://127.0.0.1:{{PORT}}/hover?file={{ABSOLUTE_PATH}}&line=0&col=4' | jq .
```

Returns the type signature and documentation for the symbol at the given position.

**Example response:**

```json
{
  "contents": {
    "kind": "markdown",
    "value": "```rescript\nstring\n```"
  },
  "range": {
    "start": { "line": 0, "character": 4 },
    "end": { "line": 0, "character": 8 }
  }
}
```

Returns 404 if no hover information is available at that position.

### Definition — Find Where Something Is Defined

```bash
curl -s 'http://127.0.0.1:{{PORT}}/definition?file={{ABSOLUTE_PATH}}&line=0&col=15' | jq .
```

Returns the file and position where the symbol is defined.

**Example response:**

```json
{
  "uri": "file:///absolute/path/to/src/App.res",
  "range": {
    "start": { "line": 0, "character": 4 },
    "end": { "line": 0, "character": 7 }
  }
}
```

Returns 404 if no definition is found.

### References — Find All Usages

```bash
curl -s 'http://127.0.0.1:{{PORT}}/references?file={{ABSOLUTE_PATH}}&line=0&col=4' | jq .
```

Returns an array of all locations where the symbol is referenced.

**Example response:**

```json
[
  {
    "uri": "file:///path/to/src/Root.res",
    "range": {
      "start": { "line": 0, "character": 15 },
      "end": { "line": 0, "character": 18 }
    }
  }
]
```

Returns an empty array `[]` if no references are found.

### Document Symbols — File Outline

```bash
curl -s 'http://127.0.0.1:{{PORT}}/document-symbols?file={{ABSOLUTE_PATH}}' | jq .
```

Returns a nested tree of all symbols (functions, types, modules) in the file.

Returns an empty array `[]` if no symbols are found.

### Workspace Symbols — Search Across Project

```bash
curl -s 'http://127.0.0.1:{{PORT}}/workspace-symbols?query=useState' | jq .
```

Returns matching symbols from all files in the project. Useful for discovering where something is defined without knowing the file.

**Example response:**

```json
[
  {
    "name": "useState",
    "kind": 12,
    "location": {
      "uri": "file:///path/to/node_modules/@rescript/react/src/React.res",
      "range": { "start": { "line": 10, "character": 0 }, "end": { "line": 10, "character": 20 } }
    },
    "containerName": "React"
  }
]
```

Returns an empty array `[]` if no matches are found.

## Error Responses

All endpoints return JSON errors with appropriate HTTP status codes:

- **400** — Missing or invalid parameters: `{"error": "missing required query parameter: file"}`
- **404** — File not readable or handler returned no result: `{"error": "no hover information available"}`

## Workflow Examples

### Understanding a symbol you're about to modify

```bash
# 1. What type is it?
curl -s 'http://127.0.0.1:{{PORT}}/hover?file=/path/to/src/App.res&line=5&col=10' | jq .contents.value

# 2. Where is it defined?
curl -s 'http://127.0.0.1:{{PORT}}/definition?file=/path/to/src/App.res&line=5&col=10' | jq .uri

# 3. Who else uses it? (to understand impact of changes)
curl -s 'http://127.0.0.1:{{PORT}}/references?file=/path/to/src/App.res&line=5&col=10' | jq '.[].uri'
```

### Exploring a file before editing

```bash
# Get the outline to understand structure
curl -s 'http://127.0.0.1:{{PORT}}/document-symbols?file=/path/to/src/App.res' | jq '.[].name'
```

### Finding a function you need

```bash
# Search by name across the workspace
curl -s 'http://127.0.0.1:{{PORT}}/workspace-symbols?query=fetch' | jq '.[] | {name, containerName, kind}'
```

## Notes

- File paths must be **absolute**.
- Source is read from disk — unsaved editor buffers are not visible.
- If the project hasn't been built yet, hover/definition/references will return 404. Use the diagnostics skill to ensure the build is complete first.
