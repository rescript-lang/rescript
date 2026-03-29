---
name: rescript-diagnostics
description: Use after editing ReScript (.res, .resi) files to check for type errors and build diagnostics. The LSP server automatically rebuilds on file save; this skill queries the HTTP diagnostics endpoint to get the current error state without triggering a separate build.
---

# ReScript Diagnostics

This skill provides access to real-time build diagnostics from the running ReScript LSP server. Use it to verify that your code changes compile correctly.

## When to Use

- After editing and saving a `.res` or `.resi` file, to check for type errors
- Before considering a task complete, to verify the project builds clean
- When you need to understand what errors exist in the project

## How It Works

The ReScript LSP server runs an HTTP endpoint that exposes current diagnostics. When you save a file, the LSP automatically rebuilds. The `/diagnostics` endpoint **blocks** until the build finishes (up to 30 seconds), so you can save and immediately query.

## Prerequisites

The editor must be running with the ReScript LSP server and `diagnostics_http` configured. The HTTP port is set via `initializationOptions` in the editor's LSP config.

## Usage

### Check diagnostics after saving

```bash
curl -s http://127.0.0.1:{{PORT}}/diagnostics | jq .
```

An empty `{}` means the project is clean. Otherwise, errors are grouped by file:

```json
{
  "/absolute/path/to/File.res": [
    {
      "range": {
        "start": { "line": 5, "character": 10 },
        "end": { "line": 5, "character": 20 }
      },
      "severity": "error",
      "message": "This has type: string\nBut it's expected to have type: int"
    }
  ]
}
```

### Check build status

The `X-Build-Status` response header tells you whether the LSP finished building:

```bash
curl -sI http://127.0.0.1:{{PORT}}/diagnostics | grep X-Build-Status
```

- `idle` — build completed, diagnostics are up to date
- `timeout` — build didn't finish within 30 seconds (rare)

### Report a problem

If you observe unexpected behavior (e.g., a file wasn't compiled after save), report it:

```bash
curl -X POST http://127.0.0.1:{{PORT}}/report -d "File Foo.res was not compiled to JS after save"
```

This creates an OTEL trace span for debugging.

## Workflow

1. Edit a `.res` file
2. Save the file (the LSP auto-rebuilds)
3. `curl -s http://127.0.0.1:{{PORT}}/diagnostics | jq .`
4. If errors exist, fix them and repeat from step 1
5. When `{}` is returned, the project is clean
