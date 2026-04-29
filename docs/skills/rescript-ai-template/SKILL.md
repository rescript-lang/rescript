---
name: rescript-ai-template
description: Template skill for agents working in a ReScript codebase with rescript-assist lint check and related AI-oriented workflows. Use when creating a project-specific skill for file-by-file editing, lint-repair loops, and semantic checks.
---

# ReScript AI Template

Use this as a starting point for a project-specific skill. Replace placeholders with repo-specific rules, commands, and ownership boundaries.

## When To Use

Use this skill when:

- editing `.res` or `.resi` files
- fixing lint findings from `rescript-assist lint check`
- doing small file-first repair loops
- checking semantic issues that depend on `.cmt/.cmti`

## Default Workflow

1. Prefer a file-first loop:
   - edit one file
   - run `rescript-assist lint check <file>`
   - fix findings
   - rerun lint for that file

2. Use root or subtree lint when:
   - a change spans multiple modules
   - resolving follow-on fallout
   - preparing a wider verification pass

3. Prefer text output by default.

4. Use `--json` only when the agent needs machine-readable output for batching or post-processing.

## Commands

```sh
rescript-assist lint check <file>
rescript-assist lint check <dir-or-root>
rescript-assist lint check <file-or-root> --json
```

Optional project verification commands:

```sh
# Replace these with project-local commands
make test-syntax
make test-tools
make test
```

## How To Interpret Output

- `severity`: `error` or `warning`
- `rule`: stable rule id
- `path`: repo-relative path
- `range`: 1-based text range in text mode
- `symbol`: resolved semantic symbol when available
- `snippet`: small local code fence for repair context

Treat typed findings as the source of truth for semantic rules like forbidden references.

## Editing Guidance

- Keep fixes narrow.
- Prefer existing project helpers and patterns.
- Do not weaken lint rules just to silence findings.
- If a typed rule does not trigger, confirm `.cmt/.cmti` artifacts exist before assuming the code is clean.

## Project Rules

Fill this section in for the actual repo:

- Forbidden modules or APIs:
  - `<example: Belt.Array.forEach>`
  - `<example: Js.Json.t>`
- Preferred replacements:
  - `<example: Array.forEach>`
  - `<example: project-local wrapper>`
- Rewrite or normalization preferences:
  - `<example: prefer switch over if in generated agent edits>`

## Repair Loop

For one-file work:

```sh
rescript-assist lint check src/File.res
```

Fix the reported findings, then rerun the same command until clean.

For broader fallout:

```sh
rescript-assist lint check src/
```

## Notes

- `lint` is for reporting.
- `rewrite` is a separate command and may be more aggressive once available.
- Git-aware filtering should be used when the repo enables that mode.
