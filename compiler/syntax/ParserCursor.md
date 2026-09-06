# Parser token cursor

The parser now separates token inspection from consumption inside `Res_parser`.
`next` consumes the current token without scanning its successor; `make` does no
scanning. This keeps the recursive-descent grammar and driver results while
removing scanner modes and manual scanner restoration from grammar code.

The implementation stays in the existing parser module. Most changes in
`res_core.ml` replace field reads with `peek`, `start_pos`, and `end_pos`; the
existing `next`, `expect`, and `optional` call sites retain their names.

## Inspection and consumption

```ocaml
let p = Parser.make "let /* comment */ x = 1" "Example.res" in
Parser.peek p;   (* Let; committed byte offset is still 0 *)
Parser.peek2 p;  (* Lident "x"; comment is still pending *)
Parser.next p;   (* consumes only Let; committed byte offset is 3 *)
Parser.peek p;   (* reuses the cached Lident *)
Parser.next p   (* consumes x and commits its leading comment once *)
```

A logical cursor records the consumed source position. Separate caches hold the
current token and, when requested, one successor.
The two slots have separate scanners so lookahead cannot advance the current
token's physical boundary. Both slots are reused for the entire file; inspection
does not allocate a new cursor or scanner. Each slot retains the scanner's
immutable result tuple directly, avoiding separate writes to three mutable fields
for each token. Checkpoints retain that same tuple. Simple arrow checks use
`peek2` or reject impossible first tokens before taking a checkpoint.

Comments and lexical diagnostics are pending until consumption. Reporting a
grammar error first publishes pending lexical diagnostics, preserving their
order. Diagnostic queries also include pending diagnostics to avoid duplicate
string errors.

`position` reads the logical cursor without scanning. AST boundaries, missing-token
diagnostics, and recovery progress checks all use this position. Consumption reuses
the token's immutable end position, so there is no separate previous-token position
or byte offset to update and restore. Before the first consumption, the cursor is
`Lexing.dummy_pos`, preserving existing location behavior. `finish` retains EOF
trivia and publishes pending warnings without moving the cursor.

## Contextual readers, without Diamond

The scanner returns individual `>` tokens and individual `<` tokens unless the
latter starts `<=`. In expression context, `peek_binary_operator` extends
an adjacent prefix into `>=`, `>>`, `>>>`, or `<<`. It never joins tokens across
whitespace or comments. Balanced lookahead uses the same query, keeping `>`
separate when it is the expected generic closer. Type arguments need no mode
stack:

```rescript
let value: array<option<int>>= [Some(1)]
let shifted = value >>> count
```

Regex reading restarts at the opening slash of `/` or `/.`, so the grammar no
longer reconstructs a missing dot. Template reading consumes the current opening
backtick or interpolation delimiter before reading raw text. Both readers discard
ordinary lookahead; raw text may already have been provisionally read as code.

## Speculation

`lookahead` always rolls back. `try_parse` commits `Some result` and rolls back
`None`; both restore on exceptions. One checkpoint implementation owns scanner
positions, cached token data, comments, diagnostics, breadcrumbs, committed
position, recovery regions, and pending warnings.

```ocaml
Parser.try_parse p (fun p ->
  let attrs = parse_attributes p in
  if Parser.peek p = And then Some attrs else None)
```

Recovery regions use persistent values rather than shared mutable refs. A failed
probe cannot suppress a later real error. In particular, an unquoted record field
in a non-arrow external type now reports the existing forbidden-inline-record
error; the valid object-type fixtures use quoted fields explicitly.

## List recovery

Comma-separated lists share one parser, with a reversal at the boundary for
callers that need source order. Recovery of unexpected `()`, `[]`, `{}`, or `<>`
groups uses that same list grammar recursively, retaining recoverable elements
and consuming the group's own closer. Closers belonging to an enclosing recovery
group remain available to it; stray closers do not end the list. The enclosing
closers are tracked only during error recovery, with an empty list on the normal
path. This replaces the type-argument-specific rule that discarded extra `<`
tokens without accounting for their closing `>`.

Type arguments report an invalid opening `(` before parsing its contents and
recover through the matching `)`. The existing diagnostic region suppresses
secondary errors within that declaration. When displayed, the diagnostic uses
the existing type printer to show the constructor with its recovered arguments,
such as `Nullable.t<'a>`. Formatting is deferred until the message is requested.
No scanner mode or additional persistent recovery state is needed.

## Validation

Cursor unit tests cover lazy consumption, cached lookahead, trivia, diagnostics,
EOF, nested rollback, exceptions, raw readers, and UTF-16/CRLF positions. Syntax
and runtime fixtures exercise nested generic closers, shifts, regex prefixes, and
template interpolation. Runtime compilation also runs the Lambda invariant check.

Compare parser ASTs and locations, syntax snapshots, round trips, the full test
suite, and the playground build against the same upstream revision. Benchmark
saved release binaries on identical real and synthetic inputs, alternating their
order; measure both elapsed parsing time and allocations. Earlier Diamond-only
PoC measurements do not establish the performance of this cursor implementation.
