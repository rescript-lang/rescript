# LSP Module - Development Notes

## Debugging with `client.log_message`

When debugging LSP handlers, the most effective approach is to add temporary
`client.log_message(MessageType::INFO, ...)` calls at each step of the handler.
These messages appear directly in the IDE's output panel (e.g. "Output > ReScript Language Server"
in VS Code), making them much easier to observe than OTEL traces or `tracing::debug!` events.

This technique was crucial for diagnosing a bug where `did_change` diagnostics were silently
empty. By adding log messages at every step — module resolution, compiler arg construction,
bsc invocation, stderr capture, and diagnostic parsing — we could see that:

1. The handler was being called correctly
2. bsc was crashing with `Fatal error: exception Invalid_argument("index out of bounds")`
   mid-output, after printing the error header but before the message body
3. The diagnostic parser correctly returned 0 results from the truncated output

The root cause turned out to be in the compiler: when using `-bs-read-stdin`, bsc read source
from stdin for parsing but then tried to re-read the file from disk for error display context
(code frame). The disk file had different content than the buffer, causing position mismatches
and out-of-bounds string indexing in `code_frame.ml`.

### Pattern for temporary logging

Since `typecheck::run` executes under a `Mutex` lock and cannot `.await`, you can return
log messages alongside the result:

```rust
pub struct ChangeResult {
    pub diagnostics: Vec<BscDiagnostic>,
    pub logs: Vec<String>,
}
```

Then in the async handler, after releasing the lock:

```rust
for log in &result.logs {
    self.client.log_message(MessageType::INFO, log).await;
}
```

Note: the `MutexGuard` must be fully dropped before any `.await` call, otherwise the
compiler will reject the code because `MutexGuard` is not `Send`. Structure the lock
scope so the guard cannot be held across an await point.

## `did_change` vs `did_save`

- **`did_change`**: Runs a single-file typecheck by piping unsaved buffer content to
  `bsc -bs-read-stdin`. Only diagnostics for the active file are returned. Diagnostics
  are published directly under the editor's URI (no `group_by_file` lookup needed).

- **`did_save`**: Runs a full incremental build. Compiles the saved file and its dependency
  closure to produce JS, then typechecks dependents. Diagnostics are published for all
  touched files.

## Tracing spans with rayon (parallel iteration)

Rayon threads do **not** inherit the tracing subscriber's thread-local span context.
If you create an `info_span!` inside a `par_iter` closure without an explicit parent,
it will have no parent and won't appear in the span tree.

Use the `parent:` argument to set the parent explicitly:

```rust
let wave_span = tracing::info_span!("lsp.typecheck.wave", file_count = wave.len());
let results: Vec<_> = {
    let _entered = wave_span.enter();
    wave
        .par_iter()
        .map(|&idx| {
            let _span = tracing::info_span!(parent: &wave_span, "lsp.typecheck.file", module = %name).entered();
            // ...
        })
        .collect()
};
```

The wrapping span (`wave_span`) is entered on the calling thread so it appears in the
tree. Each per-item span uses `parent: &wave_span` so it becomes a child despite running
on a different rayon thread.

This is the same pattern used by `build.compile_wave` / `build.compile_file` in
`compile.rs`.

### `spawn_blocking` and parent spans

`tokio::task::spawn_blocking` runs on a separate thread pool that also lacks
tracing context. A common but **broken** pattern is:

```rust
let parent_span = tracing::Span::current();
tokio::task::spawn_blocking(move || {
    let _entered = parent_span.enter();  // sets context on this thread
    do_work();  // spans created here may NOT be exported
})
```

Even though `parent_span.enter()` sets the current span, child spans created
inside `do_work()` are sometimes not exported to the OTEL collector. Instead,
pass the parent span explicitly and use `parent:` at every level:

```rust
let parent_span = tracing::Span::current();
tokio::task::spawn_blocking(move || {
    do_work(&parent_span);
})

fn do_work(parent: &tracing::Span) {
    let span = tracing::info_span!(parent: parent, "my_span");
    let _entered = span.enter();
    // child spans also need parent: &span
}
```

## `-bs-read-stdin` and `Location.stdin_source`

When bsc is invoked with `-bs-read-stdin`, the source comes from stdin rather than disk.
The compiler stores this source in `Location.stdin_source` (a global `string option ref`)
so that error reporting can use it for code frame display instead of re-reading the
(potentially stale) file from disk. This prevents position mismatches and crashes.

Key locations in the compiler:
- `compiler/bsc/rescript_compiler_main.ml` — sets `Location.stdin_source` after reading stdin
- `compiler/ml/location.ml` — `print` function uses `stdin_source` as fallback for code frames
- `compiler/ml/error_message_utils.ml` — `extract_text_at_loc` uses `stdin_source` as fallback
