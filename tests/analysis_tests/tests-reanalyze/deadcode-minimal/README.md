# Deadcode Minimal

This is a small end-to-end regression test for reactive deadcode analysis.

Historically this directory was used to minimize an incremental fixpoint bug (re-derivation
after removals). The underlying issue has since been fixed; this test remains to prevent
regressions.

## Test File (src/DeadTest.res)

```rescript
module MM: {
  let x: int
  let y: int
} = {
  let y = 55    // BUG: incorrectly marked dead in reactive mode
  let x = y     // live (externally referenced)
}

let _ = Js.log(MM.x)  // external reference to x
```

## Running

```bash
# Build
../../../../cli/rescript.js build

# Non-reactive (correct): 1 issue (signature y)
dune exec rescript-editor-analysis -- reanalyze -config -ci

# Reactive
dune exec rescript-editor-analysis -- reanalyze -config -ci -reactive
```

Reactive and non-reactive should report the same results.
