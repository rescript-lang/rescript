/* Repro for the optional-arg ownership regression on fix-reanalyze.
   Passing a function with optional args as a first-class value in a top-level
   eval should not suppress warnings on the function declaration itself. */

let formatDate = (~fmt=?, s) => s

let takesFn = _ => ()

takesFn(formatDate)

let liveCaller = () => formatDate("2024-01-01")

let _ = liveCaller()

