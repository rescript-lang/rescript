/* Passing a function with optional args as a first-class value must suppress
   optional-arg warnings even when the function also has a direct call. */

let formatDate = (~fmt=?, s) => s

let takesFn = _ => ()

takesFn(formatDate)

let liveCaller = () => formatDate("2024-01-01")

let _ = liveCaller()
