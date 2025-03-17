module M = {
    type t = promise<string>

    let a = (_t:t) => 4
    let b = (_:t) => "c"
    let xyz = (_:t, p:int) => p + 1
}

@module("meh") @taggedTemplate
external meh: (array<string>, array<string>) => M.t = "default"

let w = meh``

// let _ = w.
//           ^com

// let x = meh`foo`.
//                  ^com
