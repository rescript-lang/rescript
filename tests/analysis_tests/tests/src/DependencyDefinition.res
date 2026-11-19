// Definitions in dependency files: the stdlib and Belt.

let m = Array.map([1], x => x + 1)
//            ^def

let b = Belt.Array.map([1], x => x + 1)
//                 ^def

let n: Null.t<int> = Null.null
//          ^typ

let s: Belt.Set.Int.t = Belt.Set.Int.empty
//                                   ^def
