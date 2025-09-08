type t<'a0, 'a1> = 'a0 => 'a1
let f0: unit => int
let f1: 'a => 'a
let f2: ('a, 'b) => ('a, 'b)

let xx: unit => 'a

type logger = {log2: 'a. (string, 'a) => unit}

let log2: (logger, string, 'a) => unit
