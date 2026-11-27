type t = {mutable c: int}

let j = {c: 1}
let k = {c: 1}

let upd = () => k.c = 3

if k.c == 1 {
  upd()
  j.c = j.c + 2
  Console.log("correct")
} else {
  upd()
  j.c = j.c + 2
  Console.log("incorrect")
}

let j = 0
let k = 0
