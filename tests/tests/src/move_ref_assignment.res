type t = {mutable c: int}

let j = {c: 0}
let k = {c: 1}

if j.c == 0 {
  j.c = j.c + 2
  Console.log("j.c")
} else {
  j.c = j.c + 2
}

if k.c == 0 {
  j.c = j.c + 2
  Console.log("k.c")
} else {
  j.c = j.c + 2
}

let j = 0
let k = 0
