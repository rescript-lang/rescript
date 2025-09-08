let [] = ()
let [1, 2] = ()
let [1, 2,] = ()
let [1 : int, 2 : int] = ()
let ([1 : int, 2 : int] : array<int>) = ()

switch x {
| [] => ()
| [1, 2] => () 
| [1, 2,] => () 
| [1: int, 2 : int] => () 
| ([1: int, 2 : int] : int) => () 
}

let f = ([]) => ()
let f = ([x]) => ()
let f = ([x, y]) => x + y
let f = ([x] : int) => ()
let f = (([x] : int)) => ()

for [] in 0 to 10 {
  ()
}
for ([] in 0 to 10) {
  ()
}
for (([]) in 0 to 10) {
  ()
}
for [x] in 0 to 10 {
  ()
}
for ([x] in 0 to 10) {
  ()
}
for (([x]) in 0 to 10) {
  ()
}
for [x, y] in 0 to 10 {
  ()
}
for ([x, y] in 0 to 10) {
  ()
}
for (([x, y]) in 0 to 10) {
  ()
}
for (([x] : array<int>) in 0 to 10) {
  ()
}
