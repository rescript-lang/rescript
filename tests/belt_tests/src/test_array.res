open Belt

let v = Array.make(6, 5)

let h = Array.slice(v, ~offset=0, ~len=2)
let hh = Array.blit

let hhh = Array.concat([1, 2, 3, 4], [1, 2, 3, 5])

let u = Array.concatMany([[1, 2], [2, 3], [3, 4]])
