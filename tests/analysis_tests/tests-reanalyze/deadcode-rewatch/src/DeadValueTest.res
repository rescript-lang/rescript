let valueAlive = 1
let valueDead = 2

let valueOnlyInImplementation = 3

let rec subList = (b, e, l) =>
  switch l {
  | list{} => failwith("subList")
  | list{h, ...t} =>
    let tail = if e == 0 {
      list{}
    } else {
      subList(b - 1, e - 1, t)
    }
    if b > 0 {
      tail
    } else {
      list{h, ...tail}
    }
  }

