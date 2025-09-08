module A = {
  let makeX = () => (1, 2)

  let (xxx, yyy) = makeX()

  type t = {name: string, age: int}

  let makeT = () => {name: "", age: 0}

  let {name, age} = makeT()

  let (a | a, b) = makeX()

  type rec arr = A(array<arr>)

  let A([v1, _, _]) | _ as v1 = assert false

}

let y = A.xxx
//        ^def

let z = A.yyy

let n1 = A.name
//         ^def

let n2 = A.a
//         ^def

let n3 = A.v1
//         ^def
