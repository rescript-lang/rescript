let eachIntAsync = async (list: list<int>, f: int => unit) => {
  list->(await import(List.forEach))(f)
}

let eachIntLazy = (list: list<int>, f: int => unit) =>
  Promise.then(import(List.forEach), each => list->each(f)->Promise.resolve)

let _ = list{1, 2, 3}->eachIntLazy(n => Console.log2("lazy", n))
let _ = list{1, 2, 3}->eachIntAsync(n => Console.log2("async", n))

module type ListType = module type of List
let listAsModule = await import(module(List: ListType))

// module type ListType0 = module type of List
// module M = unpack(@res.await import(module(List: ListType0)))
module M = await List
let each = M.forEach

module N = {
  module N0 = await List
  let each = N0.forEach

  module N1 = {
    module O = await List
    let each = O.forEach
  }

  module N2 = await List
  let each = N2.forEach
}

module M0 = await List
let each = M0.forEach

module M1 = await List
let each = M1.forEach

module M2 = N.N1.O
let each2 = M2.forEach

let f = async () => {
  module M3 = await List
  M3.forEach
}

let f1 = async () => {
  module M3 = await (List: ListType)
  M3.forEach
}

let f2 = async () => {
  module M3 = await (List: ListType)
  module M4 = await (List: ListType)
  (M3.forEach, M4.forEach)
}

let f3 = async () => {
  module M3 = await List
  module M4 = await List
  (M3.forEach, M4.forEach)
}

let f4 = async () => {
  module A = await Array
  A.forEach
}

let f5 = async () => {
  module A = await Array
  module O = await Option
  (A.forEach, O.forEach)
}

let f6 = async () => {
  let a = 0
  and b = {
    module MS = await Set
    MS.forEach
  }
  module A = await Array
  (a, b, A.forEach)
}

let f7 = async () => {
  if true {
    module MI = await Int
    1
  } else {
    module MI = await Dict
    0
  }
}

let f8 = async (): int => {
  module S = await Set
  1
}

let f9 = async value => {
  switch value {
  | Some(_) =>
    module MathModule = await Math
    ()
  | None => ()
  }
}
