let eachIntAsync = async (list: list<int>, f: int => unit) => {
  list->(await Js.import(Belt.List.forEach))(f)
}

let eachIntLazy = (list: list<int>, f: int => unit) =>
  Js.Promise.then_(each => list->each(f)->Js.Promise.resolve, Js.import(Belt.List.forEach))

let _ = list{1, 2, 3}->eachIntLazy(n => Js.log2("lazy", n))
let _ = list{1, 2, 3}->eachIntAsync(n => Js.log2("async", n))

module type BeltList = module type of Belt.List
let beltAsModule = await Js.import(module(Belt.List: BeltList))

// module type BeltList0 = module type of Belt.List
// module M = unpack(@res.await Js.import(module(Belt.List: BeltList0)))
module M = await Belt.List
let each = M.forEach

module N = {
  module N0 = await Belt.List
  let each = N0.forEach

  module N1 = {
    module O = await Belt.List
    let each = O.forEach
  }

  module N2 = await Belt.List
  let each = N2.forEach
}

module M0 = await Belt.List
let each = M0.forEach

module M1 = await Belt.List
let each = M1.forEach

module M2 = N.N1.O
let each2 = M2.forEach

let f = async () => {
  module M3 = await Belt.List
  M3.forEach
}

let f1 = async () => {
  module M3 = await (Belt.List: BeltList)
  M3.forEach
}

let f2 = async () => {
  module M3 = await (Belt.List: BeltList)
  module M4 = await (Belt.List: BeltList)
  (M3.forEach, M4.forEach)
}

let f3 = async () => {
  module M3 = await Belt.List
  module M4 = await Belt.List
  (M3.forEach, M4.forEach)
}

let f4 = async () => {
  module A = await Belt.Array
  A.forEach
}

let f5 = async () => {
  module A = await Belt.Array
  module O = await Belt.Option
  (A.forEach, O.forEach)
}

let f6 = async () => {
  let a = 0
  and b = {
    module MS = await Belt.Map.String
    MS.forEach
  }
  module A = await Belt.Array
  (a, b, A.forEach)
}

let f7 = async () => {
  if true {
    module MI = await Belt.Map.Int
    1
  } else {
    module MI = await Belt.Map.Dict
    0
  }
}

let f8 = async (): int => {
  module MQ = await Belt.MutableQueue
  1
}

let f9 = async value => {
  switch value {
  | Some(_) =>
    module HashMapInt = await Belt.HashMap.Int
    ()
  | None => ()
  }
}
