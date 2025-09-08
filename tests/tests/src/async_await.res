let next = n => n + 1
let useNext = async () => next(3)

module type Impl = {
  let get: string => Js.Promise.t<string>
}

module Make = (I: Impl) => {
  let get = async key => await I.get(key)
}

let topFoo = async () => 1
let arr = [1, 2, 3]

let toplevelAwait = await topFoo()
let toplevelAwait2 = arr[await topFoo()]

let f = async (type input, value: input) => {
  await Js.Promise.resolve(1)
}

module type MT = module type of Belt.Option

let f0 = async () => {
  module O = await Belt.Option
  O.forEach
}

let f1 = async () => {
  module O: MT = await Belt.Option
  O.forEach
}
