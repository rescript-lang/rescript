@@config({
  flags: [
    // "-dparsetree"
  ],
})

open Belt

let b = List.length(list{1, 2, 3})
let a = b - 1
Console.log("hello, res")

type t = {"x": int}

type h = {...t, "y": string}

let u01: h = {"x": 3, "y": "x"}
let u: t = {"x": 3}

let h = u["x"]

%%private(let {length, add} = module(List))

%%private(let {length, add} = module(List))

%%private(let (a, b) = (1, 2))

let {length: len, add: c} = module(List)

module H = {
  module H1 = {
    let v = 3
  }
}
let u = {
  let {length: l, add} = module(List)
  list{}->add(l(list{1, 2, 3}))
}

let h = {
  let {v} = module(H.H1)
  Console.log(v)
}

let to = 3
let downto = 1

Console.log([to, downto])
