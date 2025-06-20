module Uncurried = {
  let raise = e => throw(e)

  module List = {
    let map = (l, f) => Belt.List.map(l, f)
  }
}

exception E

module StandardNotation = {
  open Uncurried

  let testRaise = () => throw(E)

  let l = List.map(list{1, 2}, x => x + 1)
  let partial = x => List.map(list{1, 2}, x)
  let ll = partial(x => x + 1)

  let withOpts = (~x=3, y, ~z=4, w) => x + y + z + w
  type unc2 = (~z: int=?, int) => int
}

open Uncurried

let testRaise = () => throw(E)

let l = List.map(list{1, 2}, x => x + 1)
let partial = List.map(list{1, 2}, ...)
let ll = partial(x => x + 1)

let withOpts = (~x=3, y, ~z=4, w) => x + y + z + w
type unc2 = (~z: int=?, int) => int
