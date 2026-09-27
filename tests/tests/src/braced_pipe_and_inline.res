module Ops = {
  let add = (x, y) => x + y
}

let direct = 2->{Ops.add(3)}
let opened = 2->{
  open Ops
  add(3)
}
let bound = 2->{
  let y = 3
  Ops.add(y)
}

@inline({never})
let increment = x => x + 1

let call = increment(3)

@inline
let literal = {"hello"}
