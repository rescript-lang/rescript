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

let x = 1
let shadowed = x->{
  let x = 2
  Ops.add(3)
}
assert(shadowed == 4)

let __tuple_internal_obj = 7
let generatedName = (1 + 1)
  ->{
    let __tuple_internal_obj = 10
    Ops.add(__tuple_internal_obj)
  }
assert(generatedName == 12)

let fanout = (1 + 1)->(x => x + __tuple_internal_obj, x => x)
let (fanoutFirst, fanoutSecond) = fanout
assert(fanoutFirst == 9)
assert(fanoutSecond == 2)

@inline({never})
let increment = x => x + 1

let call = increment(3)

@inline
let literal = {"hello"}
