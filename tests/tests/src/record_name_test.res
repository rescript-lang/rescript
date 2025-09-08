/*
To work around unused attribute checking

- we mark it used in ppx stage
we can not mark it in parsing since it won't
works for reason
*/
type t = {
  @as("THIS_IS_NOT_EXPRESSIBLE_IN_BUCKLE") mutable x: int,
  /* test key word later */
}

let f = x => {x: x}

let set = x => {
  x.x = 3
  x.x * 2
}

type x = t = private {@as("THIS_IS_NOT_EXPRESSIBLE_IN_BUCKLE") mutable x: int}

type rec t0 = {x: t0, y: int}

let f1 = u =>
  switch u {
  | {x: {x: {x: {y}}}} => y
  }

type t1 = {mutable x': int}

let f2 = (x: t1) => {
  x.x' = x.x' + 3
  {x': x.x' + 3}
}

type t2 = {@as("open") mutable x': int}

let f3 = (x: t2) => {
  x.x' = x.x' + 3
  {x': x.x' + 3}
}

type t3 = {@as("in") mutable x': int}

let f3 = (x: t3) => {
  x.x' = x.x' + 3
  {x': x.x' + 3}
}

type rec entry = {
  @as("EXACT_MAPPING_TO_JS_LABEL") x: int,
  @as("EXACT_2") y: int,
  z: obj,
}
and obj = {@as("hello") hi: int}

let f4 = ({x, y, z: {hi}}: entry) => (x + y + hi) * 2

/* either x or y is a mistake */

type t6 = {
  @as("x") x: int,
  @as("y") y: int,
}
/* allow this case */

@obj external ff: (~x: int, ~h: @as(3) _) => _ = ""
@obj external ff2: (~x: int, ~h: @as(3) _) => {"x": int} = ""
let u = () => {
  ignore(ff(~x=3))
  ff2(~x=22)
}
