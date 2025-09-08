type t0 = {x: int, y?: int, yy?: option<int>, z: int}

let f1 = {x: 3, z: 2}

let f2 = {x: 3, z: 3, y: 3}

let f3 = {...f1, y: 3}

let f4 = {...f3, yy: None}

let theseTwoShouldBeIdentical = [f4.yy, (Some(None): option<option<int>>)]

type r = {
  x: int,
  y: option<int>,
  z: int,
}

// let v0 = { x :  3 , z : 2 }

// let v2 = { ... v0 , x : 3 }

let v1: t0 = {
  x: 3,
  z: 3,
}

let v2: r = {x: 3, y: None, z: 2}

type config = {
  x: int,
  y0?: int,
  y1?: int,
  y2?: int,
  y3?: int,
  y4?: int,
  y5?: int,
  y6?: int,
  y7?: int,
  y8?: int,
  y9?: int,
  y10?: int,
  y11?: int,
  y12?: int,
  y13?: int,
  y14?: int,
  y15?: int,
  y16?: int,
  y17?: int,
  y18?: int,
  y19?: int,
  y20?: int,
  y21?: int,
  y22?: int,
  y23?: int,
  z: int,
}

let v: config = {x: 2, z: 3}

let h: config = {...v, y1: 22}

type small_config = {
  x: int,
  y0?: int,
  y1?: int,
  z: int,
}

let v1: small_config = {x: 2, z: 3}

let h10: small_config = {...v1, y1: 22}

let h11 = (v1): small_config => {
  {...v1, y1: 22}
}

type partiallyOptional = {
  aa?: int,
  bb: option<int>,
}

let po = {aa: 3, bb: Some(4)}

let _ = {...po, aa: ?None}

let setAA = (ao: option<int>) => {aa: ?ao, bb: None}

type inlinedOptional = V0({x0: string, x1?: string, x2?: int, x3: int}) | V1({y0: string, y1: int})

let ir0 = V0({x0: "v0", x3: 3})
let ir1 = V0({x0: "v0", x1: "v1", x3: 3})
let ir2 = V0({x0: "v0", x1: "v1", x2: 2, x3: 3})
let ir3 = V1({y0: "v0", y1: 1})

let pm0 = switch ir0 {
| V0({x0, x3}) => (x0, x3)
| V1({y0, y1}) => (y0, y1)
}
let pm1 = switch ir1 {
| V0({x0, x1, x3}) => (x0, x1, x3)
| V0({x0, x1: ?None, x3}) => (x0, "n/a", x3)
| V1({y0, y1}) => (y0, "n/a", y1)
}
let pm2 = switch ir2 {
| V0({x0, x1, x2, x3}) => (x0, x1, x2, x3)
| V0({x0, x1: ?None, x2, x3}) => (x0, "n/a", x2, x3)
| V0({x0, x1, x2: ?None, x3}) => (x0, x1, 0, x3)
| V0({x0, x1: ?None, x2: ?None, x3}) => (x0, "n/a", 0, x3)
| V1({y0, y1}) => (y0, "n/a", 0, y1)
}
let inlinedRecord = ir =>
  switch ir {
  | V0({x0, x1: ?Some("x1"), x2, x3}) => (x0, "x1", x2, x3)
  | V0({x0, x1: "xx1", x2, x3}) => (x0, "xx1", x2, x3)
  | V0({x0, x1, x2, x3}) => (x0, x1, x2, x3)
  | V0({x0, x1: ?None, x2, x3}) => (x0, "n/a", x2, x3)
  | V0({x0, x1, x2: ?None, x3}) => (x0, x1, 0, x3)
  | V0({x0, x1: ?None, x2: ?None, x3}) => (x0, "n/a", 0, x3)
  | V1({y0, y1}) => (y0, "n/a", 0, y1)
  }
let pm3 = inlinedRecord(ir2)
let pm4 = inlinedRecord(ir3)

type inlinedOptional2 = V0({x?: int})
let ir4 = V0({x: 3})
let ir5 = V0({})
