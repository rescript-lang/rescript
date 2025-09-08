module type X = module type of String

let u = (v: module(X)) => v

module N = {
  let s = u(module(String))
}

let v0 = {
  module V = unpack(N.s: X)
  V.length("x")
}

let v = x => {
  module V = unpack(N.s: X)
  V.length(x)
}

let suites = {
  open Mt
  list{("const", _ => Eq(1, v0)), ("other", _ => Eq(3, v("abc")))}
}

Mt.from_pair_suites(__MODULE__, suites)
