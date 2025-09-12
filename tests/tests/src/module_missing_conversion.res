module type S = module type of String

module XX = {
  include Belt.Array
  let f = x => x
}
let u = [module(String: S)]

let hh = {
  let module(String: S) = u->Array.getUnsafe(0)
  String.length("x")
}
