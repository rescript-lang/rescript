type kind = Ka | Kb | Kc | Kd
type rk = {fa: int, fb: int, fc: int}
module type S = {
  type kind2 = Ja | Jb | Jc | Jd
  type rj = {ga: int, gb: int}
  let sv: int
}
module Impl: S = {
  type kind2 = Ja | Jb | Jc | Jd
  type rj = {ga: int, gb: int}
  let sv = 1
}
let packed: module(S) = module(Impl)
