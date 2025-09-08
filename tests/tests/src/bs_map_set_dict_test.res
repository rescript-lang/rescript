let suites: ref<Mt.pair_suites> = ref(list{})
let test_id = ref(0)
let eq = (loc, x, y) => Mt.eq_suites(~suites, ~test_id, loc, x, y)
let b = (loc, v) => Mt.bool_suites(~suites, ~test_id, loc, v)

module Icmp = unpack(Belt.Id.comparable(~cmp=(x: int, y) => compare(x, y)))
module Icmp2 = unpack(Belt.Id.comparable(~cmp=(x: int, y) => compare(x, y)))

module Ic3 = unpack(Belt.Id.comparable(~cmp=(compare: (int, int) => int)))

module M = Belt.Map
module MI = Belt.Map.Int
/* module B = Belt.Bag */
module I = Array_data_util
module A = Belt.Array
module L = Belt.List
let m0: M.t<_, string, _> = M.make(~id=module(Icmp))

let m00 = Belt.Set.make(~id=module(Ic3))
module I2 = unpack(Belt.Id.comparable(~cmp=(x: int, y) => compare(y, x)))

let m = M.make(~id=module(Icmp2))
let m2: M.t<int, string, _> = M.make(~id=module(I2))
let vv = MI.empty
let vv2 = MI.empty
module Md0 = Belt.Map.Dict
let () = {
  let count = 1_000_00
  let data = ref(M.getData(m))
  let (m2_dict, m_dict) = {
    open M
    (getId(m2), getId(m))
  }
  module N = unpack(m2_dict)
  module Mm = unpack(m_dict)
  for i in 0 to count {
    data := Md0.set(data.contents, ~cmp=Mm.cmp, i, i)
  }
  let newm = M.packIdData(~data=data.contents, ~id=m_dict)
  Js.log(newm)
}
module ISet = Belt.Set
let () = {
  let m = Md0.empty
  let m11 = Md0.set(~cmp=Icmp.cmp, m, 1, 1)

  let _m20 = M.make(~id=module(Icmp))
  Js.log(m11)
}

module S0 = Belt.Set.Dict
let () = {
  let count = 100_000
  let v = ISet.make(~id=module(Icmp2))
  let m_dict = M.getId(m)
  module M = unpack(m_dict)
  let cmp = M.cmp
  let data = ref(ISet.getData(v))
  for i in 0 to count {
    data := S0.add(~cmp, data.contents, i)
  }
  Js.log(data.contents)
}

let f = M.fromArray(~id=module(Icmp), ...)
let \"=~" = (a, b) => M.eq(a, b, ...)

let () = {
  let u0 = f(A.map(I.randomRange(0, 39), x => (x, x)))
  let u1 = M.set(u0, 39, 120)
  b(
    __LOC__,
    A.every2(M.toArray(u0), A.map(I.range(0, 39), x => (x, x)), ((x0, x1), (y0, y1)) =>
      x0 == y0 && x1 == y1
    ),
  )

  b(
    __LOC__,
    L.every2(M.toList(u0), L.fromArray(A.map(I.range(0, 39), x => (x, x))), ((x0, x1), (y0, y1)) =>
      x0 == y0 && x1 == y1
    ),
  )
  eq(__LOC__, M.get(u0, 39), Some(39))
  eq(__LOC__, M.get(u1, 39), Some(120))
}

let () = {
  let u = f(A.makeByAndShuffle(10_000, x => (x, x)))
  eq(__LOC__, A.makeBy(10_000, x => (x, x)), M.toArray(u))
}

Mt.from_pair_suites(__MODULE__, suites.contents)
