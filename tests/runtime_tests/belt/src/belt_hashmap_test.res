open Mocha
open Test_utils

module N = Belt.HashMap
module S = Belt.Map.Int
module I = Array_data_util
module A = Belt.Array
module So = Belt.SortArray

let intEq = (x: int, y) => x == y
let intHash = (x: int) => Hash_utils.hash(x)
let cmp = (x: int, y) => compare(x, y)
module Y = unpack(Belt.Id.hashable(~eq=intEq, ~hash=intHash))
let empty: N.t<int, int, _> = N.make(~id=module(Y), ~hintSize=30)

describe(__MODULE__, () => {
  test("fromArray", () => {
    let u = A.concat(I.randomRange(30, 100), I.randomRange(40, 120))
    let v = A.zip(u, u)
    let xx = N.fromArray(~id=module(Y), v)
    eq(__LOC__, N.size(xx), 91)
    eq(__LOC__, So.stableSortBy(N.keysToArray(xx), cmp), I.range(30, 120))
  })

  test("mergeMany", () => {
    N.mergeMany(empty, [(1, 1), (2, 3), (3, 3), (2, 2)])
    eq(__LOC__, N.get(empty, 2), Some(2))
    eq(__LOC__, N.size(empty), 3)
  })

  test("remove", () => {
    let u = A.concat(I.randomRange(0, 100_000), I.randomRange(0, 100))
    let v = N.make(~id=module(Y), ~hintSize=40)
    N.mergeMany(v, A.zip(u, u))
    eq(__LOC__, N.size(v), 100_001)
    for i in 0 to 1_000 {
      N.remove(v, i)
    }
    eq(__LOC__, N.size(v), 99_000)
    for i in 0 to 2_000 {
      N.remove(v, i)
    }
    eq(__LOC__, N.size(v), 98_000)
    ok(__LOC__, A.every(I.range(2_001, 100_000), x => N.has(v, x)))
  })
})
