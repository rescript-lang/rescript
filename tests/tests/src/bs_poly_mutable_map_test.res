open Mocha
open Test_utils

module Icmp = unpack(Belt.Id.comparable(~cmp=(x: int, y) => compare(x, y)))
module M = Belt.MutableMap
module N = Belt.Set

module A = Belt.Array
module I = Array_data_util
let f = x => M.fromArray(~id=module(Icmp), x)
let ff = x => N.fromArray(~id=module(Icmp), x)

let randomRange = (i, j): array<(int, int)> => A.map(I.randomRange(i, j), x => (x, x))

%%private(
  let (\".!()<-", \".!()") = {
    open M
    (set, getExn)
  }
)

describe(__MODULE__, () => {
  test("mutable map operations with small range", () => {
    let a0 = f(randomRange(0, 10))
    \".!()<-"(a0, 3, 33)
    eq(__LOC__, M.getExn(a0, 3), 33)
    M.removeMany(a0, [7, 8, 0, 1, 3, 2, 4, 922, 4, 5, 6])
    eq(__LOC__, M.keysToArray(a0), [9, 10])
    M.removeMany(a0, I.randomRange(0, 100))
    eq(__LOC__, M.isEmpty(a0), true)
  })

  test("mutable map operations with large range", () => {
    let a0 = f(randomRange(0, 10000))
    \".!()<-"(a0, 2000, 33)
    a0->M.removeMany(randomRange(0, 1998)->A.map(fst))
    a0->M.removeMany(randomRange(2002, 11000)->A.map(fst))
    eq(__LOC__, a0->M.toArray, [(1999, 1999), (2000, 33), (2001, 2001)])
  })
})
