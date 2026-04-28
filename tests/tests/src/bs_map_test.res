open Mocha
open Test_utils

module M = Belt.Map.Int
module N = Belt.Set.Int
module A = Belt.Array

let mapOfArray = x => M.fromArray(x)
let setOfArray = x => N.fromArray(x)
let emptyMap = () => M.empty

describe(__MODULE__, () => {
  test("bs map test", () => {
    let v = A.makeByAndShuffle(1_000_000, i => (i, i))
    let u = M.fromArray(v)
    M.checkInvariantInternal(u)
    let firstHalf = A.slice(v, ~offset=0, ~len=2_000)
    let xx = A.reduce(firstHalf, u, (acc, (x, _)) => M.remove(acc, x))
    M.checkInvariantInternal(u)
    M.checkInvariantInternal(xx)
  })
})
