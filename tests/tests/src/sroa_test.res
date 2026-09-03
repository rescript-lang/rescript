open Mocha
open Test_utils

type pair = {
  mutable left: int,
  mutable right: int,
}

let localPair = () => {
  let pair = {left: 10, right: 20}
  pair.left = pair.left + 1
  pair.right = pair.left + pair.right
  pair.left + pair.right
}

let capturedPair = () => {
  let pair = {left: 10, right: 20}
  let bumpLeft = () => pair.left = pair.left + 1
  bumpLeft()
  bumpLeft()
  pair.left + pair.right
}

let initializationOrder = () => {
  let seen = ref(list{})
  let initialize = value => {
    seen.contents = list{value, ...seen.contents}
    value
  }
  let pair = {left: initialize(1), right: initialize(2)}
  pair.left = pair.left + 1
  (pair.left + pair.right, List.toArray(List.reverse(seen.contents)))
}

@inline(never)
let consumePair = pair => pair.left + pair.right

let escapedPair = () => {
  let pair = {left: 10, right: 20}
  consumePair(pair)
}

describe(__MODULE__, () => {
  test("scalarizes a local mutable record", () => eq(__LOC__, 42, localPair()))
  test("shares scalar fields with closures", () => eq(__LOC__, 32, capturedPair()))
  test("preserves initializer order", () => eq(__LOC__, (4, [1, 2]), initializationOrder()))
  test("retains an escaping record", () => eq(__LOC__, 30, escapedPair()))
})
