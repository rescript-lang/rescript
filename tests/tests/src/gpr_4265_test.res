open Mocha
open Test_utils

open Belt
let mockMap = MutableMap.Int.make()
let add = id => {
  mockMap->MutableMap.Int.set(id, id)
  id
}
let remove = id => mockMap->MutableMap.Int.remove(id)
let _ = add(1726)
let n = add(6667)
let _ = add(486)
let _ = remove(1726)
let n1 = mockMap->MutableMap.Int.getExn(6667)

describe(__MODULE__, () => {
  test("mutable map operations", () => eq(__LOC__, n, n1))
})
