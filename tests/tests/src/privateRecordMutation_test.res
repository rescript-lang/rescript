@@warning("-111")

open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("mutates a mutable field exposed by a private record when warning is disabled", () => {
    let item = PrivateRecordMutation.make(1)
    item.value = 2
    eq(__LOC__, item.value, 2)
    eq(__LOC__, PrivateRecordMutation.value(item), 2)
    eq(__LOC__, item.name, "stable")
  })
})
