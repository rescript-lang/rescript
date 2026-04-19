open Mocha
open Test_utils

describe(__MODULE__, () => {
  test("mutates a mutable field exposed by an @allowMutation private record", () => {
    let item = AllowMutationPrivateRecord.make(1)
    item.value = 2
    eq(__LOC__, item.value, 2)
    eq(__LOC__, AllowMutationPrivateRecord.value(item), 2)
    eq(__LOC__, item.name, "stable")
  })
})
