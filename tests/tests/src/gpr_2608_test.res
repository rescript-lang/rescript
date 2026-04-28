open Belt
open Mocha
open Test_utils

let nearestGroots = list{}

let oppHeroes = list{0}
let huntGrootCondition =
  List.length(nearestGroots) > 0 && {
      let x = oppHeroes->List.filter(h => List.headExn(nearestGroots) <= 1000)
      List.length(x) == 0
    }

let huntGrootCondition2 =
  List.length(nearestGroots) >= 0 || {
      let x = oppHeroes->List.filter(h => List.headExn(nearestGroots) <= 1000)
      List.length(x) == 0
    }

describe(__MODULE__, () => {
  test("huntGroot conditions", () => {
    eq(__LOC__, huntGrootCondition, false)
    eq(__LOC__, huntGrootCondition2, true)
  })
})
