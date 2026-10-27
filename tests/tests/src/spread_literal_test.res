open Mocha
open Test_utils

let log = ref(list{})
let note = (s, v) => {
  log := list{s, ...log.contents}
  v
}

describe(__MODULE__, () => {
  test("array spreads keep items and spreads in order", () => {
    let a = [1, 2]
    let b = [5]
    eq(__LOC__, [...a, 3, 4, ...b, 6], [1, 2, 3, 4, 5, 6])
    eq(__LOC__, [0, ...a, ...[], ...b], [0, 1, 2, 5])
  })

  test("list spreads keep items and spreads in order", () => {
    let xs = list{1, 2}
    let ys = list{5}
    eq(__LOC__, list{...xs, 3, ...ys, 6}, list{1, 2, 3, 5, 6})
    eq(__LOC__, list{0, ...xs, ...list{}, 4, ...ys}, list{0, 1, 2, 4, 5})
    eq(__LOC__, list{0, ...xs}, list{0, 1, 2})
  })

  test("items and spreads are evaluated left to right", () => {
    log := list{}
    let arr = [note("a", 1), ...note("b", [2]), note("c", 3)]
    let lst = list{note("d", 4), ...note("e", list{5}), note("f", 6)}
    eq(__LOC__, (arr, lst), ([1, 2, 3], list{4, 5, 6}))
    eq(__LOC__, List.reverse(log.contents), list{"a", "b", "c", "d", "e", "f"})
  })
})
