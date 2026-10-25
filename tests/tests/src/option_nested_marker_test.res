open Mocha
open Test_utils

@scope("Object") external isFrozen: 'a => bool = "isFrozen"

let wrap = x => Some(x)

let unwrap = (x: option<'a>) =>
  switch x {
  | Some(v) => v
  | None => assert(false)
  }

let depth = (x: option<option<option<int>>>) =>
  switch x {
  | None => 0
  | Some(None) => 1
  | Some(Some(None)) => 2
  | Some(Some(Some(_))) => 3
  }

describe(__MODULE__, () => {
  test("a nested None is the same object at each depth", () => {
    let none: option<int> = None
    eq(__LOC__, true, wrap(none) === wrap(none))
    eq(__LOC__, true, wrap(wrap(none)) === wrap(wrap(none)))
    eq(__LOC__, false, wrap(none) === Obj.magic(wrap(wrap(none))))
  })
  test("unwrapping gives the marker one level down", () => {
    let none: option<int> = None
    eq(__LOC__, true, unwrap(wrap(wrap(none))) === wrap(none))
    eq(__LOC__, true, unwrap(unwrap(wrap(wrap(none)))) === none)
  })
  test("markers are frozen", () => {
    let none: option<int> = None
    eq(__LOC__, true, isFrozen(wrap(none)))
    eq(__LOC__, true, isFrozen(wrap(wrap(none))))
  })
  test("matching and equality are unchanged", () => {
    let none: option<int> = None
    let none2: option<option<int>> = None
    eq(__LOC__, 0, depth(None))
    eq(__LOC__, 1, depth(wrap(none2)))
    eq(__LOC__, 2, depth(wrap(wrap(none))))
    eq(__LOC__, 3, depth(wrap(wrap(Some(1)))))
    eq(__LOC__, Some(Some(None)), wrap(wrap(none)))
    eq(__LOC__, true, wrap(wrap(none)) == Some(Some(None)))
  })
})
