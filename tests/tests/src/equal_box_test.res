open Mocha
open Test_utils
open Js
let (aa, bb, cc) = (eqNull, eqUndefined, eqNullable)

describe(__MODULE__, () => {
  test("eqNull_tests", () => {
    let f = () => None
    let shouldBeNull = () => Js.null

    ok(__LOC__, !eqNull(3, Js.null))
    ok(__LOC__, !eqNull(None, Js.null))
    ok(__LOC__, !eqNull("3", Js.null))
    ok(__LOC__, !eqNull('3', Js.null))
    ok(__LOC__, !eqNull(0, Js.null))
    ok(__LOC__, !eqNull(0., Js.null))
    ok(__LOC__, !eqNull(f(), Js.null))
    ok(__LOC__, eqNull(shouldBeNull(), Js.null))
    ok(__LOC__, !eqNull(1, Js.Null.return(3)))
    ok(__LOC__, eqNull(None, Js.Null.return(None)))
    ok(__LOC__, !eqNull(Some(3), Js.Null.return(None)))
  })

  test("eqNullable_tests", () => {
    let f = () => None
    let shouldBeNull = () => Js.null
    let v = Nullable.null

    ok(__LOC__, !eqNullable(3, v))
    ok(__LOC__, !eqNullable(None, v))
    ok(__LOC__, !eqNullable("3", v))
    ok(__LOC__, !eqNullable('3', v))
    ok(__LOC__, !eqNullable(0, v))
    ok(__LOC__, !eqNullable(0., v))
    ok(__LOC__, !eqNullable(f(), v))
    ok(__LOC__, eqNullable(shouldBeNull(), v))
    ok(__LOC__, !eqNullable(1, Nullable.return(3)))
    ok(__LOC__, eqNullable(None, Nullable.return(None)))
    ok(__LOC__, !eqNullable(Some(3), Nullable.return(None)))
  })

  test("eqUndefined_tests", () => {
    let f = () => None
    let shouldBeNull = () => Js.null
    let v = Undefined.empty

    ok(__LOC__, !eqUndefined(3, v))
    ok(__LOC__, eqUndefined(None, v))
    ok(__LOC__, !eqUndefined("3", v))
    ok(__LOC__, !eqUndefined('3', v))
    ok(__LOC__, !eqUndefined(0, v))
    ok(__LOC__, !eqUndefined(0., v))
    ok(__LOC__, eqUndefined(f(), v))
    ok(__LOC__, !eqUndefined(shouldBeNull(), v))
    ok(__LOC__, !eqUndefined(1, Undefined.return(3)))
    ok(__LOC__, eqUndefined(None, Undefined.return(None)))
    ok(__LOC__, !eqUndefined(Some(3), Undefined.return(None)))
  })
})
