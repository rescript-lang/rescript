open Mocha
open Test_utils

external eqNull: ('a, null<'a>) => bool = "%equal_null"
external eqNullable: ('a, nullable<'a>) => bool = "%equal_nullable"

describe(__MODULE__, () => {
  test("eqNull_tests", () => {
    let f = () => None
    let shouldBeNull = () => Null.null

    ok(__LOC__, !eqNull(3, Null.null))
    ok(__LOC__, !eqNull(None, Null.null))
    ok(__LOC__, !eqNull("3", Null.null))
    ok(__LOC__, !eqNull('3', Null.null))
    ok(__LOC__, !eqNull(0, Null.null))
    ok(__LOC__, !eqNull(0., Null.null))
    ok(__LOC__, !eqNull(f(), Null.null))
    ok(__LOC__, eqNull(shouldBeNull(), Null.null))
    ok(__LOC__, !eqNull(1, Null.make(3)))
    ok(__LOC__, eqNull(None, Null.make(None)))
    ok(__LOC__, !eqNull(Some(3), Null.make(None)))
  })

  test("eqNullable_tests", () => {
    let f = () => None
    let shouldBeNull = () => Null.null
    let v = Nullable.null

    ok(__LOC__, !eqNullable(3, v))
    ok(__LOC__, !eqNullable(None, v))
    ok(__LOC__, !eqNullable("3", v))
    ok(__LOC__, !eqNullable('3', v))
    ok(__LOC__, !eqNullable(0, v))
    ok(__LOC__, !eqNullable(0., v))
    ok(__LOC__, !eqNullable(f(), v))
    ok(__LOC__, eqNullable(shouldBeNull(), v))
    ok(__LOC__, !eqNullable(1, Nullable.make(3)))
    ok(__LOC__, eqNullable(None, Nullable.make(None)))
    ok(__LOC__, !eqNullable(Some(3), Nullable.make(None)))
  })
})
