open Mocha
open Test_utils

// A mutable field can be viewed through an immutable one: a read-only view.
type counter = {mutable count: int, label: string}
type readOnlyCounter = {count: int}

// A field mutable in both records keeps its type, and both views share the
// same object, so writes through either are seen through the other.
type box = {mutable value: int, extra: string}
type smallBox = {mutable value: int}

// The field type may still be widened when the field is immutable in the
// target, here from a closed polymorphic variant to a wider one.
type narrow = {mutable tag: [#A]}
type wideReadOnly = {tag: [#A | #B]}

describe(__MODULE__, () => {
  test("read-only view sees later writes", () => {
    let c = {count: 0, label: "c"}
    let view = (c :> readOnlyCounter)
    c.count = c.count + 1
    eq(__LOC__, 1, view.count)
  })

  test("mutable to mutable shares writes", () => {
    let b = {value: 1, extra: "x"}
    let small = (b :> smallBox)
    small.value = 2
    eq(__LOC__, 2, b.value)
    b.value = 3
    eq(__LOC__, 3, small.value)
  })

  test("immutable target field can widen its type", () => {
    let n: narrow = {tag: #A}
    let w = (n :> wideReadOnly)
    eq(__LOC__, #A, w.tag)
  })
})
