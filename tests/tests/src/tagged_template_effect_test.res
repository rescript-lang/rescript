open Mocha
open Test_utils

/***
A tagged template calls its tag function, so the call runs even when its result is unused.
*/

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

let calls = ref(0)

let countingTag: taggedTemplate<int, int> = TaggedTemplate.make((_strings, _values) => {
  calls := calls.contents + 1
  1
})

ignore(countingTag`toplevel ${1}`)

let callsAtLoad = calls.contents

let unusedResult = () => {
  let _ = countingTag`local ${2}`
}

let unusedBinding = () => {
  let unused = countingTag`binding ${3}`
  calls.contents + 10
}

let discardedArray = () => ignore([countingTag`array ${4}`])

let enabled = ref(true)

let inBranch = cond =>
  if cond {
    ignore(countingTag`branch ${5}`)
  }

// Small enough to be inlined, which puts the argument in the tag's place
external asTag: ((array<string>, int) => int) => taggedTemplate<int, int> = "%identity"

let runWith = (tag: taggedTemplate<int, int>) => ignore(tag`run ${6}`)

let useWith = (tag: taggedTemplate<int, int>) => tag`use ${7}`

let functionTagUnused = () =>
  runWith(
    asTag((_strings, value) => {
      calls := calls.contents + 1
      value
    }),
  )

let functionTag = () => useWith(asTag((_strings, value) => value + 1))

let otherTag: taggedTemplate<int, int> = TaggedTemplate.make((_strings, _values) => 2)

let conditionalTagUnused = (cond, a, b) => runWith(cond ? a : b)

let conditionalTag = (cond, a, b) => useWith(cond ? a : b)

let awaitedTagUnused = async p => {
  runWith(await p)
}

let awaitedTag = async p => useWith(await p)

let conditionalValue = (tag: taggedTemplate<int, int>, cond) => tag`value ${cond ? 1 : 2}` + 1

describe(__MODULE__, () => {
  test("a top-level unused tagged template", () => eq(__LOC__, callsAtLoad, 1))

  test("an unused tagged template", () => {
    calls := 0
    unusedResult()
    eq(__LOC__, calls.contents, 1)
  })

  test("an unused named binding of a tagged template", () => {
    calls := 0
    eq(__LOC__, unusedBinding(), 11)
  })

  test("a tagged template in a discarded array", () => {
    calls := 0
    discardedArray()
    eq(__LOC__, calls.contents, 1)
  })

  test("a tagged template in an if branch", () => {
    calls := 0
    enabled := false
    inBranch(enabled.contents)
    eq(__LOC__, calls.contents, 0)
    enabled := true
    inBranch(enabled.contents)
    eq(__LOC__, calls.contents, 1)
  })

  test("an inlined function tag", () => {
    calls := 0
    functionTagUnused()
    eq(__LOC__, calls.contents, 1)
    eq(__LOC__, functionTag(), 8)
  })

  test("an inlined conditional tag", () => {
    calls := 0
    conditionalTagUnused(enabled.contents, countingTag, otherTag)
    eq(__LOC__, calls.contents, 1)
    conditionalTagUnused(!enabled.contents, countingTag, otherTag)
    eq(__LOC__, calls.contents, 1)
    eq(__LOC__, conditionalTag(enabled.contents, countingTag, otherTag), 1)
    eq(__LOC__, conditionalTag(!enabled.contents, countingTag, otherTag), 2)
  })

  testAsync("an awaited tag", async () => {
    calls := 0
    await awaitedTagUnused(Promise.resolve(countingTag))
    eq(__LOC__, calls.contents, 1)
    eq(__LOC__, await awaitedTag(Promise.resolve(otherTag)), 2)
  })

  test("an interpolated conditional", () => {
    eq(__LOC__, conditionalValue(otherTag, enabled.contents), 3)
  })
})
