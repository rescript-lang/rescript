open Mocha
open Test_utils

type pair = {
  mutable left: int,
  mutable right: int,
}

type fieldUses = {
  mutable live: int,
  mutable deadPure: int,
  mutable deadEffect: int,
  mutable writeOnly: int,
  mutable readOnly: int,
}

type cell = {mutable value: int}

let localPair = () => {
  let pair = {left: 10, right: 20}
  pair.left = pair.left + 1
  pair.right = pair.left + pair.right
  pair.left + pair.right
}

let capturedPair = () => {
  let pair = {left: 10, right: 20}
  let bumpLeft = () => pair.left = pair.left + 1
  bumpLeft()
  bumpLeft()
  pair.left + pair.right
}

let initializationOrder = () => {
  let seen = ref(list{})
  let initialize = value => {
    seen.contents = list{value, ...seen.contents}
    value
  }
  let pair = {left: initialize(1), right: initialize(2)}
  pair.left = pair.left + 1
  (pair.left + pair.right, List.toArray(List.reverse(seen.contents)))
}

@inline(never)
let consumePair = pair => pair.left + pair.right

let escapedPair = () => {
  let pair = {left: 10, right: 20}
  consumePair(pair)
}

let fieldUseCleanup = () => {
  let effects = ref(list{})
  let mark = value => {
    effects.contents = list{value, ...effects.contents}
    value
  }
  let fields = {
    live: 1,
    deadPure: 999,
    deadEffect: mark(2),
    writeOnly: mark(3),
    readOnly: mark(4),
  }
  fields.writeOnly = mark(5)
  fields.writeOnly = mark(6)
  fields.live = fields.live + fields.readOnly
  (fields.live, List.toArray(List.reverse(effects.contents)))
}

let overwrittenBeforeRead = () => {
  let effects = ref(list{})
  let mark = value => {
    effects.contents = list{value, ...effects.contents}
    value
  }
  let cell = {value: mark(1)}
  cell.value = mark(2)
  (cell.value, List.toArray(List.reverse(effects.contents)))
}

let capturedWriteOnly = () => {
  let effects = ref(list{})
  let mark = value => {
    effects.contents = list{value, ...effects.contents}
    value
  }
  let cell = {value: mark(1)}
  let write = () => cell.value = mark(2)
  write()
  List.toArray(List.reverse(effects.contents))
}

let uncalledWriteOnlyClosure = () => {
  let effects = ref(list{})
  let mark = value => {
    effects.contents = list{value, ...effects.contents}
    value
  }
  let cell = {value: mark(1)}
  let write = () => cell.value = mark(2)
  ignore(write)
  List.toArray(List.reverse(effects.contents))
}

let readOnlyFieldSnapshotsInitializer = () => {
  let source = ref(1)
  let cell = {value: source.contents}
  source.contents = 2
  cell.value
}

describe(__MODULE__, () => {
  test("scalarizes a local mutable record", () => eq(__LOC__, 42, localPair()))
  test("shares scalar fields with closures", () => eq(__LOC__, 32, capturedPair()))
  test("preserves initializer order", () => eq(__LOC__, (4, [1, 2]), initializationOrder()))
  test("retains an escaping record", () => eq(__LOC__, 30, escapedPair()))
  test("cleans up fields according to their uses", () =>
    eq(__LOC__, (5, [2, 3, 4, 5, 6]), fieldUseCleanup())
  )
  test("preserves overwritten initializer effects", () =>
    eq(__LOC__, (2, [1, 2]), overwrittenBeforeRead())
  )
  test("removes write-only fields captured by closures", () =>
    eq(__LOC__, [1, 2], capturedWriteOnly())
  )
  test("does not evaluate writes in uncalled closures", () =>
    eq(__LOC__, [1], uncalledWriteOnlyClosure())
  )
  test("read-only fields snapshot their initializer", () =>
    eq(__LOC__, 1, readOnlyFieldSnapshotsInitializer())
  )
})
