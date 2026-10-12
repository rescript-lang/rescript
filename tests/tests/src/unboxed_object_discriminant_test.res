open Mocha
open Test_utils

// Both ordinary enums and unboxed enums have finite string runtime domains.
type startKind = | @as("start") StartKind
@unboxed type stopKind = | @as("stop") StopKind | @as("halt") HaltKind
type stopAlias = stopKind

type start = {kind: startKind, value: int}
type stop = {kind: stopAlias, reason: string}

@unboxed type named = Start(start) | Stop(stop)
module Extended = {
  @unboxed type t = Start(start) | Stop(stop) | Text(string)
}
let extend = (value: named) => (value :> Extended.t)
@inline(always)
let namedValue = (value: named) =>
  switch value {
  | Start({value}) => value
  | Stop({reason}) => String.length(reason)
  }

@unboxed
type inline =
  | Begin({kind: startKind, value: int})
  | End({kind: stopKind, reason: string})
@inline(always)
let inlineValue = value =>
  switch value {
  | Begin({value}) => value
  | End({reason}) => String.length(reason)
  }

// Source field names can differ when their JavaScript property agrees.
type renamed = {@as("event-kind") tag: startKind, value: int}
@unboxed
type mixed =
  | Named(renamed)
  | Inline({@as("event-kind") kind: stopKind, reason: string})
@inline(always)
let mixedValue = value =>
  switch value {
  | Named({value}) => value
  | Inline({reason}) => String.length(reason)
  }

@unboxed
type neighbors =
  | @as(null) Null
  | @as(undefined) Missing
  | Text(string)
  | Pair((int, int))
  | Started(start)
  | Stopped(stop)
@inline(always)
let neighborValue = value =>
  switch value {
  | Null => -1
  | Missing => -2
  | Text(text) => String.length(text)
  | Pair((left, right)) => left + right
  | Started({value}) => value
  | Stopped({reason}) => String.length(reason)
  }

let partialNeighbor = (value: neighbors) =>
  switch value {
  | Started({value}) => value
  | _ => -1
  }

type pauseKind = PauseKind
type pause = {kind: pauseKind, value: int}
@unboxed type three = Go(start) | Finish(stop) | Pause(pause)
let threeValue = value =>
  switch value {
  | Go({value}) => value
  | Finish({reason}) if String.length(reason) > 3 => 20
  | Finish(_) => 10
  | Pause({value}) => -value
  }

// An omitted string literal can expose an indexed property too; property
// discrimination must still check that it received an object.
type firstKind = | @as("f") FirstKind
type secondKind = | @as("s") SecondKind
@unboxed
type indexed =
  | @as("first") Literal
  | First({@as("0") kind: firstKind})
  | Second({@as("0") kind: secondKind})
let indexedValue = value =>
  switch value {
  | First(_) => 1
  | _ => 0
  }

module OptionalObject = {
  @unboxed type t = | @as(undefined) Missing | First(start) | Second(stop)
  let value = value =>
    switch value {
    | First({value}) => value
    | _ => -1
    }
}

module RecursiveObject = {
  // The object layout is inferred before these later declarations resolve.
  @unboxed
  type rec t = Leaf(leaf) | Branch({kind: branchKind, tail: t})
  and leaf = {kind: leafKind, value: int}
  and leafKind = LeafKind
  and branchKind = BranchKind

  let rec value = tree =>
    switch tree {
    | Leaf({value}) => value
    | Branch({tail}) => 1 + value(tail)
    }
}

let guardedValue = (value: named) =>
  switch value {
  | Start({value}) if value > 10 => "large"
  | Start(_) => "small"
  | Stop({kind: HaltKind}) => "halted"
  | Stop(_) => "stopped"
  }

// Concrete substitutions in generic records and a later valid candidate
// must work even when an earlier shared property cannot discriminate.
type genericRecord<'kind> = {kind: 'kind, value: int}
@unboxed
type generic =
  | GenericStart(genericRecord<startKind>)
  | GenericStop(genericRecord<stopAlias>)
let genericValue = value =>
  switch value {
  | GenericStart({value}) | GenericStop({value}) => value
  }

type candidateStart = {a: string, z: startKind, value: int}
type candidateStop = {a: string, z: stopKind, value: int}
@unboxed type candidate = CandidateStart(candidateStart) | CandidateStop(candidateStop)
let candidateValue = value =>
  switch value {
  | CandidateStart({value}) => value
  | CandidateStop({value}) => -value
  }

@inline(never) let id = value => value

// The external boundary must use the same representation as construction.
let externalNamed: unit => named = %raw(`() => ({kind: "halt", reason: "done"})`)
let externalInline: unit => inline = %raw(`() => ({kind: "start", value: 9})`)
let externalMixed: unit => mixed = %raw(`() => ({"event-kind": "stop", reason: "ok"})`)

module GetterFinite = {
  type manyKind = | @as("a") A | @as("b") B
  type otherKind = | @as("c") C
  @unboxed type t = Many({kind: manyKind, n: int}) | Other({kind: otherKind, n: int})
  let complete = value =>
    switch value {
    | Many(_) => 1
    | Other(_) => 2
    }
  let partial = value =>
    switch value {
    | Many(_) => 1
    | _ => -1
    }
  let guarded = value =>
    switch value {
    | Many({n}) if n > 10 => 3
    | Many(_) => 1
    | Other(_) => 2
    }
  @unboxed
  type mixed =
    | Many({kind: manyKind, n: int})
    | Other({kind: otherKind, n: int})
    | @as(null) Null
    | @as(undefined) Missing
    | Text(string)
    | Pair((int, int))
  let mixedPartial = value =>
    switch value {
    | Many(_) => 1
    | _ => -1
    }
  let mixedTwo = value =>
    switch value {
    | Many(_) => 1
    | Pair(_) => 4
    | _ => -1
    }
  let mixedComplete = value =>
    switch value {
    | Many(_) => 1
    | Other(_) => 2
    | Null => 0
    | Missing => -2
    | Text(_) => 3
    | Pair(_) => 4
    }
  let nested = (value: t) =>
    switch value {
    | Many({kind: A}) => 1
    | Many({kind: B}) => 2
    | Other(_) => 3
    }
  let nestedGuarded = (value: t, guard) =>
    switch value {
    | Many({kind: B}) if guard() => 0
    | Many({kind: A}) => 1
    | Many({kind: B}) => 2
    | Other(_) => 3
    }
  let nestedOrGuarded = (value: mixed, guard) =>
    switch value {
    | Many({kind: B, n: 12}) | Other({n: 12}) if guard() => 0
    | Many({kind: A}) => 1
    | Many({kind: B}) => 2
    | Other(_) => 3
    | Null | Missing | Text(_) | Pair(_) => -1
    }
  // Large identical fallback actions share a handler after the constructor
  // switch disappears; their pattern projection still needs this match's read.
  @inline(never)
  let projectionFallback = (value: t, guard) =>
    switch value {
    | Many({kind: B, n: 12}) | Other({n: 12}) if guard() => []
    | Many({kind}) => [
        kind :> string,
        "0",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9",
        "10",
        "11",
        "12",
        "13",
        "14",
      ]
    | Other({kind}) => [
        kind :> string,
        "0",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9",
        "10",
        "11",
        "12",
        "13",
        "14",
      ]
    }

  type mutableMany = {kind: manyKind, mutable n: int}
  @unboxed
  type mutableProjection =
    | MutableMany(mutableMany)
    | MutableOther({kind: otherKind, mutable n: int})
  @inline(never)
  let projectionMutable = (value, guard) =>
    switch value {
    | MutableMany({kind: B, n: 12}) | MutableOther({n: 12}) if guard() => []
    | MutableMany({kind, n}) => [
        kind :> string,
        Int.toString(n),
        "0",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9",
        "10",
        "11",
        "12",
        "13",
        "14",
      ]
    | MutableOther({kind, n}) => [
        kind :> string,
        Int.toString(n),
        "0",
        "1",
        "2",
        "3",
        "4",
        "5",
        "6",
        "7",
        "8",
        "9",
        "10",
        "11",
        "12",
        "13",
        "14",
      ]
    }

  type manyRecord = {kind: manyKind, n: int}
  @unboxed type named = Named(manyRecord) | Other({kind: otherKind, n: int})
  @inline(always)
  let nestedNamed = (value: named) =>
    switch value {
    | Named({kind: A}) => 1
    | Named({kind: B}) => 2
    | Other(_) => 3
    }
  let nestedMixed = (value: mixed) =>
    switch value {
    | Many({kind: A}) => 1
    | Many({kind: B}) => 2
    | Other(_) => 3
    | Null | Missing | Text(_) | Pair(_) => -1
    }
  let nestedPartial = (value: mixed) =>
    switch value {
    | Many({kind: B}) => 2
    | _ => -1
    }
  let bodyRead = (value: named, effect) =>
    switch value {
    | Named(record) =>
      effect()
      record.kind == A
    | Other(_) => false
    }
  let bodyMatch = (value: named, effect) =>
    switch value {
    | Named(record) =>
      effect()
      switch record {
      | {kind: A} => true
      | _ => false
      }
    | Other(_) => false
    }
  let bodyDestructure = (value: named, effect) =>
    switch value {
    | Named(record) =>
      effect()
      let {kind} = record
      kind == A
    | Other(_) => false
    }
  let bodyClosure = (value: named) =>
    switch value {
    | Named(record) =>
      () =>
        switch record {
        | {kind: A} => true
        | _ => false
        }
    | Other(_) => () => false
    }
  @unboxed type broad = Broad({kind: string, n: int}) | Number({kind: int, n: int})
  let nestedBroad = value =>
    switch value {
    | Broad({kind: "a"}) => 1
    | Broad({kind: "b"}) => 2
    | Broad(_) => 3
    | Number(_) => 4
    }
  let nestedNext: (unit => unit, bool) => 'value = %raw(`(effect, changing) => {
    let reads = 0;
    return {get kind() {
      effect();
      reads++;
      return changing && reads > 1 ? "a" : "b";
    }, n: 12};
  }`)
  let next: (unit => unit, bool) => 'value = %raw(`(effect, changing) => {
    let reads = 0;
    return {get kind() {
      effect();
      reads++;
      return changing && reads > 1 ? "c" : "b";
    }, n: 12};
  }`)
}

describe(__MODULE__, () => {
  test("named, inline, and mixed objects retain their payloads", () => {
    eq(__LOC__, namedValue(Start({kind: StartKind, value: 7})), 7)
    eq(__LOC__, namedValue(Stop({kind: StopKind, reason: "done"})), 4)
    eq(__LOC__, namedValue(Stop({kind: HaltKind, reason: "halt"})), 4)
    eq(__LOC__, inlineValue(Begin({kind: StartKind, value: 8})), 8)
    eq(__LOC__, inlineValue(End({kind: HaltKind, reason: "end"})), 3)
    eq(__LOC__, mixedValue(Named({tag: StartKind, value: 6})), 6)
    eq(__LOC__, mixedValue(Inline({kind: StopKind, reason: "ok"})), 2)
    eq(__LOC__, namedValue(externalNamed()), 4)
    eq(__LOC__, inlineValue(externalInline()), 9)
    eq(__LOC__, mixedValue(externalMixed()), 2)
    let extended = extend(id(Start({kind: StartKind, value: 14})))
    eq(
      __LOC__,
      switch extended {
      | Extended.Start({value}) => value
      | Stop(_) | Text(_) => -1
      },
      14,
    )
    eq(__LOC__, genericValue(id(GenericStart({kind: StartKind, value: 11}))), 11)
    eq(__LOC__, genericValue(id(GenericStop({kind: HaltKind, value: 12}))), 12)
    eq(__LOC__, candidateValue(id(CandidateStart({a: "same", z: StartKind, value: 3}))), 3)
    eq(__LOC__, candidateValue(id(CandidateStop({a: "same", z: StopKind, value: 3}))), -3)
  })

  test("folding agrees with runtime dispatch", () => {
    eq(
      __LOC__,
      namedValue(Start({kind: StartKind, value: 7})),
      namedValue(id(Start({kind: StartKind, value: 7}))),
    )
    eq(
      __LOC__,
      namedValue(Stop({kind: HaltKind, reason: "done"})),
      namedValue(id(Stop({kind: HaltKind, reason: "done"}))),
    )
    eq(
      __LOC__,
      inlineValue(End({kind: StopKind, reason: "end"})),
      inlineValue(id(End({kind: StopKind, reason: "end"}))),
    )
    eq(
      __LOC__,
      mixedValue(Named({tag: StartKind, value: 6})),
      mixedValue(id(Named({tag: StartKind, value: 6}))),
    )
    eq(
      __LOC__,
      mixedValue(Inline({kind: HaltKind, reason: "ok"})),
      mixedValue(id(Inline({kind: HaltKind, reason: "ok"}))),
    )
  })

  test("object discrimination coexists with null, strings, and arrays", () => {
    eq(__LOC__, neighborValue(id(Null)), -1)
    eq(__LOC__, neighborValue(id(Missing)), -2)
    eq(__LOC__, neighborValue(id(Text("text"))), 4)
    eq(__LOC__, neighborValue(id(Pair((2, 3)))), 5)
    eq(__LOC__, neighborValue(id(Started({kind: StartKind, value: 8}))), 8)
    eq(__LOC__, neighborValue(id(Stopped({kind: HaltKind, reason: "end"}))), 3)
    eq(__LOC__, neighborValue(Started({kind: StartKind, value: 8})), 8)
    eq(__LOC__, neighborValue(Pair((2, 3))), 5)
    eq(__LOC__, neighborValue(Null), -1)
    eq(__LOC__, partialNeighbor(id(Null)), -1)
    eq(__LOC__, partialNeighbor(id(Missing)), -1)
    eq(__LOC__, partialNeighbor(id(Pair((2, 3)))), -1)
    eq(__LOC__, partialNeighbor(id(Text("text"))), -1)
    eq(__LOC__, partialNeighbor(id(Stopped({kind: HaltKind, reason: "done"}))), -1)
    eq(__LOC__, partialNeighbor(id(Started({kind: StartKind, value: 8}))), 8)
    eq(__LOC__, indexedValue(id(Literal)), 0)
    eq(__LOC__, indexedValue(id(First({kind: FirstKind}))), 1)
    eq(__LOC__, indexedValue(id(Second({kind: SecondKind}))), 0)
    eq(__LOC__, OptionalObject.value(id(OptionalObject.Missing)), -1)
    eq(__LOC__, OptionalObject.value(id(OptionalObject.First({kind: StartKind, value: 8}))), 8)
    eq(__LOC__, OptionalObject.value(id(OptionalObject.Second({kind: StopKind, reason: "ok"}))), -1)
  })

  test("guards preserve fallthrough within an object case", () => {
    eq(__LOC__, guardedValue(id(Start({kind: StartKind, value: 20}))), "large")
    eq(__LOC__, guardedValue(id(Start({kind: StartKind, value: 2}))), "small")
    eq(__LOC__, guardedValue(id(Stop({kind: HaltKind, reason: "done"}))), "halted")
    eq(__LOC__, guardedValue(id(Stop({kind: StopKind, reason: "done"}))), "stopped")
    eq(__LOC__, threeValue(id(Go({kind: StartKind, value: 1}))), 1)
    eq(__LOC__, threeValue(id(Finish({kind: StopKind, reason: "long"}))), 20)
    eq(__LOC__, threeValue(id(Finish({kind: HaltKind, reason: "ok"}))), 10)
    eq(__LOC__, threeValue(id(Pause({kind: PauseKind, value: 3}))), -3)
  })

  test("recursive object layouts resolve later enum declarations", () => {
    open RecursiveObject
    eq(__LOC__, value(Leaf({kind: LeafKind, value: 5})), 5)
    eq(__LOC__, value(id(Leaf({kind: LeafKind, value: 5}))), 5)
    eq(__LOC__, value(Branch({kind: BranchKind, tail: Leaf({kind: LeafKind, value: 5})})), 6)
    eq(__LOC__, value(id(Branch({kind: BranchKind, tail: Leaf({kind: LeafKind, value: 5})}))), 6)
  })

  test("finite string discriminators read getters once in every dispatch path", () => {
    open GetterFinite
    let calls = ref(0)
    let effect = () => calls.contents = calls.contents + 1
    [false, true]->Array.forEach(
      changing => {
        calls.contents = 0
        eq(__LOC__, complete(next(effect, changing)), 1)
        eq(__LOC__, calls.contents, 1)
        calls.contents = 0
        eq(__LOC__, partial(next(effect, changing)), 1)
        eq(__LOC__, calls.contents, 1)
        calls.contents = 0
        eq(__LOC__, guarded(next(effect, changing)), 3)
        eq(__LOC__, calls.contents, 1)
        calls.contents = 0
        eq(__LOC__, mixedPartial(next(effect, changing)), 1)
        eq(__LOC__, calls.contents, 1)
        calls.contents = 0
        eq(__LOC__, mixedTwo(next(effect, changing)), 1)
        eq(__LOC__, calls.contents, 1)
        calls.contents = 0
        eq(__LOC__, mixedComplete(next(effect, changing)), 1)
        eq(__LOC__, calls.contents, 1)
      },
    )
    eq(__LOC__, mixedPartial(id(Null)), -1)
    eq(__LOC__, mixedPartial(id(Missing)), -1)
    eq(__LOC__, mixedPartial(id(Text("b"))), -1)
    eq(__LOC__, mixedPartial(id(Pair((1, 2)))), -1)
    eq(__LOC__, mixedTwo(id(Pair((1, 2)))), 4)
    eq(__LOC__, mixedComplete(id(Null)), 0)
    eq(__LOC__, mixedComplete(id(Missing)), -2)
  })

  test("nested payload patterns share only their match discriminator snapshot", () => {
    open GetterFinite
    let calls = ref(0)
    let effect = () => calls.contents = calls.contents + 1
    [false, true]->Array.forEach(
      changing => {
        calls.contents = 0
        eq(__LOC__, nested(nestedNext(effect, changing)), 2)
        eq(__LOC__, calls.contents, 1)
        calls.contents = 0
        eq(__LOC__, nestedGuarded(nestedNext(effect, changing), () => false), 2)
        eq(__LOC__, calls.contents, 1)
        calls.contents = 0
        eq(__LOC__, nestedBroad(nestedNext(effect, changing)), 2)
        eq(__LOC__, calls.contents, 1)
        calls.contents = 0
        eq(__LOC__, nestedOrGuarded(nestedNext(effect, changing), () => false), 2)
        eq(__LOC__, calls.contents, 1)
        calls.contents = 0
        eq(__LOC__, nestedNamed(nestedNext(effect, changing)), 2)
        eq(__LOC__, calls.contents, 1)
        calls.contents = 0
        eq(__LOC__, nestedMixed(nestedNext(effect, changing)), 2)
        eq(__LOC__, calls.contents, 1)
        calls.contents = 0
        eq(__LOC__, nestedPartial(nestedNext(effect, changing)), 2)
        eq(__LOC__, calls.contents, 1)
      },
    )
    calls.contents = 0
    let value = nestedNext(effect, true)
    eq(__LOC__, nestedOrGuarded(value, () => false), 2)
    eq(__LOC__, nestedOrGuarded(value, () => false), 1)
    eq(__LOC__, calls.contents, 2)
    calls.contents = 0
    for _iteration in 0 to 2 {
      eq(__LOC__, nestedOrGuarded(nestedNext(effect, true), () => false), 2)
    }
    eq(__LOC__, calls.contents, 3)
    eq(__LOC__, nestedOrGuarded(id(Null), () => false), -1)
    eq(__LOC__, nestedOrGuarded(id(Missing), () => false), -1)
    eq(__LOC__, nestedOrGuarded(id(Text("b")), () => false), -1)
    eq(__LOC__, nestedOrGuarded(id(Pair((1, 2))), () => false), -1)
    calls.contents = 0
    eq(__LOC__, bodyRead(nestedNext(effect, true), effect), true)
    eq(__LOC__, calls.contents, 3)
    calls.contents = 0
    eq(__LOC__, bodyMatch(nestedNext(effect, true), effect), true)
    eq(__LOC__, calls.contents, 3)
    calls.contents = 0
    eq(__LOC__, bodyDestructure(nestedNext(effect, true), effect), true)
    eq(__LOC__, calls.contents, 3)
    calls.contents = 0
    let read = bodyClosure(nestedNext(effect, true))
    eq(__LOC__, calls.contents, 1)
    eq(__LOC__, read(), true)
    eq(__LOC__, calls.contents, 2)
    eq(__LOC__, nestedPartial(id(Null)), -1)
    eq(__LOC__, nestedPartial(id(Text("b"))), -1)
  })

  test("shared projection-only fallbacks preserve stable getter snapshots", () => {
    open GetterFinite
    let calls = ref(0)
    let guards = ref(0)
    let effect = () => calls.contents = calls.contents + 1
    let guard = () => {
      guards.contents = guards.contents + 1
      false
    }
    let result = projectionFallback(nestedNext(effect, false), guard)
    eq(__LOC__, result[0], Some("b"))
    eq(__LOC__, Array.length(result), 16)
    eq(__LOC__, calls.contents, 1)
    eq(__LOC__, guards.contents, 1)
  })

  test("shared projection-only fallbacks preserve changing getter snapshots", () => {
    open GetterFinite
    let calls = ref(0)
    let effect = () => calls.contents = calls.contents + 1
    let value = nestedNext(effect, true)
    eq(__LOC__, projectionFallback(value, () => false)->Array.get(0), Some("b"))
    eq(__LOC__, calls.contents, 1)
    eq(__LOC__, projectionFallback(value, () => false)->Array.get(0), Some("a"))
    eq(__LOC__, calls.contents, 2)
  })

  test("shared fallback snapshots preserve fresh unrelated mutable payload fields", () => {
    open GetterFinite
    let calls = ref(0)
    let effect = () => calls.contents = calls.contents + 1
    let payload: mutableMany = nestedNext(effect, true)
    let result = projectionMutable(
      MutableMany(payload),
      () => {
        payload.n = 13
        false
      },
    )
    eq(__LOC__, result[0], Some("b"))
    eq(__LOC__, result[1], Some("13"))
    eq(__LOC__, calls.contents, 1)
    eq(__LOC__, payload.n, 13)
  })

  test("successful guarded matches skip the shared projection-only fallback", () => {
    open GetterFinite
    let calls = ref(0)
    let guards = ref(0)
    let effect = () => calls.contents = calls.contents + 1
    let guard = () => {
      guards.contents = guards.contents + 1
      true
    }
    eq(__LOC__, projectionFallback(nestedNext(effect, true), guard), [])
    eq(__LOC__, calls.contents, 1)
    eq(__LOC__, guards.contents, 1)
  })

  test("construction and matching evaluate effects once", () => {
    let calls = ref(0)
    let value = Start({
      kind: StartKind,
      value: {
        calls.contents = calls.contents + 1
        12
      },
    })
    eq(__LOC__, namedValue(value), 12)
    eq(__LOC__, calls.contents, 1)
    let answer = guardedValue({
      calls.contents = calls.contents + 1
      id(value)
    })
    eq(__LOC__, answer, "large")
    eq(__LOC__, calls.contents, 2)
    let next: (unit => unit) => neighbors = %raw(`effect => {
      effect();
      return {kind: "stop", reason: "done"};
    }`)
    eq(__LOC__, neighborValue(next(() => calls.contents = calls.contents + 1)), 4)
    eq(__LOC__, calls.contents, 3)
  })
})
