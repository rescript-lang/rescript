open Mocha
open Test_utils

// Arguments and literal elements are evaluated left to right, also when a later
// one is a block that compiles to statements.

type pair = {a: int, b: int}
type ops = {mutable f: int => int}

@val external makeObject: (int, int) => array<int> = "Array.of"
@obj external makeJsObject: (~a: int, ~b: int) => {"a": int, "b": int} = ""
@send external pushTo: (array<int>, int) => int = "push"

%%raw(`
globalThis.orderCounter = 0
globalThis.bumpOrderCounter = () => { globalThis.orderCounter++ }
globalThis.makeCounter = () => ({ v: 7, get(x) { return this.v + x } })
`)

@val external orderCounter: int = "orderCounter"
@val external bumpOrderCounter: unit => unit = "bumpOrderCounter"
let resetOrderCounter: unit => unit = %raw(`() => { globalThis.orderCounter = 0 }`)

type counter
@val external makeCounter: unit => counter = "makeCounter"
@send external counterGet: (counter, int) => int = "get"

type box = {@set "x": int}
let makeBox: int => box = %raw(`x => ({x})`)

// Opaque, so that the record stays a record instead of becoming a variable.
let makeOps: unit => ops = %raw(`() => ({f: x => x + 1})`)

let count = ref(0)

let topLevel = (
  count.contents,
  {
    count := count.contents + 1
    let c = count.contents
    c * 10
  },
)

describe(__MODULE__, () => {
  test("a read before a block that writes it, at the top level", () => {
    eq(__LOC__, (0, 10), topLevel)
  })

  test("a local ref that becomes a JS variable", () => {
    let n = ref(0)
    let pair = (
      n.contents,
      {
        n := n.contents + 1
        let m = n.contents
        m * 10
      },
    )
    eq(__LOC__, (0, 10), pair)
  })

  test("a local ref that a closure called in the block assigns", () => {
    let n = ref(0)
    let bump = () => n := n.contents + 1
    let pair = (
      n.contents,
      {
        bump()
        let m = n.contents
        m * 10
      },
    )
    eq(__LOC__, (0, 10), pair)
  })

  test("effects in arrays, tuples, records, dicts and objects", () => {
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    let arr = [
      mark("first", 1),
      {
        let v = mark("second", 2)
        v * v
      },
    ]
    let tup = (
      mark("first", 1),
      {
        let v = mark("second", 3)
        v * v
      },
    )
    let rec_ = {
      a: mark("first", 1),
      b: {
        let v = mark("second", 4)
        v * v
      },
    }
    let d = dict{
      "a": mark("first", 1),
      "b": {
        let v = mark("second", 5)
        v * v
      },
    }
    let o = makeJsObject(
      ~a=mark("first", 1),
      ~b={
        let v = mark("second", 6)
        v * v
      },
    )
    eq(
      __LOC__,
      [
        "first",
        "second",
        "first",
        "second",
        "first",
        "second",
        "first",
        "second",
        "first",
        "second",
      ],
      log,
    )
    eq(__LOC__, [1, 4], arr)
    eq(__LOC__, (1, 9), tup)
    eq(__LOC__, {a: 1, b: 16}, rec_)
    eq(__LOC__, dict{"a": 1, "b": 25}, d)
    eq(__LOC__, (1, 36), (o["a"], o["b"]))
  })

  test("calls of externals and of ReScript functions", () => {
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    let add = (x, y) => x + y
    let viaExternal = makeObject(
      mark("x", 1),
      {
        let v = mark("y", 2)
        v * v
      },
    )
    let viaFunction = add(
      mark("x", 1),
      {
        let v = mark("y", 2)
        v * v
      },
    )
    eq(__LOC__, ["x", "y", "x", "y"], log)
    eq(__LOC__, [1, 4], viaExternal)
    eq(__LOC__, 5, viaFunction)
  })

  test("a switch and a try as arguments", () => {
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    let viaSwitch = [
      mark("before", 0),
      switch mark("scrutinee", 1) {
      | 0 => mark("zero", 10)
      | 1 => mark("one", 11)
      | _ => mark("other", 12)
      },
    ]
    let viaTry = [
      mark("before", 0),
      try mark("body", 1) catch {
      | _ => 2
      },
    ]
    eq(__LOC__, ["before", "scrutinee", "one", "before", "body"], log)
    eq(__LOC__, [0, 11], viaSwitch)
    eq(__LOC__, [0, 1], viaTry)
  })

  test("the start of a for loop before a block in its end", () => {
    let r = ref(0)
    let visited = []
    for i in r.contents to {
      r := 2
      let e = r.contents
      e - 1
    } {
      visited->Array.push(i)
    }
    eq(__LOC__, [0, 1], visited)
  })

  test("a JS global read before a block that changes it", () => {
    resetOrderCounter()
    let pair = (
      orderCounter,
      {
        bumpOrderCounter()
        let m = orderCounter
        m * 10
      },
    )
    eq(__LOC__, (0, 10), pair)
  })

  test("the target of a JS object write before a block in the value", () => {
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    let o = ref(makeBox(0))
    let first = o.contents
    o.contents["x"] = {
      o := makeBox(100)
      let v = mark("v", 5)
      v * v
    }

    eq(__LOC__, (25, 100), (first["x"], o.contents["x"]))
  })

  test("a function read from a record field before the arguments", () => {
    let o = makeOps()
    let r = o.f({
      o.f = x => x + 100
      let y = Int.fromString("1")->Option.getOrThrow
      y * 2
    })
    eq(__LOC__, 3, r)
  })

  test("a function read from a ref before the arguments", () => {
    let fr = ref(x => x + 1)
    let r = fr.contents({
      fr := x => x + 100
      let y = Int.fromString("1")->Option.getOrThrow
      y * 2
    })
    eq(__LOC__, 3, r)
  })

  test("a held receiver is still passed as this", () => {
    let log = []
    let r = makeCounter()->counterGet({
      log->Array.push("arg")
      let y = log->Array.length
      y * 2
    })
    eq(__LOC__, 9, r)
  })

  test("a method keeps a receiver that stays in place", () => {
    let arr = [1]
    let length = arr->pushTo({
      arr->Array.push(2)
      let l = arr->Array.length
      l * 100
    })
    eq(__LOC__, 3, length)
    eq(__LOC__, [1, 2, 200], arr)
  })
})
