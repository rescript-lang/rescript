@@config({flags: ["-bs-jsx", "4"]})

open Mocha
open Test_utils

// An optional argument of an external whose presence is not known is
// evaluated once, in source order. A JS property read, which may run a getter,
// is read once, to test it for None and to unwrap it. An object-creating
// external assigns such an argument after the object literal is built, but
// evaluates it before the arguments that follow it when they have side
// effects, or read what its side effects change. The arguments before it, and
// optional arguments known to be present, stay in the literal.

@obj
external make: (~size: int=?, ~mode: string, unit) => {"size": option<int>, "mode": string} = ""

@obj
external makeBetween: (
  ~first: string,
  ~size: int=?,
  ~mode: string,
  unit,
) => {"first": string, "size": option<int>, "mode": string} = ""

@obj
external makeKnown: (
  ~first: int=?,
  ~size: int=?,
  ~mode: string,
  unit,
) => {"first": option<int>, "size": option<int>, "mode": string} = ""

@obj external makeSize: (~size: int=?, unit) => {"size": option<int>} = ""

@obj
external makeName: (
  ~name: string=?,
  ~mode: string,
  unit,
) => {"name": option<string>, "mode": string} = ""

@obj
external makeCounted: (
  ~count: int,
  ~name: string=?,
  ~mode: string,
  unit,
) => {"count": int, "name": option<string>, "mode": string} = ""

@obj
external makeLabelled: (
  ~label: string,
  ~name: string=?,
  ~mode: string,
  unit,
) => {"label": string, "name": option<string>, "mode": string} = ""

@obj
external makeLevel: (
  ~level: @int [#low | @as(10) #high]=?,
  ~mode: string,
  unit,
) => {"level": option<int>, "mode": string} = ""

@obj
external makeModeFirst: (
  ~mode: @string [@as("light-mode") #light | @as("dark-mode") #dark]=?,
  ~level: @int [#low | @as(10) #high],
  unit,
) => {"mode": option<string>, "level": int} = ""

type source

// [level] is present on the first read only.
let makeSource: unit => source = %raw(`() => ({
  reads: 0,
  value: "first",
  get name() {
    this.reads++
    return this.value
  },
  get level() {
    this.reads++
    return this.reads === 1 ? "high" : undefined
  }
})`)

@get external name: source => option<string> = "name"

type counter = {mutable count: int}

// Reading [bump] increments the count of the counter it was made with.
let makeBumper: counter => source = %raw(`c => ({
  get bump() {
    c.count = c.count + 1
    return "bumped"
  }
})`)

@get external bump: source => option<string> = "bump"
@get external level: source => option<[#low | #high]> = "level"
@get external reads: source => int = "reads"
@set external setValue: (source, string) => unit = "value"

let setGlobalSource: source => unit = %raw(`s => {
  globalThis.optionalExternalArgOnceSource = s
}`)

// A property read, though it compiles to a JS name.
@val external globalSourceName: option<string> = "optionalExternalArgOnceSource.name"

@val external globalName: option<string> = "optionalExternalArgOnceName"

let setGlobalName: string => unit = %raw(`v => {
  globalThis.optionalExternalArgOnceName = v
}`)

%%raw(`
function greet(name) {
  return name === undefined ? "Hello" : "Hello " + name
}
`)

@val external greet: (~name: string=?, unit) => string = "greet"

type target

let makeTarget: unit => target = %raw(`() => {
  const calls = []
  return { calls, setLevel: (...args) => { calls.push(args.map(String).join(" ")) } }
}`)

@get external calls: target => array<string> = "calls"

@send
external setLevel: (target, ~level: @int [#low | @as(10) #high]=?, unit) => unit = "setLevel"

@get external elementKey: React.element => Nullable.t<string> = "key"

type item = {label: string, mutable name: option<string>}

// Calls the function, which the optimizer cannot inline then.
let call: (unit => 'a) => 'a = %raw(`f => f()`)

let keys = (o: {..}) => Object.keysToArray(Obj.magic(o))

describe(__MODULE__, () => {
  test("a getter passed as an optional argument", () => {
    let s = makeSource()
    eq(__LOC__, ("Hello first", 1), (greet(~name=?name(s), ()), reads(s)))

    let s = makeSource()
    let t = makeTarget()
    t->setLevel(~level=?level(s), ())
    eq(__LOC__, (["10"], 1), (calls(t), reads(s)))

    let s = makeSource()
    let key = <li key=?{name(s)}>{React.string("item")}</li>->elementKey
    eq(__LOC__, (Some("first"), 1), (key->Nullable.toOption, reads(s)))

    let s = makeSource()
    let o = makeName(~name=?name(s), ~mode="m", ())
    eq(__LOC__, (Some("first"), 1), (o["name"], reads(s)))
  })

  test("a global with a dot passed as an optional argument", () => {
    let s = makeSource()
    setGlobalSource(s)
    eq(__LOC__, ("Hello first", 1), (greet(~name=?globalSourceName, ()), reads(s)))

    let s = makeSource()
    setGlobalSource(s)
    let o = makeName(~name=?globalSourceName, ~mode="m", ())
    eq(__LOC__, (Some("first"), 1), (o["name"], reads(s)))
  })

  test("an optional field before a field with side effects", () => {
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    let o = make(~size=?mark("size", Some(1)), ~mode=mark("mode", "dark"), ())
    eq(__LOC__, ["size", "mode"], log)
    eq(__LOC__, (Some(1), "dark"), (o["size"], o["mode"]))
  })

  test("an optional field between fields with side effects", () => {
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    let o = makeBetween(
      ~first=mark("first", "a"),
      ~size=?mark("size", None),
      ~mode=mark("mode", "b"),
      (),
    )
    eq(__LOC__, ["first", "size", "mode"], log)
    eq(__LOC__, ("a", None, "b"), (o["first"], o["size"], o["mode"]))
  })

  test("an optional field before a unit argument with side effects", () => {
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    let o = makeSize(~size=?mark("size", Some(2)), mark("unit", ()))
    eq(__LOC__, ["size", "unit"], log)
    eq(__LOC__, Some(2), o["size"])
  })

  test("an optional field known to be present stays in the literal", () => {
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    let o = makeKnown(
      ~first=mark("first", 1),
      ~size=?mark("size", Some(2)),
      ~mode=mark("mode", "m"),
      (),
    )
    eq(__LOC__, ["first", "size", "mode"], log)
    eq(__LOC__, ["first", "mode", "size"], keys(o))
  })

  test("a getter that a later field changes", () => {
    let s = makeSource()
    let o = makeName(
      ~name=?name(s),
      ~mode=call(
        () => {
          setValue(s, "second")
          "getter"
        },
      ),
      (),
    )
    eq(__LOC__, Some("first"), o["name"])
  })

  test("a field before a getter that changes it", () => {
    let c = {count: 1}
    let s = makeBumper(c)
    let o = makeCounted(~count=c.count, ~name=?bump(s), ~mode=call(() => "m"), ())
    eq(__LOC__, (1, Some("bumped"), 2), (o["count"], o["name"], c.count))
  })

  test("a global that a later field changes", () => {
    setGlobalName("first")
    let o = makeName(
      ~name=?globalName,
      ~mode=call(
        () => {
          setGlobalName("second")
          "global"
        },
      ),
      (),
    )
    eq(__LOC__, Some("first"), o["name"])
  })

  test("a dict entry that a later field changes", () => {
    let d = dict{"k": "first"}
    let o = makeName(
      ~name=?d->Dict.get("k"),
      ~mode=call(
        () => {
          d->Dict.delete("k")
          "dict"
        },
      ),
      (),
    )
    eq(__LOC__, Some("first"), o["name"])
  })

  test("an array element that a later field changes", () => {
    let a = ["first"]
    let o = makeName(
      ~name=?a->Array.get(0),
      ~mode=call(
        () => {
          a[0] = "second"
          "array"
        },
      ),
      (),
    )
    eq(__LOC__, Some("first"), o["name"])
  })

  test("a ref that a later field changes", () => {
    let r = ref(Some("first"))
    let o = makeName(
      ~name=?r.contents,
      ~mode=call(
        () => {
          r := None
          "ref"
        },
      ),
      (),
    )
    eq(__LOC__, Some("first"), o["name"])
  })

  test("a record field that a later field changes", () => {
    let item = call(() => {label: "item", name: Some("first")})
    let o = makeLabelled(
      ~label=item.label,
      ~name=?item.name,
      ~mode=call(
        () => {
          item.name = None
          "record"
        },
      ),
      (),
    )
    eq(__LOC__, ("item", Some("first"), "record"), (o["label"], o["name"], o["mode"]))
  })

  test("optional polymorphic variant fields", () => {
    let level = ref(Some(#high))
    let o = makeLevel(
      ~level=?level.contents,
      ~mode=call(
        () => {
          level := Some(#low)
          "level"
        },
      ),
      (),
    )
    eq(__LOC__, (Some(10), "level"), (o["level"], o["mode"]))

    let mode = ref(Some(#light))
    let o = makeModeFirst(
      ~mode=?mode.contents,
      ~level=call(
        () => {
          mode := Some(#dark)
          #high
        },
      ),
      (),
    )
    eq(__LOC__, (Some("light-mode"), 10), (o["mode"], o["level"]))
  })
})
