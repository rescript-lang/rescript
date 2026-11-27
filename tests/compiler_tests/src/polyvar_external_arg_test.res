open Mocha
open Test_utils

@module("mocha")
external testAsync: (string, unit => promise<unit>) => unit = "test"

// A polymorphic variant argument of an external is evaluated once, in its
// place among the arguments, and not inside a function the compiler creates.
// An event-style argument is passed as its tag and its payload, and an @as or
// @int dispatch switches on the argument in a function, so an argument with
// side effects is read from a temporary, as is a getter passed as an
// event-style argument. In a while condition, the temporary is bound inside
// the loop.

type target

let makeTarget: unit => target = %raw(`() => {
  const calls = []
  const record = (...args) => {
    calls.push(args.map(a => typeof a === "function" ? a(1) : String(a)).join(" "))
  }
  const has = (...args) => {
    record(...args)
    return args[0] === "click"
  }
  return { calls, has, on: record, onAfter: record, onBefore: record, setMode: record, setLevel: record, setLevelOpt: record }
}`)

@get external calls: target => array<string> = "calls"

@send
external on: (target, @string [#click(int => string) | #keydown(int => string)]) => unit = "on"

@send
external has: (target, @string [#click(int => string) | #keydown(int => string)]) => bool = "has"

@send
external onAs: (
  target,
  @string [@as("click!") #click(int => string) | @as("keydown!") #keydown(int => string)],
) => unit = "on"

@send
external setMode: (target, @string [@as("light-mode") #light | @as("dark-mode") #dark]) => unit =
  "setMode"

@send external setLevel: (target, @int [#low | @as(10) #high]) => unit = "setLevel"

@send
external setLevelOpt: (target, ~level: @int [#low | @as(10) #high]=?, unit) => unit = "setLevelOpt"

@send
external onAfter: (
  target,
  string,
  @string [#click(int => string) | #keydown(int => string)],
) => unit = "onAfter"

@send
external onBefore: (
  target,
  @string [#click(int => string) | #keydown(int => string)],
  string,
) => unit = "onBefore"

@obj
external makeOptions: (
  ~level: @int [#low | @as(10) #high],
  ~mode: @string [@as("light-mode") #light | @as("dark-mode") #dark]=?,
  unit,
) => _ = ""

type source

let makeSource: unit => source = %raw(`() => ({
  reads: 0,
  get current() {
    this.reads++
    return { NAME: "keydown", VAL: x => "keydown " + x }
  }
})`)

@get external current: source => [#click(int => string) | #keydown(int => string)] = "current"
@get external reads: source => int = "reads"

@val
external globalEvent: [#click(int => string) | #keydown(int => string)] = "polyvarExternalArgEvent"

let defineGlobalEvent: unit => unit => int = %raw(`() => {
  let reads = 0
  Object.defineProperty(globalThis, "polyvarExternalArgEvent", {
    configurable: true,
    get() {
      reads++
      return { NAME: "click", VAL: x => "click " + x }
    }
  })
  return () => reads
}`)

// Opaque, so that the optimizer cannot tell the argument is a literal.
let opaque: 'a => 'a = %raw(`x => x`)

external toOption: undefined<string> => option<string> = "%identity"

let click = x => "click " ++ Int.toString(x)
let keydown = x => "keydown " ++ Int.toString(x)

let lastEvent = ref(opaque(#keydown(keydown)))

describe(__MODULE__, () => {
  test("an event-style argument with side effects", () => {
    let t = makeTarget()
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    on(t, mark("event", opaque(#keydown(keydown))))
    onAs(t, mark("dispatch", opaque(#click(click))))
    eq(__LOC__, ["event", "dispatch"], log)
    eq(__LOC__, ["keydown keydown 1", "click! click 1"], calls(t))
  })

  test("a literal event with a payload that has side effects", () => {
    let t = makeTarget()
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    on(t, #click(mark("payload", click)))
    eq(__LOC__, ["payload"], log)
    eq(__LOC__, ["click click 1"], calls(t))
  })

  test("@string and @int arguments with side effects in temporaries", () => {
    let t = makeTarget()
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    setMode(t, mark("mode", opaque(#dark)))
    setLevel(t, mark("level", opaque(#high)))
    setLevelOpt(t, ~level=?mark("optional", opaque(Some(#low))), ())
    eq(__LOC__, ["mode", "level", "optional"], log)
    eq(__LOC__, ["dark-mode", "10", "0"], calls(t))
  })

  test("the arguments around an event-style argument run in order", () => {
    let t = makeTarget()
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    onAfter(t, mark("first", "a"), mark("second", opaque(#click(click))))
    onBefore(t, mark("third", opaque(#keydown(keydown))), mark("fourth", "b"))
    eq(__LOC__, ["first", "second", "third", "fourth"], log)
    eq(__LOC__, ["a click click 1", "keydown keydown 1 b"], calls(t))
  })

  test("getters read as event-style arguments", () => {
    let t = makeTarget()
    let s = makeSource()
    on(t, current(s))
    eq(__LOC__, 1, reads(s))
    let globalReads = defineGlobalEvent()
    on(t, globalEvent)
    eq(__LOC__, 1, globalReads())
    eq(__LOC__, ["keydown keydown 1", "click click 1"], calls(t))
  })

  test("refs read as event-style arguments", () => {
    let t = makeTarget()
    let r = ref(opaque(#click(click)))
    on(t, r.contents)
    r := opaque(#keydown(keydown))
    onAs(t, r.contents)
    on(t, lastEvent.contents)
    lastEvent := opaque(#click(click))
    onAs(t, lastEvent.contents)
    eq(
      __LOC__,
      ["click click 1", "keydown! keydown 1", "keydown keydown 1", "click! click 1"],
      calls(t),
    )
  })

  testAsync("awaited arguments", async () => {
    let t = makeTarget()
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    on(t, await mark("event", Promise.resolve(opaque(#keydown(keydown)))))
    onAs(t, await mark("dispatch", Promise.resolve(opaque(#click(click)))))
    setMode(t, await mark("mode", Promise.resolve(opaque(#light))))
    setLevel(t, await mark("level", Promise.resolve(opaque(#high))))
    eq(__LOC__, ["event", "dispatch", "mode", "level"], log)
    eq(__LOC__, ["keydown keydown 1", "click! click 1", "light-mode", "10"], calls(t))
  })

  test("a while condition with a temporary, and break and continue", () => {
    let t = makeTarget()
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    let i = ref(0)
    let seen = []
    while i.contents < 4 {
      i := i.contents + 1
      let j = ref(0)
      while {
        if i.contents == 3 {
          break
        }
        j := j.contents + 1
        j.contents <= i.contents && has(t, mark("event", opaque(#click(click))))
      } {
        if j.contents == 2 {
          continue
        }
        seen->Array.push((i.contents, j.contents))
      }
    }
    eq(__LOC__, 3, i.contents)
    eq(__LOC__, [(1, 1), (2, 1)], seen)
    eq(__LOC__, ["event", "event", "event"], log)
  })

  testAsync("arguments in a while condition that awaits", async () => {
    let t = makeTarget()
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    let n = ref(0)
    while n.contents < 3 && has(t, await mark("event", Promise.resolve(opaque(#click(click))))) {
      n := n.contents + 1
    }
    eq(__LOC__, 3, n.contents)
    eq(__LOC__, ["event", "event", "event"], log)

    // An argument without side effects stays in the condition
    let events = [opaque(#click(click)), opaque(#keydown(keydown))]
    let i = ref(0)
    while (
      (await Promise.resolve(true)) &&
      i.contents < Array.length(events) &&
      has(t, events->Array.getUnsafe(i.contents))
    ) {
      i := i.contents + 1
    }
    eq(__LOC__, 1, i.contents)
  })

  testAsync("awaited fields of an object-creating external", async () => {
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    let options = makeOptions(
      ~level=await mark("level", Promise.resolve(opaque(#high))),
      ~mode=await mark("mode", Promise.resolve(opaque(#dark))),
      (),
    )
    let optional = makeOptions(
      ~level=await mark("level", Promise.resolve(opaque(#low))),
      ~mode=?await mark("mode", Promise.resolve(opaque(Some(#light)))),
      (),
    )
    eq(__LOC__, ["level", "mode", "level", "mode"], log)
    eq(__LOC__, (10, Some("dark-mode")), (options["level"], options["mode"]->toOption))
    eq(__LOC__, (0, Some("light-mode")), (optional["level"], optional["mode"]->toOption))
  })
})
