@@config({flags: ["-bs-jsx", "4"]})

open Mocha
open Test_utils

// An optional argument of an external passed as `~x=?e` is left out, that
// is passed as undefined, when `e` is None, and so is a JSX key. A call, a
// lookup or a conditional cannot be read twice to test it first, so it is
// unwrapped by Primitive_option.valFromOption, which keeps undefined.

%%raw(`
function greet(name) {
  return name === undefined ? "Hello" : "Hello " + name
}

function Greeting(name) {
  this.text = greet(name)
}
`)

@val external greet: (~name: string=?, unit) => string = "greet"

type greeting
@new external makeGreeting: (~name: string=?, unit) => greeting = "Greeting"
@get external text: greeting => string = "text"

type target

let makeTarget: unit => target = %raw(`() => {
  const calls = []
  const record = (...args) => {
    calls.push(args.map(String).join(" "))
  }
  return { calls, greet, setLevel: record, setMode: record }
}`)

@get external calls: target => array<string> = "calls"

@send external greetWith: (target, ~name: string=?, unit) => string = "greet"

@send
external setLevel: (target, ~level: @int [#low | @as(10) #high]=?, unit) => unit = "setLevel"

@send
external setMode: (
  target,
  ~mode: @string [@as("light-mode") #light | @as("dark-mode") #dark]=?,
  unit,
) => unit = "setMode"

// Opaque, so that the optimizer cannot tell the argument is None.
let opaque: 'a => 'a = %raw(`x => x`)

let greetUser = (names: dict<string>, id) => greet(~name=?names->Dict.get(id), ())

// Inlined into its callers, where the argument replaces [name].
let hello = (~name=?, ()) => greet(~name?, ())

let helloUser = (names: dict<string>, id) => hello(~name=?names->Dict.get(id), ())

let item = (keys: dict<string>, id) => <li key=?{keys->Dict.get(id)}>{React.string(id)}</li>

@get external elementKey: React.element => Nullable.t<string> = "key"

describe(__MODULE__, () => {
  test("None from a call, a lookup or a conditional", () => {
    let names = dict{"ann": "Ann"}
    eq(__LOC__, "Hello Ann", greetUser(names, "ann"))
    eq(__LOC__, "Hello", greetUser(names, "bob"))
    let list = ["Ann"]
    let at = i => greet(~name=?list->Array.get(i), ())
    eq(__LOC__, ("Hello Ann", "Hello"), (at(0), at(1)))
    eq(__LOC__, "Hello", greet(~name=?opaque(None), ()))
    let pick = (b, name) => greet(~name=?b ? Some(name) : None, ())
    eq(__LOC__, ("Hello Ann", "Hello"), (pick(true, "Ann"), pick(false, "Ann")))
  })

  test("None passed to @send and @new externals", () => {
    let t = makeTarget()
    eq(__LOC__, "Hello", t->greetWith(~name=?opaque(None), ()))
    eq(__LOC__, "Hello", makeGreeting(~name=?opaque(None), ())->text)
  })

  test("None forwarded by an inlined function", () => {
    let names = dict{"ann": "Ann"}
    eq(__LOC__, ("Hello Ann", "Hello"), (helloUser(names, "ann"), helloUser(names, "bob")))
  })

  test("None for @int and @string arguments", () => {
    let t = makeTarget()
    let levels = [#high]
    let modes = [#dark]
    t->setLevel(~level=?levels->Array.get(0), ())
    t->setLevel(~level=?levels->Array.get(1), ())
    t->setMode(~mode=?modes->Array.get(0), ())
    t->setMode(~mode=?modes->Array.get(1), ())
    eq(__LOC__, ["10", "undefined", "dark-mode", "undefined"], calls(t))
  })

  test("a JSX key that is None", () => {
    let keys = dict{"a": "key-a"}
    eq(__LOC__, Some("key-a"), item(keys, "a")->elementKey->Nullable.toOption)
    eq(__LOC__, None, item(keys, "b")->elementKey->Nullable.toOption)
  })
})
