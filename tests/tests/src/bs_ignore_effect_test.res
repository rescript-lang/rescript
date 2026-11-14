open Mocha
open Test_utils

%%raw(`
function add(x,y){
  return x + y
}
`)

type rec kind<_> =
  | Float: kind<float>
  | String: kind<string>

@val external add: (@ignore kind<'a>, 'a, 'a) => 'a = "add"

let v = ref(0)

@obj external config: (~hi: int, ~lo: int, unit) => _ = ""

let h = config(~hi=2, ~lo=0, ignore(v.contents = v.contents + 1))

let z = add(
  {
    v.contents = v.contents + 1
    Float
  },
  3.0,
  2.0,
)

// The effects of a unit or @ignore argument run in its place among the
// arguments, after the arguments to its left.
@send external pushThen: (array<int>, int, unit) => int = "push"
@send external pushIgnoring: (array<int>, int, @ignore int) => int = "push"
@send external pushIgnoringOpt: (array<int>, int, ~extra: @ignore int=?, unit) => int = "push"
@obj external makeThen: (~a: int, unit) => {"a": int} = ""

// The object of a @send call is the object whatever its spec says.
@send external pushOnIgnored: (@ignore array<int>, int) => int = "push"

// An external whose arguments are all @ignore is called without them, or is
// a value when it is a JS module.
@val external countArgs: (@ignore int, @ignore int) => int = "bsIgnoreEffectCountArgs"
@module external pathModule: @ignore int => {"sep": string} = "node:path"

let defineCountArgs: unit => unit = %raw(`() => {
  globalThis.bsIgnoreEffectCountArgs = (...args) => args.length
}`)

let counter = ref(0)
let bump = () => counter := counter.contents + 1

describe(__MODULE__, () => {
  test("ignore effect 1", () => eq(__LOC__, v.contents, 2))
  test("ignore effect 2", () => eq(__LOC__, z, 5.0))

  test("unit and @ignore arguments after others", () => {
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    let arr = []
    arr->pushThen(mark("value", 1), mark("unit", ()))->ignore
    arr->pushIgnoring(mark("value", 2), mark("ignored", 0))->ignore
    let o = makeThen(~a=mark("field", 3), mark("unit", ()))
    eq(__LOC__, ["value", "unit", "value", "ignored", "field", "unit"], log)
    eq(__LOC__, [1, 2], arr)
    eq(__LOC__, 3, o["a"])
  })

  test("an optional @ignore argument that is absent", () => {
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    let arr = []
    arr->pushIgnoringOpt(mark("value", 1), ~extra=?mark("absent", None), ())->ignore
    eq(__LOC__, ["value", "absent"], log)
    eq(__LOC__, [1], arr)
  })

  test("a unit argument that changes an earlier argument", () => {
    let arr = []
    arr->pushThen(counter.contents, bump())->ignore
    eq(__LOC__, [0], arr)
  })

  test("the object of a @send call marked @ignore", () => {
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    let arr = []
    arr->pushOnIgnored(1)->ignore
    mark("object", arr)->pushOnIgnored(2)->ignore
    eq(__LOC__, ["object"], log)
    eq(__LOC__, [1, 2], arr)
  })

  test("an external whose arguments are all @ignore", () => {
    let log = []
    let mark = (s, v) => {
      log->Array.push(s)
      v
    }
    defineCountArgs()
    let pure = countArgs(1, 2)
    let effectful = countArgs(mark("first", 1), mark("second", 2))
    let path = pathModule(mark("module", 0))
    eq(__LOC__, (0, 0), (pure, effectful))
    eq(__LOC__, ["first", "second", "module"], log)
    eq(__LOC__, true, path["sep"] == "/" || path["sep"] == "\\")
  })
})
