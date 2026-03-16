// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// End-to-end test: JavaScript interop
// Tests FFI bindings, %raw, external declarations, and JS runtime interop.

let eq = (a, b) => a == b

// ─── @val: global function bindings ─────────────────────────────────
@val external parseInt2: (string, int) => int = "parseInt"
Test.run(__POS_OF__("interop @val: parseInt"), parseInt2("42", 10), eq, 42)
Test.run(__POS_OF__("interop @val: parseInt hex"), parseInt2("1f", 16), eq, 31)

@val external encodeURIComponent: string => string = "encodeURIComponent"
Test.run(
  __POS_OF__("interop @val: encodeURI"),
  encodeURIComponent("hello world"),
  eq,
  "hello%20world",
)

@val external decodeURIComponent: string => string = "decodeURIComponent"
Test.run(
  __POS_OF__("interop @val: decodeURI"),
  decodeURIComponent("hello%20world"),
  eq,
  "hello world",
)

// ─── @scope: namespaced bindings ────────────────────────────────────
@scope("Math") @val external mathPow: (float, float) => float = "pow"
@scope("Math") @val external mathSqrt: float => float = "sqrt"
@scope("Math") @val external mathPI: float = "PI"
@scope("Number") @val external numberIsInteger: float => bool = "isInteger"
@scope("Number") @val external numberIsFinite2: float => bool = "isFinite"

Test.run(__POS_OF__("interop @scope: pow"), mathPow(2.0, 8.0) > 255.9, eq, true)
Test.run(__POS_OF__("interop @scope: sqrt"), mathSqrt(144.0) > 11.99, eq, true)
Test.run(__POS_OF__("interop @scope: PI"), mathPI > 3.14, eq, true)
Test.run(__POS_OF__("interop @scope: isInteger"), numberIsInteger(5.0), eq, true)
Test.run(__POS_OF__("interop @scope: isInteger float"), numberIsInteger(5.5), eq, false)

// ─── @send: method calls ───────────────────────────────────────────
@send external arrayPush: (array<'a>, 'a) => int = "push"
@send external stringRepeat: (string, int) => string = "repeat"
@send external stringCharAt: (string, int) => string = "charAt"
@send external arraySlice: (array<'a>, int, int) => array<'a> = "slice"
@send external stringIncludes: (string, string) => bool = "includes"

Test.run(__POS_OF__("interop @send: repeat"), "ab"->stringRepeat(3), eq, "ababab")
Test.run(__POS_OF__("interop @send: charAt"), "hello"->stringCharAt(1), eq, "e")
Test.run(__POS_OF__("interop @send: includes"), "hello world"->stringIncludes("world"), eq, true)
Test.run(__POS_OF__("interop @send: slice"), [1, 2, 3, 4, 5]->arraySlice(1, 3), eq, [2, 3])

// ─── @get/@set: property access ─────────────────────────────────────
@get external arrayLength: array<'a> => int = "length"
Test.run(__POS_OF__("interop @get: length"), arrayLength([1, 2, 3]), eq, 3)
Test.run(__POS_OF__("interop @get: empty"), arrayLength([]), eq, 0)

// ─── @new: constructor calls ────────────────────────────────────────
@new external makeRegExp: (string, string) => {..} = "RegExp"
@send external regExpTest: ({..}, string) => bool = "test"

let re = makeRegExp("\\d+", "g")
Test.run(__POS_OF__("interop @new: RegExp"), regExpTest(re, "abc123"), eq, true)

@new external makeSet: unit => {..} = "Set"
@send external setAdd: ({..}, 'a) => {..} = "add"
@get external setSize: {..} => int = "size"
@send external setHas: ({..}, 'a) => bool = "has"

let s = makeSet()
let _ = s->setAdd(1)
let _ = s->setAdd(2)
let _ = s->setAdd(2) // duplicate
let _ = s->setAdd(3)
Test.run(__POS_OF__("interop Set: size"), setSize(s), eq, 3)
Test.run(__POS_OF__("interop Set: has"), setHas(s, 2), eq, true)
Test.run(__POS_OF__("interop Set: not has"), setHas(s, 5), eq, false)

// ─── @new: Map ──────────────────────────────────────────────────────
@new external makeMap: unit => {..} = "Map"
@send external mapSet: ({..}, string, 'a) => {..} = "set"
@send external mapGet: ({..}, string) => 'a = "get"
@send external mapHas: ({..}, string) => bool = "has"
@get external mapSize: {..} => int = "size"

let m = makeMap()
let _ = m->mapSet("a", 1)
let _ = m->mapSet("b", 2)
Test.run(__POS_OF__("interop Map: size"), mapSize(m), eq, 2)
Test.run(__POS_OF__("interop Map: has"), mapHas(m, "a"), eq, true)
Test.run(__POS_OF__("interop Map: get"), mapGet(m, "b"), eq, 2)

// ─── %raw: escape hatch ────────────────────────────────────────────
let rawAdd: (int, int) => int = %raw(`(a, b) => a + b`)
Test.run(__POS_OF__("interop %raw: add"), rawAdd(3, 4), eq, 7)

let rawConst: int = %raw(`42`)
Test.run(__POS_OF__("interop %raw: const"), rawConst, eq, 42)

let rawComplex: array<int> = %raw(`[1,2,3].map(x => x * x)`)
Test.run(__POS_OF__("interop %raw: complex"), rawComplex, eq, [1, 4, 9])

// ─── Null/undefined handling ────────────────────────────────────────
Test.run(__POS_OF__("interop null: some"), Nullable.make(42)->Nullable.toOption, eq, Some(42))
Test.run(__POS_OF__("interop null: null"), Nullable.null->Nullable.toOption, eq, None)
Test.run(__POS_OF__("interop null: undefined"), Nullable.undefined->Nullable.toOption, eq, None)

// ─── typeof checking ───────────────────────────────────────────────
Test.run(__POS_OF__("interop typeof: string"), Type.typeof("hello"), eq, #string)
Test.run(__POS_OF__("interop typeof: number"), Type.typeof(42), eq, #number)
Test.run(__POS_OF__("interop typeof: boolean"), Type.typeof(true), eq, #boolean)
Test.run(__POS_OF__("interop typeof: undefined"), Type.typeof(undefined), eq, #undefined)
Test.run(__POS_OF__("interop typeof: object"), Type.typeof({"a": 1}), eq, #object)
Test.run(
  __POS_OF__("interop typeof: function"),
  Type.typeof(x => x),
  eq,
  #function,
)

// ─── Promise interop ────────────────────────────────────────────────
// Promise.resolve produces correct values
let p = Promise.resolve(42)
let _ = p->Promise.then(v => {
  Test.run(__POS_OF__("interop promise: resolve"), v, eq, 42)
  Promise.resolve()
})

// Promise.all
let _ = Promise.all([Promise.resolve(1), Promise.resolve(2), Promise.resolve(3)])->Promise.then(
  results => {
    Test.run(__POS_OF__("interop promise: all"), results, eq, [1, 2, 3])
    Promise.resolve()
  },
)

// ─── Object creation ────────────────────────────────────────────────
let obj = {"name": "Alice", "age": 30}
Test.run(__POS_OF__("interop obj: name"), obj["name"], eq, "Alice")
Test.run(__POS_OF__("interop obj: age"), obj["age"], eq, 30)

// ─── Callback interop ──────────────────────────────────────────────
let callWith = (f, x) => f(x)
Test.run(__POS_OF__("interop callback: apply"), callWith(x => x + 1, 5), eq, 6)

let callWithTwo = (f, x, y) => f(x, y)
Test.run(__POS_OF__("interop callback: apply2"), callWithTwo((a, b) => a + b, 3, 4), eq, 7)

// ─── Labeled args in FFI ────────────────────────────────────────────
// ReScript labeled args work with JS interop
let makeConfig = (~host, ~port=8080, ~debug=false) => {
  `${host}:${Int.toString(port)}${debug ? " (debug)" : ""}`
}
Test.run(
  __POS_OF__("interop labels: defaults"),
  makeConfig(~host="localhost"),
  eq,
  "localhost:8080",
)
Test.run(
  __POS_OF__("interop labels: override"),
  makeConfig(~host="example.com", ~port=3000, ~debug=true),
  eq,
  "example.com:3000 (debug)",
)
