// SPDX-License-Identifier: (LGPL-3.0-or-later AND MIT)
// Point-to-point test: JS Code Generation — data structure encoding
// Tests that ReScript data structures are correctly encoded in JavaScript
// and produce correct runtime behavior.

let eq = (a, b) => a == b

// ─── Variant encoding ──────────────────────────────────────────────
// Simple variants (no payload) encode as integers
type simpleVariant = A | B | C | D | E

let variantToInt = v =>
  switch v {
  | A => 0
  | B => 1
  | C => 2
  | D => 3
  | E => 4
  }

Test.run(__POS_OF__("variant enc: A"), variantToInt(A), eq, 0)
Test.run(__POS_OF__("variant enc: C"), variantToInt(C), eq, 2)
Test.run(__POS_OF__("variant enc: E"), variantToInt(E), eq, 4)

// Variants with payloads encode as tagged objects
type payload = Empty | Single(int) | Pair(int, string) | Triple(int, string, bool)

let describePayload = p =>
  switch p {
  | Empty => "empty"
  | Single(n) => `single:${Int.toString(n)}`
  | Pair(n, s) => `pair:${Int.toString(n)},${s}`
  | Triple(n, s, b) => `triple:${Int.toString(n)},${s},${b ? "t" : "f"}`
  }

Test.run(__POS_OF__("variant pay: empty"), describePayload(Empty), eq, "empty")
Test.run(__POS_OF__("variant pay: single"), describePayload(Single(42)), eq, "single:42")
Test.run(
  __POS_OF__("variant pay: pair"),
  describePayload(Pair(1, "hi")),
  eq,
  "pair:1,hi",
)
Test.run(
  __POS_OF__("variant pay: triple"),
  describePayload(Triple(1, "a", true)),
  eq,
  "triple:1,a,t",
)

// ─── Record encoding ────────────────────────────────────────────────
// Records encode as objects with named fields
type user = {name: string, age: int, active: bool}

let u = {name: "Alice", age: 30, active: true}
Test.run(__POS_OF__("record: name"), u.name, eq, "Alice")
Test.run(__POS_OF__("record: age"), u.age, eq, 30)
Test.run(__POS_OF__("record: active"), u.active, eq, true)

// Record update
let u2 = {...u, age: 31}
Test.run(__POS_OF__("record: update preserves"), u2.name, eq, "Alice")
Test.run(__POS_OF__("record: update changes"), u2.age, eq, 31)

// Nested records
type address = {street: string, city: string}
type person = {pname: string, addr: address}

let p = {pname: "Bob", addr: {street: "123 Main St", city: "Springfield"}}
Test.run(__POS_OF__("record: nested"), p.addr.city, eq, "Springfield")

// ─── Tuple encoding ─────────────────────────────────────────────────
// Tuples encode as arrays
let t2 = (1, "two")
let (t2a, t2b) = t2
Test.run(__POS_OF__("tuple: pair fst"), t2a, eq, 1)
Test.run(__POS_OF__("tuple: pair snd"), t2b, eq, "two")

let t3 = (true, 42, "hello")
let (t3a, t3b, t3c) = t3
Test.run(__POS_OF__("tuple: triple 1"), t3a, eq, true)
Test.run(__POS_OF__("tuple: triple 2"), t3b, eq, 42)
Test.run(__POS_OF__("tuple: triple 3"), t3c, eq, "hello")

let t5 = (1, 2, 3, 4, 5)
let (_, _, _, _, t5e) = t5
Test.run(__POS_OF__("tuple: quintuple last"), t5e, eq, 5)

// ─── Option encoding ────────────────────────────────────────────────
// Some(x) encodes as x, None encodes as undefined
let someVal: option<int> = Some(42)
let noneVal: option<int> = None

Test.run(
  __POS_OF__("option: some unwrap"),
  switch someVal {
  | Some(v) => v
  | None => -1
  },
  eq,
  42,
)
Test.run(
  __POS_OF__("option: none check"),
  switch noneVal {
  | Some(_) => true
  | None => false
  },
  eq,
  false,
)

// Nested options
let nestedSome: option<option<int>> = Some(Some(42))
let nestedNone: option<option<int>> = Some(None)
Test.run(
  __POS_OF__("option: nested some"),
  switch nestedSome {
  | Some(Some(v)) => v
  | _ => -1
  },
  eq,
  42,
)
Test.run(
  __POS_OF__("option: nested none"),
  switch nestedNone {
  | Some(None) => true
  | _ => false
  },
  eq,
  true,
)

// Option utilities
Test.run(__POS_OF__("option: getOr some"), Option.getOr(Some(42), 0), eq, 42)
Test.run(__POS_OF__("option: getOr none"), Option.getOr(None, 0), eq, 0)
Test.run(__POS_OF__("option: map some"), Option.map(Some(5), x => x * 2), eq, Some(10))
Test.run(__POS_OF__("option: map none"), Option.map(None, x => x * 2), eq, None)
Test.run(__POS_OF__("option: isSome"), Option.isSome(Some(1)), eq, true)
Test.run(__POS_OF__("option: isNone"), Option.isNone(None), eq, true)
Test.run(
  __POS_OF__("option: flatMap some"),
  Option.flatMap(Some(10), x => x > 5 ? Some(x) : None),
  eq,
  Some(10),
)
Test.run(
  __POS_OF__("option: flatMap to none"),
  Option.flatMap(Some(3), x => x > 5 ? Some(x) : None),
  eq,
  None,
)

// ─── Result encoding ────────────────────────────────────────────────
let okVal: result<int, string> = Ok(42)
let errVal: result<int, string> = Error("oops")

Test.run(
  __POS_OF__("result: ok unwrap"),
  switch okVal {
  | Ok(v) => v
  | Error(_) => -1
  },
  eq,
  42,
)
Test.run(
  __POS_OF__("result: error unwrap"),
  switch errVal {
  | Ok(_) => ""
  | Error(e) => e
  },
  eq,
  "oops",
)

Test.run(__POS_OF__("result: isOk"), Result.isOk(Ok(1)), eq, true)
Test.run(__POS_OF__("result: isError"), Result.isError(Error("x")), eq, true)
Test.run(__POS_OF__("result: getOr ok"), Result.getOr(Ok(42), 0), eq, 42)
Test.run(__POS_OF__("result: getOr error"), Result.getOr(Error("x"), 0), eq, 0)
Test.run(
  __POS_OF__("result: map ok"),
  Result.map(Ok(5), x => x * 2),
  eq,
  Ok(10),
)
Test.run(
  __POS_OF__("result: map error"),
  Result.map(Error("x"), x => x * 2),
  eq,
  Error("x"),
)

// ─── Polymorphic variant encoding ───────────────────────────────────
// Poly variants encode as strings or tagged values
let descPolyvar = v =>
  switch v {
  | #red => "red"
  | #green => "green"
  | #blue => "blue"
  | #rgb(r, g, b) => `rgb(${Int.toString(r)},${Int.toString(g)},${Int.toString(b)})`
  }

Test.run(__POS_OF__("polyvar: simple"), descPolyvar(#red), eq, "red")
Test.run(__POS_OF__("polyvar: payload"), descPolyvar(#rgb(255, 128, 0)), eq, "rgb(255,128,0)")

// ─── Array encoding ─────────────────────────────────────────────────
// Arrays encode as JavaScript arrays
let emptyArr: array<int> = []
Test.run(__POS_OF__("array: empty length"), Array.length(emptyArr), eq, 0)

let nums = [1, 2, 3, 4, 5]
Test.run(__POS_OF__("array: literal"), nums, eq, [1, 2, 3, 4, 5])
Test.run(__POS_OF__("array: access"), nums[0], eq, 1)
Test.run(__POS_OF__("array: last"), nums[Array.length(nums) - 1], eq, 5)

// Array with mixed variant elements
type item = Num2(int) | Str2(string)
let mixed = [Num2(1), Str2("a"), Num2(2)]
Test.run(
  __POS_OF__("array: variant elements"),
  mixed->Array.map(i =>
    switch i {
    | Num2(n) => Int.toString(n)
    | Str2(s) => s
    }
  ),
  eq,
  ["1", "a", "2"],
)

// ─── Dict encoding ──────────────────────────────────────────────────
let dict = Dict.fromArray([("a", 1), ("b", 2), ("c", 3)])
Test.run(__POS_OF__("dict: get existing"), Dict.get(dict, "b"), eq, Some(2))
Test.run(__POS_OF__("dict: get missing"), Dict.get(dict, "z"), eq, None)
Test.run(
  __POS_OF__("dict: keys"),
  Dict.keysToArray(dict)->Array.toSorted((a, b) => String.localeCompare(a, b)),
  eq,
  ["a", "b", "c"],
)

// ─── Unboxed variant encoding ───────────────────────────────────────
@unboxed type jsonLike = String2(string) | Number2(float) | Bool2(bool) | Null2

let classifyJson = v =>
  switch v {
  | String2(s) => `str:${s}`
  | Number2(n) => `num:${Float.toString(n)}`
  | Bool2(b) => `bool:${b ? "true" : "false"}`
  | Null2 => "null"
  }

Test.run(__POS_OF__("unboxed: string"), classifyJson(String2("hi")), eq, "str:hi")
Test.run(__POS_OF__("unboxed: number"), classifyJson(Number2(3.14)), eq, "num:3.14")
Test.run(__POS_OF__("unboxed: bool"), classifyJson(Bool2(true)), eq, "bool:true")
Test.run(__POS_OF__("unboxed: null"), classifyJson(Null2), eq, "null")

// ─── List encoding ──────────────────────────────────────────────────
let l = list{1, 2, 3}
Test.run(
  __POS_OF__("list: to array"),
  List.toArray(l),
  eq,
  [1, 2, 3],
)
Test.run(
  __POS_OF__("list: length"),
  List.length(l),
  eq,
  3,
)
Test.run(
  __POS_OF__("list: head"),
  List.head(l),
  eq,
  Some(1),
)

// ─── Ref encoding ───────────────────────────────────────────────────
let r = ref(0)
Test.run(__POS_OF__("ref: initial"), r.contents, eq, 0)
r := 42
Test.run(__POS_OF__("ref: after set"), r.contents, eq, 42)
r.contents = 99
Test.run(__POS_OF__("ref: after contents set"), r.contents, eq, 99)

// ─── BigInt encoding ────────────────────────────────────────────────
let big = 9007199254740993n
Test.run(__POS_OF__("bigint: literal"), big, eq, 9007199254740993n)
Test.run(__POS_OF__("bigint: add"), 1n + 2n, eq, 3n)
Test.run(__POS_OF__("bigint: mul"), 100n * 200n, eq, 20000n)
Test.run(__POS_OF__("bigint: sub"), 10n - 3n, eq, 7n)

// ─── Exception encoding ────────────────────────────────────────────
exception CustomExn(string, int)

let testExn =
  try {
    raise(CustomExn("test", 42))
  } catch {
  | CustomExn(msg, code) => `${msg}:${Int.toString(code)}`
  | _ => "unknown"
  }
Test.run(__POS_OF__("exn: custom payload"), testExn, eq, "test:42")
